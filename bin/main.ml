(* Fourier/NTT visualizer — OCaml + brr + note (FRP).

   Architecture (deliberate, per the discussion):
   - [model] is an immutable record.
   - [action] is the only way to change it.
   - [apply : action -> model -> model] is the single pure update fn.
   - The only "global" is the Note signal holding the model. UI code
     can only [send] actions; the model changes exclusively inside
     [S.accum] through [apply]. Every render reads the current value
     of the signal — stale-closure bugs are structurally impossible. *)

open Brr
open Brr_canvas
open Note
open Note_brr

(* ---------- model ---------- *)

let compute_idft (n : int) (m : float array) : float array =
  let x =
    Fourier.Dft.make n n (fun i j ->
        let v = m.((i * n) + j) in
        Complex.{ re = v; im = 0.0 })
  in
  let raw = (Fourier.Dft.idft2d x).data in
  Array.map
    (fun c ->
      let re = Complex.(c.re) and im = Complex.(c.im) in
      Float.max 0.0 (Float.min 1.0 (Float.max re im)))
    raw

let compute_dft (n : int) (m : float array) : float array =
  let x =
    Fourier.Dft.make n n (fun i j ->
        let v = m.((i * n) + j) in
        (* bind outside: Complex.{..} opens
                                           the Complex module, whose value [i]
                                           (imaginary unit!) would shadow the
                                           loop variable [i] inside the record *)
        Complex.{ re = v; im = 0.0 })
  in
  Fourier.Dft.magnitudes (Fourier.Dft.dft2d x)

let compute_intt (n : int) (m : float array) : float array =
  let p = 257L and root = 2L in
  let x =
    Fourier.Ntt.make n n (fun i j ->
        Int64.of_int (Float.to_int (Float.round (m.((i * n) + j) *. 255.0))))
  in
  let w = Fourier.Ntt.ntt_matrix n root p in
  let w_inv = Fourier.Ntt.intt_matrix n root p in
  Fourier.Ntt.to_doubles (Fourier.Ntt.intt2d x w w_inv) p

let compute_ntt (n : int) (m : float array) : float array =
  let p = 257L and root = 2L in
  let x =
    Fourier.Ntt.make n n (fun i j ->
        Int64.of_int (Float.to_int (Float.round (m.((i * n) + j) *. 255.0))))
  in
  let w = Fourier.Ntt.ntt_matrix n root p in
  Fourier.Ntt.to_doubles (Fourier.Ntt.ntt2d x w) p

type mode = Dft | Ntt

type model = {
  n : int; (* grid size *)
  pixels :
    float array (* n*n grayscale in [0,1]; replaced, never mutated in place *);
  pixels_transformed : float array;
  brush_color : float;
  brush_size : int;
  mode : mode;
}

let empty_model n =
  let pixels = Array.make (n * n) 0.0 in
  {
    n;
    pixels;
    pixels_transformed = compute_dft n pixels;
    brush_color = 127.;
    brush_size = 1;
    mode = Dft;
  }

let draw_on_array i j arr brush_size brush_color n =
  let () =
    for ii = -brush_size + 1 to brush_size - 1 do
      let i_clamped = min n (max (i + ii) 0) in
      for jj = -brush_size + 1 to brush_size - 1 do
        let j_clamped = min n (max (j + jj) 0) in
        arr.((i_clamped * n) + j_clamped) <- brush_color
      done
    done
  in
  arr.((i * n) + j) <- brush_color;
  arr

(* ---------- actions ---------- *)

type action =
  | Draw of (int * int)
  | Clear
  | Transform
  | InverseTransform
  | Set_mode of mode
  | BrushColor of float
  | BrushSize of int

(* the single update function — pure *)
let rec apply (a : action) (m : model) : model =
  match a with
  | Draw (i, j) ->
      let p = draw_on_array i j m.pixels m.brush_size m.brush_color m.n in
      { (apply Transform m) with pixels = p }
  | Clear -> { m with pixels = Array.make (m.n * m.n) 0.0 }
  | Transform ->
      {
        m with
        pixels_transformed =
          (match m.mode with
          | Dft -> compute_dft m.n m.pixels
          | Ntt -> compute_ntt m.n m.pixels);
      }
  | InverseTransform ->
      {
        m with
        pixels =
          (match m.mode with
          | Dft -> compute_idft m.n m.pixels_transformed
          | Ntt -> compute_intt m.n m.pixels_transformed);
      }
  | Set_mode md -> { (apply Transform m) with mode = md }
  | BrushColor f -> { m with brush_color = f /. 255.0 }
  | BrushSize size -> { m with brush_size = size }

(* ---------- rendering (pure read of the model) ---------- *)

let draw_grid c m values =
  let cell = 24.0 in
  C2d.set_fill_style c (C2d.color (Jstr.v "#1010a0"));
  C2d.fill_rect c ~x:0. ~y:0. ~w:(float m.n *. cell) ~h:(float m.n *. cell);
  for i = 0 to m.n - 1 do
    for j = 0 to m.n - 1 do
      let g = values.((i * m.n) + j) in
      let v = int_of_float (Float.max 0.0 (Float.min 255.0 (g *. 255.0))) in
      let col = Printf.sprintf "rgb(%d,%d,%d)" v v v in
      C2d.set_fill_style c (C2d.color (Jstr.v col));
      C2d.fill_rect c ~x:(float j *. cell) ~y:(float i *. cell) ~w:cell ~h:cell;
      C2d.set_stroke_style c (C2d.color (Jstr.v "#2a2a30"));
      C2d.stroke_rect c
        ~x:(float j *. cell)
        ~y:(float i *. cell)
        ~w:cell ~h:cell
    done
  done

let render m input_c out_c label =
  draw_grid input_c m m.pixels;
  draw_grid out_c m m.pixels_transformed;
  El.set_children label
    [
      El.txt'
        (match m.mode with
        | Dft -> "DFT (Complex)"
        | Ntt -> "NTT (int64 mod p)");
    ]

(* ---------- event → action mapping ---------- *)

(* offset_x/y are already in target space *)
let cell_of_event n e =
  let x = Ev.Mouse.offset_x e in
  let y = Ev.Mouse.offset_y e in
  let cell = 24. in
  let j = int_of_float (x /. cell) and i = int_of_float (y /. cell) in
  if i >= 0 && i < n && j >= 0 && j < n then Some (i, j) else None

let main () =
  let n = 16 in
  let doc = G.document in

  let input_cnv = Canvas.create ~w:(n * 24) ~h:(n * 24) [] in
  let out_cnv = Canvas.create ~w:(n * 24) ~h:(n * 24) [] in
  let label = El.h2 [ El.txt' "DFT (Complex)" ] in
  let input_c = C2d.get_context input_cnv in
  let out_c = C2d.get_context out_cnv in

  (* one primitive event of actions; UI elements only send to it *)
  let (actions : action event), (send : action E.send) = E.create () in

  let mk_button (s : string) (a : action) : El.t =
    let b = El.button [ El.txt' s ] in
    Option.iter Logr.hold (E.log (Evr.on_el Ev.click (fun _ -> a) b) send);
    b
  in

  let mk_slider min max current mk_action =
    let slider = El.input () in
    El.set_at (Jstr.v "type") (Some (Jstr.v "range")) slider;
    El.set_at (Jstr.v "min") (Some (Jstr.of_float min)) slider;
    El.set_at (Jstr.v "max") (Some (Jstr.of_float max)) slider;
    El.set_at (Jstr.v "value") (Some (Jstr.of_float current)) slider;
    let evt =
      Evr.on_el Ev.input
        (fun _ ->
          match
            float_of_string_opt (Jstr.to_string (El.prop El.Prop.value slider))
          with
          | Some f -> Some (mk_action f)
          | None -> None)
        slider
    in
    (E.filter_map Fun.id evt, slider)
  in

  let brush_color_actions, brush_color =
    mk_slider 0.0 255.0 128.0 (fun f -> BrushColor f)
  in

  let brush_size_actions, brush_size =
    mk_slider 1.0 5.0 1.0 (fun f -> BrushSize (int_of_float f))
  in

  let controls =
    El.div ~at:[]
      [
        mk_button "Clear" Clear;
        mk_button "DFT" (Set_mode Dft);
        mk_button "NTT" (Set_mode Ntt);
        mk_button "InverseTransform" InverseTransform;
        brush_color;
        brush_size;
      ]
  in

  (* canvas drag: mousedown starts, mousemove draws while pressed *)
  let input_el = Canvas.to_el input_cnv in
  let down, set_down = S.create false in
  Option.iter Logr.hold
    (E.log (Evr.on_el Ev.mousedown (fun _ -> true) input_el) set_down);
  Option.iter Logr.hold
    (E.log
       (Evr.on_target Ev.mouseup (fun _ -> false) (Window.as_target G.window))
       set_down);
  let moves =
    Evr.on_el Ev.mousemove
      (fun e -> if S.value down then cell_of_event n (Ev.as_type e) else None)
      input_el
  in
  let moves =
    E.filter_map (function Some c -> Some (Draw c) | None -> None) moves
  in
  let actions =
    E.select [ actions; moves; brush_color_actions; brush_size_actions ]
  in

  (* the reactive system: one signal, one pure update *)
  let model = S.accum (empty_model n) (E.map apply actions) in
  Logr.hold @@ S.log model (fun m -> render m input_c out_c label);

  El.set_children (Document.body doc)
    [
      El.h1 [ El.txt' "Fourier / NTT visualizer" ];
      controls;
      label;
      El.div [ input_el; Canvas.to_el out_cnv ];
    ]

let () = main ()
