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

type mode = Dft | Ntt

type model = {
  n : int; (* grid size *)
  pixels :
    float array (* n*n grayscale in [0,1]; replaced, never mutated in place *);
  brush_color : float;
  mode : mode;
}

let idx m i j = (i * m.n) + j

let empty_model n =
  { n; pixels = Array.make (n * n) 0.0; brush_color = 127.; mode = Dft }

(* ---------- actions ---------- *)

type action =
  | Draw of (int * int)
  | Clear
  | Set_mode of mode
  | BrushColor of float

(* the single update function — pure *)
let apply (a : action) (m : model) : model =
  match a with
  | Draw (i, j) ->
      let p = Array.copy m.pixels in
      p.(idx m i j) <- m.brush_color;
      { m with pixels = p }
  | Clear -> { m with pixels = Array.make (m.n * m.n) 0.0 }
  | Set_mode md -> { m with mode = md }
  | BrushColor f -> { m with brush_color = f *. 0.0 }

(* ---------- transforms (pure) ---------- *)

let compute_dft (m : model) : float array =
  let x =
    Fourier.Dft.make m.n m.n (fun i j ->
        let v = m.pixels.(idx m i j) in
        (* bind outside: Complex.{..} opens
                                           the Complex module, whose value [i]
                                           (imaginary unit!) would shadow the
                                           loop variable [i] inside the record *)
        Complex.{ re = v; im = 0.0 })
  in
  Fourier.Dft.magnitudes (Fourier.Dft.dft2d x)

let compute_ntt (m : model) : float array =
  let p = 257L and root = 93L in
  let x =
    Fourier.Ntt.make m.n m.n (fun i j ->
        Int64.of_int
          (Float.to_int (Float.round (m.pixels.(idx m i j) *. 255.0))))
  in
  let w = Fourier.Ntt.ntt_matrix m.n root p in
  Fourier.Ntt.to_doubles (Fourier.Ntt.ntt2d x w) p

(* ---------- rendering (pure read of the model) ---------- *)

let draw_grid c m values =
  let cell = 24.0 in
  C2d.set_fill_style c (C2d.color (Jstr.v "#101014"));
  C2d.fill_rect c ~x:0. ~y:0. ~w:(float m.n *. cell) ~h:(float m.n *. cell);
  for i = 0 to m.n - 1 do
    for j = 0 to m.n - 1 do
      let g = values.(idx m i j) in
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
  draw_grid out_c m
    (match m.mode with Dft -> compute_dft m | Ntt -> compute_ntt m);
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

  let mk_slider : El.t =
    let slider = El.input () in
    El.set_at (Jstr.v "type") (Some (Jstr.v "range")) slider;
    El.set_at (Jstr.v "min") (Some (Jstr.v "0")) slider;
    El.set_at (Jstr.v "max") (Some (Jstr.v "255")) slider;
    El.set_at (Jstr.v "value") (Some (Jstr.v "128")) slider;
    (* Option.iter Logr.hold
      (E.log (Evr.on_el Ev.drag (fun aa -> a) slider) send); *)
    let _on_input ev =
      let val_str = El.prop El.Prop.value slider in
      (* Convert the string representation back to float safely *)
      match float_of_string_opt (Jstr.to_string val_str) with
      | None -> ()
      | Some f ->
          Option.iter Logr.hold
            (E.log (ev Ev.input (fun _ -> BrushColor f) slider) send);
          Console.(log [ Jstr.v "new slider value: "; f ])
      (* Example: Dispatch or handle the action here *)
    in
    (* let target = El.as_target slider in *)
    (* ignore (Ev.listen Ev.input on_input target); *)
    slider
  in

  let controls =
    El.div ~at:[]
      [
        mk_button "Clear" Clear;
        mk_button "DFT" (Set_mode Dft);
        mk_button "NTT" (Set_mode Ntt);
        mk_slider;
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
  let actions = E.select [ actions; moves ] in

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
