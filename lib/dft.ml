(* DFT over the complex ring — one instantiation of the generic matrix *)

module M = Matrix.Make (Ring.Complex_ring)
include M   (* re-export make/get/mul/transpose/t so callers stay short *)

(* DFT matrix: W[j,k] = exp(-2*pi*i*j*k/N) *)
let dft_matrix n =
  make n n @@ fun j k ->
  let angle = -2.0 *. Float.pi *. float (j * k) /. float n in
  Complex.polar 1.0 angle

(* 2D DFT: Y = W * X * W^T *)
let dft2d x =
  let n = x.rows in
  let w = dft_matrix n in
  mul w (mul x (transpose w))

(* 2D inverse DFT: X = conj(W) * Y * conj(W)^T / N^2.
   Since conj(W)[j,k] = exp(+2*pi*i*j*k/N) = W^-1 * N, the two /N factors
   of the textbook inverse are folded into one /N^2 at the end.
   W is symmetric, so conj(W)^T = conj(W). *)
let idft2d y =
  let n = y.rows in
  let w = dft_matrix n in
  let wbar =
    make n n @@ fun j k -> Complex.conj (get w j k) in
  let inv_n2 = 1.0 /. (float n *. float n) in
  let raw = mul wbar (mul y (transpose wbar)) in
  make n n @@ fun i j ->
  Complex.mul (get raw i j) { re = inv_n2; im = 0.0 }

(* log-magnitude as grayscale in [0,1], normalized so the brightest
   component maps to white — a fixed divisor renders sparse images black *)
let magnitudes m =
  let g = Array.make (m.rows * m.cols) 0.0 in
  let max_norm =
    Array.fold_left (fun acc c -> Float.max acc (Complex.norm c)) 0.0 m.data in
  let scale =
    if max_norm > 0.0 then 1.0 /. Float.log10 (1.0 +. max_norm) else 1.0 in
  for i = 0 to m.rows * m.cols - 1 do
    let c = m.data.(i) in
    g.(i) <- Float.log10 (1.0 +. Complex.norm c) *. scale
  done;
  g
