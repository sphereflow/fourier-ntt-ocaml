(* NTT over GF(257) — the same matrix functor, a different ring *)

module Zp257 = Ring.Zp (struct let p = 257L end)
module M = Matrix.Make (Zp257)
include M

let rec mod_pow b e p =
  if e = 0L then 1L
  else if Int64.equal (Int64.rem e 2L) 0L then
    let h = mod_pow b (Int64.div e 2L) p in
    Int64.rem (Int64.mul h h) p
  else
    Int64.rem (Int64.mul b (mod_pow b (Int64.sub e 1L) p)) p

(* NTT matrix: W[j,k] = root^(j*k) mod p *)
let ntt_matrix n root p =
  make n n @@ fun j k ->
  let e = Int64.of_int ((j * k) mod n) in
  mod_pow root e p

(* 2D NTT: Y = ((W * X) * W^T) — the ring ops apply the modulus *)
let ntt2d x w = mul w (mul x (transpose w))

(* normalized for grayscale rendering *)
let to_doubles m p =
  let q = Int64.to_float p in
  Array.map (fun v -> Int64.to_float v /. q) m.data
