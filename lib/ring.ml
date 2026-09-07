(* Ring abstraction — the OCaml analog of a Rust trait.

   Rust:  trait Ring { fn zero() -> Self; fn add(...) -> Self; ... }
   OCaml: a module type, satisfied by a module, consumed via a functor.
   Dispatch is resolved at compile time — no vtables, no instance search. *)

module type S = sig
  type t
  val zero : t
  val ( + ) : t -> t -> t
  val ( * ) : t -> t -> t
end

(* impl Ring for Complex.t *)
module Complex_ring = struct
  include Complex          (* brings t, zero, add, mul *)
  let ( + ) = add
  let ( * ) = mul
end

(* impl Ring for int64 mod p — a functor because the modulus is a parameter *)
module Zp (P : sig val p : int64 end) = struct
  type t = int64
  let zero = 0L
  (* least non-negative residue *)
  let pos x =
    let v = Int64.rem x P.p in
    if Int64.compare v 0L < 0 then Int64.add v P.p else v
  let ( + ) a b = pos (Int64.add a b)
  let ( * ) a b = pos (Int64.mul a b)
end
