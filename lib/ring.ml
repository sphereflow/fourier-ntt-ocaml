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
  include Complex (* brings t, zero, add, mul *)

  let ( + ) = add
  let ( * ) = mul
end

(* impl Ring for int64 mod p — a functor because the modulus is a parameter *)
(* scalar division / power over the integers mod p — needed for the
   inverse NTT (a field operation, not a ring one, hence kept here
   rather than in the Ring module type) *)
module type Field = sig
  include S

  val inv : t -> t
end

module type Root = sig
  val root : int64 (* element of multiplicative order N (N = grid size) *)
end

module Zp (P : sig
  val p : int64
end) =
struct
  type t = int64

  let zero = 0L

  (* least non-negative residue *)
  let pos x =
    let v = Int64.rem x P.p in
    if Int64.compare v 0L < 0 then Int64.add v P.p else v

  let ( + ) a b = pos (Int64.add a b)
  let ( * ) a b = pos (Int64.mul a b)

  (* multiplicative inverse by Fermat: a^(p-2) mod p (p prime) *)
  let inv a =
    let rec pow b e acc =
      if e = 0L then acc
      else
        let acc' =
          if Int64.equal (Int64.rem e 2L) 1L then pos (Int64.mul acc b) else acc
        in
        pow (pos (Int64.mul b b)) (Int64.div e 2L) acc'
    in
    assert (not (Int64.equal a 0L));
    pow a (Int64.sub P.p 2L) 1L
end
