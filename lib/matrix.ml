(* Matrix ring — generic over any base Ring.

   The analog of `impl Ring for Matrix<T>` in Rust: the matrix type
   ITSELF satisfies the Ring signature (zero, (+), ( * )), so matrices
   compose with the same operators as scalars:

     Z.(a + b * c)         (* matrix add / matrix multiply, in scope *)
     mul w (mul x (transpose w))

   Dispatch stays compile-time: Make(R) is a functor application,
   monomorphized like a Rust generic, no vtables.

   Subtlety: Ring.S requires [zero : t] with no size argument, but the
   zero matrix only exists for a given size. So the ring instance is
   the ring of n x n matrices for a FIXED n, provided by [Square]:
   one ring per dimension, all compile-time. *)

module Make (R : Ring.S) = struct
  module Core = struct
    type t = { rows : int; cols : int; data : R.t array }

    let make rows cols f =
      let data = Array.make (rows * cols) R.zero in
      for i = 0 to rows - 1 do
        for j = 0 to cols - 1 do
          data.((i * cols) + j) <- f i j
        done
      done;
      { rows; cols; data }

    let get m i j = m.data.((i * m.cols) + j)
    let transpose m = make m.cols m.rows @@ fun i j -> get m j i

    (* pointwise add — same dimensions required, unchecked by types *)
    let add a b = make a.rows a.cols @@ fun i j -> R.(get a i j + get b i j)

    (* the ring's multiply is matrix multiply *)
    let mul a b =
      make a.rows b.cols @@ fun i j ->
      let s = ref R.zero in
      for k = 0 to a.cols - 1 do
        s := R.(!s + (get a i k * get b k j))
      done;
      !s
  end

  include Core

  (* The ring of Dim.n x Dim.n matrices: [zero] is constant here, so the
     module matches Ring.S exactly. [type t] is [Core.t] — the SAME type,
     not a new one — so Core's operations apply unchanged. *)
  module Square (Dim : sig
    val n : int
  end) : Ring.S = struct
    include Core

    let zero = make Dim.n Dim.n @@ fun _ _ -> R.zero
    let ( + ) = add
    let ( * ) = mul
  end
end
