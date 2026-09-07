(* Matrix functor — generic over any Ring.

   The analog of `struct<T: Ring>` in Rust: one implementation,
   instantiated per ring, all dispatch resolved at compile time. *)

module Make (R : Ring.S) = struct
  type t = { rows : int; cols : int; data : R.t array }

  let make rows cols f =
    let data = Array.make (rows * cols) R.zero in
    for i = 0 to rows - 1 do
      for j = 0 to cols - 1 do
        data.(i * cols + j) <- f i j
      done
    done;
    { rows; cols; data }

  let get m i j = m.data.(i * m.cols + j)

  let mul a b =
    make a.rows b.cols @@ fun i j ->
    let s = ref R.zero in
    for k = 0 to a.cols - 1 do
      s := R.( !s + (get a i k * get b k j) )
    done;
    !s

  let transpose m = make m.cols m.rows @@ fun i j -> get m j i
end
