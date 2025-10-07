(** max_heap.mli *)

(** A priority-queue interface, parameterized by the element type. *)
module type S = sig
  type key
  type heap

  val empty     : heap
  val push      : heap -> key -> int -> heap
  val peek      : heap -> (key * int) option
  val size      : heap -> int
  val pop       : heap -> heap
  val increment : heap -> heap
end

(** Build a priority-queue implementation for an arbitrary element type. *)
module Make (K : sig type t end) : S with type key = K.t
