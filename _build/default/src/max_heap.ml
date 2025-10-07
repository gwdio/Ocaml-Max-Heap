(** A priority-queue interface, parameterized by the element type. *)
module type S = sig
  type key
  type heap

  val empty     : heap
  val push      : heap -> key   -> int -> heap
  val peek      : heap -> (key * int) option
  val size      : heap -> int
  val pop       : heap -> heap
  val increment : heap -> heap
end

(** Build a priority-queue implementation for an arbitrary element type. *)
module Make (K : sig type t end) : (S with type key = K.t) = struct
  type key = K.t

  type 'a bintree =
    | Leaf
    | Node of ('a bintree * 'a * 'a bintree)

  type heap = int * (key * int) bintree

  let floor_log2 (n : int) =
    let rec floor_log2' (n : int) (ct : int) : int =
      if n = 1 then ct else floor_log2' (n lsr 1) (ct + 1)
    in
    if n = 0 then failwith "cannot log of 0" else floor_log2' n 0

  let empty : heap = (0, Leaf)

  let push : heap -> key -> int -> heap =
    let rec downheap (sz : int) (s : key) (v : int) (heap : (key * int) bintree)
      : (key * int) bintree =
      match heap with
      | Leaf -> Node (Leaf, (s, v), Leaf)
      | Node (lt, (s', v'), rt) ->
        let t   = floor_log2 sz in
        let bit = (sz - t) lsr (t - 1) land 1 in
        let sz' = (sz - (1 lsl t)) lor (1 lsl (t - 1)) in
        if v' < v then
          if bit = 1 then
            let lt' = downheap sz' s' v' lt in
            Node (lt', (s, v), rt)
          else
            let rt' = downheap sz' s' v' rt in
            Node (lt, (s, v), rt')
        else
          if bit = 1 then
            let lt' = downheap sz' s v lt in
            Node (lt', (s', v'), rt)
          else
            let rt' = downheap sz' s v rt in
            Node (lt, (s', v'), rt')
    in
    fun (heap : heap) (s : key) (k : int) ->
      match heap with
      | x, bt -> (x + 1, downheap (x + 1) s k bt)

  let peek : heap -> (key * int) option =
    fun (heap : heap) ->
      match heap with
      | sz, hp ->
        if sz = 0 then None
        else match hp with
             | Node (_, v, _) -> Some v
             | Leaf -> failwith "size should have been 0"

  let size (heap : heap) = fst heap

  let pop : heap -> heap =
    fun (heap : heap) ->
      if fst heap = 0 then heap
      else
        let rec get_last (sz : int) (heap : (key * int) bintree) : (key * int) =
          match heap with
          | Leaf -> failwith "oops1"
          | Node (Leaf, v, _) -> v
          | Node (lt, _, rt) ->
            let t   = floor_log2 sz in
            let bit = (sz - t) lsr (t - 1) land 1 in
            let sz' = (sz - (1 lsl t)) lor (1 lsl (t - 1)) in
            if bit = 1 then get_last sz' lt else get_last sz' rt
        in
        let rec pop_last (sz : int) (heap : (key * int) bintree)
          : (key * int) bintree =
          match heap with
          | Leaf -> failwith "oops2"
          | Node (Leaf, _, _) -> Leaf
          | Node (lt, v, rt) ->
            let t   = floor_log2 sz in
            let bit = (sz lsr (t - 1)) land 1 in
            let sz' = (sz - (1 lsl t)) lor (1 lsl (t - 1)) in
            if bit = 0 then Node (pop_last sz' lt, v, rt)
            else           Node (lt, v, pop_last sz' rt)
        in
        let rec downheap_swap (heap : (key * int) bintree)
          : (key * int) bintree =
          match heap with
          | Leaf -> failwith "oops3"
          | Node (Leaf, v, Leaf) -> Node (Leaf, v, Leaf)
          | Node (Node (Leaf, (lk, lv), Leaf), (k, v), Leaf) ->
              if v < lv
              then Node (Node (Leaf, (k, v), Leaf), (lk, lv), Leaf)
              else heap
          | Node (Node (llt, (lk, lv), lrt), (k, v), Node (rlt, (rk, rv), rrt)) ->
              if lv < rv then
                if v < rv then
                  Node (Node (llt, (lk, lv), lrt), (rk, rv),
                        downheap_swap (Node (rlt, (k, v), rrt)))
                else heap
              else
                if v < lv then
                  Node (downheap_swap (Node (llt, (k, v), lrt)), (lk, lv),
                        Node (rlt, (rk, rv), rrt))
                else heap
          | Node _ -> failwith "oops4"
        in
        match heap with
        | _, Leaf -> failwith "oops5"
        | 1, _ -> (0, Leaf)
        | x, Node (lt, _, rt) ->
            (x - 1,
             downheap_swap (pop_last x (Node (lt, get_last x (snd heap), rt))))

  let increment : heap -> heap =
    let rec increment_heap (heap : (key * int) bintree) : (key * int) bintree =
      match heap with
      | Leaf -> Leaf
      | Node (lt, (k, v), rt) -> Node (increment_heap lt, (k, v + 1), increment_heap rt)
    in
    fun (heap : heap) ->
      match heap with
      | x, hp -> (x, increment_heap hp)
end
