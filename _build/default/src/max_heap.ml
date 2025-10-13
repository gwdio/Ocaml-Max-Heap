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

  (* FLATTENED NODE: lt, k, v, rt *)
  type bintree =
    | Leaf
    | Node of bintree * key * int * bintree

  (* heap now holds the flattened tree *)
  type heap = int * bintree

  let floor_log2 (n : int) =
    let rec floor_log2' (n : int) (ct : int) : int =
      if n = 1 then ct else floor_log2' (n lsr 1) (ct + 1)
    in
    if n = 0 then failwith "cannot log of 0" else floor_log2' n 0

  let empty : heap = (0, Leaf)

  (* direction for the next step (excluding the topmost 1) *)
  let step_right idx i = ((idx lsr (i - 1)) land 1) = 1

  let downheap (sz : int) (s : key) (v : int) (t : bintree) : bintree =
    let rec go node k v i =
      match node with
      | Leaf -> Node (Leaf, k, v, Leaf)
      | Node (l, k', v', r) ->
        if v' < v then
          if step_right sz i
            then Node (l, k, v, go r k' v' (i - 1))
            else Node (go l k' v' (i - 1), k, v, r)
        else
          if step_right sz i
            then Node (l, k', v', go r k v (i - 1))
            else Node (go l k v (i - 1), k', v', r)
    in
    go t s v (floor_log2 sz)


  let push : heap -> key -> int -> heap =
    fun (h : heap) (s : key) (k : int) ->
      match h with
      | x, bt -> (x + 1, downheap (x + 1) s k bt)

  let peek : heap -> (key * int) option =
    fun (h : heap) ->
      match h with
      | sz, hp ->
        if sz = 0 then None
        else match hp with
             | Node (_, k, v, _) -> Some (k, v)
             | Leaf -> failwith "size should have been 0"

  let size (h : heap) = fst h

  (* depth-driven walk over flattened nodes *)
  let get_last (sz:int) (tree:bintree) : (key * int) =
    let rec go node i =
      match node, i with
      | Leaf, _ -> failwith "oops1"
      | Node (_, k, v, _), 0 -> (k, v)
      | Node (l, _, _, r), _ ->
          if step_right sz i then go r (i - 1) else go l (i - 1)
    in
    go tree (floor_log2 sz)

  let pop_last (sz:int) (tree:bintree) : bintree =
    let rec go node i =
      match node, i with
      | Leaf, _ -> failwith "oops2"
      | Node (_, _, _, _), 0 -> Leaf
      | Node (l, k, v, r), _ ->
          if step_right sz i
          then Node (l, k, v, go r (i - 1))
          else Node (go l (i - 1), k, v, r)
    in
    go tree (floor_log2 sz)

  let pop : heap -> heap =
    fun (h : heap) ->
      if fst h = 0 then h
      else
        let rec downheap_swap (t : bintree) : bintree =
          match t with
          | Leaf -> failwith "oops3"
          | Node (Leaf, _, _, Leaf) -> t
          | Node (Node (Leaf, lk, lv, Leaf), k, v, Leaf) ->
              if v < lv
              then Node (Node (Leaf, k, v, Leaf), lk, lv, Leaf)
              else t
          | Node (Node (llt, lk, lv, lrt), k, v, Node (rlt, rk, rv, rrt)) ->
              if lv < rv then
                if v < rv then
                  Node (Node (llt, lk, lv, lrt), rk, rv,
                        downheap_swap (Node (rlt, k, v, rrt)))
                else t
              else
                if v < lv then
                  Node (downheap_swap (Node (llt, k, v, lrt)), lk, lv,
                        Node (rlt, rk, rv, rrt))
                else t
          | Node _ -> failwith "oops4"
        in
        match h with
        | _, Leaf -> failwith "oops5"
        | 1, _ -> (0, Leaf)
        | x, Node (lt, _, _, rt) ->
            let last   = get_last x (snd h) in
            let pruned = match last with
              | (k, v) -> pop_last x (Node (lt, k, v, rt))
            in
            (x - 1, downheap_swap pruned)

  let increment : heap -> heap =
    let rec inc (t : bintree) : bintree =
      match t with
      | Leaf -> Leaf
      | Node (lt, k, v, rt) -> Node (inc lt, k, v + 1, inc rt)
    in
    fun (h : heap) ->
      match h with
      | x, hp -> (x, inc hp)
end
