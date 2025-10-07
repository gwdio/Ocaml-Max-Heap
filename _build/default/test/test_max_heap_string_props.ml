open QCheck

module H = Max_heap_string

(* -------- helpers -------- *)

(* Build a queue from a list of instructions *)
type instruction = Push of string * int | Pop | Increment

let apply_instruction h = function
  | Push (s,i) -> H.push h s i
  | Pop        -> H.pop h
  | Increment  -> H.increment h

let mk_queue xs =
  List.fold_left apply_instruction H.empty xs

(* Generators similar to HW style *)
let gen_string : string Gen.t =
  let base = "abcdefghijklmnoqrsu$uur#%$@%4rw324ERp*&%" in
  Gen.(int_bound 40 >>= fun len ->
       int_bound (max 0 (String.length base - len)) >>= fun off ->
       return (String.sub base off len))

let gen_priority : int Gen.t = Gen.int_range (-5) 5

let gen_instruction : instruction Gen.t =
  Gen.(oneof
        [ map2 (fun s i -> Push (s,i)) gen_string gen_priority
        ; pure Pop
        ; pure Increment ])

let gen_program : instruction list Gen.t =
  Gen.(int_bound 10 >>= fun n -> list_size (return n) gen_instruction)

(* QCheck convenience constructors *)
let arb_string = make gen_string
let arb_prio   = make gen_priority
let arb_prog   = make gen_program

(* -------- Properties (adapted from homework) -------- *)

(* Property 1 *)
let prop1 =
  Test.make
    ~name:"P1: peek(push empty x i) = Some (x,i)"
    (pair arb_string arb_prio)
    (fun (x,i) -> H.peek (H.push H.empty x i) = Some (x,i))

(* Property 2 *)
let prop2 =
  (* generate an arbitrary starting queue q from a random program *)
  Test.make
    ~name:"P2: size(push(push q x i) y j) == size(push(push q y j) x i)"
    (triple arb_prog arb_string (triple arb_string arb_prio arb_prio))
    (fun (prog, x, (y, i, j)) ->
      let q = mk_queue prog in
      H.size (H.push (H.push q x i) y j)
      =
      H.size (H.push (H.push q y j) x i))

(* Property 3 *)
let prop3 =
  Test.make
    ~name:"P3: two pushes on empty -> peek is higher priority; ties pick earlier"
    (pair (pair arb_string arb_prio) (pair arb_string arb_prio))
    (fun ((x,i),(y,j)) ->
      let q = H.empty in
      H.peek (H.push (H.push q x i) y j)
      =
      if i >= j then Some (x,i) else Some (y,j))

(* Property 4 *)
let prop4 =
  Test.make
    ~name:"P4: peek empty = None"
    unit
    (fun () -> H.peek H.empty = None)

(* Property 5 *)
let prop5 =
  (* increment preserves “nth element by position” *)
  let remove_n q n =
    let rec loop q n =
      if n <= 0 || H.size q = 0 then q else loop (H.pop q) (n-1)
    in loop q n
  in
  Test.make
    ~name:"P5: increment preserves element position order"
    (pair arb_prog (make Gen.(int_range 0 6)))
    (fun (prog, n) ->
      let q  = mk_queue prog in
      let q' = H.increment q in
      match (H.peek (remove_n q n)), (H.peek (remove_n q' n)) with
      | None, None                 -> true
      | Some (x,_), Some (y,_)     -> x = y
      | _                          -> false)

(* Property 6 *)
let prop6 =
  Test.make
    ~name:"P6: size q = size (increment q)"
    arb_prog
    (fun prog ->
      let q = mk_queue prog in
      H.size q = H.size (H.increment q))

(* Property 7 *)
let prop7 =
  Test.make
    ~name:"P7: pop decreases size by 1 when non-empty"
    arb_prog
    (fun prog ->
      let q = mk_queue prog in
      match H.size q with
      | 0 -> H.size q = H.size (H.pop q)
      | _ -> H.size q = H.size (H.pop q) + 1)

(* Property 8 *)
let prop8 =
  (* membership preserved by pushing a (possibly different) pair *)
  let contains v q =
    let rec loop q =
      match H.peek q with
      | None -> false
      | Some x when x = v -> true
      | _ -> loop (H.pop q)
    in loop q
  in
  Test.make
    ~name:"P8: push preserves membership (unless pushing the same pair)"
    (triple arb_prog arb_string (pair arb_string (pair arb_prio arb_prio)) )
    (fun (prog, x, (y,(i,j))) ->
      let q = mk_queue prog in
      if x <> y || i <> j
      then contains (x,i) q = contains (x,i) (H.push q y j)
      else contains (x,i) (H.push q y j))

let () =
  QCheck_base_runner.run_tests_main
    [ prop1; prop2; prop3; prop4; prop5; prop6; prop7; prop8 ]
