open QCheck

module H = Max_heap_string  (* your functor instantiation: key=string *)

(* ---------- Helpers ---------- *)

type instruction = Push of string * int | Pop | Increment

let apply_instruction h = function
  | Push (s,i) -> H.push h s i
  | Pop        -> H.pop h
  | Increment  -> H.increment h

let mk_queue xs = List.fold_left apply_instruction H.empty xs

(* Slightly “sticky” random strings + small priorities *)
let gen_string : string Gen.t =
  let base = "abcdefghijklmnoqrsu$uur#%$@%4rw324ERp*&%" in
  Gen.(
    int_bound 40 >>= fun len ->
    int_bound (max 0 (String.length base - len)) >>= fun off ->
    return (String.sub base off len)
  )

let gen_priority : int Gen.t = Gen.int_range (-5) 5

let gen_push : instruction Gen.t =
  Gen.map2 (fun s i -> Push (s,i)) gen_string gen_priority

(* Pretty printers so QCheck shows a readable failing program *)
let pp_instruction = function
  | Push (s,i) -> Printf.sprintf "Push(%S,%d)" s i
  | Pop        -> "Pop"
  | Increment  -> "Increment"

let pp_program prog =
  let buf = Buffer.create 128 in
  Buffer.add_string buf "[\n  ";
  List.iteri
    (fun k ins ->
      if k > 0 then Buffer.add_string buf ";\n  ";
      Buffer.add_string buf (pp_instruction ins))
    prog;
  Buffer.add_string buf "\n]";
  Buffer.contents buf

(* ---------- A “spiky” program generator that hits 2^k ± {0,1} sizes ---------- *)

(* lengths around powers of two tend to expose your path-bit logic *)
let interesting_sizes = [1;2;3;4;7;8;9;15;16;17;31;32;33;63;64;65]

let gen_push_block : instruction list Gen.t =
  Gen.(oneofl interesting_sizes >>= fun k ->
       list_size (return k) gen_push)

let gen_tail_ops : instruction list Gen.t =
  (* a few Pops/Increment to mix states and trigger pop paths *)
  Gen.(int_range 0 5 >>= fun m ->
       list_size (return m) (oneof [Gen.return Pop; Gen.return Increment]))

let gen_spiky_program : instruction list Gen.t =
  (* 2–6 blocks of pushes targeting power-of-two boundaries, with little tails *)
  let open Gen in
  int_range 2 6 >>= fun blocks ->
  let rec loop b acc =
    if b = 0 then return (List.concat (List.rev acc))
    else
      gen_push_block >>= fun pushes ->
      gen_tail_ops  >>= fun tails ->
      loop (b - 1) ((pushes @ tails) :: acc)
  in
  loop blocks []

let arb_spiky_program : instruction list arbitrary =
  (* Use default list shrinking so you get small counterexamples *)
  make ~print:pp_program gen_spiky_program

(* ---------- The crash-hunting property ---------- *)

(* Drain the heap to provoke any pop-path bugs *)
let rec drain_all q =
  match H.peek q with
  | None   -> ()
  | Some _ -> drain_all (H.pop q)

let prop_never_raises_on_drain =
  Test.make
    ~name:"CrashFinder: program -> build -> drain (no exceptions)"
    ~count:10_000
    arb_spiky_program
    (fun prog ->
       try
         let q = mk_queue prog in
         drain_all q;
         true
       with exn ->
         (* Fail with the exact program and exception message *)
         QCheck.Test.fail_reportf
           "Raised: %s\nProgram:\n%s"
           (Printexc.to_string exn) (pp_program prog))

(* ---------- Optional: a shape-sanity property (no right child without left) ----------
   If you expose your tree type in tests (e.g., add H._debug_tree : heap -> tree),
   you can assert CBT shape here. Shown as a template only.
*)
(*
let prop_shape_ok =
  Test.make ~name:"Shape: no right child without left" arb_spiky_program (fun prog ->
    let q = mk_queue prog in
    let t = H._debug_tree q in
    no_right_without_left t && heap_order t)
*)
;;

let () =
  QCheck_base_runner.run_tests_main [ prop_never_raises_on_drain ]
