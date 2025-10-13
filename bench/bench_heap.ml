(* bench_heap.ml *)
module Heap = struct
  module M = Max_heap.Make(struct type t = int end)
  include M
end

(* ---- GC profiles ---- *)
type gc_profile =
  | Default
  | P8MB   (* 8 MiB minor *)
  | P16MB  (* 16 MiB minor *)
  | P32MB  (* 32 MiB minor *)

let parse_gc_profile () =
  match Sys.getenv_opt "GC_PROFILE" with
  | Some "8m"  -> P8MB
  | Some "16m" -> P16MB
  | Some "32m" -> P32MB
  | _          -> Default

let apply_gc_profile () =
  match parse_gc_profile () with
  | Default -> ()
  | P8MB | P16MB | P32MB as p ->
      let g = Gc.get () in
      let minor =
        match p with
        | P8MB  -> 8  * 1024 * 1024
        | P16MB -> 16 * 1024 * 1024
        | P32MB -> 32 * 1024 * 1024
        | _     -> g.minor_heap_size
      in
      (* space_overhead: higher -> fewer major GCs on alloc-heavy code *)
      Gc.set { g with
        minor_heap_size = minor;
        space_overhead  = 120;
      }

let pp_gc_profile () =
  match parse_gc_profile () with
  | Default -> "default"
  | P8MB    -> "8m"
  | P16MB   -> "16m"
  | P32MB   -> "32m"


let () =
  apply_gc_profile ();         (* set GC *)
  let n      = 200_000 in      (* items per trial *)
  let trials = 5 in

  Random.init 42;

  let time f =
    let t0 = Unix.gettimeofday () in
    let r  = f () in
    let t1 = Unix.gettimeofday () in
    (r, t1 -. t0)
  in

  let run () =
    (* push N (key=int) with random priorities *)
    let rec push_n h i =
      if i = n then h
      else
        let key = Sys.opaque_identity (Random.bits ()) in
        let pri = Sys.opaque_identity (Random.bits ()) in
        push_n (Heap.push h key pri) (i + 1)
    in
    let rec pop_all h acc =
      match Heap.peek h with
      | None -> acc
      | Some (k, p) ->
          let acc' = acc + Sys.opaque_identity k + Sys.opaque_identity p in
          pop_all (Heap.pop h) acc'
    in
    let h0  = Heap.empty in
    let h1  = push_n h0 0 in
    let acc = pop_all h1 0 in
    Sys.opaque_identity acc
  in

  Gc.compact ();
  for k = 1 to trials do
    let gc0 = Gc.quick_stat () in
    let _, secs = time run in
    let gc1 = Gc.quick_stat () in
    Printf.printf
      "trial=%d time=%.4fs minor_words=%.0f major_collections=%d promoted_words=%.0f\n%!"
      k secs
      (gc1.Gc.minor_words -. gc0.Gc.minor_words)
      (gc1.Gc.major_collections - gc0.Gc.major_collections)
      (gc1.Gc.promoted_words -. gc0.Gc.promoted_words)
  done
