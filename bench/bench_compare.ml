module Pers = struct
  module M = Max_heap.Make(struct type t = int end)
  include M
end

module ArrHeap : sig
  type heap
  val create : int -> heap
  val size   : heap -> int
  val peek   : heap -> (int * int) option
  val push   : heap -> int -> int -> unit
  val pop    : heap -> unit
end = struct
  type heap = { mutable size:int; mutable data:(int*int) array }

  let create cap =
    let cap = max 1 cap in
    { size = 0; data = Array.make cap (0, min_int) }

  let size h = h.size
  let swap a i j =
    let tmp = a.(i) in a.(i) <- a.(j); a.(j) <- tmp

  let ensure_cap h =
    if h.size = Array.length h.data then begin
      let n = h.size in
      let bigger = Array.make (n * 2) (0, min_int) in
      Array.blit h.data 0 bigger 0 n;
      h.data <- bigger
    end

  let sift_up a i =
    let rec loop i =
      if i = 0 then ()
      else
        let p = (i - 1) / 2 in
        if snd a.(p) < snd a.(i) then (swap a p i; loop p) else ()
    in loop i

  let sift_down a sz i =
    let rec loop i =
      let l = 2*i + 1 in
      let r = l + 1 in
      if l >= sz then () else
      let largest =
        let li = l in
        let ri = if r < sz && snd a.(r) > snd a.(l) then r else l in
        if snd a.(ri) > snd a.(i) then ri else i
      in
      if largest <> i then (swap a i largest; loop largest) else ()
    in loop i

  let push h k v =
    ensure_cap h;
    let i = h.size in
    h.data.(i) <- (k, v);
    h.size <- i + 1;
    sift_up h.data i

  let peek h =
    if h.size = 0 then None else Some h.data.(0)

  let pop h =
    match h.size with
    | 0 -> ()
    | 1 -> h.size <- 0
    | n ->
        swap h.data 0 (n - 1);
        h.size <- n - 1;
        sift_down h.data h.size 0
end

(* ----- benchmarking harness ----- *)
let () =
  (* GC profile via env GC_PROFILE=8m|16m|32m *)
  let apply_gc () =
    let g = Gc.get () in
    let minor =
      match Sys.getenv_opt "GC_PROFILE" with
      | Some "8m"  -> 8  * 1024 * 1024
      | Some "16m" -> 16 * 1024 * 1024
      | Some "32m" -> 32 * 1024 * 1024
      | _ -> g.minor_heap_size
    in
    let space = (match Sys.getenv_opt "GC_SPACE" with Some s -> int_of_string s | None -> 120) in
    Gc.set { g with minor_heap_size = minor; space_overhead = space }
  in
  apply_gc ();

  let n      = 200_000 in
  let trials = 5 in
  let seed   = 42 in
  Random.init seed;

  (* pre-generate the exact (key,prio) sequence once for fairness *)
  let ks = Array.init n (fun _ -> Random.bits ()) in
  let ps = Array.init n (fun _ -> Random.bits ()) in

  let now () = Unix.gettimeofday () in
  let time f =
    let g0 = Gc.quick_stat () in
    let t0 = now () in
    let r  = f () in
    let t1 = now () in
    let g1 = Gc.quick_stat () in
    let dt = t1 -. t0 in
    let d_minor = g1.Gc.minor_words -. g0.Gc.minor_words in
    let d_prom  = g1.Gc.promoted_words -. g0.Gc.promoted_words in
    let d_major = g1.Gc.major_collections - g0.Gc.major_collections in
    (r, dt, d_minor, d_major, d_prom)
  in

  let run_persistent () =
    let rec push_all h i =
      if i = n then h
      else push_all (Pers.push h ks.(i) ps.(i)) (i + 1)
    in
    let rec pop_all h acc =
      match Pers.peek h with
      | None -> acc
      | Some (k, v) -> pop_all (Pers.pop h) (acc + k + v)
    in
    let h0 = Pers.empty in
    let h1 = push_all h0 0 in
    pop_all h1 0 |> Sys.opaque_identity |> ignore
  in

  let run_array () =
    let h = ArrHeap.create 1 in
    for i = 0 to n - 1 do ArrHeap.push h ks.(i) ps.(i) done;
    let acc = ref 0 in
    while ArrHeap.size h > 0 do
      match ArrHeap.peek h with
      | None -> ()
      | Some (k, v) -> acc := !acc + k + v; ArrHeap.pop h
    done;
    Sys.opaque_identity !acc |> ignore
  in

  Gc.compact ();
  for k = 1 to trials do
    let _, t1, m1, maj1, p1 = time run_persistent in
    Printf.printf "trial=%d kind=persistent n=%d time=%.4fs minor_words=%.0f major_collections=%d promoted_words=%.0f\n%!"
      k n t1 m1 maj1 p1;
    let _, t2, m2, maj2, p2 = time run_array in
    Printf.printf "trial=%d kind=array      n=%d time=%.4fs minor_words=%.0f major_collections=%d promoted_words=%.0f\n%!"
      k n t2 m2 maj2 p2;
  done
