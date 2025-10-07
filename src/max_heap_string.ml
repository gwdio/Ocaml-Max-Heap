module T = struct
  type t = string
end

include Max_heap.Make (T)
