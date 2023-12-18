(* Dec 18, 2023 *)

let make_bloom () = Array.make 8192 min_int

let clear_bloom filter =
    Array.fill filter 0 8192 min_int

let check_bloom filter slot =
    let idx, shift = slot lsr 6, slot land 63
    in ((filter.(idx) asr shift) land 1) <> 0

let set_bloom filter slot =
    let idx, shift = slot lsr 6, slot land 63
    in filter.(idx) <- filter.(idx) lor (1 lsl shift)

