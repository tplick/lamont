
let fast_lowest_bit_index x =
    let n = x lxor (x - 1) in
    let m = n lxor (n lsr 26) in
    let y = m + (m lsl 1) in
    let z = y - (m lsr 13) in
    (z land 8191)
[@@inline]

let fast_lowest_bit_array =
    let array = Array.make 8192 0 in
    for i = 0 to 51 do
        array.(fast_lowest_bit_index (1 lsl i)) <- i
    done;
    array

let fast_lowest_bit_nonzero x =
    fast_lowest_bit_array.(fast_lowest_bit_index x)
[@@inline]

let () =
    for i = 0 to 51 do
        assert (fast_lowest_bit_nonzero (1 lsl i) = i)
    done

