
let lowest_bit_index x =
    let n = x lxor (x - 1) in
    let m = n lxor (n lsr 13) in
    (m land 8191)

let lowest_bit_array_2 =
    let array = Array.make 8192 0 in
    for i = 0 to 25 do
        array.(lowest_bit_index (1 lsl i) <- i
    done;
    array

let fast_lowest_bit_nonzero x =
    let v, n =
        if x land ((1 lsl 26) - 1) <> 0
            then x, 0
            else (x lsr 26), 26
    in n + lowest_bit_array_2.(lowest_bit_index v)

