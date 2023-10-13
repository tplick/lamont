
let analyze_deal deal counter idx =
    print_deal deal;
    List.iter (fun depth ->
      (if depth <= 0 then
        let (value, variation), node_count = evaluate_deal_alpha (ref 0) deal depth
        in Printf.printf "alpha depth %d: value %d, nodes %d\n%!" depth value node_count;
           counter := !counter + node_count))
        [4; 8];
    let (value, variation), node_count = evaluate_deal_gamma_top (ref 0) deal 52 idx
    in (* Printf.printf "gamma depth %d: value %d, nodes %d\n%!" 52 value node_count; *)
       counter := !counter + node_count

let run_benchmark limit =
    let counter = ref 0 in
    for i = 1 to limit do
        Printf.printf "#%d\n" i;
        let d = new_deal ()
        in analyze_deal d counter i;
        Printf.printf "\n"
    done;
    Printf.printf "Saw %d nodes.\n" !counter

let run_single idx =
    let counter = ref 0 in
    for i = 1 to idx do
        let d = new_deal ()
        in if i = idx then
            (Printf.printf "#%d\n" i;
             analyze_deal d counter i;
             Printf.printf "\n")
    done;
    Printf.printf "Saw %d nodes.\n" !counter

let _ =
    let arg_queue = ref (List.tl @@ Array.to_list Sys.argv) in
    let next_arg () = (let arg = List.hd !arg_queue in arg_queue := List.tl !arg_queue; arg) in
    match next_arg () with
        | "-bench" -> let limit = int_of_string (next_arg ())
                      in run_benchmark limit
        | "-single" -> let idx = int_of_string (next_arg ())
                      in run_single idx
        | _ -> Printf.printf "Bad arguments.\n"

