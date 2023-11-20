
let permutations =
    let aref = ref [] in
    for w = 0 to 3 do
    for x = 0 to 3 do
    for y = 0 to 3 do
    for z = 0 to 3 do
        if List.sort compare [w; x; y; z] = [0; 1; 2; 3] then
            aref := (Array.of_list @@ List.map (List.nth all_suits) [w; x; y; z]) :: !aref
    done
    done
    done
    done;
    Array.of_list @@ List.rev !aref

let permutation = ref permutations.(0)

let permute_card perm (Card (suit, rank)) =
    Card (perm.(Obj.magic suit), rank)

let permute_hand perm (Hand h) =
    Hand (List.map (permute_card perm) h)

let permute_deal perm (Deal d) =
    Deal {d with d_hands = List.map (fun hand -> pack_hand @@ permute_hand perm @@ unpack_hand hand) d.d_hands}

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
        in analyze_deal (permute_deal !permutation d) counter i;
        Printf.printf "\n"
    done;
    Printf.printf "Saw %d nodes.\n" !counter

let run_single idx =
    let counter = ref 0 in
    for i = 1 to idx do
        let d = new_deal ()
        in if i = idx then
            (Printf.printf "#%d\n" i;
             analyze_deal (permute_deal !permutation d) counter i;
             Printf.printf "\n")
    done;
    Printf.printf "Saw %d nodes.\n" !counter

let rec process_args next_arg =
    match next_arg () with
        | "-perm" -> permutation := permutations.(int_of_string (next_arg ()));
                     process_args next_arg
        | "-order" -> let idx = int_of_string (next_arg ()) in
                      if idx >= 0 then
                          (default_ordering := Array.to_list permutations.(idx);
                           leave_ordering_alone := true);
                      process_args next_arg
        | "-bench" -> let limit = int_of_string (next_arg ())
                      in run_benchmark limit
        | "-single" -> let idx = int_of_string (next_arg ())
                      in run_single idx
        | "-recs" -> report_recs := true;
                     process_args next_arg
        | _ -> Printf.printf "Bad arguments.\n"

let _ =
    let arg_queue = ref (List.tl @@ Array.to_list Sys.argv) in
    let next_arg () = (let arg = List.hd !arg_queue in arg_queue := List.tl !arg_queue; arg) in
    process_args next_arg

