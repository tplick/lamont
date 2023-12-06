
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

let all_cards_with_suit suit = List.filter (fun card -> suit_of_card card = suit) new_deck

let easiest_deal = new_deal_with_hands
    [Hand (all_cards_with_suit Club);
     Hand (all_cards_with_suit Diamond);
     Hand (all_cards_with_suit Heart);
     Hand (all_cards_with_suit Spade)]

let hard_deal_1 =
    let diagonal_cards n = List.filter (fun (Card (suit, rank)) ->
                                            (Obj.magic rank - Obj.magic suit) land 3 = n)
                                       new_deck in
    new_deal_with_hands
        [Hand (diagonal_cards 0);
         Hand (diagonal_cards 1);
         Hand (diagonal_cards 2);
         Hand (diagonal_cards 3)]

let hard_deal_2 =
    let diagonal_cards n = List.filter (fun (Card (suit, rank)) ->
                                            (Obj.magic rank + Obj.magic suit) land 3 = n)
                                       new_deck in
    new_deal_with_hands
        [Hand (diagonal_cards 0);
         Hand (diagonal_cards 1);
         Hand (diagonal_cards 2);
         Hand (diagonal_cards 3)]

let hardest_deal =
    let shift_suit suit n : suit =
        Obj.magic @@ (Obj.magic suit + n) land 3
    in let shift cards n =
        List.map (fun (Card (suit, rank)) ->
                    Card (shift_suit suit n, rank)) cards
    and first_hand = [
        Card (Club, R2); Card (Diamond, R3); Card (Heart, R4);
        Card (Club, R5); Card (Diamond, R6); Card (Heart, R7);
        Card (Club, R8); Card (Diamond, R9); Card (Heart, R10);
        Card (Club, RJ); Card (Diamond, RQ); Card (Heart, RK);
        Card (Club, RA)
    ]
    in new_deal_with_hands
        [Hand first_hand;
         Hand (shift first_hand 1);
         Hand (shift first_hand 2);
         Hand (shift first_hand 3)]

let run_constructed_deal d =
    let counter = ref 0 in
    analyze_deal (permute_deal !permutation d) counter 0;
    Printf.printf "\n";
    Printf.printf "Saw %d nodes.\n" !counter

let rec process_args next_arg =
    let next () = process_args next_arg
    in match next_arg () with
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
        | "-rechistory" -> report_rec_history := true;
                           process_args next_arg
        | "-show-runs" -> show_missed_runs := true;
                          process_args next_arg
        | "-easiest" -> run_constructed_deal easiest_deal
        | "-hard1" -> run_constructed_deal hard_deal_1
        | "-hard2" -> run_constructed_deal hard_deal_2
        | "-hardest" -> run_constructed_deal hardest_deal
        | "-verify" -> opt := false;
                       next ()
        | _ -> Printf.printf "Bad arguments.\n"

let _ =
    let arg_queue = ref (List.tl @@ Array.to_list Sys.argv) in
    let next_arg () = (let arg = List.hd !arg_queue in arg_queue := List.tl !arg_queue; arg) in
    process_args next_arg;
    if !report_rec_history then print_recom_history ()

