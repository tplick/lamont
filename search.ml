
let immediate_value_of_deal (Deal d) =
    match d.d_to_move, d.d_tricks with
        | 0, (ew, ns) | 2, (ew, ns) -> ew - ns
        | _, (ew, ns) -> ns - ew

let same_sides_in_deals (Deal a) (Deal b) =
    a.d_to_move land 1 = b.d_to_move land 1


(* This works! *)
let rec evaluate_deal_alpha counter deal depth =
    let is_top = (!counter = 0) in
    incr counter;
    if depth = 0
        then (immediate_value_of_deal deal, []), !counter
        else

    let successors = successors_of_deal deal in
    let deal_values = List.map
                (fun succ -> (if is_top then Printf.printf ".%!");
                             let (value, variation), _ = evaluate_deal_alpha counter succ (depth - 1)
                             in (if same_sides_in_deals deal succ then value else -value),
                                 match get_last_play succ with
                                    | Some x -> x :: variation
                                    | None -> variation)
                successors in
    if is_top then Printf.printf "\n%!";
    List.fold_left max (-1000, []) deal_values, !counter

let highest_card_in_suit suit cards =
    let cards_in_suit = List.filter (fun card -> suit_of_card card = suit) cards
    in
    let sorted_cards_in_suit = List.rev @@ List.sort compare cards_in_suit
    in
    match sorted_cards_in_suit with
        | x :: xs -> Some x
        | [] -> None

let rec first_non_null f list =
    match list with
        | [] -> None
        | x :: xs -> match f x with
            | Some _ as y -> y
            | None -> first_non_null f xs

(* N.B.  This is only valid for notrump as written. *)
let can_side_to_lead_win_a_trick (Deal d as deal) =
    let cards_to_lead = get_playable_cards deal and
        all_cards_in_hands = match d.d_hands with
            | [Hand w; Hand x; Hand y; Hand z] -> w @ x @ y @ z
    in
    first_non_null
        (fun suit -> match highest_card_in_suit suit cards_to_lead,
                           highest_card_in_suit suit all_cards_in_hands with
            | None, None -> None
            | Some _ as x, None -> x
            | None, Some _ -> None
            | Some (Card (_, rank1)) as x, Some (Card (_, rank2)) -> if rank1 = rank2 then x else None)
        (List.rev all_suits)

let cards_of_current_side (Deal d as deal) =
    let p = d.d_to_move in
    match List.nth d.d_hands p, List.nth d.d_hands ((p+2) land 3) with
        | Hand h1, Hand h2 -> h1 @ h2

let cards_of_other_side (Deal d as deal) =
    let p = d.d_to_move in
    match List.nth d.d_hands ((p+1) land 3), List.nth d.d_hands ((p+3) land 3) with
        | Hand h1, Hand h2 -> h1 @ h2

let can_side_win_next_trick (Deal d as deal) =
    let my_cards = get_playable_cards deal and
        our_cards = cards_of_current_side deal and
        their_cards = cards_of_other_side deal
    in
    List.exists (fun suit ->
        match highest_card_in_suit suit our_cards,
              highest_card_in_suit suit their_cards with
            | None, None -> false
            | Some _, None -> true
            | None, Some _ -> false
            | Some x, Some y -> rank_of_card x >= rank_of_card y)
        (List.map suit_of_card my_cards)  (* TODO: remove duplicates from this list *)


let rec evaluate_deal_beta counter deal depth =
    let is_top = (!counter = 0) in
    incr counter;
    if depth = 0
        then (immediate_value_of_deal deal, []), !counter
        else
    match depth = 4, can_side_to_lead_win_a_trick deal with
        | true, Some lead -> (immediate_value_of_deal deal + 1, [lead]), !counter
        | _ -> (

    let successors = successors_of_deal deal and
        deal_values = ref [] and
        values_seen = ref [] in
    List.iter
                (fun succ -> if List.length !values_seen = 2
                                then ()
                                else
                            ((if is_top then Printf.printf ".%!");
                             let (value, variation), _ = evaluate_deal_beta counter succ (depth - 1)
                             in
                             let adj_value, adj_variation =
                                (if same_sides_in_deals deal succ then value else -value),
                                 match get_last_play succ with
                                    | Some x -> x :: variation
                                    | None -> variation
                             in
                             deal_values := (adj_value, adj_variation) :: !deal_values;
                             values_seen := List.sort_uniq compare (adj_value :: !values_seen)))
                successors;
    if is_top then Printf.printf "\n%!";
    List.fold_left max (-1000, []) !deal_values, !counter
)


let rec count_aces = function
    | [] -> 0
    | Card (_, RA) :: xs -> 1 + count_aces xs
    | _ :: xs -> count_aces xs

(* This doesn't.  :(  *)
let rec evaluate_deal_gamma counter deal depth middle =
    incr counter;
    if depth = 0
        then (let iv = immediate_value_of_deal deal
              in if iv > middle then (middle + 1, []) else (middle - 1, []))
        else

    let iv = immediate_value_of_deal deal in
    if is_new_trick deal && iv - (depth / 4) > middle
        then (middle + 1, [])
    else if is_new_trick deal && iv + (depth / 4) < middle
        then (middle - 1, [])
        else

    if depth = 4 && iv = middle
        then (if can_side_win_next_trick deal then (middle + 1, []) else (middle - 1, []))
        else

    if depth = 8 && iv + 2 > middle
                 && count_aces (get_playable_cards deal) >= 2
        then (middle + 1, [])
        else

    let successors = successors_of_deal deal and
        best_value = ref (-1000) and
        best_variation = ref [] in
    List.iter
                (fun succ -> if !best_value > middle
                                then ()
                                else
                             let value, variation = evaluate_deal_gamma counter succ (depth - 1)
                                                    (if same_sides_in_deals deal succ then middle else -middle)
                             in let adjusted_value = (if same_sides_in_deals deal succ then value else -value)
                             in if adjusted_value > !best_value
                                    then (best_value := adjusted_value;
                                          best_variation :=
                                 match get_last_play succ with
                                    | Some x -> x :: variation
                                    | None -> variation);
                             ())
                successors;
    (!best_value, !best_variation)

let evaluate_deal_gamma_top counter deal depth =
    let middle = ref 0 and variation = ref [] in
    for d = 1 to depth do
        if d land 3 = 0
            then let (new_middle, new_variation) = evaluate_deal_gamma counter deal d !middle
                 in (middle := new_middle; variation := new_variation)
    done;
    (!middle, !variation), !counter

