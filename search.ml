
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

(*
let highest_card_in_suit suit cards =
    let cards_in_suit = List.filter (fun card -> suit_of_card card = suit) cards
    in
    let sorted_cards_in_suit = List.rev @@ List.sort compare cards_in_suit
    in
    match sorted_cards_in_suit with
        | x :: xs -> Some x
        | [] -> None
*)

let highest_card_in_suit suit cards =
    let rec highest_card_in_suit' suit' cards' acc =
        match cards', acc with
            | [], _ -> acc
            | x :: xs, None when suit_of_card x = suit' -> highest_card_in_suit' suit' xs (Some x)
            | x :: xs, Some y when suit_of_card x = suit' && rank_of_card x > rank_of_card y
                    -> highest_card_in_suit' suit' xs (Some x)
            | x :: xs, _ -> highest_card_in_suit' suit' xs acc
    in highest_card_in_suit' suit cards None

let rec first_non_null f list =
    match list with
        | [] -> None
        | x :: xs -> match f x with
            | Some _ as y -> y
            | None -> first_non_null f xs

(* N.B.  This is only valid for notrump as written. *)
let can_side_to_lead_win_a_trick (Deal d as deal) =
    let cards_to_lead = get_playable_cards deal and
        all_cards_in_hands = match unpack_hands d.d_hands with
            | [Hand w; Hand x; Hand y; Hand z] -> w @ x @ y @ z
            | _ -> raise (Failure "impossible")
    in
    first_non_null
        (fun suit -> match highest_card_in_suit suit cards_to_lead,
                           highest_card_in_suit suit all_cards_in_hands with
            | None, None -> None
            | Some _ as x, None -> x
            | None, Some _ -> None
            | Some (Card (_, rank1)) as x, Some (Card (_, rank2)) -> if rank1 = rank2 then x else None)
        (List.rev all_suits)

let cards_of_current_side (Deal d) =
    let p = d.d_to_move in
    match unpack_hand @@ List.nth d.d_hands p,
          unpack_hand @@ List.nth d.d_hands ((p+2) land 3) with
        | Hand h1, Hand h2 -> h1 @ h2

let cards_of_next_player (Deal d) =
    let p = d.d_to_move in
    match unpack_hand @@ List.nth d.d_hands ((p+1) land 3) with
        | Hand h1 -> h1

let cards_of_other_side (Deal d) =
    let p = d.d_to_move in
    match unpack_hand @@ List.nth d.d_hands ((p+1) land 3),
          unpack_hand @@ List.nth d.d_hands ((p+3) land 3) with
        | Hand h1, Hand h2 -> h1 @ h2

let get_packed_hand_to_move (Deal d) =
    List.nth d.d_hands d.d_to_move

let get_partners_packed_hand (Deal d) =
    List.nth d.d_hands (d.d_to_move lxor 2)

let get_first_opponents_packed_hand (Deal d) =
    List.nth d.d_hands (d.d_to_move lxor 1)

let get_second_opponents_packed_hand (Deal d) =
    List.nth d.d_hands (d.d_to_move lxor 3)

let get_packed_hand_of_current_side (Deal d) =
    match List.nth d.d_hands d.d_to_move,
          List.nth d.d_hands (d.d_to_move lxor 2) with
        | PackedHand h1, PackedHand h2 -> PackedHand (h1 lor h2)

let get_packed_hand_of_other_side (Deal d) =
    match List.nth d.d_hands ((d.d_to_move+1) land 3),
          List.nth d.d_hands ((d.d_to_move+3) land 3) with
        | PackedHand h1, PackedHand h2 -> PackedHand (h1 lor h2)

let can_side_win_next_trick (Deal d as deal) =
    let (PackedHand my_cards) = get_packed_hand_to_move deal and
        (PackedHand our_cards) = get_packed_hand_of_current_side deal and
        (PackedHand their_cards) = get_packed_hand_of_other_side deal
    in
    List.exists (fun shift ->
        ((my_cards lsr shift) land 8191) > 0 &&
            ((our_cards lsr shift) land 8191 > (their_cards lsr shift) land 8191))
        [0; 13; 26; 39]

let can_2nd_player_win_next_trick (Deal d as deal) =
    match get_lead deal with
        | None -> raise (Failure "impossible")
        | Some (Card (suit, rank_led) as lead)
    ->
    let our_cards = cards_of_current_side deal and
        their_cards = lead :: cards_of_next_player deal
    in
    match highest_card_in_suit suit our_cards,
          highest_card_in_suit suit their_cards with
            | None, None -> false
            | Some _, None -> true
            | None, Some _ -> false
            | Some x, Some y -> rank_of_card x >= rank_of_card y

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


let rec count_top_tricks = function
    | [] -> 0
    | Card (s1, RA) :: Card (s2, RK) :: Card (s3, RQ) :: xs when s1 = s2 && s1 = s3
            -> 3 + count_top_tricks xs
    | Card (s1, RA) :: Card (s2, RK) :: xs when s1 = s2
            -> 2 + count_top_tricks xs
    | Card (_, RA) :: xs -> 1 + count_top_tricks xs
    | _ :: xs -> count_top_tricks xs

let get_partners_cards (Deal d) =
    match unpack_hand @@ List.nth d.d_hands ((d.d_to_move + 2) land 3) with
        | Hand cards -> cards

let is_every_suit_in_cards cards =
    let suits = List.map suit_of_card cards
    in (List.length @@ List.sort_uniq compare suits) = 4

let is_every_suit_in_packed_hand (PackedHand ph) =
    ph land 8191 > 0 && (ph lsr 13) land 8191 > 0 &&
    (ph lsr 26) land 8191 > 0 && (ph lsr 39) land 8191 > 0

let count_top_tricks_in_both_hands deal =
    let my_cards = get_playable_cards deal in
    let mine = count_top_tricks my_cards and
        partners = count_top_tricks (get_partners_cards deal)
    in if partners > mine && is_every_suit_in_packed_hand (get_packed_hand_to_move deal)
            then partners
            else mine

let rec count_aces_in_cards = function
    | [] -> 0
    | Card (_, RA) :: xs -> 1 + count_aces_in_cards xs
    | _ :: xs -> count_aces_in_cards xs

let count_aces_in_packed_hand (PackedHand ph) =
    ((ph lsr 12) land 1) + ((ph lsr 25) land 1) +
    ((ph lsr 38) land 1) + ((ph lsr 51) land 1)

let count_ace_tricks_between_hands deal =
    let my_cards = get_packed_hand_to_move deal and
        partners_cards = get_partners_packed_hand deal
    in if is_every_suit_in_packed_hand my_cards && is_every_suit_in_packed_hand partners_cards
        then count_aces_in_packed_hand (get_packed_hand_of_current_side deal)
        else 0

let count_top_tricks_in_packed_hand_to_move deal =
    let PackedHand mine = get_packed_hand_to_move deal and
        PackedHand partners = get_partners_packed_hand deal and
        PackedHand opp1 = get_first_opponents_packed_hand deal and
        PackedHand opp2 = get_second_opponents_packed_hand deal
    in List.fold_left (fun acc suit_mask ->
        if mine land suit_mask > (partners lor opp1 lor opp2) land suit_mask
            then acc + 1
            else acc)
        0
        all_suit_masks

let cards_in_suit suit cards =
    List.filter (fun card -> suit_of_card card = suit) cards

let count_top_tricks_across_hands_in_suit deal suit =
    let my_cards = cards_in_suit suit (get_playable_cards deal) and
        partners_cards = cards_in_suit suit (get_partners_cards deal) in
    let top_tricks = count_top_tricks (List.rev @@ List.sort compare (my_cards @ partners_cards))
    in
    min top_tricks (min (List.length my_cards) (List.length partners_cards))

let count_top_tricks_across_hands deal =
    List.fold_left (fun acc suit ->
            acc + count_top_tricks_across_hands_in_suit deal suit) 0 all_suits

let sort_deals_by_last_play deals =
    let comp (Deal d1) (Deal d2) = match d1.d_last_play, d2.d_last_play with
        | Some x, Some y -> (Obj.magic @@ rank_of_card y) - (Obj.magic @@ rank_of_card x)
        | _, _ -> 0
    in List.sort comp deals

let rec remove_equals_from_successors succs =
    match succs with
        | [] | [_] -> succs
        | x :: y :: rest ->
            match get_last_play x, get_last_play y with
                | Some cx, Some cy when are_cards_adjacent cx cy -> remove_equals_from_successors (y :: rest)
                | _ -> x :: remove_equals_from_successors (y :: rest)

let rec maximum : int list -> int = function
    | [] -> raise (Failure "maximum of empty list")
    | [x] -> x
    | x :: xs -> let y = maximum xs in if x >= y then x else y

let without_lowest_bit mask =
    mask land (mask - 1)

let without_highest_bit mask =
    if mask = 0
        then 0
        else
    let bit = ref 0 in
    for i = 0 to 51 do
        if mask land (1 lsl i) > 0
            then bit := i
    done;
    mask lxor (1 lsl !bit)

let count_bits mask =
    let count = ref 0 in
    for i = 0 to 51 do
        if mask land (1 lsl i) > 0
            then incr count
    done;
    !count

let rec count_iter_tricks_in_suit mine partner opp1 opp2 =
    if mine = 0 || partner = 0
        then 0
        else
    if mine > partner lor opp1 lor opp2
        then 1 + count_iter_tricks_in_suit (without_highest_bit mine)
                                           (without_lowest_bit partner)
                                           (without_lowest_bit opp1)
                                           (without_lowest_bit opp2)
        else
    if partner > mine lor opp1 lor opp2
        then 1 + count_iter_tricks_in_suit (without_highest_bit partner)
                                           (without_lowest_bit mine)
                                           (without_lowest_bit opp1)
                                           (without_lowest_bit opp2)
        else
    0

let count_iter_tricks deal =
    let PackedHand mine = get_packed_hand_to_move deal and
        PackedHand partners = get_partners_packed_hand deal and
        PackedHand opp1 = get_first_opponents_packed_hand deal and
        PackedHand opp2 = get_second_opponents_packed_hand deal
    in List.fold_left (fun acc suit_mask ->
        acc + count_iter_tricks_in_suit (mine land suit_mask)
                                        (partners land suit_mask)
                                        (opp1 land suit_mask)
                                        (opp2 land suit_mask))
        0
        all_suit_masks

let count_iter_tricks_as_2nd_hand deal =
    match get_lead deal with
        | None -> raise (Failure "impossible")
        | Some lead ->
    let PackedHand mine = get_packed_hand_to_move deal and
        PackedHand partners = get_partners_packed_hand deal and
        PackedHand opp1 = get_first_opponents_packed_hand deal and
        PackedHand opp2 = get_second_opponents_packed_hand deal and
        lead_mask = 1 lsl index_of_card lead and
        lead_suit_mask = List.nth all_suit_masks (Obj.magic @@ suit_of_card lead) in
    if mine land lead_suit_mask > 0 && partners land lead_suit_mask > 0 &&
                (mine lor partners) land lead_suit_mask > (opp1 lor opp2 lor lead_mask) land lead_suit_mask
        then List.fold_left (fun acc suit_mask ->
            acc + count_iter_tricks_in_suit (mine land suit_mask)
                                            (partners land suit_mask)
                                            ((opp1 lor lead_mask) land suit_mask)
                                            ((opp2 lor lead_mask) land suit_mask))
            0
            all_suit_masks
        else 0

let can_return_early iv tricks_left reeled_off middle =
    let capped_reel = min tricks_left reeled_off
    in iv + capped_reel - (tricks_left - capped_reel) > middle

let count_top_tricks_in_hand deal =
    let PackedHand mine = get_packed_hand_to_move deal and
        PackedHand partners = get_partners_packed_hand deal and
        PackedHand opp1 = get_first_opponents_packed_hand deal and
        PackedHand opp2 = get_second_opponents_packed_hand deal in
    List.fold_left (fun acc suit_mask ->
        if (mine land suit_mask) > (partners lor opp1 lor opp2) land suit_mask &&
                count_bits (partners land suit_mask) <= 1 &&
                count_bits (opp1 land suit_mask) <= 1 &&
                count_bits (opp2 land suit_mask) <= 1
            then acc + count_bits (mine land suit_mask)
            else
        if without_highest_bit (without_highest_bit (mine land suit_mask)) > (partners lor opp1 lor opp2) land suit_mask
            then acc + 3
            else
        if without_highest_bit (mine land suit_mask) > (partners lor opp1 lor opp2) land suit_mask
            then acc + 2
            else
        if mine land suit_mask > (partners lor opp1 lor opp2) land suit_mask
            then acc + 1
            else acc + 0)
        0
        all_suit_masks

let recommendation_table = Hashtbl.create 10000

let move_successor_to_front card succs =
    let (a, b) = List.partition (fun succ -> get_last_play succ = Some card) succs
    in a @ b

let rec evaluate_deal_gamma topdepth counter deal depth middle =
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

    if depth land 3 = 0 && iv - (depth / 4) = middle - 1
                        && can_side_win_next_trick deal
        then (middle + 1, [])
        else

    if depth land 3 = 3 && iv - ((depth + 1) / 4) = middle - 1
                        && can_2nd_player_win_next_trick deal
        then (middle + 1, [])
        else

    if depth land 3 = 0 && iv + (depth / 4) = middle + 1
                        && not @@ can_side_win_next_trick deal
        then (middle - 1, [])
        else
(*
    if depth land 3 = 0 && iv + (depth / 4) > middle
                        && max (count_top_tricks_in_both_hands deal)
                               (count_ace_tricks_between_hands deal) >= depth / 4
        then (middle + 1, [])
        else
*)

    if depth land 3 = 0 && can_return_early iv (depth / 4) (max (count_top_tricks_in_both_hands deal)
                               (count_ace_tricks_between_hands deal)) middle
        then (middle + 1, [])
        else

    if depth land 3 = 0 && can_return_early iv (depth / 4)
                          (count_iter_tricks deal) middle
        then (middle + 1, [])
        else

    if depth land 3 = 0 && can_return_early iv (depth / 4)
                          (count_top_tricks_in_hand deal) middle
        then (middle + 1, [])
        else

    let successors = successors_of_deal_without_equals deal and
        best_value = ref (-1000) and
        best_variation = ref [] in
    let sorted_successors = sort_deals_by_last_play successors in
    List.iter
                (fun succ ->
                             if !best_value > middle
                                then (if depth = topdepth then Printf.printf "X%!")
                                else (if depth = topdepth then Printf.printf ".%!";
                             let value, variation = evaluate_deal_gamma topdepth counter succ (depth - 1)
                                                    (if same_sides_in_deals deal succ then middle else -middle)
                             in let adjusted_value = (if same_sides_in_deals deal succ then value else -value)
                             in if adjusted_value > !best_value
                                    then (best_value := adjusted_value;
                                          best_variation :=
                                 match get_last_play succ with
                                    | Some x -> x :: variation
                                    | None -> variation);
                             ()))
                (match depth land 3 with
                    | 0 -> (match Hashtbl.find_opt recommendation_table (get_packed_hand_to_move deal) with
                             | Some card -> move_successor_to_front card sorted_successors
                             | None -> sorted_successors)
                    | 1 -> let (wins, losses) = List.partition (fun succ -> same_sides_in_deals deal succ)
                                                               (List.rev sorted_successors)
                           in wins @ losses
                    | _ -> sorted_successors);
    (if depth = topdepth then Printf.printf "\n%!");
    (match !best_variation with
        | x :: _ -> Hashtbl.replace recommendation_table (get_packed_hand_to_move deal) x
        | [] -> ());
    (!best_value, !best_variation)

let evaluate_deal_gamma_top counter deal depth =
    let middle = ref 0 and variation = ref [] in
    for d = 1 to depth do
        if d land 3 = 0
            then let (new_middle, new_variation) = evaluate_deal_gamma depth counter deal d !middle
                 in (middle := new_middle; variation := new_variation)
    done;
    (!middle, !variation), !counter

