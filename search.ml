
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
    if without_lowest_bit mask = 0
        then 0
        else
    let bit = ref 0 and
    lower_bound =
        if mask land ((1 lsl 26) - 1) > 0
            then 0
            else 26   and
    upper_bound =
        if mask land ((1 lsl 52) - (1 lsl 26)) > 0
            then 51
            else 25
    in
    for i = lower_bound to upper_bound do
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
        if mine land suit_mask > (partners lor opp1 lor opp2) land suit_mask then
            if without_highest_bit (mine land suit_mask) > (partners lor opp1 lor opp2) land suit_mask then
                if without_highest_bit (without_highest_bit (mine land suit_mask)) > (partners lor opp1 lor opp2) land suit_mask
            then acc + 3
            else acc + 2
            else acc + 1
            else acc + 0)
        0
        all_suit_masks

let recommendation_table = Hashtbl.create 10000

let move_successor_to_front card succs =
    let (a, b) = List.partition (fun succ -> get_last_play succ = Some card) succs
    in a @ b

let get_card_led deal =
    if is_new_trick deal
        then None
        else get_lead deal

let get_suit_led deal =
    match get_card_led deal with
        | Some (Card (suit, _)) -> Some suit
        | None -> None

let bitcount_array =
    let array = Array.make 128 0 in
    array.(0) <- 0;
    for i = 1 to 127 do
        array.(i) <- array.(i/2) + (i land 1)
    done;
    array

let make_canonical_mask_from_scratch pop_suit_mask hand_suit_mask =
    let mask = ref 0 in
    for bit = 12 downto 0 do
        if pop_suit_mask land (1 lsl bit) > 0
            then (mask := 2 * !mask + (if hand_suit_mask land (1 lsl bit) > 0 then 1 else 0))
    done;
    !mask

let canonical_array =
    let array = Array.make (128 * 128) 0 in
    for i = 0 to 127 do
        for j = 0 to 127 do
            array.(i * 128 + j) <- make_canonical_mask_from_scratch i j
        done
    done;
    array

let make_canonical_mask pop_suit_mask hand_suit_mask =
    let low = canonical_array.((pop_suit_mask land 127) * 128 + (hand_suit_mask land 127)) and
        high = canonical_array.((pop_suit_mask lsr 7) * 128 + (hand_suit_mask lsr 7))
    in low + (high lsl bitcount_array.(pop_suit_mask land 127))

(*
let canonical_table =
    assert (make_canonical_mask 7 6 = 6);
    assert (make_canonical_mask 13 4 = 2);
    assert (make_canonical_mask 21 21 = 7);
    assert (make_canonical_mask 254 6 = 3);
    let table = Hashtbl.create 1600000 in
    for pop_suit_mask = 0 to 8191 do
        for hand_suit_mask = 0 to 8191 do
            if pop_suit_mask land hand_suit_mask = hand_suit_mask
                then Hashtbl.add table (pop_suit_mask, hand_suit_mask)
                                       (make_canonical_mask pop_suit_mask hand_suit_mask)
        done
    done;
    table
*)

let canonicalize_hand hand pop_mask =
    let make_mask hand' pop_mask' shift =
        (
            let hand_suit_mask = (hand' lsr shift) land 8191 and
                pop_suit_mask = (pop_mask' lsr shift) land 8191 in
            let canon_suit_mask = make_canonical_mask pop_suit_mask hand_suit_mask in
            canon_suit_mask lsl shift)
    in make_mask hand pop_mask 0 lor
       make_mask hand pop_mask 13 lor
       make_mask hand pop_mask 26 lor
       make_mask hand pop_mask 39

let make_deal_canonical (Deal d as deal) =
    let hands = d.d_hands and
        (PackedHand pop_mask) = all_remaining_packed deal in
    let canon_hands = List.map (fun (PackedHand hand) -> PackedHand (canonicalize_hand hand pop_mask)) hands in
    Deal {d with d_hands = canon_hands}

let get_hands_from_deal (Deal d) = (* d.d_hands *)
    match d.d_hands with
        | [PackedHand a; PackedHand b; PackedHand c; PackedHand d] -> (a, b, c, d)
        | _ -> raise (Failure "impossible")

let deal_for_hash (Deal d as deal) =
    let (ew, ns) = d.d_tricks in
    match (get_hands_from_deal @@ make_deal_canonical deal) with
        | (w, x, y, z) -> (w, x, y, z, d.d_to_move)

let clear_tt tt middle =
    Hashtbl.filter_map_inplace
        (fun k ((_, v) as x) ->
            if v = middle + 1 || -v = -middle + 1 then Some x else None)
    tt;
    if Hashtbl.length tt >= 10000
        then Hashtbl.clear tt
        else ()

let store_value_in_tt tt deal value middle =
   (if Hashtbl.length tt >= 10000
        then clear_tt tt middle);
    Hashtbl.replace tt (deal_for_hash deal) (immediate_value_of_deal deal, value)

let look_up_value_in_tt tt deal middle =
    match Hashtbl.find_opt tt (deal_for_hash deal) with
        | Some (stored_iv, value) ->
            let this_iv = immediate_value_of_deal deal in
            if this_iv <= stored_iv && value < middle
                then Some value
                else
            if this_iv >= stored_iv && value > middle
                then Some value
                else
            None
        | None -> None

let rec get_long_suit_tricks_in_suit mine partner opp1 opp2 =
    if mine > partner && mine > opp1 lor opp2
        then 1 + get_long_suit_tricks_in_suit (without_highest_bit mine)
                                              (without_lowest_bit partner)
                                              (without_lowest_bit opp1)
                                              (without_lowest_bit opp2)
        else
    if mine > 0 && partner > mine && partner > opp1 lor opp2
        then 1 + get_long_suit_tricks_in_suit (without_highest_bit partner)
                                              (without_lowest_bit mine)
                                              (without_lowest_bit opp1)
                                              (without_lowest_bit opp2)
        else
    0

let count_long_suit_tricks_in_hand deal =
    let PackedHand mine = get_packed_hand_to_move deal and
        PackedHand partners = get_partners_packed_hand deal and
        PackedHand opp1 = get_first_opponents_packed_hand deal and
        PackedHand opp2 = get_second_opponents_packed_hand deal in
    List.fold_left (fun acc suit_mask ->
        max acc @@ get_long_suit_tricks_in_suit (mine land suit_mask)
                                                (partners land suit_mask)
                                                (opp1 land suit_mask)
                                                (opp2 land suit_mask))
        0
        all_suit_masks

let get_deep_card (Deal d) =
    let rec get_last_card = function
        | [] -> None
        | [x] -> Some x
        | x :: xs -> get_last_card xs
    in get_last_card d.d_played

let get_top_card (Deal d) =
    match d.d_played with
        | x :: xs -> Some x
        | [] -> None

let extract x = function
    | Some y -> y
    | None -> x

let pull_suit_to_front card_option succs =
    match card_option with
        | None -> succs
        | Some x ->
            let a, b = List.partition (fun succ -> suit_of_card (extract x (get_last_play succ)) = suit_of_card x)
                                      succs
            in a @ b

(* approximate for now *)
let is_top_card_winning (Deal d) =
    match d.d_played with
        | top :: rest ->
            List.for_all (fun card -> suit_of_card card = suit_of_card top && rank_of_card card < rank_of_card top) rest
        | [] -> false

(* approximate for now *)
let is_led_card_winning (Deal d as deal) =
    match get_lead deal, d.d_played with
        | Some lead, played ->
            List.for_all (fun card -> lead = card || (suit_of_card card = suit_of_card lead && rank_of_card card < rank_of_card lead)) played

let does_card_beat a b =
    suit_of_card a = suit_of_card b && rank_of_card a > rank_of_card b

let is_top_card_winning_true (Deal d as deal) =
    match get_lead deal with
        | None -> false
        | Some lead ->
    match d.d_played with
        | top :: rest ->
            does_card_beat top lead &&
            List.for_all (fun card -> card = lead || suit_of_card card <> suit_of_card lead || does_card_beat top card) rest
        | [] -> false

let get_restricted_packed_hand_to_move deal =
    let PackedHand ph = get_packed_hand_to_move deal
    in match get_suit_led deal with
        | None -> PackedHand ph
        | Some suit ->
            let restricted = (ph land (List.nth all_suit_masks (Obj.magic suit)))
            in if restricted = 0 then PackedHand ph else PackedHand restricted



let get_lowest_bit field =
    let rec get_lowest_bit' field acc =
        if field land 1 = 1 then acc else
        if field land 255 = 0 then get_lowest_bit' (field lsr 8) (acc + 8) else
        get_lowest_bit' (field lsr 1) (acc + 1)
    in
    if field = 0
        then None
        else Some (get_lowest_bit' field 0)

let get_highest_bit field =
    let rec get_highest_bit' field acc =
        if field = 1 then acc else
        if field land lnot 255 <> 0 then get_highest_bit' (field lsr 8) (acc + 8) else
        get_highest_bit' (field lsr 1) (acc + 1)
    in
    if field = 0
        then None
        else Some (get_highest_bit' field 0)

let without_bit bit_option field =
    match bit_option with
        | Some bit -> field land lnot (1 lsl bit)
        | None -> field

let play_highest field suit_mask =
    without_bit (get_highest_bit @@ field land suit_mask) field

let play_lowest_if_any field suit_mask =
    without_bit (get_lowest_bit @@ field land suit_mask) field

let lowest_bit_as_field field =
    field land lnot (field - 1)

let two_mask = 1 + 1 lsl 13 + 1 lsl 26 + 1 lsl 39
let fold_mask field =
    let mid = field lor (field lsr 26) in
    let final = mid lor (mid lsr 13) in
    final land 8191
let get_throwaway_bit field =
    let lowest_bit_field = lowest_bit_as_field (fold_mask field) in
    let rank_mask = two_mask * lowest_bit_field in
    get_lowest_bit (field land rank_mask)

let play_lowest_or_any field suit_mask =
    match get_lowest_bit @@ field land suit_mask with
        | Some bit -> without_bit (Some bit) field
        | None -> without_bit (get_throwaway_bit field) field

let can_play_sequential_trick_in_suit mine_all partners_all opp1_all opp2_all suit_mask =
    let mine = mine_all land suit_mask and
        partners = partners_all land suit_mask and
        opp1 = opp1_all land suit_mask and
        opp2 = opp2_all land suit_mask in
    if mine > 0 && mine > partners && mine > (opp1 lor opp2)
        then `Mine
        else
    if mine > 0 && partners > mine && partners > (opp1 lor opp2)
        then `Partner
        else
    `Neither

let rec count_sequential_tricks' mine partners opp1 opp2 suit_mask_list full_mask_list =
    match suit_mask_list with
        | [] -> 0
        | suit_mask :: suit_masks_rest ->
            match can_play_sequential_trick_in_suit mine partners opp1 opp2 suit_mask with
                | `Mine -> 1 + count_sequential_tricks' (play_highest mine suit_mask)
                                                        (play_lowest_or_any partners suit_mask)
                                                        (play_lowest_if_any opp1 suit_mask)
                                                        (play_lowest_if_any opp2 suit_mask)
                                                        full_mask_list
                                                        full_mask_list
                | `Partner -> 1 + count_sequential_tricks' (play_highest partners suit_mask)
                                                           (play_lowest_or_any mine suit_mask)
                                                           (play_lowest_if_any opp1 suit_mask)
                                                           (play_lowest_if_any opp2 suit_mask)
                                                           full_mask_list
                                                           full_mask_list
                | `Neither -> count_sequential_tricks' mine partners opp1 opp2
                                                       suit_masks_rest
                                                       full_mask_list

let count_sequential_tricks deal suit_mask_list =
    let PackedHand mine = get_packed_hand_to_move deal and
        PackedHand partners = get_partners_packed_hand deal and
        PackedHand opp1 = get_first_opponents_packed_hand deal and
        PackedHand opp2 = get_second_opponents_packed_hand deal
    in
    count_sequential_tricks' mine partners opp1 opp2 suit_mask_list suit_mask_list

let count_sequential_tricks_top deal =
    max (count_sequential_tricks deal all_suit_masks)
        (count_sequential_tricks deal all_suit_masks_rev)




let get_next_opponents_packed_hand (Deal d) =
    List.nth d.d_hands ((d.d_to_move + 1) land 3)

let get_previous_opponents_packed_hand (Deal d) =
    List.nth d.d_hands ((d.d_to_move + 3) land 3)

let count_sequential_tricks_for_2nd deal suit_mask_list =
    let PackedHand mine = get_packed_hand_to_move deal and
        PackedHand partners = get_partners_packed_hand deal and
        PackedHand opp1 = get_next_opponents_packed_hand deal and
        PackedHand opp2 = get_previous_opponents_packed_hand deal and
        Some (Card (suit_led, rank_led) as card_led) = get_lead deal in
    let suit_mask = List.nth all_suit_masks (Obj.magic suit_led)
    in
    if (mine land suit_mask) > 0 && (mine land suit_mask) > (partners land suit_mask) &&
            (mine land suit_mask) > (opp1 land suit_mask) && (mine land suit_mask) > (1 lsl index_of_card card_led)
        then 1 + count_sequential_tricks' (play_highest mine suit_mask)
                                          (play_lowest_or_any partners suit_mask)
                                          (play_lowest_if_any opp1 suit_mask)
                                          opp2
                                          suit_mask_list
                                          suit_mask_list
        else
    if (mine land suit_mask) > 0 && (partners land suit_mask) > (mine land suit_mask) &&
            (partners land suit_mask) > (opp1 land suit_mask) && (partners land suit_mask) > (1 lsl index_of_card card_led)
        then 1 + count_sequential_tricks' (play_highest partners suit_mask)
                                          (play_lowest_or_any mine suit_mask)
                                          (play_lowest_if_any opp1 suit_mask)
                                          opp2
                                          suit_mask_list
                                          suit_mask_list
        else
    0

let count_sequential_tricks_for_2nd_top deal =
    max (count_sequential_tricks_for_2nd deal all_suit_masks)
        (count_sequential_tricks_for_2nd deal all_suit_masks_rev)



let rec evaluate_deal_gamma topdepth counter tts (Deal d as deal) depth middle =
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

    match (if depth land 3 = 0
                then look_up_value_in_tt (List.hd tts) deal middle
                else None) with
        | Some x when x <> middle -> (x, [])
        | _ ->

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

    if depth land 3 = 0 && can_return_early iv (depth / 4)
                          (count_long_suit_tricks_in_hand deal) middle
        then (middle + 1, [])
        else

    if depth land 3 = 0 && can_return_early iv (depth / 4)
                          (count_sequential_tricks_top deal)
                          middle
        then (middle + 1, [])
        else

    if depth land 3 = 3 && can_return_early iv ((depth+3) / 4)
                          (count_sequential_tricks_for_2nd_top deal)
                          middle
        then (middle + 1, [])
        else

    let best_value = ref (-1000) and
        best_variation = ref [] in
    let iter_body =
                (fun succ ->
                             if !best_value > middle
                                then (if depth = topdepth && topdepth >= 36 then Printf.printf "X%!")
                                else (if depth = topdepth && topdepth >= 36 then Printf.printf ".%!";
                             let value, variation = evaluate_deal_gamma topdepth counter (List.tl tts) succ (depth - 1)
                                                    (if same_sides_in_deals deal succ then middle else -middle)
                             in let adjusted_value = (if same_sides_in_deals deal succ then value else -value)
                             in if adjusted_value > !best_value
                                    then (best_value := adjusted_value;
                                          best_variation :=
                                 match get_last_play succ with
                                    | Some x -> x :: variation
                                    | None -> variation);
                             ())) and
        recommendation = (if depth land 3 = 1 && depth > 12 then None else Hashtbl.find_opt recommendation_table (get_restricted_packed_hand_to_move deal, (if is_new_trick deal then None else get_top_card deal), get_lead deal, is_top_card_winning_true deal)) in
            (match recommendation with
                | Some card -> iter_body @@ deal_after_playing card deal
                | None -> ());
            (if !best_value < middle || depth = topdepth
                then List.iter (fun succ -> if get_last_play succ <> recommendation then iter_body succ)
                (let sorted_successors = sort_deals_by_last_play @@ successors_of_deal_without_equals deal in
                 match depth land 3 with
                    | 1 -> let (wins, losses) = List.partition (fun succ -> same_sides_in_deals deal succ)
                                                               (List.rev sorted_successors)
                           in wins @ losses
                    | 3 -> let (wins, losses) = List.partition (fun succ -> is_top_card_winning succ || is_led_card_winning succ) @@ List.rev sorted_successors
                           in wins @ losses
                    | 2 -> let (wins, losses) = List.partition is_top_card_winning @@ List.rev sorted_successors
                           in wins @ losses
                    | _ -> List.rev sorted_successors));
    (if depth = topdepth && topdepth >= 36 then Printf.printf "\n%!");
    (if depth land 3 <> 1 || depth <= 12 then
     match !best_variation with
        | x :: _ -> if Some x <> recommendation then Hashtbl.replace recommendation_table (get_restricted_packed_hand_to_move deal, (if is_new_trick deal then None else get_top_card deal), get_lead deal, is_top_card_winning_true deal) x
        | [] -> ());

    let return_value = (!best_value, !best_variation)
    in (if depth land 3 = 0 then store_value_in_tt (List.hd tts) deal !best_value middle);
    return_value

let make_trans_table_tower () =
    let tower = ref [] in
    for i = 0 to 52 do
        tower := (Hashtbl.create 10000) :: !tower
    done;
    !tower

let rec print_ledger opening ledger =
    if opening then Printf.printf "[";
    match ledger with
        | [] -> Printf.printf "]\n"
        | [x] -> Printf.printf "%d]\n%!" x
        | x :: xs -> Printf.printf "%d " x; print_ledger false xs

let evaluate_deal_gamma_top counter deal depth idx =
    Hashtbl.clear recommendation_table;
    let middle = ref 0 and variation = ref [] and
        tower = make_trans_table_tower () and
        ledger = ref [] in
    for d = 1 to depth do
        if d land 3 = 0
            then (if d mod 8 = 0 then List.iter Hashtbl.clear tower;
                 let (new_middle, new_variation) =
                        evaluate_deal_gamma d counter tower deal d !middle
                 in (middle := new_middle; variation := new_variation; ledger := new_middle :: !ledger;
                     Printf.printf "gamma depth %d: value %d, cumul nodes %d, rec table has %d entries\n%!" d new_middle !counter (Hashtbl.length recommendation_table)))
    done;
    Printf.printf "#%d: " (idx);
    print_ledger true @@ List.rev !ledger;
    (!middle, !variation), !counter

