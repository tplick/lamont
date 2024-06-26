
let report_recs = ref false
let report_rec_history = ref false
let show_missed_runs = ref false
let opt = ref true
let show_progress = ref true

let int_min (x : int) (y : int) = if x <= y then x else y
let int_max (x : int) (y : int) = if x >= y then x else y

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
    match unpack_hand @@ tuple_nth d.d_hands p,
          unpack_hand @@ tuple_nth d.d_hands ((p+2) land 3) with
        | Hand h1, Hand h2 -> h1 @ h2

let cards_of_next_player (Deal d) =
    let p = d.d_to_move in
    match unpack_hand @@ tuple_nth d.d_hands ((p+1) land 3) with
        | Hand h1 -> h1

let cards_of_other_side (Deal d) =
    let p = d.d_to_move in
    match unpack_hand @@ tuple_nth d.d_hands ((p+1) land 3),
          unpack_hand @@ tuple_nth d.d_hands ((p+3) land 3) with
        | Hand h1, Hand h2 -> h1 @ h2

let get_packed_hand_to_move (Deal d) =
    tuple_nth d.d_hands d.d_to_move

let get_partners_packed_hand (Deal d) =
    tuple_nth d.d_hands (d.d_to_move lxor 2)

let get_first_opponents_packed_hand (Deal d) =
    tuple_nth d.d_hands (d.d_to_move lxor 1)

let get_second_opponents_packed_hand (Deal d) =
    tuple_nth d.d_hands (d.d_to_move lxor 3)

let get_packed_hand_of_current_side (Deal d) =
    match tuple_nth d.d_hands d.d_to_move,
          tuple_nth d.d_hands (d.d_to_move lxor 2) with
        | PackedHand h1, PackedHand h2 -> PackedHand (h1 lor h2)

let get_packed_hand_of_other_side (Deal d) =
    match tuple_nth d.d_hands ((d.d_to_move+1) land 3),
          tuple_nth d.d_hands ((d.d_to_move+3) land 3) with
        | PackedHand h1, PackedHand h2 -> PackedHand (h1 lor h2)

let get_packed_hand_of_next_player (Deal d) =
    tuple_nth d.d_hands ((d.d_to_move + 1) land 3)

let get_packed_hand_of_previous_player (Deal d) =
    tuple_nth d.d_hands ((d.d_to_move + 3) land 3)

let can_side_win_next_trick (Deal d as deal) =
    let (PackedHand my_cards) = get_packed_hand_to_move deal and
        (PackedHand our_cards) = get_packed_hand_of_current_side deal and
        (PackedHand their_cards) = get_packed_hand_of_other_side deal
    in
    List.exists (fun shift ->
        ((my_cards lsr shift) land 8191) > 0 &&
            ((our_cards lsr shift) land 8191 > (their_cards lsr shift) land 8191))
        [0; 13; 26; 39]

let highest_bit_array =
    let array = Array.make 256 (-1) in
    for i = 1 to 255 do
        array.(i) <- array.(i / 2) + 1
    done;
    array

let get_highest_bit field =
    let rec get_highest_bit' field acc =
        if field = 1 then acc else
        if field land lnot 255 <> 0 then get_highest_bit' (field lsr 8) (acc + 8) else
        highest_bit_array.(field) + acc
    in
    if field = 0
        then None
        else Some (get_highest_bit' field 0)

let highest_card_in_suit_packed suit their_cards lead_option =
    let PackedHand card_mask = their_cards in
    let highest_in_hand = (card_from_index_option @@ get_highest_bit (card_mask land mask_for_suit suit)) in
    (* max highest_in_hand lead_option *)
    match highest_in_hand, lead_option with
        | None, Some _   -> lead_option
        | Some _, None   -> highest_in_hand
        | Some x, Some y -> if rank_of_card x >= rank_of_card y
                                then highest_in_hand
                                else lead_option
        | None, None -> None

let can_2nd_player_win_next_trick (Deal d as deal) =
    match get_lead deal with
        | None -> raise (Failure "impossible")
        | Some (Card (suit, rank_led) as lead)
    ->
    let our_cards = get_packed_hand_of_current_side deal and
        their_cards = get_packed_hand_of_next_player deal
    in
    match highest_card_in_suit_packed suit our_cards None,
          highest_card_in_suit_packed suit their_cards (Some lead) with
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
    match unpack_hand @@ tuple_nth d.d_hands ((d.d_to_move + 2) land 3) with
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
    let mine, opp1, partners, opp2 = get_packed_hands_in_circle_from_current deal in
    List.fold_left (fun acc suit_mask ->
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
    int_min top_tricks (int_min (List.length my_cards) (List.length partners_cards))

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

(*
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
*)

let rec highest_bit_as_field field_0 =
    let field = ref field_0 and wol = ref (without_lowest_bit field_0)
    in
    while !wol <> 0 do
        field := !wol;
        wol := without_lowest_bit !wol
    done;
    !field
[@@inline]

let without_highest_bit mask =
    mask - highest_bit_as_field mask

(*
let count_bits mask =
    let count = ref 0 in
    for i = 0 to 51 do
        if mask land (1 lsl i) > 0
            then incr count
    done;
    !count
*)

let count_bits x =
    let count = ref 0 and field = ref x
    in
    while !field <> 0 do
        field := without_lowest_bit !field;
        incr count
    done;
    !count

let count_bits_alt = count_bits

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
    let mine, opp1, partners, opp2 = get_packed_hands_in_circle_from_current deal in
    List.fold_left (fun acc suit_mask ->
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
    let mine, opp1, partners, opp2 = get_packed_hands_in_circle_from_current deal and
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
    let capped_reel = int_min tricks_left reeled_off
    in iv + capped_reel - (tricks_left - capped_reel) > middle

let count_top_tricks_in_hand deal =
    let mine, opp1, partners, opp2 = get_packed_hands_in_circle_from_current deal in
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

let are_card_options_equal a b =
    match a, b with
        | Some (Card (suit_1, rank_1)), Some (Card (suit_2, rank_2)) -> rank_1 = rank_2 && suit_1 = suit_2
        | None, None -> true
        | _, _ -> false

let are_card_options_unequal a b =
    not @@ are_card_options_equal a b
[@@inline]

module RecHashtbl = Hashtbl.Make (struct
    type t = packed_hand * int
    let equal (PackedHand a, b) (PackedHand u, v) =
        let (===) (s : int) (t : int) = (s = t)
        in b === v && a === u
    let hash = Hashtbl.hash
end)

let recommendation_table = RecHashtbl.create 10000
let recommendation_history = Hashtbl.create 10000
let suit_ordering = Array.make 4 all_suits
let default_ordering = ref all_suits
let leave_ordering_alone = ref false

let set_default_ordering deal0 =
    let deal = ref deal0 in
    let whole_tallies = Array.make 4 1 in
    for i = 0 to 3 do
        let tallies = Array.make 4 1 in
        List.iter (fun succ ->
            let idx = match get_last_play succ with Some (Card (suit, _)) -> Obj.magic suit | None -> -1 in
            tallies.(idx) <- tallies.(idx) + 1)
            (successors_of_deal !deal);
        for s = 0 to 3 do
            whole_tallies.(s) <- whole_tallies.(s) * tallies.(s)
        done;
        deal := match !deal with (Deal d) -> Deal {d with d_to_move = (d.d_to_move + 1) land 3}
    done;
    default_ordering := List.stable_sort (fun a b -> whole_tallies.(Obj.magic a) - whole_tallies.(Obj.magic b)) all_suits

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
    let array = Array.make 8192 0 in
    array.(0) <- 0;
    for i = 1 to 8191 do
        array.(i) <- array.(i/2) + (i land 1)
    done;
    array

let push_bits_right_array =
    Array.init
        8192
        (fun x -> let n = bitcount_array.(x)
                  in (1 lsl n) - 1)

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
    let low = canonical_array.((pop_suit_mask land 127) lsl 7 + (hand_suit_mask land 127)) and
        high = canonical_array.(pop_suit_mask land lnot 127 + (hand_suit_mask lsr 7))
    in low + (high lsl bitcount_array.(pop_suit_mask land 127))
[@@inline]

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
    let make_mask_0 hand' pop_mask' =
        (
            let hand_suit_mask = (hand' lsr 0) land 8191 and
                pop_suit_mask = (pop_mask' lsr 0) land 8191 in
            let canon_suit_mask = make_canonical_mask pop_suit_mask hand_suit_mask in
            canon_suit_mask) [@@inline]
    in let value =
       (make_mask_0 hand pop_mask lsl 0) +
       (make_mask_0 (hand lsr 13) (pop_mask lsr 13) lsl 13) +
       (make_mask_0 (hand lsr 26) (pop_mask lsr 26) lsl 26) +
       (make_mask_0 (hand lsr 39) (pop_mask lsr 39) lsl 39)
    in PackedHand value

let canonicalize_entire_suit pop_mask shift =
    let suit_holding = (pop_mask lsr shift) land 8191
    in push_bits_right_array.(suit_holding) lsl shift
[@@inline]

let canonicalize_entire_hand pop_mask =
    canonicalize_entire_suit pop_mask 0 lor
    canonicalize_entire_suit pop_mask 13 lor
    canonicalize_entire_suit pop_mask 26 lor
    canonicalize_entire_suit pop_mask 39

let suit_count pop_mask shift =
    bitcount_array.((pop_mask lsr shift) land 8191)
[@@inline]

(* external vec_pext : int -> int -> int = "vector_pext_stub" [@@noalloc] *)
external vec_pext_3 : int -> int -> int -> int -> unit = "vector_pext_3_stub" [@@noalloc]
external vec_pext_3_retrieve : int -> int = "vector_pext_3_retrieve" [@@noalloc]

let make_deal_canonical (Deal dd) =
    if not !opt
        then dd.d_hands
        else
    let (PackedHand a, PackedHand b, PackedHand c, PackedHand d) = dd.d_hands in
    let pop_mask = a lor b lor c lor d in
    let (w, x, y) =
                (vec_pext_3 pop_mask a b c;
                 (vec_pext_3_retrieve 0, vec_pext_3_retrieve 1, vec_pext_3_retrieve 2)) and
        (r, s, t, u) = (suit_count pop_mask 0,
                        suit_count pop_mask 13,
                        suit_count pop_mask 26,
                        suit_count pop_mask 39) in
    let z = ((1 lsl (r + s + t + u)) - 1) lxor w lxor x lxor y in
    (PackedHand (w + r lsl 52),
     PackedHand (x + s lsl 52),
     PackedHand (y + t lsl 52),
     PackedHand (z + u lsl 52))

let get_hands_from_deal (Deal d) = d.d_hands
(*
    match d.d_hands with
        | (PackedHand a, PackedHand b, PackedHand c, PackedHand d) -> (a, b, c, d)
        | _ -> raise (Failure "impossible")
*)

let calculate_deal_for_hash (Deal d as deal) =
    match (make_deal_canonical deal) with
        | (PackedHand w, PackedHand x, PackedHand y, PackedHand z) ->
            let result = (w - x lsl 1 + y lsl 2 - z lsl 3 + d.d_to_move lsl 60, x, y, z)
            in d.d_deal_for_hash <- Some result;
            result

let deal_for_hash (Deal d as deal) =
    match d.d_deal_for_hash with
        | Some v -> v
        | None -> calculate_deal_for_hash deal
[@@inline]

let make_bloom_key (a, _, _, _) = (abs a) mod 65535

module TTHashtbl = Hashtbl.Make (struct
    type t = int * int * int * int
    let equal (a, b, c, d) (v, w, x, y) =
        let (===) (s : int) (t : int) = (s = t)
        in a === v && b === w && c === x && d === y
    let hash (a, b, c, d) =
        a mod 8191
end)

let clear_tt (tt, filter) middle =
    clear_bloom filter;
    TTHashtbl.filter_map_inplace
        (fun k ((v, m, used) as x) ->
            if !used > 0 then (decr used; set_bloom filter (make_bloom_key k); Some x) else None)
    tt

let store_value_in_tt (tt, filter) deal value middle =
   (if TTHashtbl.length tt >= 10000
        then clear_tt (tt, filter) middle);
    let d4h = deal_for_hash deal in
    TTHashtbl.replace tt d4h (immediate_value_of_deal deal, value, ref 6);
    set_bloom filter (make_bloom_key d4h)

let look_up_value_in_tt (tt, filter) deal (middle : int) =
    let d4h = deal_for_hash deal in
    if check_bloom filter (make_bloom_key d4h) = false
        then None
        else
    let hash_val = TTHashtbl.find_opt tt d4h in
    let deal_val = (match hash_val with
        | Some (stored_iv, value, _) ->
            let this_iv = immediate_value_of_deal deal in
            if this_iv <= stored_iv && value < middle
                then Some value
                else
            if this_iv >= stored_iv && value > middle
                then Some value
                else
            None
        | None -> None)
    in (match hash_val with
        | Some (_, _, used) ->
            if deal_val <> None
                then incr used
                else decr used
        | _ -> ());
    deal_val

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
    let mine, opp1, partners, opp2 = get_packed_hands_in_circle_from_current deal in
    List.fold_left (fun acc suit_mask ->
        int_max acc @@ get_long_suit_tricks_in_suit (mine land suit_mask)
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
        | None, _ -> false

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

let get_restricted_partners_packed_hand deal =
    let PackedHand ph = get_partners_packed_hand deal
    in match get_suit_led deal with
        | None -> PackedHand ph
        | Some suit ->
            let restricted = (ph land (List.nth all_suit_masks (Obj.magic suit)))
            in if restricted = 0 then PackedHand ph else PackedHand restricted



let get_lowest_bit_old field =
    let rec get_lowest_bit' field acc =
        if field land 1 = 1 then acc else
        if field land 255 = 0 then get_lowest_bit' (field lsr 8) (acc + 8) else
        get_lowest_bit' (field lsr 1) (acc + 1)
    in
    if field = 0
        then None
        else Some (get_lowest_bit' field 0)

let lowest_bit_array =
    let array = Array.make 67 None in
    for i = 0 to 61 do
        array.((1 lsl i) mod 67) <- Some i
    done;
    array

let get_lowest_bit field =
    let field_with_bit = field land lnot (field - 1)
    in lowest_bit_array.(field_with_bit mod 67)

let without_bit bit_option field =
    match bit_option with
        | Some bit -> field land lnot (1 lsl bit)
        | None -> field

(*
let play_highest field suit_mask =
    without_bit (get_highest_bit @@ field land suit_mask) field
*)

(*
let rec highest_bit_as_field field =
    let wol = without_lowest_bit field in
    if wol = 0
        then field
        else highest_bit_as_field wol
[@@inline]
*)

let play_highest field suit_mask =
    let field_in_suit = field land suit_mask
    in field - highest_bit_as_field field_in_suit

let play_lowest_if_any field suit_mask =
    let field_in_suit = field land suit_mask in
    (field - field_in_suit) + (without_lowest_bit field_in_suit)
[@@inline]

let lowest_bit_as_field field =
    field land (-field)

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
    let field_in_suit = field land suit_mask in
    match field_in_suit with
        | 0 -> without_bit (get_throwaway_bit field) field
        | _ -> (field - field_in_suit) + (without_lowest_bit field_in_suit)
[@@inline]

let rec compare_bit_counts x y =
    match x, y with
        | 0, 0 -> 0
        | _, 0 -> -1
        | 0, _ -> 1
        | _, _ -> compare_bit_counts (without_lowest_bit x) (without_lowest_bit y)

let can_play_sequential_trick_in_suit_aux mine partners opp1 opp2 suit_mask =
    let bit_compare = compare_bit_counts mine partners in
    if without_lowest_bit mine > 0 && bit_compare > 0 &&
                mine > (opp1 lor opp2) && mine > lowest_bit_as_field partners
        then `Mine
        else
    if without_lowest_bit partners > 0 && bit_compare < 0 &&
                partners > (opp1 lor opp2) && partners > lowest_bit_as_field mine
        then `Partner
        else
    if mine > 0 && mine > partners && mine > (opp1 lor opp2)
        then `Mine
        else
    if mine > 0 && partners > mine && partners > (opp1 lor opp2)
        then `Partner
        else
    `Neither

let can_play_sequential_trick_in_suit mine_all partners_all opp1_all opp2_all suit_mask =
    let mine = mine_all land suit_mask and
        partners = partners_all land suit_mask and
        opp1 = opp1_all land suit_mask and
        opp2 = opp2_all land suit_mask in
    if mine lor partners <= opp1 lor opp2
        then `Neither
        else can_play_sequential_trick_in_suit_aux mine partners opp1 opp2 suit_mask
[@@inline]

(*
let does_hand_hold_akq field =
    match get_highest_bit field with
        | Some bit ->
            let mask = 7 * (1 lsl (bit - 2))
            in field land mask = mask
        | None -> false

let does_hand_hold_aq field =
    match get_highest_bit field with
        | Some bit ->
            let mask_screen = 7 * (1 lsl (bit - 2)) and
                mask_target = 5 * (1 lsl (bit - 2))
            in field land mask_screen = mask_target
        | None -> false

let is_akq_finesse_in_order (first_, second_, third_, fourth_) suit_mask =
    let pop_mask = (first_ lor second_ lor third_ lor fourth_) land suit_mask
    in let first, second, third =
            (make_canonical_mask (fold_mask pop_mask) (fold_mask (first_ land suit_mask)),
             make_canonical_mask (fold_mask pop_mask) (fold_mask (second_ land suit_mask)),
             make_canonical_mask (fold_mask pop_mask) (fold_mask (third_ land suit_mask)))
    in  first <> 0 &&
        does_hand_hold_aq third &&
        does_hand_hold_akq (second lor third) &&
        count_bits second >= 2
*)

let rec get_nth_highest_bit field n =
    match n with
        | 1 -> (match get_highest_bit field with Some bit -> bit | None -> 64)
        | _ when n < 1 -> raise (Failure "bad n passed to get_nth_highest_bit")
        | _ -> (match get_highest_bit field with
                    | Some bit -> get_nth_highest_bit (field land lnot (1 lsl bit)) (n - 1)
                    | None -> 64)

let rec get_nth_highest_bit_as_field field n =
    match n with
        | 1 -> highest_bit_as_field field
        | _ when n < 1 -> raise (Failure "bad n passed to get_nth_highest_bit_as_field")
        | _ -> get_nth_highest_bit_as_field (without_highest_bit field) (n - 1)

let rec remove_bits_if_adjacent_to_right field mask =
    let next_mask = mask lsr 1 in
    if field land next_mask = 0
        then field
        else remove_bits_if_adjacent_to_right (field - mask) next_mask
[@@inline]

let remove_duplicate_front_bits field =
    let b1 = highest_bit_as_field field in
    match b1 with
        | 0 -> 0
        | _ -> remove_bits_if_adjacent_to_right field b1

let is_akq_finesse_in_order_aux first_0 second_0 third_0 fourth_0 suit_mask_0 =
    let first, second, third, fourth =
        (lowest_bit_as_field (first_0 land suit_mask_0),
         second_0 land suit_mask_0,
         remove_duplicate_front_bits (third_0 land suit_mask_0),
         fourth_0 land suit_mask_0) in
    let suit_mask = suit_mask_0 land (first lor second lor third lor fourth) in
  if
    first <> 0 &&
    third > (first lor second lor fourth) &&
    without_lowest_bit second <> 0 &&
    without_lowest_bit third <> 0 &&
    get_nth_highest_bit_as_field suit_mask 1 = get_nth_highest_bit_as_field third 1 &&
    get_nth_highest_bit_as_field suit_mask 2 = get_nth_highest_bit_as_field second 1 &&
    get_nth_highest_bit_as_field suit_mask 3 = get_nth_highest_bit_as_field third 2
  then get_nth_highest_bit_as_field third 2
  else 0

let is_akq_finesse_in_order first_0 second_0 third_0 fourth_0 suit_mask_0 =
    if third_0 land suit_mask_0 > (second_0 lor fourth_0) land suit_mask_0
        then is_akq_finesse_in_order_aux first_0 second_0 third_0 fourth_0 suit_mask_0
        else 0
[@@inline]

let play_specific_bit field bit =
    field land lnot (1 lsl bit)

let play_specific_bit_as_field field play =
    field - play

let rec count_sequential_tricks' mine partners opp1 opp2 suit_mask_list full_mask_list cap =
    if cap = 0
        then 0
        else
    match suit_mask_list with
        | [] -> 0
        | suit_mask :: suit_masks_rest ->
            match (if (mine lor partners) land suit_mask > (opp1 lor opp2) land suit_mask
                        then can_play_sequential_trick_in_suit mine partners opp1 opp2 suit_mask
                        else `Neither) with
                | `Mine -> 1 + count_sequential_tricks' (play_highest mine suit_mask)
                                                        (play_lowest_or_any partners suit_mask)
                                                        (play_lowest_if_any opp1 suit_mask)
                                                        (play_lowest_if_any opp2 suit_mask)
                                                        full_mask_list
                                                        full_mask_list
                                                        (cap - 1)
                | `Partner -> let can_finesse = is_akq_finesse_in_order
                                                    mine  opp1  partners  opp2
                                                    suit_mask in
                      (match can_finesse with
                          | 0 ->
                              1 + count_sequential_tricks' (play_highest partners suit_mask)
                                                           (play_lowest_or_any mine suit_mask)
                                                           (play_lowest_if_any opp2 suit_mask)
                                                           (play_lowest_if_any opp1 suit_mask)
                                                           full_mask_list
                                                           full_mask_list
                                                           (cap - 1)
                          | finesse_field ->
                                let mine_after = play_lowest_if_any mine suit_mask and
                                    opp2_after = play_lowest_if_any opp2 suit_mask in
                                int_min
                                    (* first, if opp1 plays low: *)
                             (1 + count_sequential_tricks' (play_specific_bit_as_field partners finesse_field)
                                                           mine_after
                                                           opp2_after
                                                           (play_lowest_if_any opp1 suit_mask)
                                                           full_mask_list
                                                           full_mask_list
                                                           (cap - 1))
                                    (* second, if opp1 plays high: *)
                             (1 + count_sequential_tricks' (play_highest partners suit_mask)
                                                           mine_after
                                                           opp2_after
                                                           (play_highest opp1 suit_mask)
                                                           full_mask_list
                                                           full_mask_list
                                                           (cap - 1))
                      )
                | `Neither -> count_sequential_tricks' mine partners opp1 opp2
                                                       suit_masks_rest
                                                       full_mask_list
                                                       (cap)

let count_sequential_tricks deal suit_mask_list cap =
    let mine, opp1, partners, opp2 = get_packed_hands_in_circle_from_current deal in
    count_sequential_tricks' mine partners opp1 opp2 suit_mask_list suit_mask_list cap

let count_sequential_tricks_top deal cap =
    let x = (count_sequential_tricks deal all_suit_masks cap) in
    if x = cap
        then cap
        else int_max x (count_sequential_tricks deal all_suit_masks_rev cap)




let get_next_opponents_packed_hand (Deal d) =
    tuple_nth d.d_hands ((d.d_to_move + 1) land 3)

let get_previous_opponents_packed_hand (Deal d) =
    tuple_nth d.d_hands ((d.d_to_move + 3) land 3)

let rec play_lowest_exceeding mine others suit_mask previous_tries =
    match get_lowest_bit ((mine land lnot previous_tries) land suit_mask) with
        | Some bit ->
            if (1 lsl bit) > (others land suit_mask)
                then mine land lnot (1 lsl bit)
                else play_lowest_exceeding mine others suit_mask (previous_tries lor (1 lsl bit))
        | None ->
            raise (Failure "play_lowest_exceeding")

let count_sequential_tricks_for_2nd deal suit_mask_list cap =
    let mine, opp1, partners, opp2 = get_packed_hands_in_circle_from_current deal in
    match get_lead deal with
        | None -> raise (Failure "count_sequential_tricks_for_2nd called on new trick")
        | Some (Card (suit_led, rank_led) as card_led) ->
    let suit_mask = mask_for_suit suit_led
    in
    if (mine land suit_mask) > 0 && (mine land suit_mask) > (partners land suit_mask) &&
            (mine land suit_mask) > (opp1 land suit_mask) && (mine land suit_mask) > (1 lsl index_of_card card_led)
        then 1 + count_sequential_tricks' (play_lowest_exceeding
                                                mine
                                                (partners lor opp1 lor (1 lsl index_of_card card_led))
                                                suit_mask
                                                0)
                                          (play_lowest_or_any partners suit_mask)
                                          (play_lowest_if_any opp1 suit_mask)
                                          opp2
                                          suit_mask_list
                                          suit_mask_list
                                          (cap - 1)
        else
    if (mine land suit_mask) > 0 && (partners land suit_mask) > (mine land suit_mask) &&
            (partners land suit_mask) > (opp1 land suit_mask) && (partners land suit_mask) > (1 lsl index_of_card card_led)
        then 1 + count_sequential_tricks' (play_lowest_exceeding
                                                partners
                                                (mine lor opp1 lor (1 lsl index_of_card card_led))
                                                suit_mask
                                                0)
                                          (play_lowest_or_any mine suit_mask)
                                          opp2
                                          (play_lowest_if_any opp1 suit_mask)
                                          suit_mask_list
                                          suit_mask_list
                                          (cap - 1)
        else
    0

let count_sequential_tricks_for_2nd_top deal cap =
    let x = (count_sequential_tricks_for_2nd deal all_suit_masks cap) in
    if x = cap
        then x
        else int_max x (count_sequential_tricks_for_2nd deal all_suit_masks_rev cap)

let pull_from_tt tts succs middle =
    match tts with
        | _ :: tt :: _ ->
    let any_yet = ref false in
    let a, b = List.partition (fun succ ->
                    if not !any_yet && look_up_value_in_tt tt succ middle <> None
                        then (any_yet := true; true)
                        else false)
               succs
    in a @ b
        | _ -> succs

let report_deal deal depth middle =
    print_deal deal;
    Printf.printf "depth %d,   iv %d,   middle %d\n\n%!"
        depth
        (immediate_value_of_deal deal)
        middle

let rec postpone_double_suits pulled kicked queue =
    let aos = Card (Spade, RA) in
    match queue with
        | [] -> (pulled, kicked)
        | [x] -> postpone_double_suits (x :: pulled) kicked []
        | x :: y :: rest ->
            if suit_of_card (extract aos (get_last_play x)) = suit_of_card (extract aos (get_last_play y))
                then postpone_double_suits (pulled) (y :: kicked) (x :: rest)
                else postpone_double_suits (x :: pulled) (kicked) (y :: rest)

let rec sort_suits_backwards first second =
    match first with
        | [] -> second
        | x :: xs ->
            let target_suit = (match get_last_play x with Some (Card (suit, _)) -> Some suit | None -> None) in
            let a, b = List.partition (fun succ -> match get_last_play succ with Some (Card (suit, _)) when Some suit = target_suit -> true | _ -> false) second
            in sort_suits_backwards xs (a @ b)
(*
let sort_first_four_succs succs =
    let first, second = match succs with
        | a :: b :: c :: d :: xs ->
            [a; b; c; d], xs
        | _ ->
            [], succs
    in let e = (List.rev @@ sort_deals_by_last_play first) and
           f = (List.rev @@ second) in
    e @ (sort_suits_backwards e f)
*)

let sort_first_different_suits (first, second) =
    (first) @
    sort_suits_backwards (first) (List.rev second)

let mask_for_suit_option = function
    | Some suit -> mask_for_suit suit
    | None -> -1

let restrict_packed_hand_to_suit (PackedHand ph) suit_mask =
    let overlap = ph land suit_mask in
    if overlap = 0
        then PackedHand ph
        else PackedHand overlap

let get_my_and_partners_packed_hands (Deal d) =
    match d.d_to_move, d.d_hands with
        | 0, (w, _, x, _) -> w, x
        | 1, (_, w, _, x) -> w, x
        | 2, (x, _, w, _) -> w, x
        | _, (_, x, _, w) -> w, x
[@@inline]

let make_recom_key (Deal d as deal) =
    let led_suit = get_suit_led deal in
    let led_suit_mask = mask_for_suit_option led_suit in
    let mine, partners = get_my_and_partners_packed_hands deal in
    let a = restrict_packed_hand_to_suit mine led_suit_mask and
        b = (let x = highest_bit_as_field
                    (match restrict_packed_hand_to_suit partners
                                                        led_suit_mask
                     with PackedHand x -> x)
             in let y = x - 1
             in let z = y lxor (y lsr 26)
             in z) and
        c = (match card_currently_winning_with_led_suit deal led_suit with
                Some card -> index_of_card card | None -> 52) and
        d = (match led_suit with None -> 4 | Some suit -> Obj.magic suit) and
        e = (if is_top_card_winning_true deal then 1 lsl 24 else 0) and
        f = d.d_to_move
    in (a, b lsl 25 + c lsl 6 + d lsl 12 + f lsl 18 + e)

let reset_suit_ordering player = ()
(*
    let freqs = Hashtbl.create 4 in
    List.iter (fun suit -> Hashtbl.replace freqs suit 0) all_suits;
    Hashtbl.iter (fun (_, _, _, suit_led, _, player_) card_ref ->
          if player land 1 <> player_ land 1 && suit_led = None then
            let suit = suit_of_card !card_ref in
            Hashtbl.replace freqs suit (Hashtbl.find freqs suit + 1))
        recommendation_table;
    suit_ordering.(player) <- List.stable_sort (fun a b ->
            Hashtbl.find freqs a - Hashtbl.find freqs b)
        !default_ordering
*)

let rec sort_kicked_by_ordering kicked ordering =
    match ordering with
        | [] -> kicked
        | x :: xs ->
            let a, b = List.partition (fun succ -> match get_last_play succ with Some card when suit_of_card card = x -> true | _ -> false) kicked
            in let c, d = List.partition (fun succ -> not @@ can_2nd_player_win_next_trick succ) a
            in sort_kicked_by_ordering b xs @ (c @ d)

let code_for_player player = String.make 1 "WNES".[player]

let code_for_hands packed_hands =
    let holder_of_card card_idx =
        (let indices = List.filter (fun player -> let PackedHand ph = tuple_nth packed_hands player in ph land (1 lsl card_idx) <> 0) [0; 1; 2; 3]
         in match indices with
            | x :: xs -> Some x
            | [] -> None)
    in
    let s = ref "" in
    for idx = 0 to 51 do
        let holder = holder_of_card idx in
        s := !s ^ (match holder with Some player -> code_for_player player | None -> "*")
    done;
    !s

let report_recommendation (Deal d) depth iv middle recom =
    Printf.printf ">>> %s %s %d %d %d\n"
                    (code_for_hands d.d_hands)
                    (code_for_player d.d_to_move)
                    depth
                    (iv - middle)
                    (index_of_card recom)

let add_recom_to_history deal recom =
    let list_ref =
        (match Hashtbl.find_opt recommendation_history (make_recom_key deal) with
            | Some x -> x
            | None -> let y = ref [] in Hashtbl.replace recommendation_history (make_recom_key deal) y; y)
    in list_ref := recom :: !list_ref

let string_of_suit suit =
    String.sub all_suit_string (3 * Obj.magic suit) 3

let string_of_hand ph =
    let s = ref "" in
    List.iter (fun card ->
                    s := !s ^ " " ^ string_of_card card)
              (match unpack_hand ph with Hand cards -> cards);
    !s

let print_recom_history () = ()
(*
    Hashtbl.iter (fun (ph, partner_bit, ccw_opt, lead_opt, tcw, player) card_list_ref ->
      if lead_opt = None then (
        Printf.printf "@@@ (%s, %s, %s, %s, %s, %s) ->"
            (string_of_hand ph)
            (match card_from_index_option partner_bit with Some card -> (string_of_card card) | None -> "None")
            (match ccw_opt with Some card -> (string_of_card card) | None -> "None")
            (match lead_opt with Some lead -> string_of_suit lead | None -> "None")
            (if tcw then "true" else "false")
            (String.make 1 "WNES".[player]);
        List.iter (fun card -> Printf.printf " %s" (string_of_card card)) !card_list_ref;
        Printf.printf "\n"))
      recommendation_history
*)
let symmetric_tricks_for_opponents_in_suit (Deal d as deal) suit_mask =
    let PackedHand opp1 = tuple_nth d.d_hands (d.d_to_move lxor 1) and
        PackedHand opp2 = tuple_nth d.d_hands (d.d_to_move lxor 3) and
        PackedHand ours = get_packed_hand_of_current_side deal
    in
    if opp1 land suit_mask <> 0 &&
       opp2 land suit_mask <> 0 &&
       (opp1 lor opp2) land suit_mask > ours land suit_mask

       then (* at least 1 *)
           if count_bits (opp1 land suit_mask) >= 3 &&
              count_bits (opp2 land suit_mask) >= 3 &&
              without_highest_bit (without_highest_bit ((opp1 lor opp2) land suit_mask)) > ours land suit_mask
                then 3
                else
           if count_bits (opp1 land suit_mask) >= 2 &&
              count_bits (opp2 land suit_mask) >= 2 &&
              without_highest_bit ((opp1 lor opp2) land suit_mask) > ours land suit_mask
                then 2
                else 1
       else 0

let tricks_to_be_lost_immediately deal =
    if can_side_win_next_trick deal
        then 0
        else
    List.fold_left (fun acc suit_mask -> acc + symmetric_tricks_for_opponents_in_suit deal suit_mask)
        0
        all_suit_masks

let does_current_side_hold_highest_card_in_suit deal suit =
    let PackedHand ours = get_packed_hand_of_current_side deal and
        PackedHand theirs = get_packed_hand_of_other_side deal and
        suit_mask = mask_for_suit suit
    in ours land suit_mask > theirs land suit_mask

let rec fill_in_below n =
    if n = 0
        then 0
        else n lor fill_in_below (n lsr 1)

let is_highest_card_simply_guarded deal suit =
    let PackedHand ours = get_packed_hand_of_current_side deal and
        PackedHand theirs = get_packed_hand_of_other_side deal and
        PackedHand opp1 = get_first_opponents_packed_hand deal and
        PackedHand opp2 = get_second_opponents_packed_hand deal and
        suit_mask = mask_for_suit suit
    in let
        opp1_bits = count_bits (opp1 land suit_mask) and
        opp2_bits = count_bits (opp2 land suit_mask)
    in
        opp1_bits >= 1 && opp2_bits >= 1 && int_max opp1_bits opp2_bits >= 2 &&
        count_bits ((theirs land suit_mask) land lnot
                    (fill_in_below (without_highest_bit (ours land suit_mask)))) >= 2

let number_of_highest_cards_held_by_current_side deal =
    List.fold_left (fun acc suit -> acc +
                        if does_current_side_hold_highest_card_in_suit deal suit
                            then 1
                            else 0)
                   0
                   all_suits

let tricks_before_losing_one deal =
    if is_every_suit_in_packed_hand (get_packed_hand_to_move deal) &&
       is_every_suit_in_packed_hand (get_partners_packed_hand deal) &&
            List.for_all (fun suit -> (not @@ does_current_side_hold_highest_card_in_suit deal suit) ||
                                      (is_highest_card_simply_guarded deal suit))
                         all_suits
        then Some (number_of_highest_cards_held_by_current_side deal)
        else None

let tuple_for_deal (Deal d) =
    (d.d_hands, d.d_to_move, d.d_played, d.d_tricks, d.d_turns, d.d_last_play, d.d_deal_for_hash)

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

    match (if d.d_turns land 3 = 0
                then look_up_value_in_tt (List.hd tts) deal middle
                else None) with
        | Some x when x <> middle -> (if x > middle then middle + 1 else middle - 1), []
        | _ ->

    if depth = 4 && iv = middle
        then (if can_side_win_next_trick deal then (middle + 1, []) else (middle - 1, []))
        else

    if depth land 3 = 0 && iv - (depth / 4) = middle - 1
                        && can_side_win_next_trick deal
        then (middle + 1, [])
        else

    if !opt && depth land 3 = 3 && iv - ((depth + 1) / 4) = middle - 1
            && can_2nd_player_win_next_trick deal
        then (middle + 1, [])
        else

    if depth land 3 = 0 && iv + (depth / 4) = middle + 1
                        && not @@ can_side_win_next_trick deal
        then (middle - 1, [])
        else
(*
    if depth land 3 = 0 && iv + (depth / 4) > middle
                        && int_max (count_top_tricks_in_both_hands deal)
                                   (count_ace_tricks_between_hands deal) >= depth / 4
        then (middle + 1, [])
        else
*)

    if !opt && depth land 3 = 0 && can_return_early iv (depth / 4) (int_max (count_top_tricks_in_both_hands deal)
                                  (count_ace_tricks_between_hands deal)) middle
        then (middle + 1, [])
        else

    if !opt && depth land 3 = 0 && can_return_early iv (depth / 4)
                                  (count_iter_tricks deal) middle
        then (middle + 1, [])
        else

    if !opt && depth land 3 = 0 && can_return_early iv (depth / 4)
                                  (count_top_tricks_in_hand deal) middle
        then (middle + 1, [])
        else

    if !opt && depth land 3 = 0 && can_return_early iv (depth / 4)
                          (count_long_suit_tricks_in_hand deal) middle
        then (middle + 1, [])
        else

    if !opt && depth land 3 = 0 && can_return_early iv (depth / 4)
                                  (count_sequential_tricks_top deal (depth / 4))
                                  middle
        then (middle + 1, [])
        else

    if !opt && depth land 3 = 3 && can_return_early iv ((depth+3) / 4)
                                  (count_sequential_tricks_for_2nd_top deal ((depth+3)/4))
                                   middle
        then (middle + 1, [])
        else
(*
    if depth land 3 = 0 &&
                           (let capped = int_min (tricks_to_be_lost_immediately deal) (depth / 4)
                            in let remainder = (depth / 4) - capped
                            in iv - capped + remainder < middle)
        then (middle - 1, [])
        else

    if depth land 3 = 0 && iv + (depth / 4) = middle + 1 &&
                           (match tricks_before_losing_one deal with
                                | Some value -> value < depth / 4
                                | None -> false)
        then (middle - 1, [])
        else
*)
    let best_value = ref (-1000) and
        best_variation = ref [] and
        aos = Card (Spade, RA) and
        has_printed_excl = ref false in
    (* (if d.d_turns = 40 && depth >= 12 then report_deal deal depth middle); *)
    let iter_body =
                (fun succ ->
                             if !best_value > middle
                                then (if depth = topdepth && topdepth >= -36 then (if not !has_printed_excl && !show_progress then (Printf.printf "!"; has_printed_excl := true); if !show_progress then Printf.printf " [%s]%!" (string_of_card @@ extract aos (get_last_play succ))))
                                else (if depth = topdepth && topdepth >= -36 && !show_progress then Printf.printf " %s...%!" (string_of_card @@ extract aos (get_last_play succ));
                             let value, variation = evaluate_deal_gamma topdepth counter (List.tl tts) succ (depth - 1)
                                                    (if same_sides_in_deals deal succ then middle else -middle)
                             in let adjusted_value = (if same_sides_in_deals deal succ then value else -value)
                             in if adjusted_value > !best_value
                                    then (
                                          best_value := adjusted_value;
                                          best_variation :=
                                 match get_last_play succ with
                                    | Some x -> x :: variation
                                    | None -> variation);
                             ())) and
        recommendation = (if depth land 3 = 1 && depth > 12 then None else RecHashtbl.find_opt recommendation_table (make_recom_key deal)) in
            (match recommendation with
                | Some card_ref -> iter_body @@ deal_after_playing !card_ref deal
                | None -> ());
            (if !best_value < middle || depth = topdepth
                then let recom = match recommendation with Some y -> Some !y | None -> None
                in List.iter (fun succ ->
                                    let (<>) = are_card_options_unequal in
                                    if get_last_play succ <> recom then iter_body succ)
                (let raw_successors = successors_of_deal_without_equals deal
                 in let succs_to_pass =
                    (let sorted_successors = (if depth land 3 = 0 then (fun x -> x) else sort_deals_by_last_play) @@ raw_successors in
                 match depth land 3 with
                    | 1 -> let (wins, losses) = List.partition (fun succ -> same_sides_in_deals deal succ)
                                                               (List.rev sorted_successors)
                           in pull_from_tt tts wins middle @ pull_from_tt tts losses (-middle)
                    | 3 -> let (wins, losses) = List.partition (fun succ -> is_top_card_winning succ || is_led_card_winning succ) @@ List.rev sorted_successors
                           in wins @ losses
                    | 2 -> let (wins, losses) = List.partition is_top_card_winning @@ List.rev sorted_successors
                           in wins @ losses
                    | _ -> (* let pulled, kicked = postpone_double_suits [] [] (List.rev sorted_successors)
                           in *)
                              (sort_kicked_by_ordering (List.rev sorted_successors) suit_ordering.(d.d_to_move)))
                in if not !opt then assert (List.sort compare (List.map tuple_for_deal succs_to_pass) =
                                            List.sort compare (List.map tuple_for_deal raw_successors));
                succs_to_pass));
    (if depth = topdepth && topdepth >= -36 && !show_progress then Printf.printf "\n%!");
    (if depth land 3 <> 1 || depth <= 12 then
     match !best_variation with
        | x :: _ -> (match recommendation with Some y -> y := x | None -> RecHashtbl.replace recommendation_table (make_recom_key deal) (ref x));
                    if !report_recs && !best_value > middle then report_recommendation deal depth iv middle x;
                    if !report_rec_history && !best_value > middle then add_recom_to_history deal x
        | [] -> ());

    let return_value = (!best_value, !best_variation)
    in (if d.d_turns land 3 = 0 then store_value_in_tt (List.hd tts) deal !best_value middle);
    return_value

let make_trans_table_tower () =
    let tower = ref [] in
    for i = 0 to 52 do
        tower := (TTHashtbl.create 100, make_bloom ()) :: !tower
    done;
    !tower

let rec print_ledger opening ledger =
    if opening then Printf.printf "[";
    match ledger with
        | [] -> Printf.printf "]\n"
        | [x] -> Printf.printf "%d]\n%!" x
        | x :: xs -> Printf.printf "%d " x; print_ledger false xs

let clean_tt_tower tower comparison new_middle =
    List.iter (fun (tt, filter) ->
        clear_bloom filter;
        TTHashtbl.filter_map_inplace (fun k ((v, m, u) as x) ->
            if comparison v m && comparison m new_middle && abs (new_middle - v) <= 1
                then (set_bloom filter (make_bloom_key k); Some x) else None)
            tt)
        tower



let rec remove_duplicates = function
    | [] -> []
    | x :: xs -> x :: remove_duplicates (List.filter ((<>) x) xs)

let set_ordering_from_variation variation =
    let suits = List.map suit_of_card variation in
    let ordering = remove_duplicates suits in
    Array.fill suit_ordering 0 4 (remove_duplicates @@ suit_ordering.(0) @ List.rev ordering)

let name_of_suit = function
    | Club -> "Club"
    | Diamond -> "Diamond"
    | Heart -> "Heart"
    | Spade -> "Spade"

let print_tally_of_recommended_suits () =
    let table = Hashtbl.create 4 in
    List.iter (fun suit -> Hashtbl.replace table suit 0) all_suits;
    RecHashtbl.iter (fun k v ->
        let suit = suit_of_card !v in
        Hashtbl.replace table suit (Hashtbl.find table suit + 1))
        recommendation_table;
    List.iter (fun suit -> Printf.printf "%s\t%d\n" (name_of_suit suit) (Hashtbl.find table suit))
              all_suits

let number_to_binary n0 idx =
    let s = ref "" and n = ref n0 in
    for i = 0 to 51 do
        s := !s ^ (if i = idx then "*" else string_of_int (!n land 1));
        n := !n lsr 1
    done;
    !s

let clear_tower_level (tt, filter) =
    TTHashtbl.clear tt;
    clear_bloom filter

let evaluate_deal_gamma_top counter deal depth idx =
    RecHashtbl.clear recommendation_table;
    if not !leave_ordering_alone then set_default_ordering deal;
    Array.fill suit_ordering 0 4 !default_ordering;
    let middle = ref 0 and variation = ref [] and
        tower = make_trans_table_tower () and
        ledger = ref [] in
    for d = 1 to depth do
        if d land 3 = 0
            then (if not !opt then List.iter clear_tower_level tower;
                 (* for player = 0 to 3 do reset_suit_ordering player done; *)
                 let (new_middle, new_variation) =
                        evaluate_deal_gamma d counter tower deal d !middle
                 in (clean_tt_tower tower (if new_middle > !middle then (<=) else (>=)) !middle;
                     middle := new_middle; variation := new_variation; ledger := new_middle :: !ledger;
                     if !show_progress
                        then Printf.printf "gamma depth %d: value %d, cumul nodes %d, rec table has %d entries\n%!" d new_middle !counter (RecHashtbl.length recommendation_table);
                     set_ordering_from_variation new_variation;
                     if !show_missed_runs && new_middle = d / 4 && !counter > new_middle
                            then Printf.printf "@@@\n%!"))
    done;
    if !show_progress
        then (Printf.printf "#%d: " (idx);
              print_ledger true @@ List.rev !ledger);
    (* print_tally_of_recommended_suits (); *)
    (!middle, !variation), !counter, (List.rev !ledger)

