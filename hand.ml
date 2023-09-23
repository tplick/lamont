
type suit = Club | Diamond | Heart | Spade
type rank = R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | RJ | RQ | RK | RA
type card = Card of suit * rank

let suit_of_card (Card (suit, _)) = suit
let rank_of_card (Card (_, rank)) = rank

let all_suits = [Club; Diamond; Heart; Spade]
let all_ranks = [R2; R3; R4; R5; R6; R7; R8; R9; R10; RJ; RQ; RK; RA]
let all_suit_string = "\u{2663}\u{2662}\u{2661}\u{2660}"
let all_rank_string = "23456789TJQKA"

(*
let rec are_ranks_adjacent a b = function
    | [] | [_] -> false
    | x :: y :: rest ->
        (x = a && y = b) || (x = b && y = a) || are_ranks_adjacent a b (y :: rest)
*)

let are_ranks_adjacent a b =
    match (Obj.magic a : int) - (Obj.magic b : int) with
        | -1 | 1 -> true
        | _ -> false

let are_cards_adjacent a b =
    suit_of_card a = suit_of_card b && are_ranks_adjacent (rank_of_card a) (rank_of_card b)


let rec find_index list elt =
    match list with
        | x :: xs when x = elt -> 0
        | x :: xs -> 1 + find_index xs elt
        | [] -> raise Not_found
and string_of_card (Card (suit, rank)) =
    let sus offset = String.sub all_suit_string offset 3 in
    let suit_string = match suit with Club -> sus 0  | Diamond -> sus 3
                                    | Heart -> sus 6 | Spade -> sus 9
    and rank_char = match find_index all_ranks rank with
                          | idx ->  all_rank_string.[idx]
    in
    suit_string ^ (if rank = R10 then "10" else String.make 1 rank_char )


let new_deck () =
    let deck = ref [] and iter x y = List.iter y x in
    iter all_suits (fun suit ->
        iter all_ranks (fun rank ->
            deck := Card (suit, rank) :: !deck));
    List.rev !deck

let shuffled_deck () =
    let weighted_deck = List.map (fun card -> (Random.float 1.0, card)) (new_deck ())
    in List.map (fun (_, card) -> card) @@ List.sort compare weighted_deck


type hand = Hand of card list
type deal = Deal of {
    d_hands: hand list;
    d_to_move: int;
    d_played: card list;
    d_tricks: int * int;
    d_turns: int;
    d_last_play: card option
}

let rec deal_hands () =
    deal_hands' [] [] [] [] (shuffled_deck ())
and sort xs = List.rev @@ List.sort compare xs
and deal_hands' a b c d = function
    | [] -> (sort a, sort b, sort c, sort d)
    | x :: xs -> deal_hands' b c d (x :: a) xs

let new_deal () =
    let (a, b, c, d) = deal_hands ()
    in Deal {
        d_hands = [Hand a; Hand b; Hand c; Hand d];
        d_to_move = 0;
        d_played = [];
        d_tricks = (0, 0);
        d_turns = 0;
        d_last_play = None
    }


let rec get_lead (Deal deal) =
    get_lead' deal.d_played
and get_lead' = function
    | [] -> None
    | x :: [] -> Some x
    | x :: xs -> get_lead' xs


let get_playable_cards (Deal d as deal) =
    let (Hand hand) = List.nth d.d_hands d.d_to_move in
    if d.d_turns land 3 = 0
        then hand
        else
    match get_lead deal with
        | None -> hand
        | Some (Card (suit, rank)) ->
            let following = List.filter (fun (Card (suit2, rank)) -> suit2 = suit) hand
            in match following with [] -> hand | _ -> following

let are_cards_equal (Card (s1, r1)) (Card (s2, r2)) =
    s1 == s2 && r1 == r2

(*
let hand_without_card card (Hand h) =
    Hand (List.filter (fun card' -> not @@ are_cards_equal card card') h)
*)

let hand_without_card card (Hand h) =
    let rec hand_without_card' card' card_list =
        match card_list with
            | [] -> []
            | x :: xs when are_cards_equal x card' -> xs
            | x :: xs -> x :: hand_without_card' card' xs
    in Hand (hand_without_card' card h)

let rec rotate_to_front list elt =
    match list with
        | [] -> []
        | x :: xs when are_cards_equal x elt -> list
        | x :: xs -> rotate_to_front (xs @ [x]) elt

let rotate_to_back list elt =
    match rotate_to_front list elt with
        | [] -> []
        | x :: xs -> xs @ [x]

let rotate_deal_to_winner (Deal d as deal) =
    let played = ref d.d_played in
    match get_lead deal with
        | Some (Card (lead_suit, lead_rank)) ->
            let winning_rank = ref lead_rank and idx = ref 3 in
            for j = 0 to 2 do
                match !played with
                    | (Card (suit, rank)) :: xs ->
                       (played := xs;
                        if suit = lead_suit && rank > !winning_rank
                            then (idx := j; winning_rank := rank))
                    | [] -> raise (Failure "impossible")
            done;
            idx := 3 - !idx;
            let winner = (d.d_to_move + !idx) land 3
            and (ew_tricks, ns_tricks) = d.d_tricks
            in Deal {d with d_to_move = winner;
                            d_played = rotate_to_back d.d_played (Card (lead_suit, !winning_rank));
                            d_tricks = (ew_tricks + 1 - (winner land 1),
                                        ns_tricks +     (winner land 1))}
        | None -> raise (Failure "impossible")

let end_trick (Deal d as deal) =
    if d.d_turns land 3 = 0
        then rotate_deal_to_winner deal
        else deal

let is_new_trick (Deal d) =
    d.d_turns land 3 = 0

let rec hands_after_playing hands (Deal d as deal) card idx =
    if idx = d.d_to_move
        then hand_without_card card (List.hd hands) :: List.tl hands
        else List.hd hands :: hands_after_playing (List.tl hands) deal card (idx + 1)

let deal_after_playing card (Deal d as deal) =
    let child = Deal {
        d with d_hands = hands_after_playing d.d_hands deal card 0
                         (* List.mapi (fun idx h ->
                            if d.d_to_move = idx
                                then hand_without_card card h
                                else h)
                            d.d_hands *);
               d_played = card :: (if is_new_trick deal then [] else d.d_played);
               d_to_move = (d.d_to_move + 1) land 3;
               d_turns = d.d_turns + 1;
               d_last_play = Some card
    }
    in end_trick child

let get_last_play (Deal d) = d.d_last_play

let random_child deal =
    match get_playable_cards deal with
        | [] -> deal
        | cards -> let card = List.nth cards (Random.int @@ List.length cards)
                   in deal_after_playing card deal

let rec remove_equals_from_cards cards =
    match cards with
        | [] | [_] -> cards
        | x :: y :: cards ->
            if are_cards_adjacent x y
                then remove_equals_from_cards (y :: cards)
                else x :: remove_equals_from_cards (y :: cards)

let successors_of_deal (Deal d as deal) =
    List.map (fun card -> deal_after_playing card deal) (get_playable_cards deal)

let successors_of_deal_without_equals (Deal d as deal) =
    List.map (fun card -> deal_after_playing card deal) (remove_equals_from_cards @@ get_playable_cards deal)

let sorted_successors_of_deal (Deal d as deal) =
    let successors = List.sort
                        (fun c1 c2 -> compare (rank_of_card c1) (rank_of_card c2))
                        (get_playable_cards deal)
    in List.map (fun card -> deal_after_playing card deal) successors

let print_hand name (Hand h) =
    Printf.printf "%s:  " name;
    List.iter (fun card -> Printf.printf "%s " (string_of_card card)) h

let current_card_played_by (Deal d) player =
    let cards = ref d.d_played and player_it = ref @@ (d.d_to_move - 1) land 3
    in
    while !cards <> [] && !player_it <> player do
        cards := List.tl !cards;
        player_it := (!player_it - 1) land 3
    done;
    match !cards with
        | x :: xs -> Some x
        | [] -> None

let print_deal (Deal d as deal) =
    let idx = ref 0 in
    List.iter2
        (fun name hand -> Printf.printf "%s " (if d.d_to_move = !idx then "->" else "  ");
                          print_hand name hand;
                         (match current_card_played_by deal !idx with
                                | Some card -> Printf.printf "   (%s)" (string_of_card card)
                                | None -> ());
                          Printf.printf "\n";
                          incr idx)
        ["West"; "North"; "East"; "South"] d.d_hands;
    let (ew_tricks, ns_tricks) = d.d_tricks in
    Printf.printf "EW: %d       NS: %d\n" ew_tricks ns_tricks;
    Printf.printf "%!"

let play_test () =
    let deal = ref @@ new_deal ()
    in print_deal !deal;
    for i = 1 to 52 do
        Unix.sleep 1;
        deal := random_child !deal;
        Printf.printf "\n";
        print_deal !deal
    done

