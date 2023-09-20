
type suit = Club | Diamond | Heart | Spade
type rank = R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | RJ | RQ | RK | RA
type card = Card of suit * rank

let suit_of_card (Card (suit, _)) = suit
let rank_of_card (Card (_, rank)) = rank

let all_suits = [Club; Diamond; Heart; Spade]
let all_ranks = [R2; R3; R4; R5; R6; R7; R8; R9; R10; RJ; RQ; RK; RA]
let all_suit_string = "\u{2663}\u{2662}\u{2661}\u{2660}"
let all_rank_string = "23456789TJQKA"

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
    d_turns: int
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
        d_turns = 0
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


let hand_without_card card (Hand h) =
    Hand (List.filter ((<>) card) h)

let rec rotate_to_front list elt =
    match list with
        | [] -> []
        | x :: xs when x = elt -> list
        | x :: xs -> rotate_to_front (xs @ [x]) elt

let rotate_to_back list elt =
    match rotate_to_front list elt with
        | [] -> []
        | x :: xs -> xs @ [x]

let rotate_deal_to_winner (Deal d as deal) =
    match get_lead deal with
        | Some (Card (lead_suit, lead_rank) as lead) ->
            let winning_card = ref lead and idx = ref 3 in
            for j = 0 to 2 do
                let Card (suit, rank) as play = List.nth d.d_played j
                in if suit = lead_suit && rank > rank_of_card !winning_card
                        then (idx := j; winning_card := play)
            done;
            idx := 3 - !idx;
            let winner = (d.d_to_move + !idx) land 3
            and (ew_tricks, ns_tricks) = d.d_tricks
            in Deal {d with d_to_move = winner;
                            d_played = rotate_to_back d.d_played !winning_card;
                            d_tricks = (ew_tricks + 1 - (winner land 1),
                                        ns_tricks +     (winner land 1))}
        | None -> raise (Failure "impossible")

let end_trick (Deal d as deal) =
    if d.d_turns land 3 = 0
        then rotate_deal_to_winner deal
        else deal

let is_new_trick (Deal d) =
    d.d_turns land 3 = 0

let deal_after_playing card (Deal d as deal) =
    let child = Deal {
        d with d_hands = List.map (fun h -> hand_without_card card h) d.d_hands;
               d_played = card :: (if is_new_trick deal then [] else d.d_played);
               d_to_move = (d.d_to_move + 1) land 3;
               d_turns = d.d_turns + 1
    }
    in end_trick child

let random_child deal =
    match get_playable_cards deal with
        | [] -> deal
        | cards -> let card = List.nth cards (Random.int @@ List.length cards)
                   in deal_after_playing card deal


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

