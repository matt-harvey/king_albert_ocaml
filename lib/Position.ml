type t = Foundation of Card.t | Column of Card.t list | Reserve of Card.t Option.t

let is_complete_foundation (foundation : t) : bool =
  match foundation with Foundation (rank, _) -> rank = Rank.max | _ -> false

let can_give (position : t) : bool =
  match position with
  | Foundation _ -> false
  | Column [] -> false
  | Column (_ :: _) -> true
  | Reserve Option.None -> false
  | Reserve (Option.Some _) -> true

let can_receive (position : t) (rank, suit) : bool =
  match (position, rank, suit) with
  | Foundation (top_rank, top_suit), rank, suit -> suit = top_suit && rank = succ top_rank
  | Column [], _, _ -> true
  | Column ((top_rank, top_suit) :: _), rank, suit ->
      Suit.color suit <> Suit.color top_suit && succ rank = top_rank
  | Reserve _, _, _ -> false

(** Accepts a position, returns an updated position, sans the card, plus a card obtained from that position. *)
let give (position : t) : t * Card.t option =
  match position with
  | Column (top_card :: rest) -> (Column rest, Some top_card)
  | Reserve (Some card) -> (Reserve None, Some card)
  | _ -> (position, None)

(** Accepts a position and a card, returns the updated position, including that card. *)
let receive (position : t) (card : Card.t) : t =
  match (position, card) with
  | Foundation _, card -> Foundation card
  | Column cards, card -> Column (card :: cards)
  | Reserve _, _ -> Reserve (Some card)

let to_string_at (position : t) (index : int) : string =
  match position with
  | Foundation card -> Card.to_styled_string card
  | Column cards -> (
      let list_length = List.length cards in
      let rev_index = list_length - index - 1 in
      if rev_index < 0 then "   "
      else
        let maybe_card = List.nth_opt cards rev_index in
        match maybe_card with Option.Some card -> Card.to_styled_string card | None -> "   " )
  | Reserve (Option.Some card) -> Card.to_styled_string card
  | Reserve Option.None -> "   "
