type farm =
  | A
  | B
  | C
  | D
  | E
  | F;

type side =
  | Top
  | Right
  | Bottom
  | Left;

type grape_color =
  | Purple
  | Green;

type card_color =
  | Grey
  | Yellow;

type stage =
  | Begin
  | Phase(farm)
  | PhaseEnd
  | End;

type action =
  | RevealPhase
  | RevealStretchCard
  | DrawStretch(int, int)
  | PeekPhase;

type cell_content =
  | Empty
  | Grapes(list(grape_color))
  | Castle(grape_color)
  | Farm(farm);

type stretch = (side, side);

type stretch_orientation =
  | Forward
  | Backward;

type card = (stretch, card_color);

type cell = {
  row: int,
  col: int,
  content: cell_content,
  stretch: option(stretch),
};

type grid = array(array(cell));

type direction =
  | Beginning
  | Forward
  | Backward
  | From(side)
  | Nowhere;

type board = {
  farmer: string,
  grid,
  lookahead: bool,
  round: int,
  farm_points: list((farm, int)),
};

type castles = {
  purple: cell,
  green: cell,
};

type message =
  | Mistake
  | Info
  | Tip;

type history_item =
  | Action(action)
  | Message(message, string);

type game = {
  players: list(board),
  deck: list(card),
  round: int,
  phase_deck: list(option(farm)),
  stage,
  current_card: option(card),
  yellow_cards: int,
  castles,
  farms: list(cell),
  history: list(history_item),
};