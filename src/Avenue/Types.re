type farm =
  | A
  | B
  | C
  | D
  | E
  | F;

type stage =
  | Begin
  | Phase(farm)
  | EndPhase
  | End;

type action =
  | RevealPhase
  | RevealStretchCard
  | DrawStretch(int, int)
  | PeekPhase
  | EndPhase
  | EndGame;

type grape_color =
  | Purple
  | Green;

type cell_content =
  | Empty
  | Grapes(list(grape_color))
  | Castle(grape_color)
  | Farm(farm);

type side =
  | Top
  | Right
  | Bottom
  | Left;

type stretch = (side, side);

type card_color =
  | Grey
  | Yellow;

type card = (stretch, card_color);

type cell = {
  row: int,
  col: int,
  content: cell_content,
  stretch: option(stretch),
};

type grid = array(array(cell));

type board = {
  farmer: string,
  grid,
  lookahead: bool,
  round: int,
};

type game = {
  players: list(board),
  deck: list(card),
  phase_deck: list(farm),
  stage,
  current_card: option(card),
  yellow_cards: int,
  phase_points: list(int),
  castle_points: (int, int),
  base_grid: grid,
  history: list(action),
};