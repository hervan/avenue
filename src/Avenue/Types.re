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
  | PeekPhase
  | EndPhase
  | EndGame;

type cell_content =
  | Empty
  | Grapes(list(grape_color))
  | Castle(grape_color)
  | Farm(farm);

type stretch = (side, side);

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
  round: int,
  phase_deck: list(farm),
  stage,
  current_card: option(card),
  yellow_cards: int,
  phase_points: list(int),
  castle_points: (int, int),
  history: list(action),
};