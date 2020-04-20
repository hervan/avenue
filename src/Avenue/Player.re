type t = {
  farmer: string,
  turn: int,
  lookahead: bool,
  grid: Cell.grid,
  previous_round_points: list((Farm.t, int)),
  current_round_points: option((Farm.t, int)),
};

let add_round_points = (farm, t) => {
  ...t,
  current_round_points: Some((farm, 0)),
};