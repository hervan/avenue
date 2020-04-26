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

let reset_lookahead = t => {...t, lookahead: false};

let enable_lookahead = t => {...t, lookahead: true};

let advance_turn = (turn, t) => {...t, turn};

let draw_road_on_grid_cell = (road, row, col) =>
  fun
  | t => {
      ...t,
      grid:
        t.grid
        |> Array.mapi((i, grid_row) =>
             i == row
               ? grid_row
                 |> Array.mapi((j, cell) =>
                      j == col ? {...cell, Cell.road: Some(road)} : cell
                    )
               : grid_row
           ),
    };

let keep_round_points =
  fun
  | {current_round_points: Some(points)} as t => {
      ...t,
      previous_round_points: [points, ...t.previous_round_points],
    }
  | {current_round_points: None} as t => t;

let recount_points = farms =>
  fun
  | {grid, current_round_points: Some((farm, _))} as t => {
      ...t,
      current_round_points:
        Some((
          farm,
          Points.count_points(
            farms
            |> List.find(({Cell.content}) => content == Farm(farm))
            |> Cell.to_pos,
            grid,
          ),
        )),
    }
  | {current_round_points: None} as t => t;