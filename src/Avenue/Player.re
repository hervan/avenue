// TODO: why is can_draw_road so complex?
// use action point allowance to simplify it: flipping a road allows all players to play one action,
// either drawing that card in an empty cell, or peeking at the next farm.
// which other rules can be simplified with a similar approach?
// in this case, state can be added to make a rule clearer;
// is there another way to represent tacit rules and knowledge?
// what about other common game abstractions, how would the design improve with them?
// table, hand, game box (where components are taken from),
// player screen (for things that can be revealed later), usw

type t = {
  farmer: string,
  turn: int,
  lookahead: bool,
  grid: Grid.t,
  farms: list(Cell.t),
  previous_round_points: list((Farm.t, int)),
  current_round_points: option((Farm.t, int)),
};

type action =
  | PeekFarm
  | DrawRoad(Road.t, int, int, int);

type event =
  | FlipFarm(Farm.t);

// TODO get rid of this
type reducer_action =
  | Action(action)
  | Event(event);

let setup = (player_name, base_grid) => {
  farmer: player_name,
  turn: 0,
  grid: base_grid,
  farms: [
    Grid.find(Farm(A), base_grid),
    Grid.find(Farm(B), base_grid),
    Grid.find(Farm(C), base_grid),
    Grid.find(Farm(D), base_grid),
    Grid.find(Farm(E), base_grid),
    Grid.find(Farm(F), base_grid),
  ],
  lookahead: false,
  current_round_points: None,
  previous_round_points: [],
};

let add_round_points = (farm, t) => {
  ...t,
  current_round_points: Some((farm, 0)),
};

let reset_lookahead = t => {...t, lookahead: false};

let enable_lookahead = t => {...t, lookahead: true};

let advance_turn = (turn, t) => {...t, turn};

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

let draw_road = (road, row, col, turn) =>
  fun
  | {grid} as me => {
      ...me,
      grid: grid |> Grid.draw_road(road, row, col),
      turn,
    };

let reducer = t =>
  fun
  | Event(FlipFarm(farm)) =>
    t
    |> keep_round_points
    |> add_round_points(farm)
    |> reset_lookahead
    |> recount_points(t.farms)
  | Action(PeekFarm) => t
  | Action(DrawRoad(road, row, col, turn)) =>
    t |> draw_road(road, row, col, turn);