type t = {
  turn: int,
  active_player: Player.t,
  other_players: list(Player.t),
  road_deck: list(Road.Card.t),
  farm_deck: list(Farm.t),
  stage: Stage.t,
  current_card: option(Road.Card.t),
  castles: Cell.castles,
  farms: list(Cell.t),
};

type action =
  | PeekFarm
  | FlipFarm
  | FlipRoad
  | DrawRoad(int, int);

let next_stage = ({stage, farm_deck, current_card}: t) => {
  switch (stage, current_card) {
  | (Flow(Begin | RoundEnd), _) =>
    switch (farm_deck) {
    | [_] => Stage.Flow(End)
    | [next_farm, ..._] => Round(next_farm, Zero)
    | [] => stage
    }
  | (Round(_, Four), _) => Flow(RoundEnd)
  | (Round(farm, yc), Some((_, Yellow))) => Round(farm, yc->Stage.add_yc)
  | (Round(_, _), Some((_, Grey)))
  | (Round(_, _), None)
  | (Flow(End), _) => stage
  };
};

let advance_stage = t => {...t, stage: next_stage(t)};

let discard_top_farm = ({farm_deck} as t) => {
  ...t,
  farm_deck: farm_deck |> List.tl,
};

let add_players_round_points =
  fun
  | {stage: Round(farm, Zero), active_player, other_players} as t => {
      ...t,
      active_player: active_player |> Player.add_round_points(farm),
      other_players:
        other_players |> List.map(Player.add_round_points(farm)),
    }
  | t => t;

let reset_players_lookahead = ({active_player, other_players} as t) => {
  ...t,
  active_player: {
    ...active_player,
    lookahead: false,
  },
  other_players:
    other_players
    |> List.map((player: Player.t) => {...player, lookahead: false}),
};

let enable_player_lookahead = ({active_player} as t) => {
  ...t,
  active_player: {
    ...active_player,
    lookahead: true,
  },
};

let advance_player_turn = ({active_player, turn} as t) => {
  ...t,
  active_player: {
    ...active_player,
    turn,
  },
};

let set_current_road = ({road_deck} as game) => {
  ...game,
  current_card: Some(road_deck |> List.hd),
};

let discard_top_road = ({road_deck} as game) => {
  ...game,
  road_deck: road_deck |> List.tl,
};

let advance_game_turn = ({turn} as game) => {...game, turn: turn + 1};

let set_stage = (stage, game) => {...game, stage};

let draw_road_on_grid_cell = (row, col) =>
  fun
  | {active_player, current_card: Some((road, _))} as game => {
      ...game,
      active_player: {
        ...active_player,
        grid:
          active_player.grid
          |> Array.mapi((i, grid_row) =>
               i == row
                 ? grid_row
                   |> Array.mapi((j, cell) =>
                        j == col ? {...cell, Cell.road: Some(road)} : cell
                      )
                 : grid_row
             ),
      },
    }
  | {current_card: None} as game => game;

let keep_round_points =
  fun
  | {
      active_player:
        {previous_round_points, current_round_points: Some(points)} as active_player,
    } as t => {
      ...t,
      active_player: {
        ...active_player,
        previous_round_points: [points, ...previous_round_points],
      },
    }
  | {active_player: {current_round_points: None}} as t => t;

let recount_points =
  fun
  | {
      active_player:
        {grid, current_round_points: Some((farm, _))} as active_player,
      farms,
    } as t => {
      ...t,
      active_player: {
        ...active_player,
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
      },
    }
  | {active_player: {current_round_points: None}} as t => t;