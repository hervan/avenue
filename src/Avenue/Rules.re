open Types;
open Converters;

let random_farm = () => farm_of_int(Random.int(6));

let add_yc =
  fun
  | Zero => One
  | One => Two
  | Two => Three
  | Three => Four
  | Four => Four;

let add_history = (history_item, {history} as game) => {
  ...game,
  history: [history_item, ...history],
};

let can_flip_farm = ({round_deck, stage}) =>
  switch (stage) {
  | Begin
  | RoundEnd(_) =>
    switch (round_deck) {
    | [_, _, ..._] => true
    | _ => false
    }
  | _ => false
  };

let set_stage_round_farm = ({round_deck} as game) => {
  ...game,
  stage: Round(round_deck |> List.hd, Zero),
};

let discard_top_farm = ({round_deck} as game) => {
  ...game,
  round_deck: round_deck |> List.tl,
};

let add_players_round_points = ({players, stage} as game) =>
  switch (stage) {
  | Round(farm, Zero) => {
      ...game,
      players:
        players
        |> List.map(player =>
             {...player, farm_points: [(farm, 0), ...player.farm_points]}
           ),
    }
  | _ => game
  };

let reset_players_lookahead = ({players} as game) => {
  ...game,
  players: players |> List.map(player => {...player, lookahead: false}),
};

let can_peek_farm = ({players, stage} as game) =>
  switch (stage) {
  | Round(_, _) =>
    switch (players) {
    | [me, ..._] => !me.lookahead && me.turn < game.turn
    | _ => false
    }
  | _ => false
  };

let enable_player_lookahead = ({players} as game) => {
  ...game,
  players: [{...players |> List.hd, lookahead: true}, ...players |> List.tl],
};

let advance_player_turn = ({players} as game) => {
  ...game,
  players: [{...players |> List.hd, turn: game.turn}, ...players |> List.tl],
};

let can_flip_road = ({players, deck, stage} as game) =>
  switch (stage) {
  | Round(_, _) =>
    switch (deck) {
    | [_, ..._] =>
      switch (players) {
      | [me, ..._] => me.turn == game.turn
      | _ => false
      }
    | _ => false
    }
  | _ => false
  };

let set_current_road = ({deck} as game) => {
  ...game,
  current_card: Some(deck |> List.hd),
};

let discard_top_road = ({deck} as game) => {...game, deck: deck |> List.tl};

let advance_yc_stage = ({stage, current_card} as game) => {
  ...game,
  stage:
    switch (stage) {
    | Round(farm, yc) =>
      switch (current_card) {
      | Some((_, Yellow)) => Round(farm, yc->add_yc)
      | _ => stage
      }
    | _ => stage
    },
};

let advance_game_turn = ({turn} as game) => {...game, turn: turn + 1};

let can_draw_road = (row, col, {players, stage, current_card} as game) =>
  switch (current_card) {
  | Some((_, _)) =>
    switch (stage) {
    | Round(_, _) =>
      switch (players) {
      | [{turn, grid}, ..._] =>
        turn < game.turn && grid[row][col].road == None
      | [] => false
      }
    | _ => false
    }
  | None => false
  };

let can_draw_road_somewhere = ({players} as game) =>
  switch (players) {
  | [{grid}, ..._] =>
    grid
    |> Array.to_list
    |> List.exists(grid_row =>
         grid_row
         |> Array.to_list
         |> List.exists(cell => can_draw_road(cell.row, cell.col, game))
       )
  | _ => false
  };

let draw_road_on_grid_cell = (row, col, {players, current_card} as game) =>
  switch (current_card) {
  | Some((road, _)) => {
      ...game,
      players: [
        {
          ...players |> List.hd,
          grid:
            (players |> List.hd).grid
            |> Array.mapi((i, grid_row) =>
                 i == row
                   ? grid_row
                     |> Array.mapi((j, cell) =>
                          j == col ? {...cell, road: Some(road)} : cell
                        )
                   : grid_row
               ),
        },
        ...players |> List.tl,
      ],
    }
  | None => game
  };

let update_points = ({players, farms, stage} as game) =>
  switch (stage) {
  | Round(farm_card, _) =>
    switch (players) {
    | [] => game
    | [{farm_points, grid} as me, ...other_players] =>
      switch (farm_points) {
      | [(farm, _), ...previous_points] =>
        farm == farm_card
          ? {
            ...game,
            players: [
              {
                ...me,
                farm_points: [
                  (
                    farm,
                    Points.count_points(
                      farms
                      |> List.find(cell => cell.content == Farm(farm))
                      |> to_pos,
                      grid,
                    ),
                  ),
                  ...previous_points,
                ],
              },
              ...other_players,
            ],
          }
          : game
      | [] => game
      }
    }
  | _ => game
  };

let can_end_round =
  fun
  | {players: [me, ..._], stage: Round(_, Four), turn} => me.turn == turn
  | _ => false;

let can_end_game =
  fun
  | {stage: RoundEnd(_), round_deck} => round_deck->List.length == 1
  | _ => false;

let advance_stage = (stage, game) => {...game, stage};

let end_game =
  fun
  | {stage: RoundEnd(farm)} as game => game |> advance_stage(End(farm))
  | _ as game => game;

// TODO rework process_round to be modular like actions are now
let process_round = ({players, round_deck, stage, history} as game) =>
  switch (players) {
  | [] => game
  | [me, ...other_players] =>
    switch (stage) {
    | Round(current_farm, yc) =>
      switch (yc) {
      | Four =>
        me.turn == game.turn
          ? {
            ...game,
            players: [
              {
                ...me,
                farm_points:
                  switch (me.farm_points) {
                  | [(farm, points) as current_round, ...previous_rounds] => [
                      current_farm != farm
                        ? current_round
                        : points <= 0
                            ? (farm, (-5))
                            : (
                              switch (previous_rounds) {
                              | [(_, previous_points), ..._] =>
                                points <= previous_points
                                  ? (farm, (-5)) : current_round
                              | [] => current_round
                              }
                            ),
                      ...previous_rounds,
                    ]
                  | [] => me.farm_points
                  },
              },
              ...other_players,
            ],
            stage:
              round_deck->List.length > 1
                ? RoundEnd(current_farm) : End(current_farm),
            history: [
              Event(
                round_deck->List.length > 1
                  ? RoundIsOver(current_farm) : GameIsOver,
              ),
              ...history,
            ],
          }
          : game
      | _ => game
      }
    | Begin
    | RoundEnd(_)
    | End(_) => game
    }
  };

let guide_peek_farm = game =>
  game |> can_peek_farm ? game |> add_history(Action(PeekFarm)) : game;

let guide_flip_farm = game =>
  game |> can_flip_farm ? game |> add_history(Action(FlipFarm)) : game;

let guide_flip_road = game =>
  game |> can_flip_road ? game |> add_history(Action(FlipRoad)) : game;

let guide_draw_road = game =>
  game |> can_draw_road_somewhere
    ? game |> add_history(Action(DrawRoad(0, 0))) : game;

let guide = game =>
  game
  |> guide_peek_farm
  |> guide_flip_farm
  |> guide_flip_road
  |> guide_draw_road;