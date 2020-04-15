open Types;

let can_start_game = ({stage}) =>
  switch (stage) {
  | Flow(Created) => true
  | Round(_, _)
  | Flow(_) => false
  };

let can_flip_farm = ({round_deck, stage}) =>
  switch (stage) {
  | Flow(Begin)
  | Flow(RoundEnd) =>
    switch (round_deck) {
    | [_, _, ..._] => true
    | [_]
    | [] => false
    }
  | Flow(Created)
  | Round(_, _)
  | Flow(End) => false
  };

let can_peek_farm = ({players, stage, round_deck} as game) =>
  switch (stage) {
  | Round(_, Four) => false
  | Round(_, _) =>
    switch (round_deck) {
    | []
    | [_] => false
    | [_, ..._] =>
      switch (players) {
      | [me, ..._] => !me.lookahead && me.turn < game.turn
      | [] => false
      }
    }
  | Flow(_) => false
  };

let can_flip_road = ({players, deck, stage} as game) =>
  switch (stage) {
  | Round(_, _) =>
    switch (deck) {
    | [_, ..._] =>
      switch (players) {
      | [me, ..._] => me.turn == game.turn
      | [] => false
      }
    | [] => false
    }
  | Flow(_) => false
  };

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
    | Flow(_) => false
    }
  | None => false
  };

let can_draw_road_somewhere =
  fun
  | {players: [{grid}, ..._]} as game =>
    grid
    |> Array.to_list
    |> List.exists(grid_row =>
         grid_row
         |> Array.to_list
         |> List.exists(({Cell.row, Cell.col}) =>
              can_draw_road(row, col, game)
            )
       )
  | {players: []} => false;

let can_end_round =
  fun
  | {players: [me, ..._], stage: Round(_, Four), turn} => me.turn == turn
  | {players: []}
  | {stage: Round(_, Zero | One | Two | Three) | Flow(_)} => false;

let can_end_game =
  fun
  | {stage: Flow(RoundEnd), round_deck} => round_deck->List.length == 1
  | {stage: Flow(_) | Round(_, _)} => false;

let guide_start_game = game =>
  game |> can_start_game
    ? game |> Game.add_suggestion(Control(Start)) : game;

let guide_flip_farm = game =>
  game |> can_flip_farm ? game |> Game.add_suggestion(Play(FlipFarm)) : game;

let guide_peek_farm = game =>
  game |> can_peek_farm ? game |> Game.add_suggestion(Play(PeekFarm)) : game;

let guide_flip_road = game =>
  game |> can_flip_road ? game |> Game.add_suggestion(Play(FlipRoad)) : game;

let guide_draw_road = game =>
  game |> can_draw_road_somewhere
    ? game |> Game.add_suggestion(Play(DrawRoad(0, 0))) : game;

let guide = game =>
  {...game, guide: []}
  |> guide_start_game
  |> guide_peek_farm
  |> guide_flip_farm
  |> guide_flip_road
  |> guide_draw_road;