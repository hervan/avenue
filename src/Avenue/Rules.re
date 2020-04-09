open Types;

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

let can_peek_farm = ({players, stage, round_deck} as game) =>
  switch (stage) {
  | Round(_, Four) => false
  | Round(_, _) =>
    switch (round_deck) {
    | [_] => false
    | _ =>
      switch (players) {
      | [me, ..._] => !me.lookahead && me.turn < game.turn
      | _ => false
      }
    }
  | _ => false
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

let can_draw_road_somewhere =
  fun
  | {players: [{grid}, ..._]} as game =>
    grid
    |> Array.to_list
    |> List.exists(grid_row =>
         grid_row
         |> Array.to_list
         |> List.exists(cell => can_draw_road(cell.row, cell.col, game))
       )
  | _ => false;

let can_end_round =
  fun
  | {players: [me, ..._], stage: Round(_, Four), turn} => me.turn == turn
  | _ => false;

let can_end_game =
  fun
  | {stage: RoundEnd(_), round_deck} => round_deck->List.length == 1
  | _ => false;

let guide_peek_farm = game =>
  game |> can_peek_farm
    ? game |> Game.add_history(Suggestion(PeekFarm)) : game;

let guide_flip_farm = game =>
  game |> can_flip_farm
    ? game |> Game.add_history(Suggestion(FlipFarm)) : game;

let guide_flip_road = game =>
  game |> can_flip_road
    ? game |> Game.add_history(Suggestion(FlipRoad)) : game;

let guide_draw_road = game =>
  game |> can_draw_road_somewhere
    ? game |> Game.add_history(Suggestion(DrawRoad(0, 0))) : game;

let guide = game =>
  game
  |> Game.clear_suggestions
  |> guide_peek_farm
  |> guide_flip_farm
  |> guide_flip_road
  |> guide_draw_road;