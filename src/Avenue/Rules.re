// TODO: merge with Avenue

let can_flip_farm = ({stage, farm_deck}: Avenue.t) =>
  switch (stage) {
  | Flow(Begin)
  | Flow(RoundEnd) =>
    switch (farm_deck) {
    | [_, _, ..._] => true
    | [_]
    | [] => false
    }
  | Round(_, _)
  | Flow(End) => false
  };

let can_peek_farm = (player: Player.t, {stage, farm_deck, turn}: Avenue.t) =>
  switch (stage) {
  | Round(_, Four) => false
  | Round(_, _) =>
    switch (farm_deck) {
    | []
    | [_] => false
    | [_, ..._] => !player.lookahead && player.turn < turn
    }
  | Flow(_) => false
  };

let can_flip_road = (player: Player.t, {stage, road_deck, turn}: Avenue.t) =>
  switch (stage) {
  | Round(_, _) =>
    switch (road_deck) {
    | [_, ..._] => player.turn == turn
    | [] => false
    }
  | Flow(_) => false
  };

let can_draw_road =
    (
      {grid} as player: Player.t,
      row,
      col,
      {current_card, stage, turn}: Avenue.t,
    ) =>
  switch (current_card) {
  | Some((_, _)) =>
    switch (stage) {
    | Round(_, _) => player.turn < turn && grid[row][col].road == None
    | Flow(_) => false
    }
  | None => false
  };

let can_draw_road_somewhere = (player: Player.t, avenue: Avenue.t) =>
  player.grid
  |> Array.to_list
  |> List.exists(grid_row =>
       grid_row
       |> Array.to_list
       |> List.exists(({Cell.row, Cell.col}) =>
            can_draw_road(player, row, col, avenue)
          )
     );

let has_scored_zero =
  fun
  | {Player.current_round_points: Some((_, points))} => points == 0
  | {current_round_points: None} => false;

let has_scored_less =
  fun
  | {
      Player.current_round_points: Some((_, points)),
      previous_round_points: [(_, previous_points), ..._],
    } =>
    points <= previous_points
  | {previous_round_points: []}
  | {current_round_points: None} => false;

let can_end_round = (player: Player.t) =>
  fun
  | {Avenue.stage: Round(_, Four), turn} => player.turn == turn
  | {stage: Round(_, Zero | One | Two | Three) | Flow(_)} => false;

let can_end_game = ({stage, farm_deck}: Avenue.t) =>
  switch (stage) {
  | Flow(control_stage) =>
    control_stage == RoundEnd && farm_deck->List.length == 1
  | Round(_, _) => false
  };

let suggest_play =
  fun
  | Avenue.PeekFarm => "or click the bottom deck to peek at the upcoming farm"
  | FlipFarm => "click the bottom deck to begin the next round"
  | FlipRoad => "click the top deck to flip a road card"
  | DrawRoad(_, _) => "click an empty cell to draw the face-up road";

let describe_play =
  fun
  | Avenue.PeekFarm => "you peeked at the upcoming farm"
  | FlipFarm => "you flipped a farm card to begin the next round"
  | FlipRoad => "you flipped a road card"
  | DrawRoad(row, col) => {j|you drew a road in cell ($row, $col)|j};