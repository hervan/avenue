// TODO merge with Avenue

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

let can_peek_farm = ({stage, farm_deck, active_player, turn}: Avenue.t) =>
  switch (stage) {
  | Round(_, Four) => false
  | Round(_, _) =>
    switch (farm_deck) {
    | []
    | [_] => false
    | [_, ..._] => !active_player.lookahead && active_player.turn < turn
    }
  | Flow(_) => false
  };

let can_flip_road = ({stage, road_deck, active_player, turn}: Avenue.t) =>
  switch (stage) {
  | Round(_, _) =>
    switch (road_deck) {
    | [_, ..._] => active_player.turn == turn
    | [] => false
    }
  | Flow(_) => false
  };

let can_draw_road =
    (
      row,
      col,
      {current_card, stage, active_player: {grid} as active_player, turn}: Avenue.t,
    ) =>
  switch (current_card) {
  | Some((_, _)) =>
    switch (stage) {
    | Round(_, _) => active_player.turn < turn && grid[row][col].road == None
    | Flow(_) => false
    }
  | None => false
  };

let can_draw_road_somewhere = ({active_player} as avenue: Avenue.t) =>
  active_player.grid
  |> Array.to_list
  |> List.exists(grid_row =>
       grid_row
       |> Array.to_list
       |> List.exists(({Cell.row, Cell.col}) =>
            can_draw_road(row, col, avenue)
          )
     );

let has_scored_zero =
  fun
  | {Avenue.active_player: {current_round_points: Some((_, points))}} =>
    points == 0
  | {Avenue.active_player: {current_round_points: None}} => false;

let has_scored_less =
  fun
  | {
      Avenue.active_player: {
        current_round_points: Some((_, points)),
        previous_round_points: [(_, previous_points), ..._],
      },
    } =>
    points <= previous_points
  | {Avenue.active_player: {previous_round_points: []}}
  | {Avenue.active_player: {current_round_points: None}} => false;

let can_end_round =
  fun
  | ({active_player, stage: Round(_, Four), turn}: Avenue.t) =>
    active_player.turn == turn
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