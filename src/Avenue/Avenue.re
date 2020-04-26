type t = {
  turn: int,
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

module Rules = {
  let can_flip_farm = ({stage, farm_deck}: t) =>
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

  let can_peek_farm = (player: Player.t, {stage, farm_deck, turn}: t) =>
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

  let can_flip_road = (player: Player.t, {stage, road_deck, turn}: t) =>
    switch (stage) {
    | Round(_, _) =>
      switch (road_deck) {
      | [_, ..._] => player.turn == turn
      | [] => false
      }
    | Flow(_) => false
    };

  let can_draw_road =
      ({grid} as player: Player.t, row, col, {current_card, stage, turn}: t) =>
    switch (current_card) {
    | Some((_, _)) =>
      switch (stage) {
      | Round(_, _) => player.turn < turn && grid[row][col].road == None
      | Flow(_) => false
      }
    | None => false
    };

  let can_draw_road_somewhere = (player: Player.t, avenue: t) =>
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
    | {stage: Round(_, Four), turn} => player.turn == turn
    | {stage: Round(_, Zero | One | Two | Three) | Flow(_)} => false;

  let can_end_game = ({stage, farm_deck}: t) =>
    switch (stage) {
    | Flow(control_stage) =>
      control_stage == RoundEnd && farm_deck->List.length == 1
    | Round(_, _) => false
    };

  let suggest_play =
    fun
    | PeekFarm => "or click the bottom deck to peek at the upcoming farm"
    | FlipFarm => "click the bottom deck to begin the next round"
    | FlipRoad => "click the top deck to flip a road card"
    | DrawRoad(_, _) => "click an empty cell to draw the face-up road";

  let describe_play =
    fun
    | PeekFarm => "you peeked at the upcoming farm"
    | FlipFarm => "you flipped a farm card to begin the next round"
    | FlipRoad => "you flipped a road card"
    | DrawRoad(row, col) => {j|you drew a road in cell ($row, $col)|j};
};