type t = {
  seed: option<int>,
  turn: int,
  road_deck: list<Road.Card.t>,
  farm_deck: list<Farm.t>,
  stage: Stage.t,
  current_card: option<Road.Card.t>,
  castles: Cell.castles,
}

type action =
  | FlipFarm
  | FlipRoad

let setup = (seed, base_grid, road_deck, farm_deck) => {
  seed: seed,
  turn: 0,
  road_deck: road_deck,
  farm_deck: farm_deck,
  stage: Flow(Created),
  current_card: None,
  castles: {
    purple: Grid.find(Castle(Purple), base_grid),
    green: Grid.find(Castle(Green), base_grid),
  },
}

let next_stage = ({seed, stage, farm_deck, current_card}: t) =>
  switch (stage, current_card, seed) {
  | (Flow(Created), _, Some(_)) => Stage.Flow(Ready)
  | (Flow(Ready | RoundEnd), _, Some(_)) =>
    switch farm_deck {
    | list{_} => Flow(End)
    | list{next_farm, ..._} => Round(next_farm, Zero)
    | list{} => stage
    }
  | (Round(_, Four), _, Some(_)) => Flow(RoundEnd)
  | (Round(farm, yc), Some((_, Yellow)), Some(_)) => Round(farm, yc->Stage.YellowCards.add)
  | (_, _, None)
  | (Round(_, _), Some((_, Grey)) | None, Some(_))
  | (Flow(End), _, Some(_)) => stage
  }

let advance_stage = t => {...t, stage: next_stage(t)}

let discard_top_farm = ({farm_deck} as t) => {
  ...t,
  farm_deck: farm_deck |> List.tl,
}

let reveal_current_road = ({road_deck} as t) => {
  ...t,
  current_card: Some(road_deck |> List.hd),
}

let discard_top_road = ({road_deck} as t) => {
  ...t,
  road_deck: road_deck |> List.tl,
}

let advance_turn = ({turn} as t) => {...t, turn: turn + 1}

let set_stage = (stage, t) => {...t, stage: stage}

let flip_farm = t => t |> advance_stage |> discard_top_farm

let flip_road = t => t |> reveal_current_road |> discard_top_road |> advance_stage |> advance_turn

let reducer = (t, x) =>
  switch x {
  | FlipFarm => t |> flip_farm
  | FlipRoad => t |> flip_road
  }

module Rules = {
  let can_flip_farm = ({stage, farm_deck}: t) =>
    switch stage {
    | Flow(Ready | RoundEnd) =>
      switch farm_deck {
      | list{_, _, ..._} => true
      | list{_}
      | list{} => false
      }
    | Round(_, _)
    | Flow(Created | End) => false
    }

  let can_peek_farm = (player: Player.t, {stage, farm_deck, turn}: t) =>
    switch stage {
    | Round(_, Four) => false
    | Round(_, _) =>
      switch farm_deck {
      | list{}
      | list{_} => false
      | list{_, ..._} => !player.lookahead && player.turn < turn
      }
    | Flow(_) => false
    }

  let can_flip_road = (player: Player.t, {stage, road_deck, turn}: t) =>
    switch stage {
    | Round(_, _) =>
      switch road_deck {
      | list{_, ..._} => player.turn == turn
      | list{} => false
      }
    | Flow(_) => false
    }

  let can_draw_road = ({grid} as player: Player.t, row, col, {current_card, stage, turn}: t) =>
    switch current_card {
    | Some((_, _)) =>
      switch stage {
      | Round(_, _) => player.turn < turn && grid[row][col].road == None
      | Flow(_) => false
      }
    | None => false
    }

  let can_draw_road_somewhere = (player: Player.t, avenue: t) =>
    player.grid
    |> Array.to_list
    |> List.exists(grid_row =>
      grid_row
      |> Array.to_list
      |> List.exists(({Cell.row: row, Cell.col: col}) => can_draw_road(player, row, col, avenue))
    )

  let has_scored_zero = x =>
    switch x {
    | {Player.current_round_points: Some((_, points))} => points == 0
    | {current_round_points: None} => false
    }

  let has_scored_less = x =>
    switch x {
    | {
        Player.current_round_points: Some((_, points)),
        previous_round_points: list{(_, previous_points), ..._},
      } =>
      points <= previous_points
    | {previous_round_points: list{}}
    | {current_round_points: None} => false
    }

  let can_end_round = (player: Player.t, x) =>
    switch x {
    | {stage: Round(_, Four), turn} => player.turn == turn
    | {stage: Round(_, Zero | One | Two | Three) | Flow(_)} => false
    }

  let can_end_game = ({stage, farm_deck}: t) =>
    switch stage {
    | Flow(control_stage) => control_stage == RoundEnd && farm_deck->List.length == 1
    | Round(_, _) => false
    }
}
