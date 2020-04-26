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