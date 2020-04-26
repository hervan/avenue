open Jest;
open Expect;

let bare_minimum_game = {
  active_player: {
    farmer: "",
    turn: 0,
    lookahead: false,
    grid: [||],
    current_round_points: None,
    previous_round_points: [],
  },
  other_players: [],
  Game.avenue: {
    turn: 0,
    road_deck: [],
    farm_deck: [],
    stage: Flow(Begin),
    current_card: None,
    castles: {
      purple: {
        row: 0,
        col: 0,
        content: Empty,
        road: None,
      },
      green: {
        row: 0,
        col: 0,
        content: Empty,
        road: None,
      },
    },
    farms: [],
  },
  log: [],
  guide: [],
};

let minimal_grid_contents = [|
  [|Cell.Content.Castle(Purple), Farm(A), Empty, Farm(B)|],
  [|Farm(C), Empty, Grapes([Green, Green, Green, Purple]), Empty|],
  [|Empty, Grapes([Green, Purple, Purple, Purple]), Empty, Farm(D)|],
  [|Farm(E), Empty, Farm(F), Castle(Green)|],
|];

let minimal_grid = Game.create_base_grid(minimal_grid_contents);

let road_deck = [];

let farm_deck = Farm.[A, B, C, D, E, F];

describe("Avenue.advance_stage", () => {
  let base_game = Game.create_game("me", minimal_grid, road_deck, farm_deck);
  let game_yellow = {
    ...base_game.avenue,
    current_card: Some((Road.road_of_int(0), Yellow)),
  };
  let game_a_0 =
    game_yellow |> Avenue.advance_stage |> Avenue.discard_top_farm;
  let game_a_1 = game_a_0 |> Avenue.advance_stage;
  let game_a_1_grey = {
    ...game_a_1,
    current_card: Some((Road.road_of_int(0), Grey)),
  };
  let game_a_1_yet = game_a_1_grey |> Avenue.advance_stage;
  let game_a_4 =
    game_a_1
    |> Avenue.advance_stage
    |> Avenue.advance_stage
    |> Avenue.advance_stage;
  let game_round_end_a = game_a_4 |> Avenue.advance_stage;
  let game_b_0 =
    game_round_end_a |> Avenue.advance_stage |> Avenue.discard_top_farm;
  let game_e_0 =
    game_round_end_a
    |> Avenue.discard_top_farm
    |> Avenue.discard_top_farm
    |> Avenue.discard_top_farm
    |> Avenue.advance_stage
    |> Avenue.discard_top_farm;
  let game_e_4 =
    game_e_0
    |> Avenue.advance_stage
    |> Avenue.advance_stage
    |> Avenue.advance_stage
    |> Avenue.advance_stage;
  let game_round_end_e = game_e_4 |> Avenue.advance_stage;
  let game_end_e = game_round_end_e |> Avenue.advance_stage;
  let game_end = game_end_e |> Avenue.advance_stage;

  test("should begin game with Begin stage", () => {
    expect(game_yellow.stage) |> toEqual(Stage.Flow(Begin))
  });

  test("should advance game to farm A, 0 yellow cards stage", () => {
    expect(game_a_0.stage) |> toEqual(Stage.Round(A, Zero))
  });

  test("should remove top farm card from deck", () => {
    expect(game_a_0.farm_deck |> List.hd) |> toEqual(Farm.B)
  });

  test("should advance game to farm A, 1 yellow card stage", () => {
    expect(game_a_1.stage) |> toEqual(Stage.Round(A, One))
  });

  test("should game be in farm A, 1 yellow card stage", () => {
    expect(game_a_1_grey.stage) |> toEqual(Stage.Round(A, One))
  });

  test("should current card be grey", () => {
    expect(game_a_1_grey.current_card)
    |> toEqual(Some((Road.road_of_int(0), Road.Card.Grey)))
  });

  test("should keep the game in farm A, 1 yellow card stage", () => {
    expect(game_a_1_yet.stage) |> toEqual(Stage.Round(A, One))
  });

  test("should advance game to farm A, 4 yellow card stage", () => {
    expect(game_a_4.stage) |> toEqual(Stage.Round(A, Four))
  });

  test("should advance game to round end A stage", () => {
    expect(game_round_end_a.stage) |> toEqual(Stage.Flow(RoundEnd))
  });

  test("should advance game to round B, 0 yellow cards stage", () => {
    expect(game_b_0.stage) |> toEqual(Stage.Round(B, Zero))
  });

  test("should advance game to round E, 0 yellow cards stage", () => {
    expect(game_e_0.stage) |> toEqual(Stage.Round(E, Zero))
  });

  test("should advance game to round E, 4 yellow cards stage", () => {
    expect(game_e_4.stage) |> toEqual(Stage.Round(E, Four))
  });

  test("should advance game to round end E stage", () => {
    expect(game_round_end_e.stage) |> toEqual(Stage.Flow(RoundEnd))
  });

  test("should advance game to end E stage", () => {
    expect(game_end_e.stage) |> toEqual(Stage.Flow(End))
  });

  test("should keep the game in end E stage", () => {
    expect(game_end.stage) |> toEqual(Stage.Flow(End))
  });
});

describe("Avenue.round_penalty", () => {
  test("should penalize if round points is zero", () => {
    let game = {
      ...bare_minimum_game,
      active_player: {
        ...bare_minimum_game.active_player,
        current_round_points: Some((A, 0)),
      },
      avenue: {
        ...bare_minimum_game.avenue,
        stage: Round(A, Four),
      },
      log: [(PeekFarm, [])],
    };
    expect((game |> Game.end_round).active_player.current_round_points)
    |> toEqual(Some((Farm.A, (-5))));
  });
  test(
    "should penalize if round points is zero even if previous is negative", () => {
    let game = {
      ...bare_minimum_game,
      active_player: {
        ...bare_minimum_game.active_player,
        current_round_points: Some((A, 0)),
        previous_round_points: [(B, (-5))],
      },
      avenue: {
        ...bare_minimum_game.avenue,
        stage: Round(A, Four),
      },
      log: [(PeekFarm, [])],
    };
    expect((
      (game |> Game.end_round).active_player.current_round_points,
      (game |> Game.end_round).active_player.previous_round_points,
    ))
    |> toEqual((Some((Farm.A, (-5))), [(Farm.B, (-5))]));
  });
  test("should penalize if round points is lower than previous", () => {
    let game = {
      ...bare_minimum_game,
      active_player: {
        ...bare_minimum_game.active_player,
        current_round_points: Some((A, 39)),
        previous_round_points: [(B, 40)],
      },
      avenue: {
        ...bare_minimum_game.avenue,
        stage: Round(A, Four),
      },
      log: [(PeekFarm, [])],
    };
    expect((
      (game |> Game.end_round).active_player.current_round_points,
      (game |> Game.end_round).active_player.previous_round_points,
    ))
    |> toEqual((Some((Farm.A, (-5))), [(Farm.B, 40)]));
  });
  test("should not penalize first round more than zero", () => {
    let game = {
      ...bare_minimum_game,
      active_player: {
        ...bare_minimum_game.active_player,
        current_round_points: Some((A, 1)),
      },
      avenue: {
        ...bare_minimum_game.avenue,
        stage: Round(A, Four),
      },
      log: [(PeekFarm, [])],
    };
    expect((game |> Game.end_round).active_player.current_round_points)
    |> toEqual(Some((Farm.A, 1)));
  });
  test("should not penalize round score more than previous", () => {
    let game = {
      ...bare_minimum_game,
      active_player: {
        ...bare_minimum_game.active_player,
        current_round_points: Some((A, 2)),
        previous_round_points: [(B, 1)],
      },
      avenue: {
        ...bare_minimum_game.avenue,
        stage: Round(A, Four),
      },
      log: [(PeekFarm, [])],
    };
    expect((
      (game |> Game.end_round).active_player.current_round_points,
      (game |> Game.end_round).active_player.previous_round_points,
    ))
    |> toEqual((Some((Farm.A, 2)), [(Farm.B, 1)]));
  });
  test("should penalize round score equal previous", () => {
    let game = {
      ...bare_minimum_game,
      active_player: {
        ...bare_minimum_game.active_player,
        current_round_points: Some((A, 1)),
        previous_round_points: [(B, 1)],
      },
      avenue: {
        ...bare_minimum_game.avenue,
        stage: Round(A, Four),
      },
      log: [(PeekFarm, [])],
    };
    expect((
      (game |> Game.end_round).active_player.current_round_points,
      (game |> Game.end_round).active_player.previous_round_points,
    ))
    |> toEqual((Some((Farm.A, (-5))), [(Farm.B, 1)]));
  });
});