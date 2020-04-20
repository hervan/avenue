open Jest;
open Expect;

let bare_minimum_game = {
  Game.avenue: {
    turn: 0,
    active_player: {
      farmer: "",
      turn: 0,
      lookahead: false,
      grid: [||],
      current_round_points: None,
      previous_round_points: [],
    },
    other_players: [],
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

let minimal_grid_contents_without_empty = [|
  [|Cell.Content.Castle(Purple), Farm(A), Farm(B)|],
  [|Farm(C), Grapes([Green, Green, Green, Purple]), Farm(D)|],
  [|Farm(E), Farm(F), Castle(Green)|],
|];

let minimal_grid_without_empty =
  Game.create_base_grid(minimal_grid_contents_without_empty);

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

describe("Avenue.recount_points", () => {
  let connected_grid = minimal_grid_without_empty;
  connected_grid[0][1] = {
    ...connected_grid[0][1],
    road: Some(Road.road_of_int(2)),
  };
  connected_grid[0][2] = {
    ...connected_grid[0][2],
    road: Some(Road.road_of_int(3)),
  };
  connected_grid[1][0] = {
    ...connected_grid[1][0],
    road: Some(Road.road_of_int(2)),
  };
  connected_grid[1][1] = {
    ...connected_grid[1][1],
    road: Some(Road.road_of_int(0)),
  };
  connected_grid[1][2] = {
    ...connected_grid[1][2],
    road: Some(Road.road_of_int(5)),
  };
  connected_grid[2][0] = {
    ...connected_grid[2][0],
    road: Some(Road.road_of_int(1)),
  };
  connected_grid[2][1] = {
    ...connected_grid[2][1],
    road: Some(Road.road_of_int(4)),
  };
  connected_grid[2][2] = {
    ...connected_grid[2][2],
    road: Some(Road.road_of_int(0)),
  };

  let base_game =
    Game.create_game("me", connected_grid, road_deck, farm_deck);

  let game_round_a =
    {
      ...base_game.avenue,
      stage: Round(A, Zero),
      active_player: {
        ...base_game.avenue.active_player,
        current_round_points: Some((A, 0)),
        previous_round_points: [],
      },
    }
    |> Avenue.recount_points;

  let previous_round_points_round_b =
    switch (game_round_a.active_player.current_round_points) {
    | Some(points) => [
        points,
        ...game_round_a.active_player.previous_round_points,
      ]
    | None => game_round_a.active_player.previous_round_points
    };

  let game_round_b = {
    ...game_round_a,
    stage: Round(B, Zero),
    active_player: {
      ...game_round_a.active_player,
      current_round_points: Some((B, 0)),
      previous_round_points: previous_round_points_round_b,
    },
  };

  let game_round_b_recounted = game_round_b |> Avenue.recount_points;

  test("should have correct points for round A", () => {
    expect(game_round_a.active_player.current_round_points)
    |> toEqual(Some((Farm.A, 4)))
  });

  test("should have correct points for round B", () => {
    expect((
      game_round_b.active_player.current_round_points,
      game_round_b.active_player.previous_round_points,
    ))
    |> toEqual((Some((Farm.B, 0)), [(Farm.A, 4)]))
  });

  test("should recount correct points for round B", () => {
    expect((
      game_round_b_recounted.active_player.current_round_points,
      game_round_b_recounted.active_player.previous_round_points,
    ))
    |> toEqual((Some((Farm.B, 4)), [(Farm.A, 4)]))
  });
});

describe("Avenue.round_penalty", () => {
  test("should penalize if round points is zero", () => {
    let game = {
      ...bare_minimum_game,
      avenue: {
        ...bare_minimum_game.avenue,
        stage: Round(A, Four),
        active_player: {
          ...bare_minimum_game.avenue.active_player,
          current_round_points: Some((A, 0)),
        },
      },
      log: [(PeekFarm, [])],
    };
    expect((game |> Game.end_round).avenue.active_player.current_round_points)
    |> toEqual(Some((Farm.A, (-5))));
  });
  test(
    "should penalize if round points is zero even if previous is negative", () => {
    let game = {
      ...bare_minimum_game,
      avenue: {
        ...bare_minimum_game.avenue,
        stage: Round(A, Four),
        active_player: {
          ...bare_minimum_game.avenue.active_player,
          current_round_points: Some((A, 0)),
          previous_round_points: [(B, (-5))],
        },
      },
      log: [(PeekFarm, [])],
    };
    expect((
      (game |> Game.end_round).avenue.active_player.current_round_points,
      (game |> Game.end_round).avenue.active_player.previous_round_points,
    ))
    |> toEqual((Some((Farm.A, (-5))), [(Farm.B, (-5))]));
  });
  test("should penalize if round points is lower than previous", () => {
    let game = {
      ...bare_minimum_game,
      avenue: {
        ...bare_minimum_game.avenue,
        stage: Round(A, Four),
        active_player: {
          ...bare_minimum_game.avenue.active_player,
          current_round_points: Some((A, 39)),
          previous_round_points: [(B, 40)],
        },
      },
      log: [(PeekFarm, [])],
    };
    expect((
      (game |> Game.end_round).avenue.active_player.current_round_points,
      (game |> Game.end_round).avenue.active_player.previous_round_points,
    ))
    |> toEqual((Some((Farm.A, (-5))), [(Farm.B, 40)]));
  });
  test("should not penalize first round more than zero", () => {
    let game = {
      ...bare_minimum_game,
      avenue: {
        ...bare_minimum_game.avenue,
        stage: Round(A, Four),
        active_player: {
          ...bare_minimum_game.avenue.active_player,
          current_round_points: Some((A, 1)),
        },
      },
      log: [(PeekFarm, [])],
    };
    expect((game |> Game.end_round).avenue.active_player.current_round_points)
    |> toEqual(Some((Farm.A, 1)));
  });
  test("should not penalize round score more than previous", () => {
    let game = {
      ...bare_minimum_game,
      avenue: {
        ...bare_minimum_game.avenue,
        stage: Round(A, Four),
        active_player: {
          ...bare_minimum_game.avenue.active_player,
          current_round_points: Some((A, 2)),
          previous_round_points: [(B, 1)],
        },
      },
      log: [(PeekFarm, [])],
    };
    expect((
      (game |> Game.end_round).avenue.active_player.current_round_points,
      (game |> Game.end_round).avenue.active_player.previous_round_points,
    ))
    |> toEqual((Some((Farm.A, 2)), [(Farm.B, 1)]));
  });
  test("should penalize round score equal previous", () => {
    let game = {
      ...bare_minimum_game,
      avenue: {
        ...bare_minimum_game.avenue,
        stage: Round(A, Four),
        active_player: {
          ...bare_minimum_game.avenue.active_player,
          current_round_points: Some((A, 1)),
          previous_round_points: [(B, 1)],
        },
      },
      log: [(PeekFarm, [])],
    };
    expect((
      (game |> Game.end_round).avenue.active_player.current_round_points,
      (game |> Game.end_round).avenue.active_player.previous_round_points,
    ))
    |> toEqual((Some((Farm.A, (-5))), [(Farm.B, 1)]));
  });
});