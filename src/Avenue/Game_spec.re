open Jest;
open Expect;
open Types;
open Converters;

let bare_minimum_game =
  {
    players: [
      {farmer: "", lookahead: false, grid: [||], turn: 0, farm_points: []},
    ],
    deck: [],
    turn: 0,
    round_deck: [],
    stage: Flow(Created),
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
    log: [],
    guide: [],
  }
  |> Actions.start_game;

let minimal_grid_contents = [|
  [|Castle(Purple), Farm(A), Empty, Farm(B)|],
  [|Farm(C), Empty, Grapes([Green, Green, Green, Purple]), Empty|],
  [|Empty, Grapes([Green, Purple, Purple, Purple]), Empty, Farm(D)|],
  [|Farm(E), Empty, Farm(F), Castle(Green)|],
|];

let minimal_grid = Avenue.create_base_grid(minimal_grid_contents);

let minimal_grid_contents_without_empty = [|
  [|Castle(Purple), Farm(A), Farm(B)|],
  [|Farm(C), Grapes([Green, Green, Green, Purple]), Farm(D)|],
  [|Farm(E), Farm(F), Castle(Green)|],
|];

let minimal_grid_without_empty =
  Avenue.create_base_grid(minimal_grid_contents_without_empty);

let road_deck = [];

let farm_deck = [A, B, C, D, E, F];

describe("Game.advance_stage", () => {
  let game_yellow = {
    ...Avenue.create_game("me", minimal_grid, road_deck, farm_deck),
    current_card: Some((road_of_int(0), Yellow)),
  };
  let game_a_0 = game_yellow |> Game.advance_stage |> Game.discard_top_farm;
  let game_a_1 = game_a_0 |> Game.advance_stage;
  let game_a_1_grey = {
    ...game_a_1,
    current_card: Some((road_of_int(0), Grey)),
  };
  let game_a_1_yet = game_a_1_grey |> Game.advance_stage;
  let game_a_4 =
    game_a_1 |> Game.advance_stage |> Game.advance_stage |> Game.advance_stage;
  let game_round_end_a = game_a_4 |> Game.advance_stage;
  let game_b_0 =
    game_round_end_a |> Game.advance_stage |> Game.discard_top_farm;
  let game_e_0 =
    game_round_end_a
    |> Game.discard_top_farm
    |> Game.discard_top_farm
    |> Game.discard_top_farm
    |> Game.advance_stage
    |> Game.discard_top_farm;
  let game_e_4 =
    game_e_0
    |> Game.advance_stage
    |> Game.advance_stage
    |> Game.advance_stage
    |> Game.advance_stage;
  let game_round_end_e = game_e_4 |> Game.advance_stage;
  let game_end_e = game_round_end_e |> Game.advance_stage;
  let game_end = game_end_e |> Game.advance_stage;

  test("should begin game with Begin stage", () => {
    expect(game_yellow.stage) |> toEqual(Flow(Begin))
  });

  test("should advance game to farm A, 0 yellow cards stage", () => {
    expect(game_a_0.stage) |> toEqual(Round(A, Zero))
  });

  test("should remove top farm card from deck", () => {
    expect(game_a_0.round_deck |> List.hd) |> toEqual(B)
  });

  test("should advance game to farm A, 1 yellow card stage", () => {
    expect(game_a_1.stage) |> toEqual(Round(A, One))
  });

  test("should game be in farm A, 1 yellow card stage", () => {
    expect(game_a_1_grey.stage) |> toEqual(Round(A, One))
  });

  test("should current card be grey", () => {
    expect(game_a_1_grey.current_card)
    |> toEqual(Some((road_of_int(0), Grey)))
  });

  test("should keep the game in farm A, 1 yellow card stage", () => {
    expect(game_a_1_yet.stage) |> toEqual(Round(A, One))
  });

  test("should advance game to farm A, 4 yellow card stage", () => {
    expect(game_a_4.stage) |> toEqual(Round(A, Four))
  });

  test("should advance game to round end A stage", () => {
    expect(game_round_end_a.stage) |> toEqual(Flow(RoundEnd))
  });

  test("should advance game to round B, 0 yellow cards stage", () => {
    expect(game_b_0.stage) |> toEqual(Round(B, Zero))
  });

  test("should advance game to round E, 0 yellow cards stage", () => {
    expect(game_e_0.stage) |> toEqual(Round(E, Zero))
  });

  test("should advance game to round E, 4 yellow cards stage", () => {
    expect(game_e_4.stage) |> toEqual(Round(E, Four))
  });

  test("should advance game to round end E stage", () => {
    expect(game_round_end_e.stage) |> toEqual(Flow(RoundEnd))
  });

  test("should advance game to end E stage", () => {
    expect(game_end_e.stage) |> toEqual(Flow(End))
  });

  test("should keep the game in end E stage", () => {
    expect(game_end.stage) |> toEqual(Flow(End))
  });
});

describe("Game.recount_points", () => {
  let connected_grid = minimal_grid_without_empty;
  connected_grid[0][1] = {
    ...connected_grid[0][1],
    road: Some(road_of_int(2)),
  };
  connected_grid[0][2] = {
    ...connected_grid[0][2],
    road: Some(road_of_int(3)),
  };
  connected_grid[1][0] = {
    ...connected_grid[1][0],
    road: Some(road_of_int(2)),
  };
  connected_grid[1][1] = {
    ...connected_grid[1][1],
    road: Some(road_of_int(0)),
  };
  connected_grid[1][2] = {
    ...connected_grid[1][2],
    road: Some(road_of_int(5)),
  };
  connected_grid[2][0] = {
    ...connected_grid[2][0],
    road: Some(road_of_int(1)),
  };
  connected_grid[2][1] = {
    ...connected_grid[2][1],
    road: Some(road_of_int(4)),
  };
  connected_grid[2][2] = {
    ...connected_grid[2][2],
    road: Some(road_of_int(0)),
  };

  let base_game =
    Avenue.create_game("me", connected_grid, road_deck, farm_deck);

  let game_round_a =
    {
      ...base_game,
      stage: Round(A, Zero),
      players: [{...base_game.players |> List.hd, farm_points: [(A, 0)]}],
    }
    |> Game.recount_points;

  let me_round_a = game_round_a.players |> List.hd;

  let game_round_b = {
    ...game_round_a,
    stage: Round(B, Zero),
    players: [
      {...me_round_a, farm_points: [(B, 0), ...me_round_a.farm_points]},
    ],
  };

  let game_round_b_recounted = game_round_b |> Game.recount_points;

  test("should have correct points for round A", () => {
    expect((game_round_a.players |> List.hd).farm_points)
    |> toEqual([(A, 4)])
  });

  test("should have correct points for round B", () => {
    expect((game_round_b.players |> List.hd).farm_points)
    |> toEqual([(B, 0), (A, 4)])
  });

  test("should recount correct points for round B", () => {
    expect((game_round_b_recounted.players |> List.hd).farm_points)
    |> toEqual([(B, 4), (A, 4)])
  });
});

describe("Game.round_penalty", () => {
  test("should penalize if round points is zero", () => {
    let game = {
      ...bare_minimum_game,
      stage: Round(A, Zero),
      players: [
        {...bare_minimum_game.players |> List.hd, farm_points: [(A, 0)]},
      ],
    };
    expect(((game |> Game.round_penalty).players |> List.hd).farm_points)
    |> toEqual([(A, (-5))]);
  });

  test(
    "should penalize if round points is zero even if previous is negative", () => {
    let game = {
      ...bare_minimum_game,
      stage: Round(A, Zero),
      players: [
        {
          ...bare_minimum_game.players |> List.hd,
          farm_points: [(A, 0), (B, (-5))],
        },
      ],
    };
    expect(((game |> Game.round_penalty).players |> List.hd).farm_points)
    |> toEqual([(A, (-5)), (B, (-5))]);
  });

  test("should penalize if round points is lower than previous", () => {
    let game = {
      ...bare_minimum_game,
      stage: Round(A, Zero),
      players: [
        {
          ...bare_minimum_game.players |> List.hd,
          farm_points: [(A, 39), (B, 40)],
        },
      ],
    };
    expect(((game |> Game.round_penalty).players |> List.hd).farm_points)
    |> toEqual([(A, (-5)), (B, 40)]);
  });

  test("should not penalize first round more than zero", () => {
    let game = {
      ...bare_minimum_game,
      stage: Round(A, Zero),
      players: [
        {...bare_minimum_game.players |> List.hd, farm_points: [(A, 1)]},
      ],
    };
    expect(((game |> Game.round_penalty).players |> List.hd).farm_points)
    |> toEqual([(A, 1)]);
  });

  test("should not penalize round score more than previous", () => {
    let game = {
      ...bare_minimum_game,
      stage: Round(A, Zero),
      players: [
        {
          ...bare_minimum_game.players |> List.hd,
          farm_points: [(A, 2), (B, 1)],
        },
      ],
    };
    expect(((game |> Game.round_penalty).players |> List.hd).farm_points)
    |> toEqual([(A, 2), (B, 1)]);
  });

  test("should penalize round score equal previous", () => {
    let game = {
      ...bare_minimum_game,
      stage: Round(A, Zero),
      players: [
        {
          ...bare_minimum_game.players |> List.hd,
          farm_points: [(A, 1), (B, 1)],
        },
      ],
    };
    expect(((game |> Game.round_penalty).players |> List.hd).farm_points)
    |> toEqual([(A, (-5)), (B, 1)]);
  });
});