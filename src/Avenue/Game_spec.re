open Jest;
open Expect;

let bare_minimum_game = {
  me: {
    farmer: "",
    turn: 0,
    lookahead: false,
    grid: [||],
    farms: [],
    current_round_points: None,
    previous_round_points: [],
  },
  players: [],
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
  },
  log: [],
  guide: [],
};

describe("Game.round_penalty", () => {
  test("should penalize if round points is zero", () => {
    let game = {
      ...bare_minimum_game,
      me: {
        ...bare_minimum_game.me,
        current_round_points: Some((A, 0)),
      },
      avenue: {
        ...bare_minimum_game.avenue,
        stage: Round(A, Four),
      },
      log: [(PeekFarm, [])],
    };
    expect((game |> Game.end_round).me.current_round_points)
    |> toEqual(Some((Farm.A, (-5))));
  });
  test(
    "should penalize if round points is zero even if previous is negative", () => {
    let game = {
      ...bare_minimum_game,
      me: {
        ...bare_minimum_game.me,
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
      (game |> Game.end_round).me.current_round_points,
      (game |> Game.end_round).me.previous_round_points,
    ))
    |> toEqual((Some((Farm.A, (-5))), [(Farm.B, (-5))]));
  });
  test("should penalize if round points is lower than previous", () => {
    let game = {
      ...bare_minimum_game,
      me: {
        ...bare_minimum_game.me,
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
      (game |> Game.end_round).me.current_round_points,
      (game |> Game.end_round).me.previous_round_points,
    ))
    |> toEqual((Some((Farm.A, (-5))), [(Farm.B, 40)]));
  });
  test("should not penalize first round more than zero", () => {
    let game = {
      ...bare_minimum_game,
      me: {
        ...bare_minimum_game.me,
        current_round_points: Some((A, 1)),
      },
      avenue: {
        ...bare_minimum_game.avenue,
        stage: Round(A, Four),
      },
      log: [(PeekFarm, [])],
    };
    expect((game |> Game.end_round).me.current_round_points)
    |> toEqual(Some((Farm.A, 1)));
  });
  test("should not penalize round score more than previous", () => {
    let game = {
      ...bare_minimum_game,
      me: {
        ...bare_minimum_game.me,
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
      (game |> Game.end_round).me.current_round_points,
      (game |> Game.end_round).me.previous_round_points,
    ))
    |> toEqual((Some((Farm.A, 2)), [(Farm.B, 1)]));
  });
  test("should penalize round score equal previous", () => {
    let game = {
      ...bare_minimum_game,
      me: {
        ...bare_minimum_game.me,
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
      (game |> Game.end_round).me.current_round_points,
      (game |> Game.end_round).me.previous_round_points,
    ))
    |> toEqual((Some((Farm.A, (-5))), [(Farm.B, 1)]));
  });
});