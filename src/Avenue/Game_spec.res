open Jest
open Expect

let bare_minimum_game = Game.setup(None, "")

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
      log: list{(Some(PeekFarm), list{})},
    }
    expect((game |> Game.end_round).me.previous_round_points) |> toEqual(list{(Farm.A, -5)})
  })
  test("should penalize if round points is zero even if previous is negative", () => {
    let game = {
      ...bare_minimum_game,
      me: {
        ...bare_minimum_game.me,
        current_round_points: Some((A, 0)),
        previous_round_points: list{(B, -5)},
      },
      avenue: {
        ...bare_minimum_game.avenue,
        stage: Round(A, Four),
      },
      log: list{(Some(PeekFarm), list{})},
    }
    expect((game |> Game.end_round).me.previous_round_points) |> toEqual(list{
      (Farm.A, -5),
      (Farm.B, -5),
    })
  })
  test("should penalize if round points is lower than previous", () => {
    let game = {
      ...bare_minimum_game,
      me: {
        ...bare_minimum_game.me,
        current_round_points: Some((A, 39)),
        previous_round_points: list{(B, 40)},
      },
      avenue: {
        ...bare_minimum_game.avenue,
        stage: Round(A, Four),
      },
      log: list{(Some(PeekFarm), list{})},
    }
    expect((game |> Game.end_round).me.previous_round_points) |> toEqual(list{
      (Farm.A, -5),
      (Farm.B, 40),
    })
  })
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
      log: list{(Some(PeekFarm), list{})},
    }
    expect((game |> Game.end_round).me.previous_round_points) |> toEqual(list{(Farm.A, 1)})
  })
  test("should not penalize round score more than previous", () => {
    let game = {
      ...bare_minimum_game,
      me: {
        ...bare_minimum_game.me,
        current_round_points: Some((A, 2)),
        previous_round_points: list{(B, 1)},
      },
      avenue: {
        ...bare_minimum_game.avenue,
        stage: Round(A, Four),
      },
      log: list{(Some(PeekFarm), list{})},
    }
    expect((game |> Game.end_round).me.previous_round_points) |> toEqual(list{
      (Farm.A, 2),
      (Farm.B, 1),
    })
  })
  test("should penalize round score equal previous", () => {
    let game = {
      ...bare_minimum_game,
      me: {
        ...bare_minimum_game.me,
        current_round_points: Some((A, 1)),
        previous_round_points: list{(B, 1)},
      },
      avenue: {
        ...bare_minimum_game.avenue,
        stage: Round(A, Four),
      },
      log: list{(Some(PeekFarm), list{})},
    }
    expect((game |> Game.end_round).me.previous_round_points) |> toEqual(list{
      (Farm.A, -5),
      (Farm.B, 1),
    })
  })
})
