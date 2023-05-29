open Jest
open Expect

let minimal_grid_contents_without_empty = [
  [Cell.Content.Castle(Purple), Farm(A), Farm(B)],
  [Farm(C), Grapes(list{Green, Green, Green, Purple}), Farm(D)],
  [Farm(E), Farm(F), Castle(Green)],
]

let minimal_grid_without_empty = Grid.setup(minimal_grid_contents_without_empty)

let road_deck = list{}

let farm_deck = {
  open Farm
  list{A, B, C, D, E, F}
}

describe("Player.recount_points", () => {
  let connected_grid = minimal_grid_without_empty
  connected_grid[0][1] = {
    ...connected_grid[0][1],
    road: Some(Road.of_int(2)),
  }
  connected_grid[0][2] = {
    ...connected_grid[0][2],
    road: Some(Road.of_int(3)),
  }
  connected_grid[1][0] = {
    ...connected_grid[1][0],
    road: Some(Road.of_int(2)),
  }
  connected_grid[1][1] = {
    ...connected_grid[1][1],
    road: Some(Road.of_int(0)),
  }
  connected_grid[1][2] = {
    ...connected_grid[1][2],
    road: Some(Road.of_int(5)),
  }
  connected_grid[2][0] = {
    ...connected_grid[2][0],
    road: Some(Road.of_int(1)),
  }
  connected_grid[2][1] = {
    ...connected_grid[2][1],
    road: Some(Road.of_int(4)),
  }
  connected_grid[2][2] = {
    ...connected_grid[2][2],
    road: Some(Road.of_int(0)),
  }

  let base_game = Game.load_setup(None, "me", connected_grid, road_deck, farm_deck)

  let farms = base_game.me.farms

  let player_round_a = {
    ...base_game.me,
    current_round_points: Some((A, 0)),
    previous_round_points: list{},
  } |> Player.recount_points(farms)

  let previous_round_points_round_b = switch player_round_a.current_round_points {
  | Some(points) => list{points, ...player_round_a.previous_round_points}
  | None => player_round_a.previous_round_points
  }

  let player_round_b = {
    ...player_round_a,
    current_round_points: Some((B, 0)),
    previous_round_points: previous_round_points_round_b,
  }

  let player_round_b_recounted = player_round_b |> Player.recount_points(farms)

  test("should have correct points for round A", () =>
    expect(player_round_a.current_round_points) |> toEqual(Some((Farm.A, 4)))
  )

  test("should have correct points for round B", () =>
    expect((player_round_b.current_round_points, player_round_b.previous_round_points)) |> toEqual((
      Some((Farm.B, 0)),
      list{(Farm.A, 4)},
    ))
  )

  test("should recount correct points for round B", () =>
    expect((
      player_round_b_recounted.current_round_points,
      player_round_b_recounted.previous_round_points,
    )) |> toEqual((Some((Farm.B, 4)), list{(Farm.A, 4)}))
  )
})
