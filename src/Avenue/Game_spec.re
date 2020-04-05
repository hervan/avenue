open Jest;
open Expect;

open Types;
open Converters;

let minimal_grid_contents = [|
  [|Castle(Purple), Farm(A), Empty, Farm(B)|],
  [|Farm(C), Empty, Grapes([Green, Green, Green, Purple]), Empty|],
  [|Empty, Grapes([Green, Purple, Purple, Purple]), Empty, Farm(D)|],
  [|Farm(E), Empty, Farm(F), Castle(Green)|],
|];

let minimal_grid = Avenue.create_base_grid(minimal_grid_contents);

let road_deck = [];
// (road_of_int(0), Yellow),
// (road_of_int(0), Yellow),
// (road_of_int(0), Yellow),
// (road_of_int(0), Yellow),
// (road_of_int(0), Yellow),
// (road_of_int(0), Yellow),
// (road_of_int(0), Yellow),
// (road_of_int(0), Yellow),
// (road_of_int(0), Yellow),
// (road_of_int(0), Yellow),
// (road_of_int(0), Yellow),
// (road_of_int(0), Yellow),
// (road_of_int(0), Yellow),
// (road_of_int(0), Yellow),
// (road_of_int(0), Yellow),
// (road_of_int(0), Yellow),
// (road_of_int(0), Yellow),
// (road_of_int(0), Yellow),
// (road_of_int(0), Yellow),
// (road_of_int(0), Yellow),

let farms_deck = [A, B, C, D, E, F];

// describe("Game.recount_points", () => {
//   test("should count purple grapes from purple castle", () => {
//     expect(1) |> toEqual(1)
//   })
// });

// describe("Game.round_penalty", () => {
//   test("should count purple grapes from purple castle", () => {
//     expect(1) |> toEqual(1)
//   })
// });

describe("Game.advance_stage", () => {
  let game_yellow = {
    ...Avenue.create_game("me", minimal_grid, road_deck, farms_deck),
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
    expect(game_yellow.stage) |> toEqual(Begin)
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
    expect(game_round_end_a.stage) |> toEqual(RoundEnd(A))
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
    expect(game_round_end_e.stage) |> toEqual(RoundEnd(E))
  });

  test("should advance game to end E stage", () => {
    expect(game_end_e.stage) |> toEqual(End(E))
  });

  test("should keep the game in end E stage", () => {
    expect(game_end.stage) |> toEqual(End(E))
  });
});