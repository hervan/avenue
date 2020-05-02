open Jest;
open Expect;

let minimal_grid_contents = [|
  [|Cell.Content.Castle(Purple), Farm(A), Empty, Farm(B)|],
  [|Farm(C), Empty, Grapes([Green, Green, Green, Purple]), Empty|],
  [|Empty, Grapes([Green, Purple, Purple, Purple]), Empty, Farm(D)|],
  [|Farm(E), Empty, Farm(F), Castle(Green)|],
|];

let minimal_grid = Grid.setup(minimal_grid_contents);

let road_deck = [];

let farm_deck = Farm.[A, B, C, D, E, F];

describe("Avenue.advance_stage", () => {
  let base_game =
    Game.load_setup(Some(0), "me", minimal_grid, road_deck, farm_deck)
    |> Game.begin_game;
  let game_yellow = {
    ...base_game.avenue,
    current_card: Some((Road.of_int(0), Yellow)),
  };
  let game_a_0 =
    game_yellow |> Avenue.advance_stage |> Avenue.discard_top_farm;
  let game_a_1 = game_a_0 |> Avenue.advance_stage;
  let game_a_1_grey = {
    ...game_a_1,
    current_card: Some((Road.of_int(0), Grey)),
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
    |> toEqual(Some((Road.of_int(0), Road.Card.Grey)))
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