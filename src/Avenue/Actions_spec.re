open Jest;
open Expect;
open Types;
open Converters;

let game =
  Avenue.(
    create_game(
      "",
      create_base_grid(map_A_grid_contents),
      [
        (road_of_int(0), Yellow),
        (road_of_int(1), Yellow),
        (road_of_int(2), Yellow),
        (road_of_int(3), Yellow),
      ],
      [A, B],
    )
  );

let flip_farm_game = game |> Actions.flip_farm;

let flip_road_game = flip_farm_game |> Actions.flip_road;

let peek_farm_game = flip_road_game |> Actions.peek_farm;

let flip_road2_game = peek_farm_game |> Actions.flip_road;

let draw_road_game = flip_road2_game |> Actions.draw_road(0, 0);

let flip_road3_game = draw_road_game |> Actions.flip_road;

let draw_road3_game = flip_road3_game |> Actions.draw_road(0, 1);

let flip_road4_game = draw_road3_game |> Actions.flip_road;

let draw_road4_game = flip_road4_game |> Actions.draw_road(0, 2);

let end_round_game = draw_road4_game |> Actions.end_round;

let ended_game = end_round_game |> Actions.end_game;

describe("Actions.flip_farm", () => {
  test("should stage be round for top farm", () => {
    expect(flip_farm_game.stage) |> toEqual(Round(A, Zero))
  });

  test("should farm deck be removed from top card", () => {
    expect(flip_farm_game.round_deck) |> toEqual([B])
  });
});

describe("Actions.flip_road", () => {
  test("should current card be the top card", () => {
    expect(flip_road_game.current_card)
    |> toEqual(Some((road_of_int(0), Yellow)))
  });

  test("should road deck be left with rest of deck", () => {
    expect(flip_road_game.deck)
    |> toEqual([
         (road_of_int(1), Yellow),
         (road_of_int(2), Yellow),
         (road_of_int(3), Yellow),
       ])
  });

  test("should game turn be greater than player turn", () => {
    expect(flip_road_game.turn)
    |> toBeGreaterThan((flip_road_game.players |> List.hd).turn)
  });
});

describe("Actions.peek_farm", () => {
  test("should allow player to look ahead", () => {
    expect((peek_farm_game.players |> List.hd).lookahead) |> toEqual(true)
  });

  test("should skip player turn after using", () => {
    expect((peek_farm_game.players |> List.hd).turn) |> toEqual(1)
  });
});

describe("Actions.draw_road", () => {
  test("should road be empty at 0, 0", () => {
    expect((flip_road2_game.players |> List.hd).grid[0][0].road)
    |> toEqual(None)
  });

  test("should road be drawn at 0, 0", () => {
    expect((draw_road_game.players |> List.hd).grid[0][0].road)
    |> toEqual(Some(road_of_int(1)))
  });
});

describe("Actions.end_round", () => {
  test("should stage be end of round", () => {
    expect(end_round_game.stage) |> toEqual(RoundEnd(A))
  })
});

describe("Actions.end_game", () => {
  test("should stage be end of game", () => {
    expect(ended_game.stage) |> toEqual(End(A))
  })
});