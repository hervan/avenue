open Jest;
open Expect;

let game =
  Game.(
    create_game(
      "",
      create_base_grid(map_A_grid_contents),
      [
        (Road.road_of_int(0), Yellow),
        (Road.road_of_int(1), Yellow),
        (Road.road_of_int(2), Yellow),
        (Road.road_of_int(3), Yellow),
      ],
      [A, B],
    )
  );

let flip_farm_game = game |> Game.flip_farm;

let flip_road_game = flip_farm_game |> Game.flip_road;

let peek_farm_game = flip_road_game |> Game.peek_farm;

let draw_road_game = peek_farm_game |> Game.draw_road(0, 0);

let flip_road2_game = draw_road_game |> Game.flip_road;

let draw_road2_game = flip_road2_game |> Game.draw_road(0, 1);

let flip_road3_game = draw_road2_game |> Game.flip_road;

let draw_road3_game = flip_road3_game |> Game.draw_road(0, 2);

let flip_road4_game = draw_road3_game |> Game.flip_road;

let draw_road4_game = flip_road4_game |> Game.draw_road(0, 3);

let end_round_game = draw_road4_game |> Game.end_round;

let ended_game = end_round_game |> Game.end_game;

let allow_peek_game = {
  ...flip_road_game,
  avenue: {
    ...flip_road_game.avenue,
    farm_deck: [B, C],
  },
};

let poke_game = allow_peek_game |> Game.peek_farm;

describe("Actions.flip_farm", () => {
  test("should stage be round for top farm", () => {
    expect(flip_farm_game.avenue.stage) |> toEqual(Stage.Round(A, Zero))
  });

  test("should farm deck be removed from top card", () => {
    expect(flip_farm_game.avenue.farm_deck) |> toEqual([Farm.B])
  });
});

describe("Actions.flip_road", () => {
  test("should current card be the top card", () => {
    expect(flip_road_game.avenue.current_card)
    |> toEqual(Some((Road.road_of_int(0), Road.Card.Yellow)))
  });

  test("should road deck be left with rest of deck", () => {
    expect(flip_road_game.avenue.road_deck)
    |> toEqual([
         (Road.road_of_int(1), Road.Card.Yellow),
         (Road.road_of_int(2), Road.Card.Yellow),
         (Road.road_of_int(3), Road.Card.Yellow),
       ])
  });

  test("should game turn be greater than player turn", () => {
    expect(flip_road_game.avenue.turn)
    |> toBeGreaterThan(flip_road_game.me.turn)
  });
});

describe("Actions.peek_farm", () => {
  test("should not allow player to look ahead", () => {
    expect(peek_farm_game.me.lookahead) |> toEqual(false)
  });

  test("should not skip player turn after trying to use peek", () => {
    expect(peek_farm_game.me.turn)
    |> toBeLessThan(peek_farm_game.avenue.turn)
  });

  test("should allow player to look ahead", () => {
    expect(poke_game.me.lookahead) |> toEqual(true)
  });

  test("should skip player turn after using", () => {
    expect(poke_game.me.turn) |> toEqual(poke_game.avenue.turn)
  });
});

describe("Actions.draw_road", () => {
  test("should road be empty at 0, 0", () => {
    expect(peek_farm_game.me.grid[0][0].road) |> toEqual(None)
  });

  test("should road be drawn at 0, 0", () => {
    expect(draw_road_game.me.grid[0][0].road)
    |> toEqual(Some(Road.road_of_int(0)))
  });
});

describe("Actions.end_round", () => {
  test("should stage be end of round", () => {
    expect(end_round_game.avenue.stage) |> toEqual(Stage.Flow(RoundEnd))
  })
});

describe("Actions.end_game", () => {
  test("should stage be end of game", () => {
    expect(ended_game.avenue.stage) |> toEqual(Stage.Flow(End))
  })
});