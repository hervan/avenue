open Jest;
open Expect;

let map_A_grid = Game.create_base_grid(Game.map_A_grid_contents);

describe("Game.find_content", () => {
  test("should find the green castle in map A at (0, 5)", () => {
    expect(Game.find_content(Castle(Green), map_A_grid))
    |> toEqual({Cell.row: 0, col: 5, content: Castle(Green), road: None})
  });

  test("should find the purple castle in map A at (6, 0)", () => {
    expect(Game.find_content(Castle(Purple), map_A_grid))
    |> toEqual({Cell.row: 6, col: 0, content: Castle(Purple), road: None})
  });

  test("should find farm A in map A at (0, 2)", () => {
    expect(Game.find_content(Farm(A), map_A_grid))
    |> toEqual({Cell.row: 0, col: 2, content: Farm(A), road: None})
  });

  test("should find farm B in map A at (2, 3)", () => {
    expect(Game.find_content(Farm(B), map_A_grid))
    |> toEqual({Cell.row: 2, col: 3, content: Farm(B), road: None})
  });

  test("should find farm C in map A at (3, 0)", () => {
    expect(Game.find_content(Farm(C), map_A_grid))
    |> toEqual({Cell.row: 3, col: 0, content: Farm(C), road: None})
  });

  test("should find farm D in map A at (3, 5)", () => {
    expect(Game.find_content(Farm(D), map_A_grid))
    |> toEqual({Cell.row: 3, col: 5, content: Farm(D), road: None})
  });

  test("should find farm E in map A at (4, 2)", () => {
    expect(Game.find_content(Farm(E), map_A_grid))
    |> toEqual({Cell.row: 4, col: 2, content: Farm(E), road: None})
  });

  test("should find farm F in map A at (6, 3)", () => {
    expect(Game.find_content(Farm(F), map_A_grid))
    |> toEqual({Cell.row: 6, col: 3, content: Farm(F), road: None})
  });
});