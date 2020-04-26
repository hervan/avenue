open Jest;
open Expect;

let map_A_grid = Grid.setup(Grid.map_A);

describe("Grid.find", () => {
  test("should find the green castle in map A at (0, 5)", () => {
    expect(Grid.find(Castle(Green), map_A_grid))
    |> toEqual({Cell.row: 0, col: 5, content: Castle(Green), road: None})
  });

  test("should find the purple castle in map A at (6, 0)", () => {
    expect(Grid.find(Castle(Purple), map_A_grid))
    |> toEqual({Cell.row: 6, col: 0, content: Castle(Purple), road: None})
  });

  test("should find farm A in map A at (0, 2)", () => {
    expect(Grid.find(Farm(A), map_A_grid))
    |> toEqual({Cell.row: 0, col: 2, content: Farm(A), road: None})
  });

  test("should find farm B in map A at (2, 3)", () => {
    expect(Grid.find(Farm(B), map_A_grid))
    |> toEqual({Cell.row: 2, col: 3, content: Farm(B), road: None})
  });

  test("should find farm C in map A at (3, 0)", () => {
    expect(Grid.find(Farm(C), map_A_grid))
    |> toEqual({Cell.row: 3, col: 0, content: Farm(C), road: None})
  });

  test("should find farm D in map A at (3, 5)", () => {
    expect(Grid.find(Farm(D), map_A_grid))
    |> toEqual({Cell.row: 3, col: 5, content: Farm(D), road: None})
  });

  test("should find farm E in map A at (4, 2)", () => {
    expect(Grid.find(Farm(E), map_A_grid))
    |> toEqual({Cell.row: 4, col: 2, content: Farm(E), road: None})
  });

  test("should find farm F in map A at (6, 3)", () => {
    expect(Grid.find(Farm(F), map_A_grid))
    |> toEqual({Cell.row: 6, col: 3, content: Farm(F), road: None})
  });
});