open Jest;
open Expect;
open Types;

describe("Points.count_grapes_cell", () => {
  let two_green_one_purple_grapes = Grapes([Green, Green, Purple]);
  let empty_cell: cell = {row: 0, col: 0, content: Empty, road: None};

  test("should count purple grapes from purple castle", () => {
    expect(
      Points.count_grapes_cell(
        {...empty_cell, content: Castle(Purple)},
        two_green_one_purple_grapes,
      ),
    )
    |> toEqual(1)
  });

  test("should count green grapes from green castle", () => {
    expect(
      Points.count_grapes_cell(
        {...empty_cell, content: Castle(Green)},
        two_green_one_purple_grapes,
      ),
    )
    |> toEqual(2)
  });

  test("should count all grapes from farm cell", () => {
    expect(
      Points.count_grapes_cell(
        {...empty_cell, content: Farm(A)},
        two_green_one_purple_grapes,
      ),
    )
    |> toEqual(3)
  });

  test("should count no grapes from grapes cell", () => {
    expect(
      Points.count_grapes_cell(
        {...empty_cell, content: two_green_one_purple_grapes},
        two_green_one_purple_grapes,
      ),
    )
    |> toEqual(0)
  });

  test("should count no grapes from empty cell", () => {
    expect(Points.count_grapes_cell(empty_cell, two_green_one_purple_grapes))
    |> toEqual(0)
  });
});

describe("Points.count_points", () => {
  let full_grid =
    Avenue.create_base_grid(Avenue.map_A_grid_contents)
    |> Array.mapi((i, row) =>
         row
         |> Array.mapi((j, cell) =>
              {
                ...cell,
                road:
                  Some((
                    j != 0 ? Left : i mod 2 == 0 ? Top : Bottom,
                    j != 5 ? Right : i mod 2 == 0 ? Bottom : Top,
                  )),
              }
            )
       );

  let one_line_grid = [|full_grid[0]|];

  test("should count no grapes from empty", () => {
    expect(Points.count_points((0, 4), one_line_grid)) |> toEqual(0)
  });

  test("should count no grapes from grapes", () => {
    expect(Points.count_points((0, 0), one_line_grid)) |> toEqual(0)
  });

  test("should count all grapes from farm", () => {
    expect(Points.count_points((0, 2), one_line_grid)) |> toEqual(7)
  });

  test("should count green grapes from green castle", () => {
    expect(Points.count_points((0, 5), one_line_grid)) |> toEqual(5)
  });

  test("should count no grapes from empty", () => {
    expect(Points.count_points((0, 4), full_grid)) |> toEqual(0)
  });

  test("should count no grapes from grapes", () => {
    expect(Points.count_points((0, 0), full_grid)) |> toEqual(0)
  });

  test("should count all grapes from farm", () => {
    expect(Points.count_points((0, 2), full_grid)) |> toEqual(48)
  });

  test("should count green grapes from green castle", () => {
    expect(Points.count_points((0, 5), full_grid)) |> toEqual(24)
  });

  test("should count purple grapes from purple castle", () => {
    expect(Points.count_points((6, 0), full_grid)) |> toEqual(24)
  });
});