open Types;
open Converters;

let count_points = ((row, col), grid) => {
  let cell = grid[row][col];

  let filter_grapes = grape =>
    switch (cell.content) {
    | Castle(color) => grape == color
    | Farm(_) => true
    | _ => false
    };

  let count_grapes_cell =
    fun
    | Grapes(grapes) => grapes |> List.filter(filter_grapes) |> List.length
    | _ => 0;

  let same_cell = (c1, c2) => c1.row == c2.row && c1.col == c2.col;

  let path_taken = (path, cell) =>
    List.exists(cell_ => cell_ |> same_cell(cell), path);

  let count_grapes_path = path =>
    path
    |> List.fold_left(
         (acc, cell) => acc + count_grapes_cell(cell.content),
         0,
       );

  let within_boundaries = (grid, row, col) =>
    row >= 0
    && col >= 0
    && row < (grid |> Array.length)
    && col < (grid[0] |> Array.length);

  let goes_to = (grid, side, {row, col}) =>
    switch (side) {
    | Top when within_boundaries(grid, row - 1, col) => [grid[row - 1][col]]
    | Right when within_boundaries(grid, row, col + 1) => [
        grid[row][col + 1],
      ]
    | Bottom when within_boundaries(grid, row + 1, col) => [
        grid[row + 1][col],
      ]
    | Left when within_boundaries(grid, row, col - 1) => [grid[row][col - 1]]
    | _ => []
    };

  let goes_to_list = (grid, {stretch} as cell) =>
    switch (stretch) {
    | None => []
    | Some((entry, exit)) =>
      List.append(goes_to(grid, entry, cell), goes_to(grid, exit, cell))
    };

  let goes_to_and_comes_from = (grid, cell1, cell2) =>
    goes_to_list(grid, cell1)
    |> List.exists(cell2_ =>
         cell2
         |> same_cell(cell2_)
         && goes_to_list(grid, cell2)
         |> List.exists(cell1_ => cell1 |> same_cell(cell1_))
       );

  let goes_to_and_comes_from_list = (grid, cell) =>
    goes_to_list(grid, cell)
    |> List.filter(cell_ => cell |> goes_to_and_comes_from(grid, cell_));

  let rec connected_path = (grid, path, cell) =>
    path_taken(path, cell) || cell.stretch == None
      ? path
      : goes_to_and_comes_from_list(grid, cell)
        |> List.fold_left(
             (acc_path, neighbour) =>
               connected_path(grid, acc_path, neighbour),
             [cell, ...path],
           );

  connected_path(grid, [], cell) |> count_grapes_path;
};

[@react.component]
let make = (~game as {players} as game) =>
  switch (players) {
  | [] => React.null
  | [{grid, farm_points}, ..._] =>
    let purple_points = count_points(game.castles.purple |> to_pos, grid);
    let green_points = count_points(game.castles.green |> to_pos, grid);
    let total_points =
      (farm_points |> List.fold_left((acc, (_, points)) => acc + points, 0))
      + purple_points
      + green_points;

    let yc = game.yellow_cards;
    <>
      <g
        transform="translate(0 75)"
        strokeWidth="0.1"
        stroke="black"
        fillOpacity="1"
        fill="green"
        style={ReactDOMRe.Style.make(
          ~fontSize="3.6px",
          ~fontFamily="Verdana",
          (),
        )}>
        <text>
          {(
             farm_points
             |> List.rev_map(((farm, int)) =>
                  farm->string_of_farm ++ ": " ++ int->string_of_int
                )
             |> String.concat(", ")
           )
           ->str}
        </text>
        <text y="4">
          {j|purple: $purple_points, green: $green_points, total: $total_points|j}
          ->str
        </text>
      </g>
      <g transform="translate(63 50)">
        <path
          transform="translate(10 10)"
          d={
            "M 0 -5 v 5 v -5"
            ++ (
              yc >= 1
                ? "M 0 -5 A 5 5, 0, 0, 1, 5 0 L 0 0"
                : "M 0 -5 A 5 5, 0, 0, 1, 0 -5 L 0 0"
            )
            ++ (
              yc >= 2
                ? "M 5 0 A 5 5, 0, 0, 1, 0 5 L 0 0"
                : "M 0 -5 A 5 5, 0, 0, 1, 0 -5 L 0 0"
            )
            ++ (
              yc >= 3
                ? "M 0 5 A 5 5, 0, 0, 1, -5 0 L 0 0"
                : "M 0 -5 A 5 5, 0, 0, 1, 0 -5 L 0 0"
            )
            ++ (
              yc == 4
                ? "M -5 0 A 5 5, 0, 0, 1, 0 -5 L 0 0"
                : "M 0 -5 A 5 5, 0, 0, 1, 0 -5 L 0 0"
            )
            ++ "Z"
          }
          fill="yellow"
          stroke="black"
          strokeWidth="0.25"
          style={ReactDOMRe.Style.make(~transition="d 0.5s", ())}
        />
      </g>
    </>;
  };