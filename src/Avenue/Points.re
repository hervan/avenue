open Types;
open Converters;

let filter_grapes = grape =>
  fun
  | {content: Castle(color)} => grape == color
  | {content: Farm(_)} => true
  | _ => false;

let count_grapes_cell = initial_cell =>
  fun
  | Grapes(grapes) =>
    grapes
    |> List.filter(grape => filter_grapes(grape, initial_cell))
    |> List.length
  | _ => 0;

let same_cell = (c1, c2) => c1.row == c2.row && c1.col == c2.col;

let path_taken = (path, cell) =>
  List.exists(cell_ => cell_ |> same_cell(cell), path);

let count_grapes_path = (path, initial_cell) =>
  path
  |> List.fold_left(
       (acc, cell) => acc + count_grapes_cell(initial_cell, cell.content),
       0,
     );

let within_boundaries = (grid, row, col) =>
  row >= 0
  && col >= 0
  && row < (grid |> Array.length)
  && col < (grid[0] |> Array.length);

let goes_to = (grid, {row, col}) =>
  fun
  | Top when within_boundaries(grid, row - 1, col) => [grid[row - 1][col]]
  | Right when within_boundaries(grid, row, col + 1) => [grid[row][col + 1]]
  | Bottom when within_boundaries(grid, row + 1, col) => [grid[row + 1][col]]
  | Left when within_boundaries(grid, row, col - 1) => [grid[row][col - 1]]
  | _ => [];

let goes_to_list = grid =>
  fun
  | {road: None} => []
  | {road: Some((entry, exit))} as cell =>
    List.append(goes_to(grid, cell, entry), goes_to(grid, cell, exit));

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
  path_taken(path, cell) || cell.road == None
    ? path
    : goes_to_and_comes_from_list(grid, cell)
      |> List.fold_left(
           (acc_path, neighbour) =>
             connected_path(grid, acc_path, neighbour),
           [cell, ...path],
         );

let count_points = ((row, col), grid) =>
  connected_path(grid, [], grid[row][col])
  ->count_grapes_path(grid[row][col]);

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
    let yc =
      switch (game.stage) {
      | Round(_, yc) => yc
      | _ => Zero
      };
    <g transform="translate(62 50)">
      {farm_points->List.rev
       |> List.mapi((i, (farm, points)) =>
            <g
              key={i->string_of_int}
              transform={
                "translate(0, "
                ++ (i->float_of_int *. 2.8)->Js.Float.toString
                ++ ")"
              }>
              <rect width="7" height="2.7" fill="lightgrey" rx="0.5" />
              <g transform="translate(0.5 2.4)">
                <g transform="scale(0.5)"> <Farm farm /> </g>
                <text
                  dx="3.3"
                  x="3"
                  textAnchor="end"
                  fill={
                    points <= 0
                    || i > 0
                    && points
                    <= (
                         List.nth_opt(
                           farm_points,
                           List.length(farm_points) - 1 - i,
                         )
                         |> (
                           fun
                           | Some((_farm, previous_points)) => previous_points
                           | None => 0
                         )
                       )
                      ? "red" : "forestgreen"
                  }
                  style={Theme.text("2.8px")}>
                  {points->string_of_int->str}
                </text>
              </g>
            </g>
          )
       |> arr}
      {game.round_deck
       |> List.tl
       |> List.mapi((i, _) =>
            <g
              key={(i + List.length(farm_points))->string_of_int}
              transform={
                "translate(0, "
                ++ ((i + List.length(farm_points))->float_of_int *. 2.8)
                   ->Js.Float.toString
                ++ ")"
              }>
              <rect width="7" height="2.7" fill="lightgrey" rx="0.5" />
            </g>
          )
       |> arr}
      <g transform={"translate(0, " ++ (5. *. 2.8)->Js.Float.toString ++ ")"}>
        <rect width="7" height="2.7" fill="lightgreen" rx="0.5" />
        <g transform="translate(0.5 2.4)">
          <g transform="scale(0.35)"> <Castle color=Green /> </g>
          <text
            dx="3.3"
            x="3"
            textAnchor="end"
            fill="white"
            style={Theme.text("2.8px")}>
            {green_points->string_of_int->str}
          </text>
        </g>
      </g>
      <g transform={"translate(0, " ++ (6. *. 2.8)->Js.Float.toString ++ ")"}>
        <rect width="7" height="2.7" fill="thistle" rx="0.5" />
        <g transform="translate(0.5 2.4)">
          <g transform="scale(0.35)"> <Castle color=Purple /> </g>
          <text
            dx="3.3"
            x="3"
            textAnchor="end"
            fill="white"
            style={Theme.text("2.8px")}>
            {purple_points->string_of_int->str}
          </text>
        </g>
      </g>
      <g transform="translate(14 5.5)">
        <circle
          key="face"
          cx="0"
          cy="0"
          r="5"
          fill="white"
          stroke="lightgray"
          strokeWidth="0.1"
        />
        <path
          key="hands"
          d={
            yc == Zero ? " M 0 0 v -4 v 4 h 3 h -3" : " M 0 0 v 0 v 0 h 0 h 0"
          }
          stroke="lightgray"
          strokeWidth="0.25"
          style={Theme.quick_transition("d")}
        />
        <path
          key="timer"
          d={
            " M 0 0"
            ++ (
              int_of_yc(yc) >= 1
                ? " M 0 -5 A 5 5, 0, 0, 1, 5 0 L 0 0"
                : " M 0 -5 A 5 5, 0, 0, 1, 0 -5 L 0 0"
            )
            ++ (
              int_of_yc(yc) >= 2
                ? " M 5 0 A 5 5, 0, 0, 1, 0 5 L 0 0"
                : " M 0 -5 A 5 5, 0, 0, 1, 0 -5 L 0 0"
            )
            ++ (
              int_of_yc(yc) >= 3
                ? " M 0 5 A 5 5, 0, 0, 1, -5 0 L 0 0"
                : " M 0 -5 A 5 5, 0, 0, 1, 0 -5 L 0 0"
            )
            ++ (
              int_of_yc(yc) == 4
                ? " M -5 0 A 5 5, 0, 0, 1, 0 -5 L 0 0"
                : " M 0 -5 A 5 5, 0, 0, 1, 0 -5 L 0 0"
            )
            ++ " Z"
          }
          fill="yellow"
          stroke="lightgray"
          strokeWidth={int_of_yc(yc) > 0 ? "0.1" : "0"}
          style={Theme.quick_transition("d")}
        />
      </g>
      <g transform="translate(9 12)">
        <rect width="10" height="7" fill="cornflowerblue" rx="1" />
        <text style={Theme.text("2px")} dy="2" x="5" textAnchor="middle">
          "total"->str
        </text>
        <text
          x="5"
          dy="6"
          textAnchor="middle"
          fill="white"
          style={Theme.text("4px")}>
          {total_points->string_of_int->str}
        </text>
      </g>
    </g>;
  };