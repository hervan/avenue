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

  let count_grapes =
    fun
    | Grapes(grapes) => grapes |> List.filter(filter_grapes) |> List.length
    | _ => 0;

  let opposing_side =
    fun
    | Top => Bottom
    | Bottom => Top
    | Left => Right
    | Right => Left;

  let same_cell = (c1, c2) => c1.row == c2.row && c1.col == c2.col;

  let path_taken = (path, cell) =>
    List.exists(c => c |> same_cell(cell), path);

  let move_to = (~where=Forward, cell, grid) => {
    let direction =
      switch (where, cell.stretch) {
      | (Forward, Some((_, exit))) => From(exit)
      | (Backward, Some((entry, _))) => From(entry)
      | (From(previous_exit), Some((entry, exit)))
          when previous_exit |> opposing_side == entry =>
        From(exit)
      | (From(previous_exit), Some((exit, entry)))
          when previous_exit |> opposing_side == entry =>
        From(exit)
      | (_, _) => Nowhere
      };
    (
      direction,
      switch (direction) {
      | From(Top) when cell.row > 0 => grid[cell.row - 1][cell.col]
      | From(Right) when cell.col < Array.length(grid[0]) - 1 => grid[cell.
                                                                    row][cell.
                                                                    col
                                                                    + 1]
      | From(Bottom) when cell.row < Array.length(grid) - 1 => grid[cell.row
                                                                    + 1][cell.
                                                                    col]
      | From(Left) when cell.col > 0 => grid[cell.row][cell.col - 1]
      | _ => cell
      },
    );
  };

  let rec walk_path = (~from=Beginning, path, cell, grid) => {
    path_taken(path, cell) || cell.stretch == None
      ? path
      : (
        switch (from) {
        | Beginning =>
          let (from, previous_cell) = move_to(~where=Backward, cell, grid);
          let back_path =
            from == Nowhere
              ? path : walk_path(~from, [cell, ...path], previous_cell, grid);
          let (from, next_cell) = move_to(~where=Forward, cell, grid);
          walk_path(~from, back_path, next_cell, grid);
        | From(_) as where =>
          let (from, next_cell) = move_to(~where, cell, grid);
          walk_path(~from, [cell, ...path], next_cell, grid);
        | _ => path
        }
      );
  };

  walk_path([], cell, grid)
  |> List.fold_left((acc, cell) => acc + count_grapes(cell.content), 0);
};

[@react.component]
let make = (~game as {players} as game) =>
  switch (players) {
  | [] => React.null
  | [{grid, farm_points}, ..._] =>
    let purple_points = count_points(game.castles.purple |> to_pos, grid);
    let green_points = count_points(game.castles.green |> to_pos, grid);
    let total_points =
      farm_points |> List.fold_left((acc, (_, points)) => acc + points, 0);
    <g
      transform="translate(0 77)"
      strokeWidth="0.1"
      stroke="black"
      fillOpacity="1"
      fill="green"
      style={ReactDOMRe.Style.make(
        ~fontSize="3.6",
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
      <text y="6">
        {j|purple: $purple_points, green: $green_points, total: $total_points|j}
        ->str
      </text>
    </g>;
  };