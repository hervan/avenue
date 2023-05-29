type t = array<array<Cell.t>>

let setup = contents =>
  Array.init(contents |> Array.length, row =>
    Array.init(contents[0] |> Array.length, col => {
      Cell.row: row,
      col: col,
      content: contents[row][col],
      road: None,
    })
  )

let draw_road = (road, row, col, x) =>
  switch x {
  | t =>
    t |> Array.mapi((i, grid_row) =>
      i == row
        ? grid_row |> Array.mapi((j, cell) => j == col ? {...cell, Cell.road: Some(road)} : cell)
        : grid_row
    )
  }

let find = (cell_content, grid) =>
  grid
  |> Array.to_list
  |> List.map(row =>
    row |> Array.to_list |> List.filter(({Cell.content: content}) => content == cell_content)
  )
  |> List.concat
  |> List.hd

let map_A = [
  [
    Cell.Content.Grapes(list{Green, Green, Green, Purple}),
    Grapes(list{Purple}),
    Farm(A),
    Grapes(list{Green, Green}),
    Empty,
    Castle(Green),
  ],
  [
    Grapes(list{Purple}),
    Grapes(list{Green, Green}),
    Grapes(list{Green}),
    Grapes(list{Purple}),
    Grapes(list{Green, Green, Purple}),
    Empty,
  ],
  [
    Grapes(list{Green}),
    Empty,
    Grapes(list{Purple, Purple, Green}),
    Farm(B),
    Grapes(list{Green}),
    Grapes(list{Green, Green}),
  ],
  [
    Farm(C),
    Grapes(list{Purple, Purple}),
    Grapes(list{Green, Green}),
    Empty,
    Grapes(list{Purple, Purple}),
    Farm(D),
  ],
  [
    Grapes(list{Purple, Purple}),
    Empty,
    Farm(E),
    Grapes(list{Purple}),
    Grapes(list{Green, Green, Purple}),
    Grapes(list{Purple}),
  ],
  [
    Empty,
    Grapes(list{Purple, Purple, Green}),
    Grapes(list{Green}),
    Grapes(list{Purple, Purple}),
    Grapes(list{Green}),
    Empty,
  ],
  [
    Castle(Purple),
    Grapes(list{Green}),
    Grapes(list{Purple}),
    Farm(F),
    Empty,
    Grapes(list{Purple, Purple, Purple, Green}),
  ],
]
