open Types;

[@react.component]
let make = (~cell, ~dispatch) =>
  <g
    onClick={_evt => dispatch(DrawRoad(cell.row, cell.col))}
    transform={
      "translate("
      ++ (cell.col * 10)->string_of_int
      ++ " "
      ++ (cell.row * 10)->string_of_int
      ++ ")"
    }>
    <rect
      width="10"
      height="10"
      stroke="green"
      strokeWidth="0.1"
      fillOpacity="0"
    />
    <CellContent content={cell.content} />
    {switch (cell.road) {
     | None => React.null
     | Some(road) => <RoadDraw road pos={Some((cell.row, cell.col))} />
     }}
  </g>;