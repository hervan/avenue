open Types;

[@react.component]
let make = (~cell, ~dispatch) => {
  <g
    onClick={_evt => dispatch(DrawStretch(cell.row, cell.col))}
    fillOpacity="0"
    strokeWidth="0.25"
    transform={
      "translate("
      ++ (cell.col * 10)->string_of_int
      ++ " "
      ++ (cell.row * 10)->string_of_int
      ++ ")"
    }>
    <rect width="10" height="10" stroke="green" strokeWidth="0.1" />
    <CellContent content={cell.content} />
    {switch (cell.stretch) {
     | None => React.null
     | Some(stretch) => <StretchDraw stretch />
     }}
  </g>;
};