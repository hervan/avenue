open Common;
open Types;

module Content = {
  [@react.component]
  let make = (~content) =>
    switch (content) {
    | Empty => React.null
    | Grapes(colors) =>
      colors
      |> List.mapi((i, color) => <Grape key={i |> string_of_int} color i />)
      |> arr
    | Castle(color) => <g transform="translate(2 8)"> <Castle color /> </g>
    | Farm(farm) => <g transform="translate(5 9)"> <Farm farm /> </g>
    };
};

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
    <Content content={cell.content} />
    {switch (cell.road) {
     | None => React.null
     | Some(road) =>
       <g strokeWidth="0.5">
         <Road road pos={Some((cell.row, cell.col))} />
       </g>
     }}
  </g>;