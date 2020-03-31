open Types;
open Converters;

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