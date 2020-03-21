open Types;

[@react.component]
let make = (~content) => {
  switch (content) {
  | Empty => React.null
  | Grapes(colors) =>
    colors
    |> List.mapi((i, color) => <Grape key={i |> string_of_int} color i />)
    |> Array.of_list
    |> ReasonReact.array
  | Castle(color) => <Castle color />
  | Farm(farm) => <Farm farm />
  };
};