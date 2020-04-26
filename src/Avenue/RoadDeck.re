open Common;

let grid_columns = 6;
let grid_rows = 7;

let setup = () => {
  let rec aux = (road_deck, available_cards) => {
    let (road, color) = (Random.int(6), Random.int(2));
    List.length(road_deck) == grid_columns * grid_rows
      ? road_deck
      : available_cards[road][color] == 0
          ? aux(road_deck, available_cards)
          : {
            available_cards[road][color] = available_cards[road][color] - 1;
            aux(
              [Road.Card.card_of_ints(road, color), ...road_deck],
              available_cards,
            );
          };
  };
  Random.self_init();
  aux(
    [],
    [|[|4, 3|], [|4, 3|], [|4, 3|], [|4, 3|], [|3, 4|], [|3, 4|]|],
  );
};

[@react.component]
let make = (~road_deck, ~current_card, ~dispatch) => {
  let (rotation, setRotation) = React.useState(_ => 90);
  React.useEffect1(
    _ => {
      switch (current_card) {
      | None => setRotation(_ => 90)
      | Some(_) => setRotation(_ => 0)
      };
      None;
    },
    [|current_card|],
  );
  <g
    transform="translate(63 0)"
    onClick={_evt => {
      setRotation(_ => 90);
      let _ =
        Js.Global.setTimeout(
          _ => {
            dispatch(Avenue.FlipRoad);
            setRotation(_ => 0);
          },
          500,
        );
      ();
    }}>
    {road_deck
     |> List.mapi((i, _) =>
          <rect
            key={i |> string_of_int}
            x={
              Theme.road_card_thickness
              *. (i |> float_of_int)
              |> Js.Float.toString
            }
            y={
              Theme.road_card_thickness
              *. (i |> float_of_int)
              |> Js.Float.toString
            }
            width="15"
            height="20"
            rx="2"
            fill="lightblue"
            stroke="white"
            strokeWidth="1"
            style=Theme.road_shadow
          />
        )
     |> arr}
    {switch (current_card) {
     | None =>
       <g style={Theme.rotate_card(rotation)}>
         <rect
           x={
             Theme.road_card_thickness
             *. (List.length(road_deck) |> float_of_int)
             |> Js.Float.toString
           }
           y={
             Theme.road_card_thickness
             *. (List.length(road_deck) |> float_of_int)
             |> Js.Float.toString
           }
           width="15"
           height="20"
           rx="2"
           fill="lightblue"
           stroke="white"
           strokeWidth="1"
           style=Theme.road_shadow
         />
       </g>
     | Some((road, color)) =>
       <g style={Theme.rotate_card(rotation)}>
         <rect
           x={
             Theme.road_card_thickness
             *. (List.length(road_deck) |> float_of_int)
             |> Js.Float.toString
           }
           y={
             Theme.road_card_thickness
             *. (List.length(road_deck) |> float_of_int)
             |> Js.Float.toString
           }
           width="15"
           height="20"
           rx="2"
           fill={color->Road.Card.string_of_color}
           stroke="white"
           strokeWidth="1"
           style=Theme.road_shadow
         />
         <g
           transform={
             "translate("
             ++ (
               2.5
               +. Theme.road_card_thickness
               *. (List.length(road_deck) |> float_of_int)
               |> Js.Float.toString
             )
             ++ " "
             ++ (
               5.0
               +. Theme.road_card_thickness
               *. (List.length(road_deck) |> float_of_int)
               |> Js.Float.toString
             )
             ++ ")"
           }>
           <rect
             width="10"
             height="10"
             rx="1"
             fill="white"
             stroke="white"
             strokeWidth="0"
             style=Theme.shadow
           />
           <g strokeWidth="1"> <Road road pos=None /> </g>
         </g>
       </g>
     }}
  </g>;
};