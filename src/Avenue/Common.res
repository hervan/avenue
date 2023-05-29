exception Impossible(string)

type play_action =
  | FlipFarm
  | FlipRoad
  | PeekFarm
  | DrawRoad(int, int)

type control_action =
  | Create
  | Restart
  | Undo

type action =
  | Play(play_action)
  | Control(control_action)

let str = React.string

let arr = list => list |> Array.of_list |> ReasonReact.array
