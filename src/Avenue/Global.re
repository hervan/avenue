type play_action =
  | FlipFarm
  | FlipRoad
  | PeekFarm
  | DrawRoad(int, int);

type control_action =
  | Start
  | Restart
  | Undo;

type action =
  | Play(play_action)
  | Control(control_action);