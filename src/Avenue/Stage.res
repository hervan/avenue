type flow =
  | Created
  | Ready
  | RoundEnd
  | End

module YellowCards = {
  type t =
    | Zero
    | One
    | Two
    | Three
    | Four

  let to_int = x =>
    switch x {
    | Zero => 0
    | One => 1
    | Two => 2
    | Three => 3
    | Four => 4
    }

  let add = x =>
    switch x {
    | Zero => One
    | One => Two
    | Two => Three
    | Three => Four
    | Four => Four
    }
}

type t =
  | Round(Farm.t, YellowCards.t)
  | Flow(flow)
