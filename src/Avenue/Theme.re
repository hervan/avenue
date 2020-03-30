let text = fontSize =>
  ReactDOMRe.Style.make(
    ~fontSize,
    ~fontFamily="Verdana",
    ~userSelect="none",
    (),
  );

let big_text =
  text("8px")
  |> ReactDOMRe.Style.combine(ReactDOMRe.Style.make(~fontWeight="bold", ()));

let quick_transition = property =>
  ReactDOMRe.Style.make(~transition={j|$property 0.5s|j}, ());

let no_transition = ReactDOMRe.Style.make();

let shadow = ReactDOMRe.Style.make(~filter="url(#shadow)", ());

let rotate_card = rotation =>
  ReactDOMRe.Style.make(
    ~transitionDuration="0.5s",
    ~transitionProperty="transform",
    ~transform="rotateY(" ++ rotation->string_of_int ++ "deg)",
    (),
  );