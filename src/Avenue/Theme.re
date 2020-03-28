let text = fontSize => {
  ReactDOMRe.Style.make(
    ~fontSize,
    ~fontFamily="Verdana",
    ~userSelect="none",
    (),
  );
};

let big_text =
  text("8px")
  |> ReactDOMRe.Style.combine(ReactDOMRe.Style.make(~fontWeight="bold", ()));

let quick_transition = ReactDOMRe.Style.make(~transition="d 0.5s", ());

let no_transition = ReactDOMRe.Style.make();

let shadow = ReactDOMRe.Style.make(~filter="url(#shadow)", ());