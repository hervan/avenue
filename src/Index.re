[@bs.val] external document: Js.t({..}) = "document";

let style = document##createElement("style");
document##head##appendChild(style);
style##innerHTML #= Theme.app_style;

let container = document##createElement("div");
let () = document##body##appendChild(container);

ReactDOMRe.render(<Avenue />, container);