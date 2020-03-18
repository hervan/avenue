[@react.component]
let make = () => {
  let (editing, setEditing) = React.useState(() => false);
  let (color, onChange) = React.useState(() => "#FFFF00");
  let onCancel = _evt => setEditing(_ => false);
  let onFocus = event => ReactEvent.Focus.target(event)##select();

  <div>
    <svg width="100px" height="100px" viewBox="0 0 100 100" version="1.1">
      <title> "My Ordinary SVG"->React.string </title>
      <circle
        onDoubleClick={_evt => setEditing(_ => true)}
        cx="50"
        cy="50"
        r="50"
        fill=color
      />
    </svg>
    {if (editing) {
       <form onSubmit={_ => {setEditing(_ => false)}} onBlur=onCancel>
         <input
           onBlur=onCancel
           onFocus
           onChange={event => onChange(ReactEvent.Form.target(event)##value)}
           value=color
         />
       </form>;
     } else {
       React.null;
     }}
  </div>;
};