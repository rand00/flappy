
let rec remove_children node =
  Js.Opt.iter node##.lastChild (fun last_child ->
      node##removeChild last_child;
      remove_children node
    )

