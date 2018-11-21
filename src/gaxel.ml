

let _ =
  Dom_html.window##.onload := Dom_html.handler (fun _ -> 
      Printf.printf "FOOO\n";
      Js._false
    )


