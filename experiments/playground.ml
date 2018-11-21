
type axel_state =
  | Glad_axel of string
  | Sur_axel
  | Hest_axel

type axel_record = {
  a : int;
  b : string;
}

let () =
  let r = { a = 2; b = "foo" } in
  Printf.printf "Der findes %d %s i verden" r.a r.b;
  let () = () in
  let 1 = 1 in
  Random.init 2;
  let l = [ Glad_axel "kage"; Sur_axel; Hest_axel; Glad_axel "dum router" ] in
  let string =
    match List.nth l (Random.int (List.length l)) with
    | Glad_axel ("kage" as glade_str) ->
      "WEEEEE "^glade_str^glade_str^glade_str^glade_str^glade_str
    | Glad_axel whatever -> "WEEEEE "^whatever
    | Sur_axel | Hest_axel -> "BUHUHFWIHB"
  in
  Printf.printf "\n\n!!!!!! %s !!!!!!!!\n\n" string






