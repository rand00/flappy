

type direction = [ `Left | `Right | `Up | `Down ]
               
type t = [
  | `WingFlap of int (*player*) * direction
  | `Frame of int
  | `ViewResize of int * int
  | `PauseToggle 
  ]

let (sink_e : t E.t), sink_eupd = E.create ()

let _print_sink_e = sink_e |> E.map (function
  | `ViewResize (x, y) -> log "window resize %d x %d\n" x y
  | _ -> ()
)

let feed_frp () =
  let rec loop frame =
    sink_eupd (`Frame frame);
    Lwt_js.sleep (1. /. fps) >>= fun () ->
    loop (succ frame)
  in
  Lwt.async @@ (fun () -> loop 0)

