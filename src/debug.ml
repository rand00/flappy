
let log = Printf.printf 

module RList = struct 

  open Lwt_react
  open ReactiveData
  
  let patch tag rlist = 
    rlist |> RList.event |> E.map (function
        | RList.Patch l ->
          l |> List.iter (function
              | RList.I _ -> log "rlist: %s: patch insert\n" tag
              | RList.R _ -> log "rlist: %s: patch removal\n" tag
              | RList.U _ -> log "rlist: %s: patch update\n" tag
              | RList.X _ -> log "rlist: %s: patch exchange\n" tag
            )
        | RList.Set _ -> log "rlist: %s:  set\n" tag
      )

end



