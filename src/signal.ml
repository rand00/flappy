open Lwt_react

module Infix = struct 

  let (>>=) s f = S.bind s f
  let (>>|) s f = S.map f s 

end
