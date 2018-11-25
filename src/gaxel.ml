
(*goto brian + define output:
  . idea (enough for simple game);
    . map signals of things to show to
      . unique id's 
      . render recipe (position, w/h, image ..)
    . have all signals to show in some list (variant S.t list S.t)
    . render separately; 
      . if id of variant signal exists in dom, change style etc based on render-spec
  . idea; more scaleable + experiment with frp components
    . premise;
      . don't want to use reactive html nodes, 
        ? supported by tyxml + js? (without ocsigen)
        ? not supporting dynamic frp graph?
    . make a 'component'-like structure, 
      . where each component
        . controls own output 
        . returns E.t's for events from node 
          . (e.g. onclick, on-inner-logick stuff etc)
          . as alternative to supplying callbacks to component as arg
      . on creation of a component; it should get the id of the node that it 'owns'
        > this way there is no need for reactive html;
          . but the inner component could just overwrite its inner html each time
            .. but then inner components could get their id's removed 
               > not if we can guarantee that outer component will make its id (before it renders again)
                 > can inner component be reused (and depend on outer)? 
                   > yes with E/S.fix + E/S.bind 
                     . so fix accumulates a map of id -> component, and reuses them 
                       .. reuse is very 'manual' - not declarative 
                          > this is the job of the virtual dom... 
                            .. where instead of hooking up to dom itself, it returns
                               reactive html that can be interpreted and updated incrementally by
                               vdom
                               >! use Incr_dom if I want this later
  . brian; could one make 2 step definition, (like Elm)
    . where 
      . frp is by itself, and render by itself
      . and frp returns spec including id, + a set of variants saying specifying listeners
    . problems;
      . what about S.bind - would component get new id all the time?
        . this might be fair enough
      . can frp events/signals even have an id... it should be inferred of data?
        .. but then what if the data of two signals is the same...
      . how to specify _where_ one wants listeners hookd up inside som tree..
        .. this is what vdom solves too
*)

open Lwt.Infix
open Lwt_react
open Gg
module H = Tyxml_js.Html
module R = Tyxml_js.R.Html

let tick_e, tick_eupd = E.create ()
let fps = 30.
let game_node_id = "gaxel"

module Renderable = struct 

  type t = {
    image : string;
    pos_x : int;
    pos_y : int;
    width : int;
    height : int;
  }
  
end

open Renderable

let feed_frp ()=
  let rec loop frame =
    tick_eupd frame;
    Lwt_js.sleep (1. /. fps) >>= fun () ->
    loop (succ frame)
  in
  Lwt.async @@ (fun () -> loop 0)

let circle_pos_s : (float * float) S.t =
  let circle frame =
    let t = (float frame *. 2. *. Float.pi) /. fps in
    sin t, cos t
  in
  E.map circle tick_e
  |> S.hold (0., 0.)

let circle_01_s =
  circle_pos_s
  |> S.map (fun (pos_x, pos_y) -> {
        image = "https://beautifulcoolwallpapers.files.wordpress.com/\
                 2011/09/the-best-top-desktop-horse-wallpapers-21.jpg";
        pos_x = truncate (pos_x *. 100.); (*goto*)
        pos_y = truncate (pos_y *. 100.);
        width = 1000;
        height = 1000;
      }
    )

module JsAux = struct

  let rec remove_children node =
    Js.Opt.iter node##.lastChild (fun last_child ->
        node##removeChild last_child;
        remove_children node
      )
  
end

let sp = Printf.sprintf

(*goto
  . make git commit for this version
  . make new branch that tries to use reactive tyxml
*)
let _render =
  circle_01_s |> S.map (fun renderable ->
      let open CCOpt.Infix in
      let root = Dom_html.getElementById game_node_id in
      JsAux.remove_children root;
      let renderable_node =
        H.div ~a:[
          H.a_style @@ String.concat "" [
            sp "background-image: url(%s);" renderable.image;
            "position: fixed;";
            sp "width: %dpx;" renderable.width;
            sp "height: %dpx;" renderable.height;
            sp "left: %dpx;" renderable.pos_x;
            sp "top: %dpx;" renderable.pos_y;
          ]
        ] []
        |> Tyxml_js.To_dom.of_node
      in
      root##appendChild renderable_node
    )

let main () =
  Dom_html.window##.onload := Dom_html.handler (fun _ -> 
      (* tick_eupd 0; (\*goto remove debug*\) *)
      feed_frp ();
      Printf.printf "FOOO\n";
      Js._false
    )

let _ = main ()

