
open Lwt.Infix
open Lwt_react
open ReactiveData
open Gg
module H = Tyxml_js.Html
module R = Tyxml_js.R.Html

let debug = false
let fps = 30.
let game_node_id = "gaxel"
let view_w, view_h = 1920, 1080

(* type entity_html = Html_types.body_content H.elt (\*possibly reactive*\) *)

let sp = Printf.sprintf
let log = Printf.printf 

module Game = struct

  module Event = struct

    type t = [ `WingFlap | `Frame of int ]

    let (sink_e : t E.t), sink_eupd = E.create ()

    let _print_sink_e = sink_e |> E.map (function
        | `WingFlap -> log "wing wing!\n"
        | `Frame _ -> ()
      )
    
    let feed_frp () =
      let rec loop frame =
        sink_eupd (`Frame frame);
        Lwt_js.sleep (1. /. fps) >>= fun () ->
        loop (succ frame)
      in
      Lwt.async @@ (fun () -> loop 0)

  end

  module Entity = struct

    module T = struct 
    
      type typ = [
        | `Bird
        | `Wall
        | `Background
      ]

      type t = {
        typ : typ;
        width : int;
        height : int;
        pos_x : int;
        pos_y : int;
      }

    end

    include T
    
    let init_bird () = {
      typ = `Bird;
      width = 200;
      height = 200;
      pos_x = float view_w /. 4. |> truncate;
      pos_y = float view_h /. 2. |> truncate;
    }

    let init_background () = {
      typ = `Background;
      width = view_w;
      height = view_h;
      pos_x = 0;
      pos_y = 0;
    }
    
  end

  module Model = struct

    module T = struct 
    
      type t = {
        bird : Entity.t;
        walls : Entity.t list;
        background : Entity.t;
      }

    end

    include T
    
    let init () = {
      bird = Entity.init_bird ();
      walls = [];
      background = Entity.init_background ();
    }
    
  end
  
end

open Game.Entity.T
open Game.Model.T

(* let circle_pos_s : (float * float) S.t =
 *   let circle frame =
 *     let t = (float frame *. 0.5 *. Float.pi) /. fps in
 *     sin t, cos t
 *   in
 *   E.map circle tick_e
 *   |> S.hold (0., 0.)
 * 
 * let circle_01_s =
 *   circle_pos_s
 *   |> S.map (fun (pos_x, pos_y) -> {
 *         typ = `Bird;
 *         width = 200;
 *         height = 200;
 *         pos_x = truncate (pos_x *. 100.);
 *         pos_y = truncate (pos_y *. 100.);
 *       }
 *     ) *)

(**Update*)

(*goto define update as: 'frame -> event -> model -> model'*)
(*goto make this list dynamic, based on 'alive/dead'*)
let game_entities_s : Game.Entity.T.t React.signal list React.signal = 
  let update model event =
    match event with
    | `WingFlap -> {
        model with
        bird = {
          model.bird with
          pos_y = model.bird.pos_y - 70;
        }
      }
    | `Frame frame -> {
        model with
        bird = {
          model.bird with
          pos_y = model.bird.pos_y + 10;
        };
        walls =
          model.walls |> List.map (fun wall -> {
                wall with
                pos_x = wall.pos_x - 20;
              }
            )
      }
  in
  Game.Event.sink_e
  |> E.fold update (Game.Model.init ())
  |> E.map (fun model ->
      [ model.background ]
      @ model.walls
      @ [ model.bird ]
      |> List.map S.const (*goto, here we just fill the signature..*)
    )
  |> S.hold []

(**View*)

let style_of_entity entity image = Style.make [
    Style.position `Fixed;
    Style.background_image image;
    Style.background_size `Cover;
    Style.width @@ `Px entity.width;
    Style.heigth @@ `Px entity.height;
    Style.left @@ `Px entity.pos_x;
    Style.top @@ `Px entity.pos_y;
  ]


(*>old type: Html_types.body_content H.elt list S.t*)
let reactive_view : Dom.node Js.t =
  game_entities_s
  |> S.map (fun entities_s ->
      entities_s
      |> List.map (fun entity_s ->
          let style_s =
            entity_s |> S.map (fun entity -> 
                match entity.typ with
                | `Bird ->
                  style_of_entity entity 
                    "http://media.giphy.com/media/pU8F8SZnRc8mY/giphy.gif"
                | `Wall ->
                  style_of_entity entity 
                    "http://media.giphy.com/media/pU8F8SZnRc8mY/giphy.gif"
                | `Background ->
                  style_of_entity entity 
                    "https://proxy.duckduckgo.com/iu/?u=http%3A%2F%2Fhdwpro.com\
                     %2Fwp-content%2Fuploads%2F2016%2F03%2FNature-Amazing-\
                     Picture.jpeg&f=1"
              )
          in
          H.div ~a:[ R.a_style style_s ] (
            if debug then
              [ R.pcdata begin entity_s |> S.map (fun entity ->
                    match entity.typ with
                    | `Bird -> "bird"
                    | `Wall -> "wall"
                    | `Background -> "background"
                  )
                  end
              ]
            else []
          )
        )
    )
  |> RList.from_signal
  |> R.div 
  |> Tyxml_js.To_dom.of_node

let render () =
  let root = Dom_html.getElementById game_node_id in
  Dom.appendChild root reactive_view;
  Dom_html.document##.onkeydown := Dom_html.handler (fun e ->
      Printf.printf "keycode: %d\n" e##.keyCode;
      let _ = match e##.keyCode with
        | 32 -> Game.Event.sink_eupd `WingFlap
        | _ -> ()
      in
      Js._false
    )

let main () =
  Dom_html.window##.onload := Dom_html.handler (fun _ -> 
      (* tick_eupd 0; (\*for debug*\) *)
      Game.Event.feed_frp ();
      render ();
      Js._false
    )

let _ = main ()

