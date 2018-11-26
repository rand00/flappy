
open Lwt.Infix
open Lwt_react
open ReactiveData
open Gg
module H = Tyxml_js.Html
module R = Tyxml_js.R.Html

let sp = Printf.sprintf

type entity_html = Html_types.body_content H.elt (*possibly reactive*)

module Game = struct

  module Event = struct

    type t = [ `WingFlap ]

    let (sink : t E.t), sink_upd = E.create ()
    
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
    
  end

end

open Game.Entity.T

(*goto make main module of this>*)

let tick_e, tick_eupd = E.create ()
let fps = 30.
let game_node_id = "gaxel"

let feed_frp ()=
  let rec loop frame =
    tick_eupd frame;
    Lwt_js.sleep (1. /. fps) >>= fun () ->
    loop (succ frame)
  in
  Lwt.async @@ (fun () -> loop 0)

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
  
  failwith ""

(*>old code*)
(* S.const [ 
 *   circle_01_s 
 * ] *)

(**View*)

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
                  Style.make [
                    Style.position `Fixed;
                    Style.background_image
                      "http://media.giphy.com/media/pU8F8SZnRc8mY/giphy.gif";
                    Style.background_size `Cover;
                    Style.width @@ `Px entity.width;
                    Style.heigth @@ `Px entity.height;
                    Style.left @@ `Px entity.pos_x;
                    Style.top @@ `Px entity.pos_y;
                  ]
                | `Wall -> failwith "todo"
                | `Background -> failwith "todo"
              )
          in
          H.div ~a:[ R.a_style style_s ] []
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
        | 32 -> Game.Event.sink_upd `WingFlap
        | _ -> ()
      in
      Js._false
    )

let main () =
  Dom_html.window##.onload := Dom_html.handler (fun _ -> 
      (* tick_eupd 0; (\*for debug*\) *)
      feed_frp ();
      render ();
      Js._false
    )

let _ = main ()

