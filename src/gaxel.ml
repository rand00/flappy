open Lwt.Infix
open Lwt_react
open ReactiveData
open Gg
module H = Tyxml_js.Html
module R = Tyxml_js.R.Html

let debug = false
let fps = 30.
let game_node_id = "gaxel"

(*goto game todo
  . make some way of scoring 
    . idea; have randomly generated things one can pick up 
  . make it more fun to move around 
    . idea; move left/right too?
    . idea; more interesting physics
      . idea; bouncing on walls instead of dying
  . solve the homing-missile problem in this frp game
    . idea; spawn homing missiles that one should avoid + make smash into walls
*)

let sp = Printf.sprintf
let log = Printf.printf 
let (%) f g x = f (g x)

(* type entity_html = Html_types.body_content H.elt (\*possibly reactive*\) *)

module Game = struct

  module Event = struct

    type t = [
      | `WingFlap
      | `Frame of int
      | `ViewResize of int * int
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

  end

  module Entity = struct

    module T = struct 

      type typ = [
        | `Bird
        | `Wall
        | `Background
      ][@@deriving show]

      type t = {
        typ : typ;
        width : int;
        height : int;
        pos_x : int;
        pos_y : int;
        collided : bool;
      }[@@deriving show]

    end

    include T
    
    let init_bird (view_w, view_h) =
      log "init-bird: float view_w /. 4. = %f, float view_h /. 2. = %f\n"
        (float view_w /. 4.)
        (float view_h /. 2.);
      {
        typ = `Bird;
        width = 30;
        height = 30;
        pos_x = (float view_w /. 4.) |> truncate;
        pos_y = (float view_h /. 2.) |> truncate;
        collided = false;
      }

    let init_background (view_w, view_h) = {
      typ = `Background;
      width = view_w;
      height = view_h;
      pos_x = 0;
      pos_y = 0;
      collided = false;
    }

    let init_wall frame (view_w, view_h) =
      let width = Random.int 150 + 50 in
      let height = Random.int (max 1 (view_h / 3)) + (view_h / 2) in
      let dist_mul = 4. in
      if
        frame mod truncate (fps *. dist_mul) = 0
        && Random.float 1.0 < (0.2 *. dist_mul)
      then 
        let wall = {
          typ = `Wall;
          width;
          height;
          pos_x = view_w;
          pos_y = if Random.bool () then 0 else view_h - height;
          collided = false;
        } in
        [ wall ]
      else
        []
    
    let move_x px e = {
      e with pos_x = e.pos_x + px
    }

    let move_y px e = {
      e with pos_y = e.pos_y + px
    }

    let resize (width, height) e = {
      e with width; height
    }

    let reposition (prev_w, prev_h) (w, h) e =
      let prev_x, prev_y = float e.pos_x, float e.pos_y in
      let prev_w, prev_h = max 1. (float prev_w), max 1. (float prev_h) in
      let w, h = float w, float h in
      let x = (prev_x /. prev_w) *. w |> truncate 
      and y = (prev_y /. prev_h) *. h |> truncate in
      { e with pos_x = x; pos_y = y }
    
    (*Note: e is out of bounds if whole e-area is out of bounds*)
    let is_out_of_bounds (w, h) e =
      let eps = 0 in
      let b = 
        e.pos_x + e.width < 0 - eps ||
        e.pos_y + e.height < 0 - eps ||
        e.pos_x > w + eps ||
        e.pos_y > w + eps
      in
      (* log "e.pos_x + e.width < 0 - eps = %b\n" (e.pos_x + e.width < 0 - eps);
       * log "e.pos_y + e.height < 0 - eps = %b\n" (e.pos_y + e.height < 0 - eps); 
       * log "e.pos_x > w + eps = %b\n" (e.pos_x > w + eps);
       * log "e.pos_y > w + eps = %b\n" (e.pos_y > w + eps);
       * log "is out of bounds. x = %d, y = %d, w = %d, h = %d, \
       *      view_w = %d, view_h = %d\n"
       *   e.pos_x e.pos_y
       *   e.width e.height
       *   w h; *)
      b

    let collides es e =
      let range_x e = e.pos_x, e.pos_x + e.width in
      let range_y e = e.pos_y, e.pos_y + e.height in
      (*note - this is a point <-> range relation*)
      let is_contained_1d i (start, stop) =
        start <= i && i <= stop
      in
      (*note - this is a range <-> range relation*)
      let is_collision_1d ((start,stop) as r) ((start',stop') as r') =
        is_contained_1d start  r' ||
        is_contained_1d stop   r' ||
        is_contained_1d start' r  ||
        is_contained_1d stop'  r 
      in
      (*note - this is a 2d-range <-> 2d-range relation*)
      let is_collision_2d e' =
        is_collision_1d (range_x e) (range_x e') &&
        is_collision_1d (range_y e) (range_y e')
      in
      List.exists is_collision_2d es

    let mark_if_collision es e =
      if not @@ collides es e then e else {
        e with collided = true
      }
    
  end

  module Model = struct

    module T = struct 
    
      type t = {
        bird : Entity.t;
        walls : Entity.t list;
        background : Entity.t;
      }[@@deriving show]

    end

    include T
    
    let init view_dimensions = {
      bird = Entity.init_bird view_dimensions;
      walls = [];
      background = Entity.init_background view_dimensions;
    }
    
  end
  
end

open Game.Entity.T
open Game.Model.T

(**Update*)

let game_model_s : Game.Model.t option React.signal = 
  let update model event =
    match event with
    | `WingFlap ->
      let bird =
        if model.bird.collided then model.bird else 
          model.bird
          |> Game.Entity.move_y (-70)
          |> Game.Entity.mark_if_collision model.walls
      in
      { model with bird }
    | `Frame frame ->
      let dimensions = model.background.width, model.background.height in
      let walls =
        model.walls
        |> List.map (Game.Entity.move_x (-5))
        |> List.filter (not % (Game.Entity.is_out_of_bounds dimensions))
        |> List.append (Game.Entity.init_wall frame dimensions) in
      let bird =
        model.bird
        |> Game.Entity.move_y 11
        |> Game.Entity.mark_if_collision walls
      in
      { model with bird; walls }
    | `ViewResize dimensions ->
      let prev_dimensions = (model.background.width, model.background.height) in
      let reposition e =
        Game.Entity.reposition prev_dimensions dimensions e
      in
      let bird = model.bird |> reposition in
      let walls = model.walls |> List.map reposition in
      let background = model.background |> Game.Entity.resize dimensions
      in
      { bird; walls; background }
  in
  Game.Event.sink_e
  |> E.fold update (Game.Model.init (1920, 1080))
  |> E.map CCOpt.pure
  |> S.hold None

(**View*)

let style_of_entity
    ?rotate ?extend ?background_color ?z_index
    entity image
  =
  let ext = CCOpt.get_or ~default:0 extend in
  let width, height =
    entity.width + ext * 2,
    entity.height + ext * 2
  and pos_x, pos_y =
    entity.pos_x - ext,
    entity.pos_y - ext
  in
  Style.make ([
    Style.position `Fixed;
    Style.background_image image;
    Style.background_size `Cover;
    Style.width @@ `Px width;
    Style.heigth @@ `Px height;
    Style.left @@ `Px pos_x;
    Style.top @@ `Px pos_y;
  ]
    @ (rotate |> CCOpt.map Style.rotate |> CCOpt.to_list)
    @ (background_color |> CCOpt.map Style.background_color |> CCOpt.to_list)
    @ (z_index |> CCOpt.map Style.z_index |> CCOpt.to_list)
  )

(*>old type: Html_types.body_content H.elt list S.t*)
(*> goto make this into a 'elm' library function
  . think first if this should have some other interface (e.g. taking reactive html instead!)
*)
let reactive_view : Dom.node Js.t =
  let model_to_list model =
    model.background :: model.walls @ [ model.bird ]
  in
  let render_game_entity entity =
    let entity_style = begin match entity.typ with
      | `Bird ->
        begin
          let extend = 100 in
          if entity.collided then
            style_of_entity entity
              ~extend
              ~rotate:(`Deg 90)
              "http://media.giphy.com/media/pU8F8SZnRc8mY/giphy.gif"
          else 
            style_of_entity entity
              ~extend
              "http://media.giphy.com/media/pU8F8SZnRc8mY/giphy.gif"
        end
      | `Wall ->
        style_of_entity entity
          "https://proxy.duckduckgo.com/iu/?u=http%3A%2F%2Fs14.favim.\
           com%2Forig%2F160524%2Fbts-fire-gif-suga-Favim.com-4339714.\
           gif&f=1"
      (* "https://proxy.duckduckgo.com/iu/?u=http%3A%2F%2F\
       *  www.hdwallback.net%2Fwp-content%2Fuploads%2F2017%2F12%2F\
       *  brick-wallpapers-images.jpg&f=1" *)
      | `Background ->
        style_of_entity entity 
          "https://proxy.duckduckgo.com/iu/?u=http%3A%2F%2Fhdwpro.com\
           %2Fwp-content%2Fuploads%2F2016%2F03%2FNature-Amazing-\
           Picture.jpeg&f=1"
    end
    in
    H.div ~a:[ H.a_style entity_style ] []          
  in
  let render_debug_overlay entity =
    if not debug then H.div [] else (
      let debug_text = H.pcdata (
          match entity.typ with
          | `Bird -> "bird"
          | `Wall -> "wall"
          | `Background -> "background"
        )
      in
      let background_color = "red" in
      let z_index = if entity.typ = `Bird then 10 else 0 in
      let debug_style = style_of_entity ~z_index ~background_color entity "" in
      H.div ~a:[ H.a_style debug_style ] [ debug_text ]
    )
  in
  let render_full_entity entity =
    H.div [
      render_game_entity entity;
      render_debug_overlay entity;
    ]
  in
  game_model_s |> S.map (fun model_opt ->
      model_opt
      |> CCOpt.to_list
      |> CCList.flat_map model_to_list
      |> List.map render_full_entity
    )
  |> RList.from_signal
  |> R.div 
  |> Tyxml_js.To_dom.of_node

let update_view_size () =
  let (>>=) = Js.Optdef.bind in
  let (>>|) = Js.Optdef.map in
  Dom_html.window##.innerWidth >>= fun w ->
  Dom_html.window##.innerHeight >>| fun h ->
  Game.Event.sink_eupd (`ViewResize (w, h))

let render () =
  let root = Dom_html.getElementById game_node_id in
  Dom.appendChild root reactive_view;
  Dom_html.document##.onkeydown := Dom_html.handler (fun e ->
    Printf.printf "keycode: %d\n" e##.keyCode;
    let _ = match e##.keyCode with
      | 32 -> Game.Event.sink_eupd `WingFlap
      | _ -> ()
    in
    Js._true
  );
  update_view_size () |> ignore;
  Dom_html.window##.onresize := Dom_html.handler (fun _ ->
    update_view_size () |> ignore;
    Js._true
  )


let main () =
  Dom_html.window##.onload := Dom_html.handler (fun _ -> 
    Random.self_init ();
    (* Game.Event.sink_eupd (`Frame 0); (\*for debug*\) *)
    render ();
    Game.Event.feed_frp ();
    Js._false
  )

let _ = main ()

