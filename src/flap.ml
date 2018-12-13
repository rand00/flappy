open Lwt.Infix
open Lwt_react
open ReactiveData
open Gg
module H = Tyxml_js.Html
module R = Tyxml_js.R.Html

let debug = false
let fps = 30.
let game_node_id = "flap"

(*goto game todo
  . solve the homing-missile problem in this frp game
    . idea; spawn homing missiles that one should avoid + make smash into walls
  . performance; 
    . check if wanna use request animation-frame? 
      > then would need time-diff to simulate instead - less simple..
    . think if it's possible to have the style update separately from dom-node?
  . make milkshakes move in some kind of pattern
    . could this be a general entity thing? to have an animation-spec 
      < try to avoid a function here
  . make it more fun to move around 
    . idea; have 'holes in walls' instead of just pillars
    . idea; move left/right too?
    . idea; more interesting physics
      . idea; bouncing on walls instead of dying
*)

let sp = Printf.sprintf
(*> todo make a version of log that auto-prints newline*)
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

      type id = int [@@deriving show]
      
      type typ = [
        | `Bird
        | `Wall
        | `Background
        | `Scoreboard of int
        | `Cookie
        | `Homing_missile of id option
      ][@@deriving show]

      type t = {
        typ : typ;
        width : int;
        height : int;
        pos_x : int;
        pos_y : int;
        collided : bool;
        id : id;
      }[@@deriving show]

    end

    include T

    let make_id =
      let id = ref 0 in
      fun () ->
        let r = !id in
        incr id;
        r
    
    let init_bird (view_w, view_h) = {
      typ = `Bird;
      width = 30;
      height = 30;
      pos_x = (float view_w /. 4.) |> truncate;
      pos_y = (float view_h /. 2.) |> truncate;
      collided = false;
      id = make_id ()
    }

    let init_background (view_w, view_h) = {
      typ = `Background;
      width = view_w;
      height = view_h;
      pos_x = 0;
      pos_y = 0;
      collided = false;
      id = make_id ()
    }

    let init_wall frame (view_w, view_h) =
      let dist_mul = 4. in
      let time_to_spawn =
        frame mod truncate (fps *. dist_mul) = 0
        && Random.float 1.0 < (0.2 *. dist_mul)
      in
      if not time_to_spawn then [] else (
        let width = Random.int 150 + 50 in
        let height = Random.int (max 1 (view_h / 3)) + (view_h / 2)
        in
        [
          {
            typ = `Wall;
            width;
            height;
            pos_x = view_w;
            pos_y = if Random.bool () then 0 else view_h - height;
            collided = false;
            id = make_id ()
          }
        ]
      )

    let init_cookie frame (view_w, view_h) =
      let dist_mul = 3. in
      let time_to_spawn =
        frame mod truncate (fps *. dist_mul) = 0
        && Random.float 1.0 < (0.2 *. dist_mul)
      in
      if not time_to_spawn then [] else (
        let width = 200
        and pos_x = view_w
        and pos_y = (Random.float 1. *. float view_h) |> truncate in
        [
          {
            typ = `Cookie;
            width;
            height = width;
            pos_x;
            pos_y;
            collided = false;
            id = make_id ()
          }
        ]
      )

    let init_homing_missile frame (view_w, view_h) =
      let dist_mul = 3. in
      let time_to_spawn =
        frame mod truncate (fps *. dist_mul) = 0
        && Random.float 1.0 < (0.2 *. dist_mul)
      in
      if not time_to_spawn then [] else (
        let width = 200
        and pos_x = view_w
        and pos_y = (Random.float 1. *. float view_h) |> truncate in
        [
          {
            typ = `Homing_missile None;
            width;
            height = width;
            pos_x;
            pos_y;
            collided = false;
            id = make_id ()
          }
        ]
      )
    
    let init_scoreboard (view_w, view_h) = {
      typ = `Scoreboard 0;
      width = 0;
      height = 0;
      pos_x = view_w - 150;
      pos_y = view_h - 80;
      collided = false;
      id = make_id ()
    }

    let increment_score s' e =
      match e.typ with
      | `Scoreboard s -> { e with typ = `Scoreboard (s + s') }
      | typ ->
        log "this was not a scoreboard!\n";
        e 

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
      e.pos_x + e.width < 0 - eps ||
      e.pos_y + e.height < 0 - eps ||
      e.pos_x > w + eps ||
      e.pos_y > w + eps
    
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
        cookies : Entity.t list;
        homing_missiles : Entity.t list;
        background : Entity.t;
        scoreboard : Entity.t;
      }[@@deriving show]

    end

    include T

    let to_list m = List.flatten [
      [ m.background ];
      m.walls;
      m.cookies;
      [ m.bird ];
      m.homing_missiles;
      [ m.scoreboard ];
    ]
    
    let init view_dimensions = {
      bird = Entity.init_bird view_dimensions;
      walls = [];
      cookies = [];
      homing_missiles = [];
      background = Entity.init_background view_dimensions;
      scoreboard = Entity.init_scoreboard view_dimensions;
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
        |> Game.Entity.mark_if_collision walls in
      let scoreboard, cookies =
        let cookies =
          model.cookies
          (*goto could map 'apply_movement' here instead, which should be saved pr. entity
            < should also include the info about who's being followed?
          *)
          |> List.map (Game.Entity.move_x (-10))
          |> List.filter (not % (Game.Entity.is_out_of_bounds dimensions))
          |> List.append (Game.Entity.init_cookie frame dimensions) in
        let cookies_left = 
          cookies
          |> List.filter (not % Game.Entity.collides [bird]) in
        let scored_now = List.(length cookies - length cookies_left) in
        let scoreboard =
          model.scoreboard
          |> Game.Entity.increment_score scored_now
        in
        scoreboard, cookies_left in
      (*goto 
        . make missiles retarget closest targets (in our case just bird)
        . make missiles move depending on 
          . their velocity/acceleration in some direction
          . the position of target (if any)
            . get position from bird with target id (through some system for requesting it)
              . if ref is lost, (None)
                . change ref to other target / remove ref 
              . else continue
      *)
      let homing_missiles = 
        model.homing_missiles
        |> List.map (Game.Entity.move_x (-10))
        |> List.filter (not % (Game.Entity.is_out_of_bounds dimensions))
        |> List.append (Game.Entity.init_homing_missile frame dimensions) 
      in
      {
        bird;
        walls;
        cookies;
        homing_missiles;
        scoreboard;
        background = model.background
      }
    | `ViewResize dimensions ->
      let prev_dimensions = (model.background.width, model.background.height) in
      let reposition e =
        Game.Entity.reposition prev_dimensions dimensions e
      in
      let bird = model.bird |> reposition in
      let walls = model.walls |> List.map reposition in
      let cookies = model.cookies |> List.map reposition in
      let homing_missiles = model.homing_missiles |> List.map reposition in
      let scoreboard = model.scoreboard |> reposition in
      let background = model.background |> Game.Entity.resize dimensions 
      in
      {
        bird;
        walls;
        background;
        homing_missiles;
        scoreboard;
        cookies
      }
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
    Style.font_family `Courier_new;
    Style.font_size @@ `Px 80;
    (let px = `Px 2 in Style.text_shadow px px px "rgb(52, 0, 85)");
    Style.color "rgb(52, 0, 85)";
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
  let render_game_entity entity =
    begin match entity.typ with
      | `Bird ->
        begin
          let extend = 100 in
          let style = 
            if entity.collided then
              style_of_entity entity
                ~extend
                ~rotate:(`Deg 90)
                "http://media.giphy.com/media/pU8F8SZnRc8mY/giphy.gif"
            else 
              style_of_entity entity
                ~extend
                "http://media.giphy.com/media/pU8F8SZnRc8mY/giphy.gif"
          in
          H.div ~a:[ H.a_style style ] []
        end
      | `Cookie ->
        let style = style_of_entity entity
            "https://proxy.duckduckgo.com/iu/?u=https%3A%2F%2F68.media.tumblr.\
             com%2Fc9ed5ad2224ac3e259128282211bb967%2Ftumblr_oq0gpuyQ1U1wpimvfo1\
             _500.png&f=1"
            (* "https://proxy.duckduckgo.com/iu/?u=https%3A%2F%2Ftse3.mm.bing.net\
             *  %2Fth%3Fid%3DOIP.h-BQl88HtAC9woJ2IGgZxQHaIM%26pid%3D15.1&f=1" *)
        in
        H.div ~a:[ H.a_style style ] []
      | `Wall ->
        let style = style_of_entity entity
            "https://proxy.duckduckgo.com/iu/?u=http%3A%2F%2Fs14.favim.\
             com%2Forig%2F160524%2Fbts-fire-gif-suga-Favim.com-4339714.\
             gif&f=1"
        in
        H.div ~a:[ H.a_style style ] []
      (* "https://proxy.duckduckgo.com/iu/?u=http%3A%2F%2F\
       *  www.hdwallback.net%2Fwp-content%2Fuploads%2F2017%2F12%2F\
       *  brick-wallpapers-images.jpg&f=1" *)
      | `Background ->
        let style = style_of_entity entity 
            "https://proxy.duckduckgo.com/iu/?u=http%3A%2F%2Fhdwpro.com\
             %2Fwp-content%2Fuploads%2F2016%2F03%2FNature-Amazing-\
             Picture.jpeg&f=1"
        in
        H.div ~a:[ H.a_style style ] []
      | `Scoreboard score ->
        let style = style_of_entity entity ""
        in
        let content = H.pcdata (sp "%d" score) in
        H.div ~a:[ H.a_style style ] [ content ]
    end
  in
  let render_debug_overlay entity =
    if not debug then H.div [] else (
      let debug_text = H.pcdata (
          match entity.typ with
          | `Bird -> "bird"
          | `Wall -> "wall"
          | `Background -> "background"
          | `Scoreboard _ -> "scoreboard"
          | `Cookie -> "cookie"
        )
      in
      let background_color = "red" in
      let z_index = if List.mem entity.typ [`Bird; `Cookie] then 1 else -1 in
      let debug_style = style_of_entity ~background_color ~z_index entity "" in
      let debug_style_text = style_of_entity ~z_index:2 entity "" in
      H.div ~a:[ H.a_style debug_style ] [
        H.div ~a:[ H.a_style debug_style_text ] [
          debug_text
        ]
      ]
    )
  in
  let render_full_entity entity =
    H.div [
      render_game_entity entity;
      render_debug_overlay entity;
    ]
  in
  let rlist_entity =
    game_model_s |> S.map (fun model_opt ->
        log "game_model update\n";
        model_opt
        |> CCOpt.to_list
        |> CCList.flat_map Game.Model.to_list
      )
    |> RList.from_signal
  in
  let rlist_html = rlist_entity |> RList.map render_full_entity
  in
  (* let _ = Debug.RList.patch "entity" rlist_entity in
   * let _ = Debug.RList.patch "html" rlist_html in *)
  rlist_html
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

