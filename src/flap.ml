open Lwt.Infix
open Lwt_react
open ReactiveData
open Gg
module H = Tyxml_js.Html
module R = Tyxml_js.R.Html

let debug = false
let fps = 30.
let players = 2

(*goto game todo
  . !idea; missiles; make target eachother too! (+ tune attraction (up it?))
  . finetune local multiplayer (bird v antimatter-bird)
    . make missiles remove points instead 
    . up the points given for milkshakes?
      . make them move in patterns
    . make birds interact?
  . experiment with a component structure using frp + vdom
    . could be used for animation/visualization
      . so inner state is a fix/fold over item state
      . ideas;
        . e.g. implement a trail of following feathers of birds 
          (better to be own entities? - for recursive interaction with env.)
        . explosion material flying out (might be better to be own entities..)
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
let (%>) f g x = g (f x)

module IMap = CCMap.Make(CCInt)
      
(* type entity_html = Html_types.body_content H.elt (\*possibly reactive*\) *)

module Game = struct

  module Event = struct

    type t = [
      | `WingFlap of int 
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

      type homing_missile = {
        target : t option;
        movement_vector : V2.t;
      }[@@deriving show]
      
      and typ = [
        | `Bird of int (*player no.*)
        | `Wall
        | `Background
        | `Scoreboard of (int IMap.t [@printer IMap.pp CCInt.pp CCInt.pp])
        | `Cookie
        | `Homing_missile of homing_missile
      ][@@deriving show]

      and t = {
        typ : typ;
        width : int;
        height : int;
        pos_x : int;
        pos_y : int;
        collided : bool;
        timeout : int option; (*n.o. frames*)
      }[@@deriving show]
  (*< todo remove id again if homing missiles doesn't need them*)
      
    end

    include T

    let distance e e' =
      let x , y  = float e.pos_x, float e.pos_y in
      let x', y' = float e'.pos_x, float e'.pos_y in
      sqrt ((x -. x') ** 2. +. (y -. y') ** 2.)
    
    let init_bird (view_w, view_h) i = {
      typ = `Bird i;
      width = 30;
      height = 30;
      pos_x = (float view_w /. 4.) |> truncate;
      pos_y = (float (i * view_h) /. (float players)) |> truncate;
      collided = false;
      timeout = None;
    }

    let init_background (view_w, view_h) = {
      typ = `Background;
      width = view_w;
      height = view_h;
      pos_x = 0;
      pos_y = 0;
      collided = false;
      timeout = None;
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
            timeout = None;
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
            timeout = None;
          }
        ]
      )

    module Homing_missile = struct 
    
      let init frame (view_w, view_h) =
        let dist_mul = 3. in
        let time_to_spawn =
          frame mod truncate (fps *. dist_mul) = 0
          && Random.float 1.0 < (0.2 *. dist_mul)
        in
        if not time_to_spawn then [] else (
          let width = 30
          and pos_x = view_w / 2
          and pos_y = (Random.float 1. *. float view_h) |> truncate in
          [
            {
              typ = `Homing_missile {
                  target = None;
                  movement_vector = V2.v (-1.) 0.
                };
              width;
              height = width;
              pos_x;
              pos_y;
              collided = false;
              timeout = None;
            }
          ]
        )

      let choose_target targets missile =
        let target =
          List.fold_left (fun acc e ->
              match acc with
              | None -> Some e
              | Some e' ->
                let chosen =
                  if distance missile e < distance missile e' then e else e'
                in Some chosen
            ) None targets
        in
        match missile.typ with
        | `Homing_missile missile_data ->
          { missile with typ = `Homing_missile {
                missile_data with target;
              }}
        | _ ->
          log "choose_target: was not a missile!\n";
          missile

      let move missile =
        match missile.typ with
        | `Homing_missile missile_data -> 
          let movement_vector =
            if missile.collided then
              V2.v 0. 0.
            else begin
              match missile_data.target with
              | None -> missile_data.movement_vector
              | Some target ->
                let target_vec = V2.v (float target.pos_x) (float target.pos_y) in
                let missile_vec = V2.v (float missile.pos_x) (float missile.pos_y) in
                let diff_vec = V2.(target_vec - missile_vec) in
                let dist = V2.norm diff_vec in
                let dist_factor = min (0.000014 *. sqrt dist) 0.5 in
                V2.(missile_data.movement_vector + (dist_factor * diff_vec))
            end
          in
          { missile with
            typ = `Homing_missile { missile_data with movement_vector };
            pos_x = missile.pos_x + (truncate V2.(x movement_vector));
            pos_y = missile.pos_y + (truncate V2.(y movement_vector));
          }
        | _ -> missile (*goto make 'log_and_return "tag" entity' a helper *)

    end
    
    let init_scoreboard (view_w, view_h) = {
      typ = `Scoreboard IMap.empty;
      width = 550;
      height = 0;
      pos_x = view_w - 283;
      pos_y = view_h - (80 * players);
      collided = false;
      timeout = None;
    }

    let increment_score ~scored_now ~bird e =
      match e.typ, bird.typ with
      | `Scoreboard scores, `Bird player ->
        let scores =
          scores |> IMap.update player (function
              | None -> Some scored_now
              | Some score -> Some (scored_now + score)
            )
        in
        { e with typ = `Scoreboard scores }
      | typ ->
        log "this was not a scoreboard+bird !\n";
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

    let mark_if_collision ?change es e =
      if (not @@ collides es e) || e.collided then e else
        let e = { e with collided = true } in
        match change with
        | Some change -> change e
        | None -> e

  end

  module Model = struct

    module T = struct 
    
      type t = {
        birds : Entity.t list;
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
      m.birds;
      m.homing_missiles;
      [ m.scoreboard ];
    ]
    
    let init view_dimensions = {
      birds = List.init players (Entity.init_bird view_dimensions);
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
    | `WingFlap player ->
      let birds =
        model.birds 
        |> List.map (fun bird -> 
               match bird.typ with
               | `Bird bird_player when
                      bird_player = player
                      && not bird.collided -> 
                  bird
                  |> Game.Entity.move_y (-70)
                  |> Game.Entity.mark_if_collision
                       (model.walls @ model.homing_missiles)
               | _ -> bird
             )
      in
      { model with birds }
    | `Frame frame ->
      let dimensions = model.background.width, model.background.height in
      let walls =
        model.walls
        |> List.map (Game.Entity.move_x (-5))
        |> List.filter (not % (Game.Entity.is_out_of_bounds dimensions))
        |> List.append (Game.Entity.init_wall frame dimensions) in
      let homing_missiles = 
        model.homing_missiles
        |> List.map (
               (*goto goo try add missiles as targets too
                 . keep logic internal to choose-target, as there can be more complex priorities
                *)
             Game.Entity.Homing_missile.choose_target model.birds
          %> Game.Entity.Homing_missile.move
          %> (fun e -> { e with timeout = e.timeout |> CCOpt.map pred })
          %> Game.Entity.mark_if_collision (model.birds @ model.walls)
            ~change:(fun e -> {
                  e with
                  width = e.width + 100;
                  timeout = Some (fps *. 3. |> truncate);
                })
        )
        |> List.filter (not % fun e -> e.timeout |> CCOpt.exists (fun t -> t < 0))
        |> List.filter (not % Game.Entity.is_out_of_bounds dimensions)
        |> List.append (Game.Entity.Homing_missile.init frame dimensions) in
      let birds =
        model.birds
        |> List.map (fun bird ->
            bird 
            |> Game.Entity.move_y 11
            |> Game.Entity.mark_if_collision (walls @ homing_missiles) 
          )
      in
      let scoreboard, cookies =
        let cookies =
          model.cookies
          (*goto could map 'apply_movement' here instead, which should be saved pr. entity
            < should also include the info about who's being followed?
          *)
          |> List.map (Game.Entity.move_x (-10))
          |> List.filter (not % (Game.Entity.is_out_of_bounds dimensions))
          |> List.append (Game.Entity.init_cookie frame dimensions)
        in
        model.birds
        |> List.fold_left (fun (scoreboard, cookies) bird -> 
            let cookies_left = 
              cookies
              |> List.filter (not % Game.Entity.collides [bird])
            in
            let scored_now = List.(length cookies - length cookies_left) in
            let scoreboard = Game.Entity.increment_score
                ~scored_now
                ~bird
                scoreboard
            in
            scoreboard, cookies_left
          ) (model.scoreboard, cookies)
      in
      {
        birds;
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
      let birds = model.birds |> List.map reposition in
      let walls = model.walls |> List.map reposition in
      let cookies = model.cookies |> List.map reposition in
      let homing_missiles = model.homing_missiles |> List.map reposition in
      let scoreboard = model.scoreboard |> reposition in
      let background = model.background |> Game.Entity.resize dimensions 
      in
      {
        birds;
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
    ?rotate ?extend ?background_color ?z_index ?filter
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
      Style.font_size @@ `Px 40;
      Style.text_shadow (`Px 80) (`Px 19) (`Px 2) "rgb(52, 0, 85)";
      Style.color "rgb(102, 255, 125)";
      Style.line_height @@ `Px 25;
    ]
      @ (filter |> CCOpt.map Style.filter |> CCOpt.to_list)
      @ (rotate |> CCOpt.map Style.rotate |> CCOpt.to_list)
      @ (background_color |> CCOpt.map Style.background_color |> CCOpt.to_list)
      @ (z_index |> CCOpt.map Style.z_index |> CCOpt.to_list)
    )

(*old type: Html_types.body_content H.elt list S.t*)
let reactive_view : Dom.node Js.t =
  let render_game_entity entity =
    begin match entity.typ with
      | `Bird player ->
        begin
          let extend = 100 in
          let hue_rotate_degrees =
            360. *. (float player) /. (float players) |> truncate in
          let filter = `Hue_rotate hue_rotate_degrees in
          let style = 
            if entity.collided then
              style_of_entity entity
                ~extend ~filter ~rotate:(`Deg 90) "assets/bird.gif"
            else
              style_of_entity entity
                ~extend ~filter "assets/bird.gif"
          in
          H.div ~a:[ H.a_style style ] []
        end
      | `Cookie ->
        let style = style_of_entity entity "assets/milkshake.png" in
        H.div ~a:[ H.a_style style ] []
      | `Homing_missile _ ->
        let style =
          let image, extend =
            if entity.collided then
              "assets/explosion.gif", 60
            else
              "assets/doggy.png", 120
          in
          style_of_entity entity image  ~extend
        in
        H.div ~a:[ H.a_style style ] []
      | `Wall ->
        let style = style_of_entity entity "assets/korean.gif" in
        H.div ~a:[ H.a_style style ] []
      | `Background ->
        let style = style_of_entity entity "assets/turtle_golf.jpeg" in
        H.div ~a:[ H.a_style style ] []
      | `Scoreboard score ->
        let style = style_of_entity entity "" in
        H.div ~a:[ H.a_style style ] (
            score
            |> IMap.bindings
            |> CCList.flat_map (fun (player, score) -> 
                   let content = H.pcdata (sp "Flap%d: %d" player score) in
                   [
                     H.div [ content ];
                     H.br ();
                   ]
                 )
          )
    end
  in
  let render_debug_overlay entity =
    if not debug then H.div [] else (
      let debug_text = H.pcdata (
          match entity.typ with
          | `Bird _ -> "bird"
          | `Wall -> "wall"
          | `Background -> "background"
          | `Scoreboard _ -> "scoreboard"
          | `Cookie -> "cookie"
          | `Homing_missile _ -> "homing-missile"
        )
      in
      let background_color = "red" in
      let z_index =
        let put_hitbox_on_top = match entity.typ with
          | `Bird _ | `Cookie | `Homing_missile _ -> true
          | _ -> false
        in
        if put_hitbox_on_top then 1 else -1
      in
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

let init_game () =
  let root = Dom_html.getElementById Constants.html_id in
  Dom.appendChild root reactive_view;
  Dom_html.document##.onkeydown := Dom_html.handler (fun e ->
    Printf.printf "keycode: %d\n" e##.keyCode;
    let _ = match e##.keyCode with
      (* 32 = space*)
      | 87 (*w*) ->        Game.Event.sink_eupd (`WingFlap 0) 
      | 38 (*arrow-up*) -> Game.Event.sink_eupd (`WingFlap 1)
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
    init_game ();
    Game.Event.feed_frp ();
    Js._false
  )

let _ = main ()

