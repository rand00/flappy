open Js_of_ocaml
open Lwt.Infix
open Lwt_react
open ReactiveData
open Gg

module H = Tyxml_js.Html
module R = Tyxml_js.R.Html

let debug = false
let fps = 30.
let players = 3

let sp = Printf.sprintf
(*> todo make a version of log that auto-prints newline*)
let log = Printf.printf 
let (%) f g x = f (g x)
let (%>) f g x = g (f x)

module IMap = CCMap.Make(CCInt)

(* type entity_html = Html_types.body_content H.elt (\*possibly reactive*\) *)

module Game = struct

  module Event = struct

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

  end

  module Entity = struct

    module T = struct 

      type bird = [ `Bird of bird_data ]
      [@@deriving show]
      and bird_data = {
        player : int;
        movement_vector : V2.t;
      }[@@deriving show]

      type 'a homing_missile = [ `Homing_missile of 'a ]
      [@@deriving show]

      type feathers = [ `Feathers of int (*player*) ]
      [@@deriving show]

      type cookie = [ `Cookie ]
      [@@deriving show]

      type wall = [ `Wall of [ `Top | `Bottom ] ]
      [@@deriving show]

      type background = [ `Background ]
      [@@deriving show]

      type scoreboard = [ `Scoreboard of (int IMap.t [@printer IMap.pp CCInt.pp CCInt.pp]) ]
      [@@deriving show]

      type typ = [
        | bird
        | cookie
        | feathers
        | wall
        | background
        | scoreboard
        | homing_missile_data homing_missile
      ][@@deriving show]

      and target = [ bird | homing_missile_data homing_missile ]
      
      and homing_missile_data = {
        target : target t option;
        movement_vector : V2.t;
      }[@@deriving show]

      and 'a t = {
        typ : 'a;
        width : int;
        height : int;
        pos_x : int;
        pos_y : int;
        collided : bool;
        timeout : int option; (*n.o. frames*)
      }[@@deriving show]

    end

    include T

    module To_typ = struct 
    
      let birds x = (x : bird t list :> typ t list)
      let walls x = (x : wall t list :> typ t list)
      let homing_missiles x =
        (x : homing_missile_data homing_missile t list :> typ t list)
      (*goto add coercion helpers for other types too*)

    end

    module To_target = struct 

      let birds x = (x : bird t list :> target t list)
      let homing_missiles x =
        (x : homing_missile_data homing_missile t list :> target t list)

    end

    let distance e e' =
      let x , y  = float e.pos_x, float e.pos_y in
      let x', y' = float e'.pos_x, float e'.pos_y in
      sqrt ((x -. x') ** 2. +. (y -. y') ** 2.)

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

    module Bird = struct 

      let init (view_w, view_h) i = {
        typ = `Bird { player = i; movement_vector = V2.v 0. 0. };
        width = 30;
        height = 30;
        pos_x = (float view_w /. 4.) |> truncate;
        pos_y = (float (i * view_h) /. (float players)) |> truncate;
        collided = false;
        timeout = None;
      }

      let move : Event.direction option -> bird t -> bird t =
        fun direction ({ typ = `Bird bird_data } as bird) -> 
        let fall_vector = V2.v 0. 11. in
        let slowdown = 0.9 in
        let movement_vector =
          if bird.collided then
            fall_vector
          else begin
            match direction with
            | None -> V2.(slowdown * bird_data.movement_vector)
            | Some direction -> 
              let direction_vec = match direction with
                | `Up    -> V2.v    0. (-70.)
                | `Down  -> V2.v    0.   35.
                | `Left  -> V2.v (-35.)   0.
                | `Right -> V2.v   35.    0.
              in
              let movement_vec =
                V2.(bird_data.movement_vector + direction_vec)
              in
              V2.(slowdown * movement_vec)
          end
        in
        let final_vector = V2.(movement_vector + fall_vector) in
        { bird with
          typ = `Bird { bird_data with movement_vector };
          pos_x = bird.pos_x + (truncate V2.(x final_vector));
          pos_y = bird.pos_y + (truncate V2.(y final_vector));
        }

    end

    module Feathers = struct

      let of_bird ({ typ = `Bird bird_data } as bird) =
        { bird with
          typ = `Feathers bird_data.player;
          collided = false;
          timeout = None
        }

    end

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
        let is_top = Random.bool () in
        let aspect = 0.17 in
        let height = Random.int (max 1 (view_h / 3)) + (view_h / 2) in
        let width = float height *. aspect |> truncate in
        let pos_y = if is_top then 0 else view_h - height 
        in
        [
          {
            typ = `Wall (if is_top then `Top else `Bottom);
            width;
            height;
            pos_x = view_w;
            pos_y;
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
        let width = 200 in
        let pos_x = view_w
        and pos_y = (Random.float 1. *. float (view_h - width)) |> truncate in
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
        let time_to_spawn =
          frame mod truncate (fps *. 0.5) = 0
          && Random.float 1.0 < (1. /. 4.)
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

      let choose_target targets ({ typ = `Homing_missile missile_data } as missile) =
        let target =
          List.fold_left (fun acc e ->
              match acc with
              | None -> if distance missile e > 0.1 then Some e else None
              | Some e' ->
                let factor e = match e.typ with
                  | `Bird _ -> 0.6 | _ -> 1.
                in
                let chosen =
                  let dist_e, dist_e' = distance missile e, distance missile e' in
                  let prio_e, prio_e' = factor e, factor e' in
                  if
                    prio_e *. dist_e < prio_e' *. dist_e' &&
                    dist_e > 0.1 (*to avoid itself*)
                  then e
                  else e'
                in Some chosen
            ) None targets
        in
        { missile with typ = `Homing_missile {
              missile_data with target;
            }}

      let move ({ typ = `Homing_missile missile_data } as missile) =
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
                 (* let dist = V2.norm diff_vec in *)
                 (* let dist_factor = min (0.000030 *. sqrt dist) 0.1 in *)
                 let dist_factor = 0.003 in
                 V2.(missile_data.movement_vector + (dist_factor * diff_vec))
            end
        in
        { missile with
          typ = `Homing_missile { missile_data with movement_vector };
          pos_x = missile.pos_x + (truncate V2.(x movement_vector));
          pos_y = missile.pos_y + (truncate V2.(y movement_vector));
        }

    end

    module Scoreboard = struct 
    
      let init (view_w, view_h) = {
        typ = `Scoreboard IMap.empty;
        width = 550;
        height = 0;
        pos_x = view_w - 283;
        pos_y = view_h - (80 * players);
        collided = false;
        timeout = None;
      }

      let increment
          ~scored_now
          ~bird:({ typ = `Bird {player} })
          ({ typ = `Scoreboard scores } as scoreboard) =
        let scores =
          scores |> IMap.update player (function
              | None -> Some scored_now
              | Some score -> Some (scored_now + score)
            )
        in
        { scoreboard with typ = `Scoreboard scores }

    end

  end

  module Model = struct

    module T = struct 

      type t = {
        birds :      Entity.bird Entity.t list;
        feathers :   Entity.feathers Entity.t list;
        walls :      Entity.wall Entity.t list;
        cookies :    Entity.cookie Entity.t list;
        homing_missiles :
          Entity.homing_missile_data Entity.homing_missile
            Entity.t list;
        background : Entity.background Entity.t;
        scoreboard : Entity.scoreboard Entity.t;
        paused : bool;
      }[@@deriving show]

    end

    include T

    let to_list : t -> Entity.typ Entity.t list =
      fun m -> List.flatten [
          [ (m.background    : Entity.background Entity.t  :> Entity.typ Entity.t) ];
          (m.feathers        : Entity.feathers Entity.t list :> Entity.typ Entity.t list);
          (m.walls           : Entity.wall Entity.t list   :> Entity.typ Entity.t list);
          (m.cookies         : Entity.cookie Entity.t list :> Entity.typ Entity.t list);
          (m.birds           : Entity.bird Entity.t list   :> Entity.typ Entity.t list);
          (m.homing_missiles :
             Entity.homing_missile_data
               Entity.homing_missile Entity.t list
           :> Entity.typ Entity.t list
          );
          [ (m.scoreboard    : Entity.scoreboard Entity.t :> Entity.typ Entity.t) ];
        ]

    let init view_dimensions = {
      paused = false;
      birds = List.init players (Entity.Bird.init view_dimensions);
      feathers = [];
      walls = [];
      cookies = [];
      homing_missiles = [];
      background = Entity.init_background view_dimensions;
      scoreboard = Entity.Scoreboard.init view_dimensions;
    }

    let update_birds_and_feathers
        ?player_and_direction
        ~collision_entities
        model
      =
      (*Moving birds and spawning feathers*)
      let init = [], model.feathers in
      let open Entity.T in
      let birds, feathers =
        model.birds |> List.fold_left (
          fun (acc_birds, acc_feathers) ({ typ = `Bird bird_data } as bird) -> 
            let should_move = match player_and_direction with
              | Some (player, _) -> player = bird_data.player
              | None -> true
            in
            if not should_move then
              bird :: acc_birds, acc_feathers
            else begin
              let bird, just_collided =
                let bird' = 
                  bird
                  |> Entity.Bird.move (CCOpt.map snd player_and_direction)
                  |> Entity.mark_if_collision collision_entities in
                let just_collided = bird'.collided && not bird.collided in
                bird', just_collided
              in
              if just_collided then
                let feathers = Entity.Feathers.of_bird bird in
                bird :: acc_birds, feathers :: acc_feathers
              else
                bird :: acc_birds, acc_feathers
            end
        ) init
      in
      (*Resetting birds and removing feathers*)
      let birds_assoc, feathers =
        let open Entity.T in
        let birds_of_players =
          birds |> List.map (
            fun ({ Entity.typ = `Bird {player} } as e) -> player, e
          )
        in
        let init = birds_of_players, [] in
        feathers
        |> List.fold_left (
          fun (acc_birds, feathers_left) ({ typ = `Feathers feather_player } as feather) ->
            let other_birds =
              birds |> List.filter (
                fun { Entity.typ = `Bird {player} } -> not (player = feather_player)
              )
            in
            if feather |> Entity.collides other_birds then
              let revive_bird = function
                | None -> None
                | Some bird -> 
                  Some { bird with
                         pos_x = feather.pos_x;
                         pos_y = feather.pos_y;
                         collided = false;
                         timeout = None;
                       }
              in
              let acc_birds =
                CCList.Assoc.update ~eq:CCInt.equal revive_bird
                  feather_player
                  acc_birds
              in
              acc_birds, feathers_left
            else 
              acc_birds, feather :: feathers_left
        ) init
      in
      let birds = birds_assoc |> List.map snd in
      birds, feathers

  end

end

open Game.Entity.T
open Game.Model.T

(**Update*)

let when_not_paused model model' =
  if model.paused then model else model'

let game_model_s : Game.Model.t option React.signal = 
  let update model event =
    match event with
    | `WingFlap (player, direction) ->
      when_not_paused model @@
      let birds, feathers =
        Game.Model.update_birds_and_feathers
          ~player_and_direction:(player, direction)
          ~collision_entities:(
            (model.walls           |> Game.Entity.To_typ.walls) @
            (model.homing_missiles |> Game.Entity.To_typ.homing_missiles)
          )
          model
      in
      { model with birds; feathers }
    | `Frame frame ->
      when_not_paused model @@
      let dimensions = model.background.width, model.background.height in
      let walls =
        model.walls
        |> List.map (Game.Entity.move_x (-5))
        |> List.filter (not % (Game.Entity.is_out_of_bounds dimensions))
        |> List.append (Game.Entity.init_wall frame dimensions) in
      let homing_missiles = 
        model.homing_missiles
        |> List.map (
          Game.Entity.Homing_missile.choose_target (
            (model.birds           |> Game.Entity.To_target.birds) @
            (model.homing_missiles |> Game.Entity.To_target.homing_missiles)
          )
          %> (fun e -> { e with timeout = e.timeout |> CCOpt.map pred })
          %> Game.Entity.mark_if_collision (
            (model.birds |> Game.Entity.To_typ.birds) @
            (model.walls |> Game.Entity.To_typ.walls)
          )
            ~change:(fun e -> {
                  e with
                  width = e.width + 100;
                  timeout = Some (fps *. 3. |> truncate);
                })
          %> Game.Entity.Homing_missile.move
        )
        |> List.filter (
          not % fun e -> e.timeout |> CCOpt.exists (fun t -> t < 0)
        )
        |> List.filter (not % Game.Entity.is_out_of_bounds dimensions)
        |> List.append (Game.Entity.Homing_missile.init frame dimensions) in
      let birds, feathers =
        Game.Model.update_birds_and_feathers 
          ~collision_entities:(
            (walls           |> Game.Entity.To_typ.walls) @
            (homing_missiles |> Game.Entity.To_typ.homing_missiles)
          )
          model
      in
      let cookies =
        model.cookies
        |> List.map (Game.Entity.move_x (-10))
        |> List.filter (not % (Game.Entity.is_out_of_bounds dimensions))
        |> List.append (Game.Entity.init_cookie frame dimensions)
      in
      let scoreboard, cookies =
        model.birds
        |> List.fold_left (fun (scoreboard, cookies) bird -> 
            let cookies_left = 
              cookies
              |> List.filter (not % Game.Entity.collides [bird])
            in
            let scored_now = List.(length cookies - length cookies_left) in
            let scoreboard = Game.Entity.Scoreboard.increment
                ~scored_now
                ~bird
                scoreboard
            in
            scoreboard, cookies_left
          ) (model.scoreboard, cookies)
      in
      {
        birds;
        feathers;
        walls;
        cookies;
        homing_missiles;
        scoreboard;
        background = model.background;
        paused = model.paused;
      }
    | `PauseToggle -> { model with paused = not model.paused }
    | `ViewResize dimensions ->
      let prev_dimensions = (model.background.width, model.background.height) in
      let reposition e =
        Game.Entity.reposition prev_dimensions dimensions e
      in
      let birds = model.birds |> List.map reposition in
      let feathers = model.feathers |> List.map reposition in
      let walls = model.walls |> List.map reposition in
      let cookies = model.cookies |> List.map reposition in
      let homing_missiles = model.homing_missiles |> List.map reposition in
      let scoreboard = model.scoreboard |> reposition in
      let background = model.background |> Game.Entity.resize dimensions 
      in
      {
        birds;
        feathers;
        walls;
        background;
        homing_missiles;
        scoreboard;
        cookies;
        paused = model.paused;
      }
  in
  Game.Event.sink_e
  |> E.fold update (Game.Model.init (1920, 1080))
  |> E.map CCOpt.pure
  |> S.hold None

(**View*)

let style_of_entity
    ?extend ?extend_left ?extend_right
    ?rotate ?background_color ?z_index ?filter
    entity image
  =
  let ext = CCOpt.get_or ~default:0 extend in
  let ext_l = CCOpt.get_or ~default:0 extend_left in
  let ext_r = CCOpt.get_or ~default:0 extend_right in
  let width, height =
    entity.width + ext * 2 + ext_l + ext_r,
    entity.height + ext * 2 + ext_l + ext_r
  and pos_x, pos_y =
    entity.pos_x - ext - ext_l,
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
      | `Bird {player} ->
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
      | `Feathers player ->
        let extend = 50 in
        let hue_rotate_degrees =
          360. *. (float player) /. (float players) |> truncate in
        let style = style_of_entity entity "assets/feathers.gif"
            ~extend
            ~filter:(`Hue_rotate hue_rotate_degrees)
        in
        H.div ~a:[ H.a_style style ] []
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
      | `Wall position ->
        let image =
          match position with 
          | `Bottom -> "assets/drawn/street_light.png"
          | `Top ->    "assets/drawn/hanging_power_cords.png"
        in
        let style = style_of_entity entity image in
        H.div ~a:[ H.a_style style ] []
      | `Background ->
        let style = style_of_entity entity "assets/drawn/smoggy_buildings.png" in
        H.div ~a:[ H.a_style style ] []
      | `Scoreboard score ->
        let style = style_of_entity entity "" in
        H.div ~a:[ H.a_style style ] (
          score
          |> IMap.bindings
          |> CCList.flat_map (fun (player, score) -> 
              let content = H.txt (sp "Flap%d: %d" player score) in
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
      let debug_text = H.txt (
          match entity.typ with
          | `Bird _ -> "bird"
          | `Feathers _ -> "feathers"
          | `Wall _ -> "wall"
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
        | 87 (*w*) -> Game.Event.sink_eupd (`WingFlap (0, `Up)) 
        | 65 (*a*) -> Game.Event.sink_eupd (`WingFlap (0, `Left)) 
        | 83 (*s*) -> Game.Event.sink_eupd (`WingFlap (0, `Down)) 
        | 68 (*d*) -> Game.Event.sink_eupd (`WingFlap (0, `Right)) 

        | 38 (*arrow-up*)    -> Game.Event.sink_eupd (`WingFlap (1, `Up))
        | 37 (*arrow-left*)  -> Game.Event.sink_eupd (`WingFlap (1, `Left))
        | 40 (*arrow-down*)  -> Game.Event.sink_eupd (`WingFlap (1, `Down))
        | 39 (*arrow-right*) -> Game.Event.sink_eupd (`WingFlap (1, `Right))

        | 73 (*i*) -> Game.Event.sink_eupd (`WingFlap (2, `Up))
        | 74 (*j*) -> Game.Event.sink_eupd (`WingFlap (2, `Left))
        | 75 (*k*) -> Game.Event.sink_eupd (`WingFlap (2, `Down))
        | 76 (*l*) -> Game.Event.sink_eupd (`WingFlap (2, `Right))

        | 80 (*p*) -> Game.Event.sink_eupd `PauseToggle
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

