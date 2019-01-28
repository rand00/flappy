open Gg

module IMap = CCMap.Make(CCInt)

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
let is_out_of_bounds ~dimensions:(w, h) e =
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

  let init ~players (view_w, view_h) i = {
    typ = `Bird { player = i; movement_vector = V2.v 0. 0. };
    width = 30;
    height = 30;
    pos_x = (float view_w /. 4.) |> truncate;
    pos_y = (float (i * view_h) /. (float players)) |> truncate;
    collided = false;
    timeout = None;
  }

  let move : GameEvent.direction option -> bird t -> bird t =
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

let init_wall ~frame ~dimensions:(view_w, view_h) ~fps =
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

let init_cookie ~frame ~dimensions:(view_w, view_h) ~fps =
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

  let init ~frame ~dimensions:(view_w, view_h) ~fps =
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

  let init ~players ~view_dimensions:(view_w, view_h) = {
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

