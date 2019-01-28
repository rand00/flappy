open Js_of_ocaml
open Lwt.Infix
open Lwt_react
open ReactiveData
open Gg

module H = Tyxml_js.Html
module R = Tyxml_js.R.Html

let sp = Printf.sprintf
let log = Printf.printf 

let (%) f g x = f (g x)
let (%>) f g x = g (f x)

module IMap = CCMap.Make(CCInt)

open GameEntity.T
open GameModel.T

let debug = false
let fps = 30.
let players = 3

(*Note: Intentionally evaluating else-case to test performance*)
let when_not_paused model model' =
  if model.paused then model else model'

let update model event =
  match event with
  | `WingFlap (player, direction) ->
    when_not_paused model @@
    let birds, feathers =
      GameModel.update_birds_and_feathers
        ~player_and_direction:(player, direction)
        ~collision_entities:(
          (model.walls           |> GameEntity.To_typ.walls) @
          (model.homing_missiles |> GameEntity.To_typ.homing_missiles)
        )
        model
    in
    { model with birds; feathers }
  | `Frame frame ->
    when_not_paused model @@
    let dimensions = model.background.width, model.background.height in
    let walls =
      model.walls
      |> List.map (GameEntity.move_x (-5))
      |> List.filter (not % (GameEntity.is_out_of_bounds ~dimensions))
      |> List.append (GameEntity.init_wall ~frame ~dimensions ~fps) in
    let homing_missiles = 
      model.homing_missiles
      |> List.map (fun e -> { e with timeout = e.timeout |> CCOpt.map pred })
      |> List.filter (not % fun e -> e.timeout |> CCOpt.exists (fun t -> t < 0))
      |> List.map (
        GameEntity.Homing_missile.choose_target (
          (model.birds           |> GameEntity.To_target.birds) @
          (model.homing_missiles |> GameEntity.To_target.homing_missiles)
        )
        %> GameEntity.mark_if_collision (
          (model.birds |> GameEntity.To_typ.birds) @
          (model.walls |> GameEntity.To_typ.walls)
        )
          ~change:(fun e -> {
                e with
                width = e.width + 100;
                timeout = Some (fps *. 3. |> truncate);
              })
        %> GameEntity.Homing_missile.move
      )
      |> List.filter (not % GameEntity.is_out_of_bounds ~dimensions)
      |> List.append (GameEntity.Homing_missile.init ~frame ~dimensions ~fps) in
    let birds, feathers =
      GameModel.update_birds_and_feathers 
        ~collision_entities:(
          (walls           |> GameEntity.To_typ.walls) @
          (homing_missiles |> GameEntity.To_typ.homing_missiles)
        )
        model
    in
    let cookies =
      model.cookies
      |> List.map (GameEntity.move_x (-10))
      |> List.filter (not % (GameEntity.is_out_of_bounds ~dimensions))
      |> List.append (GameEntity.init_cookie ~frame ~dimensions ~fps)
    in
    let scoreboard, cookies =
      model.birds
      |> List.fold_left (fun (scoreboard, cookies) bird -> 
          let cookies_left = 
            cookies
            |> List.filter (not % GameEntity.collides [bird])
          in
          let scored_now = List.(length cookies - length cookies_left) in
          let scoreboard = GameEntity.Scoreboard.increment
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
      GameEntity.reposition prev_dimensions dimensions e
    in
    let birds = model.birds |> List.map reposition in
    let feathers = model.feathers |> List.map reposition in
    let walls = model.walls |> List.map reposition in
    let cookies = model.cookies |> List.map reposition in
    let homing_missiles = model.homing_missiles |> List.map reposition in
    let scoreboard = model.scoreboard |> reposition in
    let background = model.background |> GameEntity.resize dimensions 
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

let game_model_s : GameModel.t option React.signal = 
  let initial_game_model =
    GameModel.init ~view_dimensions:(1920, 1080) ~players
  in
  GameEvent.sink_e
  |> E.fold update initial_game_model
  |> E.map CCOpt.pure
  |> S.hold None

let update_view_size () =
  let (>>=) = Js.Optdef.bind in
  let (>>|) = Js.Optdef.map in
  begin
    Dom_html.window##.innerWidth >>= fun w ->
    Dom_html.window##.innerHeight >>| fun h ->
    GameEvent.sink_eupd (`ViewResize (w, h))
  end |> ignore

let handle_keypresses ev =
  Printf.printf "keycode: %d\n" ev##.keyCode;
  let _ = match ev##.keyCode with
    | 87 (*w*) -> GameEvent.sink_eupd (`WingFlap (0, `Up)) 
    | 65 (*a*) -> GameEvent.sink_eupd (`WingFlap (0, `Left)) 
    | 83 (*s*) -> GameEvent.sink_eupd (`WingFlap (0, `Down)) 
    | 68 (*d*) -> GameEvent.sink_eupd (`WingFlap (0, `Right)) 

    | 38 (*arrow-up*)    -> GameEvent.sink_eupd (`WingFlap (1, `Up))
    | 37 (*arrow-left*)  -> GameEvent.sink_eupd (`WingFlap (1, `Left))
    | 40 (*arrow-down*)  -> GameEvent.sink_eupd (`WingFlap (1, `Down))
    | 39 (*arrow-right*) -> GameEvent.sink_eupd (`WingFlap (1, `Right))

    | 73 (*i*) -> GameEvent.sink_eupd (`WingFlap (2, `Up))
    | 74 (*j*) -> GameEvent.sink_eupd (`WingFlap (2, `Left))
    | 75 (*k*) -> GameEvent.sink_eupd (`WingFlap (2, `Down))
    | 76 (*l*) -> GameEvent.sink_eupd (`WingFlap (2, `Right))

    | 80 (*p*) -> GameEvent.sink_eupd `PauseToggle
    | _ -> ()
  in
  Js._true

let init_game () =
  let root = Dom_html.getElementById Constants.html_id in
  let reactive_view =
    GameView.make_view
      ~players
      ~debug
      ~game_model_s
  in
  Dom.appendChild root reactive_view;
  Dom_html.document##.onkeydown := Dom_html.handler handle_keypresses;
  update_view_size ();
  Dom_html.window##.onresize := Dom_html.handler (fun _ ->
      update_view_size ();
      Js._true
    )

let main () =
  Dom_html.window##.onload := Dom_html.handler (fun _ -> 
      Random.self_init ();
      init_game ();
      GameEvent.feed_frp ~fps;
      Js._false
    )

let _ = main ()

