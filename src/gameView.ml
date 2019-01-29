open Js_of_ocaml
open Lwt_react
open ReactiveData

open GameEntity.T
open GameModel.T

module H = Tyxml_js.Html
module R = Tyxml_js.R.Html
module IMap = CCMap.Make(CCInt)

let sp = Printf.sprintf
let log = Printf.printf 

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
let make_view :
  players:int ->
  debug:bool ->
  game_model_s:(GameModel.t option S.t) ->
  Dom.node Js.t =
  fun ~players ~debug ~game_model_s -> 
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
        (* log "game_model update\n"; *)
        model_opt
        |> CCOpt.to_list
        |> CCList.flat_map GameModel.to_list
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

