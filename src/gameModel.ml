
module T = struct 

  type t = {
    birds :      GameEntity.bird GameEntity.t list;
    feathers :   GameEntity.feathers GameEntity.t list;
    walls :      GameEntity.wall GameEntity.t list;
    cookies :    GameEntity.cookie GameEntity.t list;
    homing_missiles :
      GameEntity.homing_missile_data GameEntity.homing_missile
        GameEntity.t list;
    background : GameEntity.background GameEntity.t;
    scoreboard : GameEntity.scoreboard GameEntity.t;
    paused : bool;
  }[@@deriving show]

end

include T

let to_list : t -> GameEntity.typ GameEntity.t list =
  fun m ->
  let module Entity = GameEntity in
  List.flatten [
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

let init ~view_dimensions ~players = {
  paused = false;
  birds = List.init players (GameEntity.Bird.init ~players view_dimensions);
  feathers = [];
  walls = [];
  cookies = [];
  homing_missiles = [];
  background = GameEntity.init_background view_dimensions;
  scoreboard = GameEntity.Scoreboard.init ~players ~view_dimensions;
}

let update_birds_and_feathers
    ?player_and_direction
    ~collision_entities
    model
  =
  (*Moving birds and spawning feathers*)
  let init = [], model.feathers in
  let open GameEntity.T in
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
              |> GameEntity.Bird.move (CCOpt.map snd player_and_direction)
              |> GameEntity.mark_if_collision collision_entities in
            let just_collided = bird'.collided && not bird.collided in
            bird', just_collided
          in
          if just_collided then
            let feathers = GameEntity.Feathers.of_bird bird in
            bird :: acc_birds, feathers :: acc_feathers
          else
            bird :: acc_birds, acc_feathers
        end
    ) init
  in
  (*Resetting birds and removing feathers*)
  let birds_assoc, feathers =
    let open GameEntity.T in
    let birds_of_players =
      birds |> List.map (
        fun ({ GameEntity.typ = `Bird {player} } as e) -> player, e
      )
    in
    let init = birds_of_players, [] in
    feathers
    |> List.fold_left (
      fun (acc_birds, feathers_left) ({ typ = `Feathers feather_player } as feather) ->
        let other_birds =
          birds |> List.filter (
            fun { GameEntity.typ = `Bird {player} } -> not (player = feather_player)
          )
        in
        if feather |> GameEntity.collides other_birds then
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

