open Raylib

(* ----- Game constants ----- *)
let screen_width = 800
let screen_height = 600
let game_scale = 3.

let player_speed = 7.5

let bullet_speed = (-10.0)

let padding = 10.

let swarm_delay = 0.75
let enemy_x_max = 6
let enemy_move_amount = 35.
let enemies_per_row = 7
let enemy_count = 21

(* ----- Types ----- *)
type game_object = {
  texture : Texture.t;
  scale : float;
  position : Vector2.t;
  velocity: Vector2.t
}

type game_state = {
  player : game_object;
  enemies : game_object list;
  swarm_position : int;
  enemy_movement_clock : float;
  bullet : game_object option;
  score : int
}

(* ----- Util functions ----- *)
let apply_velocity position velocity =
  Vector2.add position velocity

let get_texture_dimensions texture =
  let width = float_of_int (Texture.width texture) *. game_scale in
  let height = float_of_int (Texture.height texture) *. game_scale in
  (width, height)

let enemy_position_of_int i =
  let m = i mod enemy_x_max in
  let x = if m = 0 then 0. else if (i / enemy_x_max) mod 2 = 0 then 1. else (-1.) in
  let y = if m = 0 then 1. else 0. in
  Vector2.create (enemy_move_amount *. x) (enemy_move_amount *. y)

let move_enemies game_state =
  List.map (fun enemy -> { enemy with position = Vector2.add enemy.position @@ enemy_position_of_int game_state.swarm_position }) game_state.enemies  

let instantiate_bullet position =
  let texture = load_texture "resources/bullet.png" in
  { texture; scale = game_scale; position; velocity = Vector2.create 0. bullet_speed }  

(* ----- Updating functions ----- *)
let update_bullet game_state =
  match game_state.bullet with
  | None ->
    if is_key_down Key.Space then
      let (player_width, _) = get_texture_dimensions game_state.player.texture in
      { game_state with bullet = Some (instantiate_bullet @@ Vector2.add (game_state.player.position) @@ Vector2.create (player_width /. 2.) (-20.)) }
    else
      game_state
  | Some bullet ->
      let new_position = apply_velocity bullet.position bullet.velocity in
      if Vector2.y new_position < 0. then
        { game_state with bullet = None }
      else
        { game_state with bullet = Some { bullet with position = new_position } }  

let update_player game_state =
  let new_velocity = Vector2.zero () in
  if is_key_down Key.A then
    Vector2.set_x new_velocity (Vector2.x new_velocity -. player_speed);
  if is_key_down Key.D then
    Vector2.set_x new_velocity (Vector2.x new_velocity +. player_speed);
  { game_state with
    player = {
      game_state.player with
      velocity = new_velocity;
      position = apply_velocity game_state.player.position game_state.player.velocity
    }
  }

let update_enemies game_state =
  if game_state.enemy_movement_clock <= 0. then
    let new_game_state = { game_state with swarm_position = game_state.swarm_position + 1; enemy_movement_clock = swarm_delay } in
    { new_game_state with enemies = move_enemies new_game_state }
  else
    { game_state with enemy_movement_clock = game_state.enemy_movement_clock -. (get_frame_time ()) }
    
let handle_collision game_state =
  match game_state.bullet with
  | None -> game_state  (* If there's no bullet, return the game state unchanged *)
  | Some bullet ->
    let check_collision enemy bullet =
      let (enemy_width, enemy_height) = get_texture_dimensions enemy.texture in
      let (enemy_x, enemy_y) = (Vector2.x enemy.position, Vector2.y enemy.position) in
      let (bullet_width, bullet_height) = get_texture_dimensions bullet.texture in
      let (bullet_x, bullet_y) = (Vector2.x bullet.position, Vector2.y bullet.position) in
      not (
        enemy_x > bullet_x +. bullet_width ||
        enemy_x +. enemy_width < bullet_x ||
        enemy_y > bullet_y +. bullet_height ||
        enemy_y +. enemy_height < bullet_y
      )
    in
    let rec collide enemies bullet acc =
      match enemies with
      | [] -> { game_state with enemies = List.rev acc }
      | e :: es when check_collision e bullet ->
        { game_state with bullet = None; enemies = acc @ es; score = game_state.score + 100 }
      | e :: es -> collide es bullet (e :: acc)
    in
    collide game_state.enemies bullet []    

(* ----- Drawing functions ----- *)
let draw_game_object game_object =
  draw_texture_ex game_object.texture game_object.position 0. game_object.scale Color.white

let draw_score score =
  draw_text (string_of_int score) 20 20 50 Color.white

let draw_bullet bullet =
  match bullet with
  | None -> ()
  | Some(bullet) -> draw_game_object bullet

(* ----- Setup functions ----- *)
let generate_enemies n =
  let rec aux n acc =
    if n = 0 then acc
    else
      let texture = load_texture "resources/enemy.png" in
      let (enemy_width, enemy_height) = get_texture_dimensions texture in

      (* Calculate x and y positions with spacing *)
      let x_pos =
        (float_of_int ((n - 1) mod enemies_per_row)) *. (enemy_width +. (padding *. 5.))
      in
      let y_pos =
        (float_of_int ((n - 1) / enemies_per_row)) *. (enemy_height +. padding)
      in

      aux (n - 1) ({
        texture;
        scale = game_scale;
        position = Vector2.create x_pos y_pos;
        velocity = Vector2.zero ()
      } :: acc)
  in
  aux n []

let load_initial_game_state () =
  let player =
    let texture = load_texture "resources/player.png" in
    let (player_width, player_height) = get_texture_dimensions texture in
    let x_pos =
      (float_of_int screen_width /. 2.0) -. (player_width /. 2.0)
    in
    let y_pos =
      float_of_int screen_height -. (player_height +. padding)
    in
    {
      texture;
      scale = game_scale;
      position = Vector2.create x_pos y_pos;
      velocity = Vector2.zero ()
    }
  in
  let enemies = generate_enemies enemy_count in
  {
    player = player;
    enemies = enemies;
    swarm_position = 0;
    enemy_movement_clock = swarm_delay;
    bullet = None; score = 0
  }


let setup () =
  init_window screen_width screen_height "Galacaml";
  set_target_fps 60;
  load_initial_game_state ()
    
(* ----- Core game loop functions ----- *)
let update game_state =
  update_player game_state |>
  update_enemies |>
  update_bullet |>
  handle_collision

let draw game_state =
  begin_drawing ();
  clear_background Color.black;
  draw_game_object game_state.player;
  List.iter draw_game_object game_state.enemies;
  draw_score game_state.score;
  draw_bullet game_state.bullet;
  end_drawing ();
  game_state

let rec loop game_state =
  match window_should_close () with
  | true -> close_window ()
  | false ->
    update game_state |> draw |> loop

let () = setup () |> loop