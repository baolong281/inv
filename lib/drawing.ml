open Graphics
open Vec2
open Arm
open Joint
open State

let draw_joint joint =
  set_line_width joint.lw;
  moveto (int_of_float joint.a.x) (int_of_float joint.a.y);
  lineto (int_of_float joint.b.x) (int_of_float joint.b.y)

let draw_arm arm = List.iter draw_joint arm.joints

let draw_skeleton state =
  set_color foreground;
  draw_circle (int_of_float state.cursor.x) (int_of_float state.cursor.y) 7;
  List.iter draw_arm state.arms

let rec event_loop state =
  let new_state = update_cursor state in
  let updated_state = update_state new_state in
  clear_graph ();
  draw_skeleton updated_state;
  let delay_time = 1.0 /. 60.0 in
  Unix.sleepf delay_time;
  event_loop updated_state

let create_window width height ~length ~breaks =
  open_graph (" " ^ string_of_int width ^ "x" ^ string_of_int height);
  let arms =
    let base = { x = float width /. 2.0; y = float height /. 2.0 } in
    List.init 1 (fun _ -> create_arm ~length ~breaks base)
  in
  event_loop { arms; cursor = { x = 0.0; y = 0.0 }; width; height }
