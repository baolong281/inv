open Graphics
open Unix

type 'a vec2 = { x : 'a; y : 'a }
type joint = { a : float vec2; b : float vec2; len : float; lw : int }
type arm = { joints : joint list; base : float vec2 }
type state = { arms : arm list; cursor : float vec2; width : int; height : int }

let update_cursor state =
  let mx, my = mouse_pos () in
  { state with cursor = { x = float mx; y = float my } }

let update_joint_b ~dest joint =
  let dist_x = dest.x -. joint.a.x in
  let dist_y = dest.y -. joint.a.y in
  let theta = atan2 dist_y dist_x in
  let len = joint.len in
  let new_b_x = joint.a.x +. (len *. cos theta) in
  let new_b_y = joint.a.y +. (len *. sin theta) in
  { x = new_b_x; y = new_b_y }

let update_joint_a ~dest ~new_b joint =
  let dx = new_b.x -. joint.a.x in
  let dy = new_b.y -. joint.a.y in
  { x = dest.x -. dx; y = dest.y -. dy }

let update_joints ~dest arm =
  let updated_joints =
    List.fold_right
      (fun joint (prev_a, joints) ->
        let new_b = update_joint_b ~dest:prev_a joint in
        let new_a = update_joint_a ~dest:prev_a ~new_b joint in
        let updated_joint = { joint with a = new_a; b = new_b } in
        (new_a, updated_joint :: joints))
      arm.joints (dest, [])
  in
  snd updated_joints

let update_arm ~cursor arm =
  let updated_joints = update_joints ~dest:cursor arm in
  let base = List.hd updated_joints in
  let dx = base.a.x -. arm.base.x in
  let dy = base.a.y -. arm.base.y in
  let updated_joints =
    List.map
      (fun joint ->
        { joint with a = { x = joint.a.x -. dx; y = joint.a.y -. dy } })
      updated_joints
  in
  { arm with joints = updated_joints }

let draw_joint joint =
  set_line_width joint.lw;
  moveto (int_of_float joint.a.x) (int_of_float joint.a.y);
  lineto (int_of_float joint.b.x) (int_of_float joint.b.y)

let draw_arm arm = List.iter draw_joint arm.joints

let update_state state =
  let updated_arms = List.map (update_arm ~cursor:state.cursor) state.arms in
  { state with arms = updated_arms }

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
  sleepf delay_time;
  event_loop updated_state

let generate_joints ~n ~step ~x_center ~y_center =
  let dist (x1, y1) (x2, y2) =
    sqrt (((x2 -. x1) ** 2.0) +. ((y2 -. y1) ** 2.0))
  in
  List.init n (fun i ->
      let x_start = (float_of_int i *. step) +. x_center in
      let a = { x = x_start; y = y_center } in
      let b = { x = x_start +. step; y = y_center } in
      let len = dist (a.x, a.y) (b.x, b.y) in
      { a; b; len; lw = 5 })

let create_arm ~length ~breaks base =
  let joints =
    generate_joints ~n:breaks
      ~step:(length /. float breaks)
      ~x_center:base.x ~y_center:base.y
  in
  { joints; base }

let create_window width height ~length ~breaks =
  open_graph (" " ^ string_of_int width ^ "x" ^ string_of_int height);
  let arms =
    let base = { x = float width /. 2.0; y = float height /. 2.0 } in
    List.init 1 (fun _ -> create_arm ~length ~breaks base)
  in
  event_loop { arms; cursor = { x = 0.0; y = 0.0 }; width; height }
