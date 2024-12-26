open Vec2
open Arm

type state = { arms : arm list; cursor : float vec2; width : int; height : int }

let update_cursor state =
  let mx, my = Graphics.mouse_pos () in
  { state with cursor = { x = float mx; y = float my } }

let update_state state =
  let updated_arms = List.map (update_arm ~cursor:state.cursor) state.arms in
  { state with arms = updated_arms }
