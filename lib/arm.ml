open Vec2
open Joint

type arm = { joints : joint list; base : float vec2 }

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

let create_arm ~length ~breaks base =
  let joints =
    generate_joints ~n:breaks
      ~step:(length /. float breaks)
      ~x_center:base.x ~y_center:base.y
  in
  { joints; base }
