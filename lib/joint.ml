open Vec2

type joint = { a : float vec2; b : float vec2; len : float; lw : int }

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
