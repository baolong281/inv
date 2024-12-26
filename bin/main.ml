let rec prompt_float prompt =
  print_string prompt;
  flush stdout;
  try read_float ()
  with Failure _ ->
    print_endline "Invalid input. Please enter a valid float.";
    prompt_float prompt

let rec prompt_int prompt =
  print_string prompt;
  flush stdout;
  try read_int ()
  with Failure _ ->
    print_endline "Invalid input. Please enter a valid integer.";
    prompt_int prompt

let () =
  let length = prompt_float "Enter segment length: " in
  let breaks = prompt_int "Enter number of breaks: " in
  let width, height = (500, 500) in
  Inv.Drawing.create_window width height ~length ~breaks
