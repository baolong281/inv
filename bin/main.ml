let () =
  let length =
    print_string "Enter segment length:";
    flush stdout;
    read_float ()
  in
  let breaks =
    print_string "Enter number of breaks length:";
    flush stdout;
    read_int ()
  in
  Inv.Arm.create_window 500 500 ~length ~breaks
