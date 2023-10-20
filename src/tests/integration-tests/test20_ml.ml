module T = Test20

let decode_pb_ref_data () = { T.f1 = 1l; f2 = { T.sub_f1 = 2l }; f3 = T.Eone }

let () =
  Printf.printf "Show is working: %s\n" @@ T.show_m (decode_pb_ref_data ())

let () =
  let mode = Test_util.parse_args () in

  match mode with
  | Test_util.Decode ->
    Test_util.decode "test20.c2ml.data" T.decode_pb_m T.pp_m
      (decode_pb_ref_data ())
  | Test_util.Encode ->
    Test_util.encode "test20.ml2c.data" T.encode_pb_m (decode_pb_ref_data ())
