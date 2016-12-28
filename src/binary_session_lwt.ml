module Nonblock_io = struct
  type 'a t = 'a Lwt.t

  type chan_endpoint = Lwt_io.input_channel * Lwt_io.output_channel

  type chan = Chan : chan_endpoint * chan_endpoint -> chan

  let make_channel () : chan =
    let (in_ch,out_ch) = Lwt_io.pipe () in
    let (in_ch',out_ch') = Lwt_io.pipe () in
    Chan ((in_ch',out_ch),(in_ch,out_ch'))

  let read_channel ((in_ch,_) : chan_endpoint) : 'a t =
    Lwt_io.read_value in_ch

  let write_channel (v : 'a) ~(flags:Marshal.extern_flags list) ((_,out_ch) : chan_endpoint) : unit t =
    Lwt_io.write_value out_ch ~flags v

  let close_channel (Chan ((ch_in0,ch_out0),(ch_in1,ch_out1)) : chan) : unit t =
    Lwt.(Lwt_io.close ch_in0 >>= fun () -> 
         Lwt_io.close ch_out0 >>= fun () ->
         Lwt_io.close ch_in1 >>= fun () ->
         Lwt_io.close ch_out1
        )

  let return = Lwt.return

  let (>>=) = Lwt.(>>=)    
end

module Make : (Binary_session.Binary_process with type 'a io = 'a Nonblock_io.t and type chan_endpoint = Nonblock_io.chan_endpoint and type chan = Nonblock_io.chan) =
  Binary_session.Make (Nonblock_io)