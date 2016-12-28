module Nonblock_io = struct
  type 'a t = 'a Lwt.t

  type chan_endpoint = Lwt_io.input_channel * Lwt_io.output_channel

  type chan = Chan : chan_endpoint * chan_endpoint -> chan

  let make_channel () : chan =
    let (in_ch,out_ch) = Lwt_io.pipe () in
    Chan ((in_ch,out_ch),(in_ch,out_ch))

  let read_channel ((in_ch,_) : chan_endpoint) : 'a t =
    Lwt_io.read_value in_ch

  let write_channel (v : 'a) ~(flags:Marshal.extern_flags list) ((_,out_ch) : chan_endpoint) : unit t =
    Lwt_io.write_value out_ch ~flags v

  let close_channel (Chan ((ch_in,ch_out),_) : chan) : unit t =
    Lwt.(Lwt_io.close ch_in >>= fun () -> Lwt_io.close ch_out)

  let return = Lwt.return

  let (>>=) = Lwt.(>>=)    
end

module Make : (Binary_session.Binary_process with type 'a io = 'a Lwt.t and type chan_endpoint = (Lwt_io.input_channel * Lwt_io.output_channel)) =
  Binary_session.Make (Nonblock_io)