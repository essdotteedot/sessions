(** A lwt based implementation of {!module:Binary_session.Binary_process}
    that uses a pipe for the communication channel between two processes.

    @author essdotteedot [<essdotteedot[at]gmail[dot]com>]
    @version 0.1.0
 *)

module Make : (Binary_session.Binary_process with type 'a io = 'a Lwt.t and type chan_endpoint = (Lwt_io.input_channel * Lwt_io.output_channel))
 (** Functor to create a module of type {!module:Binary_session.Binary_process}. *)