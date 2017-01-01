(** This module provides modules to create binary sessions types for statically verifying protocols between 
    a pair of concurrent processes. 

    Binary processes which are parametrized by binary session types can be created using {!modtype:Binary_process}. 
    A pair of processes can only be run if they have compatible (dual) session types.     

    @author essdotteedot [<essdotteedot[at]gmail[dot]com>]
    @version 0.1.0
*)

(** Abstract type which can perform monadic concurrent IO. *)
module type IO = sig

  type 'a t
  (** The monadic light weight thread type returning value ['a]. *) 

  type chan_endpoint
  (** The abstract type representing one end of a communication channel. *)

  type chan = Chan : chan_endpoint * chan_endpoint -> chan (** A channel consists of two {!type:Binary_session.IO.chan_endpoint}. *)
  (** The abstract type representing a communication channel between two processes. *)

  val make_channel : unit -> chan
  (** [make_channel ()] will return a new communication channel {!type:Binary_session.IO.chan}. *)

  val read_channel : chan_endpoint -> 'a t
  (** [read_channel end_point] reads a marshalled value from [end_point] and returns it. *)

  val write_channel : 'a -> flags:Marshal.extern_flags list -> chan_endpoint -> unit t
  (** [write_channel v flags end_point] marshals the value [v] according to [flags] and writes it to [end_point]. *)

  val close_channel : chan -> unit t
  (** [close_channel c] will close the given channel [c]. *)	

  val return : 'a -> 'a t
  (** [return v] creates a light weight thread returning [v]. *)

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t	
  (** [bind t f] is a thread which first waits for the thread [t] to terminate and then, behaves as the application of 
      function [f] to the return value of [t]. 
  *) 

end

(** A process which is parametrized by a binary session type. *)
module type Binary_process = sig

  type 'a io
  (** The abstract monadic type representing a computation returning ['a]. *)

  type chan_endpoint
  (** The abstract type representing one end of a communication channel. *)

  type ('a,'b) session constraint 'a = [>] constraint 'b = [>]
  (** The type representing a communication protocol made up of a sequence of operations between two processes. 
      The type ['a] is the sequence of operations from the point of view from the first process and ['b] 
      its dual is the sequence of operations from the point of view of the second process.
  *)

  type ('a,'b,'c) process
  (** The type representing a process returning a value of type ['a]. The type ['b] represents the next allowed
      sequnce of operations and ['c] represents the sequence of operations after performing the first operation
      in ['b]. 
  *)

  val send : 'a -> (unit,([`Send of 'a * 'b], [`Recv of 'a * 'c]) session, ('b,'c) session) process
  (** [send v] creates a process which is capable of sending a value of type ['a] ([v]) to the other process. *)

  val recv : unit -> ('a,([`Recv of 'a * 'b], [`Send of 'a * 'c]) session, ('b,'c) session) process
  (** [recv ()] creates a process which is capable of receiving a value of type ['a] to the other process. *)

  val offer : ('e,('a, 'b) session,'f) process -> ('e,('c, 'd) session,'f) process -> 
    ('e,([`Offer of (('a, 'b) session * ('c, 'd) session)], [`Choice of (('b, 'a) session * ('d, 'c) session)]) session,'f) process 
  (** [offer left_choice right_choice] creates a process which allows the other process to make a choice between
      two choices [left_choice] and [right_choice].
  *)

  val choose_left : ('e,('a, 'b) session,'f) process ->
    ('e,([`Choice of (('a, 'b) session * ('c, 'd) session)], [`Offer of (('b, 'a) session * ('d, 'c) session)]) session,'f) process
  (** [choose_left left_choice] creates a process which internally chooses [left_choice] and communicates this choice
      to the other process.
  *)

  val choose_right : ('e,('c, 'd) session,'f) process ->
    ('e,([`Choice of (('a, 'b) session * ('c, 'd) session)], [`Offer of (('b, 'a) session * ('d, 'c) session)]) session,'f) process
  (** [choose_right right_choice] creates a process which internally chooses [rigth_choice] and communicates this choice
      to the other process.
  *)

  val stop : 'a -> ('a,([`Stop], [`Stop]) session, unit) process
  (** [stop v] creates a process which stops (is not capable of performing any further operations) and returns a 
      value v.
  *)

  val lift_io : 'a io -> ('a, 'b, 'b) process
  (** [lift_io io] lifts the [io] computation into the process. The processes' capabilities are not altered. *)

  val return : 'a -> ('a,'b,'b) process
  (** [return v] creates a process which returns [v] its capabilities are not altered. *)

  val (>>=) : ('a,'b,'c) process -> ('a -> ('d,'c,'e) process) -> ('d,'b,'e) process
  (** [p1 >>= f] creates a process which is the composition of running [p1] then applying. *)  

  val run_processes : ('a, ('b,'c) session, unit) process -> ('d, ('c,'b) session, unit) process -> ((unit -> 'a io) * (unit -> 'd io)) io
  (** [run_process p1 p2] will run two processes [p1] and [p2] which have dual session types and which have
      [unit] as their end state capabilities (i.e., are complete processes). The result is a 
      {!type:Binary_session.IO.t} returning a pair of functions which may be invoked to run each process.

      Note, the channel that is opened between the two processes is closed when the processes have completed.
  *)

end

module Make (I : IO) : (Binary_process with type 'a io = 'a I.t and type chan_endpoint = I.chan_endpoint)  
(** Functor to create a module of type {!modtype:Binary_process} given a message module [I] of type {!modtype:IO}. *)