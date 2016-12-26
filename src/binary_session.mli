module type IO = sig

  type 'a t 

  type chan_endpoint

  type chan = Chan : chan_endpoint * chan_endpoint -> chan

  val make_channel : unit -> chan

  val read_channel : chan_endpoint -> 'a t

  val write_channel : 'a -> chan_endpoint -> unit t	

  val return : 'a -> 'a t

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t	

end

module type Binary_process = sig

  type 'a io

  type chan_endpoint

  type chan

  type stop

  type 'a send

  type 'a recv

  type ('a,'b) choice

  type ('a,'b) offer

  type ('a,'b) session

  type ('a,'b,'c) process

  val send : 'a -> (unit, ('a send * 'b, 'a recv * 'c) session, ('b, 'c) session) process

  val recv : unit -> ('a, ('a recv * 'b, 'a send * 'c) session, ('b, 'c) session) process

  val offer : ('e,('a, 'b) session,unit) process -> ('e,('c, 'd) session,unit) process -> 
    ('e,((('a, 'b) session, ('c, 'd) session) offer, (('b, 'a) session,('d, 'c) session) choice) session,unit) process 

  val choose_left : ('e,('a, 'b) session,unit) process ->
    ('e,((('a, 'b) session, ('c, 'd) session) choice, (('b, 'a) session,('d, 'c) session) offer) session,unit) process

  val choose_right : ('e,('c, 'd) session,unit) process ->
    ('e,((('a, 'b) session, ('c, 'd) session) choice, (('b, 'a) session,('d, 'c) session) offer) session,unit) process

  val stop : 'a -> ('a, (stop,stop) session, unit) process

  val lift_io : 'a io -> ('a, 'b, 'b) process

  val return : 'a -> ('a,'b,'b) process

  val (>>=) : ('a,'b,'c) process -> ('a -> ('d,'c,'e) process) -> ('d,'b,'e) process

  val run_process : ('a, ('b,'c) session, unit) process -> unit

  val run_processes : ('a, ('b,'c) session, unit) process -> ('d, ('c,'b) session, unit) process -> unit

end

module Make (I : IO) : (Binary_process with type 'a io = 'a I.t and type chan_endpoint = I.chan_endpoint and type chan = I.chan)  