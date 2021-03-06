module type IO = sig

  type 'a t 

  type chan_endpoint

  type chan = Chan : chan_endpoint * chan_endpoint -> chan

  val make_channel : unit -> chan

  val read_channel : chan_endpoint -> 'a t

  val write_channel : 'a -> flags:Marshal.extern_flags list -> chan_endpoint -> unit t

  val close_channel : chan -> unit t	

  val return : 'a -> 'a t

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t	  

end

module type Binary_process = sig

  type 'a io
  
  type chan_endpoint
  
  type ('a,'b) session constraint 'a = [>] constraint 'b = [>]
  
  type ('a,'b,'c) process
  
  val send : 'a -> (unit,([`Send of 'a * 'b], [`Recv of 'a * 'c]) session, ('b,'c) session) process
  
  val recv : unit -> ('a,([`Recv of 'a * 'b], [`Send of 'a * 'c]) session, ('b,'c) session) process
  
  val offer : ('e,('a, 'b) session,'f) process -> ('e,('c, 'd) session,'f) process -> 
    ('e,([`Offer of (('a, 'b) session * ('c, 'd) session)], [`Choice of (('b, 'a) session * ('d, 'c) session)]) session,'f) process 
  
  val choose_left : ('e,('a, 'b) session,'f) process ->
    ('e,([`Choice of (('a, 'b) session * ('c, 'd) session)], [`Offer of (('b, 'a) session * ('d, 'c) session)]) session,'f) process
  
  val choose_right : ('e,('c, 'd) session,'f) process ->
    ('e,([`Choice of (('a, 'b) session * ('c, 'd) session)], [`Offer of (('b, 'a) session * ('d, 'c) session)]) session,'f) process
  
  val stop : 'a -> ('a,([`Stop], [`Stop]) session, unit) process
  
  val lift_io : 'a io -> ('a, 'b, 'b) process
  
  val return : 'a -> ('a,'b,'b) process
  
  val (>>=) : ('a,'b,'c) process -> ('a -> ('d,'c,'e) process) -> ('d,'b,'e) process
  
  val run_processes : ('a, ('b,'c) session, unit) process -> ('d, ('c,'b) session, unit) process -> ((unit -> 'a io) * (unit -> 'd io)) io

end

module Make (I : IO) : (Binary_process with type 'a io = 'a I.t and type chan_endpoint = I.chan_endpoint) = struct
  type 'a io = 'a I.t

  type chan_endpoint = I.chan_endpoint  

  type ('a,'b) session constraint 'a = [>] constraint 'b = [>]

  type ('a,'b,'c) process = P : (chan_endpoint -> ('a * chan_endpoint) io) -> ('a,'b,'c) process

  type which_choice = Left_choice | Right_choice

  let send (i : 'a) : (unit,([`Send of 'a * 'b], [`Recv of 'a * 'c]) session, ('b,'c) session) process =
    P (fun ch -> I.(write_channel i ~flags:[Marshal.Closures] ch >>= fun () -> return ((),ch)))

  let recv () : ('a,([`Recv of 'a * 'b], [`Send of 'a * 'c]) session, ('b,'c) session) process = 
    P (fun ch -> I.(read_channel ch >>= fun (v : 'a) -> return (v,ch)))

  let offer (left : ('e,('a, 'b) session,'f) process) (right : ('e,('c, 'd) session,'f) process) :
    ('e,([`Offer of (('a, 'b) session * ('c, 'd) session)], [`Choice of (('b, 'a) session * ('d, 'c) session)]) session,'f) process = 
    P (fun ch -> I.(
        read_channel ch >>= function        
          Left_choice -> let P left' = left in left' ch 
        | Right_choice -> let P right' = right in right' ch
      ))

  let choose_left (left : ('e,('a, 'b) session,'f) process) : 
    ('e,([`Choice of (('a, 'b) session * ('c, 'd) session)], [`Offer of (('b, 'a) session * ('d, 'c) session)]) session,'f) process =
    P (fun ch -> I.(write_channel ~flags:[] Left_choice ch >>= fun () -> let P left' = left in left' ch))

  let choose_right (right : ('e,('c, 'd) session,'f) process) :
    ('e,([`Choice of (('a, 'b) session * ('c, 'd) session)], [`Offer of (('b, 'a) session * ('d, 'c) session)]) session,'f) process =
    P (fun ch -> I.(write_channel ~flags:[] Right_choice ch >>= fun () -> let P right' = right in right' ch))

  let stop (v : 'a) : ('a,([`Stop], [`Stop]) session, unit) process = 
    P (fun ch -> I.return (v,ch))

  let lift_io (v : 'a io) : ('a, 'b, 'b) process =
    P (fun ch -> I.(v >>= fun v' -> return (v',ch)))

  let return (v : 'a) : ('a,'b,'b) process =
    P (fun ch -> I.return (v,ch))

  let (>>=) (p : ('a,'b,'c) process) (f : ('a -> ('d,'c,'e) process)) : ('d,'b,'e) process =
    P (fun ch -> I.(let P p' = p in p' ch >>= fun (v,ch') -> let P p'' = f v in p'' ch' >>= fun (v'',ch'') -> return (v'',ch'')))  

  let run_processes (p : ('a, ('b,'c) session, unit) process) (p1 : ('d, ('c,'b) session, unit) process) : ((unit -> 'a io) * (unit -> 'd io)) io =
    I.(
      let P p' = p in
      let P p1' = p1 in
      let Chan (ep1,ep2) = I.make_channel () in
      let r1 = fun () -> (p' ep1) >>= fun (v,_) -> return v in
      let r2 = fun () -> (p1' ep2) >>= fun (v,_) -> return v in
      return (r1, r2)      
    )
end