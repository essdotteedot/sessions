# sessions [![Build Status](https://travis-ci.org/essdotteedot/sessions.svg?branch=master)](https://travis-ci.org/essdotteedot/sessions)
Library to provide session types to allow for static verification of protocols between concurrent computations.

Provides sessions types (currently binary session type) for statically verifying protocols between concurrent computations.
This library is based on the paper "Haskell Session Types with (Almost) No Class".

A session type `('a,'b) session` represents a protocol that a particular process carries out. Here `'a` and `'b` are duals of each other. 

A process `('a,'b,'c) process` is parameterized by a starting session type `'b`, `'a` is it's return value and `'c` is it's final
session type. Two processes can be run only if they have dual initial session types a final session type of `unit`.

The following operations are duals of each other :
- `Stop, Stop`
- `Send of 'a * 'b, Recv of 'a * 'b`, where `'b` is a session type
- `Offer of ('a, 'b) session * ('c, 'd) session, Choice of ('b, 'a) session * ('d, 'c) session`, where `'a`, `'b`, `'c`, `'d` are session types   

Here are some examples of processes which are duals :

```Ocaml
module BP = Binary_session_lwt.Make           

let send_str_recv_int_stop = BP.(send "hello" >>= fun () -> recv () >>= fun (i : int) -> stop ())      
let recv_str_send_int_stop = BP.(recv () >>= fun (s : string) -> send 1 >>= fun () -> stop ())        

let _ = BP.run_processes send_str_recv_int_stop recv_str_send_int_stop
```

Note that the session type associated with the process `send_str_recv_int_stop` was inferred as 

```Ocaml
([ `Send of string * [ `Recv of int * [ `Stop ] ] ],[ `Recv of string * [ `Send of int * [ `Stop ] ] ]) BP.session
```

as you can see it provides it's own session type 

```Ocaml
[ `Send of string * [ `Recv of int * [ `Stop ] ] ]]
``` 
as well as it's dual 

```Ocaml
[ `Recv of string * [ `Send of int * [ `Stop ] ] ]
``` 

The session type associated with the process `recv_str_send_int_stop` is 

```Ocaml
([ `Recv of string * [ `Send of int * [ `Stop ] ] ], [ `Send of string * [ `Recv of int * [ `Stop ] ] ]) BP.session
```
we see that it indeed has the dual of `send_str_recv_int_stop` which means that `BP.run_processes send_str_recv_int_stop recv_str_send_int_stop` will type check.

If these two processes were to differ in such a way that they were not duals then `BP.run_processes send_str_recv_int_stop recv_str_send_int_stop` would not type check.

Here is another example using offer and choice as well as recursion. 

```Ocaml
module BP = Binary_session_lwt.Make

let rec print_server () = BP.(
  offer 
    (stop ())
    (
      recv () >>= fun (s : string) ->
      lift_io (Lwt_io.printlf "print server : %s" s) >>=
      print_server
    )
)  

let rec print_client (i : int) = BP.(
  lift_io (Lwt_io.read_line Lwt_io.stdin) >>= fun (s : string) ->
  if s = "q"
  then choose_right (send (Printf.sprintf "Total lines printed : %d" (i+1)) >>= fun () -> choose_left (stop ()))
  else choose_right (send s >>= fun () -> print_client (i+1))
) 

let _ = BP.run_processes print_server (print_client 0)           
```    
Documentation
-------------

The API documentation is available [here](https://essdotteedot.github.io/sessions/).
Example programs can be found in the [examples] (examples) directory.

License
-------

[MIT License](LICENSE)
