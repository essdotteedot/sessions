module BP = Binary_session_lwt.Make

type coffee = Coffee
type tea = Tea

(* The vending machine :
   1) waits for change
   2) makes an internal choice
      - if the change is not enough then it stops
      - if there is enough change then it offers a choice
        between coffee or tea, upon receiving the decision
        from the client it dispenses the drink then stops
*)
let vending_machine () = BP.(
    lift_io (Lwt_io.printl "vending machine: waiting for funds") >>= fun () ->
    recv () >>= fun (amount : float) ->
    if amount < 2.0
    then lift_io (Lwt_io.printl "vending machine: received insufficient funds") >>= fun () -> choose_left (stop amount)
    else lift_io (Lwt_io.printl "vending machine: waiting for drink choice") >>= fun () -> choose_right (
        offer 
          (
            recv () >>= fun (c : coffee) -> 
            send "coffee" >>= fun () -> 
            lift_io (Lwt_io.printl "vending machine: dispensing coffee") >>= fun () -> 
            stop 0.0
          ) 
          (
            recv () >>= fun (t : tea) -> 
            send "tea" >>= fun () -> 
            lift_io (Lwt_io.printl "vending machine: dispensing tea") >>= fun () -> 
            stop 0.0
          ) 
      )
  )

(* The coffee client :
   1) puts in change
   2) if vending machine informs it that the funds are not enough it stops
   3) otherwise it chooses left (i.e., coffee) and receives the drink then stops
*)
let vending_client_coffee (amount : float) = BP.(
    lift_io (Lwt_io.printlf "vending machine user : putting in %f into vending machine" amount) >>= fun () ->
    send amount >>= fun () ->
    offer 
      (stop ()) 
      (choose_left (
          send Coffee >>= fun () -> 
          lift_io (Lwt_io.printl "vending machine user : picked coffee") >>= fun () ->                             
          recv () >>= fun (d : string) -> 
          lift_io (Lwt_io.printlf "vending machine user : received %s" d) >>= fun () -> 
          stop ()
        )
      )
  )

(* The tea client :
   1) puts in change
   2) if vending machine informs it that the funds are not enough it stops
   3) otherwise it chooses right (i.e., tea) and receives the drink then stops
*)
let vending_client_tea (amount : float) = BP.(
    lift_io (Lwt_io.printlf "vending machine user : putting in %f into vending machine" amount) >>= fun () ->
    send amount >>= fun () ->
    offer 
      (stop ()) 
      (
        choose_right (
          send Tea >>= fun () -> 
          lift_io (Lwt_io.printl "vending machine user : picked tea") >>= fun () ->         
          recv () >>= fun (d : string) -> 
          lift_io (Lwt_io.printlf "vending machine user : received %s" d) >>= fun () ->  
          stop ()
        )
      )
  )

(* Print server
   offer a choice between
   - stopping
   - receiving text to be printed then looping
*)
let rec print_server () = BP.(
    offer 
      (stop ())
      (
        recv () >>= fun (s : string) ->
        lift_io (Lwt_io.printlf "print server : %s" s) >>=
        print_server
      )
  )  

(* Print client
   makes an internal choice between
   - stopping
   - receiving text to be printed then looping
*)
let rec print_client (i : int) = BP.(
  lift_io (Lwt_io.read_line Lwt_io.stdin) >>= fun (s : string) ->
  if s = "q"
  then choose_right (send (Printf.sprintf "Total lines printed : %d" (i+1)) >>= fun () -> choose_left (stop ()))
  else choose_right (send s >>= fun () -> print_client (i+1))
)  

let () = Lwt_main.run (
    Lwt.(
      BP.run_processes (vending_machine ()) (vending_client_coffee 1.0) >>= fun (vm_fn,client_fn) ->
      async vm_fn ;
      client_fn () >>= fun _ ->

      Lwt_io.printl "" >>= fun () ->

      BP.run_processes (vending_client_coffee 3.0) (vending_machine ()) >>= fun (vm_fn,client_fn) ->
      async client_fn ;
      vm_fn () >>= fun _ ->

      Lwt_io.printl "" >>= fun () ->

      BP.run_processes (vending_machine ()) (vending_client_tea 1.0)  >>= fun (vm_fn,client_fn) ->
      async vm_fn ;
      client_fn () >>= fun _ ->

      Lwt_io.printl "" >>= fun () ->

      BP.run_processes (vending_machine ()) (vending_client_tea 3.0)  >>= fun (vm_fn,client_fn) ->
      async vm_fn ;
      client_fn () >>= fun _ ->

      Lwt_io.printl "" >>= fun () ->

      BP.run_processes (print_server ()) (print_client 1)  >>= fun (vm_fn,client_fn) ->
      async vm_fn ;
      client_fn () >>= fun _ ->


      return ()
    )
  )