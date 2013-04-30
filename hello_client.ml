open Printf

(* An lwt that sends the hello request *)
let say_hello = Lwt_rpc.(
  Client.with_rpc_conn (fun conn -> Lwt.(
    Client.dispatch Hello_protocol.hello_rpc conn "Hello from LWT,"
    >|= fun response -> printf "%s\n%!" response
  )))

let () = Lwt_main.run (say_hello ~port:8124 ~host:"127.0.0.1")
