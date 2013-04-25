open Lwt

let with_rpc_conn f ~port ~host =
  let inet_addr = Unix.inet_addr_of_string host in
  let sockaddr = Unix.ADDR_INET (inet_addr, port) in
  let socket_fd = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
  Lwt_unix.connect socket_fd sockaddr
  >>= fun () -> f socket_fd
  >>= fun () -> return Lwt_unix.(shutdown socket_fd SHUTDOWN_SEND)
