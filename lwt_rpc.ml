open Bin_prot.Size
open Bin_prot.Std

module Lwt_bytes = struct
  include Lwt_bytes
    
  let bin_size_t = length
  let bin_write_t_ = Bin_prot.Unsafe_write_c.bin_write_bigstring
  let bin_read_t_ = Bin_prot.Unsafe_read_c.bin_read_bigstring
end

type ('q, 'r) rpc = {
  name         : string;
  version      : int;
  bin_query    : 'q Bin_prot.Type_class.t;
  bin_response : 'r Bin_prot.Type_class.t;
}

module Result = struct
  type ('ok, 'error) t =
    | Ok of 'ok
    | Error of 'error
  with bin_io
end

module Sexp = struct
  type t =
    | Atom of string
    | List of t list
  with bin_io
end

module Rpc_error = struct
  type t =
    | Bin_io_exn of Sexp.t
    | Connection_closed
    | Write_error of Sexp.t
    | Uncaught_exn of Sexp.t
    | Unimplemented_rpc of string * [`Version of int]
    | Unknown_query_id of int64
  with bin_io
end

module Rpc_result = struct
  type 'a t = ('a, Rpc_error.t) Result.t with bin_io
end

module Query = struct
  type t = {
    tag     : string;
    version : int;
    id      : int64;
    data    : Lwt_bytes.t;
  } with bin_io
end

module Response = struct
  type t = {
    id   : int64;
    data : Lwt_bytes.t Rpc_result.t;
  } with bin_io
end

module Message = struct
  type t =
    | Heartbeat
    | Query of Query.t
    | Response of Response.t
  with bin_io
end

(*open Bin_prot.Type_class*)
(*open Lwt*)

module Protocol = struct
  open Bin_prot.Type_class

  let len_len = 8
  let last_id = ref Int64.zero
  let gen_id () = last_id := Int64.(add !last_id one); !last_id

  let write_bin_prot writer v =
    let len = writer.size v in
    Printf.eprintf "v len: %d\n%!" len;
    let tot_len = len + len_len in
    let buf = Lwt_bytes.create tot_len in
    Printf.eprintf "buf length: %d\n%!" (Lwt_bytes.length buf);
    let pos_len = Bin_prot.Write_ml.bin_write_int_64bit buf ~pos:0 len in
    Printf.eprintf "pos_len: %d\n%!" pos_len;
    let pos = writer.write buf ~pos:pos_len v in
    if pos <> tot_len then Lwt.fail (Failure "write_bin_prot")
    else Lwt.return buf

  let create ~name ~version ~bin_query ~bin_response = {
    name; version; bin_query; bin_response;
  }
end

module Client = struct
  open Bin_prot.Type_class

  let dispatch rpc socket_fd mesg = Lwt.(
    let data = Bin_prot.Utils.bin_dump rpc.bin_query.writer mesg in
    let query = {Query.tag=rpc.name; version=rpc.version;
                 id=Protocol.gen_id (); data} in
    Protocol.write_bin_prot Message.bin_writer_t (Message.Query query)
    >>= fun buf ->
    Lwt_bytes.send socket_fd buf 0 (Lwt_bytes.length buf) []
    >>= fun _ ->
    let bufsz = 1024 in
    Lwt_bytes.recv socket_fd buf 0 bufsz []
    >>= fun _ ->
    return (rpc.bin_response.reader.read buf ~pos_ref:(ref 0))
  )

  let with_rpc_conn f ~port ~host = Lwt.(
    let inet_addr = Unix.inet_addr_of_string host in
    let sockaddr = Unix.ADDR_INET (inet_addr, port) in
    let socket_fd = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
    Lwt_unix.connect socket_fd sockaddr
    >>= fun () -> f socket_fd
    >>= fun () -> return Lwt_unix.(shutdown socket_fd SHUTDOWN_SEND)
  )
end

module Server = struct
  let close (ic,oc) = Lwt.(
    catch (fun () -> Lwt_io.close oc) (fun _ -> return ())
    >>= fun () -> catch (fun () -> Lwt_io.close ic) (fun _ -> return ())
  )

  let init_socket sockaddr =
    let suck = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
    Lwt_unix.setsockopt suck Unix.SO_REUSEADDR true;
    Lwt_unix.bind suck sockaddr;
    Lwt_unix.listen suck 15;
    suck

  let process_accept ~timeout callback (client,_) =
    let ic = Lwt_io.(of_fd ~mode:input client) in
    let oc = Lwt_io.(of_fd ~mode:output client) in

    let c = callback ic oc in
    let events = match timeout with
      |None -> [c]
      |Some t -> [c; (Lwt_unix.sleep (float_of_int t)) ] in
    Lwt.(
      let _ = pick events >>= fun () -> close (ic,oc) in
      return ()
    )

  let init ~sockaddr ~timeout callback =
    let s = init_socket sockaddr in
    let rec loop () = Lwt.(
      Lwt_unix.accept s
      >>= process_accept ~timeout callback
      >>= loop
    ) in loop ()
end
