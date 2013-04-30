
(** The protocol for communicating between the hello client and server.
    There's a single RPC call exposed, which lets you send and receive a
    string.

    The [bin_query] and [bin_response] arguments are values that contain logic
    for binary serialization of the query and response types, in this case,
    both strings.

    The version number is used when you want to mint new versions of an RPC
    without disturbing older versions.
*)

let hello_rpc = Lwt_rpc.Protocol.create
  ~name:"hello-world"
  ~version:0
  ~bin_query:Bin_prot.Type_class.bin_string
  ~bin_response:Bin_prot.Type_class.bin_string

