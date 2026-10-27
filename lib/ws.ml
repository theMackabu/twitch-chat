(* WebSocket client over TLS, built on httpun-ws (framing, masking, handshake)
   and gluten-eio (I/O loop). httpun-ws-eio's own client only accepts a plain
   unix socket, so we drive gluten-eio directly with a TLS flow. *)

open Eio.Std
module Wsd = Httpun_ws.Wsd

type event = [ `Text of string | `Closed of string ]
type t = { wsd : Wsd.t; events : event Eio.Stream.t }

(* gluten-eio's signature asks for a stream socket but only uses generic flow
   operations, so expose the TLS flow as one. *)
module Tls_socket = struct
  type t = Tls_eio.t
  type tag = [ `Generic ]

  let single_read t buf = Eio.Flow.single_read t buf
  let read_methods = []
  let single_write t bufs = Eio.Flow.single_write t bufs
  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src
  let shutdown t cmd = Eio.Flow.shutdown t cmd
  let close t = Eio.Resource.close t
  let setsockopt _ _ _ = ()
  let getsockopt _ _ = invalid_arg "getsockopt on TLS flow"
end

let socket_handler = Eio.Net.Pi.stream_socket (module Tls_socket)

let sha1 s = Digestif.SHA1.(to_raw_string (digest_string s))

(* Connects, performs the upgrade handshake and returns once it succeeded.
   The connection lives in [sw]. *)
let connect ~sw ~net ~tls ~nonce ~host ~port ~resource =
  let addr =
    match Eio.Net.getaddrinfo_stream ~service:(string_of_int port) net host with
    | a :: _ -> a
    | [] -> failwith ("cannot resolve " ^ host)
  in
  let tcp = Eio.Net.connect ~sw net addr in
  let tls_flow =
    Tls_eio.client_of_flow tls
      ~host:(Domain_name.host_exn (Domain_name.of_string_exn host))
      tcp
  in
  let socket = Eio.Resource.T (tls_flow, socket_handler) in
  let events = Eio.Stream.create max_int in
  let closed reason = Eio.Stream.add events (`Closed reason) in
  let opened, set_opened = Promise.create () in
  let message = Buffer.create 4096 in
  let websocket_handler wsd =
    Promise.resolve set_opened (Ok wsd);
    let handle ~opcode ~is_fin data =
      match opcode with
      | `Text | `Binary | `Continuation ->
          if opcode <> `Continuation then Buffer.clear message;
          Buffer.add_string message data;
          if is_fin then Eio.Stream.add events (`Text (Buffer.contents message))
      | `Ping ->
          let buffer = Bigstringaf.of_string data ~off:0 ~len:(String.length data) in
          Wsd.send_pong ~application_data:{ buffer; off = 0; len = String.length data } wsd
      | `Connection_close ->
          if not (Wsd.is_closed wsd) then Wsd.close wsd;
          closed "server sent close frame"
      | `Pong | `Other _ -> ()
    in
    let frame ~opcode ~is_fin ~len:_ payload =
      let data = Buffer.create 256 in
      let rec read () =
        Httpun_ws.Payload.schedule_read payload
          ~on_eof:(fun () -> handle ~opcode ~is_fin (Buffer.contents data))
          ~on_read:(fun bs ~off ~len ->
            Buffer.add_string data (Bigstringaf.substring bs ~off ~len);
            read ())
      in
      read ()
    in
    let eof ?error () =
      match error with
      | Some (`Exn e) -> closed ("websocket error: " ^ Printexc.to_string e)
      | None -> closed "connection closed"
    in
    { Httpun_ws.Websocket_connection.frame; eof }
  in
  let error_handler err =
    let reason =
      match err with
      | `Handshake_failure (resp, _) ->
          Format.asprintf "handshake failed: %a" Httpun.Response.pp_hum resp
      | `Malformed_response m -> "malformed response: " ^ m
      | `Invalid_response_body_length _ -> "invalid response body length"
      | `Exn e -> Printexc.to_string e
    in
    ignore (Promise.try_resolve set_opened (Error reason) : bool);
    closed reason
  in
  let conn =
    Httpun_ws.Client_connection.connect ~nonce
      ~headers:(Httpun.Headers.of_list [ ("host", host) ])
      ~sha1 ~error_handler ~websocket_handler resource
  in
  let _client : Gluten_eio.Client.t =
    Gluten_eio.Client.create ~sw ~read_buffer_size:0x4000
      ~protocol:(module Httpun_ws.Client_connection)
      conn socket
  in
  match Promise.await opened with
  | Ok wsd -> { wsd; events }
  | Error reason -> failwith reason

let send t text =
  let b = Bytes.of_string text in
  Wsd.send_bytes t.wsd ~kind:`Text b ~off:0 ~len:(Bytes.length b)

let recv t = Eio.Stream.take t.events
