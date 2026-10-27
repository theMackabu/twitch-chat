open Twitch_chat

let host = "irc-ws.chat.twitch.tv"

exception Disconnected of string

let random_bytes env n =
  let buf = Cstruct.create n in
  Eio.Flow.read_exact env#secure_random buf;
  Cstruct.to_string buf

let random_nick env =
  let digits =
    String.map (fun c -> Char.chr (48 + (Char.code c mod 10))) (random_bytes env 8)
  in
  "justinfan" ^ digits

let timestamp clock =
  let tm = Unix.localtime (Eio.Time.now clock) in
  Printf.sprintf "%02d:%02d:%02d" tm.tm_hour tm.tm_min tm.tm_sec

(* Runs one connection until it closes; always ends by raising. *)
let session env ~tls ~channel ~on_connected =
  let log fmt = Printf.ksprintf (fun s -> Eio.Flow.copy_string (s ^ "\n") env#stderr) fmt in
  Eio.Switch.run @@ fun sw ->
  let ws =
    Ws.connect ~sw ~net:env#net ~tls ~nonce:(random_bytes env 16) ~host ~port:443
      ~resource:"/"
  in
  let send line = Ws.send ws (line ^ "\r\n") in
  let nick = random_nick env in
  send "CAP REQ :twitch.tv/tags twitch.tv/commands";
  send ("NICK " ^ nick);
  send ("JOIN #" ^ channel);
  log "connected to %s as %s, joining #%s" host nick channel;
  on_connected ();
  let handle_line line =
    match Irc.parse line with
    | None -> ()
    | Some ({ command = "PING"; _ } as m) ->
        send ("PONG :" ^ (match m.params with p :: _ -> p | [] -> "tmi.twitch.tv"))
    | Some ({ command = "PRIVMSG"; params = [ _; text ]; _ } as m) ->
        let name =
          match List.assoc_opt "display-name" m.tags with
          | Some n when n <> "" -> n
          | _ -> Option.value (Irc.nick m) ~default:"?"
        in
        Eio.Flow.copy_string
          (Printf.sprintf "[%s] %s: %s\n" (timestamp env#clock) name text)
          env#stdout
    | Some { command = "RECONNECT"; _ } -> raise (Disconnected "server requested reconnect")
    | Some { command = "NOTICE"; params; _ } ->
        log "notice: %s" (String.concat " " params)
    | Some _ -> ()
  in
  let rec loop () =
    match Ws.recv ws with
    | `Closed reason -> raise (Disconnected reason)
    | `Text frame ->
        String.split_on_char '\n' frame
        |> List.iter (fun l -> if String.trim l <> "" then handle_line l);
        loop ()
  in
  loop ()

let () =
  let channel =
    match Sys.argv with
    | [| _; c |] when c <> "" ->
        let c = String.lowercase_ascii c in
        if c.[0] = '#' then String.sub c 1 (String.length c - 1) else c
    | _ ->
        prerr_endline "usage: twitch_chat <channel>";
        exit 2
  in
  Mirage_crypto_rng_unix.use_default ();
  Eio_main.run @@ fun env ->
  let log s = Eio.Flow.copy_string (s ^ "\n") env#stderr in
  let tls =
    match Ca_certs.authenticator () with
    | Error (`Msg m) -> failwith ("ca-certs: " ^ m)
    | Ok authenticator -> (
        match Tls.Config.client ~authenticator () with
        | Ok c -> c
        | Error (`Msg m) -> failwith ("tls config: " ^ m))
  in
  let delay = ref 1.0 in
  while true do
    (try session env ~tls ~channel ~on_connected:(fun () -> delay := 1.0) with
    | Eio.Cancel.Cancelled _ as e -> raise e
    | Disconnected reason -> log ("disconnected: " ^ reason)
    | e -> log ("connection error: " ^ Printexc.to_string e));
    log (Printf.sprintf "reconnecting in %.0fs" !delay);
    Eio.Time.sleep env#clock !delay;
    delay := Float.min 30.0 (!delay *. 2.0)
  done
