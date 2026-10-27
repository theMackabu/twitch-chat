open Twitch_chat

let check name cond = if not cond then failwith ("FAILED: " ^ name)

let parse_exn line =
  match Irc.parse line with Some m -> m | None -> failwith ("no parse: " ^ line)

let () =
  (* Full Twitch PRIVMSG: tags with escapes and an empty value, prefix, trailing param. *)
  let m =
    parse_exn
      "@badge-info=;color=#1E90FF;display-name=Some\\sUser;emotes=;msg\\:x=a\\:b;mod=0 \
       :someuser!someuser@someuser.tmi.twitch.tv PRIVMSG #chan :hello there :) ;x\r"
  in
  check "command" (m.command = "PRIVMSG");
  check "prefix" (m.prefix = Some "someuser!someuser@someuser.tmi.twitch.tv");
  check "nick" (Irc.nick m = Some "someuser");
  check "params" (m.params = [ "#chan"; "hello there :) ;x" ]);
  check "display-name unescaped" (List.assoc_opt "display-name" m.tags = Some "Some User");
  check "semicolon escape" (List.assoc_opt "msg\\:x" m.tags = Some "a;b");
  check "empty tag value" (List.assoc_opt "emotes" m.tags = Some "");
  check "tag count" (List.length m.tags = 6);

  (* No tags, no prefix. *)
  let m = parse_exn "PING :tmi.twitch.tv" in
  check "ping command" (m.command = "PING");
  check "ping prefix" (m.prefix = None && m.tags = []);
  check "ping trailing" (m.params = [ "tmi.twitch.tv" ]);

  (* Middle params only, and a trailing param that is empty. *)
  let m = parse_exn ":tmi.twitch.tv CAP * ACK :" in
  check "cap params" (m.params = [ "*"; "ACK"; "" ]);
  check "server prefix nick" (Irc.nick m = Some "tmi.twitch.tv");

  (* Numeric command, trailing param starting with ':' keeps the inner colon. *)
  let m = parse_exn ":tmi.twitch.tv 001 justinfan123 ::-) Welcome" in
  check "numeric" (m.command = "001");
  check "trailing with colon" (m.params = [ "justinfan123"; ":-) Welcome" ]);

  check "empty line" (Irc.parse "" = None);
  print_endline "irc parser tests passed"
