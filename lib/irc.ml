(* IRC message parsing, with IRCv3 tags as used by Twitch. *)

type message = {
  tags : (string * string) list;
  prefix : string option;
  command : string;
  params : string list;
}

(* IRCv3 tag value escapes: \: -> ';', \s -> ' ', \\ -> '\', \r, \n. *)
let unescape_tag_value s =
  let b = Buffer.create (String.length s) in
  let n = String.length s in
  let rec go i =
    if i < n then
      if s.[i] = '\\' && i + 1 < n then begin
        (match s.[i + 1] with
        | ':' -> Buffer.add_char b ';'
        | 's' -> Buffer.add_char b ' '
        | 'r' -> Buffer.add_char b '\r'
        | 'n' -> Buffer.add_char b '\n'
        | c -> Buffer.add_char b c);
        go (i + 2)
      end
      else if s.[i] = '\\' then go (i + 1)
      else begin
        Buffer.add_char b s.[i];
        go (i + 1)
      end
  in
  go 0;
  Buffer.contents b

let parse_tags s =
  String.split_on_char ';' s
  |> List.filter_map (fun kv ->
         if kv = "" then None
         else
           match String.index_opt kv '=' with
           | None -> Some (kv, "")
           | Some i ->
               Some
                 ( String.sub kv 0 i,
                   unescape_tag_value
                     (String.sub kv (i + 1) (String.length kv - i - 1)) ))

(* Split off the first space-delimited word; skips repeated spaces. *)
let rec word s =
  match String.index_opt s ' ' with
  | Some 0 -> word (String.sub s 1 (String.length s - 1))
  | Some i -> (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1))
  | None -> (s, "")

let rec parse_params s =
  if s = "" then []
  else if s.[0] = ':' then [ String.sub s 1 (String.length s - 1) ]
  else if s.[0] = ' ' then parse_params (String.sub s 1 (String.length s - 1))
  else
    let p, rest = word s in
    p :: parse_params rest

let parse line =
  let line =
    let n = String.length line in
    if n > 0 && line.[n - 1] = '\r' then String.sub line 0 (n - 1) else line
  in
  let tags, rest =
    if line <> "" && line.[0] = '@' then
      let t, rest = word line in
      (parse_tags (String.sub t 1 (String.length t - 1)), rest)
    else ([], line)
  in
  let prefix, rest =
    if rest <> "" && rest.[0] = ':' then
      let p, rest = word rest in
      (Some (String.sub p 1 (String.length p - 1)), rest)
    else (None, rest)
  in
  let command, rest = word rest in
  if command = "" then None
  else Some { tags; prefix; command; params = parse_params rest }

(* "nick!user@host" -> "nick" *)
let nick m =
  Option.map
    (fun p -> match String.index_opt p '!' with Some i -> String.sub p 0 i | None -> p)
    m.prefix
