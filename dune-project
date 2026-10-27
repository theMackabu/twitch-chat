(lang dune 3.20)

(name twitch_chat)

(generate_opam_files false)

(package
 (name twitch_chat)
 (synopsis "Print a Twitch channel's chat to stdout, anonymously, over WebSocket")
 (depends
  (ocaml
   (>= 5.2))
  eio_main
  tls
  tls-eio
  ca-certs
  mirage-crypto-rng
  domain-name
  httpun
  httpun-ws
  bigstringaf
  cstruct
  gluten-eio
  digestif))
