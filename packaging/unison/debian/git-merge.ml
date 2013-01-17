#!/usr/bin/ocamlrun ocaml

#use "topfind";;
#require "fileutils";;

open FileUtil;;

let save fn =
  let tmp =
    Filename.temp_file ("unison-merge-"^(Filename.basename fn)) ".bak"
  in
    cp [fn] tmp;
    fn, tmp

let restore ?(ext="") (fn, tmp) =
  mv tmp (fn^ext)

let fns =
  [
    "debian/changelog";
    "debian/control";
    "debian/gbp.conf";
  ]

let baks =
  List.rev_map save fns

let () =
  try
    let cmd =
      "git merge "^(String.concat " " (List.tl (Array.to_list Sys.argv)))
    in
      prerr_endline ("Runnning "^cmd);
      match Sys.command cmd with
        | 0 ->
            List.iter restore baks
        | n ->
            Printf.eprintf
              "Command '%s' exited with code %d/n%!"
              cmd
              n;
            List.iter (restore ~ext:".old") baks

  with e ->
    rm (List.map snd baks)
