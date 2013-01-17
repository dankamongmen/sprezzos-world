
#load "str.cma"

let option_no_arg opt hlp =
 List.iter print_endline
   [
     ".TP";
     ".B \\"^opt;
     hlp
   ]

let option_arg opt arg hlp =
 List.iter print_endline
   [
     ".TP";
     ".B \\"^opt^" "^arg;
     hlp
   ]

let () =
  let fn =
    Filename.temp_file "unison-" ".txt"
  in
    begin
      try
        let cmd =
          (String.concat " " (List.tl (Array.to_list Sys.argv)))^
          " -help > "^
          (Filename.quote fn)
        in
          match Sys.command cmd with
            | 2 ->
                begin
                  let actions =
                    (* *)
                    (List.map
                       (fun (s, f) -> Str.regexp s, f)
                       [
                         " *\\(-[a-z_-]+\\) xxx +\\(.*\\)",
                         (fun s ->
                            option_arg
                              (Str.matched_group 1 s)
                              "xxx"
                              (Str.matched_group 2 s));

                         " *\\(-[a-z_-]+\\) n +\\(.*\\)",
                         (fun s ->
                            option_arg
                              (Str.matched_group 1 s)
                              "n"
                              (Str.matched_group 2 s));

                         " *\\(-[a-z_-]+\\) +\\(.*\\)",
                         (fun s ->
                            option_no_arg
                              (Str.matched_group 1 s)
                              (Str.matched_group 2 s));
                       ])
                    @
                    (List.map
                       (fun s -> Str.regexp s, ignore)
                       [
                         "Advanced options:";
                         "Basic options:";
                         "Usage: unison \\[options\\]";
                         " *or unison root1 root2 \\[options\\]";
                         " *or unison profilename \\[options\\]";
                         "^$";
                       ])
                  in
                  let chn =
                    open_in fn
                  in
                    begin
                      try
                        while true do
                          let line =
                            input_line chn
                          in
                            try
                              let (r, a) =
                                List.find
                                  (fun (r, a) ->
                                     Str.string_match r line 0)
                                  actions
                              in
                              let _b : bool =
                                (* Ensure that we run string_match just
                                   before calling a, to fill the good
                                   variable in Str
                                 *)
                                Str.string_match r line 0
                              in
                                a line

                            with Not_found ->
                              failwith
                                (Printf.sprintf
                                   "No matching regexp for '%s'"
                                   line)
                        done
                      with End_of_file ->
                        ()
                    end;
                    close_in chn
                end
            | n ->
                Printf.eprintf "Command '%s' exited with code %d"
                  cmd
                  n
      with e ->
        Sys.remove fn;
        raise e
    end;
    Sys.remove fn

