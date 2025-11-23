let split_ws (s : string) : string list =
  let str_l = String.split_on_char ' ' s in
  List.filter (fun t -> t <> "") str_l

let rec loop (fs : Filesystem.filesystem) : unit =
  (* Affiche le chemin courant en couleur  *)
  Printf.printf "\027[32m%s\027[0m> %!" (Filesystem.path_to_string fs.current_path);
  match read_line () with
  | exception End_of_file -> print_newline ()
  | line -> print_string line;
      let tokens = split_ws line in 
      begin
        match tokens with
        | "quit" :: _ | "exit" :: _ ->
            print_endline "Exit"
        | [] -> loop fs
        |"pwd"::tokens'->(match tokens' with
            |[]-> Commands.pwd fs;
                  loop fs
            | _->print_endline"pwd ne prends aucun arguments";
                loop fs)

        |"mkdir"::tokens'-> (match tokens' with 
                              |x::[]-> (match (Filesystem.isName x) with 
                                    |Some (Name x') -> let fs' = Commands.mkdir (Name x') fs in loop fs' (* pour éviter cette erreur modifier la fonction isName afin qu'elle retourne le string sous forme de Name 
                              si tout est bon et None sinon avec la signature string->Name option *)
                                    |None-> print_endline "le nom d'un fichier";loop fs)
                              |_-> print_endline "mkdir ne prends qu'un seul argument";
                                    loop fs)

        | "touch"::tockens' -> begin
            match tockens' with
              |x::[] -> let fs' = Commands.touch x fs in loop fs'
              |_-> print_endline "touch: ce commande ne prends qu'un seul argumen";
                  loop fs
          end

        | "ls"::tockens'->(match tockens' with 
                              |[]->Commands.ls fs; 
                                loop fs
                              |_->print_endline "ls ne prends aucun n'arguments";
                                loop fs)

        | "cat"::tockens' -> begin
            match tockens' with
              | [] -> print_endline "cat : cette commande prends en argument le nom d'un fichier existant dans ce répertoire, puis affiche ce fichier";
                    loop fs
              | x::[] -> Commands.cat x fs; loop fs
              | _ -> print_endline "cat : Cette command ne prend qu'un seul argument"; loop fs
            end
            
        | "write"::tockens' -> begin
          match tockens' with 
              |x::[] -> let nd = Commands.estPresentBis (Name x) fs.root.children in begin 
                match nd with
                  |None -> print_endline "Aucun fichier ne porte ce nom dans ce répertoire"; loop fs
                  |Some nd' -> begin 
                    match nd' with |File fl -> let str = read_line () in let fs' = Commands.write fl.name str fs in loop fs'
                          |Dir _d -> print_endline "Aucun fichier ne porte ce nom dans ce répertoire"; loop fs
                    end
                  end
              |[] -> print_endline "write : cette commande prends en argument le nom d'un fichier existant dans ce répertoire et permet de saisir du te'xte dans ce fichier";
              |_::_ -> print_endline "write : Cette command ne prend qu'un seul argument"; loop fs end
        (* Insérer les commandes ici *)

        | command :: _ ->
            Printf.eprintf "Command not found: %s\n%!" command;
            loop fs
      end
