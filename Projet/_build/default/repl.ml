let split_ws (s : string) : string list =
  let str_l = String.split_on_char ' ' s in
  List.filter (fun t -> t <> "") str_l

open Commands
open Filesystem

let rec loop (fs : Filesystem.filesystem) : unit =
  (* Affiche le chemin courant en couleur  *)
  Printf.printf "\027[32m%s\027[0m> %!" (Filesystem.path_to_string fs.current_path);
  match read_line () with
  | exception End_of_file -> print_newline ()
  | line ->
      let tockens = split_ws line in 
      begin
        match tockens with
        | "quit" :: _ | "exit" :: _ ->
            print_endline "Exit"
        | [] -> loop fs
        |"pwd"::tockens'->(match tockens' with
            |[]-> pwd fs;
                  loop fs
            | _->print_endline"pwd ne prends aucun arguments";
                loop fs)

        |"mkdir"::tockens'-> (match tockens' with 
              |x::[]-> 
                if x.[0] <>'.' then 
                (match (Filesystem.isName x) with 
                    |Some (Name x') -> let fs' = mkdir (Name x') fs in loop fs'
                    |None-> print_endline "mkdir:le nom d'un fichier ne doit pas contenir de \"/\"";loop fs)
                else (let fs' = let path = (List.map (fun x-> Name x) (split_sh x)) 
                              in mkdirEx (List.tl path) fs 
                                in loop fs')
              |_-> print_endline "mkdir ne prends qu'un seul argument";
                    loop fs)

        | "touch"::tockens' -> (match tockens' with 
              |x::[]-> 
                if x.[0] <>'.' then 
                (match (isName x) with 
                    |Some (Name x') -> let fs' = touch x' fs in loop fs'
                    |None-> print_endline "touch:le nom d'un fichier ne doit pas contenir de \"/\"";loop fs)
                else (let fs' = let path = (List.map (fun x-> Name x) (split_sh x)) 
                              in touchEx (List.tl path) fs 
                                in loop fs')
              |_-> print_endline "touch : Cette commande ne prends qu'un seul argument";
                    loop fs)

        | "ls"::tockens'->(match tockens' with 
                              |[]->ls fs; 
                                loop fs
                              |_->print_endline "ls: cette commande ne prends aucun argument";
                                loop fs)

        | "cat"::tockens' -> begin
            match tockens' with
              | [] -> print_endline "cat : cette commande prends en argument le nom d'un fichier existant dans ce répertoire, puis affiche ce fichier";
                    loop fs
              | x::[] -> cat x fs; loop fs
              | _ -> print_endline "cat : Cette command ne prend qu'un seul argument"; loop fs
            end
            
        | "write"::tockens' -> begin
          match tockens' with
              |[] -> print_endline "Erreur de syntaxe : write <nomdufichier> \"le contenu du texte à rajouter\""; loop fs
              |_::[] -> print_endline "Erreur de syntaxe : write <nomdufichier> \"le contenu du texte à rajouter\""; loop fs
              |x::xs -> let nd = estPresentBis (Name x) fs.root.children in begin (*on ne vérifie pas si x est contient un "/", parce qu'on est sûr que si le fichier de ce nom existe ça été déjà vérifier à la création avec touch*)
                match nd with
                  |None -> print_endline "Aucun fichier ne porte ce nom dans ce répertoire \n syntaxe : write <nomdufichier> \"le contenu du texte à rajouter\""; loop fs
                  |Some nd' -> begin 
                    match nd' with 
                      |File fl -> let str = concat " " xs in let fs' = write fl str fs in loop fs'
                      |Dir _d -> print_endline "Aucun fichier ne porte ce nom dans ce répertoire"; loop fs
                    end
                  end
                end
        (* Insérer les commandes ici *)
        |"cd"::tockens' -> begin
          match tockens' with
              |y::[] -> let res = cd (split_sh y) fs in begin(*split_sh nous permet de décomposer le chemin donner en argument en une liste de name qui correspond au chemin à suivre dans l'arbre*)
                  match fst res with
                    |true -> loop (snd res)
                    |false -> loop fs
                  end
              |_->print_endline "cd: Cette commande prends en arguments un seul arguments qui est un chemin vers le répertoire dans lequel vous souhaitez vous rendre ";
                  loop fs
            end
        |"rm"::tockens' -> begin
          match tockens' with
          |x::[] -> if x.[0]<>'.' then let fs' = rm (Name x) fs in loop fs'
                  else (let fs' = let len = String.length x in rmext (String.sub x 0 (len-1)) fs 
                                  in loop fs')
          |_ -> print_endline "rm: cette commande prend exactement un argument"; loop fs 
        end

            (*Commande find  action*)
        |"find"::tockens' -> begin
           match tockens' with
            | x::[] -> find (Name x) fs; loop fs
            | _ -> print_endline "find: cette commande prend exactement un argument"; loop fs
         end
           (*Commande cp action*)
         |"cp"::tockens' -> begin
          match tockens' with
          | x::xs::[] -> 
            let fs' = cp (Name x) (List.map (fun x-> Name x) (split_sh xs)) fs in 
           loop fs'
          |_->print_endline "cp : Cette commande neprends que deux arguments"; loop fs
         end
        |"mv"::tockens' -> begin
          match tockens' with
          | x::xs::[] -> 
            let fs' = mv (Name x) (List.map (fun x-> Name x) (split_sh xs)) fs in 
           loop fs'
          |_->print_endline "mv : Cette commande neprends que deux arguments"; loop fs
         end

        | command :: _ ->
            Printf.eprintf "Command not found: %s\n%!" command;
            loop fs
      end
    

  