
(*Module de base défini par filesystem.ml est Filesystem*)
open Filesystem 

(*fonction récursive pour  qui vérifie si un élts précis est présent dans notre liste*)
let rec estPresent node liste= match liste with 
       |[] -> false 
       | x::xs -> (match (node,x) with 
                    |(File nf,File fl)->if fl.name=nf.name then true 
                            else estPresent node xs
                    |(Dir nd,Dir d)->d.name=nd.name || estPresent node xs
                    |_->false)

(*pwd*)
let pwd fs = print_endline (path_to_string fs.current_path)

(*mkdir permet de vérifier si un fichier  est dans un des repertoire de filesystem, Si c'est le cas, elle renvoi la confirmation ... Sinon elle creer ce element dans le directory de filesystem*)
let mkdir nameD fs = 

    match (cd_current_dir fs fs.current_path) with 
    |Some d -> let nd = estPresentBis nameD d.children
            in begin
                match nd with 
                |None -> let new_dir = add_to_dir fs.current_path fs.root (Dir ({name=nameD; children=[]}))
                        in {root=new_dir; current_path=fs.current_path}
                |Some ndO-> begin 
                    match ndO with 
                        |File _fl -> print_endline "mkdir: impossible de créer le répertoire de ce nom car un fichier portant ce nom existe";
                                    fs
                        |Dir _d -> print_endline "mkdir: impossible de créer un répertoire de ce nom car un dossier de ce nom existe déjà"; 
                                    fs
                            end
                end
    |None -> print_string "yes";fs (*On n'est sur de ne jamais atteindre ce cas car le current_path du filesystem est bien fait donc chaque name correspond forcément à un dossier, on s'assure de garder ces propriété lors de la création ou la suppresion d'un dossier  *)

(*touch file_name*)
let touch file_name fs = match Filesystem.isName file_name with
    |Some file_name' -> let current_dir = cd_current_dir fs fs.current_path in begin
        match current_dir with 
            |Some dir' -> let node = estPresentBis file_name' dir'.children
                    in begin 
                        match node with
                            |Some nd -> begin 
                                match nd with
                                    |Dir _d -> print_endline "touch: impossible de créer le fichier de ce nom car le dossier portant ce nom existe";
                                                fs
                                    |File _fl-> print_endline "Il existe déja un fichier portant ce nom";
                                            fs
                                        end
                            |None-> let new_dir = add_to_dir fs.current_path fs.root (File ({name=file_name'; content=""}))
                        in {root=new_dir; current_path=fs.current_path} end
            |None -> fs(*On n'est sur de ne jamais atteindre ce cas car le current_path du filesystem est bien fait donc chaque name correspond forcément à un dossier, 
                    on s'assure de garder ces propriété lors de la création ou la suppresion d'un dossier *)
                    end
    |None -> print_endline "touch: Le nom d'un ne doit pas contenir le caractére '/'";
            fs


(*ls directory_name*)
let ls fs = let d = cd_current_dir fs fs.current_path in
    match d with 
    |Some d' -> if d'.children = [] then print_endline "Ce répertoire vide"
        else begin 
        let rec aux l = match l with
            |[]->print_string ""
            |x::xs-> begin
            match x with 
                |File fl-> begin match fl.name with 
                            | Name s->print_endline s; 
                                aux xs end
                |Dir d-> begin match d.name with 
                            | Name s->print_endline (s^"/");
                                aux xs end
            end
        in aux d'.children
        end
    |None -> print_string "" (*on n'affiche rien, ce cas ne devrait jamais arriver si le current path est bien créer et bien gérer lors de l'éxécution des commandes*)

(*cat file_name*)
let cat file_name fs = let file_name' = Filesystem.isName file_name
    in match file_name' with
        |Some nm -> let node = estPresentBis nm fs.root.children 
            in begin match node with
            |None -> print_endline "cat: Erreur un fichier portant ce nom n'existe pas dans ce répertoire"
            |Some node -> begin
                            match node with 
                                |File fl-> let str = fl.content in if str = "" then print_endline "Ce fichier est vide" 
                                                                    else print_endline str
                                |Dir _d -> print_endline "cat: Erreur un fichier portant ce nom n'existe pas dans ce répertoire. Mais il y existe un dossier de ce nom"
                        end
                    end
        |None -> print_endline "cat: Erreur un fichier portant ce nom n'existe pas dans ce répertoire"

(*write file_name texte_a_ajouter
dans texte à ajouter, on a le droit de mettre un ou plusieurs '"'(pas besoin de déspécialiser) dès lors que tout le texte qu'on souhaite ajouté est entre ""*)
let write (fl:file) str fs = 
    let new_file = {name=fl.name; content=fl.content^str}
        in (let new_root = {name=fs.root.name;children=
                                                let new_children = remove (fs.root.children) fl.name in (File new_file)::new_children} 
                                    in {root=new_root;current_path=fs.current_path})


(*cd : qui permet de se déplacer dans l’arborescence en suivant le chemin
relatif nomchemin pour modifier le chemin courant. Elle affiche une erreur si ce chemin
relatif ne mène à aucun répertoire existant*)
(*Cette fonction prends en paramétre une liste de name qui cooresponds au chemin que l'on veut suivre pour se déplacer dans un autre répertoire et un filesystem,
retourne un couple de boléen et un filsystem qui indique si on s'est déplacé dans le répertoire comme voulu ou non et un nouveau filesystem avec le même arborescence(root),
mais avec un current_path probabablement différent  *)
(*ok/error constructor*)
let cd nom_chemin fs = 
    match cd_current_dir fs fs.current_path with 
    |Some c_dir -> begin
    let rec aux lst acc dir_p= 
        match lst with 
        |[] -> true,{root=fs.root;current_path=acc}
        |x::xs->
            if (x= "..") then 
                let acc' = begin
                match acc with
                    |[] -> print_endline "Vous êtes déjà à la racine cd .. ne marche plus" ; []
                    |_ -> removeLast acc 
                    end
                in  begin 
                    match cd_current_dir fs acc' with 
                        |Some d -> aux xs acc' d
                        |None -> false,fs (*Ce cas ne devrait jamais arriver dans le cas où le current_path du filesystem est bien fait, donc chaque name correspond forcément à un dossier, 
                                on s'assure de garder ces propriété lors de la création ou la suppresion d'un dossier  *)
                    end
            else(begin 
                match estPresentBis (Name x) dir_p.children with(*peut être enlever search et mettre isPresent*)
                    |None -> print_endline "cd : ce chemin est invalide"; false,fs
                    |Some y -> begin  match y with
                        | Dir d -> aux xs (acc@[d.name]) d
                        | File _ -> print_endline "cd : ce chemin est invalide"; false,fs
                    end
                end)
    in aux nom_chemin fs.current_path c_dir end
    |None -> false,fs (*Ce cas ne devrait jamais arriver dans le cas où le current_path du filesystem est bien fait, donc chaque name correspond forcément à un dossier,
             on s'assure de garder ces propriété lors de la création ou la suppresion d'un dossier  *)


(*mv nomdelelement nomduchemin qui permet de déplacer un fichier ou un réper-
toire nomdelelement dans le chemin relatif nomduchemin. Elle affiche une erreur
si aucun élément ne porte nom nomdelelement dans le répertoire courant, ou si un
élément porte déjà ce nom dans le répertoire accesible via nomduchemin.*)
let mv node_name _ fs = let d = cd_current_dir fs fs.current_path in 
    (*On vérifie s'il y a un node de name node_name après s'être déplacer sur dossier courant dèja*)
    match d with 
    |Some d' -> let node = estPresentBis node_name d'.children
        in (match node with
            |Some _node' -> fs
            |None -> fs )
    |None -> fs (* on peut pas avoir ce cas là avec le current_path, s'il est bien fait et mise à jour*)

let find nomFichier fs =
   let rec auxFind liste chemin =
     match liste with 
         |[] -> None
         | x::xs -> let _ = Filesystem.comparer nomFichier liste in
             match x with
              |File f -> if f.name = nomFichier then ( 
                print_endline(path_to_string (chemin @[f.name]));
                 Some(chemin @ [f.name]))
                        else auxFind xs chemin
              |Dir d -> if d.name = nomFichier  then (
                print_endline(path_to_string (chemin @ (d.name ::[]))^"/") ;
                auxFind xs chemin)
                else  
                    match auxFind d.children (chemin @ (d.name ::[])) with
                    |None -> auxFind xs chemin
                    |Some chemin -> Some chemin
            in 
            ignore (auxFind fs.root.children fs.current_path)

   


(*rm command idée*)
let rm node_name fs = match cd_current_dir fs fs.current_path with 
    |None ->print_endline"Erreur"; fs
    |Some dir -> 
        match estPresentBis node_name dir.children with
           |None -> print_endline "Elt pas present"; fs
           |Some _ -> 
            let new_children = removeBis dir.children node_name in
            let dir_nouveau = {name = dir.name; children = new_children}  in  
            let new_root = Filesystem.replace_dir fs.root fs.current_path dir_nouveau 
    in {fs with root = new_root}
      
let cp node_name path fs =
  match cd_current_dir fs fs.current_path with
  | None ->
      print_endline "Erreur: répertoire courant invalide";
      fs
  | Some dir -> begin
      match estPresentBis node_name dir.children with
          |None -> print_endline "cp: elts introuvable"; 
             fs
          |Some elt ->  let start_copie = copieBis elt in
            begin
              match cd_current_dir fs path with
               |None -> print_endline "cp : chemin cible invalide"; 
                  fs
               |Some dir_cible-> begin
                   match estPresentBis node_name dir_cible.children with
                        |Some _ -> print_endline"cp: un elt de ce nom existe déjà"; fs
                        |None -> let dir_nouveau = 
                            {name = dir_cible.name; children = start_copie :: dir_cible.children} in
                            let nouveau_root = 
                                Filesystem.replace_dir fs.root path dir_nouveau in 
                                     {fs with root = nouveau_root}
                        end
                end
        end

let mkdirEx path_p fs = 
    let path_rev = List.rev path_p in
    match path_rev with 
    |nameD::path_rev' -> let path = List.rev path_rev' in (
        match cd_current_dir fs path with
        |Some d -> let nd = estPresentBis nameD d.children
                in (
                    match nd with 
                    |None -> let new_dir = add_to_dir path fs.root (Dir ({name=nameD; children=[]}))
                            in {root=new_dir; current_path=fs.current_path}
                    |Some ndO-> ( 
                        match ndO with 
                            |File _fl -> print_endline "mkdir: impossible de créer le répertoire de ce nom car un fichier portant ce nom existe dans le répertoire cible";
                                        fs
                            |Dir _d -> print_endline "mkdir: impossible de créer un répertoire de ce nom car un dossier de ce nom existe déjà dans le répertoire cible"; 
                                        fs ))
        
        |None -> print_endline "mkdir: Chemin invalide";fs )
    |[] -> print_endline "mkdir: Chemin invalide";fs


(*touchEx file_name*)
let touchEx path_p fs = 
    let path_rev = List.rev path_p in
    match path_rev with 
    |nameD::path_rev' -> let path = List.rev path_rev' in (
        match cd_current_dir fs path with
        |Some d -> let nd = estPresentBis nameD d.children
                in (
                    match nd with 
                    |None -> ( match isName (name_to_string nameD) with
                        |Some nameD' -> let new_dir = add_to_dir path fs.root (File ({name=nameD'; content=""}))
                            in {root=new_dir; current_path=fs.current_path}
                        |None -> print_endline "touch: Le nom d'un ne doit pas contenir le caractére '/'"; fs)
                    |Some ndO-> ( 
                        match ndO with 
                            |File _fl -> print_endline "touch: impossible de créer le fichier de ce nom car un fichier portant ce nom existe dans le répertoire cible";
                                        fs
                            |Dir _d -> print_endline "touch: impossible de créer un fichier de ce nom car un dossier de ce nom existe déjà dans le répertoire cible"; 
                                        fs ))
        
        |None -> print_endline "touch: Chemin invalide";fs )
    |[] -> print_endline "touch: Chemin invalide";fs
