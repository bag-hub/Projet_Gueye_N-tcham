
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

(*Cette fonction prends en paramétre le nom d'un dossier ou un fichier et une liste de node(dossier ou fichier) et retourne ce node s'il existe dans la liste et Node sinon*)
let rec estPresentBis node liste = match liste with
                            |[]-> None
                            |x::xs-> begin
                                match x,node with 
                                    |File fl,Name nf-> if fl.name=Name nf then Some (File fl) else estPresentBis node xs
                                    |Dir d,Name nf-> if d.name=Name nf then Some (Dir d) else estPresentBis node xs
                                    end

(*Cette fonction permet de convertir un name en un string*)
let nameToString name = match name with |Name name->name
(*pwd*)
let pwd fs = print_endline (path_to_string fs.current_path)

(*mkdir*)
(*let mkdir nameD fs = if  estPresent (Dir {name=nameD;children=[]}) fs.root.children then fs
                    else let new_dir = {name=fs.root.name; children=(Dir {name=nameD;children=[]})::fs.root.children}
                        in {root=new_dir; current_path=fs.current_path}*)
                        
(*mkdir permet de vérifier si un fichier  est dans un des repertoire de filesystem, Si c'est le cas, elle renvoi la confirmation ... Sinon elle creer ce element dans le directory de filesystem*)
let mkdir nameD fs = 
    let nd = estPresentBis nameD fs.root.children
    in match nd with 
        |None -> let new_dir = {name=fs.root.name; children=(Dir {name=nameD;children=[]})::fs.root.children}
                    in {root=new_dir; current_path=fs.current_path}
        |Some ndO-> begin 
            match ndO with 
                |File _fl -> print_endline "mkdir: impossible de créer le répertoire de ce nom car un fichier portant ce nom existe";
                            fs
                |Dir _d -> print_endline "mkdir: impossible de créer un répertoire de ce nom car un dossier de ce nom existe déjà"; 
                            fs
                    end

(*touch*)
let touch file_name fs = match Filesystem.isName file_name with
    |Some file_name' -> let node = estPresentBis file_name' fs.root.children
                    in begin 
                        match node with
                            |Some nd -> begin 
                                match nd with
                                    |Dir _d -> print_endline "touch: impossible de créer le fichier de ce nom car le dossier portant ce nom existe";
                                                fs
                                    |File _fl-> print_endline "Il existe déja un fichier portant ce nom";
                                            fs
                                        end
                            |None-> let nouveau_root = {name=fs.root.name;children=(File {name=file_name';content=""})::fs.root.children} in {root=nouveau_root;current_path=fs.current_path}
                        end
    |None -> print_endline "touch: Le nom d'un ne doit pas contenir le caractére \"/\"";
            fs
    (*if estPresent (File {name=namef;content=""}) fs.root.children then fs
    else let new_dir = {name=fs.root.name; children=(File {name=namef;content=""})::fs.root.children}
                        in {root=new_dir; current_path=fs.current_path}*)


(*ls directory_name*)
let ls fs = if fs.root.children = [] then print_endline "Ce répertoire vide"
    else begin 
    let rec aux l = match l with
        |[]->print_string ""
        |x::xs-> begin
            match x with 
                |File fl-> begin match fl.name with 
                            | Name s->print_endline s; 
                                aux xs end
                |Dir d-> begin match d.name with 
                            | Name s->print_endline s;
                                aux xs end
            end
    in aux fs.root.children
    end

(*cat file_name*)
let cat file_name fs = let file_name' = Filesystem.isName file_name
    in match file_name' with
        |Some nm -> let node = estPresentBis nm fs.root.children 
            in begin match node with
            |None -> print_string "cat: Erreur un fichier portant ce nom n'existe pas dans ce répertoire"
            |Some node -> begin
                            match node with 
                                |File fl-> let str = fl.content in if str = "" then print_endline "Ce fichier est vide" 
                                                                    else print_endline str
                                |Dir _d -> print_string "cat: Erreur un fichier portant ce nom n'existe pas dans ce répertoire. Mais il y existe un dossier de ce nom"
                        end
                    end
        |None -> print_string "cat: Erreur un fichier portant ce nom n'existe pas dans ce répertoire"

(*write file_name*)
let write file_name str fs = 
    let node = estPresentBis file_name fs.root.children in
    begin
        match node with 
            |None -> print_endline "Aucun fichier ne porte ce nom dans ce répertoire"; fs
            |Some nd -> begin 
                 match nd with
                    |Dir _d -> print_endline "Aucun fichier ne porte ce nom dans ce répertoire"; fs
                    |File fl -> let new_file = {name=fl.name;content=fl.content^str}
                                in (let new_root = {name=fs.root.name;children=let new_children = Filesystem.remove fs.root.children fl.name 
                                                                                in File new_file::new_children} 
                                    in {root=new_root;current_path=fs.current_path})
                        end
    end


(*write file_name
let write file_name str fs = 
    if not (estPresent (File {name=file_name;content=""}) fs.root.children) then failwith "Ce fichier est inexistant dans ce répertoire courant"
    else let new_file = {fs.}

Recupere les champs nom et chidren du repertoir courant qui est filesystem.ml et créer un nouveau repertoire rep_creer

let mkdir (fs : filesystem) (rep_creer : name) : filesystem =  
    let rec extac_dir  dir = match dir with
        |{name; children} -> let verif_present =  estPresent (function
            |Dir d when d.name = rep_creer -> true
            |File f when f.name = rep_creer -> true
            |_ -> false) children in 

            if (verif_present) then 
                failwith "Erreur : Un element a déja ce nom"
            else 
                let new_rep = Dir {
                    name = rep_creer; children = []
                } in {dir with children = new_rep :: children} in
                {fs with root = extac_dir fs.root};;

touch Création de fichier vide dans le repertoir courant filesystem
let touch  (fs : filesystem) (fichier_creer : name) : filesystem = 
   let rec touch_fil t_fil = match t_fil
        |{name; children} -> let verif_touch = 
               estPresent (function
                 | Dir d when d.name = fichier_creer -> true
                 | File f when f.name = fichier_creer -> true
                 |_ -> false) children in

                 if(verif_touch) then failwith "Deja present"
                 else 
                     let new_file = File { name = fichier_creer; content = ""}
                                 {t_fil with children = new_file :: children} in

                                 {fs with root = touch_fil fs.root};;

let ls(fs filesystem*)