
(*Module de base défini par filesystem.ml est Filesystem*)
open Filesystem 

(*fonction récursive pour  qui vérifie si un élts précis est présent dans notre liste*)(*A supprimer???*)
let rec estPresent node liste= match liste with 
       |[] -> false 
       | x::xs -> (match (node,x) with 
                    |(File nf,File fl)->if fl.name=nf.name then true 
                            else estPresent node xs
                    |(Dir nd,Dir d)->d.name=nd.name || estPresent node xs
                    |_->false)

let rec comparer node liste= match liste with
        |[]-> print_string " repertoire est vide"
        | x::xs -> match x with 
                 |Dir _ -> print_string "Rien faire"
                 |File f -> if f.name = node then print_string "Trouver" else comparer node xs

                   
                 



(*Cette fonction permet de convertir un name en un string*)
let nameToString name = match name with |Name name->name
(*pwd*)
let pwd fs = print_endline (path_to_string fs.current_path)

(*mkdir permet de vérifier si un fichier  est dans un des repertoire de filesystem, Si c'est le cas, elle renvoi la confirmation ... Sinon elle creer ce element dans le directory de filesystem*)
let mkdir nameD fs = 
    match (cd_current_dir fs) with 
    |Some d -> let nd = estPresentBis nameD d.children
            in begin
                match nd with 
                |None -> let new_dir = {name=fs.root.name; children=(Dir {name=nameD;children=[]})::fs.root.children}
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
    |None -> print_endline "touch: Le nom d'un ne doit pas contenir le caractére '/'";
            fs


(*ls directory_name*)
let ls fs = if fs.root.children = [] then print_endline "Ce répertoire vide"
    else begin 
    let rec aux l = match l with
        |[]->print_endline ""
        |x::xs-> begin
            match x with 
                |File fl-> begin match fl.name with 
                            | Name s->print_endline s; 
                                aux xs end
                |Dir d-> begin match d.name with 
                            | Name s->print_endline (s^"/");
                                aux xs end
            end
    in aux fs.root.children
    end

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
dans le texte à ajouter on doit pas mettre un caractére '"' tout seul ou au début d'un mot ou à la fin d'un sans le déspécialiser avec '\', on autoriser le fait demettre '"' au milieu d'un mot*)
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
let cd nom_chemin fs = 
    let rec aux lst acc = 
        match lst with 
        |[] -> true,{root=fs.root;current_path=acc}
        |x::xs->
            if (x= "..") then 
                let acc' = begin
                match acc with
                    |[] -> print_endline "Vous êtes déjà à la racine cd .. ne marche plus" ; []
                    |_ -> removeLast acc 
                    end
                in aux xs acc'
            else(begin 
                match Filesystem.search fs.root.children (Name x) with(*peut être enlever search et mettre isPresent*)
                    |None -> print_endline "cd : ce chemin est invalide"; false,fs
                    |Some y -> aux xs (acc@[y.name])
                end)
    in aux nom_chemin fs.current_path

(*(*find nomdufichier, qui affiche tous les fichiers s’appelant nomdufichier dans
la sous-arborescence partant du répertoire courant. Le chemin absolu de chacun de ces
fichiers est affiché.*)
let find file_name fs = ()

(*rm nomdelelement qui permet de supprimer un fichier ou un répertoire nomdelelement
du répertoire courant. Elle affiche une erreur si aucun élément ne porte le nom nomdelelement
dans le répertoire courant.*)
let rm node_name = ()

(*mv nomdelelement nomduchemin qui permet de déplacer un fichier ou un réper-
toire nomdelelement dans le chemin relatif nomduchemin. Elle affiche une erreur
si aucun élément ne porte nom nomdelelement dans le répertoire courant, ou si un
élément porte déjà ce nom dans le répertoire accesible via nomduchemin.*)
let mv node_name path_p = ()

(*cp nomdelelement nomduchemin qui permet de copier (en le dupliquant) un fi-
chier ou un répertoire nomdelelement dans le chemin relatif nomduchemin. Elle
affiche une erreur dans les mêmes cas que la commande mv.*)
let mv file_name path_p = ()*)



