(*(*mkdir permet de vérifier si un fichier  est dans un des repertoire de filesystem, Si c'est le cas, elle renvoi la confirmation ... Sinon elle creer ce element dans le directory de filesystem*)


(*Module de base défini par filesystem.ml est Filesystem*)






open Filesystem 

(*fonction récursive pour  qui vérifie si un élts précis est présent dans notre liste*)
let rec estPresent present maliste = match maliste with 
       |[] -> false 
       | x::y -> if present x then true 
                   else estPresent present y;;

(* Recupere les champs nom et chidren du repertoir courant qui est filesystem.ml et créer un nouveau repertoire rep_creer*)

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

(*touch Création de fichier vide dans le repertoir courant filesystem*)
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



let ls(fs filesystem) filesystem = 
    let rec ls_fich
*)
              