(*Module de base défini par filesystem.ml est Filesystem*)
open Filesystem 

let rec estPresent present maliste = match maliste witl 
       |[] -> false 
       | x::y -> if (present x) then true 
                   else (present y);;

(* Recupere les champs nom et chidren du repertoir courant qui est filesystem.ml*)

let mkdir (fs : filesystem) (rep_creer : name) : filesystem =  
    let rec add_dir  dir = match dir with
        |{name; children} -> let verif_present =  estPresent(function
            |Dir d when d.name = rep_creer -> true)
            |File f when f.name = rep_creer -> true
            |_ -> false) children in 

            if (verif_present) then 
                failwith "Erreur : Un element a déja ce nom"
            else 
                let new_rep = Dir {
                    name = rep_creer; children = []
                } in {dir with children = new_rep :: children} in
                {fs with root = add_dir fs.root}