type name = Name of string
(** Le nom ne contient pas de '/' *)

type path = name list

type file = { 
  name: name; 
  content: string 
  }

type directory = {
   name: name; 
   children: node list
   }
and node = File of file | Dir of directory

type filesystem = { 
  root: directory; 
  current_path: path 
   }

(*fs =
/usr
/texte.txt
mkdir fs nouveau 
fs = /usr
      /texte.txt
      /nouveau*)
(* Les déclarations des deux types suivants sont mutuellement récursifs:
  - le type directory des répertoires est un enregistrement dont le champ children contient une liste de node
  - le type node est un type de données algébrique dont un des constructeurs encapsule une valeur de type directory
*)


let init () =
  let root_dir = { 
    name = Name "/"; children = [] 
    } in
  (* Le répertoire racine est le seul nom de répertoire qui peut contenir un '/' *)
  { 
    root = root_dir; 
    current_path = [] }


let path_to_string path =
  let names = List.map (fun (Name s) -> s) path in
  "/"^ (String.concat "/" names)

(*Cette fonction prends en paramétre le nom d'un dossier ou un fichier et une liste de node(dossier ou fichier) et retourne ce node s'il existe dans la liste et Node sinon*)
(*donné le fs en paramétre pour rechercher la présence du dossier dans le répertoire courant avec current_path*)
let rec estPresentBis node liste = match liste with
                            |[]-> None
                            |x::xs-> begin
                                match x,node with 
                                    |File fl,Name nf-> if fl.name=Name nf then Some (File fl) else estPresentBis node xs
                                    |Dir d,Name nf-> if d.name=Name nf then Some (Dir d) else estPresentBis node xs
                                    end

(*!!!!!!!!!!!!!!!!pas besoin*)
(*let concat a b = match a,b with
                  |Name a,Name b -> Name (a^b)*)

(*fonction qui permet de vérifer si le nom d'un node contient un "/" dans l'utilisation de touch ou mkdir
elle prends en paramétre un string, et retourne None si ce string contient un caractére '/' et le Name de ce string sinon*)
(*??????*)
let isName str = 
  let lst = List.of_seq(String.to_seq str) (*je voulais créer une liste de chaine de caractère du string str*)
    in let rec aux l acc =
       match l with
        |[] -> Some (Name acc)
        |x::xs -> if x='/' then None else (print_endline acc; aux xs ((acc^String.make 1 x)))
    in aux lst ""

(*Cette fonction enleve l'élément e de type de la liste de type 'a list donnés en paramétre s'il e présent et renvoie la nouvelle liste obtenu, la même liste sinon*)
let rec remove l_node node_name = match l_node with
    |[] -> []
    |x::xs -> begin
      match x with
        |File fl -> if fl.name=node_name then xs else File fl::(remove xs node_name)
        |Dir _d -> x::(remove xs node_name)
      end


(*?????*)
let split_sh str =
  List.map (fun x -> Name x) (String.split_on_char '/' str) (*Si on a deux caractéres '/' consecutifs on a dans la liste des chaines vides, on ne les enléve pas afin que cela nous de nous arrêter dès qu'on a une chaine vide dans la fonction puisque on est sur que le name d'un node n'est pas vide *)

let rec search lst nm = match lst with
  |[] -> None 
  |x::xs -> begin 
    match x with 
      |Dir d -> if d.name=nm then Some d else search xs nm
      |File _fl-> None
    end