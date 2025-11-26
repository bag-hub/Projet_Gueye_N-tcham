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
let concat a b = match a,b with
                  |Name a,Name b -> Name (a^b)

(*fonction qui permet de vérifer si le nom d'un node dans l'utilisation de touch ou makdir contient un "/"*)
(*??????*)
let isName str = 
  let liste = String.split_on_char ' ' str 
    in let rec aux l acc = match l,acc with
        |[],(Some Name acc) ->Some (Name acc)
        |x::xs,Some (Name acc) -> (match x with 
                                    |Some (Name x)-> aux xs (Some (concat (Name acc) (Name x)))
                                    |_-> None)
        |_->None
      in aux (List.map (fun x->(Some (Name x))) liste) (Some (Name ""))

(*Cette fonction enleve l'élément e de type de la liste de type 'a list donnés en paramétre s'il e présent et renvoie la nouvelle liste obtenu, la même liste sinon*)
let rec remove l_node node_name = match l_node with
    |[] -> []
    |x::xs -> begin
      match x with
        |File fl -> if fl.name=node_name then xs else File fl::(remove xs node_name)
        |Dir _d -> x::(remove xs node_name)
      end
let split_sh (s : string) : string list =
  let str_l = String.split_on_char '/' s in
  List.filter (fun t -> t <> "") str_l

let rec search lst nm = match lst with
  |[] -> None 
  |x::xs -> begin 
    match x with 
      |Dir d -> if d.name=nm then Some d else search xs nm
      |File _fl-> None
    end