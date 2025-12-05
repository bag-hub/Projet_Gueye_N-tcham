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
let rec estPresentBis node_name liste = 
  match liste with
    |[]-> None
    |x::xs-> begin
        match x with 
            |File fl -> if fl.name=node_name then Some (File fl) else estPresentBis node_name xs
            |Dir d -> if d.name=node_name then Some (Dir d) else estPresentBis node_name xs
            end


(*tester si le un string est un name pour vérifier si le nom d'un node que l'on veux créer est valide*)
let isName str = 
  let lst = List.of_seq(String.to_seq str) (*je voulais créer une liste de chaine de caractère du string str*)
    in let rec aux l acc =
       match l with
        |[] -> Some (Name acc)
        |x::xs -> if x='/' then None else (aux xs ((acc^String.make 1 x)))
    in aux lst ""

(*conversion d'un name en un string, utilisée dans touch*)
let name_to_string name_p = match name_p with |Name str -> str

(*Cette fonction enleve l'élément e de type de la liste de type 'a list donnés en paramétre s'il e présent et renvoie la nouvelle liste obtenu, la même liste sinon*)
(*On peut l'utiliser dans mv*)
let rec remove l_node node_name = match l_node with
    |[] -> []
    |x::xs -> begin
      match x with
        |File fl -> if fl.name=node_name then xs else File fl::(remove xs node_name)
        |Dir _d -> x::(remove xs node_name)
      end


(*permet de donné une liste de name à partir d'un string en décomposant avec le caractère '\' , elle est utilisée dans cd, elle peur servir pour le cas du chemin relatif*)
let split_sh str =
  let l_str = String.split_on_char '/' str 
 in List.filter (fun t -> t <> "") l_str(*on supprime les chînes vide de la liste *)

(*Fonctions ajoutés pour la commande write*)

(*concaténe les éléments d'une liste de string, on l'utilise dans le repl pour write, elle nous permet de concaténer tout en vérifiant si la syntaxe est respectée*)

let concat sep l_str = 
  let rec aux acc l nb = match l with
    |[] -> let len_s = String.length acc in
      if((len_s=0) && (not(nb=0))) then (print_endline "Erreur : syntaxe : write <nomdufichier> \"le contenu du texte à rajouter\" " ; "")(*ca cas correspond à lorsque l'ulisateur ne met pas un guillement fermant*)
      else acc
  
    |x::xs -> let len = String.length x in
      if nb = 0 then (
        if x.[0] = '"' then let x' = String.sub x 1 (len-1) in aux x' xs 1
        else (print_endline "Erreur : syntaxe : write <nomdufichier> \"le contenu du texte à rajouter\" " ; ""))
      else if (xs = []) then (
        if (x.[len-1]='"') then let x' = String.sub x 0 (len-1) in acc^sep^x'
        else (print_endline "Erreur : syntaxe : write <nomdufichier> \"le contenu du texte à rajouter\", merci de mettre le texte entre \"\" " ; "")
      )
      else aux (acc^sep^x) xs (nb+1)
  in aux "" l_str 0

(*Cette fonction permet de se déplacer dans le dossier qui correspond au current_path du filesystem, comme un auxParcourir de l'arbre vers un noeud interne correspond
On l'utilise dans repl pour le cas de mkdir, touch,...*)
let cd_current_dir fs path_p = 
  let rec aux l dir = 
    match l with
      |[] -> Some dir
      |x::xs ->(
        match estPresentBis x dir.children with
          |None -> None
          |Some node ->( match node with 
            |Dir d -> aux xs d 
            |File _f -> None )
      )
  in aux path_p fs.root 


(*cette fonction permet de supprimer le dernier dernier éléent d'une liste est renvoie la liste restant, on l'utilise dans cd pour '..' dans le chemin*)
let removeLast p:path = 
  let rec aux l acc = match l with 
    |[] -> [] (* cas de liste vide*)
    |_::[] -> acc (* on enlève le dernier élément de liste*)
    |x::xs -> aux xs (x::acc)
in List.rev( aux p [] )

(*Ajoute le node dans le fs au dossier correspodant à path_p*)
(*let add_node fs path_p = 
  let aux*)
(*----1----*)
(*qui sera utiliser dans le find*)
(*1 / Comparer le nom de fichier auquels je veux appliquer find au fichier deja dans mon repertoire...*)
let rec comparer node liste= match liste with
        |[]-> false
        | x::xs -> match x with 
                 |Dir _ -> comparer node xs
                 |File f -> if f.name = node 
                  then
                    true else comparer node xs

(*Pour le rm*)
(*RemoveBis va nous permettre de supprimer un fichier si on le trouve dans le 
repertoire courrant et de retourner un repertoire modifier ... Dans le cas ou c'est dans un sous dossier, on aura la meme action...
Si l'elet qu'on veux supprimer est un dossier, tout le dossier sera supprimer d'un coup*)
let rec removeBis l_node node_name = match l_node with
    |[] -> []
    |x::xs -> begin
      match x with
        |File fl -> if fl.name=node_name then 
            removeBis xs node_name
            else 
              File fl::removeBis xs node_name
        |Dir d ->
              if d.name = node_name then
                removeBis xs node_name 
              else 
                x :: removeBis xs node_name 
      end

(*Cette fonction va me permettre de remonter la modofocation ou la suppression d'un elet à toute l'arborescence que j'ai construit dans rm*)
let rec replace_dir dir path new_dir =
  match path with
  | [] -> new_dir
  | name :: rest ->
      let rec auxParcourir enfants =
        match enfants with
        | [] -> []
        | x :: xs ->
            match x with
            | Dir d ->
                if d.name = name then
                  let d_remplace = replace_dir d rest new_dir in
                  Dir d_remplace :: auxParcourir xs
                else
                  x :: auxParcourir xs
            | File _ -> x :: auxParcourir xs
      in
      { dir with children = auxParcourir dir.children }


      (*cette fonction copieBis permet de copier un fichier avec le meme contebu et dans le cas d'un dossier , recopie toute l'arborescence de ce node.... genre duplique le dossier avec les fichier à l'intériur*)
let copieBis node = 
  let rec  auxCopieBis s_node = match s_node with
    |File f -> File {name = f.name; content = f.content}
    |Dir d -> let enfant_copie = List.map auxCopieBis d.children 
      in 
         Dir{name =d.name; children = enfant_copie} 
        in 
        auxCopieBis node

(*Ajoute node au directory donné en argument en suivant le chemin relatif path*)
let rec add_to_dir path_p dir_p node_p =
  match path_p with 
    |[] -> {name=dir_p.name; children=node_p::(dir_p.children)}
    |x::xs -> 
      let new_children = List.map (fun node ->
        match node with 
          |Dir d -> if d.name=x then Dir (add_to_dir xs d node_p)
                else node
          |File _ ->  node) dir_p.children 
      in {name=dir_p.name; children=new_children}



(*Une fonction qui me pert de retourner le dernier elt*)
let rec dernier_elt liste = 
     match liste with 
        |[]-> None
        |x::[] -> Some x
        |_::xs -> dernier_elt xs
              

(*Cette fonction sert à supprimer un node qui est au repertoire correspond à path dans le filesystem*)
let delete_node node_name path fs =
  match cd_current_dir fs path with
  | None ->
      print_endline "rm: Le répertoire parent spécifié n'existe pas ou est invalide";
      fs
  | Some dir_cible -> begin
      match estPresentBis node_name dir_cible.children with
          | None -> 
              print_endline ("rm: L'élément \"" ^ (name_to_string node_name) ^ "\" est introuvable dans le répertoire cible."); 
              fs
          | Some _elt -> let new_children = removeBis dir_cible.children node_name in let dir_nouveau = {name = dir_cible.name; children = new_children} 
                in let nouveau_root = replace_dir fs.root path dir_nouveau in {fs with root = nouveau_root}
        end
    