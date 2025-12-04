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
        |x::xs -> if x='/' then None else (aux xs ((acc^String.make 1 x)))
    in aux lst ""

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
  String.split_on_char '/' str (*Si on a deux caractéres '/' consecutifs on a dans la liste des chaines vides, on ne les enléve pas afin que cela nous de nous arrêter dès qu'on a une chaine vide dans la fonction puisque on est sur que le name d'un node n'est pas vide *)

(*?????*)
let rec search lst nm = match lst with
  |[] -> None 
  |x::xs -> begin 
    match x with 
      |Dir d -> if d.name=nm then Some d else search xs nm
      |File _fl-> None
    end

(*Fonctions ajoutés pour la commande write*)

(*concaténe les éléments d'une liste de string, on l'utilise dans le repl pour write, elle nous permet de concaténer tout en vérifiant si la syntaxe est respectée*)
(*let concat sep l_str = 

  (*on vérifie s'il y a '"' au début du texte à ajouter dans un fi*)
  let rec aux acc l nb= 
    if nb = 0 then (
      match l with
        (*on teste si c'est le string de la liste qui commence un caractére '"' est le premier de la liste l_str (puisque acc est vide) et on enlève ce caractère*)
        |x::xs -> if x.[0]=='"' then let x'=String.sub x 1 (String.length x -1) in aux (acc^x') xs (nb+1)
            else (print_endline "write : le texte à écrire dans le fichier doit être entre \"...\" \n syntaxe : write <nomdufichier> \"le contenu du texte à rajouter\""; "")
        |[] -> ""(* cas où l'utilisateur fait write file_name " " avec 0 ou plusieurs espaces, ce cas d'erreur est déja gérer dans le repl *)
    )
    
    else(
      match l with
        (*|x::[] -> let len  = String.length x in 
          if len = 0 then ""
          else if (x.[len -1]='"') then let x'=String.sub x 0 (len -1) in acc^sep^x'
              else begin print_endline "Erreur : syntaxe : write <nomdufichier> \"le contenu du texte à rajouter\" pour le écrire le caractère '\"' entre deux espaces dans le texte vous devez l'échapper avec '\', par exemple \\\"" ;
              "" end (*On retourne la chaîne vide en cas d'erreur*)*)
        |[] -> let len  = String.length acc in if len = 0 then ""
        else if (acc.[len -1]='"') then String.sub acc 0 (len -1)
              else begin print_endline "Erreur : syntaxe : write <nomdufichier> \"le contenu du texte à rajouter\" pour le écrire le caractère '\"' entre deux espaces dans le texte vous devez l'échapper avec '\', par exemple \\\"" ;
              "" end
        |x::xs -> let len = String.length x in begin
          match x.[0],x.[len -1] with 
            |'"',_ -> print_endline "Erreur : syntaxe : write <nomdufichier> \"le contenu du texte à rajouter\" pour le écrire le caractère '\"' entre deux espaces dans le texte vous devez l'échapper avec '\', par exemple \\\"";
                    ""
            (*cas d'échappement de '"' avec le '\'*)
            |'\\',_ -> (
              if len > 1 then (
                if (x.[1]='"') then let x'=String.sub x 1 (String.length x -1) in aux (acc^sep^x') xs (*on enlève le caractère '\' *)
                else aux (acc^sep^x) xs) (nb+1)
              else aux (acc^sep^x) xs (nb+1)) 
            (*cas d'un mot avec un '"' dans le texte pas la fin puis que la liste de mot n'est pas vide, on teste si l'utilisateur l'a bien déspécialiser*)
            |(_,'"')-> if (len >1) then (
                        if (x.[len-2]== '\\') then let x'=String.sub x 0 (String.length x -1) in aux (acc^sep^x') xs (nb+1)
                        else (print_endline "Erreur de syntaxe : write <nomdufichier> \"le contenu du texte à rajouter\"" ;""))
                      else ( print_endline "Erreur : syntaxe : write <nomdufichier> \"le contenu du texte à rajouter\" pour le écrire le caractère '\"' entre deux espaces dans le texte vous devez l'échapper avec '\', par exemple \\\"" ;
                                                  "" )
            |(_,_)-> aux (acc^sep^x) xs (nb+1)
          end)
  in aux "" l_str 0 *)

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
        |[]-> print_string " repertoire est vide"
        | x::xs -> match x with 
                 |Dir _ -> print_string "Rien faire"
                 |File f -> if f.name = node then print_string "Trouver" else comparer node xs

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
        |Dir d ->if d.name = node_name then
              removeBis xs node_name
              else 
                let children_nouveau = removeBis d.children node_name
                in 
                let dir_nouveau = Dir {d with children = children_nouveau} 
                in dir_nouveau :: removeBis xs node_name
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

let copieBis node = 
  let rec  auxCopieBis s_node = match s_node with
    |File f -> File {name = f.name; content = f.content}
    |Dir d -> let enfant_copie = List.map auxCopieBis d.children in 
    Dir{name =d.name; children = enfant_copie} in auxCopieBis node
(*Ajoute le node dans le fs au dossier correspodant à path_p*)
(*let add_node fs path_p node =
  let rec aux c_dir l = 
    match path_p with
      |[] -> let d = cd_current_dir fs path_p in begin
        match d with
          |Some d' -> let dr = estPresentBis (File node) (*vérifier dans le dir_courant et l'ajouer, reconcstruire l'arbre*)
        (*let new_root = {name=fs.root.name; children = c_dir::fs.root.children }
    in {root=new_root; current_path=[]}*)
      |x::xs -> remove fs.current_path x.name*)
