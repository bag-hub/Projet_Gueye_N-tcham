type name = Name of string
(** Le nom ne contient pas de '/' *)

type path = name list

type file = { name: name; content: string }

type directory = { name: name; children: node list }
and node = File of file | Dir of directory

type filesystem = { root: directory; current_path: path }

val init : unit -> filesystem

val path_to_string : path -> string

(*Cette fonction prends en paramétre le nom d'un dossier ou un fichier et une liste de node(dossier ou fichier) et retourne ce node s'il existe dans la liste et Node sinon*)
val estPresentBis : name -> node list -> node option


(*fonction qui permet de vérifer si le nom d'un node contient un "/" dans l'utilisation de touch ou mkdir
elle prends en paramétre un string, et retourne None si ce string contient un caractére '/' et le Name de ce string sinon*)
val isName : string -> name option

(*Cette fonction enleve le node de nom name de la liste de type node list donnés en paramétre s'il e présent et renvoie la nouvelle liste obtenu, la même liste sinon*)
val remove : node list -> name -> node list

(*permet de donné une liste de name à partir d'un string en décomposant avec le caractère '\' , elle est utilisée dans cd*)
val split_sh : string -> string list

(*????*)
val search : node list -> name -> directory option

(*On a choisi de définir une fonction concat pour vérifier si on veux mettre des '"' dans le texte de la commande write est ce qu'on les a bien déspécilisé ce caractère*)
val concat : string -> string list -> string

(*Cette fonction permet de se déplacer dans le dossier qui correspond au current_path du filesystem, comme un parcours de l'arbre vers un noeud interne correspond
On l'utilise dans repl pour le cas de mkdir, touch,...*)
val cd_current_dir : filesystem -> path -> directory option

(*cette fonction permet de supprimer le dernier dernier éléent d'une liste est renvoie la liste restant, on l'utilise dans cd pour '..' dans le chemin*)
val removeLast : path -> path

(*cette fonction permet de supprimer le dernier dernier éléent d'une liste est renvoie la liste restant, on l'utilise dans cd pour '..' dans le chemin*)
val comparer: name -> node list -> unit

(*cette fonction permet de supprimer le dernier dernier éléent d'une liste est renvoie la liste restant, on l'utilise dans cd pour '..' dans le chemin*)
val removeBis: node list -> name -> node list

(* Cette fonction va me permettre de le mettre à la bonne place dan,s l'arborescence*)
val replace_dir : directory -> name list -> directory -> directory

(*cette fonction copieBis permet de copier un fichier avec le meme contebu et dans le cas d'un dossier , recopie toute l'arborescence de ce node.... genre duplique le dossier avec les fichier à l'intériur*)
val copieBis : node -> node




(*(*Ajoute le node dans le fs au dossier correspodant à path_p*)
val add_node : filesystem -> path -> node -> filesystem*)

(*(*Ajoute node au directory donné en argument en suivant le chemin relatif path*)
val add_dir : directory -> path -> node -> directory*)