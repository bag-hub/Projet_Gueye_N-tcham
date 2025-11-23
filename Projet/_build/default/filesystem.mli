type name = Name of string
(** Le nom ne contient pas de '/' *)

type path = name list

type file = { name: name; content: string }

type directory = { name: name; children: node list }
and node = File of file | Dir of directory

type filesystem = { root: directory; current_path: path }


val path_to_string : path -> string

val init : unit -> filesystem

(*fonction qui permet de vérifer si le nom d'un node dans l'utilisation de touch ou makdir contient un "/"*)
val isName : string -> name option

(*Cette fonction enleve l'élément e de type de la liste de type 'a list donnés en paramétre s'il e présent et renvoie la nouvelle liste obtenu, la même liste sinon*)
val remove : node list -> name -> node list