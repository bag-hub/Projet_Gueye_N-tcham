-----------------------------------------------------------------------------
#Cet documents va nous permettre d'expliquer notre code et donner les precisions sur les cas qu'on na traiter#.

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
"PWD Command" : 

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
"MKDIR COMMAND" :

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
"LS Command" : 

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
"CAT Command" : 

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
"WRITE Command" : 

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
"FIND Command" : 

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
"CP Command" : 

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
"MV Command" : 
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
"FIND Command" : 
La commande find que nous avons implémentée permet de rechercher un élément depuis la racine de notre repertoire courant (de l’arborescence du système de fichiers)
  Etant donné que find commence depuis le repertoire racine elle parcourt recursivement la liste des enfants de chaque répertoire (Elle utilise la fonction comparer pour verifier si le nom qu'on recherche se trouve  dans notre repertoire) et :
     * Si l’élément est un fichier et que son nom correspond à celui recherché, le chemin absolu est affiché de la racine jusqu'à cet elemnt
     * Si l’élément est un dossier et que son nom correspond, le chemin absolu est également affiché (extension que nous avons choisie que nous avons pour cette commande)de la meme maniere.
     * Sinon, la fonction continue la recherche dans les sous-répertoires jusqu'a ce qu'il le trouve.... Si elle ne trouve pas , elle ne retourne rien (None Option).
  La fonction affiche tous les chemins absolus qui correspondent à l'elts qu'on cherche et qui ont été trouvés.
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
"RM Command"
La commande rm permet de supprimer un élément du répertoire courant.
Étapes d'action :
    On récupère le répertoire courant grâce à cd_current_dir.
    On vérifie si un élément portant le nom donné existe dans ce répertoire avec estPresentBis.
    Si aucun élément n’est trouvé, un message d’erreur est affiché.
    Si l’élément est trouvé :
        La fonction removeBis est utilisée pour supprimer ce fichier ou ce dossier de la liste des enfantsdu repeertoire courant
        Un nouveau répertoire est reconstruit avec cette liste mise à jour sans l'element supprimer nafin de reajuster l'arborescence de notre systeme de fichier.
        La fonction replace_dir remonte la modification dans toute l’arborescence afin de mettre à jour la racine du système de fichiers.

