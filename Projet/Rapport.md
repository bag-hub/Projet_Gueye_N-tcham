-----------------------------------------------------------------------------
#Cet documents va nous permettre d'expliquer notre code et donner les precisions sur les cas qu'on na traiter#.
#---- GUEYE BAYE && N'TCHAM KOMINA ROGER ----#

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
"pwd" : 
Elle affiche le chemin absolu du répertoire courant. Elle part toujours de la racine de notre arborescence (notre système de fichier en général).
L'action d'exécution que fait notre commande "PWD" se résume comme suit:
  -On récupère le chemin courant stocké dans le "current_path" du système de fichiers (Ce chemin est une liste des noms représentant les directory successifs depuis la racine). 
  -On transforme cette liste de noms en une chaîne de caractères avec la fonction "path_to_string" représentant le chemin complet qui nous sera retourner dans le terminal. Ce qui garantit que le chemin part de la racine de l'arborescence.

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
"mkdir" :
La commande "mkdir" permet de créer un nouveau directory dans le répertoire courant. Elle vérifie d’abord que le nom choisi n’existe pas déjà, puis le créer dans mon arborescence, le directory si elle n'y est pas déjà.
L'action d'exécution se déroule comme suit :
  -On récupère le répertoire courant grâce à la fonction "cd_current_dir".
  -On vérifie si un élément portant ce nom existe déjà dans ce répertoire avec la fonction "estPresentBis".
  -Si un élément existe déjà, un message d’erreur est générer pour le signaler; sinon le nouveau directory se creer et notre arborescence se met à jours avec le nouveau directory.

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
"ls" : 
Notre commande "ls" nous sert juste à lister le contenu du répertoire courant. On l'utilise juste pour parcourir notre arborescence et visualiser les éléments présent dans notre système de fichier ou dans un quelconques directory de notre arborescence et les afficher. Son action est détaillé comme suit :
  -On récupère le répertoire courant grâce à la fonction "cd_current_dir".
  -On parcourt la liste des enfants de ce répertoire.
  -On affiche successivement les noms de chaque élément présent dans ce répertoire.
En gros on explore notre arbre et on collecte pour affichage les elts de notre arbre.

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
"TOUCH Command" : 
Notre "touch" gère seulement la création de fichier vide. 
Action d'exécution:
  -La commande "touch" commence par vérifier la validité du nom grâce à la fonction "isName". 
  -Si le nom est correct, elle cherche dans le répertoire courant avec la fonction "estPresentBis" pour voir si un élément existant a déjà ce nom. 
  -Si aucun élément n’existe, elle crée un nouveau fichier vide. Sinon, elle affiche un message de retour signalant l’erreur.

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
"WRITE Command" : 

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
"CAT Command" : 

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
