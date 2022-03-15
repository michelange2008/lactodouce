# IMPORT FICHIER lacto.xlsx avec boucle pour les différentes feuilles du fichier
if (file.exists("~/R/Statistiques/lacto")) {
  setwd("~/R/Statistiques/lacto")
}

if (file.exists("~/MichelangeCloud/Statistiques/lacto")) {
  setwd("~/MichelangeCloud/Statistiques/lacto")
}



lacto <- NULL # on crée un data-frame global vide
sheet_names <- excel_sheets("lacto.xlsx") # On récupère la liste des feuilles
nb_sheets <- length(sheet_names) # On stocke le nombre de feuilles
hormones <- c("prl", "pgs", "est")
for(i in 1:nb_sheets){
  if(sheet_names[i]%in%hormones) { # Cette boucle est destinée à ne pas prendre en compte les autres feuilles
    name <- sheet_names[i] # On récupère le nom de la feuille
    data <- read_excel("lacto.xlsx", sheet = i) # lecture et stockage
    data$animal <- as.character(data$animal)
    data$animal <- factor(data$animal) # On transforme les id des animaux en facteurs
    data$hormone <- name # ajout d'une variable avec le nom original de la feuille
    data$ferme <- factor(data$ferme) # On convertit les valeurs en facteurs
    data$etat <- factor(data$etat)
    data$hormone <- factor(data$hormone)
    data$lait <- factor(data$lait)
    data <- data %>% dplyr::select(-num_ferme) # On supprime la colonne num_ferme
    lacto <- bind_rows(lacto, data)
    assign(name, data) # On nomme le data-frame qui vient d'être créé avec le nom de la sheet
    rm(name, data) # on efface les données intermédiaires
  }
}

# Suppression des colonnes inutiles
col_to_del <- c("t4", "JT1", "JT2", "JT3", "JT4")
lacto_dosage <- lacto %>% dplyr::select(-all_of(col_to_del))
# Création d'un fichier avec les valeurs quantitatives (dosages + nb de jours)
col_to_del <- c("t4", "JT4")
lacto_jours <- lacto %>% dplyr::select(-all_of(col_to_del))

# Création du fichier long à partir du fichier vide
lacto_long <- lacto_dosage %>% gather(temps, dosage, -hormone, -ferme, -etat, -animal, -lait)
lacto_long$hormone <- factor(lacto_long$hormone)
prl_long <- lacto_long %>% filter(hormone == "prl")
pgs_long <- lacto_long %>% filter(hormone == "pgs")
est_long <-lacto_long %>% filter(hormone == "est")

