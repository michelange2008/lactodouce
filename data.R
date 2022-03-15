if (file.exists("~/R/Statistiques/lacto")) {
  setwd("~/R/Statistiques/lacto")
}

if (file.exists("~/MichelangeCloud/Statistiques/lacto")) {
  setwd("~/MichelangeCloud/Statistiques/lacto")
}


source("byblos.R") # Chargement des bibliothèques
source("data_src.R") # script pour récupérer les données et produire des fichiers long par hormone

fermes <- levels(lacto_long$ferme)

dosages <- function(hrm, tps = NULL, farm = NULL) {
  if (tps == "tous") {
    if (is.null(farm)) {
      datan <- lacto_long %>% dplyr::filter(hormone == hrm)
      p <- ggplot(datan, aes(x = temps, y = dosage, fill = etat)) + geom_boxplot() + geom_jitter(color="black", size = 0.4, alpha=0.8)
    }
    else {
      datan <- lacto_long %>% dplyr::filter(hormone == hrm, ferme == farm )
    }
  }
  else {
    if (is.null(farm)) {
      datan <- lacto_long %>% dplyr::filter(hormone == hrm, temps == tps)
      p <- ggplot(datan, aes(x = temps, y = dosage, fill = etat)) + geom_boxplot() + geom_jitter(color="black", size = 0.4, alpha=0.8)
    }
    else {
      datan <- lacto_long %>% dplyr::filter(hormone == hrm, temps == tps, ferme == farm )
    }
    
  }
  p <- ggplot(datan, aes(x = temps, y = dosage, fill = etat)) + geom_boxplot() + geom_jitter(color="black", size = 0.4, alpha=0.8) + facet_wrap(~ferme)
  return(p)
}

# datas <- dosages("prl", tps = "t1")
# datas
