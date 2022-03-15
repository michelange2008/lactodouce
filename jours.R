# Script destiné à gérer les dosages en fonction des jours de mise-bas et de stimulation
source("byblos.R")
source("data_src.R")

# liste hormones
abbreviation <- c('prl', 'est', 'pgs')
nom_long <- c("Prolactine", "Progestérone", "Estradiol")
nom_tres_long <- c("la prolactinémie", "la progestéronémie", "l'estradiolémie")
hormones <- data.frame(nom_long, abbreviation, nom_tres_long)

#liste choix
choix <- c("gestantes en lactation", "vides en production", "vides sans lait")

# Construction d'un fichier long
tab_t1 <- lacto_jours %>% dplyr::select(-t2, -t3, - JT2, -JT3) %>% rename("dosage" = t1, "jours" = JT1)
tab_t2 <- lacto_jours %>% dplyr::select(-t1, -t3, - JT1, -JT3) %>% rename("dosage" = t2, "jours" = JT2)
tab_t3 <- lacto_jours %>% dplyr::select(-t2, -t1, - JT2, -JT1) %>% rename("dosage" = t3, "jours" = JT3)
lacto_jours_long <- rbind(tab_t1, tab_t2, tab_t3)
rm(tab_t1, tab_t2, tab_t3)

# Fonction de construction du graphique
p <- function(h_num, gestlait = "lait", methode = NULL) {
  data = lacto_jours_long %>% dplyr::filter(lacto_jours_long$hormone == hormones$abbreviation[h_num])
  if(gestlait == "état") {
    plot <- ggplot(data, aes(x=jours, y=dosage, colour=etat))
  }
  else {
    plot <- ggplot(data, aes(x=jours, y=dosage, colour=lait))
  }
  plot <- plot + geom_point() + theme(legend.position = "none") +
    labs(title = paste("Evolution de",hormones$nom_tres_long[h_num] , "avant et après la mise-bas"), 
         x = "Jours", y = hormones$nom_long[h_num], colour = "Situation physiologique",
         subtitle = "Entre chèvres gestantes et non-gestantes") + 
    theme(legend.background = element_rect(fill = "gray93"),
          legend.position = c(0.15, 0.82))
  if(is.null(methode) == FALSE) {
    plot <- plot + geom_smooth(method = methode)
  }
  return(plot)
}

p1 <- p(2)
p1


