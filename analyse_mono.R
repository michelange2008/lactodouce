if (file.exists("~/R/Statistiques/lacto")) {
  setwd("~/R/Statistiques/lacto")
}

source("byblos.R") # Chargement des bibliothèques
source("data_src.R")
##############################
# CREATION DE FICHIERS AVEC UNIQUEMENT LE TEMPS INDIQUÉ CI-DESSOUS ET SANS LES ANIMAUX A ETAT INDÉTERMINÉ
temps <- "t2"
prl_t <- prl_long[(prl_long$temps == temps & prl_long$etat != "indetermine"),]
pgs_t <- pgs_long[(pgs_long$temps == temps & pgs_long$etat != "indetermine"),]
est_t <- est_long[(est_long$temps == temps & est_long$etat != "indetermine"),]

#############################
# CONCATENATION DES TROIS FICHIERS
prl_t_l <- prl_t %>%  dplyr::select(animal, ferme, etat, dosage) # on ne garde que les colonne animal, ferme, etat, dosage
colnames(prl_t_l)[colnames(prl_t_l) == "dosage"] <- "Prolactine" # On remplace le titre de colonne dosage par le nom de l'hormone
pgs_t_l <- pgs_t %>% dplyr::select(dosage) # On ne garde que la colonne dosage
colnames(pgs_t_l) <- c("Progestérone") # On remplace le titre de colonne dosage par le nom de l'hormone
est_t_l <- est_t %>% dplyr::select(dosage) # On ne garde que la colonne dosage
colnames(est_t_l) <- c("Estradiol") # On remplace le titre de colonne dosage par le nom de l'hormone
hrm_t <- cbind(prl_t_l, pgs_t_l, est_t_l)
rm(prl_t_l, pgs_t_l, est_t_l) # suppression des fichiers temporaires

###########################
# suppression de la valeur manquante sur toute la ligne
hrm_t <- subset(hrm_t, hrm_t$animal != 90064)
# Suppression des valeurs manquantes par hormone
prl_t_na <- prl_t[is.na(prl_t$dosage) == F,]
pgs_t_na <- pgs_t[is.na(pgs_t$dosage) == F,]
est_t_na <- est_t[is.na(pgs_t$dosage) == F,]

hormones = colnames(hrm_t[4:6])

plot_par_hormone <- function(hormone) {
  plt <- ggplot(hrm_t, aes_string(x="ferme", y=hormone, fill="etat")) + 
    geom_boxplot() +
    facet_wrap(~ferme, scale="free") + ggtitle(hormone) +
    labs(title=hormone, subtitle = "Différences selon les éleveurs") +
    scale_fill_brewer(type = "qual", palette = "Dark2")
  
  print(plt)
}
for (h in hormones) {
  plot_par_hormone(h)  
}

# Modèle linéaire sans facteur aléatoire
lm_prl <- lm(Prolactine~ etat, data = hrm_t)
anova(lm_prl)
summary(lm_prl)
lm_pgs <- lm(Progestérone~ etat, data = hrm_t)
anova(lm_pgs)
summary(lm_pgs)
lm_est <- lm(Estradiol~ etat, data = hrm_t)
anova(lm_est)
summary(lm_est)

# idem avec la fonction GLS
gls_prl <- gls(dosage ~ etat , data = prl_t_na, method = "REML", weights = varIdent(form = ~1 | etat) )
anova(gls_prl)
gls_pgs <- gls(dosage ~ etat , data = pgs_t_na, method = "REML", weights = varIdent(form = ~1 | etat) )
anova(gls_pgs)
gls_est <- gls(dosage ~ etat , data = est_t_na, method = "REML", weights = varIdent(form = ~1 | etat) )
anova(gls_est)

# Modèle avec facteur aléatoire avec fonction lme
lme_prl <- lme(dosage~ etat , data = prl_t_na, random = ~1|ferme, method = "REML", weights = varIdent(form = ~1 | etat))
gls_prl <- gls(dosage ~ etat, data = prl_t_na, method = "REML", weights = varIdent(form = ~1 | etat) )
anova(gls_prl, lme_prl)
anova(lme_prl)
summary(lme_prl)

lme_pgs <- lme(dosage ~ etat , data = pgs_t_na, random = ~1|ferme, method = "REML", weights = varIdent(form = ~1 | etat))
anova(lme_pgs)
summary(lme_pgs)
lme_est <- lme(dosage ~ etat , data = est_t_na, random = ~1|ferme, method = "REML", weights = varIdent(form = ~1 | etat))
anova(lme_est)
summary(lme_est)

anova(gls_pgs, lme_pgs)
anova(gls_est, lme_est)
# Modèle avec facteur aléatoire avec la fonction lmer
lmer_out <- lmer(Prolactine ~ etat + (1 | etat), data = hrm_t, REML = T, na.action = na.omit())

lme_prl <- lme(dosage~ etat + ferme , data = prl_t_na, random = ~1|ferme, method = "REML", weights = varIdent(form = ~1 | etat))
anova(gls_prl, lme_prl)
summary(lme_prl)

gls_prl_id <- gls(dosage ~ etat + ferme , data = prl_t_na, method = "REML", weights = varIdent(form = ~1 | etat) )
lme_prl_id <- lme(dosage~ etat + ferme , data = prl_t_na, random = ~1|animal, method = "REML", weights = varIdent(form = ~1 | etat))
anova(gls_prl, lme_prl)



# Kruskal Wallis par hormone

prl.kruskal <- prl_t %>% kruskal_test(dosage~etat)
pgs.kruskal <- pgs_t %>% kruskal_test(dosage~etat)
est.kruskal <- est_t %>% kruskal_test(dosage~etat)
# Analyse exploratoire
donnees <- list(prl_t, est_t, pgs_t)

expl <- function(data) {
  library(DataExplorer)  
  introduce(data)
  plot_intro(data)
  plot_str(data)
  plot_missing(data)
  profile_missing(data)
  plot_bar(data, by="etat")
  plot_histogram(data)
  plot_qq(data, by="etat")
}

for (donnee in donnees) {
  expl(donnee)
}

expl(hrm_t)



