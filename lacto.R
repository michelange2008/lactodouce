# http://regnault.perso.math.cnrs.fr/R_tuto/Intro_modeles_lineaires_mixtes.html#4_la_formalisation_du_mod%C3%A8le
if (file.exists("~/R/Statistiques/lacto")) {
  setwd("~/R/Statistiques/lacto")
}

if (file.exists("~/MichelangeCloud/Statistiques/lacto")) {
  setwd("~/MichelangeCloud/Statistiques/lacto")
}


source("byblos.R") # Chargement des bibliothèques
source("data_src.R") # script pour récupérer les données et produire des fichiers long par hormone


# Synthèse générale des données
synthèse <- lacto_long %>% 
  group_by(hormone) %>% 
  summarise(N=n(), moy=mean(dosage, na.rm=TRUE), med=median(dosage, na.rm=TRUE), ecart_type=sd(dosage, na.rm=TRUE))
# Représentation de l'ensemble des données
ggplot(lacto_long, aes(x = temps, y = dosage, fill = etat)) + geom_boxplot() + facet_wrap(~hormone, scale = "free")
ggplot(prl_long, aes(x = temps, y = dosage, fill = etat)) + geom_boxplot() + geom_jitter(color="black", size = 0.4, alpha=0.8) + facet_wrap(~ferme)
ggplot(pgs_long, aes(x = temps, y = dosage, fill = etat)) + geom_boxplot() + geom_jitter(color="black", size = 0.4, alpha=0.8) + facet_wrap(~ferme)
ggplot(est_long, aes(x = temps, y = dosage, fill = etat)) + geom_boxplot() + geom_jitter(color="black", size = 0.4, alpha=0.8) + facet_wrap(~ferme)
ggplot(prl_long, aes(x = temps, y = dosage, fill = etat)) + geom_boxplot() + geom_jitter(color="black", size = 0.4, alpha=0.8) + scale_fill_manual(values=c("#999999", "#E69F00"))
library(lattice)

# Description des variables avec les regroupements
describeBy(x= prl_long$dosage, group=list(prl_long$etat, prl_long$temps), mat = T)
# Visualisation de ces variables
prl_summary <- function(x) {
  m <- mean(x)
  ymin <- m - sd(x)
  ymax <- m + sd(x)
  return(c(y=m, ymin=ymin, ymax=ymax))
}
p <- ggplot(data = prl_long, aes(x = etat, y=dosage, fill=temps))
p <- p + geom_violin()
p <- p + scale_fill_brewer(palette = "PRGn")
p <- p + theme(legend.position = "right")
p <- p +geom_dotplot(binaxis = "y", stackdir = "center", position = "dodge", dotsize = 1/4)
p <- p + stat_summary(fun.data = prl_summary, geom = "pointrange", color="red", size=0.50, position = position_dodge((0.9)))
p
################################################################################
# Ajustements par le modele mixte en utilisant la fonction lme du package nlme
################################################################################
prl_long_sansNA <- prl_long %>% filter(is.na(dosage) == FALSE)
lme_prl <- lme(dosage~ etat + temps +etat:ferme, data = prl_long_sansNA, random = ~1|ferme, method = "REML", weights = varIdent(form=~1 | etat))

# valeurs extrêmes
residus<-residuals(lme_prl, type="normalized")
prl_long_sansNA$residus <- residus
grubbs.test(prl_long_sansNA$residus, type=10, opposite=FALSE, two.sided=FALSE) # Test des valeurs extrêmes
# Eliminer les outliers
prl_long.clean <- prl_long_sansNA # On crée un fichier copie sur lequel on va enlever les outliers
#########################################################################################
# Elimination des valeurs influentes 
#########################################################################################
data.frame() -> valeur.influentes
while(grubbs.test(prl_long.clean$residus, type = 10, opposite = FALSE, two.sided = FALSE)$p.value < 0.05)
 {
   max <- which.max(abs(prl_long.clean$residus)) # On stocke la valeur maximale dans max
   valeur.influentes <- rbind(valeur.influentes, prl_long.clean[max, ])
   prl_long.clean <- prl_long.clean[ -max, ] # Et on supprime la ligne max
}


lmer_prl <- lmer(dosage ~ etat + temps + etat:temps+(1 | ferme), data = prl_long_sansNA, REML = T)
residuals2 <- residuals(lmer_prl, type="pearson", scaled=T)
prl_long_sansNA$residus2 <- residuals2
grubbs.test(prl_long_sansNA$residus2, type = 10, opposite = FALSE, two.sided = FALSE)

prl_long.clean2<-prl_long_sansNA
data.frame()->valeur.influentes2
while(grubbs.test(prl_long.clean2$residus2, type = 10, opposite = FALSE, two.sided = FALSE)$p.value <0.05)  { 
  max<-which.max(abs(prl_long.clean2$residus2)) #cherche la valeur maximale qu'on stocke dans l'objet max
  # récupère les observations considérées comme influentes et les stocke                                                        
  valeur.influentes2<-rbind(valeur.influentes2,prl_long.clean2[max, ])
  prl_long.clean2<-prl_long.clean2[ -max, ] # supprime la valeur maximale de prl_long.clean
}
valeur.influentes2
###################################################################
# Évaluation des valeurs influentes (valable que pour lmer)
##################################################################
cd2 <- hlm_influence(lmer_prl, level = 1)
# Évaluation des valeurs influentes par la distance de Cook
dotplot_diag(x=cd2$cooksd, cutoff = "internal", name = "cooks.distance", modify = FALSE)
# Évaluation des valeurs influentes par la différence d'ajustement
dotplot_diag(x=cd2$mdffits, cutoff = "internal", name = "mdffits", modify = FALSE)
# Evalutation des valeurs influentes par le ration de covariance
dotplot_diag(x=cd2$covratio, cutoff = "internal", name = "covratio", modify = FALSE)

##################################################################
# VERIFIER LES CONDITIONS D'APPLICATION
#################################################################

# NORMALITE
n1 = shapiro.test(prl_long_sansNA$residus)
n2 = shapiro.test(prl_long.clean$residus)
n3 = shapiro.test(prl_long.clean2$residus)
r<-data.frame(W=c(n1$statistic, n2$statistic, n3$statistic),
              p=c(n1$p.value, n2$p.value, n3$p.value))
dimnames(r)[[1]]<-c("jeu de données complet", "jeu de données nettoyées lme", "jeu de données nettoyées lmer")
kable(pandoc.table(r, style='simple',split.tables=200))
# DISTRIBUTION DES RESIDUS
# Fonction pour tracer la dispersion des résidus et leur normalité
plotresid <- function(datas) {
  pl <- ggplot(datas, aes(x=residus))+geom_histogram(aes(y=..density..))
  pl <- pl + stat_function(fun = dnorm, colour="red", 
                           args = list(mean = mean(datas$residus, na.rm = TRUE),
                                       sd = sd(datas$residus, na.rm = TRUE)))
  pl <- pl + theme(plot.title = element_text(size=12)) + labs(x = "Distribution du résidu")
  print(pl)
}

# Sur les données de base
plotresid(prl_long_sansNA)
# Sur les données nettoyées
plotresid(prl_long.clean) # ça sert à rien car le prl_log.clean est identique au prl_long car problème d'éliminationd des outliers
plotresid(prl_long.clean2)

# Fonction d'affichage des résidus
plotresidlin <- function(datas) {
  p2 <- ggplot(datas, aes(sample=residus)) + stat_qq()
  p2 <- p2 + theme(plot.title = element_text(size=12)) + ggtitle("QQplot")
  print(p2)
}
# Fonction pour tracer la dispersion des résidus et leur normalité
plotresid <- function(datas) {
  pl <- ggplot(datas, aes(x=residus))+geom_histogram(aes(y=..density..))
  pl <- pl + stat_function(fun = dnorm, colour="red", 
                           args = list(mean = mean(datas$residus, na.rm = TRUE),
                                       sd = sd(datas$residus, na.rm = TRUE)))
  pl <- pl + theme(plot.title = element_text(size=12)) + labs(x = "Distribution du résidu")
  print(pl)
}
plotresidlin(prl_long_sansNA)
plotresidlin(prl_long.clean)
plotresidlin(prl_long.clean2)

# Autre façon de tester la normalité uniquement pour lmer
plotindenorm <- function(datas) {
  aleatoires <- lmer(dosage~1+(1|ferme), data = datas)
  pr01 <- profile(aleatoires)
  xyplot(pr01, aspect = 1.3, layout(c(3,1)))
  xyplot(pr01, aspect = 1.3, layout=c(3,1), absVal=T) 
  splom(pr01)
}

plotindenorm(prl_long_sansNA)
plotindenorm(prl_long.clean)
plotindenorm(prl_long.clean2)

################################################################################
# TESTER L'EFFET ALEATOIRE
################################################################################

# AVEC LME
Modele.GLS <- gls(dosage ~ etat + temps + etat:ferme, 
                  data=prl_long_sansNA, method = "REML",
                  weights = varIdent(form = ~1 | etat))
anova(Modele.GLS, lme_prl) # si p-value < 0.05 cela vaut le coup de garder l'effet aléatoire(ferme)

# AVEC LMER
# Le package lme4 est censé avoir une fonction ranova mais apparemment ce n'est pas le cas
ranova(lmer_prl)
dotplot(ranef(lmer_prl, condVar=T))

# Calcul de l'ICC
prl_lme0 <- lme(dosage~1, random=~1|ferme, data=prl_long.clean)
VarCorr(prl_lme0)
icc <- ICC1.lme(dosage, ferme, prl_long_sansNA)
print(icc)
icc1 <- performance::icc(prl_lme0)
print(icc1)
prl_lmer1 <- lmer(dosage ~ etat +(1|ferme), data=prl_long_sansNA, REML = F)
prl_lmer2 <- lmer(dosage ~ temps + (1|ferme), data=prl_long_sansNA, REML = F)
prl_lmer3 <- lmer(dosage ~ temps + etat + etat:temps + (1|ferme), data = prl_long_sansNA, REML = F)
anova(prl_lmer1, prl_lmer2, prl_lmer3)
prl_long$animal <- as.factor(prl_long$animal)
# Modèle linéaire mixte sans mesure répétée
modele0_prl <- lme(dosage~etat*ferme, random = ~1|animal, data = prl_long, method = "ML")
# Spécifier le croisement entre un effet aléatoire (animal) et un effet fixe (etat)
modele1a_prl <- lme(dosage~etat*ferme, random=~1|animal/etat, data=prl_long, method = "ML")
# Spécifier une pente aléatoire: intercept pour chaque modalité du niveau de la variable etat
modele1b_prl <- lme(dosage~etat*ferme, random=~etat|animal, data=prl_long, method = "ML")
anova(modele0_prl, modele1a_prl, modele1b_prl)
interaction.plot(prl_long$etat, prl_long$animal, prl_long$dosage, las = 1, trace.label = "Chèvre", xlab="Etat", ylab="Prolactine")
modele3a_prl <- lme(dosage~etat*ferme, random = ~1|animal/ferme/etat, data = prl_long, method = "ML")
anova(modele0_prl, modele1b_prl, modele3a_prl)
