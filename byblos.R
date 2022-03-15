# chargment des bibliothèques 

## First specify the packages of interest
packages = c(
  "dplyr", 
  "tidyverse", 
  "ggplot2", 
  "ggpubr", 
  "hrbrthemes", "viridis", 
  "readxl", 
  "forcats",
  "nlme",
  "lme4",
  "lmerTest",
  "HLMdiag", 
  "rstatix", 
  "sjstats",
  "outliers", 
  "knitr", 
  "pander", 
  "psych"
  )


## Now load or install&load all
package.check <- lapply(packages, 
                        FUN = function(x) { 
                          if (!require(x, character.only = TRUE)) {
                            install.packages(x, dependencies = TRUE)
                            library(x, character.only = TRUE)
                          }
                        }
)

# "ggpubr", # amélioration des graphiques
# "hrbrthemes", "viridis", # Mise en page des graphiques
# "readxl", # Lecture des fichier excell
# "forcats", # Réarrangement des données de type factors
# "nlme", # Modèle linéraire mixte
# "lme4", # Modèle linéaire mixte
# "lmerTest", # Modèle linéaire mixte (fonction lmer modifie celle de lme4 )
# "HLMdiag", # ajustement des modèles mixtes (notamment si un facteur à un seul niveau) (case_delete)
# "psych", describe by
# "rstatix", # Tests statistiques de base: ANOVA, Krsulkal-Wallis, etc.
# "outliers", # recherche les outliers par la méthode du chi2 (grubbTest)
# "knitr", # outils pour rapport dynamiques
# "pander", # cf Pandoc markdown ?
# "psychometric", # (???)
# "sjstats", # fonctions statistiques non pourvues par d'autres packages