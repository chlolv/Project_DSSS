#-----------------------------------------------------#
#
#     Project in Data Science for Social Sciences
#       Descriptive statistics for the computation of and indicator of satisfaction
#
#                               Chloe Lavest & Yasmine Houri
#                               Academic year 2021-2022
#
#-----------------------------------------------------#

<<<<<<< HEAD
# Import packages
library("haven")
library("plyr")
library("dplyr")
library("stringr")
library("purrr")
library("foreign")
library("data.table")
library("gtsummary")
library(conflicted)


conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("mutate", "dplyr")

# Chargement des données
# Import data
# Yasmine
# setwd("C:\\Users\\HP\\Documents\\cours_ensae\\3A\\Projet DSSS\\gardiens_paix")
# path <- "C:/Users/HP/Documents/cours_ensae/3A/Projet DSSS/gardiens_paix"

# Chloé
setwd("C:\\Users\\chloe\\OneDrive\\Bureau\\3A\\Projet DSSS")
path <- "gardiens_paix"

=======
source("0_settings.R")
>>>>>>> origin/new_scripts

df_spss <- readRDS("df_spss")
df_sav <- readRDS("df_sav")

# Identification des variables de satisfaction
vars_satisf <- c("AUTCHOI","EMPLMUN","COMAUT","MOTIFAU","CONSENT","RAISCONS",
                 "PORTUNI","IDMET","IDAPN","METIER","BIENVU","FORMCOM",
                 "BONFORM","PLUSTIR","PLUSSOC","PLUSADM", "PLUSTEC", 
                 "PLUSHUM", "PLUSJUR", "PLUSRAP")

# Distribution a plat des variables de satisfaction (labels explicites)
df_spss %>% tbl_summary(
  include = all_of(vars_satisf),
  statistic = list(all_continuous() ~ "{mean} ({sd})",
                   all_categorical() ~ "{p}% ({n})"),
  label = list(AUTCHOI ~ "Aurait choisi un autre emploi",
               EMPLMUN ~ "Eventualité emploi police privée",
               COMAUT ~ "Métier comme un autre",
               MOTIFAU ~ "Motif métier comme un autre",
               CONSENT ~ "Conseillerait d'entrer dans la police",
               RAISCONS ~ "Motif du conseil",
               PORTUNI ~ "Opinion port de l'uniforme",
               IDMET ~ "Evolution idée du futur métier",
               IDAPN ~ "Evolution idée de la Police Nationale",
               METIER ~ "Idée du métier par rapport aux représentations",
               BIENVU ~ "Considération et avantages",
               FORMCOM ~ "Opinion sur tronc commun",
               BONFORM ~ "Bien préparé aux contextes et aux publics",
               PLUSTIR ~ "Aurait souhaité plus de tir",
               PLUSSOC ~ "Aurait souhaité plus de connaissance de la société",
               PLUSADM ~ "Aurait souhaité plus de connaissances administratives",
               PLUSTEC ~ "Aurait souhaité plus de techniques policières",
               PLUSHUM ~ "Aurait souhaité plus de formation aux relations humaines",
               PLUSJUR ~ "Aurait souhaité plus de connaissances juridiques",
               PLUSRAP ~ "Aurait souhaité plus d'expression écrite")) %>%
  as_gt() %>%
  gt::gtsave(filename = paste0("Code/satisfaction","/satisfaction_spss.html"))

# Distribution a plat des variables de satisfaction (labels numeriques)
df_sav %>% tbl_summary(
  include = all_of(vars_satisf),
  statistic = list(all_continuous() ~ "{mean} ({sd})",
                   all_categorical() ~ "{p}% ({n})"),
  label = list(AUTCHOI ~ "Aurait choisi un autre emploi",
               EMPLMUN ~ "Eventualité emploi police privée",
               COMAUT ~ "Métier comme un autre",
               MOTIFAU ~ "Motif métier comme un autre",
               CONSENT ~ "Conseillerait d'entrer dans la police",
               RAISCONS ~ "Motif du conseil",
               PORTUNI ~ "Opinion port de l'uniforme",
               IDMET ~ "Evolution idée du futur métier",
               IDAPN ~ "Evolution idée de la Police Nationale",
               METIER ~ "Idée du métier par rapport aux représentations",
               BIENVU ~ "Considération et avantages",
               FORMCOM ~ "Opinion sur tronc commun",
               BONFORM ~ "Bien préparé aux contextes et aux publics",
               PLUSTIR ~ "Aurait souhaité plus de tir",
               PLUSSOC ~ "Aurait souhaité plus de connaissance de la société",
               PLUSADM ~ "Aurait souhaité plus de connaissances administratives",
               PLUSTEC ~ "Aurait souhaité plus de techniques policières",
               PLUSHUM ~ "Aurait souhaité plus de formation aux relations humaines",
               PLUSJUR ~ "Aurait souhaité plus de connaissances juridiques",
               PLUSRAP ~ "Aurait souhaité plus d'expression écrite")) %>%
  as_gt() %>%
  gt::gtsave(filename = paste0("Code/satisfaction","/satisfaction_sav.html"))



# Binarisation des variables conservees pour l'indicateur

df <- df_sav

df$AUTCHOI[which(df$AUTCHOI != 1)] <- 0

df$EMPLMUN[which(df$EMPLMUN == 4)] <- 0
df$EMPLMUN[which(df$EMPLMUN != 0)] <- 1

df$COMAUT[which(df$COMAUT != 1)] <- 0

df$CONSENT[which(df$CONSENT == 2)] <- 1 # 
df$CONSENT[which(df$CONSENT != 2)] <- 0 # resolved an issue here

df$PORTUNI[which(df$PORTUNI != 1)] <- 0

df$IDMET[which(df$IDMET != 2)] <- 0
df$IDMET[which(df$IDMET == 2)] <- 1

df$IDAPN[which(df$IDAPN != 2)] <- 0
df$IDAPN[which(df$IDAPN == 2)] <- 1

df$METIER[which(df$METIER != 3)] <- 0
df$METIER[which(df$METIER == 3)] <- 1

df$BIENVU[which(df$BIENVU != 2)] <- 0
df$BIENVU[which(df$BIENVU == 2)] <- 1

df$FORMCOM[which(df$FORMCOM != 2)] <- 0
df$FORMCOM[which(df$FORMCOM == 2)] <- 1

df$BONFORM[which(df$BONFORM != 2)] <- 0
df$BONFORM[which(df$BONFORM == 2)] <- 1

for (col in vars_satisf[14:20]){
  df[which(df[,col] != 2),col] <- 0
  df[which(df[,col] == 2),col] <- 1
}

# Save file
saveRDS(df_sav, "df_sav")

# Distribution a plat des variables de statisfaction binarisees
df %>% tbl_summary(
  include = all_of(vars_satisf),
  statistic = list(all_continuous() ~ "{mean} ({sd})",
                   all_categorical() ~ "{p}% ({n})"),
  label = list(AUTCHOI ~ "Aurait choisi un autre emploi",
               EMPLMUN ~ "Eventualité emploi police privée",
               COMAUT ~ "Métier comme un autre",
               MOTIFAU ~ "Motif métier comme un autre",
               CONSENT ~ "Conseillerait d'entrer dans la police",
               RAISCONS ~ "Motif du conseil",
               PORTUNI ~ "Opinion port de l'uniforme",
               IDMET ~ "Evolution idée du futur métier",
               IDAPN ~ "Evolution idée de la Police Nationale",
               METIER ~ "Idée du métier par rapport aux représentations",
               BIENVU ~ "Considération et avantages",
               FORMCOM ~ "Opinion sur tronc commun",
               BONFORM ~ "Bien préparé aux contextes et aux publics",
               PLUSTIR ~ "Aurait souhaité plus de tir",
               PLUSSOC ~ "Aurait souhaité plus de connaissance de la société",
               PLUSADM ~ "Aurait souhaité plus de connaissances administratives",
               PLUSTEC ~ "Aurait souhaité plus de techniques policières",
               PLUSHUM ~ "Aurait souhaité plus de formation aux relations humaines",
               PLUSJUR ~ "Aurait souhaité plus de connaissances juridiques",
               PLUSRAP ~ "Aurait souhaité plus d'expression écrite")) %>%
  as_gt() %>%
  gt::gtsave(filename = paste0("Code/satisfaction","/satisfaction_dummies_sav.html"))

