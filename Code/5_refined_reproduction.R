#-----------------------------------------------------#
#
#     Project in Data Science for Social Sciences
#       Refined model (variable selection amongst reproduction variables)
#
#                               Chloe Lavest & Yasmine Houri
#                               Academic year 2021-2022
#
#-----------------------------------------------------#

# Import packages
library("haven")
library("plyr")
library("dplyr")
library("stringr")
library("purrr")
library("foreign")
library("data.table")
library("gtsummary")
library("tidyverse")
library("zoo")
library("Hmisc")
library("corrplot")

# Chargement des données
# Import data
rm(list=objects())
setwd("C:\\Users\\HP\\Documents\\cours_ensae\\3A\\Projet DSSS\\gardiens_paix")
path <- "C:/Users/HP/Documents/cours_ensae/3A/Projet DSSS/gardiens_paix"
# df_spss <- readRDS("df_spss")
df <- readRDS("df_sav")
df[is.na(df)] <- 0
w1_w2 <- read.csv2("w1_w2.csv")
wave1_test <- read.csv2("wave1_test.csv")
wave5 <- df[which(df$vague == 5),]


# Identification des variables de satisfaction
vars_satisf <- c("AUTCHOI","CONSENT","IDMET","IDAPN","METIER")
vars_demog <- c('SEXE','AGE','STATUT','NOMBENF','DIPLOME','ENFANCE')
vars_repro <- c("par_pol","cont_ami")

# Matrice de correlation des variables de reproduction
wave1.rcorr <- rcorr(as.matrix(na.omit(wave1_test[,vars_repro])))
wave1.coeff <- wave1.rcorr$r
wave1.p <- wave1.rcorr$P
# Visualisation de la matrice
corr_plot <- corrplot(wave1.coeff)
# Les variables les plus correlees sont: (cont_fam, pere_pol),
# (cont_fam, cont_ami), (par_pol, mere_pol)
# Les choix operes ci-dessus sont donc justifies (supp par_pol et cont_fam)

# Nouvelles regressions avec un nouveau score
# Here you calculate the index (rank)
wave5 <- wave5 %>%
  mutate(score5 = AUTCHOI + IDMET + CONSENT + IDAPN + METIER)
table(wave5$score5)
wave5$z_score <- (wave5$score5 - mean(wave5$score5))/sd(wave5$score5)

# Regressions
reg <- function(vars_repro_arg, vars_demog_arg, vars_satisf_arg){
  repro_reg <- w1_w2[,c("CIDENTIT",vars_repro_arg, vars_demog_arg)]
  wave5_satisf <- wave5[,c("CIDENTIT",vars_satisf_arg)]
  data_reg <- merge(wave5_satisf,repro_reg, by="CIDENTIT", all=F)
  data_reg <- select(data_reg,-c("CIDENTIT"))
  return(data_reg)
}

# Baseline (1)
data_reg <- reg(vars_repro, vars_demog[NULL], "z_score")
lm.fit <- lm(z_score ~ ., data = data_reg)
summary(lm.fit)

# Ajustement (2)
data_reg <- reg(vars_repro,vars_demog[1],"z_score")
lm.fit <- lm(z_score ~ ., data = data_reg)
summary(lm.fit)

# Ajustement (3)
data_reg <- reg(vars_repro,vars_demog[1:2],"z_score")
lm.fit <- lm(z_score ~ ., data = data_reg)
summary(lm.fit)

# Ajustement (4)
data_reg <- reg(vars_repro,vars_demog,"z_score")
lm.fit <- lm(z_score ~ ., data = data_reg)
summary(lm.fit)

