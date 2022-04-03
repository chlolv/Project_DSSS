#-----------------------------------------------------#
#
#     Project in Data Science for Social Sciences
#       Forward selection
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
library(caret)
library(leaps)
library(MASS)
library(conflicted)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("rename", "dplyr")

# Chargement des données
# Import data
rm(list=objects())

# Yasmine
# setwd("C:\\Users\\HP\\Documents\\cours_ensae\\3A\\Projet DSSS\\gardiens_paix")
# path <- "C:/Users/HP/Documents/cours_ensae/3A/Projet DSSS/gardiens_paix"

# Chloé
setwd("C:\\Users\\chloe\\OneDrive\\Bureau\\3A\\Projet DSSS\\Git_project\\Data")
path <- "gardiens_paix"

# df_spss <- readRDS("df_spss")
df <- readRDS("df_sav")
df[is.na(df)] <- 0
w1_w2 <- read.csv2("w1_w2.csv")
wave1_test <- read.csv2("wave1_test.csv")
wave5 <- df[which(df$vague == 5),]


# Identification des variables de satisfaction
vars_satisf <- c("AUTCHOI","CONSENT","IDMET","IDAPN","METIER")
vars_demog <- c('SEXE','AGE','STATUT','NOMBENF','DIPLOME','ENFANCE')
vars_repro <- c("par_pol","cont_fam")

# Nouvelles regressions avec un nouveau score
# Here you calculate the index (rank)
wave5 <- wave5 %>%
  dplyr::mutate(score5 = AUTCHOI + IDMET + CONSENT + IDAPN + METIER)
table(wave5$score5)
wave5$z_score <- (wave5$score5 - mean(wave5$score5))/sd(wave5$score5)
# Score reproduction
wave1_test <- wave1_test %>%
  dplyr::mutate(repro2 = cont_fam + cont_ami)
table(wave1_test$repro2)

# Regressions
reg <- function(vars_repro_arg, vars_demog_arg, vars_satisf_arg){
  repro_reg <- w1_w2[,c("CIDENTIT",vars_repro_arg, vars_demog_arg)]
  wave5_satisf <- wave5[,c("CIDENTIT",vars_satisf_arg)]
  data_reg <- merge(wave5_satisf,repro_reg, by="CIDENTIT", all=F)
  data_reg <- dplyr::select(data_reg,-c("CIDENTIT"))
  return(data_reg)
}

# Regressions
reg2 <- function(vars_repro_arg, vars_demog_arg, vars_satisf_arg){
  repro_reg <- wave1_test[,c("CIDENTIT",vars_repro_arg, vars_demog_arg)]
  wave5_satisf <- wave5[,c("CIDENTIT",vars_satisf_arg)]
  data_reg <- merge(wave5_satisf,repro_reg, by="CIDENTIT", all=F)
  data_reg <- dplyr::select(data_reg,-c("CIDENTIT"))
  return(data_reg)
}

##### z-score variable repro
# Here you calculate the index (rank)
vars_repro <- c("exp_ant", "cont_fam", "cont_ami", "par_pol",
                "mere_pol", "pere_pol", "conj_pol", "ant_pol")
wave1_test <- wave1_test %>%
  dplyr::mutate(score6 = exp_ant + cont_fam + cont_ami + par_pol + conj_pol + ant_pol)
table(wave1_test$score6)
wave1_test$z_score6 <- (wave1_test$score6 - mean(wave1_test$score6, na.rm = T))/sd(wave1_test$score6, na.rm = T)

# Stepwise pipeline
# 1. Fit the full model
data_reg <- reg2("z_score6",vars_demog,"z_score")
data_reg <- na.omit(data_reg)
full.model <- lm(z_score ~ ., data = data_reg)
# 2. Stepwise regression model
step.model <- stepAIC(full.model, direction = "both",
                      trace = FALSE)
summary(step.model)

data_corr1 <- wave5 %>%
  select(CIDENTIT, AUTCHOI, CONSENT, IDAPN, METIER, IDMET, score5)

data_corr <- wave1_test %>%
  select(CIDENTIT, conj_pol, mere_pol, pere_pol, ant_pol,
  cont_ami,cont_fam, cont_aut, exp_ant, par_pol, score6) %>%
  plyr::join(data_corr1, by = 'CIDENTIT', type = "inner") %>%
  select(-CIDENTIT) %>%
  rename(Service = exp_ant) %>%
  rename(Conseil = CONSENT) %>%
  rename(Idee.metier = METIER) %>%
  rename(Idee.metier2 = IDMET) %>%
  rename(Satis.5 = score5) %>%
  rename(Repro.6 = score6) %>%
  rename(Police.conjoint = conj_pol) %>%
  rename(Police.mere = mere_pol) %>%
  rename(Police.pere = pere_pol) %>%
  rename(Police.parents = par_pol) %>%
  rename(Contact.fam = cont_fam) %>%
  rename(Idee.police = IDAPN) %>%
  rename(Police.ante = ant_pol) %>%
  rename(Contact.autres = cont_aut) %>%
  rename(Autre.metier = AUTCHOI) %>%
  rename(Contact.ami = cont_ami)

corr_satis_repro <- cor(data_corr, use = "complete.obs")

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

# Matrice de p-value de la corrélation
p.mat <- cor.mtest(data_corr)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(corr_satis_repro, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, number.cex = 0.5,
         # Cacher les coefficients de corrélation sur la diagonale
         diag=FALSE 
)


# Comments on the corrplot:
# Our reproduction score on 6 variables is only
# weakly correlated with all varibales which are not part of
# it's construction. It is positively correlated with all satisfaction
# variables except for IDAPN (Idee.police).


