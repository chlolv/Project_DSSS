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

# conflict_prefer("select", "dplyr")

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


col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des etiquettes de textes
         # Combiner avec le niveau de significativité
         p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         # Cacher les coefficients de corrélation sur la diagonale
         diag=FALSE 
)
