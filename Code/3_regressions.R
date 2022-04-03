#-----------------------------------------------------#
#
#     Project in Data Science for Social Sciences
#       Computation of and indicator of satisfaction
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
setwd("C:\\Users\\chloe\\OneDrive\\Bureau\\3A\\Projet DSSS\\Git_project\\Data")
path <- "gardiens_paix"

# df_spss <- readRDS("df_spss")
df <- readRDS("df_sav")
df[is.na(df)] <- 0
w1_w2 <- read.csv2("w1_w2.csv")

# Identification des variables de satisfaction
vars_satisf <- c("AUTCHOI","CONSENT","IDMET","IDAPN","METIER")
vars_demog <- c('SEXE','AGE','STATUT','NOMBENF','DIPLOME','ENFANCE')
# vars_repro <- c("exp_ant", "cont_fam", "cont_ami", "par_pol",
#                 "mere_pol", "pere_pol", "conj_pol", "ant_pol")
vars_repro <- c("exp_ant", "cont_ami",
                "mere_pol", "pere_pol", "conj_pol", "ant_pol")


# Regressions preliminaires
# Define regression function
wave5 <- df %>% filter(vague==5)
reg <- function(vars_repro_arg, vars_demog_arg, vars_satisf_arg){
  repro_reg <- w1_w2[,c("CIDENTIT",vars_repro_arg, vars_demog_arg)]
  wave5_satisf <- wave5[,c("CIDENTIT",vars_satisf_arg)]
  data_reg <- merge(wave5_satisf,repro_reg, by="CIDENTIT", all=F)
  data_reg <- select(data_reg,-c("CIDENTIT"))
  return(data_reg)
}
                                ############################
                                ######## AUTCHOI  ##########
                                ############################


# Baseline (1)
data_reg <- reg(vars_repro, vars_demog[NULL], "AUTCHOI")
lm.fit <- lm(AUTCHOI ~ ., data = data_reg)
summary(lm.fit)

# Ajustement (2)
data_reg <- reg(vars_repro,vars_demog[1],"AUTCHOI")
lm.fit <- lm(AUTCHOI ~ ., data = data_reg)
summary(lm.fit)

# Ajustement (3)
data_reg <- reg(vars_repro,vars_demog[1:2],"AUTCHOI")
lm.fit <- lm(AUTCHOI ~ ., data = data_reg)
summary(lm.fit)

# Ajustement (4)
data_reg <- reg(vars_repro,vars_demog,"AUTCHOI")
lm.fit <- lm(AUTCHOI ~ ., data = data_reg)
summary(lm.fit)


                                 ############################
                                 ######## IDMET   ###########
                                 ############################

# Baseline (1)
data_reg <- reg(vars_repro, vars_demog[NULL], "IDMET")
lm.fit <- lm(IDMET ~ ., data = data_reg)
summary(lm.fit)

# Ajustement (2)
data_reg <- reg(vars_repro,vars_demog[1],"IDMET")
lm.fit <- lm(IDMET ~ ., data = data_reg)
summary(lm.fit)

# Ajustement (3)
data_reg <- reg(vars_repro,vars_demog[1:2],"IDMET")
lm.fit <- lm(IDMET ~ ., data = data_reg)
summary(lm.fit)

# Ajustement (4)
data_reg <- reg(vars_repro,vars_demog,"IDMET")
lm.fit <- lm(IDMET ~ ., data = data_reg)
summary(lm.fit)

                                    ############################
                                    ######## CONSENT   #########
                                    ############################


# Baseline (1)
data_reg <- reg(vars_repro, vars_demog[NULL], "CONSENT")
lm.fit <- lm(CONSENT ~ ., data = data_reg)
summary(lm.fit)

# Ajustement (2)
data_reg <- reg(vars_repro,vars_demog[1],"CONSENT")
lm.fit <- lm(CONSENT ~ ., data = data_reg)
summary(lm.fit)

# Ajustement (3)
data_reg <- reg(vars_repro,vars_demog[1:2],"CONSENT")
lm.fit <- lm(CONSENT ~ ., data = data_reg)
summary(lm.fit)

# Ajustement (4)
data_reg <- reg(vars_repro,vars_demog,"CONSENT")
lm.fit <- lm(CONSENT ~ ., data = data_reg)
summary(lm.fit)

                                  ############################
                                  ######## IDAPN   ###########
                                  ############################


# Baseline (1)
data_reg <- reg(vars_repro, vars_demog[NULL], "IDAPN")
lm.fit <- lm(IDAPN ~ ., data = data_reg)
summary(lm.fit)

# Ajustement (2)
data_reg <- reg(vars_repro,vars_demog[1],"IDAPN")
lm.fit <- lm(IDAPN ~ ., data = data_reg)
summary(lm.fit)

# Ajustement (3)
data_reg <- reg(vars_repro,vars_demog[1:2],"IDAPN")
lm.fit <- lm(IDAPN ~ ., data = data_reg)
summary(lm.fit)

# Ajustement (4)
data_reg <- reg(vars_repro,vars_demog,"IDAPN")
lm.fit <- lm(IDAPN ~ ., data = data_reg)
summary(lm.fit)


                                     ############################
                                     ######## METIER   ##########
                                     ############################

# Baseline (1)
data_reg <- reg(vars_repro, vars_demog[NULL], "METIER")
lm.fit <- lm(METIER ~ ., data = data_reg)
summary(lm.fit)

# Ajustement (2)
data_reg <- reg(vars_repro,vars_demog[1],"METIER")
lm.fit <- lm(METIER ~ ., data = data_reg)
summary(lm.fit)

# Ajustement (3)
data_reg <- reg(vars_repro,vars_demog[1:2],"METIER")
lm.fit <- lm(METIER ~ ., data = data_reg)
summary(lm.fit)

# Ajustement (4)
data_reg <- reg(vars_repro,vars_demog,"METIER")
lm.fit <- lm(METIER ~ ., data = data_reg)
summary(lm.fit)


####################
### SCORE 5 ###########################################################
####################

wave5 <- wave5 %>%
  mutate(score5 = AUTCHOI + IDMET + CONSENT + IDAPN + METIER)
table(wave5$score5)


# Baseline (1)
data_reg <- reg(vars_repro, vars_demog[NULL], "score5")
lm.fit <- lm(score5 ~ ., data = data_reg)
summary(lm.fit)

# Ajustement (2)
data_reg <- reg(vars_repro,vars_demog[1],"score5")
lm.fit <- lm(score5 ~ ., data = data_reg)
summary(lm.fit)

# Ajustement (3)
data_reg <- reg(vars_repro,vars_demog[1:2],"score5")
lm.fit <- lm(score5 ~ ., data = data_reg)
summary(lm.fit)

# Ajustement (4)
data_reg <- reg(vars_repro,vars_demog,"score5")
lm.fit <- lm(score5 ~ ., data = data_reg)
summary(lm.fit)

####################
### SCORE 3 ###########################################################
####################

wave5 <- df %>%
  filter(vague==5) %>%
  mutate(score3 = AUTCHOI + CONSENT + METIER)
table(wave5$score3)


# Baseline (1)
data_reg <- reg(vars_repro, vars_demog[NULL], "score3")
lm.fit <- lm(score3 ~ ., data = data_reg)
summary(lm.fit)

# Ajustement (2)
data_reg <- reg(vars_repro,vars_demog[1],"score3")
lm.fit <- lm(score3 ~ ., data = data_reg)
summary(lm.fit)

# Ajustement (3)
data_reg <- reg(vars_repro,vars_demog[1:2],"score3")
lm.fit <- lm(score3 ~ ., data = data_reg)
summary(lm.fit)

# Ajustement (4)
data_reg <- reg(vars_repro,vars_demog,"score3")
lm.fit <- lm(score3 ~ ., data = data_reg)
summary(lm.fit)