#-----------------------------------------------------#
#
#     Project in Data Science for Social Sciences
#       Refined model using a reproduction score variable
#
#                               Chloe Lavest & Yasmine Houri
#                               Academic year 2021-2022
#
#-----------------------------------------------------#

#---------- PATHS ----------#

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(gsub("Code","",getwd()))
path <- "Data"

#-----------------------------------------------------#

source("Code/3_pre_processing_for_regs.R")

#-----------------------------------------------------#

# Construction du z_score de reproduction
wave1_test <- wave1_test %>%
  mutate(score5_repro = exp_ant+mere_pol+pere_pol+conj_pol+ant_pol)
table(wave1_test$score5_repro)
wave1_test$zscore5_repro <- (wave1_test$score5_repro - mean(wave1_test$score5_repro, na.rm = T))/sd(wave1_test$score5_repro, na.rm = T)
table(wave1_test$zscore5_repro)

wave1_test <- wave1_test %>%
  mutate(score4_repro = exp_ant+par_pol+conj_pol+ant_pol)
table(wave1_test$score4_repro)
wave1_test$zscore4_repro <- (wave1_test$score4_repro - mean(wave1_test$score4_repro, na.rm = T))/sd(wave1_test$score4_repro, na.rm = T)
table(wave1_test$zscore4_repro)

wave1_test <- wave1_test %>%
  mutate(score3_repro = exp_ant+par_pol+conj_pol)
table(wave1_test$score3_repro)
wave1_test$zscore3_repro <- (wave1_test$score3_repro - mean(wave1_test$score3_repro, na.rm = T))/sd(wave1_test$score3_repro, na.rm = T)
table(wave1_test$zscore3_repro)

# Construction du z_score de satisfaction
wave5 <- wave5 %>%
  mutate(score5 = AUTCHOI + IDMET + CONSENT + IDAPN + METIER)
table(wave5$score5)
wave5$z_score <- (wave5$score5 - mean(wave5$score5))/sd(wave5$score5)


# Regressions
reg <- function(vars_repro_arg, vars_demog_arg, vars_satisf_arg){
  repro_reg <- wave1_test[,c("CIDENTIT",vars_repro_arg, vars_demog_arg)]
  wave5_satisf <- wave5[,c("CIDENTIT",vars_satisf_arg)]
  data_reg <- merge(wave5_satisf,repro_reg, by="CIDENTIT", all=F)
  data_reg <- select(data_reg,-c("CIDENTIT"))
  return(data_reg)
}

# Baseline (1)
data_reg <- reg("score3_repro", vars_demog[NULL], "z_score")
lm.fit <- lm(z_score ~ ., data = data_reg)
summary(lm.fit)

# Ajustement (2)
data_reg <- reg("score3_repro",vars_demog[1],"z_score")
lm.fit <- lm(z_score ~ ., data = data_reg)
summary(lm.fit)

# Ajustement (3)
data_reg <- reg("zscore3_repro",vars_demog[1:2],"z_score")
lm.fit <- lm(z_score ~ ., data = data_reg)
summary(lm.fit)

# Ajustement (4)
data_reg <- reg("zscore3_repro",vars_demog,"z_score")
lm.fit <- lm(z_score ~ ., data = data_reg)
summary(lm.fit)

