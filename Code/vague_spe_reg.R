#-----------------------------------------------------#
#
#     Project in Data Science for Social Sciences
#       Vague-specific regressions
#
#                               Chloe Lavest & Yasmine Houri
#                               Academic year 2021-2022
#
#-----------------------------------------------------#

cwd <- getwd()
setwd(gsub("Code","",cwd))
#-----------------------------------------------------#

source("Code/3_pre_processing_for_regs.R")
rm(wave1_test, wave5)

#---------- DATA LOADING ----------#
df <- readRDS("Data/df_sav")
df[is.na(df)] <- 0
waves <- list()
for (i in (1:5)){
  vague <- df %>% filter(vague==i)
  waves[[i]] <- vague
  rm(vague)
}

#---------- CREATE z_score ----------#

for (i in 2:5){
  waves[[i]] <- waves[[i]] %>%
  mutate(satis.4 = AUTCHOI + IDMET + CONSENT + IDAPN)
  waves[[i]]$z_satis.4 <- (waves[[i]]$satis.4 - mean(waves[[i]]$satis.4, na.rm = T))/sd(waves[[i]]$satis.4, na.rm = T)
  assign(paste0("wave",as.character(i)),waves[[i]])
}

#---------- FUNCTIONS ----------#
reg <- function(wave,vars_repro_arg, vars_demog_arg, vars_satisf_arg){
  repro_reg <- w1_w2[,c("CIDENTIT",vars_repro_arg, vars_demog_arg)]
  wave_satisf <- wave[,c("CIDENTIT",vars_satisf_arg)]
  data_reg <- merge(wave_satisf,repro_reg, by="CIDENTIT", all=F)
  data_reg <- select(data_reg,-c("CIDENTIT"))
  return(data_reg)
}

#---------- REGRESSIONS WAVE 2 ----------#
# Baseline (1)
data_reg <- reg(wave2,vars_repro, vars_demog[NULL], "z_satis.4")
lm.fit <- lm(z_satis.4 ~ ., data = data_reg)
summary(lm.fit)

# Ajustement (2)
data_reg <- reg(wave2,vars_repro,vars_demog[1],"z_satis.4")
lm.fit <- lm(z_satis.4 ~ ., data = data_reg)
summary(lm.fit)

# Ajustement (3)
data_reg <- reg(wave2,vars_repro,vars_demog[1:2],"z_satis.4")
lm.fit <- lm(z_satis.4 ~ ., data = data_reg)
summary(lm.fit)

# Ajustement (4)
data_reg <- reg(wave2,vars_repro,vars_demog,"z_satis.4")
lm.fit <- lm(z_satis.4 ~ ., data = data_reg)
summary(lm.fit)

#---------- REGRESSIONS WAVE 3 ----------#
# Baseline (1)
data_reg <- reg(wave3,vars_repro, vars_demog[NULL], "z_satis.4")
lm.fit <- lm(z_satis.4 ~ ., data = data_reg)
summary(lm.fit)

# Ajustement (2)
data_reg <- reg(wave4,vars_repro,vars_demog[1],"z_satis.4")
lm.fit <- lm(z_satis.4 ~ ., data = data_reg)
summary(lm.fit)

# Ajustement (3)
data_reg <- reg(wave4,vars_repro,vars_demog[1:2],"z_satis.4")
lm.fit <- lm(z_satis.4 ~ ., data = data_reg)
summary(lm.fit)

# Ajustement (4)
data_reg <- reg(wave4,vars_repro,vars_demog,"z_satis.4")
lm.fit <- lm(z_satis.4 ~ ., data = data_reg)
summary(lm.fit)