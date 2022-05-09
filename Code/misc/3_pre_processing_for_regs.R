#-----------------------------------------------------#
#
#     Project in Data Science for Social Sciences
#       Computation of and indicator of satisfaction
#
#                               Chloe Lavest & Yasmine Houri
#                               Academic year 2021-2022
#
#-----------------------------------------------------#

cwd <- getwd()
setwd(gsub("Code","",cwd))
#-----------------------------------------------------#

source("Code/0_settings.R")

#---------- DATA LOADING ----------#
df <- readRDS("Data/df_sav")
df[is.na(df)] <- 0
wave1_test <- read.csv2("wave1_test.csv")


#---------- VARIABLES SELECTION ----------#
vars_satisf <- c("AUTCHOI","CONSENT","IDMET","IDAPN","METIER")
vars_demog <- c('SEXE','AGE','STATUT','NOMBENF','DIPLOME','ENFANCE')
vars_repro <- c("exp_ant", "cont_fam", "cont_ami", "par_pol",
                "mere_pol", "pere_pol", "conj_pol", "ant_pol")

#---------- DATAFRAME PROCESSING ----------#
wave5 <- df %>% filter(vague==5)

wave5 <- wave5 %>%
  mutate(satis.5 = AUTCHOI + IDMET + CONSENT + IDAPN + METIER)
table(wave5$satis.5)
wave5$z_satis.5 <- (wave5$satis.5 - mean(wave5$satis.5))/sd(wave5$satis.5)

wave3 <- df %>% filter(vague==3)


# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
