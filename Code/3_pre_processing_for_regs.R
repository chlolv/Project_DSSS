#-----------------------------------------------------#
#
#     Project in Data Science for Social Sciences
#       Computation of and indicator of satisfaction
#
#                               Chloe Lavest & Yasmine Houri
#                               Academic year 2021-2022
#
#-----------------------------------------------------#

#---------- DATA LOADING ----------#
df <- readRDS("df_sav")
df[is.na(df)] <- 0
w1_w2 <- read.csv2("w1_w2.csv")
wave1_test <- read.csv2("wave1_test.csv")


#---------- FUNCTIONS ----------#
reg <- function(vars_repro_arg, vars_demog_arg, vars_satisf_arg){
  repro_reg <- w1_w2[,c("CIDENTIT",vars_repro_arg, vars_demog_arg)]
  wave5_satisf <- wave5[,c("CIDENTIT",vars_satisf_arg)]
  data_reg <- merge(wave5_satisf,repro_reg, by="CIDENTIT", all=F)
  data_reg <- select(data_reg,-c("CIDENTIT"))
  return(data_reg)
}


#---------- VARIABLES SELECTION ----------#
vars_satisf <- c("AUTCHOI","CONSENT","IDMET","IDAPN","METIER")
vars_demog <- c('SEXE','AGE','STATUT','NOMBENF','DIPLOME','ENFANCE')
# vars_repro <- c("exp_ant", "cont_ami",
#                 "mere_pol", "pere_pol", "conj_pol", "ant_pol")
vars_repro <- c("exp_ant", "cont_fam", "cont_ami", "par_pol",
                "mere_pol", "pere_pol", "conj_pol", "ant_pol")
# vars_repro <- c("par_pol","cont_ami")

#---------- DATAFRAME PROCESSING ----------#
wave5 <- df %>% filter(vague==5)

wave5 <- wave5 %>%
  mutate(satis.5 = AUTCHOI + IDMET + CONSENT + IDAPN + METIER)
table(wave5$satis.5)
wave5$z_satis.5 <- (wave5$satis.5 - mean(wave5$satis.5))/sd(wave5$satis.5)

