#-----------------------------------------------------#
#
#     Project in Data Science for Social Sciences
#       REGRESSION ANALYSIS
#
#                               Chloe Lavest & Yasmine Houri
#                               Academic year 2021-2022
#
#-----------------------------------------------------#

# rm(list = ls())

#-----------------------------------------------------#

# Executer 'Code/repro_score.R' a la main
controles <- c("SEXE", "AGE", "DIPLOME")

valeurs <- c("SEXE", "AGE", "DIPLOME",
             "IMPORMET", "RASPEUR", "ORDREINI", "EFFIREGL", "PUBOPIN")

controles_nosex <- c("AGE", "DIPLOME")

#----------------------------------------------------------------#
#----------------------------------------------------------------#

#                   REGRESSION ANALYSIS

#----------------------------------------------------------------#
#----------------------------------------------------------------#

### WAVE 5

reg_wave5 <- reg_controles(df = acm_satis_w5, var_dep = "satis_score_w5",
              var_indep = "acm_score",
              controles = controles)
stargazer(reg_wave5$models[1],reg_wave5$models[3],reg_wave5$models[4],title="Régressions vague 5", align=TRUE,
          no.space = TRUE, font.size = 'small', out = "results/reg_wave5.tex",
          column.labels=c('Model 1', 'Model 2', 'Model 3'))

# Ajout des variables de valeurs
reg_wave5_values <- reg_controles(df = acm_satis_w5, var_dep = "satis_score_w5",
                           var_indep = "acm_score",
                           controles = valeurs)
stargazer(reg_wave5_values$models[1],reg_wave5_values$models[4],reg_wave5_values$models[9],
          title="Régressions vague 5", align=TRUE,
          no.space = TRUE, font.size = 'small', out = "results/reg_wave5_values.tex",
          column.labels=c('Model 1', 'Model 2', 'Model 3'))

# Femmes
### WAVE 5

reg_wave5_F <- reg_controles(df = acm_satis_w5 %>% filter(SEXE == 'Femme'), var_dep = "satis_score_w5",
                           var_indep = "acm_score",
                           controles = controles_nosex)
stargazer(reg_wave5_F$models[1],reg_wave5_F$models[3],title="Régressions vague 5", align=TRUE,
          no.space = TRUE, font.size = 'small', out = "results/reg_wave5_F.tex",
          column.labels=c('Model 1', 'Model 2'))

# Hommes
### WAVE 5

reg_wave5_H <- reg_controles(df = acm_satis_w5 %>% filter(SEXE == 'Homme'), var_dep = "satis_score_w5",
                           var_indep = "acm_score",
                           controles = controles_nosex)
stargazer(reg_wave5_H$models[1],reg_wave5_H$models[3],title="Régressions vague 5", align=TRUE,
          no.space = TRUE, font.size = 'small', out = "results/reg_wave5_H.tex",
          column.labels=c('Model 1', 'Model 2'))

### WAVE 3

reg_wave3 <- reg_controles(df = acm_satis_w3, var_dep = "satis_score_w3",
                           var_indep = "acm_score",
                           controles = controles)
stargazer(reg_wave3$models[1],reg_wave3$models[3],reg_wave3$models[4],
          title="Régressions vague 3", align=TRUE,
          no.space = TRUE, font.size = 'small', out = "results/reg_wave3.tex",
          column.labels=c('Model 1', 'Model 2', 'Model 3'))

# Ajout des variables de valeurs
reg_wave3_values <- reg_controles(df = acm_satis_w3, var_dep = "satis_score_w3",
                                  var_indep = "acm_score",
                                  controles = valeurs)
stargazer(reg_wave3_values$models[1],reg_wave3_values$models[4],reg_wave3_values$models[9],
          title="Régressions vague 3", align=TRUE,
          no.space = TRUE, font.size = 'small', out = "results/reg_wave3_values.tex",
          column.labels=c('Model 1', 'Model 2', 'Model 3'))

# Femmes
### WAVE 3

reg_wave3_F <- reg_controles(df = acm_satis_w3 %>% filter(SEXE == 'Femme'), var_dep = "satis_score_w3",
                             var_indep = "acm_score",
                             controles = controles_nosex)
stargazer(reg_wave3_F$models[1],reg_wave3_F$models[3],title="Régressions vague 3", align=TRUE,
          no.space = TRUE, font.size = 'small', out = "results/reg_wave3_F.tex",
          column.labels=c('Model 1', 'Model 2'))

# Hommes
### WAVE 3

reg_wave3_H <- reg_controles(df = acm_satis_w3 %>% filter(SEXE == 'Homme'), var_dep = "satis_score_w3",
                             var_indep = "acm_score",
                             controles = controles_nosex)
stargazer(reg_wave3_H$models[1],reg_wave3_H$models[3],title="Régressions vague 3", align=TRUE,
          no.space = TRUE, font.size = 'small', out = "results/reg_wave3_H.tex",
          column.labels=c('Model 1', 'Model 2'))



### DEPENDENCY EFFECT (WAVE 3 on WAVE 5)
acm_satis_w3_w5 <- acm_satis_w5 %>%
  merge(select(acm_satis_w3, c("CIDENTIT", "satis_score_w3")), on = "CIDENTIT", all = FALSE)

dep_effect <- reg_controles(df = acm_satis_w3_w5, var_dep = "satis_score_w5",
                          var_indep = c("acm_score","satis_score_w3"),
                          controles = controles)


#----------------------------------------------------------------#
#----------------------------------------------------------------#

#                   REGRESSIONS WITH INTERACTION TERM:
#                   SEXE

#----------------------------------------------------------------#
#----------------------------------------------------------------#

### WAVE 5

lm.fit5 <- lm(satis_score_w5 ~ acm_score*SEXE + AGE + DIPLOME, data = acm_satis_w5)
pdf("results/plot_interaction_w5.pdf")
plot(allEffects(lm.fit5))
dev.off()

### WAVE 3

lm.fit3 <- lm(satis_score_w3 ~ acm_score*SEXE + AGE + DIPLOME, data = acm_satis_w3)
pdf("results/plot_interaction_w3.pdf")
plot(allEffects(lm.fit3))
dev.off()


