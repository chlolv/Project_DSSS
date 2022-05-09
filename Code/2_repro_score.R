#-----------------------------------------------------#
#
#     Project in Data Science for Social Sciences
#       ACM FOR REPRODUCTION & SATISFACTION SCORE
#
#                               Chloe Lavest & Yasmine Houri
#                               Academic year 2021-2022
#
#-----------------------------------------------------#

rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#-----------------------------------------------------#

source("settings.R")

#----------------------------------------------------------------#
#----------------------------------------------------------------#

#                   ACM FOR REPRODUCTION SCORE
#                   WAVES 3 & 5

#----------------------------------------------------------------#
#----------------------------------------------------------------#


# On met en forme le dataframe pour l'acm
df_acm = wave1 %>%
  select(CIDENTIT, AGE, DIPLOME, SEXE, STATUT, NOMBENF, ENFANCE, NATCONT, PAOUGA,
         ant_pol, conj_pol, mere_pol, pere_pol, repro_mere, repro_pere,
         IMPORMET, RASPEUR, ORDREINI, EFFIREGL, PUBOPIN) %>%
  mutate(conj_pol = factor(conj_pol, levels = c(0,1), labels = c("Non", "Oui"))) %>%
  mutate(mere_pol = factor(mere_pol, levels = c(0,1), labels = c("Non", "Oui"))) %>%
  mutate(pere_pol = factor(pere_pol, levels = c(0,1), labels = c("Non", "Oui"))) %>%
  mutate(ant_pol = factor(ant_pol, levels = c(0,1), labels = c("Non", "Oui"))) %>%
  mutate(PAOUGA = factor(PAOUGA, levels = c("G.A.", "non concerné", "P.A."), 
                         labels = c("Gendarme", "Non", "Policier"))) %>%
  mutate(NATCONT = factor(NATCONT, levels = c("autres (généralement collègues du service national)",
                                              "non concerné", "non réponse", "un ami personnel ou de votre famille",
                                              "un membre de votre famille", "un voisin", "une connaissance lointaine"),
                          labels = c("Autres", "Non", NA_real_, "Ami", "Famille", "Voisin", "Autres"))) %>%
  rename(Contact = NATCONT) %>%
  rename(Service = PAOUGA)


# on fait l'acm
results_acm <- dudi.acm(select(df_acm, -c("CIDENTIT", "AGE", "DIPLOME", "SEXE", "STATUT", "NOMBENF", 
                                          "ENFANCE", "repro_mere", "repro_pere",
                                          "IMPORMET", "RASPEUR", "ORDREINI", "EFFIREGL", "PUBOPIN")), scannf = FALSE, nf = 5)
fviz_screeplot(results_acm) 
summary(results_acm)
# On regarde la projection des variables de reproduction et de leurs modalités dans le plan
# constitué des deux axes principaux
pdf("results/acm_repro_score.pdf")
fviz_mca_var(results_acm, axes = c(1, 2),
             geom = c("point", "text"), label = "all",
             invisible = "none", labelsize = 4, pointsize = 3, col.var = "blue4",
             alpha.var = "contrib", shape.var = 16, col.quanti.sup = "blue",
             col.quali.sup = "darkgreen", col.circle = "grey70",
             map = "symmetric",repel = TRUE) +
  labs(title = "ACM sur la socialisation primaire", x = "Première Dimension",
       y = "Deuxième Dimension")
  theme_classic()
dev.off()

# on regarde la contribution des variables 

pdf("results/acm_repro_contrib_var_first_axis.pdf")
fviz_contrib(results_acm, choice = "var", axes = 1) +
  labs(title = "Contribution des variables de l'ACM à la Première Dimension") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "black"))
dev.off()

# fviz_contrib(results_acm, choice = "var", axes = 2)



# On recode les variables socio-dem pour faire des projections

df_acm <- df_acm %>%
  mutate(AGE = factor(AGE, levels = c("entre 21 et 25 ans", "entre 26 et 30 ans",
                                        "moins de 21 ans", "plus de 30 ans"), 
                       labels = c("21-25", "26-30", "<21", ">30"))) %>%
  mutate(DIPLOME = factor(DIPLOME, levels = c("aucun", "baccalauréat d'enseignement général",
                                              "baccalauréat technique", "BEPC", "BP, BEP",
                                              "BTS", "CAP", "certificat d'études",
                                              "Diplôme universitaire (DEUG et au-delà)", "non réponse"), 
                      labels = c("Aucun", "Bac. Général", "Bac Technique", "BEPC", "Brevet pro",
                                 "BTS", "CAP", "Primaire", "Univ.", NA_real_))) %>%
  mutate(SEXE = factor(SEXE, levels = c("féminin","masculin"), labels = c("Femme", "Homme")))
  

# On projette qqs variables socio-dem

pdf("results/acm_repro_with_sex.pdf")
fviz_mca_ind(results_acm, geom = "point", alpha.ind = .25, habillage = df_acm$SEXE, addEllipses = TRUE) +
  labs(title = "Projection d'une variable additionnelle (sexe) sur l'ACM", x = "Première Dimension",
       y = "Deuxième Dimension") +
  theme_classic()
dev.off()

# On récupère le score de reproduction 
# resultats_acm --> li --> Axis1

df_acm <- df_acm %>% 
  mutate(acm_score = results_acm$li$Axis1)


#----------------------------------------------------------------#
#----------------------------------------------------------------#

#                   ACM FOR SATISFACTION SCORE
#                   WAVE 5

#----------------------------------------------------------------#
#----------------------------------------------------------------#


acm_satis_w5 <- wave5 %>% 
  select(CIDENTIT, IDMET, AUTCHOI, IDAPN, CONSENT) %>%
  mutate(IDMET = factor(IDMET, levels = c('évolué positivement', "n'a pas changé",'évolué négativement'),
                        labels = c("Meilleure", "Egale", "Pire"))) %>%
  mutate(AUTCHOI = factor(AUTCHOI, levels = c('oui','non'), labels = c("Oui", "Non"))) %>%
  mutate(CONSENT = factor(CONSENT, levels = c('oui','non'), labels = c("Oui", "Non"))) %>%
  mutate(IDAPN = factor(IDAPN, levels = c('évolué positivement', "n'a pas changé",'évolué négativement'),
                        labels = c("Meilleure", "Egale", "Pire"))) %>%
  merge(df_acm[c("CIDENTIT", "acm_score", "SEXE", "AGE","STATUT", "DIPLOME", "NOMBENF", "ENFANCE","repro_pere","repro_mere", 
                 "IMPORMET", "RASPEUR", "ORDREINI", "EFFIREGL", "PUBOPIN")],
                    by = "CIDENTIT",
                    all = FALSE)

# on fait l'acm 
results_acm_satis_w5 <- dudi.acm(select(acm_satis_w5, -c("CIDENTIT", "acm_score", "SEXE", "AGE","STATUT",
                                                         "DIPLOME", "NOMBENF", "ENFANCE", "repro_mere", 
                                                         "repro_pere","IMPORMET", "RASPEUR", "ORDREINI", 
                                                         "EFFIREGL", "PUBOPIN")), scannf = FALSE, nf = 5)

fviz_screeplot(results_acm_satis_w5) 
summary(results_acm_satis_w5)

# On regarde la peojection des variables de reproduction et de leurs modalités dans le plan
# constituté des deux axes principaux

pdf("results/acm_satis_w5.pdf")
fviz_mca_var(results_acm_satis_w5, axes = c(1, 2),
             geom = c("point", "text"), label = "all",
             invisible = "none", labelsize = 4, pointsize = 3, col.var = "blue4",
             alpha.var = "contrib", shape.var = 16, col.quanti.sup = "blue",
             col.quali.sup = "darkgreen", col.circle = "grey70",
             map = "symmetric",repel = TRUE) +
  labs(title = "ACM sur la satisfaction (post-formation)", x = "Première Dimension",
       y = "Deuxième Dimension") +
  theme_classic()
dev.off()

# on regarde la contribution des variables 

pdf("results/acm_satis_w5_contrib_var_first_axis.pdf")
fviz_contrib(results_acm_satis_w5, choice = "var", axes = 1) +
  labs(title = "Contribution des variables de l'ACM à la Première Dimension") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "black"))
dev.off()

pdf("results/acm_satis_w5_with_sex.pdf")
fviz_mca_ind(results_acm_satis_w5, geom = "point", alpha.ind = .25, habillage = acm_satis_w5$SEXE, addEllipses = TRUE) +
  labs(title = "Projection d'une variable additionnelle (sexe) sur l'ACM", x = "Première Dimension",
       y = "Deuxième Dimension") +
  theme_classic()
dev.off()

# On récupère le score de reproduction 
# resultats_acm --> li --> Axis1

acm_satis_w5 <- acm_satis_w5 %>% 
  mutate(satis_score_w5 = results_acm_satis_w5$li$Axis1)

#----------------------------------------------------------------#
#----------------------------------------------------------------#

#                   ACM FOR SATISFACTION SCORE
#                   WAVE 3

#----------------------------------------------------------------#
#----------------------------------------------------------------#


acm_satis_w3 <- wave3 %>% 
  select(CIDENTIT, IDMET, AUTCHOI, IDAPN, CONSENT) %>%
  mutate(IDMET = factor(IDMET, levels = c('évolué positivement', "n'a pas changé",'évolué négativement'),
                        labels = c("Meilleure", "Egale", "Pire"))) %>%
  mutate(AUTCHOI = factor(AUTCHOI, levels = c('oui','non'), labels = c("Oui", "Non"))) %>%
  mutate(CONSENT = factor(CONSENT, levels = c('oui','non'), labels = c("Oui", "Non"))) %>%
  mutate(IDAPN = factor(IDAPN, levels = c('évolué positivement', "n'a pas changé",'évolué négativement'),
                        labels = c("Meilleure", "Egale", "Pire"))) %>%
  merge(df_acm[c("CIDENTIT", "acm_score", "SEXE", "AGE","STATUT", "DIPLOME", "NOMBENF", "ENFANCE", 
                 "repro_pere", "repro_mere", "IMPORMET", "RASPEUR", "ORDREINI", "EFFIREGL", "PUBOPIN")],
                       by = "CIDENTIT",
                       all = FALSE)

# on fait l'acm 
results_acm_satis_w3 <- dudi.acm(select(acm_satis_w3, -c("CIDENTIT", "acm_score", "SEXE", "AGE","STATUT",
                                                         "DIPLOME", "NOMBENF", "ENFANCE", "repro_mere", 
                                                         "repro_pere", "IMPORMET", "RASPEUR", "ORDREINI",
                                                         "EFFIREGL", "PUBOPIN")), scannf = FALSE, nf = 5)

fviz_screeplot(results_acm_satis_w3)
summary(results_acm_satis_w3)

# On regarde la peojection des variables de reproduction et de leurs modalités dans le plan
# constituté des deux axes principaux

pdf("results/acm_satis_w3.pdf")
fviz_mca_var(results_acm_satis_w3, axes = c(1, 2),
             geom = c("point", "text"), label = "all",
             invisible = "none", labelsize = 4, pointsize = 3, col.var = "blue4",
             alpha.var = "contrib", shape.var = 16, col.quanti.sup = "blue",
             col.quali.sup = "darkgreen", col.circle = "grey70",
             map = "symmetric",repel = TRUE) +
  labs(title = "ACM sur la satisfaction (5 ans d'expérience)", x = "Première Dimension",
       y = "Deuxième Dimension") +
  theme_classic()
dev.off()


# on regarde la contribution des variables 

pdf("results/acm_satis_w3_contrib_var_first_axis.pdf")
fviz_contrib(results_acm_satis_w3, choice = "var", axes = 1) +
  labs(title = "Contribution des variables de l'ACM à la Première Dimension") +
  theme(axis.text.x = element_text(angle = 45, hjust=1), panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "black"))
dev.off()

pdf("results/acm_satis_w3_with_sex.pdf")
fviz_mca_ind(results_acm_satis_w3, geom = "point", alpha.ind = .25, habillage = acm_satis_w3$SEXE, addEllipses = TRUE) +
  labs(title = "Projection d'une variable additionnelle (sexe) sur l'ACM", x = "Première Dimension",
       y = "Second Dimension") +
  theme_classic()
dev.off()


# On récupère le score de reproduction 
# resultats_acm --> li --> Axis1

acm_satis_w3 <- acm_satis_w3 %>% 
  mutate(satis_score_w3 = results_acm_satis_w3$li$Axis1)


#----------------------------------------------------------------#
#----------------------------------------------------------------#

#                   EXPLORATORY WORK

#----------------------------------------------------------------#
#----------------------------------------------------------------#

df_sup <- wave1 %>%
  select(NATCONT, PAOUGA, ant_pol, conj_pol, mere_pol, pere_pol, 
         IMPORMET, RASPEUR, ORDREINI, EFFIREGL, PUBOPIN) %>%
  mutate(conj_pol = factor(conj_pol, levels = c(0,1), labels = c("Non", "Oui"))) %>%
  mutate(mere_pol = factor(mere_pol, levels = c(0,1), labels = c("Non", "Oui"))) %>%
  mutate(pere_pol = factor(pere_pol, levels = c(0,1), labels = c("Non", "Oui"))) %>%
  mutate(ant_pol = factor(ant_pol, levels = c(0,1), labels = c("Non", "Oui"))) %>%
  mutate(PAOUGA = factor(PAOUGA, levels = c("G.A.", "non concerné", "P.A."), 
                         labels = c("Gendarme", "Non", "Policier"))) %>%
  mutate(NATCONT = factor(NATCONT, levels = c("autres (généralement collègues du service national)",
                                              "non concerné", "non réponse", "un ami personnel ou de votre famille",
                                              "un membre de votre famille", "un voisin", "une connaissance lointaine"),
                          labels = c("Autres", "Non", NA_real_, "Ami", "Famille", "Voisin", "Autres"))) %>%
  rename(Contact = NATCONT) %>%
  rename(Service = PAOUGA) %>%
  mutate(IMPORMET = factor(IMPORMET, levels = c("non réponse", "de faire un travail intéressant",
                                                "de travailler dans une bonne ambiance", "de pouvoir compter sur les collègues",
                                                "de respecter le règlement"),
                          labels = c(NA_real_, "Intérêt", "Ambiance", "Collège", "Règlement"))) %>%
  mutate(RASPEUR = factor(RASPEUR, levels = c("non réponse", "rassurer les honnêtes gens",
                                              "faire peur aux délinquants"),
                           labels = c(NA_real_, "Rassurer", "Faire peur"))) %>%
  mutate(ORDREINI = factor(ORDREINI, levels = c("non réponse", "je m'en tiendrai aux ordres reçus",
                                                "je prendrai des initiatives"),
                          labels = c(NA_real_, "Ordres", "Initiatives"))) %>%
  mutate(EFFIREGL = factor(EFFIREGL, levels = c("non réponse","souvent", "rarement", "jamais"),
                           labels = c(NA_real_, "Svt", "Rmt", "Jms"))) %>%
  mutate(PUBOPIN = factor(PUBOPIN, levels = c("non réponse","plutôt favorable",
                                              "plutôt défavorable", "plutôt indifférente"),
                           labels = c(NA_real_, "Fav", "Défav", "Indif")))
  
# To do it for all names
# df_sup[] <- lapply(df_sup, factor) # the "[]" keeps the dataframe structure


var_beliefs <- MCA(df_sup, quali.sup = 7:11)
fviz_mca_var(var_beliefs, axes = c(1, 2),
             geom = c("point", "text"), label = "all",
             invisible = "none", labelsize = 4, pointsize = 3, col.var = "blue4",
             alpha.var = "contrib", shape.var = 16, col.quanti.sup = "blue",
             col.quali.sup = "pink", col.circle = "grey70",
             map = "symmetric",repel = TRUE) +
  labs(title = "MCA for Reproduction Score", x = "First Dimension",
       y = "Second Dimension") +
  theme_classic()


fviz_mca_ind(var_beliefs, geom = "point", alpha.ind = .25, habillage = 11, addEllipses = TRUE) +
  labs(title = "MCA with additional variable and individuals", x = "First Dimension",
       y = "Second Dimension") +
  theme_classic()
