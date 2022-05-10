#-----------------------------------------------------#
#
#     Project in Data Science for Social Sciences
#       Heterogeneity
#
#                               Chloe Lavest & Yasmine Houri
#                               Academic year 2021-2022
#
#-----------------------------------------------------#
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("settings.R")


vars_socdem <- c("vague", "SEXE", "AGE", "STATUT",
                "DIPLOME", "NOMBENF", "repro_mere", "repro_pere")
vars_satis <- c("vague", "IDMET", "AUTCHOI", "IDAPN", "CONSENT")
vars_socialisation <- c("vague", "NATCONT", "PAOUGA",
                        "ant_pol", "conj_pol", "mere_pol", "pere_pol")

#----------------------------------------------------------------#
#----------------------------------------------------------------#

#                   Statistiques descriptives
#                   Socio-démographiques

#----------------------------------------------------------------#
#----------------------------------------------------------------#

id_w1w3 <- intersect(wave1$CIDENTIT,wave3$CIDENTIT)
id_w1w5 <- intersect(wave1$CIDENTIT,wave5$CIDENTIT)
wave13 <- wave1 %>% select(c("CIDENTIT",vars_socdem)) %>% 
  filter(CIDENTIT %in% id_w1w3) %>% mutate(vague=3)
wave15 <- wave1 %>% filter(CIDENTIT %in% id_w1w5) %>% 
  select(c("CIDENTIT",vars_socdem)) %>% mutate(vague=5)
df_socdem <- rbind(wave1 %>% select(c("CIDENTIT",all_of(vars_socdem))),wave13,wave15) %>% mutate(vague=as.factor(vague)) %>%
  mutate(SEXE = factor(SEXE, levels = c("masculin", "féminin"), labels = c("Homme", "Femme")))

table1::label(df_socdem$vague) <- "Vague"

table1::label(df_socdem$SEXE) <- "Sexe"
table1::label(df_socdem$AGE) <- "Âge"
table1::label(df_socdem$STATUT) <- "Statut conjugal"
table1::label(df_socdem$DIPLOME) <- "Diplôme"
table1::label(df_socdem$NOMBENF) <- "Nombre d'enfant(s)"
table1::label(df_socdem$repro_mere) <- "Mobilité sociale (mère)"
table1::label(df_socdem$repro_pere) <- "Mobilité sociale (père)"

socdem <- table1(~ SEXE + AGE + STATUT + NOMBENF + DIPLOME + repro_pere + repro_mere | vague, 
                 data = df_socdem, overall=F)
table_creation(socdem,title="Variables sociodémographiques",long=1,location="heterogeneity")

socdem_sexe <- table1(~ AGE + STATUT + NOMBENF + DIPLOME + repro_pere + repro_mere | SEXE, 
                 data = df_socdem %>% filter(vague == 1), overall=F)


# On vérifie l'indépendance par rapport à la vague
df_pval <- df_socdem %>% 
  mutate(STATUT = factor(STATUT, levels = c("célibataire vivant seul(e)", "célibataire vivant en couple", "marié(e)"),
                         labels = c("Célibataire vivant seul.e", "Couple cohabitant", "Marié.e"))) %>%
  mutate(NOMBENF = factor(NOMBENF, levels = c("un", "deux", "trois ou plus", "aucun"),
                          labels = c("un", "deux ou plus", "deux ou plus", "aucun"))) %>%
  mutate(DIPLOME = factor(DIPLOME, levels = c("aucun", "baccalauréat d'enseignement général",
                                              "baccalauréat technique", "BEPC", "BP, BEP",
                                              "BTS", "CAP", "certificat d'études",
                                              "Diplôme universitaire (DEUG et au-delà)"), 
                          labels = c("Aucun", "Bac. Général", "Bac Technique", "BEPC", "Brevet pro",
                                     "BTS", "CAP", "Primaire", "Univ."))) %>%
  drop_na()

table1(~ SEXE + AGE + STATUT + NOMBENF + DIPLOME + repro_pere + repro_mere | vague, 
       data = df_pval %>% filter(vague != 5) , overall=F, extra.col=list(`P-value`=pvalue)) # Tout est indépendant

table1(~ SEXE + AGE + STATUT + NOMBENF + DIPLOME + repro_pere + repro_mere | vague, 
       data = df_pval %>% filter(vague != 3) , overall=F, extra.col=list(`P-value`=pvalue)) # Tout est indépendant

pval_socdem_sexe <- table1(~ AGE + STATUT + NOMBENF + DIPLOME + repro_pere + repro_mere | SEXE, 
       data = df_pval %>% filter(vague == 1) , overall=F, extra.col=list(`P-value`=pvalue))
table_creation(pval_socdem_sexe, title="Variables sociodémographiques selon le sexe (Vague 1)",
               long=1,location="heterogeneity")

#----------------------------------------------------------------#
#----------------------------------------------------------------#

#                   Statistiques descriptives
#                   Satisfaction

#----------------------------------------------------------------#
#----------------------------------------------------------------#

wave13 <- wave3 %>% select(c("CIDENTIT","SEXE","vague",all_of(vars_satis))) %>%
  filter(CIDENTIT %in% id_w1w3)
wave15 <- wave5 %>% filter(CIDENTIT %in% id_w1w5) %>%
  select(c("CIDENTIT","SEXE","vague",all_of(vars_satis)))
df_satis <- rbind(wave13,wave15) %>% mutate(vague=as.factor(vague))%>%
  filter(SEXE %in% c("masculin","féminin")) %>%
  mutate(IDMET = factor(IDMET, levels = c('évolué positivement', "n'a pas changé",'évolué négativement'),
                        labels = c("Meilleure", "Egale", "Pire"))) %>%
  mutate(AUTCHOI = factor(AUTCHOI, levels = c('oui','non'), labels = c("Oui", "Non"))) %>%
  mutate(CONSENT = factor(CONSENT, levels = c('oui','non'), labels = c("Oui", "Non"))) %>%
  mutate(IDAPN = factor(IDAPN, levels = c('évolué positivement', "n'a pas changé",'évolué négativement'),
                        labels = c("Meilleure", "Egale", "Pire"))) %>%
  mutate(SEXE = factor(SEXE, levels = c("masculin", "féminin"), labels = c("Homme", "Femme")))

table1::label(df_satis$vague) <- "Vague"

table1::label(df_satis$IDMET) <- "Idée du métier depuis l'entrée en formation"
table1::label(df_satis$AUTCHOI) <- "Choisir un autre métier"
table1::label(df_satis$IDAPN) <- "Idée de la police depuis l'entrée en formation"
table1::label(df_satis$CONSENT) <- "Conseiller à autrui d'enter dans la police"

sexe_vague <- table1(~ AUTCHOI+CONSENT+IDMET+IDAPN| SEXE+vague,
                 data = df_satis, overall=F)
table_creation(sexe_vague,title="Variables de satisfaction (selon le sexe)",long=1,location="heterogeneity")

vague <- table1(~ AUTCHOI+CONSENT+IDMET+IDAPN| vague,
                     data = df_satis, overall=F)
table_creation(sexe_vague,title="Variables de satisfaction",long=1,location="heterogeneity")

# On vérifie l'indépendance par rapport au sexe

pval_satis_w3 <- table1(~ AUTCHOI+CONSENT+IDMET+IDAPN | SEXE,
       data = df_satis %>% filter(vague == 3) %>% drop_na(), overall=F, extra.col=list(`P-value`=pvalue))
table_creation(pval_satis_w3,title="Indépendance du sexe et de la satisfaction (Vague 3)",
               long=1,location="heterogeneity")


pval_satis_w5 <- table1(~ AUTCHOI+CONSENT+IDMET+IDAPN| SEXE,
       data = df_satis %>% filter(vague == 5) %>% drop_na(), overall=F, extra.col=list(`P-value`=pvalue))
table_creation(pval_satis_w5,title="Indépendance du sexe et de la satisfaction (Vague 5)",
               long=1,location="heterogeneity")

#----------------------------------------------------------------#
#----------------------------------------------------------------#

#                   Statistiques descriptives
#                   Socialisation

#----------------------------------------------------------------#
#----------------------------------------------------------------#

wave13 <- wave1 %>% 
  filter(CIDENTIT %in% id_w1w3) %>% 
  select(c("CIDENTIT","SEXE",vars_socialisation)) %>% 
  mutate(vague=3)

wave15 <- wave1 %>% 
  filter(CIDENTIT %in% id_w1w5) %>% 
  select(c("CIDENTIT","SEXE", vars_socialisation)) %>% 
  mutate(vague=5)

df_socialisation <- rbind(wave1 %>% select(c("CIDENTIT", "SEXE", all_of(vars_socialisation))),wave13,wave15) %>% 
  mutate(vague=as.factor(vague)) %>%
  mutate(SEXE = factor(SEXE, levels = c("masculin", "féminin"), labels = c("Homme", "Femme"))) %>%
  mutate(mere_pol = factor(mere_pol, levels = c(0,1), labels = c("Non", "Oui"))) %>%
  mutate(pere_pol = factor(pere_pol, levels = c(0,1), labels = c("Non", "Oui"))) %>%
  mutate(conj_pol = factor(conj_pol, levels = c(0,1), labels = c("Non", "Oui"))) %>%
  mutate(ant_pol = factor(ant_pol, levels = c(0,1), labels = c("Non", "Oui")))

table1::label(df_socialisation$vague) <- "Vague"

table1::label(df_socialisation$NATCONT) <- "Contact dans la police"
table1::label(df_socialisation$PAOUGA) <- "Service militaire"
table1::label(df_socialisation$ant_pol) <- "Expérience professionnelle antérieure dans la police"
table1::label(df_socialisation$conj_pol) <- "Conjoint dans la police"
table1::label(df_socialisation$mere_pol) <- "Mère dans la police"
table1::label(df_socialisation$pere_pol) <- "Père dans la police"

socialisation <- table1(~ NATCONT+PAOUGA+ant_pol+conj_pol+mere_pol+pere_pol| vague,
                     data = df_socialisation, overall=F)
table_creation(socialisation,title="Variables de socialisation",long=1,location="heterogeneity")

df_socialisation_pval <- df_socialisation %>%
  mutate(PAOUGA = factor(PAOUGA, levels = c("G.A.", "non concerné", "P.A."), 
                         labels = c("Gendarme", "Non", "Policier"))) %>%
  mutate(NATCONT = factor(NATCONT, levels = c("autres (généralement collègues du service national)",
                                              "non concerné",  "un ami personnel ou de votre famille",
                                              "un membre de votre famille", "un voisin", "une connaissance lointaine"),
                          labels = c("Autres", "Non", "Ami", "Famille", "Voisin", "Autres")))

# On vérifie l'indépendance par rapport au sexe

pval_socia <- table1(~ NATCONT+PAOUGA+ant_pol+conj_pol+mere_pol+pere_pol | SEXE,
                     data = df_socialisation_pval %>% filter(vague == 1) %>% drop_na(), overall=F,
                     extra.col=list(`P-value`=pvalue))
table_creation(pval_satis_w5,title="Indépendance du sexe et de la satisfaction (Vague 5)",
               long=1,location="heterogeneity")