#-----------------------------------------------------#
#
#     Project in Data Science for Social Sciences
#       Descriptive Statistics
#
#                               Chloé Lavest & Yasmine Houri
#                               Academic year 2021-2022
#
#-----------------------------------------------------#
cwd <- getwd()
setwd(gsub("Code","",cwd))


#---------- Settings  ----------#
source("Code/0_settings.R")


#----------  Dataloading ----------#
graphs <- "Code/graphs"

#path = file.path(paste0(path, "gardienspaixq1.sav"))
init = read.spss(paste0(path,"/", "gardienspaixq1.sav"))

path2 = file.path(paste0(path,"/", "gardienspaixq2.sav"))
init2 = read.spss(path2)

wave1 <- as.data.frame(init)
wave2 <- as.data.frame(init2)


#---------- WAVE 1 ----------#
# Constructing variables for analysis

wave1_soc = c("PROFPERE", "PROFMERE", "PROFCONJ", "EMPLANT", "PAOUGA", "CONTANT", "NATCONT")

# En vague 2 on a des variables supplémentaires : 
# Policiers dans la famile		BPOLICFA
# Lien de parenté avec policier le plus proche		BPARENTE
# Grade du policier le plus proche		BQUELGRA


table1(~ PROFCONJ, data = wave1)
length(unique(wave1$PROFCONJ))
unique(wave1$PROFCONJ)

wave1_test <- wave1 %>%
  mutate(profconj = as.numeric(PROFCONJ)) %>% # 1 = Non réponse, 36 = Pas de conjoint
  mutate(conj_pol = case_when( profconj %in% c(18, 22, 33, 35) ~ 1,
                               profconj == 36 ~ 0,
                               profconj == 1 ~ NA_real_,
                             TRUE ~ 0)) %>%
  mutate(emplant = as.numeric(EMPLANT)) %>%
  mutate(profpere = as.numeric(PROFPERE)) %>%
  mutate(profmere = as.numeric(PROFMERE)) %>%
  mutate(mere_pol = case_when( profmere %in% c(25, 29, 41, 43) ~ 1,
                               profmere == 44 ~ 0,
                               profmere == 1 ~ NA_real_,
                               TRUE ~ 0)) %>%
  mutate(pere_pol = case_when( profpere %in% c(24, 28, 29, 43, 44, 45) ~ 1,
                               profpere == 46 ~ 0,
                               profpere == 1 ~ NA_real_,
                               TRUE ~ 0)) %>%
  mutate(ant_pol = case_when( emplant %in% c(18, 22, 23, 35, 37) ~ 1,
                              emplant == 38 ~ 0,
                              emplant == 1 ~ NA_real_,
                               TRUE ~ 0)) %>%
  mutate(cont_ami = case_when( NATCONT == "un ami personnel ou de votre famille" ~ 1,
                               NATCONT == "non concerné" ~ 0,
                               NATCONT == "non réponse" ~ NA_real_,
                               TRUE ~ 0)) %>%
  mutate(cont_fam = case_when( NATCONT == "un membre de votre famille" ~ 1,
                               NATCONT == "non concerné" ~ 0,
                               NATCONT == "non réponse" ~ NA_real_,
                               TRUE ~ 0)) %>%
  mutate(cont_aut = case_when( NATCONT == "autres (généralement collègues du service national)" ~ 1,
                               NATCONT == "un voisin" ~ 1,
                               NATCONT == "une connaissance lointaine" ~ 1,
                               NATCONT == "non concerné" ~ 0,
                               NATCONT == "non réponse" ~ NA_real_,
                               TRUE ~ 0)) %>%
  mutate(exp_ant = case_when( PAOUGA == "P.A." ~ 1,
                              PAOUGA == "G.A." ~ 1,
                              PAOUGA == "non concerné" ~ 0,
                              PAOUGA == "non réponse" ~ NA_real_,
                              TRUE ~ 0)) %>%
  mutate(contant = case_when( CONTANT == "un ou des gendarmes" ~ 1,
                              CONTANT == "un ou des policiers" ~ 1,
                              CONTANT == "des policiers et des gendarmes" ~ 2,
                              CONTANT == "non réponse" ~ NA_real_,
                              TRUE ~ 0)) %>%
  mutate(par_pol = case_when(pere_pol == 1 & mere_pol == 1 ~ 1,
                             pere_pol == 0 | mere_pol == 0 ~ 0,
                             TRUE ~ 0))
   

table1(~ EMPLANT, data = wave1)
table1(~ PROFPERE, data = wave1)
table1(~ PROFMERE, data = wave1)

write.csv2(wave1_test,"Data/wave1_test.csv")

# PROFCONJ
# 18 = Policiers
# 22 = Militaires sans autre précision
# 33 = Militaire du contingent
# 35 = Gardiens, gradés et enquêteurs

# EMPLANT
# 18 = Policiers
# 22 = Gendarmes(sans autre indication)
# 23 = Militaires (sans autre indication), sous-officier
# 35 = Militaire du contingent
# 37 = Officiers autres armes

# PROFPERE
# 24 = Policiers
# 28 = Gendarmes
# 29 = Militaires, sous-officier
# 43 = Gardiens, gradés et enquêteurs
# 44 = inspecteurs, commissaires et officiers
# 45 = Officiers autres armes

# PROFMERE
# 25 = Policiers
# 29 = Militaires, sous-officier
# 41 = Militaires du contingent
# 43 = Gardiens, gradés et enquêteurs


#---------- WAVE 2 ----------#
# Constructing variables for analysis

# Policiers dans la famile = BPOLICFA --> oui/non
# Lien de parenté avec policier le plus proche = BPARENTE --> parent/fratrie/conjoint/autre
# Grade du policier le plus proche = BQUELGRA --> gardiens et gradés/officiers et inspecteurs/commissaires


# Quand on compare les chiffres entre BPOLICEFA en vague 2 et 
# CONTANT en vague 1 on constate que 14 policiers supplémentaires déclarent avoir
# un policier dans leur famille
# Ces 14 policiers semblent être les individus dont le conjoint est policier
# ce qui serait alors très cohérent avec la vague 1 (puisqu'en vague 1 famille et
# conjoint sont des modalités séparées)
# On est donc rassurés sur la cohérence des réponses là-dessus 

w1_w2 <- wave2 %>%
  select(CIDENTIT, BPOLICFA, BPARENTE, BQUELGRA) %>%
  merge(wave1_test, by = "CIDENTIT", all.x = FALSE, all.y = FALSE) %>%
  mutate_at(c("exp_ant", "cont_fam", "cont_ami", "par_pol",
                "mere_pol", "pere_pol", "conj_pol", "ant_pol"), 
            .funs = factor,
            levels = c(0,1,0),
            labels = c("Non", "Oui", "Non concerné"))

# Quand on merge les bases en vague 1 et en vague 2 on obtient quelque chose
# de plutôt étonnant. Alors que la vague 1 est censée contenir tous les individus
# On s'attend à retrouver l'intrégralité des policiers en vague 2 dans le merge 
# càd 1157 individus, or certains des individus de la vague 2 n'ont pas d'identifiant
# en vague 1 (124)

#---------- Descriptive Statistics ----------#

table1::label(w1_w2$exp_ant) <- "Service militaire dans la police"
table1::label(w1_w2$cont_fam) <- "Contact au sein de la police : Famille"
table1::label(w1_w2$cont_ami) <- "Contact au sein de la police : Ami"
table1::label(w1_w2$par_pol) <- "Les deux parents travaillent dans la police"
table1::label(w1_w2$mere_pol) <- "La mère travaille dans la police"
table1::label(w1_w2$pere_pol) <- "Le père travaille dans la police"
table1::label(w1_w2$conj_pol) <- "Le conjoint travaille dans la police"
table1::label(w1_w2$ant_pol) <- "L'emploi antérieur était dans la police"

table1(~ exp_ant + cont_fam + cont_ami + par_pol + 
         mere_pol + pere_pol + conj_pol + ant_pol +
         BPOLICFA + BPARENTE + BQUELGRA, data = w1_w2)


write.csv2(w1_w2,"Data/w1_w2.csv")
