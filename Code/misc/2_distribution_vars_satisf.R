#-----------------------------------------------------#
#
#     Project in Data Science for Social Sciences
#       Descriptive statistics for the computation of and indicator of satisfaction
#
#                               Chloe Lavest & Yasmine Houri
#                               Academic year 2021-2022
#
#-----------------------------------------------------#

cwd <- getwd()
setwd(gsub("Code","",cwd))
#-----------------------------------------------------#

source("Code/0_settings.R")

df_spss <- readRDS("df_spss")
df_sav <- readRDS("df_sav")

# Identification des variables de satisfaction
vars_satisf <- c("AUTCHOI","EMPLMUN","COMAUT","MOTIFAU","CONSENT","RAISCONS",
                 "PORTUNI","IDMET","IDAPN","METIER","BIENVU","FORMCOM",
                 "BONFORM","PLUSTIR","PLUSSOC","PLUSADM", "PLUSTEC", 
                 "PLUSHUM", "PLUSJUR", "PLUSRAP")

# Distribution a plat des variables de satisfaction (labels explicites)
df_spss %>% tbl_summary(
  include = all_of(vars_satisf),
  statistic = list(all_continuous() ~ "{mean} ({sd})",
                   all_categorical() ~ "{p}% ({n})"),
  label = list(AUTCHOI ~ "Aurait choisi un autre emploi",
               EMPLMUN ~ "Eventualité emploi police privée",
               COMAUT ~ "Métier comme un autre",
               MOTIFAU ~ "Motif métier comme un autre",
               CONSENT ~ "Conseillerait d'entrer dans la police",
               RAISCONS ~ "Motif du conseil",
               PORTUNI ~ "Opinion port de l'uniforme",
               IDMET ~ "Evolution idée du futur métier",
               IDAPN ~ "Evolution idée de la Police Nationale",
               METIER ~ "Idée du métier par rapport aux représentations",
               BIENVU ~ "Considération et avantages",
               FORMCOM ~ "Opinion sur tronc commun",
               BONFORM ~ "Bien préparé aux contextes et aux publics",
               PLUSTIR ~ "Aurait souhaité plus de tir",
               PLUSSOC ~ "Aurait souhaité plus de connaissance de la société",
               PLUSADM ~ "Aurait souhaité plus de connaissances administratives",
               PLUSTEC ~ "Aurait souhaité plus de techniques policières",
               PLUSHUM ~ "Aurait souhaité plus de formation aux relations humaines",
               PLUSJUR ~ "Aurait souhaité plus de connaissances juridiques",
               PLUSRAP ~ "Aurait souhaité plus d'expression écrite")) %>%
  as_gt() %>%
  gt::gtsave(filename = paste0("Code/satisfaction","/satisfaction_spss.html"))

# Distribution a plat des variables de satisfaction (labels numeriques)
df_sav %>% tbl_summary(
  include = all_of(vars_satisf),
  statistic = list(all_continuous() ~ "{mean} ({sd})",
                   all_categorical() ~ "{p}% ({n})"),
  label = list(AUTCHOI ~ "Aurait choisi un autre emploi",
               EMPLMUN ~ "Eventualité emploi police privée",
               COMAUT ~ "Métier comme un autre",
               MOTIFAU ~ "Motif métier comme un autre",
               CONSENT ~ "Conseillerait d'entrer dans la police",
               RAISCONS ~ "Motif du conseil",
               PORTUNI ~ "Opinion port de l'uniforme",
               IDMET ~ "Evolution idée du futur métier",
               IDAPN ~ "Evolution idée de la Police Nationale",
               METIER ~ "Idée du métier par rapport aux représentations",
               BIENVU ~ "Considération et avantages",
               FORMCOM ~ "Opinion sur tronc commun",
               BONFORM ~ "Bien préparé aux contextes et aux publics",
               PLUSTIR ~ "Aurait souhaité plus de tir",
               PLUSSOC ~ "Aurait souhaité plus de connaissance de la société",
               PLUSADM ~ "Aurait souhaité plus de connaissances administratives",
               PLUSTEC ~ "Aurait souhaité plus de techniques policières",
               PLUSHUM ~ "Aurait souhaité plus de formation aux relations humaines",
               PLUSJUR ~ "Aurait souhaité plus de connaissances juridiques",
               PLUSRAP ~ "Aurait souhaité plus d'expression écrite")) %>%
  as_gt() %>%
  gt::gtsave(filename = paste0("Code/satisfaction","/satisfaction_sav.html"))





# Distribution a plat des variables de statisfaction binarisees
df %>% tbl_summary(
  include = all_of(vars_satisf),
  statistic = list(all_continuous() ~ "{mean} ({sd})",
                   all_categorical() ~ "{p}% ({n})"),
  label = list(AUTCHOI ~ "Aurait choisi un autre emploi",
               EMPLMUN ~ "Eventualité emploi police privée",
               COMAUT ~ "Métier comme un autre",
               MOTIFAU ~ "Motif métier comme un autre",
               CONSENT ~ "Conseillerait d'entrer dans la police",
               RAISCONS ~ "Motif du conseil",
               PORTUNI ~ "Opinion port de l'uniforme",
               IDMET ~ "Evolution idée du futur métier",
               IDAPN ~ "Evolution idée de la Police Nationale",
               METIER ~ "Idée du métier par rapport aux représentations",
               BIENVU ~ "Considération et avantages",
               FORMCOM ~ "Opinion sur tronc commun",
               BONFORM ~ "Bien préparé aux contextes et aux publics",
               PLUSTIR ~ "Aurait souhaité plus de tir",
               PLUSSOC ~ "Aurait souhaité plus de connaissance de la société",
               PLUSADM ~ "Aurait souhaité plus de connaissances administratives",
               PLUSTEC ~ "Aurait souhaité plus de techniques policières",
               PLUSHUM ~ "Aurait souhaité plus de formation aux relations humaines",
               PLUSJUR ~ "Aurait souhaité plus de connaissances juridiques",
               PLUSRAP ~ "Aurait souhaité plus d'expression écrite")) %>%
  as_gt() %>%
  gt::gtsave(filename = paste0("Code/satisfaction","/satisfaction_dummies_sav.html"))

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
