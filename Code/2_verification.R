#-----------------------------------------------------#
#
#     Project in Data Science for Social Sciences
#       Descriptive statistics for the computation of and indicator of satisfaction
#
#                               Chloe Lavest & Yasmine Houri
#                               Academic year 2021-2022
#
#-----------------------------------------------------#
rm(list = ls())

# Import packages
library("haven")
library("plyr")
library("dplyr")
library("stringr")
library("purrr")
library("foreign")
library("data.table")
library("gtsummary")
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
setwd("C:\\Users\\chloe\\OneDrive\\Bureau\\3A\\Projet DSSS")
path <- "gardiens_paix"


df_spss <- readRDS("df_spss")
df_sav <- readRDS("df_sav")

# Identification des variables de satisfaction
vars_satisf <- c("AUTCHOI","EMPLMUN","COMAUT","MOTIFAU","CONSENT","RAISCONS",
                 "PORTUNI","IDMET","IDAPN","METIER","BIENVU","FORMCOM",
                 "BONFORM","PLUSTIR","PLUSSOC","PLUSADM", "PLUSTEC", 
                 "PLUSHUM", "PLUSJUR", "PLUSRAP")

df <- df_sav


df$AUTCHOI[which(df$AUTCHOI != 1)] <- 0
# df_sav
# AUTCHOIX == 1 : Oui j'aurais choisi un autre emploi (1322)
# AUTCHOIX == 2 : Non je n'aurais pas choisi un autre emploi (2122)
# AUTCHOIX == NA : 1812
# df
# AUTCHOIX == 0 : Non je n'aurais pas choisi un autre emploi (2122)
# AUTCHOIX == 1 : Oui j'aurais choisi un autre emploi (2122)
# AUTCHOIX == NA : 1812
# Pour les analyses, les valeurs élevées dénotent la NON satisfaction


df$CONSENT[which(df$CONSENT == 2)] <- 1 # initially 1 was positive and 0 negative
df$CONSENT[which(df$CONSENT != 1)] <- 0 # need to go back to this bc it seems to me that there is an issue
# df_sav
# CONSENT == 1 : Oui je conseillerais cet emploi (2554)
# CONSENT == 2 : Non je ne conseillerais pas cet emploi (881)
# CONSENT == NA : 1821
# df
# CONSENT == 0 : Aucun
# CONSENT == 1 : 3435
# CONSENT == NA : 1821

df$IDMET[which(df$IDMET != 2)] <- 0
df$IDMET[which(df$IDMET == 2)] <- 1
# df_sav
# IDMET == 1 : Mon idée a évolué positivement (1036)
# IDMET == 2 : Mon idée a évolué négativement (760)
# IDMET == 3 : Mon idée n'a pas évolué (1598)
# IDMET == NA : 1862
# df
# IDMET == 0 : Mon idée n'a pas évolué/a évolué positivement (2634)
# IDMET == 1 : Mon idée a évolué négativement (760)
# IDMET == NA : 1821 (1862)


df$IDAPN[which(df$IDAPN != 2)] <- 0 # ok
df$IDAPN[which(df$IDAPN == 2)] <- 1

df$METIER[which(df$METIER != 3)] <- 0 # ok
df$METIER[which(df$METIER == 3)] <- 1