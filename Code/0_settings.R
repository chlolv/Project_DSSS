#-----------------------------------------------------#
#
#     Project in Data Science for Social Sciences
#       Settings and packages
#
#                               Chloe Lavest & Yasmine Houri
#                               Academic year 2021-2022
#
#-----------------------------------------------------#

#---------- PACKAGES ----------#

library("haven")
library("plyr")
library("dplyr")
library("stringr")
library("purrr")
library("foreign")
library("data.table")
library("gtsummary")
library("conflicted")
library("tidyverse")
library("haven")
library("cli")
library("labelled")
library("questionr")
library("table1")


conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("mutate", "dplyr")


#---------- USER PATHS ----------#

# Yasmine
# setwd("C:\\Users\\HP\\Documents\\cours_ensae\\3A\\Projet DSSS\\gardiens_paix")
# path <- "C:/Users/HP/Documents/cours_ensae/3A/Projet DSSS/gardiens_paix"

# Chloé
setwd("C:\\Users\\chloe\\OneDrive\\Bureau\\3A\\Projet DSSS")
path <- "gardiens_paix"
