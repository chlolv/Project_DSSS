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


#---------- PATHS ----------#

cwd <- getwd()
setwd(gsub("Code","",cwd))
path <- "Data"
