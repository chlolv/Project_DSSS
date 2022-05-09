#-----------------------------------------------------#
#
#     Project in Data Science for Social Sciences
#       Data preprocessing for the computation of and indicator of satisfaction
#
#                               Chloe Lavest & Yasmine Houri
#                               Academic year 2021-2022
#
#
#
#           MAIN SCRIPT
#
#
#-----------------------------------------------------#


rm(list = ls())


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
library("rstudioapi")


conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("mutate", "dplyr")


#---------- PATHS ----------#

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#setwd(gsub("Code","",getwd()))
path <- "Data"


source("1_data_preprocessing.R")

source("1_socialisation_init.R")

source("2_distribution_vars_satisf.R")

source("3_pre_processing_for_regs.R")
