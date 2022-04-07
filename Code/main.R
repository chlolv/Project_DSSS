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

source("0_settings.R")

source("1_data_preprocessing.R")
source("1_socialisation_init.R")

source("2_distribution_vars_satif.R")

source("3_pre_processing_for_reg.R")

# Les scripts de régression sont à nettoyer mais je propose d'attendre qu'on sache quels modèles on garde
# Le nom de la variable de score de satisfaction a été modifié pour satis.5 (et le z_score s'appelle mnt
# z_satis.5)