#-----------------------------------------------------#
#
#     Project in Data Science for Social Sciences
#       Data preprocessing for the computation of and indicator of satisfaction
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

# Creation base spss (labels explicites)
# Import data
# Yasmine
# setwd("C:\\Users\\HP\\Documents\\cours_ensae\\3A\\Projet DSSS\\gardiens_paix")
# path <- "C:/Users/HP/Documents/cours_ensae/3A/Projet DSSS/gardiens_paix"

# Chloé
setwd("C:\\Users\\chloe\\OneDrive\\Bureau\\3A\\Projet DSSS")
path <- "gardiens_paix"


files <- list.files(path,pattern="gardien")
data_spss <- list()
i = 1
for (file in files){
  data_spss[[i]] <- read.spss(paste0(path,"/",file))
  i=i+1
}
dataset_spss <- Reduce(function(...) merge(..., all=T), data_spss)

# Traitements vague 1
vague1 <- as.data.frame(data_spss[[1]])
vague1$vague <- 1
names(vague1)[which(names(vague1)=="AUTCHOIX")] <- "AUTCHOI"
names(vague1)[which(names(vague1)=="MIEUPREP")] <- "MIEUPRE"
names(vague1)[which(names(vague1)=="EMPLMUNI")] <- "EMPLMUN"
names(vague1)[which(names(vague1)=="PORTUNIF")] <- "PORTUNI"

# Traitements vague 2
vague2 <- as.data.frame(data_spss[[2]])
vague2$vague <- 2
names(vague2) <- gsub("B", "", names(vague2))

# Traitements vague 3
vague3 <- as.data.frame(data_spss[[3]])
vague3$vague <- 3
names(vague3) <- gsub("C", "", names(vague3))

# Traitements vague 4
vague4 <- as.data.frame(data_spss[[4]])
vague4$vague <- 4
names(vague4) <- gsub("E", "", names(vague4))

# Traitements vague 5
vague5 <- as.data.frame(data_spss[[5]])
vague5$vague <- 5
names(vague5) <- gsub("F", "", names(vague5))

# Traitements vague 6
vague6 <- as.data.frame(data_spss[[6]])
vague6$vague <- 6
names(vague6) <- gsub("G", "", names(vague6))

# Rbind: creation d'un dataset reunissant toutes les vagues
df_spss <- rbind.fill(vague1,vague2,vague3,vague4,vague5,vague6)
df_spss <- df_spss %>% relocate(vague, .after = CIDENTIT)

# Save file
saveRDS(df_spss, "df_spss")

################################################################################

# Creation base sav (labels recodes)
# Import data
data_sav <- list()
i = 1
for (file in files){
  data_sav[[i]] <- read_sav(paste0(path,"/",file))
  i=i+1
}
dataset_sav <- Reduce(function(...) merge(..., all=T), data_sav)

# Traitements vague 1
vague1 <- as.data.frame(data_sav[[1]])
vague1$vague <- 1
names(vague1)[which(names(vague1)=="AUTCHOIX")] <- "AUTCHOI"
names(vague1)[which(names(vague1)=="MIEUPREP")] <- "MIEUPRE"
names(vague1)[which(names(vague1)=="EMPLMUNI")] <- "EMPLMUN"
names(vague1)[which(names(vague1)=="PORTUNIF")] <- "PORTUNI"

# Traitements vague 2
vague2 <- as.data.frame(data_sav[[2]])
vague2$vague <- 2
names(vague2) <- gsub("B", "", names(vague2))

# Traitements vague 3
vague3 <- as.data.frame(data_sav[[3]])
vague3$vague <- 3
names(vague3) <- gsub("C", "", names(vague3))

# Traitements vague 4
vague4 <- as.data.frame(data_sav[[4]])
vague4$vague <- 4
names(vague4) <- gsub("E", "", names(vague4))

# Traitements vague 5
vague5 <- as.data.frame(data_sav[[5]])
vague5$vague <- 5
names(vague5) <- gsub("F", "", names(vague5))

# Traitements vague 6
vague6 <- as.data.frame(data_sav[[6]])
vague6$vague <- 6
names(vague6) <- gsub("G", "", names(vague6))

# Rbind: creation d'un dataset reunissant toutes les vagues
df_sav <- rbind.fill(vague1,vague2,vague3,vague4,vague5,vague6)
df_sav <- df_sav %>% relocate(vague, .after = CIDENTIT)

# Save file
saveRDS(df_sav, "df_sav")

