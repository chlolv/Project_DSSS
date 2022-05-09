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

#---------- PATHS ----------#

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(gsub("Code","",getwd()))
path <- "Data"

source("Code/0_settings.R")

# Identification des variables de satisfaction
vars_satisf <- c("AUTCHOI","EMPLMUN","COMAUT","MOTIFAU","CONSENT","RAISCONS",
                 "PORTUNI","IDMET","IDAPN","METIER","BIENVU","FORMCOM",
                 "BONFORM","PLUSTIR","PLUSSOC","PLUSADM", "PLUSTEC", 
                 "PLUSHUM", "PLUSJUR", "PLUSRAP")

files <- list.files(path,pattern="gardien")
data_spss <- list()
i = 1
for (file in files){
  data_spss[[i]] <- read.spss(paste0(path,"/",file))
  i=i+1
}
#dataset_spss <- Reduce(function(...) merge(..., all=T), data_spss)

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
names(vague2) <- gsub("^B", "", names(vague2))

# Traitements vague 3
vague3 <- as.data.frame(data_spss[[3]])
vague3$vague <- 3
names(vague3) <- gsub("^C", "", names(vague3))
names(vague3)[names(vague3) == 'IDENTIT'] <- 'CIDENTIT'

# Traitements vague 4
vague4 <- as.data.frame(data_spss[[4]])
vague4$vague <- 4
names(vague4) <- gsub("^E", "", names(vague4))

# Traitements vague 5
vague5 <- as.data.frame(data_spss[[5]])
vague5$vague <- 5
names(vague5) <- gsub("^F", "", names(vague5))

# Traitements vague 6
vague6 <- as.data.frame(data_spss[[6]])
vague6$vague <- 6
names(vague6) <- gsub("^G", "", names(vague6))

# Rbind: creation d'un dataset reunissant toutes les vagues
df_spss <- rbind.fill(vague1,vague2,vague3,vague4,vague5,vague6)
df_spss <- df_spss %>% relocate(vague, .after = CIDENTIT)

# Save file
saveRDS(df_spss, "Data/df_spss")

#saveRDS(df_spss, "..\\df_spss")

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
vague1 <- as.data.frame(data_spss[[1]])
vague1$vague <- 1
names(vague1)[which(names(vague1)=="AUTCHOIX")] <- "AUTCHOI"
names(vague1)[which(names(vague1)=="MIEUPREP")] <- "MIEUPRE"
names(vague1)[which(names(vague1)=="EMPLMUNI")] <- "EMPLMUN"
names(vague1)[which(names(vague1)=="PORTUNIF")] <- "PORTUNI"

# Traitements vague 2
vague2 <- as.data.frame(data_spss[[2]])
vague2$vague <- 2
names(vague2) <- gsub("^B", "", names(vague2))

# Traitements vague 3
vague3 <- as.data.frame(data_spss[[3]])
vague3$vague <- 3
names(vague3) <- gsub("^C", "", names(vague3))
names(vague3)[names(vague3) == 'IDENTIT'] <- 'CIDENTIT'

# Traitements vague 4
vague4 <- as.data.frame(data_spss[[4]])
vague4$vague <- 4
names(vague4) <- gsub("^E", "", names(vague4))

# Traitements vague 5
vague5 <- as.data.frame(data_spss[[5]])
vague5$vague <- 5
names(vague5) <- gsub("^F", "", names(vague5))

# Traitements vague 6
vague6 <- as.data.frame(data_spss[[6]])
vague6$vague <- 6
names(vague6) <- gsub("^G", "", names(vague6))

# Rbind: creation d'un dataset reunissant toutes les vagues
df_sav <- rbind.fill(vague1,vague2,vague3,vague4,vague5,vague6)
df_sav <- df_sav %>% relocate(vague, .after = CIDENTIT)

# Binarisation des variables conservees pour l'indicateur

df <- df_sav

df$AUTCHOI[which(df$AUTCHOI != 1)] <- 0

df$EMPLMUN[which(df$EMPLMUN == 4)] <- 0
df$EMPLMUN[which(df$EMPLMUN != 0)] <- 1

df$COMAUT[which(df$COMAUT != 1)] <- 0

df$CONSENT[which(df$CONSENT == 2)] <- 1 # 
df$CONSENT[which(df$CONSENT != 2)] <- 0 # resolved an issue here

df$PORTUNI[which(df$PORTUNI != 1)] <- 0

df$IDMET[which(df$IDMET != 2)] <- 0
df$IDMET[which(df$IDMET == 2)] <- 1

df$IDAPN[which(df$IDAPN != 2)] <- 0
df$IDAPN[which(df$IDAPN == 2)] <- 1

df$METIER[which(df$METIER != 3)] <- 0
df$METIER[which(df$METIER == 3)] <- 1

df$BIENVU[which(df$BIENVU != 2)] <- 0
df$BIENVU[which(df$BIENVU == 2)] <- 1

df$FORMCOM[which(df$FORMCOM != 2)] <- 0
df$FORMCOM[which(df$FORMCOM == 2)] <- 1

df$BONFORM[which(df$BONFORM != 2)] <- 0
df$BONFORM[which(df$BONFORM == 2)] <- 1

for (col in vars_satisf[14:20]){
  df[which(df[,col] != 2),col] <- 0
  df[which(df[,col] == 2),col] <- 1
}

# Save file
saveRDS(df_sav, "df_sav")

# Save file
saveRDS(df_sav, "Data/df_sav")

#saveRDS(df_spss, "..\\df_sav")

