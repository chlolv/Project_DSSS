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
library("dplyr")
library("stringr")
library("purrr")
library("foreign")
library("data.table")
library("conflicted")
library("tidyverse")
library("haven")
library("cli")
library("labelled")
library("questionr")
library("table1")
library("ade4")
library("factoextra")
library("RColorBrewer")
library("effects")
library('xtable')
library("FactoMineR")
library("factoextra")
library("stargazer")

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("rename", "dplyr")


#---------- PATHS ----------#

cwd <- getwd()
setwd(gsub("Code","",cwd))
# path <- "Data"

#---------- DATA LOADING ----------#
df <- readRDS("Data/df_sav")
df[is.na(df)] <- 0

# Ascension sociale, reproduction, déclassement

# Pere
reproduction_p <- unique(df$PROFPERE)[c(1,6,7,9,15,18,19,20,22,23,36,38,39,43)]
declassement_p <- unique(df$PROFPERE)[c(3,11,12,24,26,29,30,31,32,35,42,45)]
ascension_p <- unique(df$PROFPERE)[c(2,4,5,8,10,13,14,17,21,25,28,33,34,37,40,41,44)]

# Mere
reproduction_m <- unique(df$PROFMERE)[c(1,3,7,10,11,12,14,18,25,26,30,31,34,38,41)]
declassement_m <- unique(df$PROFMERE)[c(15,17,20,21,23,32,39,40,42,44)]
ascension_m <- unique(df$PROFMERE)[c(2,4,5,8,9,16,19,22,27,28,29,33,35,36,37,43)]

df <- df %>%
  mutate(repro_mere = case_when(PROFMERE %in% dput(declassement_m) ~ "declassement",
                                PROFMERE %in% dput(reproduction_m) ~ "reproduction",
                                PROFMERE %in% dput(ascension_m) ~ "ascension")) %>%
  mutate(repro_pere = case_when(PROFPERE %in% dput(declassement_p) ~ "declassement",
                                PROFPERE %in% dput(reproduction_p) ~ "reproduction",
                                PROFPERE %in% dput(ascension_p) ~ "ascension")) %>%
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

wave1 <- df %>% filter(vague==1)
wave3 <- df %>% filter(vague==3)
wave5 <- df %>% filter(vague==5)


#------------------- FUNCTIONS ----------------------------------#

table_creation <- function(table1output, title = "", long = 0, location = "")  {
  if (class(table1output)[1] != "table1" | class(table1output)[2] != "html" | class(table1output)[3] !="character") {
    stop("tablea_creation takes as input the result of table1()")
  }
  
  if (long != 0 & long != 1) {
    stop("Long need to be in (0,1)")
  }
  
  
  df <- as.data.frame(table1output)
  # Choosing the layout of the table
  new_table <- xtable(df, caption = title, align = xalign(df))
  digits(new_table) <- 0
  # Converting in .tex 
  #print(paste0(substitute(table1output), "_test",".tex"))
  if (long == 1) {
    add.to.row <- list(pos = list(0), command = NULL)
    command <- paste0("\\hline\n\\endhead\n",
                      "\\hline\n",
                      "\\multicolumn{", dim(new_table)[2] + 1, "}{l}",
                      "{\\footnotesize Continued on next page}\n",
                      "\\endfoot\n",
                      "\\endlastfoot\n")
    add.to.row$command <- command
    print(new_table, file= file.path(location,paste0(substitute(table1output),".tex")), append=F, table.placement = "H", booktabs = TRUE,
          caption.placement="top", hline.after = c(-1,nrow(df)), include.rownames = FALSE,
          include.colnames = TRUE, tabular.environment="longtable", add.to.row = add.to.row, floating = FALSE)
    return(new_table) 
  } else {
    print(new_table, file= file.path(location,paste0(substitute(table1output),".tex")), append=F, table.placement = "H", booktabs = TRUE,
          caption.placement="top", hline.after = c(-1,nrow(df)), include.rownames = FALSE,
          include.colnames = TRUE)
    return(new_table)
  }
  
}


my.render.cat <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y,
                                                  sprintf("%0.0f %%", PCTnoNA))))
}


reg_controles <- function(df, var_dep, var_indep, controles, ...) {
  mod_summaries <- list()
  models <- list()
  controles <- append(var_indep, controles)
  df_reg <- df %>%
    select(all_of(var_dep), all_of(controles))
  for(i in 2:ncol(df_reg)) {                 
    predictors_i <- colnames(df_reg)[2:i]    
    models[[i - 1]] <- lm(df_reg[,all_of(var_dep)] ~ . , data = df_reg[all_of(predictors_i)], 
                          na.action = na.omit, ...)
    mod_summaries[[i-1]] <- summary(lm(df_reg[,all_of(var_dep)] ~ . , data = df_reg[all_of(predictors_i)], 
                                       na.action = na.omit, ...))
  }
  return_list = c("models" = list(models), "summaries" = list(mod_summaries))
  #return(models)
  #return(mod_summaries)
  return(return_list)
  
}


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))