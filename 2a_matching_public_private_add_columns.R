##############################################
#### unir outras colunas na base de match ####
##############################################

# abrir base final com match
# para cada ano, ficar só com PIS e CBO e fazer lista
# abrir a base da RAIS de cada ano e selecionar as colunas de interesse
# filtrar na RAIS de cada ano esses PIS-CBO

# fazer uma lista dos CNPJs de cada ano
# abrir a base da RAIS estabelecimentos e ficar com CNPJ e Nome
# juntar o nome na base de indivíduos.

#####################
#### DEFINITIONS ####
#####################
rm(list=ls())
gc()

library("tidyverse")
library("readxl")
library("data.table")

# Para facilitar
select <- dplyr::select
filter <- dplyr::filter
`%notin%` = Negate(`%in%`)

personal_directory <- "C:/Users/taina/Desktop/RAIS/matching/dec2021/"
directory <- "C:/Users/taina/Desktop/RAIS/"

years <- as.character(c(1998:2004))

##################################
#### FUNCTION TO OPEN RAIS DF ####
##################################
open_files <- function(years_files = NA_character_, file_to_read = NA_character_){
  if(years_files == "2002"){
    df <- fread(file_to_read, 
                sep=";", 
                header=TRUE, 
                na.strings="NA",
                showProgress = TRUE, 
                colClasses = "character",
                select = c("OCUPACAO", "PIS", 
                           "DT ADMISSAO", "RADIC CNPJ", "GRAU INSTR", "MES DESLIG", "CAUSA DESLI", "MUNICIPIO",
                           "SEXO", "TEMP EMPR", "TP VINCL", "EMP EM 31/12", "DT NASCIMENT")) %>% 
      rename(cbo = `OCUPACAO`,
             date_adm = `DT ADMISSAO`,
             CNPJ_root = `RADIC CNPJ`,
             educ = `GRAU INSTR`,
             month_quit = `MES DESLIG`,
             reason_quit = `CAUSA DESLI`,
             municipality = MUNICIPIO,
             gender = SEXO,
             time_employed = `TEMP EMPR`,
             tp_contract = `TP VINCL`,
             employed_dec = `EMP EM 31/12`,
             b_date = `DT NASCIMENT`) %>%
      mutate(cbo = str_replace(cbo, "CBO ", ""),
             age = as.numeric(years_files) - as.numeric(substr(b_date,(nchar(b_date)+1)-4,nchar(b_date))),
             date_adm = ifelse(nchar(date_adm) == 7, paste0("0", date_adm), date_adm),
             date_adm = as.Date(date_adm, format = "%d%m%Y"),
             date_quit = ifelse(month_quit == "00", NA,
                                as.Date(paste0("30/", month_quit, "/", years_files), format = "%d/%m/%Y")),
             CNPJ_root_std = base::sprintf("%08.0f", as.numeric(as.character(CNPJ_root)))) %>%
      select(-b_date, -month_quit)
    
  } else if (years_files %in% as.character(1995:1998)){
    df <- fread(file_to_read, 
                sep=";", 
                header=TRUE, 
                na.strings="NA",
                showProgress = TRUE, 
                colClasses = "character",
                select = c("CBO 94 Ocupação", "PIS", 
                           "Ano Admissão", "CNPJ Raiz", "Grau Instrução 2005-1985", "Mês Admissão",
                           "Mês Desligamento", "Motivo Desligamento", "Município", "Sexo Trabalhador", "Tempo Emprego",
                           "Tipo Vínculo", "Vínculo Ativo 31/12", "Idade", "Tipo Admissão")) %>%
      rename(cbo = `CBO 94 Ocupação`,
             year_adm = `Ano Admissão`,
             CNPJ_root = `CNPJ Raiz`,
             educ = `Grau Instrução 2005-1985`,
             month_adm = `Mês Admissão`,
             month_quit = `Mês Desligamento`,
             reason_quit = `Motivo Desligamento`,
             municipality = `Município`,
             gender = `Sexo Trabalhador`,
             time_employed = `Tempo Emprego`,
             tp_contract = `Tipo Vínculo`,
             employed_dec = `Vínculo Ativo 31/12`,
             age = `Idade`) %>%
      mutate(cbo = str_replace(cbo, "CBO ", ""),
             date_adm = as.Date(paste0("01/", month_adm, "/", year_adm), format = "%d/%m/%Y"),
             date_quit = ifelse(month_quit == "00", NA,
                                as.Date(paste0("30/", month_quit, "/", years_files), format = "%d/%m/%Y")),
             CNPJ_root_std = base::sprintf("%08.0f", as.numeric(as.character(CNPJ_root)))) %>% 
      select(-month_adm, -year_adm, -month_quit)
    
  } else if (years_files %in% as.character(c(1999:2001))){
    df <- fread(file_to_read, 
                sep=";", 
                header=TRUE, 
                na.strings="NA",
                showProgress = TRUE, 
                colClasses = "character",
                select = c("CBO 94 Ocupação", "PIS", 
                           "Ano Admissão", "CNPJ Raiz", "Grau Instrução 2005-1985", "Mês Admissão",
                           "Mês Desligamento", "Motivo Desligamento", "Município", "Sexo Trabalhador", 
                           "Tempo Emprego", "Tipo Vínculo", "Vínculo Ativo 31/12", "Idade", "Tipo Admissão")) %>%
      rename(cbo = `CBO 94 Ocupação`,
             year_adm = `Ano Admissão`,
             CNPJ_root = `CNPJ Raiz`,
             educ = `Grau Instrução 2005-1985`,
             month_adm = `Mês Admissão`,
             month_quit = `Mês Desligamento`,
             reason_quit = `Motivo Desligamento`,
             municipality = `Município`,
             gender = `Sexo Trabalhador`,
             time_employed = `Tempo Emprego`,
             tp_contract = `Tipo Vínculo`,
             employed_dec = `Vínculo Ativo 31/12`,
             age = `Idade`)%>%
      mutate(cbo = str_replace(cbo, "CBO ", ""),
             date_adm = as.Date(paste0("01/", month_adm, "/", year_adm), format = "%d/%m/%Y"),
             date_quit = ifelse(month_quit == "00", NA,
                                as.Date(paste0("30/", month_quit, "/", years_files), format = "%d/%m/%Y")),
             CNPJ_root_std = base::sprintf("%08.0f", as.numeric(as.character(CNPJ_root)))) %>%
      select(-month_adm, -year_adm, -month_quit)
    
  } else{
    df <- fread(file_to_read, 
                sep=";", 
                header=TRUE, 
                na.strings="NA",
                showProgress = TRUE, 
                colClasses = "character",
                select = c("OCUPACAO 94", "PIS", 
                           "HORAS CONTR", "REM DEZ (R$)", "REM MED (R$)", "ULT REM", "SAL CONTR"
                           ,"DT ADMISSAO", "RADIC CNPJ", "GRAU INSTR", "MES DESLIG", "CAUSA DESLI",
                           "MUNICIPIO", "SEXO", "TEMP EMPR", "TP VINCULO", "EMP EM 31/12", "DT NASCIMENT")) %>% 
      rename(cbo = `OCUPACAO 94`,
             date_adm = `DT ADMISSAO`,
             CNPJ_root = `RADIC CNPJ`,
             educ = `GRAU INSTR`,
             month_quit = `MES DESLIG`,
             reason_quit = `CAUSA DESLI`,
             municipality = MUNICIPIO,
             gender = SEXO,
             time_employed = `TEMP EMPR`,
             tp_contract = `TP VINCULO`,
             employed_dec = `EMP EM 31/12`,
             b_date = `DT NASCIMENT`) %>%
      mutate(cbo = str_replace(cbo, "CBO ", ""),
             age = as.numeric(years_files) - as.numeric(substr(b_date,(nchar(b_date)+1)-4,nchar(b_date))),
             date_adm = ifelse(nchar(date_adm) == 7, paste0("0", date_adm), date_adm),
             date_adm = as.Date(date_adm, format = "%d%m%Y"),
             date_quit = ifelse(month_quit == "00", NA,
                                as.Date(paste0("30/", month_quit, "/", years_files), format = "%d/%m/%Y")),
             CNPJ_root_std = base::sprintf("%08.0f", as.numeric(as.character(CNPJ_root)))) %>%
      select(-b_date, -month_quit)
  }
  
  return(df)
}

####################################################################
#### OPEN FINAL MATCH DF TO GET PIS AND CBO OF EACH OBSERVATION ####
####################################################################
list_PIS_cbo <- list()

for (i in seq_along(years)) {
  list_PIS_cbo[[i]] <- readRDS(paste0(personal_directory, "list_final_df/final_df_", years[i], ".RDS")) %>% 
    select(cbo, PIS) %>% 
    mutate(PIS_cbo = paste0(PIS, "-", cbo)) %>% 
    pull(PIS_cbo)
}

##########################################################
#### OPEN RAIS AND FILTER FOR THE PIS-CBO OF INTEREST ####
##########################################################
list_cnpj <- list()
for(i in seq_along(years)){
  
  print(years[i])
  
  # list files for each year
  files <- grep(list.files(path = paste0(directory, years[i]), 
                           all.files = FALSE,
                           full.names = TRUE), 
                pattern= ('Estb|ESTB|IGNORADO'), 
                inv=TRUE, value=TRUE)
  
  PIS_cbo_vec <- list_PIS_cbo[[i]]
  
  list_data_temp <- list()
  for(j in seq_along(files)) { 
    # open RAIS file
    list_data_temp[[j]] <- open_files(years_file = years[i], file_to_read = files[j]) %>%
      # filter for corresponding PIS-cbo
      mutate(PIS_cbo = paste0(PIS, "-", cbo)) %>% 
      filter(PIS_cbo %in% PIS_cbo_vec) 
    
    print(j)
  }
  
  #combine all RAIS data
  df_RAIS <- bind_rows(list_data_temp) %>% select(-PIS_cbo)
  
  # open matched data
  df_match <- readRDS(paste0(personal_directory, "list_final_df/final_df_", years[i], ".RDS")) %>% 
    # merge RAIS columns into matched df
    left_join(df_RAIS, by = c("PIS", "cbo"))
  
  # save a "full df" for each year
  saveRDS(df_match, paste0(personal_directory, "list_final_df_full/final_df_full_", years[i], ".RDS"))
  
  # save all CNPJ for each year
  list_cnpj[[i]] <- df_RAIS %>% distinct(CNPJ_root) %>% pull(CNPJ_root)
  
  # clean environment
  remove(df_RAIS, df_match, list_data_temp)
}

#######################################################
#### GETTING CNPJ NAMES FROM RAIS ESTABELECIMENTOS ####
#######################################################
nat_jur_fed <- as.character(c(1015, 1040, 1074, 1104, 1139, 1163, 1252, 10))

years_names_cnpj <- c("2002", "2003", "2004")
# SÓ TEM NOME EM 2002, 2003 e 2004, e parece que só em 2002 está correto
list_cnpj_names <- list()
for(i in seq_along(years_names_cnpj)){
  
  print(years_names_cnpj[i])
  
  # list files for each year
  files <- grep(list.files(path = paste0(directory, years_names_cnpj[i]), 
                           all.files = FALSE,
                           full.names = TRUE), 
                pattern= ('Estb|ESTB'), 
                inv=FALSE, value=TRUE)
  
  list_files <- list()
  for (j in seq_along(files)) {
    list_files[[j]] <- read_delim(files[j], 
                                  "|",
                                  escape_double = FALSE,
                                  col_types = cols(`CLAS CNAE 95` = col_character(), 
                                                   `NATUR JUR` = col_character(), 
                                                   `RADIC CNPJ` = col_character(), 
                                                   `RAZAO SOCIAL` = col_character()),
                                  locale = locale(decimal_mark = ",", grouping_mark = ".", 
                                                  encoding = "WINDOWS-1252"),
                                  trim_ws = TRUE) %>% 
      rename(CNPJ_root = `RADIC CNPJ`,
             agency_name = `RAZAO SOCIAL`,
             cnae95 = `CLAS CNAE 95`,
             natjur = `NATUR JUR`) %>% 
      select(CNPJ_root, agency_name, cnae95, natjur) %>% 
      filter(natjur %in% nat_jur_fed & cnae95 == "75116")
      
  }
  
  list_cnpj_names[[i]] <- bind_rows(list_files) %>% 
    mutate(year = years_names_cnpj[i],
           CNPJ_root_std = base::sprintf("%08.0f", as.numeric(as.character(CNPJ_root)))) %>% 
    filter(agency_name != "TESTE") 
  
}

cnpj_names <- bind_rows(list_cnpj_names) %>% 
  count(CNPJ_root_std, agency_name) %>% 
  group_by(CNPJ_root_std) %>% 
  slice(which.max(n)) %>% 
  ungroup()

# adding an old dataset with CNPJ names (from code "analise_paula.Rmd")
rais_panel_public <- readRDS(paste0(directory,"_misc/rais_panel_public.Rds")) %>% 
  distinct(cnpj_raiz_std, name) %>% 
  rename(CNPJ_root_std = cnpj_raiz_std,
         agency_name = name)

CNPJs_filter <- rais_panel_public$CNPJ_root_std

cnpj_names <- cnpj_names %>% 
  select(-n) %>% 
  filter(CNPJ_root_std %notin% CNPJs_filter) %>% 
  bind_rows(rais_panel_public) %>% 
  distinct(CNPJ_root_std, .keep_all = TRUE) %>% 
  mutate(agency_name = ifelse(agency_name == "ISC", "TRIBUNAL DE CONTAS DA UNIAO", agency_name))

write.csv2(cnpj_names, file = paste0(directory, "matching/dec2021/all_cnpj_names.csv"))

################################################
#### INCLUDING CNPJ NAMES IN THE FINAL DATA ####
################################################
for (i in seq_along(years)) {
  df <- readRDS(file = paste0(personal_directory, "list_final_df_full/final_df_full_", years[i], ".RDS")) %>% 
    mutate(CNPJ_root_std = base::sprintf("%08.0f", as.numeric(as.character(CNPJ_root)))) %>%
    filter(PIS != "00000000000") %>% 
    distinct(PIS, cbo, CNPJ_root_std, .keep_all = TRUE) %>% 
    left_join(cnpj_names, by = "CNPJ_root_std")
    
  df_cnpj_na <- df %>% filter(is.na(agency_name))
  
  print(nrow(df_cnpj_na %>% distinct(CNPJ_root_std)))
  print(nrow(df %>% distinct(CNPJ_root_std)))
  
  # save a "full df" for each year
  saveRDS(df, paste0(personal_directory, "list_final_df_full/final_df_full_", years[i], ".RDS"))
  saveRDS(df_cnpj_na, paste0(personal_directory, "list_final_df_full/final_df_full_cnpj_na_", years[i], ".RDS"))
}

###############################################################
#### FILTERING ONLY FOR AGENCIES USED IN THE EXPERT SURVEY ####
###############################################################
load(paste0(directory, "matching/dec2021/agencies_manifesto.RData")) 
write.csv2(ag_scores, file = paste0(directory, "matching/dec2021/all_agencies_exp_survey.csv"))

CNPJ_exp_sur <- read.csv2(paste0(directory, "matching/dec2021/all_cnpj_names_expert_survey.csv")) %>% 
  mutate(CNPJ_root_std = base::sprintf("%08.0f", as.numeric(as.character(CNPJ_root_std)))) %>% 
  filter(expert_survey == 1) %>% 
  select(-X, -agency_name)

# nrow(CNPJ_exp_sur %>% distinct(agency_name)) # hoje tenho uma base com 70 nomes

agencies <- list()
for (i in seq_along(years)) {
  print(years[i])
  
  df <- readRDS(file = paste0(personal_directory, "list_final_df_full/final_df_full_", years[i], ".RDS")) 
  
  print(nrow(df))
  
  df <- df %>% 
    left_join(CNPJ_exp_sur, by = "CNPJ_root_std") %>% 
    filter(expert_survey == 1)
  
  print(nrow(df))
  print(nrow(df %>% distinct(agency_name)))
  
  CNPJ_df <- df %>% distinct(CNPJ_root_std) %>% pull(CNPJ_root_std)
  
  agencies[[i]] <- CNPJ_exp_sur %>% filter(CNPJ_root_std %in% CNPJ_df) %>% 
    mutate(year = years[i])
  
  # save a "full df" for each year
  saveRDS(df, paste0(personal_directory, "list_final_df_full_exp_survey/final_df_full_exp_sur_", years[i], ".RDS"))
}

CNPJ_exp_sur <- read.csv2(paste0(directory, "matching/dec2021/all_cnpj_names_expert_survey.csv")) %>% 
  mutate(CNPJ_root_std = base::sprintf("%08.0f", as.numeric(as.character(CNPJ_root_std)))) %>% 
  filter(expert_survey == 1) %>% 
  select(-X, -expert_survey)

for(i in seq_along(agencies)){
  df <- agencies[[i]] %>% 
    select(-expert_survey)
  
  names(df) <- c("CNPJ_root_std", paste0("year_", years[i]))
  
  CNPJ_exp_sur <- CNPJ_exp_sur %>% 
    left_join(df, by = "CNPJ_root_std")
}

write.csv2(CNPJ_exp_sur, file = paste0(directory, "matching/dec2021/all_cnpj_names_expert_survey_years.csv"))

####################################################
#### EXPORTING ANONIMIZED DATA TO SEND TO PAULA ####
####################################################
pis_vec <- c()

for (i in seq_along(years)) {
  df <- readRDS(paste0(personal_directory, "list_final_df_full_exp_survey/final_df_full_exp_sur_", years[i], ".RDS"))
  
  pis_vec <- unique(c(pis_vec, (df %>% pull(PIS))))
}

set.seed(05585040)

pis_ID <- tibble("PIS" = pis_vec) %>% 
  distinct(PIS) %>% 
  mutate(ID_fake = sample(length(pis_vec)))


for (i in seq_along(years)) {
  df <- readRDS(paste0(personal_directory, "list_final_df_full_exp_survey/final_df_full_exp_sur_", years[i], ".RDS")) %>% 
    left_join(pis_ID, by = "PIS") %>% 
    select(-PIS)
  
  saveRDS(df, paste0(personal_directory, "list_final_df_full_exp_survey_anon/final_df_full_exp_sur_anon_", 
                     years[i], ".RDS"))
}
