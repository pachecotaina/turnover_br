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

# ################################################################### #
####   OPEN FINAL MATCH DF TO GET PIS AND CBO OF EACH OBSERVATION  ####
# ################################################################### #
list_PIS_cbo <- list()

for (i in seq_along(years)) {
  list_PIS_cbo[[i]] <- readRDS(paste0(personal_directory, 
                                      "list_final_df_full_exp_survey_anon/final_df_full_exp_sur_anon_", 
                                      years[i], ".RDS")) %>% 
    distinct(cbo, type, year)
}

df_cbo <- bind_rows(list_PIS_cbo)

table(df_cbo$type, df_cbo$year)

# for most of the CBOs the matching was direct


# ########################################################################## #
####      FINDING MATCHES FOR WHEN THE MATCHING IS INDIRECT - SAME PIS    ####
# ########################################################################## #
list_PIS_cbo <- list()
for (i in seq_along(years)) {
  list_PIS_cbo[[i]] <- readRDS(paste0(personal_directory, 
                                      "list_final_df_full_exp_survey/final_df_full_exp_sur_", 
                                      years[i], ".RDS")) #%>% 
    #distinct(PIS, cbo, type, year)
}

df_cbo_pis <- bind_rows(list_PIS_cbo)

df_cbo_pis %>% 
  count(year, type) %>% 
  group_by(year) %>% 
  mutate(nn = sum(n),
         pct = n/nn) %>% 
  select(-n, -nn) %>% 
  pivot_wider(names_from = year, values_from = pct)

table(df_cbo_pis$year, df_cbo_pis$type)


list_PIS_past_years <- list()
for (i in seq_along(years)) {
  df_aux <- readRDS(paste0(personal_directory,
                                             "list_cbo_match_wages_public_past_years/cbo_match_wages_public_past_years_", 
                                             years[i], ".RDS")) %>% 
    select(PIS, cbo_original, cbo) %>% 
    rename(cbo_match = cbo,
           cbo = cbo_original)
  
  vec_pis <- unique(df_aux$PIS)
  
  list_PIS_past_years[[i]] <- list_PIS_cbo[[i]] %>% 
    filter(PIS %in% vec_pis) %>% 
    left_join(df_aux, by = "PIS") %>% 
    select(PIS, cbo.x, cbo.y, cbo_match, type, year) %>% 
    filter(type != "Direct match by CBO")
}

df_pis_past_years <- bind_rows(list_PIS_past_years) %>% 
  count(cbo.x, cbo_match, year)

write.csv(df_pis_past_years,
          paste0(personal_directory, "cbo_matching_type2_and3.csv"))





# ########################################################################## #
####                INCLUDING CBO MATCH IN THE FINAL DF                   ####
# ########################################################################## #
list_PIS_cbo <- list()
for (i in seq_along(years)) {
  list_PIS_cbo[[i]] <- readRDS(paste0(personal_directory, 
                                      "list_final_df_full_exp_survey/final_df_full_exp_sur_", 
                                      years[i], ".RDS"))
}

list_PIS_past_years <- list()
for (i in seq_along(years)) {
  df_aux <- readRDS(paste0(personal_directory,
                           "list_cbo_match_wages_public_past_years/cbo_match_wages_public_past_years_", 
                           years[i], ".RDS")) %>% 
    select(PIS, cbo_original, cbo) %>% 
    rename(cbo_match = cbo,
           cbo = cbo_original)
  
  vec_pis <- unique(df_aux$PIS)
  
  list_PIS_past_years[[i]] <- list_PIS_cbo[[i]] %>% 
    filter(PIS %in% vec_pis) %>% 
    filter(type != "Direct match by CBO") %>% 
    left_join(df_aux, by = "PIS") %>% 
    select(PIS, cbo.x, cbo_match) %>% 
    distinct(PIS, cbo.x, .keep_all = TRUE) %>% 
    rename(cbo = cbo.x)
}


for (i in seq_along(years)) {
  list_PIS_cbo[[i]] <- list_PIS_cbo[[i]] %>% 
    left_join(list_PIS_past_years[[i]], by = c("PIS", "cbo")) %>% 
    mutate(cbo_match = ifelse(type == "Direct match by CBO", cbo, cbo_match))
}

# teste <- list_PIS_cbo[[i]] 
# 
# teste %>%
#   count(is.na(cbo_match), type)
# 
# teste1 <- readRDS(paste0(personal_directory, 
#                          "list_final_df_full_exp_survey/final_df_full_exp_sur_", 
#                          years[i], ".RDS")) 

pis_vec <- c()
for (i in seq_along(years)) {
  df <- list_PIS_cbo[[i]]
  
  pis_vec <- unique(c(pis_vec, (df %>% pull(PIS))))
}

set.seed(05585040)

pis_ID <- tibble("PIS" = pis_vec) %>% 
  distinct(PIS) %>% 
  mutate(ID_fake = sample(length(pis_vec)))


for (i in seq_along(years)) {
  df <- list_PIS_cbo[[i]] %>% 
    left_join(pis_ID, by = "PIS") %>% 
    select(-PIS)
  
  saveRDS(df, 
          paste0(personal_directory, "list_final_df_full_exp_survey_anon/final_df_full_exp_sur_anon_cbo_match_", 
                 years[i], ".RDS"))
}




