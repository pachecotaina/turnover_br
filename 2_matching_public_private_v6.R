######################
#### CBo matching ####
######################
# 1) Para os anos de 2001, 2002, 2003 e 2004 (2 antes e 2 depois de Lula) classificar todas as observações em público
# ou privado com base em TODAS as Naturezas Jurídicas;
# 2) Para a base de dados classificada, filtrar para CBO privado OU público E federal;
# 3) Gerar uma lista com CBO públicos federais para esses anos;
# 4) Manualmente classificar os CBOs públicos federais que NÃO PODEM ser encontrados no setor público (ex. juiz);
# 5) Excluir da base CBO que são públics e foram classificados como privado. 
# Decidimos excluir porque não sabemos se o erro foi no preenchimento do CBO ou da Natureza Jurídica;
# 6) Com essa base limpa, fazer o match ano a ano. 
# Esse match vai buscar CBOs que em um mesmo ano foram encontrados tanto no setor público quando no setor privado.
# Exemplos: professores, enfermeiros, advogados, economistas...;
# 7) Fazer uma lista dos CBOs classificados como público que não tiveram match;
# 8) Para os CBOs de 7), buscar outras posições das pessoas nos três anos anteriores (menor ano será 1998) no setor privado.


# colunas do banco de dados a nível individual
    # PIS >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> ()
    # CBO >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> ()
    # NatJur >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> ()
    # CNPJ raiz/radical >>>>>>>>>>>>>>>>>>>>> ()
    # salario contratual >>>>>>>>>>>>>>>>>>>> ()
    # remuneração media >>>>>>>>>>>>>>>>>>>>> ()
    # remuneração em dez >>>>>>>>>>>>>>>>>>>> ()
    # última remuneração >>>>>>>>>>>>>>>>>>>> ()
    # horas de trabalho contratual >>>>>>>>>> ()
    # data início do contrato >>>>>>>>>>>>>>> ()
    # tipo de admissão >>>>>>>>>>>>>>>>>>>>>> ()
    # data/mês fim do contrato >>>>>>>>>>>>>> ()
    # status do emrpego em 31/dez >>>>>>>>>>> ()
    # causa do desligamento >>>>>>>>>>>>>>>>> ()
    # tipo de contrato >>>>>>>>>>>>>>>>>>>>>> ()
    # data de nascimento >>>>>>>>>>>>>>>>>>>> ()
    # gênero >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> ()
    # grau de instrução >>>>>>>>>>>>>>>>>>>>> ()
    # tempo de emprego >>>>>>>>>>>>>>>>>>>>>> ()

rm(list=ls())
gc()

library("tidyverse")
library("readxl")
library("data.table")

# Para facilitar
select <- dplyr::select
filter <- dplyr::filter
`%notin%` = Negate(`%in%`)

# personal_directory <- "//fs-eesp-01/EESP/Usuarios/taina.pacheco/"
# directory <- "//fs-eesp-01/eesp-bd-02/RAIS_IDENTIFICADA/"

personal_directory <- "C:/Users/taina/Desktop/RAIS/matching/dec2021/"
directory <- "C:/Users/taina/Desktop/RAIS/"

#years
years_lula <- as.character(c(2001:2004))
years <- as.character(c(1995:2004))

#http://www.mtecbo.gov.br/
nat_jur_fed <- as.character(c(1015, 1040, 1074, 1104, 1139, 1163, 1252, 10))
nat_jur <- as.character(c(10,
                          # Natureza Jurídica 1995
                          # https://concla.ibge.gov.br/estrutura/natjur-estrutura/natureza-juridica-1995.html
                          1015, #Poder Executivo Federal
                          1023, #Poder Executivo Estadual
                          1031, #Poder Executivo Municipal
                          1040, #Poder Legislativo Federal
                          1058, #Poder Legislativo Estadual
                          1066, #Poder Legislativo Municipal
                          1074, #Poder Judiciario Federal
                          1082, #Poder Judiciario Estadual
                          1090, #Orgão Autonomo de Direito Público
                          1104, #Autarquia Federal
                          1112, #Autarquia Estadual
                          1120, #Autarquia Municipal
                          1139, #Fundação Federal
                          1147, #Fundação Estadual
                          1155, #Fundação Municipal
                          1996, #Outras Formas de Organização da Administração Pública
                          2011, #Empresa Publica - Sociedade por Quotas de Responsabilidade Limitada
                          2020, #Empresa Publica - Sociedade Anonima de Capital Fechado
                          2038, #Sociedade Anonima de Capital Aberto com Controle Acionário Estatal (sociedade de economia mista) 
                          
                          # Natureza jurídica 2002
                          # https://concla.ibge.gov.br/estrutura/natjur-estrutura/natureza-juridica-2002.html
                          1015, #Órgão Público do Poder Executivo Federal
                          1023, #Órgão Público do Poder Executivo Estadual ou do Distrito Federal
                          1031, #Órgão Público do Poder Executivo Municipal
                          1040, #Órgão Público do Poder Legislativo Federal
                          1058, #Órgão Público do Poder Legislativo Estadual ou do Distrito Federal
                          1066, #Órgão Público do Poder Legislativo Municipal
                          1074, #Órgão Público do Poder Judiciário Federal
                          1082, #Órgão Público do Poder Judiciário Estadual
                          1104, #Autarquia Federal
                          1112, #Autarquia Estadual ou do Distrito Federal
                          1120, #Autarquia Municipal
                          1139, #Fundação Federal
                          1147, #Fundação Estadual ou do Distrito Federal
                          1155, #Fundação Municipal
                          1163, #Órgão Público Autônomo da União
                          1171, #Órgão Público Autônomo Estadual ou do Distrito Federal
                          1180, #Órgão Público Autônomo Municipal
                          2011, #Empresa Pública
                          2038)) #Sociedade de Economia Mista 

# There were changes in Natureza Jurídica after 2002, but none of them altering the classification of public entities.
# https://concla.ibge.gov.br/classificacoes/por-tema/organizacao-juridica
                        
open_files <- function(years_files = NA_character_, file_to_read = NA_character_){
  if(years_files == "2002"){
    df <- fread(file_to_read, 
                sep=";", 
                header=TRUE, 
                na.strings="NA",
                showProgress = TRUE, 
                colClasses = "character",
                select = c("OCUPACAO", "PIS", "NATUR JUR",
                           "HORAS CONTR", "REM DEZ (R$)", "REM MED (R$)", "ULT REM", "SAL CONTR"
                           # ,"DT ADMISSAO", "RADIC CNPJ", "GRAU INSTR", "MES DESLIG", "CAUSA DESLI", "MUNICIPIO",
                           # "SEXO", "TEMP EMPR", "TP VINCL", "EMP EM 31/12", "DT NASCIMENT"
                           )) %>% 
      rename(natjur = `NATUR JUR`, 
             cbo = `OCUPACAO`,
             # date_adm = `DT ADMISSAO`,
             # CNPJ_root = `RADIC CNPJ`,
             # educ = `GRAU INSTR`,
             # month_quit = `MES DESLIG`,
             # reason_quit = `CAUSA DESLI`,
             # municipality = MUNICIPIO,
             # gender = SEXO,
             # time_employed = `TEMP EMPR`,
             # tp_contract = `TP VINCL`,
             # employed_dec = `EMP EM 31/12`,
             # b_date = `DT NASCIMENT`,
             hours_contract = `HORAS CONTR`,
             wage_dec = `REM DEZ (R$)`,
             wage_mean = `REM MED (R$)`,
             wage_last = `ULT REM`,
             wage_contract = `SAL CONTR`) %>% 
      mutate(cbo = str_replace(cbo, "CBO ", ""),
             # age = as.numeric(years_files) - as.numeric(substr(b_date,(nchar(b_date)+1)-4,nchar(b_date))),
             # date_adm = ifelse(nchar(date_adm) == 7, paste0("0", date_adm), date_adm),
             # date_adm = as.Date(date_adm, format = "%d%m%Y"),
             # date_quit = ifelse(month_quit == "00", NA, 
             #                    as.Date(paste0("30/", month_quit, "/", years_files), format = "%d/%m/%Y")),
             # wage_dec = as.numeric(str_replace(wage_dec, ",", ".")),
             wage_mean = as.numeric(str_replace(wage_mean, ",", ".")),
             wage_last = as.numeric(str_replace(wage_last, ",", ".")),
             wage_contract = as.numeric(str_replace(wage_contract, ",", ".")),
             wage_hour = ifelse(as.numeric(hours_contract) == 0, 0, 
                                as.numeric(str_replace(wage_mean, ",", "."))/as.numeric(hours_contract))) #%>% 
      # select(-b_date, -month_quit)
      # filter(cbo %in% c("20496", "20512", "20528", "20544",
      #                     "21420", "21490", 
      #                     "24592", "24608", "24624", "24640", 
      #                     "16416", "16432", "16448", 
      #                     "28688", "28704", 
      #                     "39385"))

  } else if (years_files %in% as.character(1995:1998)){
    df <- fread(file_to_read, 
                sep=";", 
                header=TRUE, 
                na.strings="NA",
                showProgress = TRUE, 
                colClasses = "character",
                select = c("CBO 94 Ocupação", "PIS", "Natureza Jurídica", 
                           "Vl Remun Dezembro (SM)", "Vl Remun Média (SM)", "Qtd Hora Contr"
                           # ,"Ano Admissão", "CNPJ Raiz", "Grau Instrução 2005-1985", "Mês Admissão",
                           # "Mês Desligamento", "Motivo Desligamento", "Município", "Sexo Trabalhador", "Tempo Emprego",
                           # "Tipo Vínculo", "Vínculo Ativo 31/12", "Idade", "Tipo Admissão"
                )) %>% 
      rename(natjur = `Natureza Jurídica`, 
             cbo = `CBO 94 Ocupação`,
             # year_adm = `Ano Admissão`,
             # CNPJ_root = `CNPJ Raiz`,
             # educ = `Grau Instrução 2005-1985`,
             # month_adm = `Mês Admissão`,
             # month_quit = `Mês Desligamento`,
             # reason_quit = `Motivo Desligamento`,
             # municipality = `Município`,
             # gender = `Sexo Trabalhador`,
             # time_employed = `Tempo Emprego`,
             # tp_contract = `Tipo Vínculo`,
             # employed_dec = `Vínculo Ativo 31/12`,
             # age = `Idade`,
             hours_contract = `Qtd Hora Contr`,
             wage_dec = `Vl Remun Dezembro (SM)`,
             wage_mean = `Vl Remun Média (SM)`) %>% 
      mutate(cbo = str_replace(cbo, "CBO ", ""),
             wage_last = NA_character_,
             wage_contract = NA_character_,
             wage_hour = ifelse(as.numeric(hours_contract) == 0, 0, 
                                as.numeric(str_replace(wage_mean, ",", "."))/as.numeric(hours_contract))) # %>% 
    # select(-month_adm, -year_adm, -month_quit)
    # filter(cbo %in% c("20496", "20512", "20528", "20544",
    #                     "21420", "21490", 
    #                     "24592", "24608", "24624", "24640", 
    #                     "16416", "16432", "16448", 
    #                     "28688", "28704", 
    #                     "39385"))
    
  } else if (years_files %in% as.character(1995)){
    df <- df %>% 
      mutate(# Maio 1995,  Lei nº 9032, de 1995, salário mínimo: R$ 100,00
        wage_dec = as.numeric(str_replace(wage_dec, ",", ".")) * 100,
        wage_mean = as.numeric(str_replace(wage_mean, ",", ".")) * 100,
        wage_hour = ifelse(as.numeric(hours_contract) == 0, 0, 
                           as.numeric(str_replace(wage_mean, ",", "."))/as.numeric(hours_contract))) 
  } else if (years_files %in% as.character(1996)){
    df <- df %>% 
      mutate(# Maio 1996,  Lei nº 9971, de 1996, salário mínimo: R$ 120,00
        wage_dec = as.numeric(str_replace(wage_dec, ",", ".")) * 112,
        wage_mean = as.numeric(str_replace(wage_mean, ",", ".")) * 112,
        wage_hour = ifelse(as.numeric(hours_contract) == 0, 0, 
                           as.numeric(str_replace(wage_mean, ",", "."))/as.numeric(hours_contract))) 
  } else if (years_files %in% as.character(1997)){
    df <- df %>% 
      mutate(# Maio 1997,  Lei nº 9971, de 2000, salário mínimo: R$ 130,00
        wage_dec = as.numeric(str_replace(wage_dec, ",", ".")) * 120,
        wage_mean = as.numeric(str_replace(wage_mean, ",", ".")) * 120,
        wage_hour = ifelse(as.numeric(hours_contract) == 0, 0, 
                           as.numeric(str_replace(wage_mean, ",", "."))/as.numeric(hours_contract))) 
  } else if (years_files %in% as.character(1998)){
    df <- df %>% 
      mutate(# Maio 1998,  Lei nº 9971, de 2000, salário mínimo: R$ 130,00
             wage_dec = as.numeric(str_replace(wage_dec, ",", ".")) * 130,
             wage_mean = as.numeric(str_replace(wage_mean, ",", ".")) * 130,
             wage_hour = ifelse(as.numeric(hours_contract) == 0, 0, 
                                as.numeric(str_replace(wage_mean, ",", "."))/as.numeric(hours_contract))) 
  } else if (years_files %in% as.character(c(1999:2001))){
    df <- fread(file_to_read, 
                sep=";", 
                header=TRUE, 
                na.strings="NA",
                showProgress = TRUE, 
                colClasses = "character",
                select = c("CBO 94 Ocupação", "PIS", "Natureza Jurídica", 
                           "Vl Remun Dezembro Nom", "Vl Remun Média Nom", "Qtd Hora Contr"
                           # ,"Ano Admissão", "CNPJ Raiz", "Grau Instrução 2005-1985", "Mês Admissão",
                           # "Mês Desligamento", "Motivo Desligamento", "Município", "Sexo Trabalhador", "Tempo Emprego",
                           # "Tipo Vínculo", "Vínculo Ativo 31/12", "Idade", "Tipo Admissão"
                           )) %>% 
      rename(natjur = `Natureza Jurídica`, 
             cbo = `CBO 94 Ocupação`,
             # year_adm = `Ano Admissão`,
             # CNPJ_root = `CNPJ Raiz`,
             # educ = `Grau Instrução 2005-1985`,
             # month_adm = `Mês Admissão`,
             # month_quit = `Mês Desligamento`,
             # reason_quit = `Motivo Desligamento`,
             # municipality = `Município`,
             # gender = `Sexo Trabalhador`,
             # time_employed = `Tempo Emprego`,
             # tp_contract = `Tipo Vínculo`,
             # employed_dec = `Vínculo Ativo 31/12`,
             # age = `Idade`,
             hours_contract = `Qtd Hora Contr`,
             wage_dec = `Vl Remun Dezembro Nom`,
             wage_mean = `Vl Remun Média Nom`)%>% 
      mutate(cbo = str_replace(cbo, "CBO ", ""),
             # date_adm = as.Date(paste0("01/", month_adm, "/", year_adm), format = "%d/%m/%Y"),
             # date_quit = ifelse(month_quit == "00", NA, 
             #                    as.Date(paste0("30/", month_quit, "/", years_files), format = "%d/%m/%Y")),
             wage_dec = as.numeric(str_replace(wage_dec, ",", ".")),
             wage_mean = as.numeric(str_replace(wage_mean, ",", ".")),
             wage_last = NA_character_,
             wage_contract = NA_character_,
             wage_hour = ifelse(as.numeric(hours_contract) == 0, 0, 
                                as.numeric(str_replace(wage_mean, ",", "."))/as.numeric(hours_contract))) #%>% 
      # select(-month_adm, -year_adm, -month_quit)
      # filter(cbo %in% c("20496", "20512", "20528", "20544",
      #                     "21420", "21490", 
      #                     "24592", "24608", "24624", "24640", 
      #                     "16416", "16432", "16448", 
      #                     "28688", "28704", 
      #                     "39385"))
    
  } else{
    df <- fread(file_to_read, 
                sep=";", 
                header=TRUE, 
                na.strings="NA",
                showProgress = TRUE, 
                colClasses = "character",
                select = c("OCUPACAO 94", "PIS", "NATUR JUR", 
                           "HORAS CONTR", "REM DEZ (R$)", "REM MED (R$)", "ULT REM", "SAL CONTR"
                           # ,"DT ADMISSAO", "RADIC CNPJ", "GRAU INSTR", "MES DESLIG", "CAUSA DESLI", 
                           # "MUNICIPIO", "SEXO", "TEMP EMPR", "TP VINCULO", "EMP EM 31/12", "DT NASCIMENT"
                           )) %>% 
      rename(natjur = `NATUR JUR`, 
             cbo = `OCUPACAO 94`,
             # date_adm = `DT ADMISSAO`,
             # CNPJ_root = `RADIC CNPJ`,
             # educ = `GRAU INSTR`,
             # month_quit = `MES DESLIG`,
             # reason_quit = `CAUSA DESLI`,
             # municipality = MUNICIPIO,
             # gender = SEXO,
             # time_employed = `TEMP EMPR`,
             # tp_contract = `TP VINCULO`,
             # employed_dec = `EMP EM 31/12`,
             # b_date = `DT NASCIMENT`,
             hours_contract = `HORAS CONTR`,
             wage_dec = `REM DEZ (R$)`,
             wage_mean = `REM MED (R$)`,
             wage_last = `ULT REM`,
             wage_contract = `SAL CONTR`) %>% 
      mutate(cbo = str_replace(cbo, "CBO ", ""),
             # age = as.numeric(years_files) - as.numeric(substr(b_date,(nchar(b_date)+1)-4,nchar(b_date))),
             # date_adm = ifelse(nchar(date_adm) == 7, paste0("0", date_adm), date_adm),
             # date_adm = as.Date(date_adm, format = "%d%m%Y"),
             # date_quit = ifelse(month_quit == "00", NA, 
             #                    as.Date(paste0("30/", month_quit, "/", years_files), format = "%d/%m/%Y")),
             wage_dec = as.numeric(str_replace(wage_dec, ",", ".")),
             wage_mean = as.numeric(str_replace(wage_mean, ",", ".")),
             wage_last = as.numeric(str_replace(wage_last, ",", ".")),
             wage_contract = as.numeric(str_replace(wage_contract, ",", ".")),
             wage_hour = ifelse(as.numeric(hours_contract) == 0, 0, 
                                as.numeric(str_replace(wage_mean, ",", "."))/as.numeric(hours_contract))) #%>% 
      # select(-b_date, -month_quit)
    # filter(cbo %in% c("20496", "20512", "20528", "20544",
    #                       "21420", "21490", 
    #                       "24592", "24608", "24624", "24640", 
    #                       "16416", "16432", "16448", 
    #                       "28688", "28704", 
    #                       "39385"))
  }
  
  return(df)
}

list_cbo <- list()
for(i in seq_along(years)){
  
  print(years[i])
  
  # list files for each year
  files <- grep(list.files(path = paste0(directory, years[i]), 
                           all.files = FALSE,
                           full.names = TRUE), 
                pattern= ('Estb|ESTB|IGNORADO'), 
                inv=TRUE, value=TRUE)
  
  list_data_temp <- list()
  list_cbo_temp <- list()
  for(j in seq_along(files)) { 
    # 1) Para os anos de 2001, 2002, 2003 e 2004 (2 antes e 2 depois de Lula) classificar todas as 
    # observações em público ou privado com base em TODAS as Naturezas Jurídicas;
    list_data_temp[[j]] <- open_files(years_file = years[i], file_to_read = files[j]) %>% 
      mutate(public = 0, public = ifelse(natjur %in% nat_jur, 1, public),
             federal = 0, federal = ifelse(natjur %in% nat_jur_fed, 1, federal)) %>% 
    # 2) Para a base de dados classificada, filtrar para CBO privado OU público E federal;
      filter(public == 0 | federal == 1) %>% 
    # criar marcação de ano
      mutate(year = years[i])
    
    # 3) Gerar uma lista com CBO públicos federais para esses anos;
    list_cbo_temp[[j]] <- list_data_temp[[j]] %>% 
      filter(federal == 1) %>% 
      distinct(cbo) %>% 
      select(cbo)
    
    print(j)
  }
  
  df <- bind_rows(list_data_temp)
  saveRDS(df, paste0(personal_directory, "list_data_wages/data_wages_", years[i], ".RDS"))
  
  list_cbo[[i]] <- bind_rows(list_cbo_temp) %>% distinct(cbo)
  
  remove(df, list_data_temp, list_cbo_temp)
}

# 4) Manualmente classificar os CBOs públicos federais que NÃO PODEM ser encontrados no setor público (ex. juiz);
cbo_federais <- bind_rows(list_cbo) %>% distinct(cbo)

# write.csv(cbo_federais, file = paste0(personal_directory, "cbos_federal.csv"))

remove(list_cbo)
# CBOs que são apenas públicos:
# No CBO 94 não havia presidente, por exemplo. Está dentro de dirigentes do setor público
# 31920 >	Agente de saúde pública
# 21420 >	Funcionário público federal superior (1114-05 no CBO 2002)
# 31490 >	Outros serventuários da justiça e trabalhadores assemelhados
# 31390 >	Outros agentes superiores de polícia
# 21490 >	Outros funcionários públicos superiores
# 21320 >	Juiz federal
# 31440 >	Oficial de justiça
# 21330 >	Juiz estadual
# 31430 >	Escrivão
# 58290 >	Outros policiais e trabalhadores assemelhados
# 31420 >	Tabelião
# 21390 >	Outros membros superiores do Poder Judiciário
# 21350 >	Juiz do trabalho
# 21340 >	Juiz militar
# 21220 >	Membro superior do Poder Executivo
# 21130 >	Deputado federal
# 21360 >	Juiz eleitoral
# 31320 >	Delegado de polícia
# 21120 >	Senador
# 21140 >	Vereador
# 21430 >	Funcionário público estadual superior
# 12920 >	Procurador da fazenda nacional
# 58250 >	Papiloscopista policial
# 31340 >	Perito criminal
# 58430 >	Policial rodoviário
# 39385 >	Despachante aduaneiro
# 58230 >	Detetive de polícia
cbo_only_public <- as.character(c(31920, 21420, 31490, 31390, 21490, 21320, 31440, 21330, 31430, 
                                  58290, 31420, 21390, 21350, 21340, 21220, 21130, 21360, 31320, 
                                  21120, 21140, 21430, 12920, 58250, 31340, 58430, 39385, 58230))

# 5) Excluir da base CBO que são públics e foram classificados como privado. 
# Decidimos excluir porque não sabemos se o erro foi no preenchimento do CBO ou da Natureza Jurídica;
# list_data <- readRDS(paste0(personal_directory, "list_data.RDS"))

files <- list.files(path = paste0(personal_directory, "list_data_wages/"), 
                    all.files = FALSE,
                    full.names = TRUE)

for (i in seq_along(files)) {
  df <- readRDS(files[i])
  
  print(years[i])
  print(nrow(df))
  # os CBOs que não estão na lista podem ser públicos e privados
  df1 <- df %>% filter(cbo %notin% cbo_only_public)
  
  # para os CBOs que estão na lista só vamos manter as observações corretamente classificadas como públicas
  df2 <- df %>% 
    filter(cbo %in% cbo_only_public) %>% 
    filter(federal == 1)
  
  df <- rbind(df1, df2)
  print(nrow(df))
  
  saveRDS(df, paste0(personal_directory, "/list_data_wages_clean/data_wages_clean_", years[i], ".RDS"))
  
  remove(df, df1, df2)
}

# Número de observações que perdemos por ano:
# 1995: 27,562,711 - 27,541,093 = 21,618
# 1996: 26,516,841 - 26,493,344 = 23,497
# 1997: 27,863,657 - 27,837,125 = 26,532
# 1998: 28,194,129 - 28,158,249 = 35,880
# 1999: 28,460,449 - 28,404,008 = 56,441
# 2000: 30,473,813 - 30,428,651 = 45,162
# 2001: 32,019,990 - 31,966,857 = 53,133
# 2002: 36,986,619 - 36,914,780 = 71,839
# 2003: 33,930,145 - 33,878,305 = 51,840
# 2004: 36,376,481 - 36,325,087 = 51,394

# list_data <- readRDS(paste0(personal_directory, "list_data_wages_clean.RDS"))
# 6) Com essa base limpa, fazer o match ano a ano. 
files <- list.files(path = paste0(personal_directory, "list_data_wages_clean/"), 
                    all.files = FALSE,
                    full.names = TRUE)

list_cbo_matched <- list()
list_cbo_not_matched <- list()
for (i in seq_along(files)) {
  print(years[i])
  
  df <- readRDS(files[i])
  
  cbos_duplicates <- df %>% 
    count(cbo, public) %>% 
    add_count(cbo, name = "duplicates") %>% 
    filter(duplicates > 1) %>% 
    ungroup() %>% 
    distinct(cbo) %>% 
    pull(cbo)
  
  df_cbo_matched <- df %>% 
    filter(cbo %in% cbos_duplicates) %>% 
    group_by(cbo, public) %>% 
    mutate(observations = n(),
           wage_mean_mean = mean(as.numeric(wage_mean), na.RM = TRUE),
           wage_mean_median = median(as.numeric(wage_mean), na.RM = TRUE),
           # wage_mean_var = ifelse(n %in% c(1, 2), 0, var(as.numeric(wage_mean), na.RM = TRUE)),
           wage_mean_min = min(as.numeric(wage_mean), na.RM = TRUE),
           wage_mean_max = max(as.numeric(wage_mean), na.RM = TRUE),
           hours_contract = mean(as.numeric(hours_contract), na.rm = TRUE),
           wage_hour_mean = mean(as.numeric(wage_hour), na.rm = TRUE),
           year = years[i]) %>% 
    ungroup() 
  
  saveRDS(df_cbo_matched, 
          paste0(personal_directory, "list_cbo_direct_match_wages/cbo_direct_match_wages_", years[i], ".RDS"))
  
  df_cbo_no_matched <- df %>% 
    filter(cbo %notin% cbos_duplicates) %>% 
    filter(public == 1) %>% 
    mutate(year = years[i]) %>% 
    ungroup()
  
  saveRDS(df_cbo_no_matched, 
          paste0(personal_directory, "list_cbo_no_direct_match_wages/cbo_no_direct_match_wages_", years[i], ".RDS"))
  
  remove(df, df_cbo_matched, df_cbo_no_matched)
    
}

files <- list.files(path = paste0(personal_directory, "list_cbo_direct_match_wages/"), 
                    all.files = FALSE,
                    full.names = TRUE)

for(i in seq_along(files)){
  df <- readRDS(files[i])
  
  df <- df %>% 
    filter(federal == 1)
  
  saveRDS(df, 
          paste0(personal_directory, "list_cbo_direct_match_wages_federal/cbo_direct_match_wages_federal_", 
                 years[i], ".RDS"))
}

# 7) Fazer uma lista dos CBOs classificados como público que não tiveram match;
files <- list.files(path = paste0(personal_directory, "list_cbo_no_direct_match_wages/"), 
                    all.files = FALSE,
                    full.names = TRUE)

vec_cbo_no_direct_match <- c()
for(i in seq_along(files)){
  df <- readRDS(files[i])
  
  df <- df %>% 
    filter(federal == 1)
  
  vec_cbo_no_direct_match <- c(vec_cbo_no_direct_match, 
                               (df %>% distinct(cbo) %>% pull(cbo)))
  
  saveRDS(df, 
          paste0(personal_directory, "list_cbo_no_direct_match_wages_federal/cbo_no_direct_match_wages_federal_", 
                 years[i], ".RDS"))
}

vec_cbo_no_direct_match <- unique(vec_cbo_no_direct_match)

# 8) Para os CBOs de 7), buscar outras posições das pessoas nos três anos anteriores (menor ano será 1995) no setor privado.
files <- list.files(path = paste0(personal_directory, "list_cbo_no_direct_match_wages_federal/"), 
                    all.files = FALSE,
                    full.names = TRUE)

files_bsearch <- list.files(path = paste0(personal_directory, "list_data_wages_clean/"), 
                    all.files = FALSE,
                    full.names = TRUE)

rename_fun <- function(x){
  x <- paste0(x, "2")
  
  return(x)
}

sub_comma_fun <- function(str){
  str <- gsub(",", ".", str)
  
  return(str)
}

inflation_index <- read_csv(paste0(personal_directory, "input/INPC_correction.csv"), 
                            col_types = cols(INPC = col_number(), 
                                             year_destination = col_character(), 
                                             year_origin = col_character()))
for(i in length(files):4){
  df <- readRDS(files[i])
  
  CBO_vector <- df %>% 
    distinct(cbo) %>% 
    pull(cbo)
  
  df <- df %>% 
    mutate_at(c("hours_contract", "wage_dec", "wage_mean", "wage_last", "wage_contract", 
                "public", "federal", "wage_hour"), as.numeric) %>% 
    mutate_at(c("cbo", "PIS", "natjur", "year"), as.character) %>% 
    filter(cbo %in% CBO_vector) %>% 
  # excluir PIS "errados"
    filter(PIS %notin% c("00000000000", "0", "99999999999"))
  
  PIS_vector <- df %>% 
    filter(PIS != "0") %>% 
    pull(PIS)

  list_aux <- list()
  PIS_matched <- c()
  j <- 1
  for (k in c(i - 1, i - 2, i - 3)) { #starts at year - 1
    if(k <= 0){
      print(k)
    } 
    else {
      df_bsearch <- readRDS(files_bsearch[k]) %>% 
        # excluir PIS "errados"
        filter(PIS %notin% c("00000000000", "0", "99999999999"))
      
      inflation <- inflation_index %>% 
        filter(year_destination == years[i],
               year_origin == years[k]) %>% 
        pull(INPC)
      
      list_aux[[j]] <- df_bsearch %>% 
        filter(public == 0) %>% 
        filter(PIS %in% PIS_vector) %>% 
        mutate_at(c("wage_dec", "wage_mean", "wage_last", "wage_contract"), sub_comma_fun) %>% 
        mutate_at(c("hours_contract", "wage_dec", "wage_mean", "wage_last", "wage_contract", 
                    "public", "federal", "wage_hour"), as.numeric) %>% 
        mutate_at(c("cbo", "PIS", "natjur", "year"), as.character) %>% 
        mutate(wage_dec = wage_dec*inflation, 
               wage_mean = wage_mean*inflation, 
               wage_last = wage_last*inflation, 
               wage_contract = wage_contract*inflation,
               wage_hour = wage_hour*inflation)
        
      PIS_matched <- c(PIS_matched, 
                       (list_aux[[j]] %>% pull(PIS)))
      
      PIS_vector <- PIS_vector[PIS_vector %notin% PIS_matched]
      
      j <- j + 1
      
      remove(df_bsearch)
    }
  }
  df2 <- bind_rows(list_aux) %>% 
    distinct(PIS, cbo, hours_contract, wage_contract, wage_hour, year, .keep_all = TRUE)
  
  df <- df %>% 
    select(PIS, cbo) %>% 
    rename(cbo_original = cbo) %>%
    left_join(df2, by = "PIS")
  
  df_matched <- df %>% 
    filter(!is.na(cbo)) %>% 
    add_count(cbo_original, cbo)
  
  saveRDS(df_matched, 
          paste0(personal_directory, 
                 "list_cbo_match_wages_public_past_years/cbo_match_wages_public_past_years_", 
                 years[i], ".RDS"))
  
  df_n_matched <- df %>% 
    filter(is.na(cbo))
  
  saveRDS(df_n_matched, 
          paste0(personal_directory, 
                 "list_cbo_n_match_wages_public_past_years/list_cbo_n_match_wages_public_past_years_", 
                 years[i], ".RDS"))
  
  print(i)
  
  remove(df, df_matched, df_n_matched)
}

# 8a) padronizar o banco de dados para juntar com os outros depois
files_matched <- list.files(path = paste0(personal_directory, "list_cbo_match_wages_public_past_years/"), 
                            all.files = FALSE,
                            full.names = TRUE)

files_clean <- list.files(path = paste0(personal_directory, "list_cbo_no_direct_match_wages_federal/"), 
                          all.files = FALSE,
                          full.names = TRUE)

files_clean <- files_clean[4:length(files_clean)]

for (i in seq_along(files_matched)) {
  df_matched <- readRDS(files_matched[i]) %>% 
    select(PIS, cbo_original, wage_mean, hours_contract, wage_hour) %>% 
    rename(wage_mean_mean = wage_mean,
           wage_hour_mean = wage_hour,
           hours_contract_mean = hours_contract,
           cbo = cbo_original) %>% 
    mutate(wage_mean_median = NA_integer_,
           wage_mean_min = NA_integer_,
           wage_mean_max = NA_integer_) 
  
  PIS_matched <- df_matched %>% 
    distinct(PIS) %>% pull(PIS)
  
  df_clean <- readRDS(files_clean[i]) %>% 
    filter(PIS %in% PIS_matched) %>% 
    left_join(df_matched, by = c("PIS", "cbo")) %>% 
    mutate(observations = NA_integer_)
  
  saveRDS(df_clean, 
          paste0(personal_directory, 
                 "list_cbo_match_wages_public_past_years_df/cbo_match_wages_public_past_years_df", 
                 years[i+3], ".RDS"))
}


# 9) fazer uma lista dos CBOs que não foram classificados
files <- list.files(path = paste0(personal_directory, "list_cbo_n_match_wages_public_past_years/"), 
                    all.files = FALSE,
                    full.names = TRUE)

files_matched <- list.files(path = paste0(personal_directory, "list_cbo_match_wages_public_past_years/"), 
                            all.files = FALSE,
                            full.names = TRUE)

list_cbo_not_matched <- list()
cbo_matched <- c()
for(i in seq_along(files)){
  cbo_aux <- readRDS(files[i]) %>% distinct(cbo_original) %>% pull(cbo_original)
  
  cbo_matched <- c(cbo_matched, cbo_aux)
  
  list_cbo_not_matched[[i]] <- readRDS(files[i]) 
}

cbo_matched <- unique(cbo_matched)

cbo_no_match <- vec_cbo_no_direct_match[vec_cbo_no_direct_match %notin% cbo_matched]
# only the CBO 19245 (Arqueólogo) was not matched

cbo_n_matched_all <- bind_rows(list_cbo_not_matched)

# 10) para os PIS com match, tirar a média salarial por CBO e inputar essa média nos PIS sem match
files <- list.files(path = paste0(personal_directory, "list_cbo_n_match_wages_public_past_years/"), 
                    all.files = FALSE,
                    full.names = TRUE)

files_matched <- list.files(path = paste0(personal_directory, "list_cbo_match_wages_public_past_years/"), 
                            all.files = FALSE,
                            full.names = TRUE)

files_clean <- list.files(path = paste0(personal_directory, "list_cbo_no_direct_match_wages_federal/"), 
                          all.files = FALSE,
                          full.names = TRUE)

files_clean <- files_clean[4:length(files_clean)]

for (i in seq_along(files_matched)) {
  df_matched <- readRDS(files_matched[i]) %>% 
    group_by(cbo_original) %>% 
    summarise(wage_mean_mean = mean(as.numeric(wage_mean), na.rm = TRUE),
              wage_mean_median = median(as.numeric(wage_mean), na.rm = TRUE),
              wage_mean_min = min(as.numeric(wage_mean), na.rm = TRUE),
              wage_mean_max = max(as.numeric(wage_mean), na.rm = TRUE),
              wage_hour_mean = mean(as.numeric(wage_hour), na.rm = TRUE)) %>% 
    rename(cbo = cbo_original)
  
  PIS_n_matched <- readRDS(files[i]) %>% 
    distinct(PIS) %>% pull(PIS)
  
  df_clean <- readRDS(files_clean[i]) %>% 
    filter(PIS %in% PIS_n_matched) %>% 
    left_join(df_matched, by = "cbo") %>% 
    mutate(observations = NA_integer_)
  
  saveRDS(df_clean, 
          paste0(personal_directory, 
                 "list_cbo_n_match_wages_public_past_years_mean_cbo/cbo_n_match_wages_public_past_years_mean_cbo", 
                 years[i+3], ".RDS"))
}

# 11) juntando todas as bases finais
# direct match
files_direct_match <- list.files(path = paste0(personal_directory, "list_cbo_direct_match_wages_federal/"), 
                                 all.files = FALSE,
                                 full.names = TRUE)[4:10]
# direct past position
files_past_positions <- list.files(path = paste0(personal_directory, "list_cbo_match_wages_public_past_years_df/"), 
                            all.files = FALSE,
                            full.names = TRUE)

# mean wage for last positions with the same "original" CBO
files_mean_cbo_past_positions <- list.files(path = paste0(personal_directory, 
                                                          "list_cbo_n_match_wages_public_past_years_mean_cbo/"), 
                                            all.files = FALSE,
                                            full.names = TRUE)

# unindo as bases
for (i in seq_along(files_direct_match)) {
  df_direct_match <- readRDS(files_direct_match[i]) %>% 
    mutate(type = "Direct match by CBO")
  
  df_past_positions <- readRDS(files_past_positions[i]) %>% 
    select(-hours_contract_mean) %>% 
    mutate(type = "PIS past position in the private sector")
  
  df_mean_cbo_past_positions <- readRDS(files_mean_cbo_past_positions[i]) %>% 
    mutate(type = "Same CBO PIS past position in the private sector")
  
  df <- rbind(df_direct_match, df_past_positions, df_mean_cbo_past_positions)
  
  saveRDS(df, 
          paste0(personal_directory, 
                 "list_final_df/final_df_", 
                 years[i+3], ".RDS"))
  
  print(i)
}


files_final <- list.files(path = paste0(personal_directory, "list_final_df/"), 
                          all.files = FALSE,
                          full.names = TRUE)

df_final <- readRDS(files_final[4])

summary(df_final$wage_mean)
summary(df_final$wage_mean_mean)

summary(df_final$wage_hour)
summary(df_final$wage_hour_mean)

df_final %>% 
  select(PIS, wage_mean, wage_mean_mean) %>% 
  filter(wage_mean > 0 & wage_mean_mean > 0 & wage_mean < 10000 & wage_mean_mean < 10000) %>% 
  pivot_longer(cols = c("wage_mean", "wage_mean_mean"), names_to = "wage_type", values_to = "value") %>% 
  ggplot(aes(x = wage_type, y = value))+
  geom_violin() +
  # geom_boxplot(width = 0.1) +
  scale_color_grey() +
  theme_classic()

ggsave(filename = paste0(directory, "matching/dec2021/figures/vplot_wage_mean_vs_wage_mean_mean.png"),
       units = "cm", 
       width = 17,
       height = 10)

df_final %>% 
  select(PIS, wage_mean, wage_mean_mean, type) %>% 
  filter(wage_mean > 0 & wage_mean_mean > 0 & wage_mean < 10000 & wage_mean_mean < 10000) %>% 
  pivot_longer(cols = c("wage_mean", "wage_mean_mean"), names_to = "wage_type", values_to = "value") %>% 
  ggplot(aes(x = wage_type, y = value, color = type))+
  geom_violin() +
  # geom_boxplot(width = 0.1) +
  #scale_color_grey() +
  theme_classic()+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.text = element_text(size = 6))

ggsave(filename = paste0(directory, "matching/dec2021/figures/vplot_wage_mean_vs_wage_mean_mean_by_type.png"),
       units = "cm", 
       width = 17,
       height = 10)

df_final %>% 
  select(PIS, wage_hour, wage_hour_mean) %>% 
  filter(wage_hour > 0 & wage_hour_mean > 0 & wage_hour < 500 & wage_hour_mean < 500) %>% 
  pivot_longer(cols = c("wage_hour", "wage_hour_mean"), names_to = "wage_type", values_to = "value") %>% 
  ggplot(aes(x = wage_type, y = value))+
  geom_violin() +
  # geom_boxplot(width = 0.1) +
  scale_color_grey() +
  theme_classic()

ggsave(filename = paste0(directory, "matching/dec2021/figures/vplot_wage_hour_vs_wage_hour_mean.png"),
       units = "cm", 
       width = 17,
       height = 10)

df_final %>% 
  select(PIS, wage_hour, wage_hour_mean, type) %>% 
  filter(wage_hour > 0 & wage_hour_mean > 0 & wage_hour < 500 & wage_hour_mean < 500) %>% 
  pivot_longer(cols = c("wage_hour", "wage_hour_mean"), names_to = "wage_type", values_to = "value") %>% 
  ggplot(aes(x = wage_type, y = value, color = type))+
  geom_violin() +
  # geom_boxplot(width = 0.1) +
  #scale_color_grey() +
  theme_classic()+
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.text = element_text(size = 6))

ggsave(filename = paste0(directory, "matching/dec2021/figures/vplot_wage_hour_vs_wage_hour_mean_by_type.png"),
       units = "cm", 
       width = 17,
       height = 10)

df_final_export <- df_final %>% 
  sample_n(250000) %>% 
  select(-PIS, -wage_dec, -wage_contract, -wage_last)

write.csv(df_final_export, file = paste0(personal_directory, "list_final_df/df_final_2001_sample.csv"))
