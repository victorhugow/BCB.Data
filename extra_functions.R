#Funções
library(tidyverse)

dup_check = function(x) c(duplicated(x, fromLast = TRUE)  | duplicated(x))

check_zero = function(x) ifelse(x == 0 & !is.na(x), TRUE, FALSE)

fix_date <- function(x){as.Date(paste0(x, '01'), '%Y%m%d')}

`%nin%` = Negate(`%in%`)

cadastro_agrupa_prudencial <- readRDS('dados/ifdata/raw/cadastro_instituicao.rds')  %>% mutate(Data = Data %>% fix_date()) %>%
  mutate(CodConglomeradoPrudencial = ifelse(is.na(CodConglomeradoPrudencial), CodInst, CodConglomeradoPrudencial)) %>%
  ungroup() %>%
  select(CodInst, Data, NomeInstituicao, Tcb, SegmentoTb, Atividade, 
         CodConglomeradoFinanceiro, CodConglomeradoPrudencial, CnpjInstituicaoLider, Situacao)

#Agrupa tudo conglomerados prudenciais a partir dos dados de cadastro
agrupa_prudencial = function(x){

  x %>%
    merge(.,cadastro_agrupa_prudencial[,c('Data','CodInst', 'CodConglomeradoPrudencial')], by = c('Data','CodInst'),all.x=T) %>%
    unique() %>%
    select(-CodInst) %>%
    group_by(Data,CodConglomeradoPrudencial) %>%
    summarise_all(sum, na.rm=T) %>% ungroup() %>%
    rename(CodInst=CodConglomeradoPrudencial)
}


agrupa_prudencial_2 = function(x){
  # cadastro <- readRDS('dados/ifdata/raw/cadastro_instituicao.rds')  %>% mutate(Data = Data %>% fix_date()) %>%
  #   mutate(CodConglomeradoPrudencial = ifelse(is.na(CodConglomeradoPrudencial), CodInst, CodConglomeradoPrudencial)) %>%
  #   ungroup() %>%
  #   select(CodInst, Data, NomeInstituicao, Tcb, SegmentoTb, Atividade, 
  #          CodConglomeradoFinanceiro, CodConglomeradoPrudencial, CnpjInstituicaoLider, Situacao)
  x %>%
    merge(.,cadastro_agrupa_prudencial[,c('Data','CodInst', 'CodConglomeradoPrudencial')], by = c('Data','CodInst'),all.x=T) %>%
    unique() %>%
    select(-CodInst) %>%
    group_by(Data,CodConglomeradoPrudencial, Grupo, Tipo) %>%
    summarise_all(sum, na.rm=T) %>% ungroup() %>%
    rename(CodInst=CodConglomeradoPrudencial)
}

#Rename de modalidades de crédito PF
rename_modalidade_pf = function(x){
  x %>%
    rename('MOD_PF_CARTAO_CREDITO' = `Cartão de Crédito`, 'MOD_PF_EMPREST_CONSIG_FOLHA' = `Empréstimo com Consignação em Folha`, 
           'MOD_PF_EMPREST_SEM_CONSIG_FOLHA' = `Empréstimo sem Consignação em Folha`, 'MOD_PF_HABITACAO' = `Habitação`, 
           'MOD_PF_OUTROS' = `Outros Créditos`,  'MOD_PF_RURAL_AGROINDUST' = `Rural e Agroindustrial`, 
           'MOD_PF_VEICULOS' = `Veículos`, 'MOD_PF_EXTERIOR' = `Total Exterior Pessoa Física`, 'TOTAL_PF' = `Total da Carteira de Pessoa Física`)}

#Rename de modalidades de crédito PJ
rename_modalidade_pj <- function(x){
  x %>%
    rename('MOD_PJ_CAPITAL_GIRO' = `Capital de Giro`, 'MOD_PJ_CAPITAL_GIRO_ROTATIVO' =  `Capital de Giro Rotativo`,
           'MOD_PJ_CHEQUE_ESPECIAL_CONTA_GAR' = `Cheque Especial e Conta Garantida`, 'MOD_PJ_COMEX' = `Comércio Exterior`, 
           'MOD_PJ_FIN_INFRAEST' = `Financiamento de Infraestrutura/Desenvolvimento/Projeto e Outros Créditos`, 'MOD_PJ_HABITACAO' = `Habitacional`, 
           'MOD_PJ_INVESTIMENTO' = `Investimento`, 'MOD_PJ_RECEBIVEIS' = `Operações com Recebíveis`, 
           'MOD_PJ_OUTROS' = `Outros Créditos`, 'MOD_PJ_RURAL_AGROINDUST' = `Rural e Agroindustrial`, 
           'MOD_PJ_EXTERIOR' = `Total Exterior Pessoa Jurídica`, 'TOTAL_PJ' = `Total da Carteira de Pessoa Jurídica`)}

#Rename de Atividade
rename_atividade <- function(x){
  x %>%
    rename('ATIV_ADMPUB_DEFESA_SEGURIDADE_SOCIAL' = `Administração Pública, Defesa e Seguridade Social`, 'ATIV_PECUARIA_FLORESTAL_PESCA_AQUICULTURA' = `Agricultura, Pecuária, Produção Florestal, Pesca e Aquicultura`, 
           'ATIV_COMERCIO_REPAR_VEIC' = `Comércio, Reparação de Veículos Automotores e Motocicletas`, 'ATIV_CONSTRUCAO' = `Construção`,
           'ATIV_IND_TRANSFORM' = `Indústrias de Transformação`, 'ATIV_IND_EXTRATIVAS' = `Industrias Extrativas`, 
           'ATIV_OUTROS' = `Outros`,'ATIV_SERV_IND_UTIL_PUB' = `Serviços Industriais de Utilidade Pública`,
           'ATIV_TRANSP_ARMAZEN_CORREIO' = `Transporte, Armazenagem e Correio`, 'ATIV_N_INFO' = `Atividade não Informada ou não se Aplica` ,
           'ATIV_TOTAL_N_INDIV' = `Total não Individualizado Pessoa Jurídica`,'ATIV_TOTAL_EXTERIOR' = `Total Exterior Pessoa Jurídica`, 'TOTAL_PJ' = `Total da Carteira de Pessoa Jurídica`)}

#Rename de fontes de Receita
rename_receitas <- function(x){
  x %>% rename('RENDA_OPR_CREDITO' = `78203`,
               'RENDA_OPR_LEASING' = `78204`,
               'RENDA_OPR_TVM' = `78205`, 
               'RENDA_OPR_DERIVATIVOS' = `78206`,
               'RENDA_OPR_CAMBIO' = `78207`,
               'RENDA_COMPULS' = `78231`,
               'RENDA_SERVICOS' = `78216`,
               'RENDA_TARIFAS' = `78217`,
               'RENDA_PARTICIP' = `78221`,
               'RENDA_OUTROS' = `78222`)}

#Ajuste da DRE para refletir os resultados do trimestre
ajuste_dre = function(x){
  # 1. De acordo com a Lei nº 4.595, de 31 de dezembro de 1964, as instituições financeiras devem apurar resultados em 30 de junho e em 31 de dezembro de cada ano, obrigatoriamente, 
  # com observância às regras contábeis estabelecidas pelo CMN. Dessa forma, em março e setembro, os dados contábeis referentes às receitas e às despesas correspondem aos saldos 
  # acumulados entre janeiro e março e entre julho e setembro, respectivamente. Os demonstrativos de resultado de junho e dezembro registram os 
  # valores acumulados entre janeiro e junho e entre julho e dezembro, respectivamente.
  x %>% 
    arrange(CodInst, Conta, Data) %>%
    mutate(Ano = lubridate::year(Data), Trimestre = lubridate::quarter(Data), Semestre = lubridate::semester(Data)) %>%
    group_by(Ano,Semestre,TipoInstituicao, CodInst, NomeRelatorio, NumeroRelatorio, Grupo, Conta) %>%
    mutate(Saldo = ifelse(lubridate::month(Data) %in% c(6,12), Saldo[2]-Saldo[1], Saldo)) %>%
    ungroup() %>% select(-Trimestre, -Semestre, -Ano)
}

#Ajustar Fintechs com 2 instituições no relatório de conglomerados prudenciais — que pertencem a um mesmo conglomerado prudencial.
ajusta_indicadores_prudencial = function(x){
  # #Ajustar Fintechs com 2 instituições no relatório de conglomerados prudenciais — que pertencem a um mesmo conglomerado prudencial. No caso, se o valor de uma for NA ou nulo, substituir pelo valor da outra. 
  # E no final, ficar somente com a instiuição que tem Prudencial no Nome ou cujo CodInst comece com C. Vale sobretudo para o relatório de Capital
  
  # cadastro <- readRDS('dados/ifdata/raw/cadastro_instituicao.rds')  %>% mutate(Data = Data %>% fix_date()) %>%
  #   mutate(CodConglomeradoPrudencial = ifelse(is.na(CodConglomeradoPrudencial), CodInst, CodConglomeradoPrudencial))

  df_ajustado = x %>%
    merge(., cadastro_agrupa_prudencial[,c('Data',"CodInst", 'CodConglomeradoPrudencial', "NomeInstituicao")], all.x=T, by = c('CodInst', 'Data')) %>%
    group_by(Data) %>% mutate(dupPrudencial = dup_check(CodConglomeradoPrudencial)) %>% ungroup() %>% #Quem está duplicado?
    arrange(CodConglomeradoPrudencial,CodInst,Data) %>%
    group_by(CodConglomeradoPrudencial, Data) %>%
    mutate_at(vars(-1,-2),~ ifelse(is.na(.),.[!is.na(.)][1L],.)) %>%
    ungroup() %>%
    mutate(drop = ifelse(dupPrudencial & !str_detect(CodInst, '^C'), 1, 0)) %>%
    filter(drop == 0) %>%
    select(-drop, -dupPrudencial, -NomeInstituicao, -CodConglomeradoPrudencial) 
  
  return(df_ajustado)
}
