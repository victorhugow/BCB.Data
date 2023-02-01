#Projeto Rating dos Bancos

## Tipo de Instituição: 
# 1 - Conglomerados Prudenciais e Instituições Independentes, 
# 2 - Conglomerados Financeiros e Instituições Independentes, 
# 3 - Instituições Individuais, 
# 4 - Instituições com Operações de Câmbio

# #Tipo Relatório: 
# T - para baixar todos
# 1 Resumo
# 2 Ativo
# 3 Passivo
# 4 Demonstração de Resultado
# 5 Informações de Capital
# 6 Segmentação
# 7 Carteira de Crédito Ativa - Por indexador
# 8 Carteira de crédito ativa - por nível de risco da operação
# 9 Carteira de crédito ativa - por região geográfica
# 10 Carteira de crédito ativa - quantidade de clientes e de operações
# 11 Carteira de crédito ativa Pessoa Física - modalidade e prazo de vencimento
# 12 Carteira de crédito ativa Pessoa Jurídica -  por atividade econômica (CNAE)
# 13 Carteira de crédito ativa Pessoa Jurídica - modalidade e prazo de vencimento
# 14 Carteira de crédito ativa Pessoa Jurídica - por porte do tomador
# 15 Movimentação de Câmbio no Trimestre

rm(list=ls())

library(httr)
library(jsonlite)
library(tidyverse)

#Data
fix_date <- function(x){as.Date(paste0(x, '01'), '%Y%m%d')}

#Função para conectar a API e baixar
get_if_data <- function(tipo_relatorio, tipo_instituicao,ano_mes){
  require(httr)
  require(jsonlite)
  api_call <- paste0("https://olinda.bcb.gov.br/olinda/servico/IFDATA/versao/v1/odata/IfDataValores(AnoMes=@AnoMes,TipoInstituicao=@TipoInstituicao,Relatorio=@Relatorio)?@AnoMes=", ano_mes, 
                     "&@TipoInstituicao=", tipo_instituicao ,
                     "&@Relatorio='", tipo_relatorio, 
                     "'&$format=json")
  raw_response <- GET(api_call)
  resp_char <- rawToChar(raw_response$content)
  Encoding(resp_char) <- "UTF-8"
  parsed <- fromJSON(resp_char, flatten = T)$value
  return(parsed)
}

get_instituicoes_financeiras <- function(ano_mes){
api_call <- paste0("https://olinda.bcb.gov.br/olinda/servico/IFDATA/versao/v1/odata/IfDataCadastro(AnoMes=@AnoMes)?@AnoMes=",ano_mes,
                   "&$format=json&$select=CodInst,Data,NomeInstituicao,DataInicioAtividade,Tcb,Td,Tc,SegmentoTb,Atividade,Uf,Municipio,Sr,CodConglomeradoFinanceiro,CodConglomeradoPrudencial,CnpjInstituicaoLider,Situacao")
raw_response <- GET(api_call)
resp_char <- rawToChar(raw_response$content)
Encoding(resp_char) <- "UTF-8"
parsed <- fromJSON(resp_char, flatten = T)$value
return(parsed)
}

# Baixando dados --------
# 
# ## Conglomerados Prudenciais e Instituições Independentes ----------- 
# 
# #Baixando relatórios para varios periodos
#   #Periodo
#   periodo <- seq.Date(from = as.Date('2010-12-01'), to=as.Date('2022-09-01'), by = '3 months') %>% format('%Y%m')
#   
#   #Cadastro Instituição
#   cadastro <- map_df(periodo, function(x) get_instituicoes_financeiras(ano_mes = x))
#   
#   #Relatórios
#   relatorios_cong_prud <- map_df(periodo, function(x) get_if_data(ano_mes = x,tipo_instituicao = 1, tipo_relatorio = 'T'))
#   
#   relatorios_cong_prud_nomes = relatorios_cong_prud$NomeRelatorio %>% unique()
#   
#   relatorios_cong_prud <- data.table::as.data.table(relatorios_cong_prud)
#   
#   relatorios_cong_prud_list = map(relatorios_cong_prud_nomes, 
#                             function(x) relatorios_cong_prud[NomeRelatorio==x])
#   
#   names(relatorios_cong_prud_list) <- relatorios_cong_prud_nomes
#   relatorios_cong_prud_list <- lapply(relatorios_cong_prud_list, function(x) x %>% mutate(Data = fix_date(AnoMes)))
# 
#   saveRDS(relatorios_cong_prud_list,'dados/cong_prud_relatorios.rds')
#   relatorios_cong_prud_list <- readRDS('dados/cong_prud_relatorios.rds')
#   # #Resumo
#   # resumo <- map_df(periodo, function(x) get_if_data(ano_mes = x,tipo_instituicao = 1, tipo_relatorio = 1))
#   # #Ativo
#   # ativo <- map_df(periodo, function(x) get_if_data(ano_mes = x,tipo_instituicao = 1, tipo_relatorio = 2))
#   # #Passivo
#   # passivo <- map_df(periodo, function(x) get_if_data(ano_mes = x,tipo_instituicao = 1, tipo_relatorio = 3))
#   # #DRE
#   # dre <- map_df(periodo, function(x) get_if_data(ano_mes = x,tipo_instituicao = 1, tipo_relatorio = 4))
#   # #Capital
#   # capital <- map_df(periodo, function(x) get_if_data(ano_mes = x,tipo_instituicao = 1, tipo_relatorio = 5))
#   # 
#   # #Apendando nos dados antigos
#   # resumo_old = readRDS('dados/resumo.rds') %>% rbind(.,resumo) %>% distinct()
#   # ativo_old = readRDS('dados/ativo.rds') %>% rbind(.,ativo) %>% distinct()
#   # passivo_old = readRDS('dados/passivo.rds') %>% rbind(.,passivo) %>% distinct()
#   # dre_old = readRDS('dados/dre.rds') %>% rbind(.,dre) %>% distinct()
#   # capital_old = readRDS('dados/capital.rds') %>% rbind(.,capital) %>% distinct()
#   # cadastro_old = readRDS('dados/cadastro_instituicao.rds') %>% rbind(.,cadastro) %>% distinct()
#   # 
#   # saveRDS(resumo_old, 'dados/resumo.rds')
#   # saveRDS(ativo_old, 'dados/ativo.rds')
#   # saveRDS(passivo_old, 'dados/passivo.rds')
#   # saveRDS(dre_old, 'dados/dre.rds')
#   # saveRDS(capital_old, 'dados/capital.rds')
#   # saveRDS(cadastro_old, 'dados/cadastro_instituicao.rds')
  

## Conglomerados Financeiros e Instituições Independentes ----------- 
#Baixando relatórios para varios periodos

#Periodo
periodo <- seq.Date(from = as.Date('2009-12-01'), to=as.Date('2022-09-01'), by = '3 months') %>% format('%Y%m')

# 1 Resumo
relatorios_cong_fin <- map_df(periodo, function(x) get_if_data(ano_mes = x,tipo_instituicao = 2, tipo_relatorio = 'T'))

relatorio_disp = relatorios_cong_fin$NomeRelatorio %>% unique()

relatorios_cong_fin <- data.table::as.data.table(relatorios_cong_fin)

relatorio_disp_list = map(relatorio_disp, 
    function(x) relatorios_cong_fin[NomeRelatorio==x])

names(relatorio_disp_list) <- relatorio_disp
relatorio_disp_list <- lapply(relatorio_disp_list, function(x) x %>% mutate(Data = fix_date(AnoMes)))


# # 2 Resumo
# resumo_cong_fin <- map_df(periodo, function(x) get_if_data(ano_mes = x,tipo_instituicao = 2, tipo_relatorio = 1))
# 
# # 3 Resumo
# resumo_cong_fin <- map_df(periodo, function(x) get_if_data(ano_mes = x,tipo_instituicao = 2, tipo_relatorio = 1))
# 
# # 4 Resumo
# resumo_cong_fin <- map_df(periodo, function(x) get_if_data(ano_mes = x,tipo_instituicao = 2, tipo_relatorio = 1))
# 
# 
# # 7 Carteira de Crédito Ativa - Por indexador
# cart_indexador <- map_df(periodo, function(x) get_if_data(ano_mes = x,tipo_instituicao = 2, tipo_relatorio = 7))
# 
# # 8 Carteira de crédito ativa - por nível de risco da operação
# cart_niv_risco <- map_df(periodo, function(x) get_if_data(ano_mes = x,tipo_instituicao = 2, tipo_relatorio = 8))
# 
# # 9 Carteira de crédito ativa - por região geográfica
# cart_reg_geog <- map_df(periodo, function(x) get_if_data(ano_mes = x,tipo_instituicao = 2, tipo_relatorio = 9))
# 
# # 10 Carteira de crédito ativa - quantidade de clientes e de operações
# cart_qtd_clie_opr <- map_df(periodo, function(x) get_if_data(ano_mes = x,tipo_instituicao = 2, tipo_relatorio = 10))
# 
# # 11 Carteira de crédito ativa Pessoa Física - modalidade e prazo de vencimento
# cart_pf_modalidade_prazo <- map_df(periodo, function(x) get_if_data(ano_mes = x,tipo_instituicao = 2, tipo_relatorio = 11))
# 
# # 12 Carteira de crédito ativa Pessoa Jurídica -  por atividade econômica (CNAE)
# cart_pj_cnae <- map_df(periodo, function(x) get_if_data(ano_mes = x,tipo_instituicao = 2, tipo_relatorio = 12))
# 
# # 13 Carteira de crédito ativa Pessoa Jurídica - modalidade e prazo de vencimento
# cart_pj_modalidade_prazo <- map_df(periodo, function(x) get_if_data(ano_mes = x,tipo_instituicao = 2, tipo_relatorio = 13))
# 
# # 14 Carteira de crédito ativa Pessoa Jurídica - por porte do tomador
# cart_pj_porte_tomador <- map_df(periodo, function(x) get_if_data(ano_mes = x,tipo_instituicao = 2, tipo_relatorio = 14))

# lista_df_inst_fin <- list(resumo_cong_fin,cart_indexador,
#      cart_niv_risco,
#      cart_reg_geog,
#      cart_qtd_clie_opr,
#      cart_pf_modalidade_prazo,
#      cart_pj_cnae,
#      cart_pj_modalidade_prazo,
#      cart_pj_porte_tomador)


# names(lista_df_inst_fin) <- lapply(lista_df_inst_fin, function(x) x$NomeRelatorio %>% unique()) %>% unlist()
# lista_df_inst_fin <- lapply(lista_df_inst_fin, function(x) x %>% mutate(Data = fix_date(AnoMes)))

saveRDS(relatorio_disp_list,'dados/cong_finan_carteira_credito.rds')
