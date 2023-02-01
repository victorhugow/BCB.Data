#API - IF.Data 

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

#Data
fix_date <- function(x){as.Date(paste0(x, '01'), '%Y%m%d')}

#Cadastro dos Conglomerados e Instiuições 
get_instituicoes_financeiras <- function(ano_mes){
  require(httr)
  require(jsonlite)
  api_call <- paste0("https://olinda.bcb.gov.br/olinda/servico/IFDATA/versao/v1/odata/IfDataCadastro(AnoMes=@AnoMes)?@AnoMes=",ano_mes,
                     "&$format=json&$select=CodInst,Data,NomeInstituicao,DataInicioAtividade,Tcb,Td,Tc,SegmentoTb,Atividade,Uf,Municipio,Sr,CodConglomeradoFinanceiro,CodConglomeradoPrudencial,CnpjInstituicaoLider,Situacao")
  raw_response <- GET(api_call)
  resp_char <- rawToChar(raw_response$content)
  Encoding(resp_char) <- "UTF-8"
  parsed <- fromJSON(resp_char, flatten = T)$value
  return(parsed)
}

#Função para conectar a API e baixar
get_if_data <- function(tipo_relatorio = 'T', tipo_instituicao,ano_mes){
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
get_relatorios <- function(ano_inicial, ano_final, tipo_instituicao, tipo_relatorio = 'T'){
    
    require(tidyverse) 
    fix_date <- function(x){as.Date(paste0(x, '01'), '%Y%m%d')}
  
    #Criando o periodo de download
    data_inicial = paste0(ano_inicial-1, '-12-01')
    data_final = paste0(ano_final, '-12-01')
    periodo <- seq.Date(from = as.Date(data_inicial), to=as.Date(data_final), by = '3 months')[-1] %>% format('%Y%m')
    
    #Baixando os relatorios com a função get_if_data
    relatorios_cong_fin <- map_df(periodo, function(x) get_if_data(ano_mes = x,
                                                                   tipo_instituicao = tipo_instituicao, tipo_relatorio = tipo_relatorio))
    
    #Organizando
    relatorio_disp = relatorios_cong_fin$NomeRelatorio %>% unique()
    relatorios_cong_fin <- data.table::as.data.table(relatorios_cong_fin)
    relatorio_disp_list = map(relatorio_disp, 
                              function(x) relatorios_cong_fin[NomeRelatorio==x])
    names(relatorio_disp_list) <- relatorio_disp
    relatorio_disp_list <- lapply(relatorio_disp_list, function(x) x %>% mutate(Data = fix_date(AnoMes)))
    
    return(relatorio_disp_list)
}

