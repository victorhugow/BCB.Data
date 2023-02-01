#SCR.data - Painel de Operações de Créditos
#Os dados são retornados em arquivos zip e então em csvs com os dados mensais. O que a função faz é baixar e juntar todos os meses.
#Os dados são pesados, não recomendo baixar muitos anos juntamente.
get_scr_data <- function(ano_incial, ano_final){
  #SCR
  require(purrr)
  require(tidyverse)
  require(curl)
  require(data.table)

  ano = ano_incial:ano_final
  tp_dir = tempdir()
  links <- paste0('https://www.bcb.gov.br/pda/desig/planilha_',  ano ,'.zip')
  file_name <- paste0(tp_dir,'\\planilha_', ano, '.zip')
  list_files = list(links = links, file_name = file_name)
  
  #Loop para baixar varios anos, unzipar e juntar todos os meses
  map2_df(.x = links, .y = file_name, function(x,y){
  
  curl::curl_download(url = x,destfile = y, mode = 'wb')

  files = utils::unzip(zipfile = y ,
                       exdir = tp_dir, overwrite = T)
  
  #Adicionando um ano a partir do nome do arquivo por conta de missing data na data nas planilhas
  ano_str <- as.character(str_extract(files, pattern = '\\d{4}'))
  files_ano = list.files(tp_dir, pattern = '.csv',full.names = T)
  files_ano = files_ano[str_detect(files_ano, pattern = ano_str)]
  
  #Lendo todos os arquivos daquele ano
  data_scr_ano = map_df(files_ano,function(file_csv){
    read.csv(file_csv, sep = ';', dec = ',', ) %>%
      mutate_at(vars(a_vencer_ate_90_dias:ativo_problematico), ~ as.numeric(.)) %>%
      mutate(data_base_arquivo = file_csv %>% str_extract(.,'planilha_\\d{6}\\.csv') %>% str_extract(., '\\d{6}') %>% paste0(.,'01') %>% as.Date('%Y%m%d'))})
  
  file.remove(c(files_ano,y))
  
  return(data_scr_ano)

})
  
}
