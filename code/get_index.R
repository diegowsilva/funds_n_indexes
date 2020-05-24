

##########################3
# IMA

extract_daily_ima = function(ref_date){
  tmp_file = "tmp/temp_ima.csv"
  entra_na_pagina_url <- "https://www.anbima.com.br/informacoes/ima/ima-sh.asp"
  entra_na_pagina_response <- GET(entra_na_pagina_url)
  entra_na_pagina_cookies <- cookies(entra_na_pagina_response)
  
  cookies <- entra_na_pagina_cookies$value %>% setNames(entra_na_pagina_cookies$name)
  
  clica_no_download_url <- "https://www.anbima.com.br/informacoes/ima/ima-sh-down.asp"
  
  dados = list(
    "Tipo" = "",
    "DataRef" = "", 
    "Pai" = "ima",
    "escolha" = "2",
    "Idioma" = "PT",
    "saida" = "csv",
    #"Dt_Ref_Ver" = "20200430",
    "Dt_Ref" = ref_date
  )
  
  clica_no_download_html <- POST(
    clica_no_download_url, 
    encode = "form", 
    set_cookies(.cookies = cookies), 
    body = dados
    , write_disk(tmp_file, overwrite = TRUE)
  )
  
  if(readLines(tmp_file) %>% length > 1){
    df_temp = read.csv(tmp_file, skip = 3, header = F,sep=";",stringsAsFactors = F) %>%
      .[,c(2,3)]
    
    data_row = df_temp[,2] %>% gsub("\\.","",.)  %>% gsub(",","\\.",.) %>% as.numeric()
    
    out_df = data_row %>% t %>% as.data.frame()
    
    names(out_df) = c("irf_m1p","irf_m","ima_b5" ,"ima_b5p", "ima_b","ima_c","ima_s", "ima_geral_ex_c","ima_geral" )
    
    out_df = cbind("date"=as.Date(df_temp[1,1],"%d/%m/%Y"),out_df)
    return(out_df)
  }
  
}

#extract_daily_ima("05/05/2020")

extract_batch_ima = function(ini_date,end_date){
  message("## Extraindo IMA")
  lapply(format(seq(as.Date(ini_date,"%d/%m/%Y"),as.Date(end_date,"%d/%m/%Y"),by="day"),"%d/%m/%Y"), extract_daily_ima) %>% 
    do.call(rbind,.) %>% 
    return
}



## Dolar
# https://br.investing.com/currencies/usd-brl-historical-data


extract_batch_usdbrl = function(ini_date,end_date){
  message("## Extraindo USD/BRL")
  pega_tabela_url <- "https://br.investing.com/instruments/HistoricalDataAjax"
  dados = list(
    'curr_id' = '2103'
    ,'smlID' = '107254'
    ,'header' = 'USD/BRL Dados Históricos'
    ,'st_date' = ini_date
    ,'end_date' = end_date
    ,'interval_sec' = 'Daily'
    ,'sort_col' = 'date'
    ,'sort_ord' = 'DESC'
    , 'action' = 'historical_data'
  )
  
  pega_tabela_response <- POST(
    pega_tabela_url, 
    body = dados, 
    add_headers("User-Agent" = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/50.0.2661.75 Safari/537.36",
                "X-Requested-With" = "XMLHttpRequest"),
    accept("text/plain, */*; q=0.01"),
    encode = "form"
  )
  
  raw_usd = pega_tabela_response %>%
    content() %>%
    html_nodes('table') %>%
    html_table(.,fill=TRUE) %>%
    .[[1]]
  
  df_usd = raw_usd[,1:2]
  
  df_usd[,1] %<>% gsub("\\.","/",.) %>% as.Date(.,"%d/%m/%Y")
  df_usd[,2] %<>% gsub(",","\\.",.) %>% as.numeric()
  
  names(df_usd) = c("date","usdbrl_close")
  
  df_usd %>% return
  
}


## IBOV


extract_batch_ibov = function(ini_date,end_date){
  message("## Extraindo IBOV")
  pega_tabela_url <- "https://br.investing.com/instruments/HistoricalDataAjax"
  dados = list(
    'curr_id' = '17920'
    ,'smlID' = '2036142'
    ,'header' = 'Ibovespa Dados Históricos'
    ,'st_date' = ini_date
    ,'end_date' = end_date
    ,'interval_sec' = 'Daily'
    ,'sort_col' = 'date'
    ,'sort_ord' = 'DESC'
    , 'action' = 'historical_data'
  )
  
  pega_tabela_response <- POST(
    pega_tabela_url, 
    body = dados, 
    add_headers("User-Agent" = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/50.0.2661.75 Safari/537.36",
                "X-Requested-With" = "XMLHttpRequest"),
    accept("text/plain, */*; q=0.01"),
    encode = "form"
  )
  
  raw_data = pega_tabela_response %>%
    content() %>%
    html_nodes('table') %>%
    html_table(.,fill=TRUE) %>%
    .[[1]]
  
  df_out = raw_data[,1:2]
  
  df_out[,1] %<>% gsub("\\.","/",.) %>% as.Date(.,"%d/%m/%Y")
  df_out[,2] %<>% gsub("\\.","",.) %>% gsub(",","\\.",.) %>% as.numeric()
  
  names(df_out) = c("date","ibov_close")
  
  df_out %>% return
  
}

## S&P
extract_batch_sp500 = function(ini_date,end_date){
  
  message("## Extraindo S&P 500")
  pega_tabela_url <- "https://br.investing.com/instruments/HistoricalDataAjax"
  dados = list(
    'curr_id' = '166'
    ,'smlID' = '2030167'
    ,'header' = 'SPY - Dados Históricos'
    ,'st_date' = ini_date
    ,'end_date' = end_date
    ,'interval_sec' = 'Daily'
    ,'sort_col' = 'date'
    ,'sort_ord' = 'DESC'
    , 'action' = 'historical_data'
  )
  
  pega_tabela_response <- POST(
    pega_tabela_url, 
    body = dados, 
    add_headers("User-Agent" = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/50.0.2661.75 Safari/537.36",
                "X-Requested-With" = "XMLHttpRequest"),
    accept("text/plain, */*; q=0.01"),
    encode = "form"
  )
  
  raw_data = pega_tabela_response %>%
    content() %>%
    html_nodes('table') %>%
    html_table(.,fill=TRUE) %>%
    .[[1]]
  
  df_out = raw_data[,1:2]
  
  df_out[,1] %<>% gsub("\\.","/",.) %>% as.Date(.,"%d/%m/%Y")
  df_out[,2] %<>% gsub("\\.","",.) %>% gsub(",","\\.",.) %>% as.numeric()
  
  names(df_out) = c("date","sp500_close")
  
  df_out %>% return
  
}

spawn_index_dataframe = function(ini_date,end_date){
  df_ima = extract_batch_ima(ini_date,end_date)
  df_usdbrl = extract_batch_usdbrl(ini_date,end_date)
  df_ibov = extract_batch_ibov(ini_date,end_date)
  df_sp500 = extract_batch_sp500(ini_date,end_date)
  
  merge(
    merge(
      merge(df_ibov
            ,df_usdbrl
            ,by = "date")
      ,df_ima
      ,by = "date")
    ,df_sp500
    ,by = "date") %>% return
  
}

message("## get_index.R loaded")

