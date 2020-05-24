rm(list=ls())

source("code/set_packages.R")
source("code/get_index.R")
source("code/get_fund.R")

ini_date = '18/05/2020'
end_date = '20/05/2020'

cnpj_list = get_funds_at_least(1e3,1e9)

df_funds = extract_funds_by_cnpj_list(ini_date,end_date,cnpj_list)
df_index = spawn_index_dataframe(ini_date,end_date)

df_joined = merge(df_funds
                  ,df_index
                  ,by = "date")

write.csv(df_joined,file=paste0("data/output_",Sys.time() %>% gsub("[^0-9 ]","",.) %>% gsub(" ","_",.),".csv"),row.names = F)