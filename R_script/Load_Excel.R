library(readxl)
library(dplyr)
library(readr)
library(stringr)
library(moonBook)
library(tidyr)
library(lubridate)

url<-'https://0qjcra.dm.files.1drv.com/y4mJj0z3-5FSo1VrxnLho8xz_L_rRYogq9PdjFeOJUQT9od4DQRTCTFO3LAz-YVSvoJL_-_gxh0a5rGie7zqc5trV8aennod2pPTyMwHfFmYheEtF8CCRgpXRWisdW0pg4EACS6M_M1-yPRYXqXcz_egA0Sqv76KQbeJkK7J3hmP6k?access_token=EwAAA61DBAAUzl/nWKUlBg14ZGcybuC4/OHFdfEAASQBzUQ3U21ORkIe/ddwiHZA3pKAWE9BGsSA%2buoxFO5LFWxDsP8rR8ttXesOIxLoouFdhDLncs3GRctjgQzph09194P5UuSsVsG825VAVFE591O%2bp/bQMRWK2V%2b9Hrq58qqOj2uXeUje9HXLxZjYajVzPWeHBsxOJAcZGFDvidCzYeeK/lxanwfXt1dgLABcpDyFNwBwiKvDeLmmcZZSxGFgiMMmRxV3BEwgz1sttuNcHWb2cx14cWpdv0MrlYz78RJKFrZohiY6IsjeR%2baV6XHhT4Ni0ookgiARPjybMXTGf%2bs9fg/3dVAaasGGih7uLVj/nRSNVnW8wXBwE5aw2akDZgAACA6IsOrUi/wD0AHKqWB3TrXI5BsXXKUiYK2YVw7SBvO8csz3uWjobvxaOXQbUlIWhUuocsc9TgpY4%2b%2b6fMYNJzKd9EAN7gl55oyo7u/FIE7s0nCV78PbfsxUiyFJYhJC8rS/62VUtmLLItUcJGh4BO3Og51EWsl/YcC/txWMg3mMhHEujCGwp9vOWvvbiPet8Qb5Lx923tDJkMVakL3db9Y/YTsgI3OBk3SY0T3pCf1Zw19Cz9WHRh4HTIHoVqQU5RdJ%2bUinQ9R3Q9VfAlmxxn4aPwKanbq7eSw1qW33jrxR6gwUGZLoz%2bKAw8DQ5BHSflfQ3eEpywhG6UqLuKWrgNG%2bw69I6lz%2b1591yAwCTs8Z9jpnF%2b6bTy/v82Snz6rlhUEai/HF4cGuTI8hrELe9BNwpIB4As62wHMsiAvQFiW6c3jpzRNgggfzNcI8LtXr%2b4I3Lci3fDownDNw0J/PG/AHm4N0rYVqfT9DxpR%2bCTWhSFm/jKAbxsODCkbx%2bJt1RHBqScrDAWQjqjkaJCvxU3G4qNa%2b0ZSwDINsw1rF9sRoc3IuH8JAcULWCIm8pJ9JfycPk9hd1A%2b5jgFmSmO5qfKb7I5e7G8HtVohykfo7/fHhKgUDiDoUKN3igYC'
fname<-'MPS_YHC.xlsx'
curl::curl_download(url, fname)

read_excel(fname,n_max=2,col_names=F)%>%t()%>%as.data.frame()->tmp
var_name<-paste0(tmp[[1]],'_',gsub(" ","_",tmp[[2]]))



raw_data<-read_excel(fname,skip=2,col_names=F,na=c('',"ND"))

names(raw_data)<-var_name
eff_var<-var_name[!is.na(tmp[[1]])]
raw_data$Info_병록번호<-str_pad(raw_data$Info_병록번호,8,pad='0')


time_data<-read_csv('update_time.txt')

A=ymd(time_data$Op_date)+hms(time_data$Op_time)
B=ymd(time_data$Dx_date)+hms(time_data$Dx_time)
time_data$DO_interval=as.numeric(A-B)/60



anal_data<-raw_data%>%
  select(eff_var)%>%
  replace_na(list(Outcome_Death=0))%>%
  left_join(.,time_data%>%select(병록번호,DO_interval),by=c("Info_병록번호"="병록번호"))


##########



