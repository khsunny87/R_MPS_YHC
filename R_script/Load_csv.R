library(dplyr)
library(readr)
library(stringr)
library(moonBook)


#loa
fname<-'MPS_data.csv'

read_csv(fname,n_max=2,col_names=F)%>%t()%>%as.data.frame()->tmp
var_name<-paste0(tmp[[1]],'_',tmp[[2]])


raw_data<-read_csv(fname,skip=2,col_names=F)
names(raw_data)<-var_name


eff_var<-var_name[!is.na(tmp[[1]])]

anal_data<-raw_data%>%
  select(eff_var)



