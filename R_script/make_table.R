library(moonBook)
library(ztable)
library(stringr)


mytable(~Cate_Gr,data=anal_data)

anal_data%>%
  select(Cate_Gr,Info_Male,Info_Age)%>%
  mytable(Cate_Gr~.,data=.)

anal_data%>%
  select(starts_with('Cate_'))%>%
  mytable(Cate_Gr~.-`Cate_preopCT유무`-`Cate_내용`-`Cate_sxFU`,data=.)


anal_data%>%
  select(Cate_Gr,starts_with('PMH_'))%>%
  mytable(Cate_Gr~.,data=.)

anal_data%>%
  select(Cate_Gr,starts_with('Lab_'))%>%
  mytable(Cate_Gr~.-Lab_TnT,data=.)

anal_data%>%
  select(Cate_Gr,starts_with('Op_'))%>%
  mytable(Cate_Gr~.-Op_Asc-Op_Hemi-Op_Partial-Op_Total-Op_Desc-Op_TA-Op_Abdao-Op_Iliac,data=.)

anal_data%>%
  select(Cate_Gr,starts_with('Comb_'))%>%
  mytable(Cate_Gr~.-Comb_PVDbypssname,data=.)

anal_data%>%
  select(Cate_Gr,starts_with('CPB_'))%>%
  mutate(CPB_Cannulation=if_else(str_detect(CPB_Cannulation,'^Combined'),'Combined',if_else(CPB_Cannulation%in%c('RAX','LAX'),'Axillary',CPB_Cannulation)))%>%
  mytable(Cate_Gr~.,data=.,show.total=T)%>%compress()%>%
  ztable()

anal_data%>%
  select(Cate_Gr,starts_with('PostOp_'))%>%
  mytable(Cate_Gr~.,data=.)


anal_data%>%
  select(Cate_Gr,starts_with('Cx_'))%>%
  mutate(Cx_pulmo=Cx_Tracheo|Cx_Pneumonia|Cx_Vent_7d|Cx_Reintu,Cx_CVA=Cx_CVA_TIA|Cx_CVA_Stroke|Cx_Brain_Hemo|Cx_CVA_Sequela)%>%
  mytable(Cate_Gr~.,data=.,method=3,catMethod=0,show.total=TRUE)%>%
  compress(add.label='F')%>%
  ztable()



  
anal_data%>%
  mytable(Cate_Gr~DO_interval,data=.,show.total=TRUE)%>%
  ztable()
