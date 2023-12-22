#描述性统计分析
library(dplyr)
Description_set<-bodysize_set3 %>% mutate(icd_MI=factor(icd_MI,levels=c(0,1),labels=c("no","yes")),risk_MI=factor(risk_MI,levels=c(0,1,2),labels=c("no","yes","death")),icd_STEMI=factor(icd_STEMI,levels=c(0,1),labels=c("no","yes")),risk_STEMI=factor(risk_STEMI,levels=c(0,1,2),labels=c("no","yes","death")),icd_NSTEMI=factor(icd_NSTEMI,levels=c(0,1),labels=c("no","yes")),risk_NSTEMI=factor(risk_NSTEMI,levels=c(0,1,2),labels=c("no","yes","death")),icd_stroke=factor(icd_stroke,levels=c(0,1),labels=c("no","yes")),risk_stroke=factor(risk_stroke,levels=c(0,1,2),labels=c("no","yes","death")),icd_IS=factor(icd_IS,levels=c(0,1),labels=c("no","yes")),risk_IS=factor(risk_IS,levels=c(0,1,2),labels=c("no","yes","death")),icd_IH=factor(icd_IH,levels=c(0,1),labels=c("no","yes")),risk_IH=factor(risk_IH,levels=c(0,1,2),labels=c("no","yes","death")),icd_SH=factor(icd_SH,levels=c(0,1),labels=c("no","yes")),risk_SH=factor(risk_SH,levels=c(0,1,2),labels=c("no","yes","death")),icd_AF=factor(icd_AF,levels=c(0,1),labels=c("no","yes")),risk_AF=factor(risk_AF,levels=c(0,1,2),labels=c("no","yes","death")),icd_HF=factor(icd_HF,levels=c(0,1),labels=c("no","yes")),risk_HF=factor(risk_HF,levels=c(0,1,2),labels=c("no","yes","death")),icd_cvd=factor(icd_cvd,levels=c(0,1),labels=c("no","yes")),risk_cvd=factor(risk_cvd,levels=c(0,1,2),labels=c("no","yes","death"))) #bodysize_set3变量转因子

library(epiDisplay)
attr(Description_set,"var.labels")<-c("baseline age","","baseline year",rep("",131)) #设置标签值
attr(Description_set,"var.labels")[44]<-"body size change from birth to 10y"
attr(Description_set,"var.labels")[45]<-"body size change from birth to adult"
attr(Description_set,"var.labels")[126]<-"body size change from 10y to adult"
attr(Description_set,"var.labels")[127]<-"Townsend Deprivation Index"

table1<-tableStack(vars = c(1,3,39:45,114,116,118:134),by="none",iqr=NULL,dataFrame = Description_set,test = FALSE) #不分类汇总
table1<-tableStack(vars = c(1,3,39:45,114,116,118:134),by="none",iqr=c(1,3,41,43,114,116,127),dataFrame = Description_set,test = FALSE) #不分类汇总(四分位数间距)
write.csv(table1,file = "table1.csv")

table2<-tableStack(vars = c(1,3,39:45,114,116,118:134),by=icd_cvd,iqr=NULL,dataFrame = Description_set,percent = "row",test = TRUE) #按cvd发病汇总,均数标准差
table2<-tableStack(vars = c(1,3,39:45,114,116,118:134),by=icd_cvd,dataFrame = Description_set,percent = "row",test = TRUE) #按cvd发病汇总
write.csv(table2,file = "table2.csv") #复制到tables.xlsx里


tableStack(vars = c(44:45,126),by=icd_MI,dataFrame = Description_set,test = FALSE)
tableStack(vars = c(44:45,126),by=icd_stroke,dataFrame = Description_set,test = FALSE)
tableStack(vars = c(44:45,126),by=icd_AF,dataFrame = Description_set,test = FALSE)
tableStack(vars = c(44:45,126),by=icd_HF,dataFrame = Description_set,test = FALSE)
tableStack(vars = c(44:45,126),by=icd_cvd,dataFrame = Description_set,test = FALSE)
