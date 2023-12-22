#打开数据集
library(Hmisc)
datadir<-"C:/Users/W/Desktop"
sasexe<-"F:/SAS/SASHome/SASFoundation/9.4/sas.exe"
mydatabase<-sas.get(libraryName = datadir,member = "body_cvd",sasprog = sasexe)

bodysize_set1<-mydatabase
#parental diabetes原数据集
mydatabase$parental_diabetes0<-ifelse(mydatabase$n_20107_0_0==9|mydatabase$n_20107_0_1==9|mydatabase$n_20107_0_2==9|mydatabase$n_20107_0_3==9|mydatabase$n_20107_0_4==9|mydatabase$n_20107_0_5==9|mydatabase$n_20107_0_6==9|mydatabase$n_20107_0_7==9|mydatabase$n_20107_0_8==9|mydatabase$n_20107_0_9==9,1,0)
mydatabase$parental_diabetes0[is.na(mydatabase$parental_diabetes0)]<-0
mydatabase$parental_diabetes0[mydatabase$n_20107_0_0==-11|mydatabase$n_20107_0_1==-11|mydatabase$n_20107_0_2==-11|mydatabase$n_20107_0_3==-11|mydatabase$n_20107_0_4==-11|mydatabase$n_20107_0_5==-11|mydatabase$n_20107_0_6==-11|mydatabase$n_20107_0_7==-11|mydatabase$n_20107_0_8==-11|mydatabase$n_20107_0_9==-11]<-NA
mydatabase$parental_diabetes0[mydatabase$n_20107_0_0==-13|mydatabase$n_20107_0_1==-13|mydatabase$n_20107_0_2==-13|mydatabase$n_20107_0_3==-13|mydatabase$n_20107_0_4==-13|mydatabase$n_20107_0_5==-13|mydatabase$n_20107_0_6==-13|mydatabase$n_20107_0_7==-13|mydatabase$n_20107_0_8==-13|mydatabase$n_20107_0_9==-13]<-NA
mydatabase$parental_diabetes0[is.na(mydatabase$n_20107_0_0)&is.na(mydatabase$n_20107_0_1)&is.na(mydatabase$n_20107_0_2)&is.na(mydatabase$n_20107_0_3)&is.na(mydatabase$n_20107_0_4)&is.na(mydatabase$n_20107_0_5)&is.na(mydatabase$n_20107_0_6)&is.na(mydatabase$n_20107_0_7)&is.na(mydatabase$n_20107_0_8)&is.na(mydatabase$n_20107_0_9)]<-NA #T1
mydatabase$parental_diabetes1<-ifelse(mydatabase$n_20107_1_0==9|mydatabase$n_20107_1_1==9|mydatabase$n_20107_1_2==9|mydatabase$n_20107_1_3==9|mydatabase$n_20107_1_4==9|mydatabase$n_20107_1_5==9|mydatabase$n_20107_1_6==9|mydatabase$n_20107_1_7==9,1,0)
mydatabase$parental_diabetes1[is.na(mydatabase$parental_diabetes1)]<-0
mydatabase$parental_diabetes1[mydatabase$n_20107_1_0==-11|mydatabase$n_20107_1_1==-11|mydatabase$n_20107_1_2==-11|mydatabase$n_20107_1_3==-11|mydatabase$n_20107_1_4==-11|mydatabase$n_20107_1_5==-11|mydatabase$n_20107_1_6==-11|mydatabase$n_20107_1_7==-11]<-NA
mydatabase$parental_diabetes1[mydatabase$n_20107_1_0==-13|mydatabase$n_20107_1_1==-13|mydatabase$n_20107_1_2==-13|mydatabase$n_20107_1_3==-13|mydatabase$n_20107_1_4==-13|mydatabase$n_20107_1_5==-13|mydatabase$n_20107_1_6==-13|mydatabase$n_20107_1_7==-13]<-NA
mydatabase$parental_diabetes1[is.na(mydatabase$n_20107_1_0)&is.na(mydatabase$n_20107_1_1)&is.na(mydatabase$n_20107_1_2)&is.na(mydatabase$n_20107_1_3)&is.na(mydatabase$n_20107_1_4)&is.na(mydatabase$n_20107_1_5)&is.na(mydatabase$n_20107_1_6)&is.na(mydatabase$n_20107_1_7)]<-NA #T2
mydatabase$parental_diabetes2<-ifelse(mydatabase$n_20107_2_0==9|mydatabase$n_20107_2_1==9|mydatabase$n_20107_2_2==9|mydatabase$n_20107_2_3==9|mydatabase$n_20107_2_4==9|mydatabase$n_20107_2_5==9|mydatabase$n_20107_2_6==9|mydatabase$n_20107_2_7==9,1,0)
mydatabase$parental_diabetes2[is.na(mydatabase$parental_diabetes2)]<-0
mydatabase$parental_diabetes2[mydatabase$n_20107_2_0==-11|mydatabase$n_20107_2_1==-11|mydatabase$n_20107_2_2==-11|mydatabase$n_20107_2_3==-11|mydatabase$n_20107_2_4==-11|mydatabase$n_20107_2_5==-11|mydatabase$n_20107_2_6==-11|mydatabase$n_20107_2_7==-11]<-NA
mydatabase$parental_diabetes2[mydatabase$n_20107_2_0==-13|mydatabase$n_20107_2_1==-13|mydatabase$n_20107_2_2==-13|mydatabase$n_20107_2_3==-13|mydatabase$n_20107_2_4==-13|mydatabase$n_20107_2_5==-13|mydatabase$n_20107_2_6==-13|mydatabase$n_20107_2_7==-13]<-NA
mydatabase$parental_diabetes2[is.na(mydatabase$n_20107_2_0)&is.na(mydatabase$n_20107_2_1)&is.na(mydatabase$n_20107_2_2)&is.na(mydatabase$n_20107_2_3)&is.na(mydatabase$n_20107_2_4)&is.na(mydatabase$n_20107_2_5)&is.na(mydatabase$n_20107_2_6)&is.na(mydatabase$n_20107_2_7)]<-NA #T3
mydatabase$parental_diabetes3<-ifelse(mydatabase$n_20107_3_0==9|mydatabase$n_20107_3_1==9|mydatabase$n_20107_3_2==9|mydatabase$n_20107_3_3==9|mydatabase$n_20107_3_4==9|mydatabase$n_20107_3_5==9,1,0)
mydatabase$parental_diabetes3[is.na(mydatabase$parental_diabetes3)]<-0
mydatabase$parental_diabetes3[mydatabase$n_20107_3_0==-11|mydatabase$n_20107_3_1==-11|mydatabase$n_20107_3_2==-11|mydatabase$n_20107_3_3==-11|mydatabase$n_20107_3_4==-11|mydatabase$n_20107_3_5==-11]<-NA
mydatabase$parental_diabetes3[mydatabase$n_20107_3_0==-13|mydatabase$n_20107_3_1==-13|mydatabase$n_20107_3_2==-13|mydatabase$n_20107_3_3==-13|mydatabase$n_20107_3_4==-13|mydatabase$n_20107_3_5==-13]<-NA
mydatabase$parental_diabetes3[is.na(mydatabase$n_20107_3_0)&is.na(mydatabase$n_20107_3_1)&is.na(mydatabase$n_20107_3_2)&is.na(mydatabase$n_20107_3_3)&is.na(mydatabase$n_20107_3_4)&is.na(mydatabase$n_20107_3_5)]<-NA #T4

#maternal diabetes原数据集
mydatabase$maternal_diabetes0<-ifelse(mydatabase$n_20110_0_0==9|mydatabase$n_20110_0_1==9|mydatabase$n_20110_0_2==9|mydatabase$n_20110_0_3==9|mydatabase$n_20110_0_4==9|mydatabase$n_20110_0_5==9|mydatabase$n_20110_0_6==9|mydatabase$n_20110_0_7==9|mydatabase$n_20110_0_8==9|mydatabase$n_20110_0_9==9|mydatabase$n_20110_0_10==9,1,0)
mydatabase$maternal_diabetes0[is.na(mydatabase$maternal_diabetes0)]<-0
mydatabase$maternal_diabetes0[mydatabase$n_20110_0_0==-11|mydatabase$n_20110_0_1==-11|mydatabase$n_20110_0_2==-11|mydatabase$n_20110_0_3==-11|mydatabase$n_20110_0_4==-11|mydatabase$n_20110_0_5==-11|mydatabase$n_20110_0_6==-11|mydatabase$n_20110_0_7==-11|mydatabase$n_20110_0_8==-11|mydatabase$n_20110_0_9==-11|mydatabase$n_20110_0_10==-11]<-NA
mydatabase$maternal_diabetes0[mydatabase$n_20110_0_0==-13|mydatabase$n_20110_0_1==-13|mydatabase$n_20110_0_2==-13|mydatabase$n_20110_0_3==-13|mydatabase$n_20110_0_4==-13|mydatabase$n_20110_0_5==-13|mydatabase$n_20110_0_6==-13|mydatabase$n_20110_0_7==-13|mydatabase$n_20110_0_8==-13|mydatabase$n_20110_0_9==-13|mydatabase$n_20110_0_10==-13]<-NA
mydatabase$maternal_diabetes0[is.na(mydatabase$n_20110_0_0)&is.na(mydatabase$n_20110_0_1)&is.na(mydatabase$n_20110_0_2)&is.na(mydatabase$n_20110_0_3)&is.na(mydatabase$n_20110_0_4)&is.na(mydatabase$n_20110_0_5)&is.na(mydatabase$n_20110_0_6)&is.na(mydatabase$n_20110_0_7)&is.na(mydatabase$n_20110_0_8)&is.na(mydatabase$n_20110_0_9)&is.na(mydatabase$n_20110_0_10)]<-NA #T1
mydatabase$maternal_diabetes1<-ifelse(mydatabase$n_20110_1_0==9|mydatabase$n_20110_1_1==9|mydatabase$n_20110_1_2==9|mydatabase$n_20110_1_3==9|mydatabase$n_20110_1_4==9|mydatabase$n_20110_1_5==9|mydatabase$n_20110_1_6==9,1,0)
mydatabase$maternal_diabetes1[is.na(mydatabase$maternal_diabetes1)]<-0
mydatabase$maternal_diabetes1[mydatabase$n_20110_1_0==-11|mydatabase$n_20110_1_1==-11|mydatabase$n_20110_1_2==-11|mydatabase$n_20110_1_3==-11|mydatabase$n_20110_1_4==-11|mydatabase$n_20110_1_5==-11|mydatabase$n_20110_1_6==-11]<-NA
mydatabase$maternal_diabetes1[mydatabase$n_20110_1_0==-13|mydatabase$n_20110_1_1==-13|mydatabase$n_20110_1_2==-13|mydatabase$n_20110_1_3==-13|mydatabase$n_20110_1_4==-13|mydatabase$n_20110_1_5==-13|mydatabase$n_20110_1_6==-13]<-NA
mydatabase$maternal_diabetes1[is.na(mydatabase$n_20110_1_0)&is.na(mydatabase$n_20110_1_1)&is.na(mydatabase$n_20110_1_2)&is.na(mydatabase$n_20110_1_3)&is.na(mydatabase$n_20110_1_4)&is.na(mydatabase$n_20110_1_5)&is.na(mydatabase$n_20110_1_6)]<-NA #T2
mydatabase$maternal_diabetes2<-ifelse(mydatabase$n_20110_2_0==9|mydatabase$n_20110_2_1==9|mydatabase$n_20110_2_2==9|mydatabase$n_20110_2_3==9|mydatabase$n_20110_2_4==9|mydatabase$n_20110_2_5==9|mydatabase$n_20110_2_6==9,1,0)
mydatabase$maternal_diabetes2[is.na(mydatabase$maternal_diabetes2)]<-0
mydatabase$maternal_diabetes2[mydatabase$n_20110_2_0==-11|mydatabase$n_20110_2_1==-11|mydatabase$n_20110_2_2==-11|mydatabase$n_20110_2_3==-11|mydatabase$n_20110_2_4==-11|mydatabase$n_20110_2_5==-11|mydatabase$n_20110_2_6==-11]<-NA
mydatabase$maternal_diabetes2[mydatabase$n_20110_2_0==-13|mydatabase$n_20110_2_1==-13|mydatabase$n_20110_2_2==-13|mydatabase$n_20110_2_3==-13|mydatabase$n_20110_2_4==-13|mydatabase$n_20110_2_5==-13|mydatabase$n_20110_2_6==-13]<-NA
mydatabase$maternal_diabetes2[is.na(mydatabase$n_20110_2_0)&is.na(mydatabase$n_20110_2_1)&is.na(mydatabase$n_20110_2_2)&is.na(mydatabase$n_20110_2_3)&is.na(mydatabase$n_20110_2_4)&is.na(mydatabase$n_20110_2_5)&is.na(mydatabase$n_20110_2_6)]<-NA #T3
mydatabase$maternal_diabetes3<-ifelse(mydatabase$n_20110_3_0==9|mydatabase$n_20110_3_1==9|mydatabase$n_20110_3_2==9|mydatabase$n_20110_3_3==9|mydatabase$n_20110_3_4==9,1,0)
mydatabase$maternal_diabetes3[is.na(mydatabase$maternal_diabetes3)]<-0
mydatabase$maternal_diabetes3[mydatabase$n_20110_3_0==-11|mydatabase$n_20110_3_1==-11|mydatabase$n_20110_3_2==-11|mydatabase$n_20110_3_3==-11|mydatabase$n_20110_3_4==-11]<-NA
mydatabase$maternal_diabetes3[mydatabase$n_20110_3_0==-13|mydatabase$n_20110_3_1==-13|mydatabase$n_20110_3_2==-13|mydatabase$n_20110_3_3==-13|mydatabase$n_20110_3_4==-13]<-NA
mydatabase$maternal_diabetes3[is.na(mydatabase$n_20110_3_0)&is.na(mydatabase$n_20110_3_1)&is.na(mydatabase$n_20110_3_2)&is.na(mydatabase$n_20110_3_3)&is.na(mydatabase$n_20110_3_4)]<-NA #T4

#maternal hypertension原数据集
mydatabase$maternal_hypertension0<-ifelse(mydatabase$n_20110_0_0==8|mydatabase$n_20110_0_1==8|mydatabase$n_20110_0_2==8|mydatabase$n_20110_0_3==8|mydatabase$n_20110_0_4==8|mydatabase$n_20110_0_5==8|mydatabase$n_20110_0_6==8|mydatabase$n_20110_0_7==8|mydatabase$n_20110_0_8==8|mydatabase$n_20110_0_9==8|mydatabase$n_20110_0_10==8,1,0)
mydatabase$maternal_hypertension0[is.na(mydatabase$maternal_hypertension0)]<-0
mydatabase$maternal_hypertension0[mydatabase$n_20110_0_0==-11|mydatabase$n_20110_0_1==-11|mydatabase$n_20110_0_2==-11|mydatabase$n_20110_0_3==-11|mydatabase$n_20110_0_4==-11|mydatabase$n_20110_0_5==-11|mydatabase$n_20110_0_6==-11|mydatabase$n_20110_0_7==-11|mydatabase$n_20110_0_8==-11|mydatabase$n_20110_0_9==-11|mydatabase$n_20110_0_10==-11]<-NA
mydatabase$maternal_hypertension0[mydatabase$n_20110_0_0==-13|mydatabase$n_20110_0_1==-13|mydatabase$n_20110_0_2==-13|mydatabase$n_20110_0_3==-13|mydatabase$n_20110_0_4==-13|mydatabase$n_20110_0_5==-13|mydatabase$n_20110_0_6==-13|mydatabase$n_20110_0_7==-13|mydatabase$n_20110_0_8==-13|mydatabase$n_20110_0_9==-13|mydatabase$n_20110_0_10==-13]<-NA
mydatabase$maternal_hypertension0[is.na(mydatabase$n_20110_0_0)&is.na(mydatabase$n_20110_0_1)&is.na(mydatabase$n_20110_0_2)&is.na(mydatabase$n_20110_0_3)&is.na(mydatabase$n_20110_0_4)&is.na(mydatabase$n_20110_0_5)&is.na(mydatabase$n_20110_0_6)&is.na(mydatabase$n_20110_0_7)&is.na(mydatabase$n_20110_0_8)&is.na(mydatabase$n_20110_0_9)&is.na(mydatabase$n_20110_0_10)]<-NA #T1
mydatabase$maternal_hypertension1<-ifelse(mydatabase$n_20110_1_0==8|mydatabase$n_20110_1_1==8|mydatabase$n_20110_1_2==8|mydatabase$n_20110_1_3==8|mydatabase$n_20110_1_4==8|mydatabase$n_20110_1_5==8|mydatabase$n_20110_1_6==8,1,0)
mydatabase$maternal_hypertension1[is.na(mydatabase$maternal_hypertension1)]<-0
mydatabase$maternal_hypertension1[mydatabase$n_20110_1_0==-11|mydatabase$n_20110_1_1==-11|mydatabase$n_20110_1_2==-11|mydatabase$n_20110_1_3==-11|mydatabase$n_20110_1_4==-11|mydatabase$n_20110_1_5==-11|mydatabase$n_20110_1_6==-11]<-NA
mydatabase$maternal_hypertension1[mydatabase$n_20110_1_0==-13|mydatabase$n_20110_1_1==-13|mydatabase$n_20110_1_2==-13|mydatabase$n_20110_1_3==-13|mydatabase$n_20110_1_4==-13|mydatabase$n_20110_1_5==-13|mydatabase$n_20110_1_6==-13]<-NA
mydatabase$maternal_hypertension1[is.na(mydatabase$n_20110_1_0)&is.na(mydatabase$n_20110_1_1)&is.na(mydatabase$n_20110_1_2)&is.na(mydatabase$n_20110_1_3)&is.na(mydatabase$n_20110_1_4)&is.na(mydatabase$n_20110_1_5)&is.na(mydatabase$n_20110_1_6)]<-NA #T2
mydatabase$maternal_hypertension2<-ifelse(mydatabase$n_20110_2_0==8|mydatabase$n_20110_2_1==8|mydatabase$n_20110_2_2==8|mydatabase$n_20110_2_3==8|mydatabase$n_20110_2_4==8|mydatabase$n_20110_2_5==8|mydatabase$n_20110_2_6==8,1,0)
mydatabase$maternal_hypertension2[is.na(mydatabase$maternal_hypertension2)]<-0
mydatabase$maternal_hypertension2[mydatabase$n_20110_2_0==-11|mydatabase$n_20110_2_1==-11|mydatabase$n_20110_2_2==-11|mydatabase$n_20110_2_3==-11|mydatabase$n_20110_2_4==-11|mydatabase$n_20110_2_5==-11|mydatabase$n_20110_2_6==-11]<-NA
mydatabase$maternal_hypertension2[mydatabase$n_20110_2_0==-13|mydatabase$n_20110_2_1==-13|mydatabase$n_20110_2_2==-13|mydatabase$n_20110_2_3==-13|mydatabase$n_20110_2_4==-13|mydatabase$n_20110_2_5==-13|mydatabase$n_20110_2_6==-13]<-NA
mydatabase$maternal_hypertension2[is.na(mydatabase$n_20110_2_0)&is.na(mydatabase$n_20110_2_1)&is.na(mydatabase$n_20110_2_2)&is.na(mydatabase$n_20110_2_3)&is.na(mydatabase$n_20110_2_4)&is.na(mydatabase$n_20110_2_5)&is.na(mydatabase$n_20110_2_6)]<-NA #T3
mydatabase$maternal_hypertension3<-ifelse(mydatabase$n_20110_3_0==8|mydatabase$n_20110_3_1==8|mydatabase$n_20110_3_2==8|mydatabase$n_20110_3_3==8|mydatabase$n_20110_3_4==8,1,0)
mydatabase$maternal_hypertension3[is.na(mydatabase$maternal_hypertension3)]<-0
mydatabase$maternal_hypertension3[mydatabase$n_20110_3_0==-11|mydatabase$n_20110_3_1==-11|mydatabase$n_20110_3_2==-11|mydatabase$n_20110_3_3==-11|mydatabase$n_20110_3_4==-11]<-NA
mydatabase$maternal_hypertension3[mydatabase$n_20110_3_0==-13|mydatabase$n_20110_3_1==-13|mydatabase$n_20110_3_2==-13|mydatabase$n_20110_3_3==-13|mydatabase$n_20110_3_4==-13]<-NA
mydatabase$maternal_hypertension3[is.na(mydatabase$n_20110_3_0)&is.na(mydatabase$n_20110_3_1)&is.na(mydatabase$n_20110_3_2)&is.na(mydatabase$n_20110_3_3)&is.na(mydatabase$n_20110_3_4)]<-NA #T4

#maternal smoking 原数据集
mydatabase<-within(mydatabase,{maternal_smoking0<-NA;maternal_smoking0[mydatabase$n_1787_0_0==1]<-1;maternal_smoking0[mydatabase$n_1787_0_0==0]<-0;}) #T1
mydatabase<-within(mydatabase,{maternal_smoking1<-NA;maternal_smoking1[mydatabase$n_1787_1_0==1]<-1;maternal_smoking1[mydatabase$n_1787_1_0==0]<-0;}) #T2
mydatabase<-within(mydatabase,{maternal_smoking2<-NA;maternal_smoking2[mydatabase$n_1787_2_0==1]<-1;maternal_smoking2[mydatabase$n_1787_2_0==0]<-0;}) #T3

#feeding patterns 原数据集
mydatabase<-within(mydatabase,{breastfeeding0<-NA;breastfeeding0[mydatabase$n_1677_0_0==1]<-1;breastfeeding0[mydatabase$n_1677_0_0==0]<-0}) #T1
mydatabase<-within(mydatabase,{breastfeeding1<-NA;breastfeeding1[mydatabase$n_1677_1_0==1]<-1;breastfeeding1[mydatabase$n_1677_1_0==0]<-0}) #T2
mydatabase<-within(mydatabase,{breastfeeding2<-NA;breastfeeding2[mydatabase$n_1677_2_0==1]<-1;breastfeeding2[mydatabase$n_1677_2_0==0]<-0}) #T3



#定义暴露
bodysize_set1<-within(bodysize_set1,{Comparative_body_size_at_age_10<-NA;Comparative_body_size_at_age_10<-factor(mydatabase$n_1687_0_0,levels=c(3,1,2),labels=c("About Average","Thinner","Plumper"))}) #n_1687_0_0，十岁体型三分类变量，将不知道、不回答定义为缺失,ref为3

bodysize_set1<-within(bodysize_set1,{BirthWeight_3group<-NA;BirthWeight_3group[bodysize_set1$n_20022_0_0>=4]<-3;BirthWeight_3group[bodysize_set1$n_20022_0_0>2.5&bodysize_set1$n_20022_0_0<4]<-2;BirthWeight_3group[bodysize_set1$n_20022_0_0<=2.5]<-1})
bodysize_set1$BirthWeight_3group<-factor(bodysize_set1$BirthWeight_3group,levels = c(2,1,3),labels= c("Normal","Low","High"))#n_20022_0_0,生成出生体重三分类变量，ref为NBW
bodysize_set1<-within(bodysize_set1,{Birth_Weight_kg<-NA;Birth_Weight_kg<-as.vector(bodysize_set1$n_20022_0_0)})#n_20022_0_0,数值型向量

bodysize_set1<-within(bodysize_set1,{BMI_3group<-NA;BMI_3group[bodysize_set1$n_21001_0_0>=30]<-"Obesity";BMI_3group[bodysize_set1$n_21001_0_0<30&bodysize_set1$n_21001_0_0>=25]<-"Overweight";BMI_3group[bodysize_set1$n_21001_0_0<25]<-"Normal or Thinner"})
bodysize_set1$BMI_3group<-factor(bodysize_set1$BMI_3group,levels = c("Normal or Thinner","Overweight","Obesity"))#n_21001_0_0,BMI三分类变量,ref为Normal or Thinner
bodysize_set1<-within(bodysize_set1,{BMI<-NA;BMI<-as.vector(bodysize_set1$n_21001_0_0)})#n_21001_0_0,数值型向量

bodysize_set1<-within(bodysize_set1,{bodychange1<-NA;bodychange1[bodysize_set1$BirthWeight_3group=="Low"&bodysize_set1$Comparative_body_size_at_age_10=="Thinner"]<-2;bodychange1[bodysize_set1$BirthWeight_3group=="Low"&bodysize_set1$Comparative_body_size_at_age_10=="About Average"]<-3;bodychange1[bodysize_set1$BirthWeight_3group=="Low"&bodysize_set1$Comparative_body_size_at_age_10=="Plumper"]<-4;bodychange1[bodysize_set1$BirthWeight_3group=="Normal"&bodysize_set1$Comparative_body_size_at_age_10=="Thinner"]<-5;bodychange1[bodysize_set1$BirthWeight_3group=="Normal"&bodysize_set1$Comparative_body_size_at_age_10=="About Average"]<-1;bodychange1[bodysize_set1$BirthWeight_3group=="Normal"&bodysize_set1$Comparative_body_size_at_age_10=="Plumper"]<-6;bodychange1[bodysize_set1$BirthWeight_3group=="High"&bodysize_set1$Comparative_body_size_at_age_10=="Thinner"]<-7;bodychange1[bodysize_set1$BirthWeight_3group=="High"&bodysize_set1$Comparative_body_size_at_age_10=="About Average"]<-8;bodychange1[bodysize_set1$BirthWeight_3group=="High"&bodysize_set1$Comparative_body_size_at_age_10=="Plumper"]<-9})
bodysize_set1$bodychange1<-factor(bodysize_set1$bodychange1,levels = c(1:9),labels = c("Normal BW → About Average at 10 years old","Low BW → Thinner at 10 years old","Low BW → About Average at 10 years old","Low BW → Plumper at 10 years old","Normal BW → Thinner at 10 years old","Normal BW → Plumper at 10 years old","High BW → Thinner at 10 years old","High BW → About Average at 10 years old","High BW → Plumper at 10 years old"))

bodysize_set1<-within(bodysize_set1,{
  bodychange2<-NA;bodychange2[bodysize_set1$BirthWeight_3group=="Low"&bodysize_set1$Comparative_body_size_at_age_10=="Thinner"&bodysize_set1$BMI_3group=="Normal or Thinner"]<-2
  bodychange2[bodysize_set1$BirthWeight_3group=="Low"&bodysize_set1$Comparative_body_size_at_age_10=="Thinner"&bodysize_set1$BMI_3group=="Overweight"]<-3
  bodychange2[bodysize_set1$BirthWeight_3group=="Low"&bodysize_set1$Comparative_body_size_at_age_10=="Thinner"&bodysize_set1$BMI_3group=="Obesity"]<-4
  bodychange2[bodysize_set1$BirthWeight_3group=="Low"&bodysize_set1$Comparative_body_size_at_age_10=="About Average"&bodysize_set1$BMI_3group=="Normal or Thinner"]<-5
  bodychange2[bodysize_set1$BirthWeight_3group=="Low"&bodysize_set1$Comparative_body_size_at_age_10=="About Average"&bodysize_set1$BMI_3group=="Overweight"]<-6
  bodychange2[bodysize_set1$BirthWeight_3group=="Low"&bodysize_set1$Comparative_body_size_at_age_10=="About Average"&bodysize_set1$BMI_3group=="Obesity"]<-7
  bodychange2[bodysize_set1$BirthWeight_3group=="Low"&bodysize_set1$Comparative_body_size_at_age_10=="Plumper"&bodysize_set1$BMI_3group=="Normal or Thinner"]<-8
  bodychange2[bodysize_set1$BirthWeight_3group=="Low"&bodysize_set1$Comparative_body_size_at_age_10=="Plumper"&bodysize_set1$BMI_3group=="Overweight"]<-9
  bodychange2[bodysize_set1$BirthWeight_3group=="Low"&bodysize_set1$Comparative_body_size_at_age_10=="Plumper"&bodysize_set1$BMI_3group=="Obesity"]<-10
  bodychange2[bodysize_set1$BirthWeight_3group=="Normal"&bodysize_set1$Comparative_body_size_at_age_10=="Thinner"&bodysize_set1$BMI_3group=="Normal or Thinner"]<-11
  bodychange2[bodysize_set1$BirthWeight_3group=="Normal"&bodysize_set1$Comparative_body_size_at_age_10=="Thinner"&bodysize_set1$BMI_3group=="Overweight"]<-12
  bodychange2[bodysize_set1$BirthWeight_3group=="Normal"&bodysize_set1$Comparative_body_size_at_age_10=="Thinner"&bodysize_set1$BMI_3group=="Obesity"]<-13
  bodychange2[bodysize_set1$BirthWeight_3group=="Normal"&bodysize_set1$Comparative_body_size_at_age_10=="About Average"&bodysize_set1$BMI_3group=="Normal or Thinner"]<-1
  bodychange2[bodysize_set1$BirthWeight_3group=="Normal"&bodysize_set1$Comparative_body_size_at_age_10=="About Average"&bodysize_set1$BMI_3group=="Overweight"]<-14
  bodychange2[bodysize_set1$BirthWeight_3group=="Normal"&bodysize_set1$Comparative_body_size_at_age_10=="About Average"&bodysize_set1$BMI_3group=="Obesity"]<-15
  bodychange2[bodysize_set1$BirthWeight_3group=="Normal"&bodysize_set1$Comparative_body_size_at_age_10=="Plumper"&bodysize_set1$BMI_3group=="Normal or Thinner"]<-16
  bodychange2[bodysize_set1$BirthWeight_3group=="Normal"&bodysize_set1$Comparative_body_size_at_age_10=="Plumper"&bodysize_set1$BMI_3group=="Overweight"]<-17
  bodychange2[bodysize_set1$BirthWeight_3group=="Normal"&bodysize_set1$Comparative_body_size_at_age_10=="Plumper"&bodysize_set1$BMI_3group=="Obesity"]<-18
  bodychange2[bodysize_set1$BirthWeight_3group=="High"&bodysize_set1$Comparative_body_size_at_age_10=="Thinner"&bodysize_set1$BMI_3group=="Normal or Thinner"]<-19
  bodychange2[bodysize_set1$BirthWeight_3group=="High"&bodysize_set1$Comparative_body_size_at_age_10=="Thinner"&bodysize_set1$BMI_3group=="Overweight"]<-20
  bodychange2[bodysize_set1$BirthWeight_3group=="High"&bodysize_set1$Comparative_body_size_at_age_10=="Thinner"&bodysize_set1$BMI_3group=="Obesity"]<-21
  bodychange2[bodysize_set1$BirthWeight_3group=="High"&bodysize_set1$Comparative_body_size_at_age_10=="About Average"&bodysize_set1$BMI_3group=="Normal or Thinner"]<-22
  bodychange2[bodysize_set1$BirthWeight_3group=="High"&bodysize_set1$Comparative_body_size_at_age_10=="About Average"&bodysize_set1$BMI_3group=="Overweight"]<-23
  bodychange2[bodysize_set1$BirthWeight_3group=="High"&bodysize_set1$Comparative_body_size_at_age_10=="About Average"&bodysize_set1$BMI_3group=="Obesity"]<-24
  bodychange2[bodysize_set1$BirthWeight_3group=="High"&bodysize_set1$Comparative_body_size_at_age_10=="Plumper"&bodysize_set1$BMI_3group=="Normal or Thinner"]<-25
  bodychange2[bodysize_set1$BirthWeight_3group=="High"&bodysize_set1$Comparative_body_size_at_age_10=="Plumper"&bodysize_set1$BMI_3group=="Overweight"]<-26
  bodychange2[bodysize_set1$BirthWeight_3group=="High"&bodysize_set1$Comparative_body_size_at_age_10=="Plumper"&bodysize_set1$BMI_3group=="Obesity"]<-27
})
bodysize_set1$bodychange2<-factor(bodysize_set1$bodychange2,levels = c(1:27),labels = c("Normal BW → About Average at 10 years old → Normal at adulthood","Low BW → Thinner at 10 years old → Normal at adulthood","Low BW → Thinner at 10 years old → Overweight at adulthood","Low BW → Thinner at 10 years old → Obesity at adulthood","Low BW → About Average at 10 years old → Normal at adulthood","Low BW → About Average at 10 years old → Overweight at adulthood","Low BW → About Average at 10 years old → Obesity at adulthood","Low BW → Plumper at 10 years old → Normal at adulthood","Low BW → Plumper at 10 years old → Overweight at adulthood","Low BW → Plumper at 10 years old → Obesity at adulthood","Normal BW → Thinner at 10 years old → Normal at adulthood","Normal BW → Thinner at 10 years old → Overweight at adulthood","Normal BW → Thinner at 10 years old → Obesity at adulthood","Normal BW → About Average at 10 years old → Overweight at adulthood","Normal BW → About Average at 10 years old → Obesity at adulthood","Normal BW → Plumper at 10 years old → Normal at adulthood","Normal BW → Plumper at 10 years old → Overweight at adulthood","Normal BW → Plumper at 10 years old → Obesity at adulthood","High BW → Thinner at 10 years old → Normal at adulthood","High BW → Thinner at 10 years old → Overweight at adulthood","High BW → Thinner at 10 years old → Obesity at adulthood","High BW → About Average at 10 years old → Normal at adulthood","High BW → About Average at 10 years old → Overweight at adulthood","High BW → About Average at 10 years old → Obesity at adulthood","High BW → Plumper at 10 years old → Normal at adulthood","High BW → Plumper at 10 years old → Overweight at adulthood","High BW → Plumper at 10 years old → Obesity at adulthood"))

#定义死亡
bodysize_set1<-within(bodysize_set1,{death<-as.character(bodysize_set1$s_40000_0_0);death[is.na(death)]<-"0";death[death!="0"]<-2}) #s_40000_0_0，二分类，将有死亡定义为2
bodysize_set1$death<-as.numeric(bodysize_set1$death) #s_40000_0_0,数值型向量
library(lubridate)
bodysize_set1<-within(bodysize_set1,{deathyear<-year(bodysize_set1$s_40000_0_0)}) #提取死亡年份

#定义结局MI
bodysize_set1<-within(bodysize_set1,{icd_MI<-as.character(bodysize_set1$s_42000_0_0);icd_MI[is.na(icd_MI)]<-0;icd_MI[icd_MI=="1900-01-01"]<-NA;icd_MI[icd_MI!="0"]<-1}) #根据s_42000_0_0定义是否发生MI，0未发病，1发病
bodysize_set1$icd_MI<-as.numeric(bodysize_set1$icd_MI)#s_42000_0_0,数值型向量
bodysize_set1<-within(bodysize_set1,{year_MI<-year(bodysize_set1$s_42000_0_0)}) #提取MI首发年份
bodysize_set1$followstop_MI<- ifelse(bodysize_set1$icd_MI==1,bodysize_set1$year_MI,ifelse(bodysize_set1$death==2,bodysize_set1$deathyear,2022))#定义MI随访终止年份 #如果death为1，icd为1，则MIyear；否则if death为1，icd为0，则deathyear，否则if death为0，icd为1，MIyear；否则2022
bodysize_set1$timeMI_bw<-bodysize_set1$followstop_MI - bodysize_set1$birthyear #定义Birth Weight随访时间
bodysize_set1$timeMI_10y<-bodysize_set1$timeMI_bw-10 #定义10岁bodysize随访时间
bodysize_set1$timeMI_BMI<-bodysize_set1$followstop_MI- bodysize_set1$t1year#定义中年BMI随访时间
bodysize_set1$risk_MI<-ifelse(bodysize_set1$icd_MI==1,1,ifelse(bodysize_set1$death==0,0,2)) #定义死亡竞争风险变量,0未发病，1发病，2死亡  #如果icd为1，则1，否则（如果death为0，0，2）

#定义结局STEMI
bodysize_set1<-within(bodysize_set1,{icd_STEMI<-as.character(bodysize_set1$s_42002_0_0);icd_STEMI[is.na(icd_STEMI)]<-0;icd_STEMI[icd_STEMI=="1900-01-01"]<-NA;icd_STEMI[icd_STEMI!="0"]<-1}) #根据s_42002_0_0定义是否发生STEMI，0未发病，1发病
bodysize_set1$icd_STEMI<-as.numeric(bodysize_set1$icd_STEMI)#s_42002_0_0,数值型向量
bodysize_set1<-within(bodysize_set1,{year_STEMI<-year(bodysize_set1$s_42002_0_0)}) #提取STEMI首发年份
bodysize_set1$followstop_STEMI<-ifelse(bodysize_set1$icd_STEMI==1,bodysize_set1$year_STEMI,ifelse(bodysize_set1$death==2,bodysize_set1$deathyear,2022))#定义STEMI随访终止年份 
bodysize_set1$timeSTEMI_bw<-bodysize_set1$followstop_STEMI - bodysize_set1$birthyear #定义Birth Weight随访时间
bodysize_set1$timeSTEMI_10y<-bodysize_set1$timeSTEMI_bw-10 #定义10岁bodysize随访时间
bodysize_set1$timeSTEMI_BMI<-bodysize_set1$followstop_STEMI- bodysize_set1$t1year#定义中年BMI随访时间
bodysize_set1$risk_STEMI<-ifelse(bodysize_set1$icd_STEMI==1,1,ifelse(bodysize_set1$death==0,0,2)) #定义死亡竞争风险变量,0未发病，1发病，2死亡  #如果icd为1，则1，否则（如果death为0，0，2）

#定义结局NSTEMI
bodysize_set1<-within(bodysize_set1,{icd_NSTEMI<-as.character(bodysize_set1$s_42004_0_0);icd_NSTEMI[is.na(icd_NSTEMI)]<-0;icd_NSTEMI[icd_NSTEMI=="1900-01-01"]<-NA;icd_NSTEMI[icd_NSTEMI!="0"]<-1}) #根据s_42004_0_0定义是否发生NSTEMI，0未发病，1发病
bodysize_set1$icd_NSTEMI<-as.numeric(bodysize_set1$icd_NSTEMI)#s_42004_0_0,数值型向量
bodysize_set1<-within(bodysize_set1,{year_NSTEMI<-year(bodysize_set1$s_42004_0_0)}) #提取NSTEMI首发年份
bodysize_set1$followstop_NSTEMI<-ifelse(bodysize_set1$icd_NSTEMI==1,bodysize_set1$year_NSTEMI,ifelse(bodysize_set1$death==2,bodysize_set1$deathyear,2022))#定义NSTEMI随访终止年份 
bodysize_set1$timeNSTEMI_bw<-bodysize_set1$followstop_NSTEMI - bodysize_set1$birthyear #定义Birth Weight随访时间
bodysize_set1$timeNSTEMI_10y<-bodysize_set1$timeNSTEMI_bw-10 #定义10岁bodysize随访时间
bodysize_set1$timeNSTEMI_BMI<-bodysize_set1$followstop_NSTEMI- bodysize_set1$t1year#定义中年BMI随访时间
bodysize_set1$risk_NSTEMI<-ifelse(bodysize_set1$icd_NSTEMI==1,1,ifelse(bodysize_set1$death==0,0,2)) #定义死亡竞争风险变量,0未发病，1发病，2死亡  #如果icd为1，则1，否则（如果death为0，0，2）

#定义结局stroke
bodysize_set1<-within(bodysize_set1,{icd_stroke<-as.character(bodysize_set1$s_42006_0_0);icd_stroke[is.na(icd_stroke)]<-0;icd_stroke[icd_stroke=="1900-01-01"]<-NA;icd_stroke[icd_stroke!="0"]<-1}) #根据s_42006_0_0定义是否发生stroke，0未发病，1发病
bodysize_set1$icd_stroke<-as.numeric(bodysize_set1$icd_stroke) #s_42006_0_0,数值型向量
bodysize_set1<-within(bodysize_set1,{year_stroke<-year(bodysize_set1$s_42006_0_0)}) #提取stroke首发年份
bodysize_set1$followstop_stroke<-ifelse(bodysize_set1$icd_stroke==1,bodysize_set1$year_stroke,ifelse(bodysize_set1$death==2,bodysize_set1$deathyear,2022))#定义stroke随访终止年份 
bodysize_set1$timestroke_bw<-bodysize_set1$followstop_stroke - bodysize_set1$birthyear #定义Birth Weight随访时间
bodysize_set1$timestroke_10y<-bodysize_set1$timestroke_bw-10 #定义10岁bodysize随访时间
bodysize_set1$timestroke_BMI<-bodysize_set1$followstop_stroke- bodysize_set1$t1year#定义中年BMI随访时间
bodysize_set1$risk_stroke<-ifelse(bodysize_set1$icd_stroke==1,1,ifelse(bodysize_set1$death==0,0,2)) #定义死亡竞争风险变量,0未发病，1发病，2死亡 #如果icd为1，则1，否则（如果death为0，0，2）

#定义结局ischaemic stroke，IS
bodysize_set1<-within(bodysize_set1,{icd_IS<-as.character(bodysize_set1$s_42008_0_0);icd_IS[is.na(icd_IS)]<-0;icd_IS[icd_IS=="1900-01-01"]<-NA;icd_IS[icd_IS!="0"]<-1}) #根据s_42008_0_0定义是否发生ischaemic stroke，0未发病，1发病
bodysize_set1$icd_IS<-as.numeric(bodysize_set1$icd_IS) #s_42008_0_0,数值型向量
bodysize_set1<-within(bodysize_set1,{year_IS<-year(bodysize_set1$s_42008_0_0)}) #提取ischaemic stroke首发年份
bodysize_set1$followstop_IS<-ifelse(bodysize_set1$icd_IS==1,bodysize_set1$year_IS,ifelse(bodysize_set1$death==2,bodysize_set1$deathyear,2022))#定义ischaemic stroke随访终止年份 
bodysize_set1$timeIS_bw<-bodysize_set1$followstop_IS - bodysize_set1$birthyear #定义Birth Weight随访时间
bodysize_set1$timeIS_10y<-bodysize_set1$timeIS_bw-10 #定义10岁bodysize随访时间
bodysize_set1$timeIS_BMI<-bodysize_set1$followstop_IS- bodysize_set1$t1year#定义中年BMI随访时间
bodysize_set1$risk_IS<-ifelse(bodysize_set1$icd_IS==1,1,ifelse(bodysize_set1$death==0,0,2)) #定义死亡竞争风险变量,0未发病，1发病，2死亡 #如果icd为1，则1，否则（如果death为0，0，2）

#定义结局intracerebral haemorrhage，IH
bodysize_set1<-within(bodysize_set1,{icd_IH<-as.character(bodysize_set1$s_42010_0_0);icd_IH[is.na(icd_IH)]<-0;icd_IH[icd_IH=="1900-01-01"]<-NA;icd_IH[icd_IH!="0"]<-1}) #根据s_42010_0_0定义是否发生intracerebral haemorrhage，0未发病，1发病
bodysize_set1$icd_IH<-as.numeric(bodysize_set1$icd_IH) #s_42010_0_0,数值型向量
bodysize_set1<-within(bodysize_set1,{year_IH<-year(bodysize_set1$s_42010_0_0)}) #提取intracerebral haemorrhage首发年份
bodysize_set1$followstop_IH<-ifelse(bodysize_set1$icd_IH==1,bodysize_set1$year_IH,ifelse(bodysize_set1$death==2,bodysize_set1$deathyear,2022))#定义intracerebral haemorrhage随访终止年份 
bodysize_set1$timeIH_bw<-bodysize_set1$followstop_IH - bodysize_set1$birthyear #定义Birth Weight随访时间
bodysize_set1$timeIH_10y<-bodysize_set1$timeIH_bw-10 #定义10岁bodysize随访时间
bodysize_set1$timeIH_BMI<-bodysize_set1$followstop_IH- bodysize_set1$t1year#定义中年BMI随访时间
bodysize_set1$risk_IH<-ifelse(bodysize_set1$icd_IH==1,1,ifelse(bodysize_set1$death==0,0,2)) #定义死亡竞争风险变量,0未发病，1发病，2死亡 #如果icd为1，则1，否则（如果death为0，0，2）

#定义结局subarachnoid haemorrhage，SH
bodysize_set1<-within(bodysize_set1,{icd_SH<-as.character(bodysize_set1$s_42012_0_0);icd_SH[is.na(icd_SH)]<-0;icd_SH[icd_SH=="1900-01-01"]<-NA;icd_SH[icd_SH!="0"]<-1}) #根据s_42012_0_0定义是否发生subarachnoid haemorrhage，0未发病，1发病
bodysize_set1$icd_SH<-as.numeric(bodysize_set1$icd_SH) #s_42012_0_0,数值型向量
bodysize_set1<-within(bodysize_set1,{year_SH<-year(bodysize_set1$s_42012_0_0)}) #提取subarachnoid haemorrhage首发年份
bodysize_set1$followstop_SH<-ifelse(bodysize_set1$icd_SH==1,bodysize_set1$year_SH,ifelse(bodysize_set1$death==2,bodysize_set1$deathyear,2022))#定义subarachnoid haemorrhage随访终止年份 
bodysize_set1$timeSH_bw<-bodysize_set1$followstop_SH - bodysize_set1$birthyear #定义Birth Weight随访时间
bodysize_set1$timeSH_10y<-bodysize_set1$timeSH_bw-10 #定义10岁bodysize随访时间
bodysize_set1$timeSH_BMI<-bodysize_set1$followstop_SH- bodysize_set1$t1year#定义中年BMI随访时间
bodysize_set1$risk_SH<-ifelse(bodysize_set1$icd_SH==1,1,ifelse(bodysize_set1$death==0,0,2)) #定义死亡竞争风险变量,0未发病，1发病，2死亡 #如果icd为1，则1，否则（如果death为0，0，2）

#定义结局AF
bodysize_set1<-within(bodysize_set1,{icd_AF<-as.character(bodysize_set1$s_131350_0_0);icd_AF[is.na(icd_AF)]<-0;icd_AF[icd_AF=="1902-02-02"]<-NA;icd_AF[icd_AF!="0"]<-1}) #根据s_131350_0_0定义是否发生AF，0未发病，1发病
bodysize_set1$icd_AF<-as.numeric(bodysize_set1$icd_AF) #s_131350_0_0,数值型向量
bodysize_set1<-within(bodysize_set1,{year_AF<-year(bodysize_set1$s_131350_0_0)}) #提取AF首发年份
bodysize_set1$followstop_AF<-ifelse(bodysize_set1$icd_AF==1,bodysize_set1$year_AF,ifelse(bodysize_set1$death==2,bodysize_set1$deathyear,2022))#定义AF随访终止年份 
bodysize_set1$timeAF_bw<-bodysize_set1$followstop_AF - bodysize_set1$birthyear #定义Birth Weight随访时间
bodysize_set1$timeAF_10y<-bodysize_set1$timeAF_bw-10 #定义10岁bodysize随访时间
bodysize_set1$timeAF_BMI<-bodysize_set1$followstop_AF- bodysize_set1$t1year#定义中年BMI随访时间
bodysize_set1$risk_AF<-ifelse(bodysize_set1$icd_AF==1,1,ifelse(bodysize_set1$death==0,0,2)) #定义死亡竞争风险变量,0未发病，1发病，2死亡 #如果icd为1，则1，否则（如果death为0，0，2）

#定义结局HF
bodysize_set1<-within(bodysize_set1,{icd_HF<-as.character(bodysize_set1$s_131354_0_0);icd_HF[is.na(icd_HF)]<-0;icd_HF[icd_HF!="0"]<-1}) #根据s_131354_0_0定义是否发生HF，0未发病，1发病
bodysize_set1$icd_HF<-as.numeric(bodysize_set1$icd_HF) #s_131354_0_0,数值型向量
bodysize_set1<-within(bodysize_set1,{year_HF<-year(bodysize_set1$s_131354_0_0)}) #提取HF首发年份
bodysize_set1$followstop_HF<-ifelse(bodysize_set1$icd_HF==1,bodysize_set1$year_HF,ifelse(bodysize_set1$death==2,bodysize_set1$deathyear,2022))#定义HF随访终止年份 
bodysize_set1$timeHF_bw<-bodysize_set1$followstop_HF - bodysize_set1$birthyear #定义Birth Weight随访时间
bodysize_set1$timeHF_10y<-bodysize_set1$timeHF_bw-10 #定义10岁bodysize随访时间
bodysize_set1$timeHF_BMI<-bodysize_set1$followstop_HF- bodysize_set1$t1year#定义中年BMI随访时间
bodysize_set1$risk_HF<-ifelse(bodysize_set1$icd_HF==1,1,ifelse(bodysize_set1$death==0,0,2)) #定义死亡竞争风险变量,0未发病，1发病，2死亡 #如果icd为1，则1，否则（如果death为0，0，2）

#定义总结局cvd（MI,stroke,AF,HF）
bodysize_set1<-within(bodysize_set1,{num_cvd<-NA;num_cvd[icd_MI==0&icd_HF==0&icd_AF==0&icd_stroke==0]<-0;num_cvd[icd_MI==1&icd_HF==0&icd_AF==0&icd_stroke==0]<-1;num_cvd[icd_MI==0&icd_HF==1&icd_AF==0&icd_stroke==0]<-1;num_cvd[icd_MI==0&icd_HF==0&icd_AF==1&icd_stroke==0]<-1;num_cvd[icd_MI==0&icd_HF==0&icd_AF==0&icd_stroke==1]<-1;num_cvd[icd_MI==1&icd_HF==1&icd_AF==0&icd_stroke==0]<-2;num_cvd[icd_MI==1&icd_HF==0&icd_AF==1&icd_stroke==0]<-2;num_cvd[icd_MI==1&icd_HF==0&icd_AF==0&icd_stroke==1]<-2;num_cvd[icd_MI==0&icd_HF==1&icd_AF==1&icd_stroke==0]<-2;num_cvd[icd_MI==0&icd_HF==1&icd_AF==0&icd_stroke==1]<-2;num_cvd[icd_MI==0&icd_HF==0&icd_AF==1&icd_stroke==1]<-2;num_cvd[icd_MI==1&icd_HF==1&icd_AF==1&icd_stroke==0]<-3;num_cvd[icd_MI==1&icd_HF==1&icd_AF==0&icd_stroke==1]<-3;num_cvd[icd_MI==1&icd_HF==0&icd_AF==1&icd_stroke==1]<-3;num_cvd[icd_MI==0&icd_HF==1&icd_AF==1&icd_stroke==1]<-3;num_cvd[icd_MI==1&icd_HF==1&icd_AF==1&icd_stroke==1]<-4})#cvd数量
bodysize_set1$icd_cvd<-ifelse(bodysize_set1$icd_MI==0&bodysize_set1$icd_stroke==0&bodysize_set1$icd_AF==0&bodysize_set1$icd_HF==0,0,1) #四个均0，为0；1个及以上，为1
min1<-pmin(bodysize_set1$followstop_MI,bodysize_set1$followstop_stroke)
min2<-pmin(min1,bodysize_set1$followstop_AF)
bodysize_set1$followstop_cvd<-pmin(min2,bodysize_set1$followstop_HF)
bodysize_set1$timecvd_bw<-bodysize_set1$followstop_cvd - bodysize_set1$birthyear #定义Birth Weight随访时间
bodysize_set1$timecvd_10y<-bodysize_set1$timecvd_bw-10 #定义10岁bodysize随访时间
bodysize_set1$timecvd_BMI<-bodysize_set1$followstop_cvd- bodysize_set1$t1year#定义中年BMI随访时间
bodysize_set1$risk_cvd<-ifelse(bodysize_set1$icd_cvd==1,1,ifelse(bodysize_set1$death==0,0,2)) #定义死亡竞争风险变量,0未发病，1发病，2死亡 #如果icd为1，则1，否则（如果death为0，0，2）

#定义协变量
bodysize_set1$sex<-factor(bodysize_set1$n_31_0_0,levels=c(1,0),labels=c("male","female")) #性别转因子
bodysize_set1$race<-factor(bodysize_set1$race_n,levels = c(1,2),labels = c("white","non-white"))  #种族转因子
bodysize_set1$cvd_FH<-factor(bodysize_set1$cvd_family_history,levels = c(0,1),labels = c("n","y")) #cvd家族史转因子

bodysize_set1$maternal_smoking<-ifelse(mydatabase$maternal_smoking0==1|mydatabase$maternal_smoking1==1|mydatabase$maternal_smoking2==1,1,0)
bodysize_set1$maternal_smoking[is.na(bodysize_set1$maternal_smoking)]<-0
bodysize_set1$maternal_smoking[is.na(mydatabase$maternal_smoking0)&is.na(mydatabase$maternal_smoking1)&is.na(mydatabase$maternal_smoking2)]<-NA #定义maternal smoking,缺失68126,三次中有1次为1定义为1，否则为0
bodysize_set1$breastfeeding<-ifelse(mydatabase$breastfeeding0==1|mydatabase$breastfeeding1==1|mydatabase$breastfeeding2==1,1,0)
bodysize_set1$breastfeeding[is.na(bodysize_set1$breastfeeding)]<-0
bodysize_set1$breastfeeding[is.na(mydatabase$breastfeeding0)&is.na(mydatabase$breastfeeding1)&is.na(mydatabase$breastfeeding2)]<-NA #定义feeding patterns
bodysize_set1$maternal_hypertension<-ifelse(mydatabase$maternal_hypertension0==1|mydatabase$maternal_hypertension1==1|mydatabase$maternal_hypertension2==1|mydatabase$maternal_hypertension3==1,1,0)
bodysize_set1$maternal_hypertension[is.na(bodysize_set1$maternal_hypertension)]<-0
bodysize_set1$maternal_hypertension[is.na(mydatabase$maternal_hypertension0)&is.na(mydatabase$maternal_hypertension1)&is.na(mydatabase$maternal_hypertension2)&is.na(mydatabase$maternal_hypertension3)]<-NA #定义maternal hypertension，T0\1\2\3中有一次是1定义为1，四次全部为NA定义NA，其它定义为0
bodysize_set1$maternal_diabetes<-ifelse(mydatabase$maternal_diabetes0==1|mydatabase$maternal_diabetes1==1|mydatabase$maternal_diabetes2==1|mydatabase$maternal_diabetes3==1,1,0)
bodysize_set1$maternal_diabetes[is.na(bodysize_set1$maternal_diabetes)]<-0
bodysize_set1$maternal_diabetes[is.na(mydatabase$maternal_diabetes0)&is.na(mydatabase$maternal_diabetes1)&is.na(mydatabase$maternal_diabetes2)&is.na(mydatabase$maternal_diabetes3)]<-NA #定义maternal diabetes
bodysize_set1$parental_diabetes<-ifelse(mydatabase$parental_diabetes0==1|mydatabase$parental_diabetes1==1|mydatabase$parental_diabetes2==1|mydatabase$parental_diabetes3==1,1,0)
bodysize_set1$parental_diabetes[is.na(bodysize_set1$parental_diabetes)]<-0
bodysize_set1$parental_diabetes[is.na(mydatabase$parental_diabetes0)&is.na(mydatabase$parental_diabetes1)&is.na(mydatabase$parental_diabetes2)&is.na(mydatabase$parental_diabetes3)]<-NA #定义parental diabetes

bodysize_set1<-within(bodysize_set1,{bodychange3<-NA;bodychange3[bodysize_set1$BMI_3group=="Overweight"&bodysize_set1$Comparative_body_size_at_age_10=="Thinner"]<-3;bodychange3[bodysize_set1$BMI_3group=="Overweight"&bodysize_set1$Comparative_body_size_at_age_10=="About Average"]<-5;bodychange3[bodysize_set1$BMI_3group=="Overweight"&bodysize_set1$Comparative_body_size_at_age_10=="Plumper"]<-8;bodychange3[bodysize_set1$BMI_3group=="Normal or Thinner"&bodysize_set1$Comparative_body_size_at_age_10=="Thinner"]<-2;bodychange3[bodysize_set1$BMI_3group=="Normal or Thinner"&bodysize_set1$Comparative_body_size_at_age_10=="About Average"]<-1;bodychange3[bodysize_set1$BMI_3group=="Normal or Thinner"&bodysize_set1$Comparative_body_size_at_age_10=="Plumper"]<-7;bodychange3[bodysize_set1$BMI_3group=="Obesity"&bodysize_set1$Comparative_body_size_at_age_10=="Thinner"]<-4;bodychange3[bodysize_set1$BMI_3group=="Obesity"&bodysize_set1$Comparative_body_size_at_age_10=="About Average"]<-6;bodychange3[bodysize_set1$BMI_3group=="Obesity"&bodysize_set1$Comparative_body_size_at_age_10=="Plumper"]<-9})
bodysize_set1$bodychange3<-factor(bodysize_set1$bodychange3,levels = c(1:9),labels = c("About Average at 10 years old → Normal or Thinner","Thinner at 10 years old → Normal or Thinner","Thinner at 10 years old → Overweight","Thinner at 10 years old → Obesity","About Average at 10 years old → Overweight","About Average at 10 years old → Obesity","Plumper at 10 years old → Normal or Thinner","Plumper at 10 years old → Overweight","Plumper at 10 years old → Obesity"))


bodysize_set1$TDI<-as.vector(bodysize_set1$n_189_0_0) #Townsend index
library(dplyr)
bodysize_set1<-bodysize_set1 %>% mutate(maternal_hypertension=factor(maternal_hypertension,levels=c(0,1),labels=c("n","y")),maternal_diabetes=factor(maternal_diabetes,levels=c(0,1),labels=c("n","y")),parental_diabetes=factor(parental_diabetes,levels=c(0,1),labels=c("n","y")),breastfeeding=factor(breastfeeding,levels=c(1,0),labels=c("y","n")),maternal_smoking=factor(maternal_smoking,levels=c(0,1),labels=c("n","y")),                  education_levels=factor(education1,levels=c(1,2,3),labels=c("<=10","11-12",">12")),leisure_2group=factor(leisure,levels=c(1,0),labels=c("y","n")),sports_2group=factor(sports,levels=c(1,0),labels=c("y","n")),physical_activity=factor(pa_level,levels=c(0,1,2),labels=c("low","moderate","high")),income_levels=factor(income,levels=c(1,2,3,4),labels=c("<18000","18000-30999","31000-51999",">=52000")),smoking_status=factor(smoking_b,levels = c(0,1,2),labels = c("never","previous","current")),alcohol_status=factor(alcohol_status_b,levels = c(0,1,2),labels = c("never","previous","current")))

#删除缺失值
library(tidyr)
bodysize_set2 <- bodysize_set1 %>% drop_na(Birth_Weight_kg) 
bodysize_set2 <- bodysize_set2 %>% drop_na(Comparative_body_size_at_age_10) 
bodysize_set2 <- bodysize_set2 %>% drop_na(BMI)
bodysize_set2 <- bodysize_set2 %>% drop_na(icd_MI)
bodysize_set2 <- bodysize_set2 %>% drop_na(icd_stroke)
bodysize_set2 <- bodysize_set2 %>% drop_na(icd_AF) 
bodysize_set2 <- bodysize_set2 %>% drop_na(icd_HF) 
bodysize_set2 <- bodysize_set2 %>% drop_na(icd_cvd) 

bodysize_set2 <- bodysize_set2 %>% drop_na(cvd_FH)
bodysize_set2 <- bodysize_set2 %>% drop_na(maternal_hypertension)
bodysize_set2 <- bodysize_set2 %>% drop_na(maternal_diabetes)
bodysize_set2 <- bodysize_set2 %>% drop_na(parental_diabetes)
bodysize_set2 <- bodysize_set2 %>% drop_na(breastfeeding)
bodysize_set2 <- bodysize_set2 %>% drop_na(maternal_smoking)


#建立分析数据库
bodysize_set3<-subset(bodysize_set2,timeAF_BMI>0&timeHF_BMI>0&timestroke_BMI>0&timeMI_BMI>0) #删除变量中的负值(即删除基线之前发病的观测)
bodysize_set3<-bodysize_set3[,-c(1:95)] #删除不需要的变量

library(tidyr) #删除生活方式因素缺失
bodysize_set3 <- bodysize_set3 %>% drop_na(TDI)
bodysize_set3 <- bodysize_set3 %>% drop_na(income_levels)
bodysize_set3 <- bodysize_set3 %>% drop_na(education_levels)
bodysize_set3 <- bodysize_set3 %>% drop_na(leisure_2group)
bodysize_set3 <- bodysize_set3 %>% drop_na(sports_2group)
bodysize_set3 <- bodysize_set3 %>% drop_na(physical_activity)
bodysize_set3 <- bodysize_set3 %>% drop_na(smoking_status)
bodysize_set3 <- bodysize_set3 %>% drop_na(alcohol_status)

library(beepr)
beep(8)