#model 3 
sensitivity1_set<-subset(bodysize_set3,Birth_Weight_kg>=1&Birth_Weight_kg<=5)
sensitivity2_set1<-subset(bodysize_set3,cvd_FH=="y")

library(epiDisplay)
# sensitivity analysis 1
tableStack(vars = c(44:45,126),by="none",iqr=NULL,dataFrame = sensitivity1_set,test = FALSE) 
tableStack(vars = c(44:45,126),by=icd_MI,dataFrame = sensitivity1_set,test = FALSE)
tableStack(vars = c(44:45,126),by=icd_stroke,dataFrame = sensitivity1_set,test = FALSE)
tableStack(vars = c(44:45,126),by=icd_AF,dataFrame = sensitivity1_set,test = FALSE)
tableStack(vars = c(44:45,126),by=icd_HF,dataFrame = sensitivity1_set,test = FALSE)
tableStack(vars = c(44:45,126),by=icd_cvd,dataFrame = sensitivity1_set,test = FALSE)
# sensitivity analysis 2
tableStack(vars = c(44:45,126),by="none",iqr=NULL,dataFrame = sensitivity2_set1,test = FALSE) 
tableStack(vars = c(44:45,126),by=icd_MI,dataFrame = sensitivity2_set1,test = FALSE)
tableStack(vars = c(44:45,126),by=icd_stroke,dataFrame = sensitivity2_set1,test = FALSE)
tableStack(vars = c(44:45,126),by=icd_AF,dataFrame = sensitivity2_set1,test = FALSE)
tableStack(vars = c(44:45,126),by=icd_HF,dataFrame = sensitivity2_set1,test = FALSE)
tableStack(vars = c(44:45,126),by=icd_cvd,dataFrame = sensitivity2_set1,test = FALSE)




library(survival)
# sensitivity1
#table2-birthweight
BirthWeight_sensitivity1<-vector('list',length(1:9))
for (i in 1:9) {
  n=i+44+i*6
  x=n-3
  BirthWeight_sensitivity1[[i]]<-summary(coxph(Surv(sensitivity1_set[,n],sensitivity1_set[,x])~BirthWeight_3group+sex+race+cvd_FH+maternal_smoking+breastfeeding+maternal_hypertension+maternal_diabetes+parental_diabetes,data = sensitivity1_set))
  names(BirthWeight_sensitivity1)[i]<-paste0(names(sensitivity1_set[x]),"~",names(sensitivity1_set[n]),"sensitivity1")
  print(c(n,x))
}
BirthWeight_sensitivity1[[10]]<-summary(coxph(Surv(timecvd_bw,icd_cvd)~BirthWeight_3group+sex+race+cvd_FH+maternal_smoking+breastfeeding+maternal_hypertension+maternal_diabetes+parental_diabetes,data = sensitivity1_set))
names(BirthWeight_sensitivity1)[10]<-"icd_cvd~timecvd_bw sensitivity1"
#table2-bodysize10years
bodysize10y_sensitivity1<-vector('list',length(1:9))
for (i in 1:9) {
  n=i+45+i*6
  x=n-4
  bodysize10y_sensitivity1[[i]]<-summary(coxph(Surv(sensitivity1_set[,n],sensitivity1_set[,x])~Comparative_body_size_at_age_10+BirthWeight_3group+sex+race+cvd_FH+maternal_smoking+breastfeeding+maternal_hypertension+maternal_diabetes+parental_diabetes,data = sensitivity1_set))
  names(bodysize10y_sensitivity1)[i]<-paste0(names(sensitivity1_set[x]),"~",names(sensitivity1_set[n])," sensitivity1")
  print(c(n,x))
}
bodysize10y_sensitivity1[[10]]<-summary(coxph(Surv(timecvd_10y,icd_cvd)~Comparative_body_size_at_age_10+BirthWeight_3group+sex+race+cvd_FH+maternal_smoking+breastfeeding+maternal_hypertension+maternal_diabetes+parental_diabetes,data = sensitivity1_set))
names(bodysize10y_sensitivity1)[10]<-"icd_cvd~timecvd_10y sensitivity1"
#table2-BMI
BMI_sensitivity1<-vector('list',length(1:9))
for (i in 1:9) {
  n=i+46+i*6
  x=n-5
  BMI_sensitivity1[[i]]<-summary(coxph(Surv(sensitivity1_set[,n],sensitivity1_set[,x])~BMI_3group+Comparative_body_size_at_age_10+BirthWeight_3group+age_55+sex+race+cvd_FH+maternal_smoking+breastfeeding+maternal_hypertension+maternal_diabetes+parental_diabetes+TDI+education_levels+physical_activity+smoking_status+alcohol_status,data = sensitivity1_set))
  names(BMI_sensitivity1)[i]<-paste0(names(sensitivity1_set[x]),"~",names(sensitivity1_set[n])," sensitivity1")
  print(c(n,x))
}
BMI_sensitivity1[[10]]<-summary(coxph(Surv(timecvd_BMI,icd_cvd)~BMI_3group+Comparative_body_size_at_age_10+BirthWeight_3group+age_55+sex+race+cvd_FH+maternal_smoking+breastfeeding+maternal_hypertension+maternal_diabetes+parental_diabetes+TDI+education_levels+physical_activity+smoking_status+alcohol_status,data = sensitivity1_set))
names(BMI_sensitivity1)[10]<-"icd_cvd~timecvd_BMI sensitivity1"


#table3
bodychange1_sensitivity1<-vector('list',length(1:9))
for (i in 1:9) {
  n=i+45+i*6
  x=n-4
  bodychange1_sensitivity1[[i]]<-summary(coxph(Surv(sensitivity1_set[,n],sensitivity1_set[,x])~bodychange1+sex+race+cvd_FH+maternal_smoking+breastfeeding+maternal_hypertension+maternal_diabetes+parental_diabetes,data = sensitivity1_set))
  names(bodychange1_sensitivity1)[i]<-paste0(names(sensitivity1_set[x]),"~",names(sensitivity1_set[n])," joint1_sensitivity1")
  print(c(n,x))
}
bodychange1_sensitivity1[[10]]<-summary(coxph(Surv(timecvd_10y,icd_cvd)~bodychange1+sex+race+cvd_FH+maternal_smoking+breastfeeding+maternal_hypertension+maternal_diabetes+parental_diabetes,data = sensitivity1_set))
names(bodychange1_sensitivity1)[10]<-"icd_cvd~timecvd_10y joint1_sensitivity1"

#table4
bodychange3_sensitivity1<-vector('list',length(1:9))
for (i in 1:9) {
  n=i+46+i*6
  x=n-5
  bodychange3_sensitivity1[[i]]<-summary(coxph(Surv(sensitivity1_set[,n],sensitivity1_set[,x])~bodychange3+BirthWeight_3group+age_55+sex+race+cvd_FH+maternal_smoking+breastfeeding+maternal_hypertension+maternal_diabetes+parental_diabetes,data = sensitivity1_set))
  names(bodychange3_sensitivity1)[i]<-paste0(names(sensitivity1_set[x]),"~",names(sensitivity1_set[n])," joint3_sensitivity1")
  print(c(n,x))
}
bodychange3_sensitivity1[[10]]<-summary(coxph(Surv(timecvd_BMI,icd_cvd)~bodychange3+BirthWeight_3group+age_55+sex+race+cvd_FH+maternal_smoking+breastfeeding+maternal_hypertension+maternal_diabetes+parental_diabetes,data = sensitivity1_set))
names(bodychange3_sensitivity1)[10]<-"icd_cvd~timecvd_BMI joint3_sensitivity1"


#table5
bodychange2_sensitivity1<-vector('list',length(1:9))
for (i in 1:9) {
  n=i+46+i*6
  x=n-5
  bodychange2_sensitivity1[[i]]<-summary(coxph(Surv(sensitivity1_set[,n],sensitivity1_set[,x])~bodychange2+age_55+sex+race+cvd_FH+maternal_smoking+breastfeeding+maternal_hypertension+maternal_diabetes+parental_diabetes,data = sensitivity1_set))
  names(bodychange2_sensitivity1)[i]<-paste0(names(sensitivity1_set[x]),"~",names(sensitivity1_set[n])," joint2_sensitivity1")
  print(c(n,x))
}
bodychange2_sensitivity1[[10]]<-summary(coxph(Surv(timecvd_BMI,icd_cvd)~bodychange2+age_55+sex+race+cvd_FH+maternal_smoking+breastfeeding+maternal_hypertension+maternal_diabetes+parental_diabetes,data = sensitivity1_set))
names(bodychange2_sensitivity1)[10]<-"icd_cvd~timecvd_BMI joint2_sensitivity1"





# sensitivity2
library(epiDisplay)
#table2-birth weight
BirthWeight_sensitivity2<-vector('list',length(1:9))
for (i in 1:9) {
  n=i+44+i*6
  x=n-3
  BirthWeight_sensitivity2[[i]]<-summary(coxph(Surv(sensitivity2_set1[,n],sensitivity2_set1[,x])~BirthWeight_3group+sex+race+maternal_smoking+breastfeeding+maternal_hypertension+maternal_diabetes+parental_diabetes,data = sensitivity2_set1))
  names(BirthWeight_sensitivity2)[i]<-paste0(names(sensitivity2_set1[x]),"~",names(sensitivity2_set1[n]),"sensitivity2")
  print(c(n,x))
}
BirthWeight_sensitivity2[[10]]<-summary(coxph(Surv(timecvd_bw,icd_cvd)~BirthWeight_3group+sex+race+maternal_smoking+breastfeeding+maternal_hypertension+maternal_diabetes+parental_diabetes,data = sensitivity2_set1))
names(BirthWeight_sensitivity2)[10]<-"icd_cvd~timecvd_bw sensitivity2"
#table2-bodysize10years
bodysize10y_sensitivity2<-vector('list',length(1:9))
for (i in 1:9) {
  n=i+45+i*6
  x=n-4
  bodysize10y_sensitivity2[[i]]<-summary(coxph(Surv(sensitivity2_set1[,n],sensitivity2_set1[,x])~Comparative_body_size_at_age_10+BirthWeight_3group+sex+race+maternal_smoking+breastfeeding+maternal_hypertension+maternal_diabetes+parental_diabetes,data = sensitivity2_set1))
  names(bodysize10y_sensitivity2)[i]<-paste0(names(sensitivity2_set1[x]),"~",names(sensitivity2_set1[n])," sensitivity2")
  print(c(n,x))
}
bodysize10y_sensitivity2[[10]]<-summary(coxph(Surv(timecvd_10y,icd_cvd)~Comparative_body_size_at_age_10+BirthWeight_3group+sex+race+maternal_smoking+breastfeeding+maternal_hypertension+maternal_diabetes+parental_diabetes,data = sensitivity2_set1))
names(bodysize10y_sensitivity2)[10]<-"icd_cvd~timecvd_10y sensitivity2"
#table2-BMI
BMI_sensitivity2<-vector('list',length(1:9))
for (i in 1:9) {
  n=i+46+i*6
  x=n-5
  BMI_sensitivity2[[i]]<-summary(coxph(Surv(sensitivity2_set1[,n],sensitivity2_set1[,x])~BMI_3group+Comparative_body_size_at_age_10+BirthWeight_3group+age_55+sex+race+maternal_smoking+breastfeeding+maternal_hypertension+maternal_diabetes+parental_diabetes+TDI+education_levels+physical_activity+smoking_status+alcohol_status,data = sensitivity2_set1))
  names(BMI_sensitivity2)[i]<-paste0(names(sensitivity2_set1[x]),"~",names(sensitivity2_set1[n])," sensitivity2")
  print(c(n,x))
}
BMI_sensitivity2[[10]]<-summary(coxph(Surv(timecvd_BMI,icd_cvd)~BMI_3group+Comparative_body_size_at_age_10+BirthWeight_3group+age_55+sex+race+maternal_smoking+breastfeeding+maternal_hypertension+maternal_diabetes+parental_diabetes+TDI+education_levels+physical_activity+smoking_status+alcohol_status,data = sensitivity2_set1))
names(BMI_sensitivity2)[10]<-"icd_cvd~timecvd_BMI sensitivity2"

#table3
bodychange1_sensitivity2<-vector('list',length(1:9))
for (i in 1:9) {
  n=i+45+i*6
  x=n-4
  bodychange1_sensitivity2[[i]]<-summary(coxph(Surv(sensitivity2_set1[,n],sensitivity2_set1[,x])~bodychange1+sex+race+maternal_smoking+breastfeeding+maternal_hypertension+maternal_diabetes+parental_diabetes,data = sensitivity2_set1))
  names(bodychange1_sensitivity2)[i]<-paste0(names(sensitivity2_set1[x]),"~",names(sensitivity2_set1[n])," joint1_sensitivity2")
  print(c(n,x))
}
bodychange1_sensitivity2[[10]]<-summary(coxph(Surv(timecvd_10y,icd_cvd)~bodychange1+sex+race+maternal_smoking+breastfeeding+maternal_hypertension+maternal_diabetes+parental_diabetes,data = sensitivity2_set1))
names(bodychange1_sensitivity2)[10]<-"icd_cvd~timecvd_10y joint1_sensitivity2"
#table4
bodychange3_sensitivity2<-vector('list',length(1:9))
for (i in 1:9) {
  n=i+46+i*6
  x=n-5
  bodychange3_sensitivity2[[i]]<-summary(coxph(Surv(sensitivity2_set1[,n],sensitivity2_set1[,x])~bodychange3+BirthWeight_3group+age_55+sex+race+maternal_smoking+breastfeeding+maternal_hypertension+maternal_diabetes+parental_diabetes,data = sensitivity2_set1))
  names(bodychange3_sensitivity2)[i]<-paste0(names(sensitivity2_set1[x]),"~",names(sensitivity2_set1[n])," joint3_sensitivity2")
  print(c(n,x))
}
bodychange3_sensitivity2[[10]]<-summary(coxph(Surv(timecvd_BMI,icd_cvd)~bodychange3+BirthWeight_3group+age_55+sex+race+maternal_smoking+breastfeeding+maternal_hypertension+maternal_diabetes+parental_diabetes,data = sensitivity2_set1))
names(bodychange3_sensitivity2)[10]<-"icd_cvd~timecvd_BMI joint3_sensitivity2"


#table5
bodychange2_sensitivity2<-vector('list',length(1:9))
for (i in 1:9) {
  n=i+46+i*6
  x=n-5
  bodychange2_sensitivity2[[i]]<-summary(coxph(Surv(sensitivity2_set1[,n],sensitivity2_set1[,x])~bodychange2+age_55+sex+race+maternal_smoking+breastfeeding+maternal_hypertension+maternal_diabetes+parental_diabetes,data = sensitivity2_set1))
  names(bodychange2_sensitivity2)[i]<-paste0(names(sensitivity2_set1[x]),"~",names(sensitivity2_set1[n])," joint2_sensitivity2")
  print(c(n,x))
}
bodychange2_sensitivity2[[10]]<-summary(coxph(Surv(timecvd_BMI,icd_cvd)~bodychange2+age_55+sex+race+maternal_smoking+breastfeeding+maternal_hypertension+maternal_diabetes+parental_diabetes,data = sensitivity2_set1))
names(bodychange2_sensitivity2)[10]<-"icd_cvd~timecvd_BMI joint2_sensitivity2"
