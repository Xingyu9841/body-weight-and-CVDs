library(survival)

#3.model 3 
#birth weight
BirthWeight_model3<-vector('list',length(1:9))
for (i in 1:9) {
  n=i+44+i*6
  x=n-3
  BirthWeight_model3[[i]]<-summary(coxph(Surv(bodysize_set3[,n],bodysize_set3[,x])~BirthWeight_3group+sex+race+cvd_FH+maternal_smoking+breastfeeding+maternal_hypertension+maternal_diabetes+parental_diabetes,data = bodysize_set3))
  names(BirthWeight_model3)[i]<-paste0(names(bodysize_set3[x]),"~",names(bodysize_set3[n])," model3")
  print(c(n,x))
}
BirthWeight_model3[[10]]<-summary(coxph(Surv(timecvd_bw,icd_cvd)~BirthWeight_3group+sex+race+cvd_FH+maternal_smoking+breastfeeding+maternal_hypertension+maternal_diabetes+parental_diabetes,data = bodysize_set3))
names(BirthWeight_model3)[10]<-"icd_cvd~timecvd_bw model3"

#10y
bodysize10y_model3<-vector('list',length(1:9))
for (i in 1:9) {
  n=i+45+i*6
  x=n-4
  bodysize10y_model3[[i]]<-summary(coxph(Surv(bodysize_set3[,n],bodysize_set3[,x])~Comparative_body_size_at_age_10+BirthWeight_3group+sex+race+cvd_FH+maternal_smoking+breastfeeding+maternal_hypertension+maternal_diabetes+parental_diabetes,data = bodysize_set3))
  names(bodysize10y_model3)[i]<-paste0(names(bodysize_set3[x]),"~",names(bodysize_set3[n])," model3")
  print(c(n,x))
}
bodysize10y_model3[[10]]<-summary(coxph(Surv(timecvd_10y,icd_cvd)~Comparative_body_size_at_age_10+BirthWeight_3group+sex+race+cvd_FH+maternal_smoking+breastfeeding+maternal_hypertension+maternal_diabetes+parental_diabetes,data = bodysize_set3))
names(bodysize10y_model3)[10]<-"icd_cvd~timecvd_10y model3"

#BMI
BMI_model3<-vector('list',length(1:9))
for (i in 1:9) {
  n=i+46+i*6
  x=n-5
  BMI_model3[[i]]<-summary(coxph(Surv(bodysize_set3[,n],bodysize_set3[,x])~BMI_3group+Comparative_body_size_at_age_10+BirthWeight_3group+age_55+sex+race+cvd_FH+maternal_smoking+breastfeeding+maternal_hypertension+maternal_diabetes+parental_diabetes+TDI+education_levels+physical_activity+smoking_status+alcohol_status,data = bodysize_set3))
  names(BMI_model3)[i]<-paste0(names(bodysize_set3[x]),"~",names(bodysize_set3[n])," model3")
  print(c(n,x))
}
BMI_model3[[10]]<-summary(coxph(Surv(bodysize_set3$timecvd_BMI,icd_cvd)~BMI_3group+Comparative_body_size_at_age_10+BirthWeight_3group+age_55+sex+race+cvd_FH+maternal_smoking+breastfeeding+maternal_hypertension+maternal_diabetes+parental_diabetes+TDI+education_levels+physical_activity+smoking_status+alcohol_status,data = bodysize_set3))
names(BMI_model3)[10]<-"icd_cvd~timecvd_BMI model3"

#joint analysis
#birth weight to 10y, NBW to About Average as Ref
bodychange1_model3<-vector('list',length(1:9))
for (i in 1:9) {
  n=i+45+i*6
  x=n-4
  bodychange1_model3[[i]]<-summary(coxph(Surv(bodysize_set3[,n],bodysize_set3[,x])~bodychange1+sex+race+cvd_FH+maternal_smoking+breastfeeding+maternal_hypertension+maternal_diabetes+parental_diabetes,data = bodysize_set3))
  names(bodychange1_model3)[i]<-paste0(names(bodysize_set3[x]),"~",names(bodysize_set3[n])," joint1_model3")
  print(c(n,x))
}
bodychange1_model3[[10]]<-summary(coxph(Surv(timecvd_10y,icd_cvd)~bodychange1+sex+race+cvd_FH+maternal_smoking+breastfeeding+maternal_hypertension+maternal_diabetes+parental_diabetes,data = bodysize_set3))
names(bodychange1_model3)[10]<-"icd_cvd~timecvd_10y joint1_model3"
#birth weight to 10y to midlife，"NBW" to "About Average" to "normal or thinner" as Ref
bodychange2_model3<-vector('list',length(1:9))
for (i in 1:9) {
  n=i+46+i*6
  x=n-5
  bodychange2_model3[[i]]<-summary(coxph(Surv(bodysize_set3[,n],bodysize_set3[,x])~bodychange2+age_55+sex+race+cvd_FH+maternal_smoking+breastfeeding+maternal_hypertension+maternal_diabetes+parental_diabetes,data = bodysize_set3))
  names(bodychange2_model3)[i]<-paste0(names(bodysize_set3[x]),"~",names(bodysize_set3[n])," joint2_model3")
  print(c(n,x))
}
bodychange2_model3[[10]]<-summary(coxph(Surv(timecvd_BMI,icd_cvd)~bodychange2+age_55+sex+race+cvd_FH+maternal_smoking+breastfeeding+maternal_hypertension+maternal_diabetes+parental_diabetes,data = bodysize_set3))
names(bodychange2_model3)[10]<-"icd_cvd~timecvd_BMI joint2_model3"
#10y to midlife，"About Average" to "normal or thinner"  as Ref
bodychange3_model3<-vector('list',length(1:9))
for (i in 1:9) {
  n=i+46+i*6
  x=n-5
  bodychange3_model3[[i]]<-summary(coxph(Surv(bodysize_set3[,n],bodysize_set3[,x])~bodychange3+BirthWeight_3group+age_55+sex+race+cvd_FH+maternal_smoking+breastfeeding+maternal_hypertension+maternal_diabetes+parental_diabetes,data = bodysize_set3))
  names(bodychange3_model3)[i]<-paste0(names(bodysize_set3[x]),"~",names(bodysize_set3[n])," joint3_model3")
  print(c(n,x))
}
bodychange3_model3[[10]]<-summary(coxph(Surv(timecvd_BMI,icd_cvd)~bodychange3+BirthWeight_3group+age_55+sex+race+cvd_FH+maternal_smoking+breastfeeding+maternal_hypertension+maternal_diabetes+parental_diabetes,data = bodysize_set3))
names(bodychange3_model3)[10]<-"icd_cvd~timecvd_BMI joint3_model3"