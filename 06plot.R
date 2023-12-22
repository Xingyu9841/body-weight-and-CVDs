## sankey plot

library(dplyr)
library(ggplot2)
library(ggalluvial)
sankeydata<-data.frame(bodysize_set3$BirthWeight_3group,bodysize_set3$Comparative_body_size_at_age_10,bodysize_set3$BMI_3group,factor(bodysize_set3$icd_cvd,labels = c("no","yes")))
names(sankeydata)<-c("Birth weight","Body weight at age 10 years","BMI at midlife","cvd or not")
sankeydata<-group_by(sankeydata,`Birth weight`,`Body weight at age 10 years`,`BMI at midlife`,`cvd or not`)%>%summarise(.,count=n())
sankeydata<-as.data.frame(sankeydata)
sankeydata<-within(sankeydata,{`Body weight at age 10 years`<-factor(`Body weight at age 10 years`,labels = c(" Normal "," Low "," High "))})
p<-
  ggplot(sankeydata,aes(y=count,axis1=`Birth weight`,axis2=`Body weight at age 10 years`,axis3= `BMI at midlife`))+
  geom_alluvium(aes(fill=`Birth weight`,alpha=1),curve_type = "sigmoid")+
  geom_stratum(width = 1/4,reverse=TRUE,discern=TRUE)+
  scale_x_continuous(breaks=1:3,labels = c("Birth weight","Body weight at age 10 years","BMI at midlife"),expand = c(0,0))+
  scale_y_continuous(breaks = seq(0,200000,by=25000),labels = c("0","25k","50k","75k","100k","125k","150k","175k","200k"),expand = c(0,0))+
  geom_text(stat = "stratum",aes(label=after_stat(stratum)),reverse=TRUE,size=6,angle=0,family="mono",discern=TRUE)+
  labs(x="Weight transitions from birth → age 10 years → midlife",y="Frequency")+
  theme_bw()+
  theme(
    axis.title.x = element_text(family = "serif",face = "bold",size = 21,vjust = -3),
    axis.text = element_text(size = 18,family = "mono"),
    axis.text.x = element_text(vjust = -1),
    axis.title.y = element_text(family = "serif",face = "bold",size = 21,vjust = 3.5),
    legend.text = element_text(family = "mono",size = 13),
    legend.title = element_text(family = "mono",size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    #plot.title = element_text(family = "serif",face = "bold",size = 24),
    plot.margin = margin(t=1,l=1,r=0.5,b=1.5,unit = "cm")
  )+
  scale_fill_manual(values = c("#67C2A3","#FFFFCC","#E31A1C"))+
  guides(alpha="none",color="none")
p
ggsave(p,filename="Sankey Diagram.pdf",path = "C:\\Users\\W\\Desktop",width = 22,height = 12,dpi = 300)




##  forest plots

library(grid)
library(readxl)

# table3~4
table3<-read.csv("C:\\Users\\W\\Desktop\\Table3横向.csv",header = TRUE)
#table3$`          `<-ifelse(is.na(table3$HR),table3$Subgroup,paste0(table3$Subgroup,"          "))
table3$`               `<-ifelse(is.na(table3$HR),table3$Subgroup,paste0("   ",table3$Subgroup,"     ")) #缩进

table3$Total<-ifelse(is.na(table3$Total),"",table3$Total)

table3$CVDs..n.<-ifelse(is.na(table3$CVDs..n.),"",table3$CVDs..n.)
table3$MI..n.<-ifelse(is.na(table3$MI..n.),"",table3$MI..n.)
table3$Stroke..n.<-ifelse(is.na(table3$Stroke..n.),"",table3$Stroke..n.)
table3$AF..n.<-ifelse(is.na(table3$AF..n.),"",table3$AF..n.)
table3$HF..n.<-ifelse(is.na(table3$HF..n.),"",table3$HF..n.)

table3$'CVDs (n)'<-table3$CVDs..n.
table3$''<-paste(rep(" ",20),collapse = "")
table3$'HR (95%CI)'<-sprintf("%.2f (%.2f, %.2f)",table3$HR,table3$X95CI.lower,table3$X95CI.upper)
table3$'HR (95%CI)'<-ifelse(table3$`HR (95%CI)`=="1.00 (1.00, 1.00)","Ref",table3$`HR (95%CI)`)
table3$'HR (95%CI)'<-ifelse(table3$`HR (95%CI)`=="NA (NA, NA)","",table3$`HR (95%CI)`)

table3$'MI (n)'<-table3$MI..n.
table3$' '<-paste(rep(" ",20),collapse = "")
table3$' HR (95%CI)'<-sprintf("%.2f (%.2f, %.2f)",table3$HR.1,table3$X95CI.lower.1 ,table3$X95CI.upper.1)
table3$' HR (95%CI)'<-ifelse(table3$` HR (95%CI)`=="1.00 (1.00, 1.00)","Ref",table3$` HR (95%CI)`)
table3$' HR (95%CI)'<-ifelse(table3$` HR (95%CI)`=="NA (NA, NA)","",table3$` HR (95%CI)`)

table3$'Stroke (n)'<-table3$Stroke..n.
table3$'  '<-paste(rep(" ",20),collapse = "")
table3$' HR (95%CI) '<-sprintf("%.2f (%.2f, %.2f)",table3$HR.2,table3$X95CI.lower.2,table3$X95CI.upper.2)
table3$' HR (95%CI) '<-ifelse(table3$` HR (95%CI) `=="1.00 (1.00, 1.00)","Ref",table3$` HR (95%CI) `)
table3$' HR (95%CI) '<-ifelse(table3$` HR (95%CI) `=="NA (NA, NA)","",table3$` HR (95%CI) `)

table3$'AF (n)'<-table3$AF..n.
table3$'   '<-paste(rep(" ",20),collapse = "")
table3$'  HR (95%CI) '<-sprintf("%.2f (%.2f, %.2f)",table3$HR.3,table3$X95CI.lower.3,table3$X95CI.upper.3)
table3$'  HR (95%CI) '<-ifelse(table3$`  HR (95%CI) `=="1.00 (1.00, 1.00)","Ref",table3$`  HR (95%CI) `)
table3$'  HR (95%CI) '<-ifelse(table3$`  HR (95%CI) `=="NA (NA, NA)","",table3$`  HR (95%CI) `)

table3$'HF (n)'<-table3$HF..n.
table3$'    '<-paste(rep(" ",20),collapse = "")
table3$'  HR (95%CI)  '<-sprintf("%.2f (%.2f, %.2f)",table3$HR.4,table3$X95CI.lower.4,table3$X95CI.upper.4)
table3$'  HR (95%CI)  '<-ifelse(table3$`  HR (95%CI)  `=="1.00 (1.00, 1.00)","Ref",table3$`  HR (95%CI)  `)
table3$'  HR (95%CI)  '<-ifelse(table3$`  HR (95%CI)  `=="NA (NA, NA)","",table3$`  HR (95%CI)  `)

library(forestploter)
tm<-forest_theme(base_size = 10,ci_pch = 19,refline_col = "darkred",footnote_col = "grey20",footnote_fontface = "italic",footnote_cex = 0.6)
table3p<-forest(table3[,c(23,2,24:38)],
                est=list(table3$HR,
                         table3$HR.1,
                         table3$HR.2,
                         table3$HR.3,
                         table3$HR.4),
                lower=list(table3$X95CI.lower,
                           table3$X95CI.lower.1,
                           table3$X95CI.lower.2,
                           table3$X95CI.lower.3,
                           table3$X95CI.lower.4),
                upper=list(table3$X95CI.upper,
                           table3$X95CI.upper.1,
                           table3$X95CI.upper.2,
                           table3$X95CI.upper.3,
                           table3$X95CI.upper.4),
                ci_column=c(4,7,10,13,16),
                ref_line=1,
                xlim=c(0.8,3.7),
                ticks_at=c(1,3),
                #footnote = " \n Abbreviation: MI, myocardial infarction; AF, atrial fibrillation; HF, heart failure; CVDs, cardiovascular diseases; HR, hazard ratio",
                theme = tm)
table3p<-edit_plot(table3p,row=c(1,11),gp=gpar(fontface="bold",fontsize=11))

table3p<-add_border(table3p,row=1,where = "top",gp=gpar(lwd=1.5))
table3p<-add_border(table3p,row=20,where = "bottom",gp=gpar(lwd=1.5))
table3p<-edit_plot(table3p,col = 1:2 ,which = "background",gp=gpar(fill="white"))
table3p<-edit_plot(table3p,col = 3:5 ,which = "background",gp=gpar(fill="#EFEFEF"))
table3p<-edit_plot(table3p,col = 6:8 ,which = "background",gp=gpar(fill="white"))
table3p<-edit_plot(table3p,col = 9:11 ,which = "background",gp=gpar(fill="#EFEFEF"))
table3p<-edit_plot(table3p,col = 12:14 ,which = "background",gp=gpar(fill="white"))
table3p<-edit_plot(table3p,col = 15:17 ,which = "background",gp=gpar(fill="#EFEFEF"))
table3p #19*5.5


# table 5横向
library(grid)
library(readxl)
table5<-read.csv("C:\\Users\\W\\Desktop\\Table5横向.csv",header = TRUE)
table5$`Weight transition patterns from Birth to 10 years to Midlife`<-ifelse(is.na(table5$HR),table5$Subgroup,paste0(table5$Subgroup,"       "))

table5$'CVDs (n)'<-table5$CVDs..n.
table5$''<-paste(rep(" ",20),collapse = "")
table5$'HR (95%CI)'<-sprintf("%.2f (%.2f, %.2f)",table5$HR,table5$X95CI.lower,table5$X95CI.upper)
table5$'HR (95%CI)'<-ifelse(table5$`HR (95%CI)`=="1.00 (1.00, 1.00)","Ref",table5$`HR (95%CI)`)

table5$'MI (n)'<-table5$MI..n.
table5$' '<-paste(rep(" ",20),collapse = "")
table5$' HR (95%CI)'<-sprintf("%.2f (%.2f, %.2f)",table5$HR.1,table5$X95CI.lower.1 ,table5$X95CI.upper.1)
table5$' HR (95%CI)'<-ifelse(table5$` HR (95%CI)`=="1.00 (1.00, 1.00)","Ref",table5$` HR (95%CI)`)

table5$'Stroke (n)'<-table5$Stroke..n.
table5$'  '<-paste(rep(" ",20),collapse = "")
table5$' HR (95%CI) '<-sprintf("%.2f (%.2f, %.2f)",table5$HR.2,table5$X95CI.lower.2,table5$X95CI.upper.2)
table5$' HR (95%CI) '<-ifelse(table5$` HR (95%CI) `=="1.00 (1.00, 1.00)","Ref",table5$` HR (95%CI) `)

table5$'AF (n)'<-table5$AF..n.
table5$'   '<-paste(rep(" ",20),collapse = "")
table5$'  HR (95%CI) '<-sprintf("%.2f (%.2f, %.2f)",table5$HR.3,table5$X95CI.lower.3,table5$X95CI.upper.3)
table5$'  HR (95%CI) '<-ifelse(table5$`  HR (95%CI) `=="1.00 (1.00, 1.00)","Ref",table5$`  HR (95%CI) `)

table5$'HF (n)'<-table5$HF..n.
table5$'    '<-paste(rep(" ",20),collapse = "")
table5$'  HR (95%CI)  '<-sprintf("%.2f (%.2f, %.2f)",table5$HR.4,table5$X95CI.lower.4,table5$X95CI.upper.4)
table5$'  HR (95%CI)  '<-ifelse(table5$`  HR (95%CI)  `=="1.00 (1.00, 1.00)","Ref",table5$`  HR (95%CI)  `)


library(forestploter)
tm<-forest_theme(base_size = 10,ci_pch = 19,refline_col = "darkred",footnote_col = "grey20",footnote_fontface = "italic",footnote_cex = 0.6)
table5p<-forest(table5[,c(23,2,24:38)],
                est=list(table5$HR,
                         table5$HR.1,
                         table5$HR.2,
                         table5$HR.3,
                         table5$HR.4),
                lower=list(table5$X95CI.lower,
                           table5$X95CI.lower.1,
                           table5$X95CI.lower.2,
                           table5$X95CI.lower.3,
                           table5$X95CI.lower.4),
                upper=list(table5$X95CI.upper,
                           table5$X95CI.upper.1,
                           table5$X95CI.upper.2,
                           table5$X95CI.upper.3,
                           table5$X95CI.upper.4),
                ci_column=c(4,7,10,13,16),
                ref_line=1,
                xlim=c(0.3,6),
                ticks_at=c(1,6),
                #footnote = "\n Abbreviation: MI, myocardial infarction; AF, atrial fibrillation; HF, heart failure; CVDs, cardiovascular diseases; HR, hazard ratio",
                theme = tm)
table5p<-add_border(table5p,row=1,where = "top",gp=gpar(lwd=1.5))
table5p<-add_border(table5p,row=27,where = "bottom",gp=gpar(lwd=1.5))
table5p<-edit_plot(table5p,col = 1:2 ,which = "background",gp=gpar(fill="white"))
table5p<-edit_plot(table5p,col = 3:5 ,which = "background",gp=gpar(fill="#EFEFEF"))
table5p<-edit_plot(table5p,col = 6:8 ,which = "background",gp=gpar(fill="white"))
table5p<-edit_plot(table5p,col = 9:11 ,which = "background",gp=gpar(fill="#EFEFEF"))
table5p<-edit_plot(table5p,col = 12:14 ,which = "background",gp=gpar(fill="white"))
table5p<-edit_plot(table5p,col = 15:17 ,which = "background",gp=gpar(fill="#EFEFEF"))
table5p #20*7
