Sys.setlocale("LC_TIME", "English")
options(scipen=999)

# table of patient characteristics --------------
get.summary.characteristics<-function(working.data, working.name){

summary.characteristics1<-data.frame(Overall=t(working.data %>% 
                           # mutate(index_year=year(cohort_start_date)) %>% 
                           summarise(n=nice.num.count(length(person_id)),
                                     # min.index.date=min(cohort_start_date),
                                     # max.index.date=max(cohort_start_date),
                                     age=paste0(nice.num.count(median(age)),  " [",
                                                nice.num.count(quantile(age,probs=0.25)),  " to ",
                                                nice.num.count(quantile(age,probs=0.75)),   "]" ), 
                                     age.under.20=paste0(nice.num.count(sum(age_gr3=="<20")),
                                                      " (",  nice.num((sum(age_gr3=="<20")/length(person_id))*100),  "%)"),
                                     age.20_29=paste0(nice.num.count(sum(age_gr3=="20-29")),
                                                      " (",  nice.num((sum(age_gr3=="20-29")/length(person_id))*100),  "%)"),
                                     age.30_39=paste0(nice.num.count(sum(age_gr3=="30-39")),
                                                      " (",  nice.num((sum(age_gr3=="30-39")/length(person_id))*100),  "%)"), 
                                     age.40_49=paste0(nice.num.count(sum(age_gr3=="40-49")),
                                                      " (",  nice.num((sum(age_gr3=="40-49")/length(person_id))*100),  "%)"), 
                                     age.50_59=paste0(nice.num.count(sum(age_gr3=="50-59")),
                                                      " (",  nice.num((sum(age_gr3=="50-59")/length(person_id))*100),  "%)"), 
                                     age.60_69=paste0(nice.num.count(sum(age_gr3=="60-69")),
                                                      " (",  nice.num((sum(age_gr3=="60-69")/length(person_id))*100),  "%)"), 
                                     age.70_79=paste0(nice.num.count(sum(age_gr3=="70-79")),
                                                      " (",  nice.num((sum(age_gr3=="70-79")/length(person_id))*100),  "%)"), 
                                     age.80u=paste0(nice.num.count(sum(age_gr3==">=80")),
                                                    " (",  nice.num((sum(age_gr3==">=80")/length(person_id))*100),  "%)"), 
                                     sex.male=paste0(nice.num.count(sum(gender=="Male")),
                                                     " (",  nice.num((sum(gender=="Male")/length(person_id))*100),
                                                     "%)"),
                                     prior_obs_years=paste0(nice.num(median(prior_obs_years)),
                                                            " [", nice.num(quantile(prior_obs_years,probs=0.25)), 
                                                            " to ", nice.num(quantile(prior_obs_years,probs=0.75)),   "]" ),
                                     medea.1=paste0(nice.num.count(sum(medea=="Q1", na.rm = TRUE)),
                                                     " (",  nice.num((sum(medea=="Q1", na.rm = TRUE)/length(person_id))*100),
                                                     "%)"),
                                     medea.2=paste0(nice.num.count(sum(medea=="Q2", na.rm = TRUE)),
                                                     " (",  nice.num((sum(medea=="Q2", na.rm = TRUE)/length(person_id))*100),
                                                     "%)"),
                                     medea.3=paste0(nice.num.count(sum(medea=="Q3", na.rm = TRUE)),
                                                     " (",  nice.num((sum(medea=="Q3", na.rm = TRUE)/length(person_id))*100),
                                                     "%)"),
                                     medea.4=paste0(nice.num.count(sum(medea=="Q4", na.rm = TRUE)),
                                                     " (",  nice.num((sum(medea=="Q4", na.rm = TRUE)/length(person_id))*100),
                                                     "%)"),
                                     medea.5=paste0(nice.num.count(sum(medea=="Q5", na.rm = TRUE)),
                                                     " (",  nice.num((sum(medea=="Q5", na.rm = TRUE)/length(person_id))*100),
                                                     "%)"),
                                     medea.missing=paste0(nice.num.count(sum(is.na(medea))),
                                                     " (",  nice.num((sum(is.na(medea))/length(person_id))*100),
                                                     "%)"),
                                     smoking.yes=paste0(nice.num.count(sum(smoke=="Current some day smoker", na.rm = TRUE)),
                                                     " (",  nice.num((sum(smoke=="Current some day smoker", na.rm = TRUE)/length(person_id))*100),
                                                     "%)"),
                                     smoking.ex=paste0(nice.num.count(sum(smoke=="Former smoker", na.rm = TRUE)),
                                                     " (",  nice.num((sum(smoke=="Former smoker", na.rm = TRUE)/length(person_id))*100),
                                                     "%)"),
                                     smoking.no=paste0(nice.num.count(sum(smoke=="Never smoker", na.rm = TRUE)),
                                                     " (",  nice.num((sum(smoke=="Never smoker", na.rm = TRUE)/length(person_id))*100),
                                                     "%)"),
                                     smoking.missing=paste0(nice.num.count(sum(is.na(smoke))),
                                                     " (",  nice.num((sum(is.na(smoke))/length(person_id))*100),
                                                     "%)") ,
                                     smoke.time=paste0(nice.num(median(abs(smoke.time.to.start)/365.25, na.rm = TRUE)),  " [",
                                                nice.num(quantile(abs(smoke.time.to.start)/365.25,probs=0.25, na.rm = TRUE)),  " to ",
                                                nice.num(quantile(abs(smoke.time.to.start)/365.25,probs=0.75, na.rm = TRUE)),   "]" ),
                                     bmi=paste0(nice.num.count(median(bmi, na.rm = TRUE)),  " [",
                                                nice.num.count(quantile(bmi,probs=0.25, na.rm = TRUE)),  " to ",
                                                nice.num.count(quantile(bmi,probs=0.75, na.rm = TRUE)),   "]" ),
                                     bmi.time=paste0(nice.num(median(abs(bmi.time.to.start)/365.25, na.rm = TRUE)),  " [",
                                                nice.num(quantile(abs(bmi.time.to.start)/365.25,probs=0.25, na.rm = TRUE)),  " to ",
                                                nice.num(quantile(abs(bmi.time.to.start)/365.25,probs=0.75, na.rm = TRUE)),   "]" ),
                                      
                                     )
    ))

    # and all the conds and medications
summary.characteristics2<-data.frame(Overall=t(working.data %>% 
                           summarise_at(.vars = all_of(c(paste0(cond.names, ".all.history"))), 
                                        .funs = function(x, tot){
                                          paste0(nice.num.count(sum(x, na.rm = TRUE)),
                                                 " (", nice.num((sum(x, na.rm = TRUE)/tot)*100), "%)")
                                        } , tot=nrow(working.data))) )



summary.characteristics<-bind_rows(summary.characteristics1,
                                   summary.characteristics2)
    
  summary.characteristics$Overall<-as.character(summary.characteristics$Overall)
  
  rownames(summary.characteristics)<-str_to_sentence(rownames(summary.characteristics))
  rownames(summary.characteristics)<-str_replace(rownames(summary.characteristics),
                                                 "Sex.male", "Sex: Male")
  rownames(summary.characteristics)<-str_replace(rownames(summary.characteristics),
                                                 "Prior_obs_years", "Years of prior observation time")
  rownames(summary.characteristics)<-str_replace_all(rownames(summary.characteristics) , "_", " ")
  
  #obscure any counts less than 5
  summary.characteristics$Overall<-
    ifelse(str_sub(summary.characteristics$Overall, 1, 2) %in%  c("1 ","2 ", "3 ","4 "),
           "<5",summary.characteristics$Overall)
  summary.characteristics$var<-row.names(summary.characteristics)
  row.names(summary.characteristics)<-1:nrow(summary.characteristics)
  
  summary.characteristics <-summary.characteristics %>% 
    mutate(var=ifelse(var=="Cond.comp",
                      "One or more condition of interest", var )) %>% 
    mutate(var=ifelse(var=="Drug.comp",
                      "One or more medication of interest", var )) %>% 
    mutate(var=ifelse(var=="Cond.drug.comp",
                      "One or more condition/ medication of interest", var ))
  
  
  
  summary.characteristics %>% 
    select(var, Overall) %>% 
    rename(!!working.name:=Overall)
  
}

table1<-get.summary.characteristics(person, "Overall") %>% 
   left_join(get.summary.characteristics(person %>% 
                                           filter(!is.na(person$COVID19_diagnosis_test_date)),
                                         "Outpatient COVID-19 diagnosis or positive test")) %>% 
   left_join(get.summary.characteristics(person %>% 
                                           filter(!is.na(person$COVID19_hospital_date )),
                                         "Hospitalised with COVID-19")) %>% 
   left_join(get.summary.characteristics(person %>% 
                                           filter(!is.na(person$COVID19_death_date)),
                                         "Died with COVID-19"))

table1<-table1 %>%
     mutate(var=ifelse(var=="Age.under.20", "Age: Under 20", var))  %>%
     mutate(var=ifelse(var=="Age.20 29", "Age: 20 to 29", var))  %>%
     mutate(var=ifelse(var=="Age.30 39", "Age: 30 to 39", var))  %>%
     mutate(var=ifelse(var=="Age.40 49", "Age: 40 to 49", var))  %>%
     mutate(var=ifelse(var=="Age.50 59", "Age: 50 to 59", var))  %>%
     mutate(var=ifelse(var=="Age.60 69", "Age: 60 to 69", var))  %>%
     mutate(var=ifelse(var=="Age.70 79", "Age: 70 to 79", var))  %>%
     mutate(var=ifelse(var=="Age.80u", "Age: 80 or older", var)) 
table1<-table1 %>% 
  mutate(var=str_replace_all(var,".all.history", "")) %>% 
  mutate(var=str_replace_all(var,"Copd", "COPD")) 

write.csv(table1,here("StudyOutput", "Table1.csv"), row.names = FALSE)

# outpatient cohort entry ----------------------

person %>% 
  select(person_id, age_gr2, gender,
         COVID19_diagnosis_test_date,
         COVID19_diagnosis_narrow_date,
         COVID19_diagnosis_broad_date,
         COVID19_PCR_positive_test_date,
         COVID19_positive_test_date) %>%
  rename("Outpatient COVID-19"="COVID19_diagnosis_test_date") %>% 
  rename("PCR or antigen positive test"="COVID19_positive_test_date") %>% 
  rename("PCR positive test"="COVID19_PCR_positive_test_date") %>% 
  rename("COVID-19 diagnosis (narrow definition)"="COVID19_diagnosis_narrow_date") %>% 
  rename("COVID-19 diagnosis (broad definition)"="COVID19_diagnosis_broad_date") %>% 
  pivot_longer(!c(person_id, age_gr2, gender),names_to = "group", values_to = "date",
   values_drop_na = TRUE)%>% 
  mutate(group=factor(group,
                      levels=c(
                        "PCR positive test","PCR or antigen positive test",
                        "COVID-19 diagnosis (narrow definition)",
                        "COVID-19 diagnosis (broad definition)" ,
                        "Outpatient COVID-19"))) %>% 
  ggplot(aes(fill=group))+
  facet_wrap(group ~ ., ncol=1)+
  geom_histogram(aes(date), colour="black", binwidth = 5)+
  theme_bw()+
  theme(legend.title = element_blank(),
        axis.text=element_text(size=16),
        axis.title=element_text(size=16,face="bold"),
        strip.text = element_text(size=16, face="bold"),
        strip.text.y.left = element_text(angle = 0),
        strip.background = element_rect( fill="#f7f7f7"), 
        legend.text=element_text(size=16),
        legend.position = "top")+
  ylab("N")+
  xlab("Date of cohort entry")+
  scale_fill_manual(values=c("#e34a33", "#b30000", "#2b8cbe", "#045a8d", "#984ea3"))+
  scale_y_continuous(labels = scales::comma)+
  guides(fill=guide_legend(nrow=3,byrow=TRUE)) +
  scale_x_date(date_breaks = "5 month", date_labels =  "%b %Y") 

ggsave(here("StudyOutput",
  "plot.entry.outpatient.cohorts.png"),
       width=10.5, height=12)

# cohort entry over time ----------------------
person %>% 
  select(person_id, age_gr2, gender,
         COVID19_diagnosis_test_date,
         COVID19_hospital_date,
         COVID19_death_date)%>%
  rename("Outpatient COVID-19"="COVID19_diagnosis_test_date") %>% 
  rename("COVID-19 hospitalisation"="COVID19_hospital_date") %>% 
  rename("COVID-19 death"="COVID19_death_date") %>% 
  pivot_longer(!c(person_id, age_gr2, gender),names_to = "group", values_to = "date",
   values_drop_na = TRUE)%>% 
  mutate(group=factor(group,
                      levels=c(
                        "Outpatient COVID-19",
                        "COVID-19 hospitalisation",
                        "COVID-19 death" ))) %>% 
    ggplot(aes(fill=group))+
  facet_grid(group ~ age_gr2, scales = "free_y")+
  geom_histogram(aes(date), colour="black",binwidth = 5)+
  theme_bw()+
  theme(legend.title = element_blank(),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        strip.text = element_text(size=16, face="bold"),
        strip.text.y.left = element_text(angle = 0),
        strip.background = element_rect( fill="#f7f7f7"), 
        legend.text=element_text(size=16),
        legend.position = "top")+
  ylab("N")+
  xlab("Date of cohort entry") +
  scale_y_continuous(labels = scales::comma)+
  scale_fill_manual(values=c( "#984ea3","#ff7f00", "#4daf4a")) +
  scale_x_date(date_breaks = "5 month", date_labels =  "%b %Y") 

ggsave(here("StudyOutput",
  "plot.entry.study.cohorts.png"),
       width=12.5, height=11)

# age by cohort and sex ----

person %>% 
  select(person_id, age, gender,
         COVID19_diagnosis_test_date,
         COVID19_hospital_date,
         COVID19_death_date)%>%
  rename("Outpatient COVID-19"="COVID19_diagnosis_test_date") %>% 
  rename("COVID-19 hospitalisation"="COVID19_hospital_date") %>% 
  rename("COVID-19 death"="COVID19_death_date") %>% 
  mutate("General population"=as.Date(study.start.date)) %>% 
  pivot_longer(!c(person_id, age, gender),names_to = "group", values_to = "date",
   values_drop_na = TRUE) %>% 
  mutate(group=factor(group,
                      levels=c("General population",
                        "Outpatient COVID-19",
                        "COVID-19 hospitalisation",
                        "COVID-19 death" ))) %>%  
  ggplot()+
  facet_grid(group ~ gender ,
             scales = "free_y", switch = "y")+
  geom_histogram(aes(age), binwidth = 2, fill="grey", colour="black")+
  theme_bw()+
  xlab("Age (years)")+
  scale_y_continuous(position = "right", labels = scales::comma)+   
  scale_x_continuous(limits = c(0,110))+
  theme(panel.spacing = unit(1, "lines"),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        strip.text = element_text(size=14, face="bold"),
        strip.background = element_rect( fill="#f7f7f7", colour=NA,size = 1.4),
        panel.border = element_rect(color = NA, fill = NA, size = 1.2),
        strip.text.y.left = element_text(angle = 0))

ggsave(here("StudyOutput",
  "age.histogram.png"),
        dpi=600,
        width = 14, height = 9)

# symptoms -----

plot.data<-bind_rows(
person %>% 
  group_by(month = lubridate::floor_date(COVID19_diagnosis_test_date, "month")) %>%
  summarise(prop=sum(!is.na(cough))/sum(!is.na(COVID19_diagnosis_test_date))) %>% 
  mutate(group="Cough") %>% 
  filter(!is.na(month)),
person %>% 
  group_by(month = lubridate::floor_date(COVID19_diagnosis_test_date, "month")) %>%
  summarise(prop=sum(!is.na(dyspnea))/sum(!is.na(COVID19_diagnosis_test_date))) %>% 
  mutate(group="Dyspnea") %>% 
  filter(!is.na(month)),
person %>% 
  group_by(month = lubridate::floor_date(COVID19_diagnosis_test_date, "month")) %>%
  summarise(prop=sum(!is.na(diarrhea))/sum(!is.na(COVID19_diagnosis_test_date))) %>% 
  mutate(group="Diarrhea") %>% 
  filter(!is.na(month)),
person %>% 
  group_by(month = lubridate::floor_date(COVID19_diagnosis_test_date, "month")) %>%
  summarise(prop=sum(!is.na(fever))/sum(!is.na(COVID19_diagnosis_test_date))) %>% 
  mutate(group="Fever") %>% 
  filter(!is.na(month)),
person %>% 
  group_by(month = lubridate::floor_date(COVID19_diagnosis_test_date, "month")) %>%
  summarise(prop=sum(!is.na(myalgia))/sum(!is.na(COVID19_diagnosis_test_date))) %>% 
  mutate(group="Myalgia") %>% 
  filter(!is.na(month)),
person %>% 
  group_by(month = lubridate::floor_date(COVID19_diagnosis_test_date, "month")) %>%
  summarise(prop=sum(!is.na(anosmia_hyposmia_dysgeusia))/sum(!is.na(COVID19_diagnosis_test_date))) %>% 
  mutate(group="Anosmia, hyposmia, or dysgeusia") %>% 
  filter(!is.na(month)),
person %>% 
  group_by(month = lubridate::floor_date(COVID19_diagnosis_test_date, "month")) %>%
  summarise(prop=sum(!is.na(malaise_fatigue))/sum(!is.na(COVID19_diagnosis_test_date))) %>% 
  mutate(group="Malaise or fatigue") %>% 
  filter(!is.na(month)),
person %>% 
  group_by(month = lubridate::floor_date(COVID19_diagnosis_test_date, "month")) %>%
  summarise(prop=sum(!is.na(pain))/sum(!is.na(COVID19_diagnosis_test_date))) %>% 
  mutate(group="Pain") %>% 
  filter(!is.na(month)),
person %>% 
  group_by(month = lubridate::floor_date(COVID19_diagnosis_test_date, "month")) %>%
  summarise(prop=sum(!is.na(headache))/sum(!is.na(COVID19_diagnosis_test_date))) %>% 
  mutate(group="Headache") %>% 
  filter(!is.na(month))
)

plot.data<-plot.data %>% 
  mutate(group=factor(
    group,
    levels=c("Anosmia, hyposmia, or dysgeusia",
             "Cough",
             "Dyspnea", "Diarrhea",
             "Fever",
             "Headache",
             "Malaise or fatigue",
             "Myalgia","Pain" )))

plot.data %>% 
  ggplot(aes(x=month, y=prop, colour=group))+
  facet_wrap(vars(group))+
  geom_line(size=1.25)+
  geom_point(size=3)+
  theme_bw()+
  theme(legend.position="none",
        axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
        strip.text = element_text(size=14, face="bold"),
        strip.text.y.left = element_text(angle = 0),
        strip.background = element_rect( fill="#f7f7f7"))+
  ylab("Proportion of cohort")+
  xlab("Date of cohort entry") +
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values=c("#377eb8", "#4daf4a", "#984ea3")) +
  scale_x_date(date_breaks = "5 month", date_labels =  "%b %Y") 


ggsave(here("StudyOutput",
  "plot.sytptoms.png"),
       width=12.5, height=11)



