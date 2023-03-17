
## Study dates ----
study.start.date<-as.Date("01/03/2020", "%d/%m/%Y")
study.end.date<-as.Date("30/06/2022", "%d/%m/%Y")

## Study population -----
# All persons in the db as of start date -----
person<-person_db %>% 
   select("person_id" , "gender_concept_id", "year_of_birth", "month_of_birth", "day_of_birth", 
         "location_id") %>% 
  left_join(observation_period_db %>% 
  select("person_id", "observation_period_start_date", "observation_period_end_date"))%>% 
  collect() %>% 
  filter(observation_period_start_date<study.start.date)%>% 
  filter(observation_period_end_date>=study.start.date) 

# start exclusion table -----
exclusion_table <- tibble(N_current=nrow(person), exclusion_reason=NA)


# get age and gender -----
person$dob<- paste(person$year_of_birth, 
                   person$month_of_birth, 
                   person$day_of_birth, sep="-") %>% ymd() %>% as.Date()
# age as of 1st March
person<-person %>% 
  mutate(age=floor(as.numeric(difftime(study.start.date,
                                        dob,
                                        units="days"))/365.25))
quantile(person$age)

# gender
#8507 male
#8532 female
person$gender<-ifelse(person$gender_concept_id==8507, "Male",
                      ifelse(person$gender_concept_id==8532, "Female", NA ))
#table(person$gender, useNA = "always")

# age age groups ----
person<-person %>% 
  mutate(age_gr=ifelse(age<20,  "<20",
           ifelse(age>=20 &  age<=44,  "20-44",
                       ifelse(age>=45 & age<=54,  "45-54",
                              ifelse(age>=55 & age<=64,  "55-64",
                                     ifelse(age>=65 & age<=74, "65-74", 
                                            ifelse(age>=75 & age<=84, "75-84",      
                                                   ifelse(age>=85, ">=85",
                                                          NA)))))))) %>% 
  mutate(age_gr= factor(age_gr, 
                        levels = c("<20","20-44","45-54", "55-64",
                                   "65-74", "75-84",">=85"))) 
table(person$age_gr, useNA = "always")

# wider age groups
person<-person %>% 
  mutate(age_gr2=ifelse(age<=44,  "<=44",
                        ifelse(age>=45 & age<=64,  "45-64",    
                               ifelse(age>=65, ">=65",
                                      NA)))) %>% 
  mutate(age_gr2= factor(age_gr2, 
                         levels = c("<=44", "45-64",">=65")))
table(person$age_gr2, useNA = "always")

# another alternative set of age groups
person<-person %>% 
  mutate(age_gr3= ifelse(age<20,  "<20",
                         ifelse(age>=20 &  age<=29,  "20-29",
                         ifelse(age>=30 &  age<=39,  "30-39",
                         ifelse(age>=40 &  age<=49,  "40-49",
                                ifelse(age>=50 &  age<=59,  "50-59",
                                       ifelse(age>=60 & age<=69,  "60-69",
                                              ifelse(age>=70 & age<=79,  "70-79",      
                                                     ifelse(age>=80, ">=80",
                                                            NA))))))))) %>% 
  mutate(age_gr3= factor(age_gr3, 
                         levels = c("<20", "20-29","30-39","40-49", "50-59",
                                    "60-69", "70-79",">=80")))
table(person$age_gr3, useNA = "always")



# add prior observation time -----
person<-person %>%  
  mutate(prior_obs_days=as.numeric(difftime(study.start.date,
                                            observation_period_start_date,
                                            units="days"))) %>% 
  mutate(prior_obs_years=prior_obs_days/365.25)


## Covid cohorts -----
# collect each cohort and join to study pop -----
for(i in 1:length(cohorts$id)){
working.id<-  cohorts$id[i]
working.name<- str_replace_all(paste0(cohorts$name[i], "_date"), " ", "_")

person<-person %>% 
  left_join(cohorts_db %>% 
   filter(cohort_definition_id==working.id) %>% 
   filter(cohort_start_date>=study.start.date) %>% 
   select(subject_id, cohort_start_date) %>% 
   rename("person_id"="subject_id") %>% 
   rename(!!working.name:="cohort_start_date") %>% 
   collect())
}


# No history of COVID-19  -----
# between 01/01/2020 and 29/02/2020
person<-person %>% 
  anti_join(bind_rows(
  person %>% 
  filter(!is.na(COVID19_diagnosis_broad_date) & 
           COVID19_diagnosis_broad_date>=as.Date("01/01/2020", "%d/%m/%Y") & 
           COVID19_diagnosis_broad_date<=as.Date("29/02/2020", "%d/%m/%Y")),
  person %>% 
  filter(!is.na(COVID19_positive_test_date) & 
           COVID19_positive_test_date>=as.Date("01/01/2020", "%d/%m/%Y") & 
           COVID19_positive_test_date<=as.Date("29/02/2020", "%d/%m/%Y"))) %>% 
  select(person_id) %>% 
  distinct())

exclusion_table<-rbind(exclusion_table,
                      c(nrow(person), 
                        "History of COVID-19"))

## Exclude anyone hospitalised on study.start.date ------

ip.codes<-c(9201, 262)
# add all descendents
ip.codes.w.desc<-concept_ancestor_db %>%
  filter(ancestor_concept_id  %in% ip.codes ) %>% 
  collect() %>% 
  select(descendant_concept_id) %>% 
  distinct() %>% 
  pull()

person<-person %>% 
  anti_join(visit_occurrence_db %>% 
  filter(visit_concept_id %in% ip.codes.w.desc) %>% 
  filter(visit_start_date<=study.start.date) %>% 
  filter(visit_end_date>=study.start.date) %>% 
  select(person_id) %>% 
  distinct() %>% 
  collect()) 


exclusion_table<-rbind(exclusion_table,
                      c(nrow(person), 
                        "Hospitalised on study start date"))


# get covid-19 hospitalisation over follow-up ----
hospitalisations_db<-cohorts_db %>% 
     select(subject_id, cohort_start_date) %>% 
     rename("person_id"="subject_id") %>% 
     distinct() %>% 
     inner_join(visit_occurrence_db %>% 
     filter(visit_concept_id %in% c(ip.codes.w.desc)) %>% 
     filter(visit_start_date >= study.start.date)  %>% 
     select(person_id, visit_start_date, visit_end_date) %>% 
     distinct())
hospitalisations<-hospitalisations_db %>% 
  collect()

# index hospitalisation
# between 21 days before up to three days after
index_hosptialisation<-hospitalisations %>% 
  mutate(dtime=as.numeric(difftime(cohort_start_date, visit_start_date, units="days"))) %>% 
  filter(dtime >= -21)%>% 
  filter(dtime <= 3) %>% 
  select(-dtime) %>% 
  arrange(person_id, visit_start_date) %>% 
  group_by(person_id) %>% 
  mutate(seq=1:length(person_id)) %>% 
  filter(seq==1) %>% 
  select(person_id, visit_start_date) %>% # date of admission is index date
  rename("COVID19_hospital_date"="visit_start_date") 
person<-person %>% 
  left_join(index_hosptialisation)

# drop any diangosis/ test results on or after hospitalisation date -----
person<-person %>% 
  mutate(COVID19_diagnosis_broad_date=if_else(!is.na(COVID19_hospital_date) &
    COVID19_diagnosis_broad_date>=COVID19_hospital_date,
                 as.Date(NA), COVID19_diagnosis_broad_date)) %>% 
  mutate(COVID19_diagnosis_narrow_date=if_else(!is.na(COVID19_hospital_date) &
   COVID19_diagnosis_narrow_date>=COVID19_hospital_date,
                 as.Date(NA), COVID19_diagnosis_narrow_date)) %>% 
  mutate(COVID19_positive_test_date=if_else(!is.na(COVID19_hospital_date) &
   COVID19_positive_test_date>=COVID19_hospital_date,
                 as.Date(NA), COVID19_positive_test_date)) %>% 
  mutate(COVID19_PCR_positive_test_date=if_else(!is.na(COVID19_hospital_date) &
   COVID19_PCR_positive_test_date>=COVID19_hospital_date,
                 as.Date(NA), COVID19_PCR_positive_test_date))


# get covid-19 ICU admission over follow-up ----
icu_db<-cohorts_db %>% 
  select(subject_id, cohort_start_date) %>% 
  rename("person_id"="subject_id") %>% 
  distinct() %>% 
  inner_join(visit_occurrence_db %>% 
               filter(visit_concept_id %in% c(32037)) %>% 
               filter(visit_start_date >= study.start.date)  %>% 
               select(person_id, visit_start_date, visit_end_date) %>% 
               distinct())
icu<-icu_db %>% 
  collect()

# index icu
# between 21 days before up to three days after
index_icu<-icu %>% 
  mutate(dtime=as.numeric(difftime(cohort_start_date, visit_start_date, units="days"))) %>% 
  filter(dtime >= -21)%>% 
  filter(dtime <= 3) %>% 
  select(-dtime) %>% 
  arrange(person_id, visit_start_date) %>% 
  group_by(person_id) %>% 
  mutate(seq=1:length(person_id)) %>% 
  filter(seq==1) %>% 
  select(person_id, visit_start_date) %>% # date of admission is index date
  rename("COVID19_icu_date"="visit_start_date") 
person<-person %>% 
  left_join(index_icu)

# drop any diangosis/ test results on or after icu admission date -----
person<-person %>% 
  mutate(COVID19_diagnosis_broad_date=if_else(!is.na(COVID19_icu_date) &
                                                COVID19_diagnosis_broad_date>=COVID19_icu_date,
                                              as.Date(NA), COVID19_diagnosis_broad_date)) %>% 
  mutate(COVID19_diagnosis_narrow_date=if_else(!is.na(COVID19_icu_date) &
                                                 COVID19_diagnosis_narrow_date>=COVID19_icu_date,
                                               as.Date(NA), COVID19_diagnosis_narrow_date)) %>% 
  mutate(COVID19_positive_test_date=if_else(!is.na(COVID19_icu_date) &
                                              COVID19_positive_test_date>=COVID19_icu_date,
                                            as.Date(NA), COVID19_positive_test_date)) %>% 
  mutate(COVID19_PCR_positive_test_date=if_else(!is.na(COVID19_icu_date) &
                                                  COVID19_PCR_positive_test_date>=COVID19_icu_date,
                                                as.Date(NA), COVID19_PCR_positive_test_date))

# get covid-19 death date  -----
covid.deaths<-cohorts_db %>% 
     select(subject_id, cohort_start_date) %>% 
     rename("person_id"="subject_id") %>% 
     distinct() %>% 
     inner_join(death_db %>% 
  select(person_id, death_date))%>% 
  collect() %>% 
  mutate(dtime=as.numeric(difftime(cohort_start_date, death_date, units="days"))) %>% 
  filter(dtime >= -28)%>% 
  select(-dtime) %>% 
  arrange(person_id, death_date) %>% 
  group_by(person_id) %>% 
  mutate(seq=1:length(person_id)) %>% 
  filter(seq==1) %>% 
  select(person_id, death_date) %>% # date of admission is index date
  rename("COVID19_death_date"="death_date") 
person<-person %>% 
  left_join(covid.deaths)

# drop any diangosis/ test results on or after death date -----
person<-person %>% 
  mutate(COVID19_diagnosis_broad_date=if_else(!is.na(COVID19_death_date) &
    COVID19_diagnosis_broad_date>=COVID19_death_date,
                 as.Date(NA), COVID19_diagnosis_broad_date)) %>% 
  mutate(COVID19_diagnosis_narrow_date=if_else(!is.na(COVID19_death_date) &
   COVID19_diagnosis_narrow_date>=COVID19_death_date,
                 as.Date(NA), COVID19_diagnosis_narrow_date)) %>% 
  mutate(COVID19_positive_test_date=if_else(!is.na(COVID19_death_date) &
   COVID19_positive_test_date>=COVID19_death_date,
                 as.Date(NA), COVID19_positive_test_date)) %>% 
  mutate(COVID19_PCR_positive_test_date=if_else(!is.na(COVID19_death_date) &
   COVID19_PCR_positive_test_date>=COVID19_death_date,
                 as.Date(NA), COVID19_PCR_positive_test_date))

# Add diagnosis or test positive cohort ----
person<-person %>% left_join(
  person %>% 
  select(person_id,
         COVID19_diagnosis_broad_date,
         COVID19_positive_test_date) %>% 
  filter(!is.na(COVID19_diagnosis_broad_date) | 
          !is.na(COVID19_positive_test_date)) %>%
  pivot_longer(!person_id, names_to = "type", values_to = "COVID19_diagnosis_test_date",
               values_drop_na = TRUE) %>% 
  arrange(person_id, COVID19_diagnosis_test_date) %>% 
  group_by(person_id) %>% 
  filter(row_number()==1))
  

# -------
## add condition history ------
# add each condition to person
for(n in 1:length(cond.codes)){# add each to person
  working.code<-cond.codes[n]
  working.name<-cond.names[n]

working.persons.all.history <- cohortTableComorbidities_db %>% 
  filter(cohort_definition_id==working.code) %>%
  rename("person_id"="subject_id") %>% 
  rename("condition_start_date"="cohort_start_date") %>%  
  select(person_id, condition_start_date) %>% 
  collect() 
if(nrow(working.persons.all.history)>0){
working.persons.all.history <-  working.persons.all.history %>% 
  inner_join(person %>% select(person_id),
               by = "person_id") %>% 
  filter(study.start.date>condition_start_date) %>% # before index date
  select(person_id) %>% 
  mutate(working.cond.all.hist=1)
}


working.persons.one.year <- cohortTableComorbidities_db %>% 
  filter(cohort_definition_id==working.code) %>%
  rename("person_id"="subject_id") %>% 
  rename("condition_start_date"="cohort_start_date") %>% 
  select(person_id, condition_start_date) %>% 
  collect()
if(nrow(working.persons.one.year)>0){
working.persons.one.year<-working.persons.one.year %>% 
  inner_join(person %>% select(person_id),
               by = "person_id") %>% 
  filter(condition_start_date<(study.start.date) & 
        condition_start_date>=(study.start.date-years(1))) %>% 
  select(person_id) %>% 
  mutate(working.cond.one.year=1)
}


working.persons.30.days <- cohortTableComorbidities_db %>%
  filter(cohort_definition_id==working.code) %>%
  rename("person_id"="subject_id") %>% 
  rename("condition_start_date"="cohort_start_date") %>% 
  select(person_id, condition_start_date) %>% 
  collect()

if(nrow(working.persons.30.days)>0){
  working.persons.30.days<-working.persons.30.days %>% 
  inner_join(person %>% select(person_id),
               by = "person_id") %>% 
  filter(condition_start_date<(study.start.date) & 
           condition_start_date>=(study.start.date-days(30))) %>% 
  select(person_id) %>% 
  mutate(working.cond.30.days=1)
}

if(nrow(working.persons.all.history)>0){
    person<-person %>%
      left_join(working.persons.all.history,
                by = "person_id") %>% 
      rename(!!paste0(working.name, ".all.history"):="working.cond.all.hist")
  } else {
    person$working.cond.all.hist<-0
    person<-person %>% 
      rename(!!paste0(working.name, ".all.history"):="working.cond.all.hist")
  }

if(nrow(working.persons.one.year)>0){
  person<-person %>%
    left_join(working.persons.one.year,
              by = "person_id") %>% 
    rename(!!paste0(working.name, ".one.year"):="working.cond.one.year")
} else {
  person$working.cond.one.year<-0
  person<-person %>% 
    rename(!!paste0(working.name, ".one.year"):="working.cond.one.year")
}

if(nrow(working.persons.30.days)>0){
  person<-person %>%
    left_join(working.persons.30.days,
              by = "person_id") %>% 
    rename(!!paste0(working.name, ".30.days"):="working.cond.30.days")
} else {
  person$working.cond.30.days<-0
  person<-person %>% 
    rename(!!paste0(working.name, ".30.days"):="working.cond.30.days")
}
  
}
#to zero if absent
person<-person %>%
  mutate(across(all_of(paste0(cond.names, ".all.history")), ~ replace_na(.x, 0))) %>%
  mutate(across(all_of(paste0(cond.names, ".one.year")), ~ replace_na(.x, 0))) %>%
  mutate(across(all_of(paste0(cond.names, ".30.days")), ~ replace_na(.x, 0)))
# get BMI ----
# BMI data
BMI<-measurement_db %>% 
  filter( measurement_concept_id	=="3038553") %>% 
  collect()
# drop any records with a value 
# exclude BMI< 15 and >60 
quantile(BMI$value_as_number) # nobody with one over 60, but some below 15
BMI<-BMI %>% 
  filter(value_as_number>=15)
quantile(BMI$value_as_number)

# get most recent record

# reasonable dates?
#hist(year(BMI$measurement_date))
#min to 2006
BMI$days<- as.numeric(difftime(BMI$measurement_date,
                               as.Date("2006/01/01"),
                               units="days"))
sum(BMI$days<0) 
BMI<-BMI %>% 
  filter(days>=0)
#min(BMI$measurement_date)

# max date- healthy start date
BMI$days<- as.numeric(difftime(BMI$measurement_date,
                               study.start.date,
                               units="days"))
sum(BMI$days>0)
BMI<-BMI %>% 
  filter(days<=0)

# most recent value
BMI<-BMI %>% 
  arrange(person_id,desc(measurement_date)) %>% 
  group_by(person_id) %>% 
  mutate(seq=1:length(person_id)) %>%  
  filter(seq==1) %>% 
  select(-seq) %>% 
  ungroup()


#length(unique(BMI$person_id))/ nrow(BMI) #check
#hist(BMI$value_as_number)

# as with smoking, add four variants to person
# bmi based on all time, prior 5, prior 2, and prior 1
BMI$time.to.start<- as.numeric(difftime(BMI$measurement_date,
                               study.start.date,
                               units="days") )
hist(BMI$time.to.start/365.25)


person<-person %>% 
  left_join(BMI %>% 
              select(person_id, value_as_number,time.to.start) %>%
              rename(bmi=value_as_number,
                     bmi.time.to.start=time.to.start))

prop.table(table(!is.na(person$bmi)))
quantile(person$bmi.time.to.start/365.25, na.rm = TRUE)

rm(BMI)






# ggplot()+
#   geom_density(aes(age),
#                  colour="black",
#                  data=person) +
#   geom_density(aes(age),
#                  colour="red",
#                  data=person %>% filter(!is.na(bmi)))


# medea -----
# U1 is quintile 1 of MEDEA which is the least deprived areas, 
# U5 is quintile 5 and represent the most deprived areas. "R" is or rural areas for which we cannot
# calculate MEDEA. And "U" means a person is assigned to a urban area but the quintile of MEDEA is missing.
MEDEA<-observation_db%>% 
  filter( observation_source_value	=="medea11") %>% 
  collect()
# no individuals have more than one record
length(unique(MEDEA$person_id))/ nrow(MEDEA)
table(MEDEA$value_as_string, useNA="always")

MEDEA <- MEDEA %>%
  mutate(medea = ifelse (str_detect(value_as_string, "1"), "Q1",
                               ifelse(str_detect(value_as_string, "2"), "Q2",
                                      ifelse(str_detect(value_as_string, "3"), "Q3",
                                             ifelse(str_detect(value_as_string, "4"), "Q4",
                                                    ifelse(str_detect(value_as_string, "5"), "Q5",
                                                          ifelse(str_detect(value_as_string, "0"), "Missing", 
                                                                NA)))))))

table(MEDEA$medea, useNA="always")

# drop missing
MEDEA<-MEDEA %>% 
  filter(medea %in%
           c("Q1","Q2","Q3","Q4","Q5"))
table(MEDEA$medea, useNA="always")

# add to person
person<-person %>% 
  left_join(MEDEA %>% 
              select(person_id, medea)   )
prop.table(table(person$medea, useNA="always"))

rm(MEDEA)

# smoking status -----
smoking<-observation_db %>%
   filter(observation_concept_id=="43054909") %>% 
   filter(observation_source_value =="tab") %>%
  left_join(concept_db,
            by=c("value_as_concept_id"="concept_id")) %>%
  select(person_id, observation_concept_id,concept_name,observation_date) %>%
    collect()

# individuals have more than one record
length(unique(smoking$person_id))/ nrow(smoking)

#values from before 2006?
smoking$days<- as.numeric(difftime(smoking$observation_date,
                                   as.Date("2006/01/01"),
                                   units="days"))
sum(smoking$days<0)
sum(smoking$days<0)/nrow(smoking)
# drop these
smoking<-smoking %>%
  filter(smoking$days>=0)


# drop values after start date
smoking$days<- as.numeric(difftime(smoking$observation_date,
                                   study.start.date))
sum(smoking$days>0)
smoking<-smoking %>%
  filter(smoking$days<=0)

 # smoking %>%
 # ggplot()+
 #   geom_histogram(aes(observation_date))

# keep most recent record for an individual
smoking<-smoking %>%
  arrange(person_id, desc(observation_date)) %>%
  group_by(person_id) %>%
  mutate(seq=1:length(person_id))

smoking <- smoking %>%
  filter(seq==1) %>%
  select(-seq)

# smoking based on all time
smoking$time.to.start<- as.numeric(difftime(smoking$observation_date,
                                   study.start.date,
                                   units="days") )
hist(smoking$time.to.start/365.25)

person<-person %>%
  left_join(smoking %>%
              select(person_id, concept_name, time.to.start) %>%
              rename(smoke=concept_name,
                     smoke.time.to.start=time.to.start))

prop.table(table(person$smoke, useNA = "always"))
quantile(person$smoke.time.to.start/365.25, na.rm = TRUE)

rm(smoking)



## add symptom info at time of condition/ test date  -----
# at time of covid19 diagnosis, test pos, and hosp- same day

# cough, 
# dyspnea, 
# Diarrhea
# Fever
# Myalgia 
# Anosmia OR Hyposmia OR Dysgeusia episodes
# Malaise/ fatigue
#Pain 

# cough  ---------
cough.codes<-concept_ancestor_db %>%
  filter(ancestor_concept_id  %in% c(254761,4089228) ) %>% 
  collect() %>% 
  select(descendant_concept_id) %>% 
  distinct() %>% 
  pull()

cough<-condition_occurrence_db %>%  # condition table
  filter(condition_concept_id %in% cough.codes) %>% # code of interest
  select(person_id, condition_start_date) %>%  # just need stat_date
  collect() %>% 
  right_join(person %>% 
       filter(!is.na(COVID19_diagnosis_test_date)) %>%  # only those with diagnosis
       select(person_id, COVID19_diagnosis_test_date)) %>% 
  filter(condition_start_date>=(COVID19_diagnosis_test_date-days(2))) %>% 
  filter(condition_start_date<=(COVID19_diagnosis_test_date+days(2)))  # within two days either side
cough<-cough %>% 
  mutate(cough=1) %>% 
  select(person_id, cough) %>% 
  distinct()
person<-person %>% 
  left_join(cough)
rm(cough)



# dyspnea  ---------
dyspnea.codes<-concept_ancestor_db %>%
  filter(ancestor_concept_id  %in% c(312437, 4263848, 4060052,4206307,
                                     4094132, 4244276) ) %>% 
  collect() %>% 
  select(descendant_concept_id) %>% 
  distinct() %>% 
  pull()

dyspnea<-condition_occurrence_db %>%  # condition table
  filter(condition_concept_id %in% dyspnea.codes) %>% # code of interest
  select(person_id, condition_start_date) %>%  # just need stat_date
  collect() %>% 
  right_join(person %>% 
       filter(!is.na(COVID19_diagnosis_test_date)) %>%  # only those with diagnosis
       select(person_id, COVID19_diagnosis_test_date)) %>% 
  filter(condition_start_date>=(COVID19_diagnosis_test_date-days(2))) %>% 
  filter(condition_start_date<=(COVID19_diagnosis_test_date+days(2)))  # within two days either side
dyspnea<-dyspnea %>% 
  mutate(dyspnea=1) %>% 
  select(person_id, dyspnea) %>% 
  distinct()
person<-person %>% 
  left_join(dyspnea)
rm(dyspnea)




# diarrhea  ---------
diarrhea.codes<-concept_ancestor_db %>%
  filter(ancestor_concept_id  %in% c(196523, 80141,
                                     197484,198337,
                                     4261727,
                                     4091519, 4145808,
                                     4249551) ) %>% 
  collect() %>% 
  select(descendant_concept_id) %>% 
  distinct() %>% 
  pull()
diarrhea<-condition_occurrence_db %>%  # condition table
  filter(condition_concept_id %in% diarrhea.codes) %>% # code of interest
  select(person_id, condition_start_date) %>%  # just need stat_date
  collect() %>% 
  right_join(person %>% 
       filter(!is.na(COVID19_diagnosis_test_date)) %>%  # only those with diagnosis
       select(person_id, COVID19_diagnosis_test_date)) %>% 
  filter(condition_start_date>=(COVID19_diagnosis_test_date-days(2))) %>% 
  filter(condition_start_date<=(COVID19_diagnosis_test_date+days(2)))  # within two days either side
diarrhea<-diarrhea %>% 
  mutate(diarrhea=1) %>% 
  select(person_id, diarrhea) %>% 
  distinct()
person<-person %>% 
  left_join(diarrhea)
rm(diarrhea)



# fever  ---------
fever.codes<-concept_ancestor_db %>%
  filter(ancestor_concept_id  %in% c(437663) ) %>% 
  collect() %>% 
  select(descendant_concept_id) %>% 
  distinct() %>% 
  pull()

fever<-condition_occurrence_db %>%  # condition table
  filter(condition_concept_id %in% fever.codes) %>% # code of interest
  select(person_id, condition_start_date) %>%  # just need stat_date
  collect() %>% 
  right_join(person %>% 
       filter(!is.na(COVID19_diagnosis_test_date)) %>%  # only those with diagnosis
       select(person_id, COVID19_diagnosis_test_date)) %>% 
  filter(condition_start_date>=(COVID19_diagnosis_test_date-days(2))) %>% 
  filter(condition_start_date<=(COVID19_diagnosis_test_date+days(2)))  # within two days either side
fever<-fever %>% 
  mutate(fever=1) %>% 
  select(person_id, fever) %>% 
  distinct()
person<-person %>% 
  left_join(fever)
rm(fever)




# myalgia  ---------
myalgia.codes<-concept_ancestor_db %>%
  filter(ancestor_concept_id  %in% c(4150129) ) %>% 
  collect() %>% 
  select(descendant_concept_id) %>% 
  distinct() %>% 
  pull()


myalgia<-condition_occurrence_db %>%  # condition table
  filter(condition_concept_id %in% myalgia.codes) %>% # code of interest
  select(person_id, condition_start_date) %>%  # just need stat_date
  collect() %>% 
  right_join(person %>% 
       filter(!is.na(COVID19_diagnosis_test_date)) %>%  # only those with diagnosis
       select(person_id, COVID19_diagnosis_test_date)) %>% 
  filter(condition_start_date>=(COVID19_diagnosis_test_date-days(2))) %>% 
  filter(condition_start_date<=(COVID19_diagnosis_test_date+days(2)))  # within two days either side
myalgia<-myalgia %>% 
  mutate(myalgia=1) %>% 
  select(person_id, myalgia) %>% 
  distinct()
person<-person %>% 
  left_join(myalgia)
rm(myalgia)



# anosmia_hyposmia_dysgeusia  ---------
anosmia_hyposmia_dysgeusia.codes<-concept_ancestor_db %>%
  filter(ancestor_concept_id  %in% c(436235,43530714,4185711) ) %>% 
  collect() %>% 
  select(descendant_concept_id) %>% 
  distinct() %>% 
  pull()
anosmia_hyposmia_dysgeusia<-condition_occurrence_db %>%  # condition table
  filter(condition_concept_id %in% anosmia_hyposmia_dysgeusia.codes) %>% # code of interest
  select(person_id, condition_start_date) %>%  # just need stat_date
  collect() %>% 
  right_join(person %>% 
       filter(!is.na(COVID19_diagnosis_test_date)) %>%  # only those with diagnosis
       select(person_id, COVID19_diagnosis_test_date)) %>% 
  filter(condition_start_date>=(COVID19_diagnosis_test_date-days(2))) %>% 
  filter(condition_start_date<=(COVID19_diagnosis_test_date+days(2)))  # within two days either side
anosmia_hyposmia_dysgeusia<-anosmia_hyposmia_dysgeusia %>% 
  mutate(anosmia_hyposmia_dysgeusia=1) %>% 
  select(person_id, anosmia_hyposmia_dysgeusia) %>% 
  distinct()
person<-person %>% 
  left_join(anosmia_hyposmia_dysgeusia)
rm(anosmia_hyposmia_dysgeusia)





# malaise_fatigue  ---------
malaise_fatigue.codes<-concept_ancestor_db %>%
  filter(ancestor_concept_id  %in% c(4272240,4223659,
                                     439926) ) %>% 
  collect() %>% 
  select(descendant_concept_id) %>% 
  distinct() %>% 
  pull()
malaise_fatigue<-condition_occurrence_db %>%  # condition table
  filter(condition_concept_id %in% malaise_fatigue.codes) %>% # code of interest
  select(person_id, condition_start_date) %>%  # just need stat_date
  collect() %>% 
  right_join(person %>% 
       filter(!is.na(COVID19_diagnosis_test_date)) %>%  # only those with diagnosis
       select(person_id, COVID19_diagnosis_test_date)) %>% 
  filter(condition_start_date>=(COVID19_diagnosis_test_date-days(2))) %>% 
  filter(condition_start_date<=(COVID19_diagnosis_test_date+days(2)))  # within two days either side
malaise_fatigue<-malaise_fatigue %>% 
  mutate(malaise_fatigue=1) %>% 
  select(person_id, malaise_fatigue) %>% 
  distinct()
person<-person %>% 
  left_join(malaise_fatigue)
rm(malaise_fatigue)



# pain  ---------
pain.codes<-concept_ancestor_db %>%
   filter(ancestor_concept_id  %in% c(4329041,442555,4147218,
                                      4308649) ) %>% 
  collect() %>% 
  select(descendant_concept_id) %>% 
  distinct() %>% 
  pull()

pain<-condition_occurrence_db %>%  # condition table
  filter(condition_concept_id %in% pain.codes) %>% # code of interest
  select(person_id, condition_start_date) %>%  # just need stat_date
  collect() %>% 
  right_join(person %>% 
       filter(!is.na(COVID19_diagnosis_test_date)) %>%  # only those with diagnosis
       select(person_id, COVID19_diagnosis_test_date)) %>% 
  filter(condition_start_date>=(COVID19_diagnosis_test_date-days(2))) %>% 
  filter(condition_start_date<=(COVID19_diagnosis_test_date+days(2)))  # within two days either side
pain<-pain %>% 
  mutate(pain=1) %>% 
  select(person_id, pain) %>% 
  distinct()
person<-person %>% 
  left_join(pain)
rm(pain)






# headache  ---------
headache.codes<-concept_ancestor_db %>%
   filter(ancestor_concept_id  %in% c(375527,378253) ) %>% 
  collect() %>% 
  select(descendant_concept_id) %>% 
  distinct() %>% 
  pull()

headache<-condition_occurrence_db %>%  # condition table
  filter(condition_concept_id %in% headache.codes) %>% # code of interest
  select(person_id, condition_start_date) %>%  # just need stat_date
  collect() %>% 
  right_join(person %>% 
       filter(!is.na(COVID19_diagnosis_test_date)) %>%  # only those with diagnosis
       select(person_id, COVID19_diagnosis_test_date)) %>% 
  filter(condition_start_date>=(COVID19_diagnosis_test_date-days(2))) %>% 
  filter(condition_start_date<=(COVID19_diagnosis_test_date+days(2)))  # within two days either side
headache<-headache %>% 
  mutate(headache=1) %>% 
  select(person_id, headache) %>% 
  distinct()
person<-person %>% 
  left_join(headache)
rm(headache)



## save ----
save(list=c("person"),
     file = here("WorkingData", "DataForAnalysis.RData"))



