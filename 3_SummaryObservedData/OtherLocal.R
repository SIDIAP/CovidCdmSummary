library(naniar)
library(UpSetR)




# ----------
# Intersection between cohorts ------


upset.data<-person %>% 
  # mutate(gpop=1) %>% 
  mutate(COVID19_diagnosis_narrow=
           ifelse(!is.na(COVID19_diagnosis_narrow_date),1,NA)) %>% 
  mutate(COVID19_diagnosis_broad=
           ifelse(!is.na(COVID19_diagnosis_broad_date),1,NA)) %>% 
  mutate(COVID19_positive_test=
           ifelse(!is.na(COVID19_positive_test_date),1,NA)) %>%
  # mutate(COVID19_diagnosis_test=
  #          ifelse(!is.na(COVID19_diagnosis_test_date),1,NA)) %>%
  mutate(COVID19_PCR_positive_test=
           ifelse(!is.na(COVID19_PCR_positive_test_date),1,NA)) %>% 
  select(person_id,
         # gpop,
         # COVID19_diagnosis_test,
         COVID19_diagnosis_narrow,
         COVID19_diagnosis_broad,
         COVID19_positive_test,
         COVID19_PCR_positive_test) %>%
  pivot_longer(!person_id, names_to = "group", values_to = "seen",
   values_drop_na = TRUE)
  
  
upset.data<-upset.data %>% 
  select(person_id, group, seen)%>%
  # mutate(seen=1)  %>%
  distinct() %>% 
  pivot_wider(names_from = group, values_from = seen,
              values_fill = 0)
upset.data<-as.data.frame(upset.data)

upset.plot<-upset(upset.data,
                  order.by = "freq",
                  text.scale=1.85,
                  mainbar.y.label="Intersection",
                  sets.bar.color = "black",main.bar.color ="black",
                  matrix.color = "red",set_size.show = FALSE,
                  sets.x.label = "Total")
  upset.plot

png(file=here("2 Analysis", "updated.upset.plot.png") ,
    width =1000 , height = 550) 
upset.plot
dev.off()




trace(Make_main_bar)





## Intersection between cohorts ------
upset.data<-person %>% 
  # mutate(gpop=1) %>% 
  mutate(COVID19_diagnosis_test=
           ifelse(!is.na(COVID19_diagnosis_test_date),1,NA)) %>% 
  mutate(COVID19_hospital=
           ifelse(!is.na(COVID19_hospital_date),1,NA)) %>% 
  mutate(COVID19_death=
           ifelse(!is.na(COVID19_death_date),1,NA)) %>% 
  select(person_id,
         # gpop,
         COVID19_diagnosis_test,
         COVID19_hospital,
         COVID19_death) %>%
  pivot_longer(!person_id, names_to = "group", values_to = "seen",
   values_drop_na = TRUE)
  
  
upset.data<-upset.data %>% 
  select(person_id, group, seen)%>%
  # mutate(seen=1)  %>%
  distinct() %>% 
  pivot_wider(names_from = group, values_from = seen,
              values_fill = 0)
upset.data<-as.data.frame(upset.data)

upset.plot<-upset(upset.data,
                  order.by = "freq",
                  text.scale=1.85,
                  mainbar.y.label="Intersection",
                  sets.bar.color = "black",main.bar.color ="black",
                  matrix.color = "red",set_size.show = FALSE,
                  sets.x.label = "Total")
upset.plot

png(file=here("2 Analysis", "updated.upset.plot.png") ,
    width =1000 , height = 550) 
upset.plot
dev.off()












# eulerr-------
install.packages("eulerr")
library(eulerr)



table(!is.na(person$COVID19_diagnosis_broad_date),
      !is.na(person$COVID19_diagnosis_broad_date),
      !is.na(person$COVID19_diagnosis_broad_date),
      !is.na(person$COVID19_diagnosis_broad_date))

person %>% 
  mutate(a=ifelse(!is.na(COVID19_diagnosis_broad_date), "yes", "no")) %>%
  mutate(b=ifelse(!is.na(COVID19_diagnosis_narrow_date), "yes", "no")) %>% 
  mutate(c=ifelse(!is.na(COVID19_positive_test_date), "yes", "no")) %>% 
  mutate(d=ifelse(!is.na(COVID19_PCR_positive_test_date), "yes", "no")) %>%  
  group_by(a,b,c,d) %>% 
  tally()

fit <- euler(c("a" = sum(!is.na(person$COVID19_diagnosis_broad_date)) , 
               "b" = sum(!is.na(person$COVID19_diagnosis_narrow_date)) , 
               "c" = sum(!is.na(person$COVID19_positive_test_date)) ,
               "d"=sum(!is.na(person$COVID19_PCR_positive_test_date)) ,
               "a&b" = sum(!is.na(person$COVID19_diagnosis_broad_date) & 
                  !is.na(person$COVID19_diagnosis_narrow_date)), 
               "a&c" = sum(!is.na(person$COVID19_diagnosis_broad_date) & 
                  !is.na(person$COVID19_positive_test_date)),
               "a&d" = sum(!is.na(person$COVID19_diagnosis_broad_date) & 
                  !is.na(person$COVID19_PCR_positive_test_date)),
               "b&c" = sum(!is.na(person$COVID19_diagnosis_narrow_date) & 
                  !is.na(person$COVID19_positive_test_date)),
               "b&d" = sum(!is.na(person$COVID19_diagnosis_narrow_date) & 
                  !is.na(person$COVID19_PCR_positive_test_date)),
               "c&d" = sum(!is.na(person$COVID19_positive_test_date) & 
                  !is.na(person$COVID19_PCR_positive_test_date)),
               "a&b&c" =sum(!is.na(person$COVID19_diagnosis_broad_date) & 
    !is.na(person$COVID19_diagnosis_narrow_date)& 
    !is.na(person$COVID19_positive_test_date)),
               "a&c&d"=sum(!is.na(person$COVID19_diagnosis_broad_date) & 
    !is.na(person$COVID19_positive_test_date)& 
    !is.na(person$COVID19_PCR_positive_test_date)),
               "a&b&d"=sum(!is.na(person$COVID19_diagnosis_broad_date) & 
    !is.na(person$COVID19_diagnosis_narrow_date)& 
    !is.na(person$COVID19_PCR_positive_test_date)),
               "b&c&d" =sum(!is.na(person$COVID19_diagnosis_narrow_date) & 
    !is.na(person$COVID19_positive_test_date)& 
    !is.na(person$COVID19_PCR_positive_test_date)),
               "a&b&c&d" =sum(!is.na(person$COVID19_diagnosis_broad_date) & 
    !is.na(person$COVID19_diagnosis_narrow_date)& 
    !is.na(person$COVID19_positive_test_date)& 
    !is.na(person$COVID19_PCR_positive_test_date))))
                 

                
                 


# Customize colors, remove borders, bump alpha, color labels white
plot(fit)
