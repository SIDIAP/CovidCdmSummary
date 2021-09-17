
# instantiate study cohorts -----
cohort.sql<-list.files(here("1_BuildCohorts","Cohorts", "sql"))
cohort.sql<-cohort.sql[cohort.sql!="CreateCohortTable.sql"]
cohorts<-tibble(id=as.integer(1:length(cohort.sql)),
                        file=cohort.sql,
                        name=str_replace(cohort.sql, ".sql", ""))


if(create.cohortTable==TRUE){
print(paste0("- Getting cohorts"))
  
conn <- connect(connectionDetails)
# create empty cohorts table
sql<-readSql(here("1_BuildCohorts","Cohorts","sql","CreateCohortTable.sql"))
sql<-SqlRender::translate(sql, targetDialect = targetDialect)
renderTranslateExecuteSql(conn=conn, 
                            sql,
                            cohort_database_schema =  results_database_schema,
                            cohort_table = cohortTable)
rm(sql)
  
for(cohort.i in 1:length(cohorts$id)){
working.id<-cohorts$id[cohort.i]
print(paste0("-- Getting: ",  cohorts$name[cohort.i],
                 " (", cohort.i, " of ", length(cohorts$name), ")"))

sql<-readSql(here("1_BuildCohorts","Cohorts","sql",cohorts$file[cohort.i])) 
sql <- sub("BEGIN: Inclusion Impact Analysis - event.*END: Inclusion Impact Analysis - person", "", sql)

sql<-SqlRender::translate(sql, targetDialect = targetDialect)
renderTranslateExecuteSql(conn=conn, 
                              sql, 
                              cdm_database_schema = cdm_database_schema,
                              vocabulary_database_schema = vocabulary_database_schema,
                              target_database_schema = results_database_schema,
                              # results_database_schema = results_database_schema,
                              target_cohort_table = cohortTable,
                              target_cohort_id = working.id)  
  }
disconnect(conn)
}

# link to table
cohorts_db<-tbl(db, sql(paste0("SELECT * FROM ",
                                        results_database_schema,".",
                                        cohortTable)))

cohorts_db %>% 
  group_by(cohort_definition_id) %>% 
  tally()



# instantiate comorbidity cohorts ----
# for those people that are in our exposure cohorts
cond.codes<-c("434621",  
              "317009",   
              "443392", 
              "201820",
              "321588",
              "316866",
              "4030518",
              "255573",
              "4182210")
cond.names<-c("autoimmune_disease",
              "asthma",
              "malignant_neoplastic_disease",
              "diabetes_mellitus",
              "heart_disease",
              "hypertensive_disorder",
              "renal_impairment",
              "copd",
              "dementia")

if(create.cohortTableComorbidities==TRUE){
# add the concept ids of interest to the cohortTableProfiles table in the results 
# schema in the cdm
# these will be the code of interest and all of its descendants
print(paste0("-- Getting conditions"))

# template sql
conn <- connect(connectionDetails)
# create empty cohorts table
sql<-readSql(here("1_BuildCohorts","Cohorts","sql","CreateCohortTable.sql"))
sql<-SqlRender::translate(sql, targetDialect = targetDialect)
renderTranslateExecuteSql(conn=conn, 
                          sql,
                          cohort_database_schema =  results_database_schema,
                          cohort_table = cohortTableComorbidities)
rm(sql)

for(cohort.i in 1:length(cond.codes)){
  
  working.id<-cond.codes[cohort.i]
  print(paste0("-- Getting: ",  cond.names[cohort.i],
               " (", cohort.i, " of ", length(cond.names), ")"))
  
  sql<-readSql(here("1_BuildCohorts","ComorbidityCohorts","sql", "Condition_template.sql")) 
  sql <- sub("BEGIN: Inclusion Impact Analysis - event.*END: Inclusion Impact Analysis - person", "", sql)
  sql<-SqlRender::translate(sql, targetDialect = targetDialect)
  renderTranslateExecuteSql(conn=conn, 
                            sql, 
                            cdm_database_schema = cdm_database_schema,
                            Condition_top_code = cond.codes[cohort.i],
                            vocabulary_database_schema = vocabulary_database_schema,
                            target_database_schema = results_database_schema,
                            target_cohort_table = cohortTableComorbidities,
                            target_cohort_id = as.integer(working.id))  
}

disconnect(conn)
} else {
  print(paste0("Skipping creating comorbidity cohorts")) 
}


# link to table
cohortTableComorbidities_db<-tbl(db, sql(paste0("SELECT * FROM ",
                                           results_database_schema,
                                           ".", cohortTableComorbidities)))%>% 
  mutate(cohort_definition_id=as.integer(cohort_definition_id)) 

cohortTableComorbidities_db %>%
  group_by(cohort_definition_id) %>%
  tally()

