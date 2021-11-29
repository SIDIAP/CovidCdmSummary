

#install.packages("renv") # if not already installed, install renv from CRAN
# renv::restore() # this should prompt you to install the various packages required for the study
# renv::activate()

# packages ------
library(SqlRender)
library(DatabaseConnector)
library(FeatureExtraction)
library(here)
library(lubridate)
library(stringr)
library(ggplot2)
library(DBI)
library(dbplyr)
library(dplyr)
library(tidyr)
library(kableExtra)
library(RSQLite)
library(rmarkdown)
library(tableone)
library(scales)
library(forcats)
library(epiR)
library(RPostgreSQL)
library(cmprsk)
library(mstate)
library(broom)
library(rms)
# please load the above packages 
# you should have them all available, with the required version, after
# having run renv::restore above

# set up  ------
output.folder<-here::here("output")
# the path to a folder (that exists) where the results from this analysis will be saved

oracleTempSchema<-NULL

# If you haven´t already, save database details to .Renviron by running:
# usethis::edit_r_environ()
server<-"...."
server_dbi<-"...."

user<-Sys.getenv("DB_USER")
password<- Sys.getenv("DB_PASSWORD")
port<-Sys.getenv("DB_PORT") 
host<-Sys.getenv("DB_HOST") 

connectionDetails <-DatabaseConnector::downloadJdbcDrivers("postgresql", here::here())
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "postgresql",
                                                                server =server,
                                                                user = user,
                                                                password = password,
                                                                port = port ,
                                                                pathToDriver = here::here())
db <- dbConnect(RPostgreSQL::PostgreSQL(),
                dbname = server_dbi,
                port = port,
                host = host, 
                user = user, 
                password = password)



# The OHDSI DatabaseConnector connection details
targetDialect <-"...." 
# This is your sql dialect used with the OHDSI SqlRender package

# The name of the schema that contains the OMOP CDM with patient-level data
cdm_database_schema<-"...."
# The name of the schema that contains the vocabularies
vocabulary_database_schema<-"...."

# The name of the schema where a results table will be created 
results_database_schema<-"...."

# Tables to be created in your results schema for this analysis
# You can keep the above names or change them
# Note, any existing tables in your results schema with the same name will be overwritten
cohortTable<-"...."
cohortTableComorbidities<-"...."

# The name/ acronym for your database (to be used in the titles of reports, etc)
db.name<-"...."

# if you have already created the cohorts, you can set this to FALSE to skip instantiating these cohorts again
create.cohortTable<-FALSE
create.cohortTableComorbidities<-FALSE


# run the analysis ------
start<-Sys.time()
source(here("RunStudy.R"))
Sys.time()-start





