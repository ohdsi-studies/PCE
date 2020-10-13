Pooled cohort equation model validation study
=============

<img src="https://img.shields.io/badge/Study%20Status-Repo%20Created-lightgray.svg" alt="Study Status: Repo Created">

- Analytics use case(s): **-**
- Study type: **-**
- Tags: **-**
- Study lead: **-**
- Study lead forums tag: **[[Lead tag]](https://forums.ohdsi.org/u/[Lead tag])**
- Study start date: **-**
- Study end date: **-**
- Protocol: **-**
- Publications: **-**
- Results explorer: **-**

This package will implement an existing model and validate it in OMOP CDM data

Instructions To Install and Run Package From Github
===================

- Make sure you have PatientLevelPrediction installed:

```r
  # get the latest PatientLevelPrediction
  install.packages("devtools")
  devtools::install_github("OHDSI/PatientLevelPrediction", ref = 'development')
  # check the package
  PatientLevelPrediction::checkPlpInstallation()
```

- Then install the study package:
```r
  # install the network package
  devtools::install_github("ohdsi-studies/PCE")
```

- Execute the study by running the code in (extras/CodeToRun.R) but make sure to edit the settings:
```r
library(PCE)
# USER INPUTS
#=======================

# The folder where the study intermediate and result files will be written:
outputFolder <- "./PCEResults"

# Details for connecting to the server:
dbms <- "you dbms"
user <- 'your username'
pw <- 'your password'
server <- 'your server'
port <- 'your port'

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = pw,
                                                                port = port)

# Add the database containing the OMOP CDM data
cdmDatabaseSchema <- 'cdm database schema'
# Add a database with read/write access as this is where the cohorts will be generated
cohortDatabaseSchema <- 'work database schema'

oracleTempSchema <- NULL

# table name where the cohorts will be generated
cohortTable <- 'PCECohort'

# TAR settings
sampleSize <- NULL
riskWindowStart <- 1
startAnchor <- 'cohort start'
riskWindowEnd <- 10*365
endAnchor <- 'cohort start'
firstExposureOnly <- F
removeSubjectsWithPriorOutcome <- T
priorOutcomeLookback <- 99999
requireTimeAtRisk <- F
minTimeAtRisk <- 1
includeAllOutcomes <- T


#=======================

PCE::execute(connectionDetails = connectionDetails,
                         cdmDatabaseSchema = cdmDatabaseSchema,
                         cdmDatabaseName = cdmDatabaseName,
                         cohortDatabaseSchema = cohortDatabaseSchema,
                         cohortTable = cohortTable,
                         setting = data.frame(tId = rep(c(18941,18942,18943,18944),3), 
                                                        oId = c(rep(18945,4),rep(18935,4),rep(18936,4)), 
                                                        model = rep(c('pooled_male_non_black_model.csv',
                                                                      'pooled_female_non_black_model.csv',
                                                                      'pooled_female_black_model.csv',
                                                                      'pooled_male_black_model.csv'),3)
                         ),
                         sampleSize = sampleSize, 
                         recalibrate = T,
                         riskWindowStart = riskWindowStart,
                         startAnchor = startAnchor,
                         riskWindowEnd = riskWindowEnd,
                         endAnchor = endAnchor,
                         firstExposureOnly = firstExposureOnly,
                         removeSubjectsWithPriorOutcome = removeSubjectsWithPriorOutcome,
                         priorOutcomeLookback = priorOutcomeLookback,
                         requireTimeAtRisk = requireTimeAtRisk,
                         minTimeAtRisk = minTimeAtRisk,
                         includeAllOutcomes = includeAllOutcomes,
                         outputFolder = outputFolder,
                         createCohorts = T,
                         runAnalyses = T,
                         viewShiny = T,
                         packageResults = T, 
                         minCellCount= 5,
                         verbosity = "INFO",
                         cdmVersion = 5)
```
# Development status
Under development.
