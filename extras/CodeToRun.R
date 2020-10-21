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

# with original calibration
PCE::execute(connectionDetails = connectionDetails,
                                cdmDatabaseSchema = cdmDatabaseSchema,
                                cdmDatabaseName = cdmDatabaseName,
                                cohortDatabaseSchema = cohortDatabaseSchema,
                                cohortTable = cohortTable,
                                setting = data.frame(tId = rep(c(1325,1322,1326,1328,
                                                                 1358,1359,1360,1361),2), 
                                                     oId = c(rep(1354,8),rep(1357,8)), 
                                                     model = rep(c('pooled_male_black_model.csv',
                                                                   'pooled_female_black_model.csv',
                                                                   'pooled_male_non_black_model.csv',
                                                                   'pooled_female_non_black_model.csv',
                                                                   'pooled_male_black_model.csv',
                                                                   'pooled_female_black_model.csv',
                                                                   'pooled_male_non_black_model.csv',
                                                                   'pooled_female_non_black_model.csv'
                                                                   ),2)
                                ),
                                sampleSize = sampleSize, 
                                recalibrate = F,
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
                                viewShiny = F,
                                packageResults = T, 
                                minCellCount= 5,
                                verbosity = "INFO",
                                cdmVersion = 5)

# recalibrate
PCE::execute(connectionDetails = connectionDetails,
             cdmDatabaseSchema = cdmDatabaseSchema,
             cdmDatabaseName = paste0(cdmDatabaseName,'_recalibration'),
             cohortDatabaseSchema = cohortDatabaseSchema,
             cohortTable = cohortTable,
             setting = data.frame(tId = rep(c(1325,1322,1326,1328,
                                              1358,1359,1360,1361),2), 
                                  oId = c(rep(1354,8),rep(1357,8)), 
                                  model = rep(c('pooled_male_black_model.csv',
                                                'pooled_female_black_model.csv',
                                                'pooled_male_non_black_model.csv',
                                                'pooled_female_non_black_model.csv',
                                                'pooled_male_black_model.csv',
                                                'pooled_female_black_model.csv',
                                                'pooled_male_non_black_model.csv',
                                                'pooled_female_non_black_model.csv'
                                  ),2)
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
             createCohorts = F,
             runAnalyses = T,
             viewShiny = F,
             packageResults = T, 
             minCellCount= 5,
             verbosity = "INFO",
             cdmVersion = 5)



