# Copyright 2020 Observational Health Data Sciences and Informatics
#
# This file is part of PCE
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Execute the Study
#'
#' @details
#' This function executes the PCE Study.
#' 
#' @param connectionDetails    An object of type \code{connectionDetails} as created using the
#'                             \code{\link[DatabaseConnector]{createConnectionDetails}} function in the
#'                             DatabaseConnector package.
#' @param cdmDatabaseSchema    Schema name where your patient-level data in OMOP CDM format resides.
#'                             Note that for SQL Server, this should include both the database and
#'                             schema name, for example 'cdm_data.dbo'.
#' @param cdmDatabaseName      Shareable name of the database 
#' @param cohortDatabaseSchema Schema name where intermediate data can be stored. You will need to have
#'                             write priviliges in this schema. Note that for SQL Server, this should
#'                             include both the database and schema name, for example 'cdm_data.dbo'.
#' @param cohortTable          The name of the table that will be created in the work database schema.
#'                             This table will hold the target population cohorts used in this
#'                             study.
#' @param oracleTempSchema     Should be used in Oracle to specify a schema where the user has write
#'                             priviliges for storing temporary tables.
#' @param setting              A data.frame with the tId, oId, model triplets to run - if NULL it runs all possible combinations                
#' @param sampleSize           How many patients to sample from the target population  
#' @param recalibrate          Recalibrate the model intercept and slop
#' @param recalibrateInterceptOnly  Recalibrate the intercept only.                           
#' @param riskWindowStart      The start of the risk window (in days) relative to the startAnchor.                           
#' @param startAnchor          The anchor point for the start of the risk window. Can be "cohort start" or "cohort end".
#' @param riskWindowEnd        The end of the risk window (in days) relative to the endAnchor parameter
#' @param endAnchor            The anchor point for the end of the risk window. Can be "cohort start" or "cohort end".
#' @param firstExposureOnly    Should only the first exposure per subject be included? Note that this is typically done in the createStudyPopulation function,
#' @param removeSubjectsWithPriorOutcome Remove subjects that have the outcome prior to the risk window start?
#' @param priorOutcomeLookback How many days should we look back when identifying prior outcomes?
#' @param requireTimeAtRisk    Should subject without time at risk be removed?
#' @param minTimeAtRisk        The minimum number of days at risk required to be included
#' @param includeAllOutcomes   (binary) indicating whether to include people with outcomes who are not observed for the whole at risk period
#' @param standardCovariates   Use this to add standard covariates such as age/gender
#' @param outputFolder         Name of local folder to place results; make sure to use forward slashes
#'                             (/). Do not use a folder on a network drive since this greatly impacts
#'                             performance.
#' @param createCohorts        Create the cohortTable table with the target population and outcome cohorts?
#' @param createTable1         Create the Table 1 - a characteristic of the target populations
#' @param runAnalyses          Run the model development
#' @param aggregateCohorts     Run this after runAnalyses to calculate the performance for combination of males and females, black and non-black
#' @param viewShiny            View the results as a shiny app
#' @param packageResults       Should results be packaged for later sharing?     
#' @param minCellCount         The minimum number of subjects contributing to a count before it can be included 
#'                             in packaged results.
#' @param verbosity            Sets the level of the verbosity. If the log level is at or higher in priority than the logger threshold, a message will print. The levels are:
#'                                         \itemize{
#'                                         \item{DEBUG}{Highest verbosity showing all debug statements}
#'                                         \item{TRACE}{Showing information about start and end of steps}
#'                                         \item{INFO}{Show informative information (Default)}
#'                                         \item{WARN}{Show warning messages}
#'                                         \item{ERROR}{Show error messages}
#'                                         \item{FATAL}{Be silent except for fatal errors}
#'                                         }                              
#' @param cdmVersion           The version of the common data model  
#' @param overwrite            T overwrite the results, F only runs analyses that are currently empty                            
#'
#' @examples
#' \dontrun{
#' connectionDetails <- createConnectionDetails(dbms = "postgresql",
#'                                              user = "joe",
#'                                              password = "secret",
#'                                              server = "myserver")
#'
#' execute(connectionDetails,
#'         cdmDatabaseSchema = "cdm_data",
#'         cdmDatabaseName = 'shareable name of the database'
#'         cohortDatabaseSchema = "study_results",
#'         cohortTable = "cohort",
#'         outcomeId = 1,
#'         oracleTempSchema = NULL,
#'         riskWindowStart = 1,
#'         startAnchor = 'cohort start',
#'         riskWindowEnd = 365,
#'         endAnchor = 'cohort start',
#'         outputFolder = "c:/temp/study_results", 
#'         createCohorts = T,
#'         runAnalyses = T,
#'         aggregateCohorts = T,
#'         viewShiny = F,
#'         packageResults = F,
#'         minCellCount = 10,
#'         verbosity = "INFO",
#'         cdmVersion = 5)
#' }
#'
#' @export
execute <- function(connectionDetails,
                    cdmDatabaseSchema,
                    cdmDatabaseName = 'friendly database name',
                    cohortDatabaseSchema = cdmDatabaseSchema,
                    cohortTable = "cohort",
                    oracleTempSchema = cohortDatabaseSchema,
                    setting = NULL,
                    sampleSize = NULL,
                    recalibrate = F,
                    recalibrateInterceptOnly = F,
                    riskWindowStart = 1,
                    startAnchor = 'cohort start',
                    riskWindowEnd = 365,
                    endAnchor = 'cohort start',
                    firstExposureOnly = F,
                    removeSubjectsWithPriorOutcome = F,
                    priorOutcomeLookback = 99999,
                    requireTimeAtRisk = F,
                    minTimeAtRisk = 1,
                    includeAllOutcomes = T,
                    outputFolder,
                    createCohorts = F,
                    createTable1 = F,
                    runAnalyses = F,
                    aggregateCohorts = T,
                    viewShiny = F,
                    packageResults = F,
					          minCellCount = 10,
                    verbosity = "INFO",
                    cdmVersion = 5,
					          overwrite = T) {
  if (!file.exists(file.path(outputFolder,cdmDatabaseName)))
    dir.create(file.path(outputFolder,cdmDatabaseName), recursive = TRUE)
  
  ParallelLogger::addDefaultFileLogger(file.path(outputFolder,cdmDatabaseName, "log.txt"))
  
  ## add existing model protocol code?
  
  if (createCohorts) {
    ParallelLogger::logInfo("Creating cohorts")
    createCohorts(connectionDetails = connectionDetails,
                  cdmDatabaseSchema = cdmDatabaseSchema,
                  cohortDatabaseSchema = cohortDatabaseSchema,
                  cohortTable = cohortTable,
                  oracleTempSchema = oracleTempSchema,
                  outputFolder = file.path(outputFolder, cdmDatabaseName)) 
  }
  
  if(runAnalyses){
    # add standardCovariates if included 
    
    analysisSettings <- getAnalyses(setting, outputFolder,cdmDatabaseName)
    
    for(i in 1:nrow(analysisSettings)){
      
      ParallelLogger::logInfo(paste0('Running ',analysisSettings$analysisId[i]))
      
      if(!overwrite){
        plpRsultFolderExists <- dir.exists(file.path(outputFolder,cdmDatabaseName,analysisSettings$analysisId[i], 'plpResult','model'))
        if(plpRsultFolderExists){ParallelLogger::logInfo(paste0('Result exists for ',analysisSettings$analysisId[i], ' not overwritting'))}
      } else{
        plpRsultFolderExists <- F
      }
      
      if(!plpRsultFolderExists){
        
        pathToStandard <- system.file("settings", gsub('_model.csv','_standard_features.csv',analysisSettings$model[i]), package = "PCE")
        if(file.exists(pathToStandard)){
          standTemp <- read.csv(pathToStandard)$x
          
          standSet <- list()
          length(standSet) <- length(standTemp)
          names(standSet) <- standTemp
          for(j in 1:length(standSet)){
            standSet[[j]] <- T
          }
          
          pathToInclude <- system.file("settings", gsub('_model.csv','_standard_features_include.csv',analysisSettings$model[i]), package = "PCE")
          incS <- read.csv(pathToInclude)$x
          standSet$includedCovariateIds <- incS
          
          standardCovariates <- do.call(FeatureExtraction::createCovariateSettings,standSet)
          
        } else{
          standardCovariates <- NULL
        }
        
        #getData
        ParallelLogger::logInfo("Extracting data")
        plpData <- tryCatch({getData(connectionDetails = connectionDetails,
                                     cdmDatabaseSchema = cdmDatabaseSchema,
                                     cdmDatabaseName = cdmDatabaseName,
                                     cohortDatabaseSchema = cohortDatabaseSchema,
                                     cohortTable = cohortTable,
                                     cohortId = analysisSettings$targetId[i],
                                     outcomeId = analysisSettings$outcomeId[i],
                                     oracleTempSchema = oracleTempSchema,
                                     model = analysisSettings$model[i],
                                     standardCovariates = standardCovariates,
                                     firstExposureOnly = firstExposureOnly,
                                     sampleSize = sampleSize,
                                     cdmVersion = cdmVersion)},
                            error = function(e){ParallelLogger::logError(e); return(NULL)})
        
        if(!is.null(plpData)){
          
          
          # get table 1
          table1 <- tryCatch({getTable1(plpData)}, error = function(e){ParallelLogger::logError(e); return(NULL)})
          
          #create pop
          ParallelLogger::logInfo("Creating population")
          population <- tryCatch({PatientLevelPrediction::createStudyPopulation(plpData = plpData, 
                                                                                outcomeId = analysisSettings$outcomeId[i],
                                                                                riskWindowStart = riskWindowStart,
                                                                                startAnchor = startAnchor,
                                                                                riskWindowEnd = riskWindowEnd,
                                                                                endAnchor = endAnchor,
                                                                                firstExposureOnly = firstExposureOnly,
                                                                                removeSubjectsWithPriorOutcome = removeSubjectsWithPriorOutcome,
                                                                                priorOutcomeLookback = priorOutcomeLookback,
                                                                                requireTimeAtRisk = requireTimeAtRisk,
                                                                                minTimeAtRisk = minTimeAtRisk,
                                                                                includeAllOutcomes = includeAllOutcomes)},
                                 error = function(e){ParallelLogger::logError(e); return(NULL)})
          
          
          # if less than 20 outcomes dont run
          if(sum(population$outcomeCount >0)<10){
            ParallelLogger::logInfo('Less that 10 outcomes so not running...')
          }
          
          if(sum(population$outcomeCount >0)>=10){
            
            
            if(!is.null(population)){
              # apply the model:
              plpModel <- list(model = getModel(analysisSettings$model[i]),
                               analysisId = analysisSettings$analysisId[i],
                               hyperParamSearch = NULL,
                               index = NULL,
                               trainCVAuc = NULL,
                               modelSettings = list(model = analysisSettings$model[i], 
                                                    modelParameters = NULL),
                               metaData = NULL,
                               populationSettings = attr(population, "metaData"),
                               trainingTime = NULL,
                               varImp = data.frame(covariateId = getModel(analysisSettings$model[i])$covariateId,
                                                   covariateValue = getModel(analysisSettings$model[i])$points),
                               dense = T,
                               cohortId = analysisSettings$cohortId[i],
                               outcomeId = analysisSettings$outcomeId[i],
                               covariateMap = NULL,
                               predict = predictExisting(model = analysisSettings$model[i])
              )
              attr(plpModel, "type") <- 'existing'
              class(plpModel) <- 'plpModel'
              
              
              
              ParallelLogger::logInfo("Applying and evaluating model")
              result <- tryCatch({PatientLevelPrediction::applyModel(population = population,
                                                                     plpData = plpData,
                                                                     plpModel = plpModel)},
                                 error = function(e){ParallelLogger::logError(e); return(NULL)})
              
              if(!is.null(result)){
                result$inputSetting$database <- cdmDatabaseName
                result$inputSetting$modelSettings <- list(model = 'existing model', name = analysisSettings$model[i], param = getModel(analysisSettings$model[i]))
                result$inputSetting$dataExtrractionSettings$covariateSettings <- plpData$metaData$call$covariateSettings
                result$inputSetting$populationSettings <- attr(population, "metaData")
                result$executionSummary  <- list()
                result$model <- plpModel
                result$analysisRef <- list()
                result$covariateSummary <- tryCatch({PatientLevelPrediction:::covariateSummary(plpData = plpData, population = population, model = plpModel)},
                                                    error = function(e){ParallelLogger::logError(e); return(NULL)})
                
                
                if(recalibrate){
                  # add code here
                  
                  # recalibrate each time 2/3/5/10 years and add to prediction plus save values
                  
                  predictionWeak <- result$prediction
                  
                  ### Extract data
                  
                  # this has to be modified per model...
                  #1- 0.9533^exp(x-86.61) = p
                  #log(log(1-p)/log(0.9533))+86.61 = x
                  
                  if(analysisSettings$model[i] == "pooled_female_non_black_model.csv"){
                    lp <- log(log(1-predictionWeak$value)/log(0.9665))- 29.18
                  }else if(analysisSettings$model[i] == "pooled_male_non_black_model.csv"){
                    lp <- log(log(1-predictionWeak$value)/log(0.9144)) + 61.18
                  }else if(analysisSettings$model[i] == "pooled_female_black_model.csv"){
                    lp <- log(log(1-predictionWeak$value)/log(0.9533))+86.61
                  } else{
                    lp <- log(log(1-predictionWeak$value)/log(0.8954)) + 19.54
                  }
                  
                  #t <- predictionWeak$survivalTime # observed follow up time
                  t <- apply(cbind(predictionWeak$daysToCohortEnd, predictionWeak$survivalTime), 1, min)
                  
                  y <- ifelse(predictionWeak$outcomeCount>0,1,0)  # observed outcome
                  
                  extras <- c()
                  for(yrs in c(2,3,5,10)){
                    t_temp <- t
                    y_temp <- y
                    y_temp[t_temp>365*yrs] <- 0
                    t_temp[t_temp>365*yrs] <- 365*yrs
                    S<- survival::Surv(t_temp, y_temp) 
                    #### Intercept + Slope recalibration
                    f.slope <- survival::coxph(S~lp)
                    h.slope <- max(survival::basehaz(f.slope)$hazard)  # maximum OK because of prediction_horizon
                    lp.slope <- stats::predict(f.slope)
                    p.slope.recal <- 1-exp(-h.slope*exp(lp.slope))
                    predictionWeak$value <- p.slope.recal
                    predictionWeak$new <- p.slope.recal
                    colnames(predictionWeak)[ncol(predictionWeak)] <- paste0('value',yrs,'year')
                    
                    # TODO save the recalibration stuff somewhere?
                    extras <- rbind(extras,
                                    c(analysisSettings$analysisId[i],"validation",paste0("h.slope_",yrs),h.slope),
                                    c(analysisSettings$analysisId[i],"validation",paste0("f.slope_",yrs),f.slope$coefficients['lp']))
                    
                  }
                  
                  
                  result$prediction <- predictionWeak # use 10 year prediction value
                  performance <- PatientLevelPrediction::evaluatePlp(result$prediction, plpData)
                  
                  # reformatting the performance 
                  performance <- reformatePerformance(performance,analysisSettings$analysisId[i])
                  
                  result$performanceEvaluation <- performance
                  
                  result$performanceEvaluation$evaluationStatistics <- rbind(result$performanceEvaluation$evaluationStatistics,extras)
                  
                }
                
                
                if(recalibrateInterceptOnly & !recalibrate){
                  # recalibrate each time 2/3/5/10 years and add to prediction plus save values
                  
                  predictionWeak <- result$prediction
                  
                  ### Extract data
                  
                  # this has to be modified per model...
                  #1- 0.9533^exp(x-86.61) = p
                  #log(log(1-p)/log(0.9533))+86.61 = x
                  
                  if(analysisSettings$model[i] == "pooled_female_non_black_model.csv"){
                    lp <- log(log(1-predictionWeak$value)/log(0.9665))- 29.18
                  }else if(analysisSettings$model[i] == "pooled_male_non_black_model.csv"){
                    lp <- log(log(1-predictionWeak$value)/log(0.9144)) + 61.18
                  }else if(analysisSettings$model[i] == "pooled_female_black_model.csv"){
                    lp <- log(log(1-predictionWeak$value)/log(0.9533))+86.61
                  } else{
                    lp <- log(log(1-predictionWeak$value)/log(0.8954)) + 19.54
                  }
                  
                  #t <- predictionWeak$survivalTime # observed follow up time
                  t <- apply(cbind(predictionWeak$daysToCohortEnd, predictionWeak$survivalTime), 1, min)
                  y <- ifelse(predictionWeak$outcomeCount>0,1,0)  # observed outcome
                  
                  extras <- c()
                  for(yrs in c(2,3,5,10)){
                    t_temp <- t
                    y_temp <- y
                    y_temp[t>365*yrs] <- 0
                    t_temp[t>365*yrs] <- 365*yrs
                    S<- survival::Surv(t_temp, y_temp) 
                    
                    f.intercept <- survival::coxph(S~offset(lp))
                    h.intercept <- max(survival::basehaz(f.intercept)$hazard)  # maximum OK because of prediction_horizon
                    p.intercept.recal <- 1-exp(-h.intercept*exp(lp-mean(lp)))
                    
                    predictionWeak$value <- p.intercept.recal
                    predictionWeak$new <- p.intercept.recal
                    colnames(predictionWeak)[ncol(predictionWeak)] <- paste0('value',yrs,'year')
                    
                    # TODO save the recalibration stuff somewhere?
                    extras <- rbind(extras,
                                    c(analysisSettings$analysisId[i],"validation",paste0("h.intercept_",yrs),h.intercept))
                    
                  }
                  
                  
                  result$prediction <- predictionWeak # use 10 year prediction value
                  performance <- PatientLevelPrediction::evaluatePlp(result$prediction, plpData)
                  
                  # reformatting the performance 
                  performance <- reformatePerformance(performance,analysisSettings$analysisId[i])
                  
                  result$performanceEvaluation <- performance
                  
                  result$performanceEvaluation$evaluationStatistics <- rbind(result$performanceEvaluation$evaluationStatistics,extras)
                  
                  
                }
                
                # CUSTOM CODE FOR SURVIVAL METRICS
                #=======================================
                # here we add the 2/3/5 year surivival metrics to prediction
                result <- tryCatch({getSurvivialMetrics(plpResult = result, 
                                                        recalibrate = recalibrate | recalibrateInterceptOnly, 
                                                        analysisId = analysisSettings$analysisId[i],
                                                        model = analysisSettings$model[i])},
                                   error = function(e){ParallelLogger::logError(e); return(result)})
                #=======================================
                
                
                
                if(!dir.exists(file.path(outputFolder,cdmDatabaseName))){
                  dir.create(file.path(outputFolder,cdmDatabaseName))
                }
                ParallelLogger::logInfo("Saving results")
                PatientLevelPrediction::savePlpResult(result, file.path(outputFolder,cdmDatabaseName,analysisSettings$analysisId[i], 'plpResult'))
                saveRDS(table1, file.path(outputFolder,cdmDatabaseName,analysisSettings$analysisId[i], 'plpResult','table1.rds'))
                ParallelLogger::logInfo(paste0("Results saved to:",file.path(outputFolder,cdmDatabaseName,analysisSettings$analysisId[i])))
                
                
              } # result not null
              
            } # population not null
            
          } # count >= 10
          
        }# plpData not null
        
      }# overwrite or non exists
      
    }
  }
  
  if(aggregateCohorts == T){
    agg <- tryCatch({getAggregatePerm(outputFolder,cdmDatabaseName)},
                    error = function(e){ParallelLogger::logError(e);
                      ParallelLogger::logInfo("Aggregate cohorts failed...")})
  }
  
  
  if (packageResults) {
    ParallelLogger::logInfo("Packaging results")
    packageResults(outputFolder = file.path(outputFolder,cdmDatabaseName),
                   minCellCount = minCellCount)
  }
  
  # [TODO] add create shiny app
  viewer <- TRUE
  if(viewShiny) {
    viewer <- tryCatch({
      PatientLevelPrediction::viewMultiplePlp(file.path(outputFolder,cdmDatabaseName))},
      error = function(e){ParallelLogger::logError(e);
        ParallelLogger::logInfo("No results to view...")})
  }

   
  return(viewer)
}




