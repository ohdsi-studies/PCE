getAggregatePerm <- function(outputFolder,cdmDatabaseName){
  # loads settings
  settings <- read.csv(file.path(outputFolder,cdmDatabaseName, 'settings.csv'))
  settings <- settings[,colnames(settings)!='X']
  
  settings$targetName <- as.character(settings$targetName)
  settings$outcomeName <- as.character(settings$outcomeName )
  settings$targetId <- as.double(settings$targetId)
  settings$outcomeId <- as.double(settings$outcomeId)
  settings$model  <- as.character(settings$model)
  settings$cohortName <- as.character(settings$cohortName)
  settings$analysisId <- as.character(settings$analysisId)
  settings$valDatabase <- as.character(settings$valDatabase)
  settings$modelSettingName <- as.character(settings$modelSettingName)
  
  # combine per outcome:
  oids <- unique(settings$outcomeId)
  
  for(i in 1:length(oids)){
    
    tIds <- c(1322, 1325, 1326, 1328)
    prediction <- c()
    templateResult <- NULL
    resultsToLoad <- settings$analysisId[settings$targetId%in%tIds & settings$outcomeId==oids[i]]
    for(aId in resultsToLoad){
      resLoc <- file.path(outputFolder,cdmDatabaseName, aId, 'plpResult')
      if(dir.exists(resLoc)){
        templateResult <- PatientLevelPrediction::loadPlpResult(resLoc)
        predTemp <- templateResult$prediction
        
        if(!is.null(predTemp)){
          prediction <- rbind(prediction, predTemp)
        }
      }
    }
    
    if(!is.null(templateResult)){
      
      result <- templateResult
      result$prediction <- prediction
      q <- unique(stats::quantile(prediction$value, (1:(100 - 1))/100))
      if(length(q)==99){
        result$performanceEvaluation$calibrationSummary$predictionThreshold <- c(0, q)
      }
      
      # update analysisIds
      result$performanceEvaluation$calibrationSummary$analysisId <- paste0('Analysis_',50+1*i)
      result$performanceEvaluation$demographicSummary$analysisId <- paste0('Analysis_',50+1*i)
      result$performanceEvaluation$evaluationStatistics <- as.data.frame(result$performanceEvaluation$evaluationStatistics)
      result$performanceEvaluation$evaluationStatistics$analysisId <- paste0('Analysis_',50+1*i)
      result$performanceEvaluation$evaluationStatistics$Value <- as.character(result$performanceEvaluation$evaluationStatistics$Value)
      result$performanceEvaluation$evaluationStatistics$Value[result$performanceEvaluation$evaluationStatistics$Metric == 'populationSize'] <- nrow(prediction)
      result$performanceEvaluation$evaluationStatistics <- result$performanceEvaluation$evaluationStatistics[-grep('_',result$performanceEvaluation$evaluationStatistics$Metric),]
      result$performanceEvaluation$predictionDistribution$analysisId <- paste0('Analysis_',50+1*i)
      result$performanceEvaluation$thresholdSummary$analysisId <- paste0('Analysis_',50+1*i)
      
      result <- getSurvivialMetrics(plpResult = result, 
                                    recalibrate = F, 
                                    analysisId = paste0('Analysis_',50+1*i),
                                    model = 'combined')
      #=======================================
      
      
      
      if(!dir.exists(file.path(outputFolder,cdmDatabaseName))){
        dir.create(file.path(outputFolder,cdmDatabaseName))
      }
      ParallelLogger::logInfo("Saving results")
      PatientLevelPrediction::savePlpResult(result, file.path(outputFolder,cdmDatabaseName,paste0('Analysis_',50+1*i), 'plpResult'))
      ParallelLogger::logInfo(paste0("Results saved to:",file.path(outputFolder,cdmDatabaseName,paste0('Analysis_',50+1*i))))
      
    }
    
    
    newSet <- c(
      targetId = -1,
      targetName = 'Persons who are statin-risk eligible',
      outcomeId = as.double(oids[i]),
      outcomeName = unique(settings$outcomeName[settings$outcomeId==oids[i]]),
      model = 'combined',
      modelSettingsId = 1, 
      analysisId = paste0('Analysis_',50+1*i),
      cohortName = "Persons who are statin-risk eligible",
      devDatabase = NA,
      valDatabase = unique(settings$valDatabase),
      modelSettingName = 'combined',
      populationSettingId = 1,
      covariateSettingId = 1
    )
    settings <- rbind(settings,newSet)
    
    
    # combine nd eval
    
    #=======
    tIds2 <- c(1358, 1359, 1360, 1361)
    prediction <- c()
    templateResult <- NULL
    resultsToLoad <- settings$analysisId[settings$targetId%in%tIds2 & settings$outcomeId==oids[i]]
    for(aId in resultsToLoad){
      resLoc <- file.path(outputFolder,cdmDatabaseName, aId, 'plpResult')
      if(dir.exists(resLoc)){
        templateResult <- PatientLevelPrediction::loadPlpResult(resLoc)
        predTemp <- templateResult$prediction
        
        if(!is.null(predTemp)){
          prediction <- rbind(prediction, predTemp)
        }
      }
    }
    
    if(!is.null(templateResult)){
      
      result <- templateResult
      result$prediction <- prediction
      q <- unique(stats::quantile(prediction$value, (1:(100 - 1))/100))
      if(length(q)==99){
        result$performanceEvaluation$calibrationSummary$predictionThreshold <- c(0, q)
      }
      
      # update analysisIds
      result$performanceEvaluation$calibrationSummary$analysisId <- paste0('Analysis_',50+2*i)
      result$performanceEvaluation$demographicSummary$analysisId <- paste0('Analysis_',50+2*i)
      result$performanceEvaluation$evaluationStatistics <- as.data.frame(result$performanceEvaluation$evaluationStatistics)
      result$performanceEvaluation$evaluationStatistics$analysisId <- paste0('Analysis_',50+2*i)
      result$performanceEvaluation$evaluationStatistics$Value <- as.character(result$performanceEvaluation$evaluationStatistics$Value)
      result$performanceEvaluation$evaluationStatistics$Value[result$performanceEvaluation$evaluationStatistics$Metric == 'populationSize'] <- nrow(prediction)
      result$performanceEvaluation$evaluationStatistics <- result$performanceEvaluation$evaluationStatistics[-grep('_',result$performanceEvaluation$evaluationStatistics$Metric),]
      result$performanceEvaluation$predictionDistribution$analysisId <- paste0('Analysis_',50+2*i)
      result$performanceEvaluation$thresholdSummary$analysisId <- paste0('Analysis_',50+2*i)
      
      
      result <- getSurvivialMetrics(plpResult = result, 
                                    recalibrate = F, 
                                    analysisId = paste0('Analysis_',50+2*i),
                                    model = 'combined')
      #=======================================
      
      
      
      if(!dir.exists(file.path(outputFolder,cdmDatabaseName))){
        dir.create(file.path(outputFolder,cdmDatabaseName))
      }
      ParallelLogger::logInfo("Saving results")
      PatientLevelPrediction::savePlpResult(result, file.path(outputFolder,cdmDatabaseName,paste0('Analysis_',50+2*i), 'plpResult'))
      ParallelLogger::logInfo(paste0("Results saved to:",file.path(outputFolder,cdmDatabaseName,paste0('Analysis_',50+2*i))))
      
    }
    
    newSet <- c(
      targetId = -1,
      targetName = 'Persons who are statin-risk eligible not censored at statin initiation',
      outcomeId = oids[i],
      outcomeName = unique(settings$outcomeName[settings$outcomeId==oids[i]]),
      model = 'combined',
      modelSettingsId = 1, 
      analysisId = paste0('Analysis_',50+2*i),
      cohortName = "Persons who are statin-risk eligible not censored at statin initiation",
      devDatabase = NA,
      valDatabase = unique(settings$valDatabase),
      modelSettingName = 'combined',
      populationSettingId = 1,
      covariateSettingId = 1
    )
    settings <- rbind(settings,newSet)
    
    
  }
  
  # save the new settings
  settings <- write.csv(settings,file.path(outputFolder,cdmDatabaseName, 'settings.csv'))
  
  return(T)
}