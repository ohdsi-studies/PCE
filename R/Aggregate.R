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
    table1 <- NULL
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
        
        #table 1
        if(file.exists(file.path(outputFolder,cdmDatabaseName, aId, 'plpResult','table1.rds'))){
          tab1 <- readRDS(file.path(outputFolder,cdmDatabaseName, aId, 'plpResult','table1.rds'))
          
          if(!is.null(table1)){
            tab1 <- tab1 %>% dplyr::mutate(mean2 = mean, N2 = N, stdev2 = stdev) %>% dplyr::select(covariateName,mean2,N2,stdev2)
          table1 <- dplyr::full_join(table1, tab1, by = 'covariateName')
          table1[is.na(table1)] <- 0
          table1 <- table1 %>% dplyr::transmute(covariateName = covariateName,
                             mean = (mean*N +mean2*N2)/(N+N2),
                             stdev = sqrt(((N-1)*stdev^2 +(N2-1)*stdev2^2)/(N+N2-1) + N*N2*(mean-mean2)^2/((N+N2)*(N+N2-1))),
                             N = N+N2)
          } else{
            table1 <- tab1
          }
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
      result$performanceEvaluation$calibrationSummary$analysisId <- paste0('Analysis_',50+(2*i)-1)
      result$performanceEvaluation$demographicSummary$analysisId <- paste0('Analysis_',50+(2*i)-1)
      result$performanceEvaluation$evaluationStatistics <- as.data.frame(result$performanceEvaluation$evaluationStatistics)
      result$performanceEvaluation$evaluationStatistics$analysisId <- paste0('Analysis_',50+(2*i)-1)
      result$performanceEvaluation$evaluationStatistics$Value <- as.character(result$performanceEvaluation$evaluationStatistics$Value)
      result$performanceEvaluation$evaluationStatistics$Value[result$performanceEvaluation$evaluationStatistics$Metric == 'populationSize'] <- nrow(prediction)
      result$performanceEvaluation$evaluationStatistics <- result$performanceEvaluation$evaluationStatistics[-grep('_',result$performanceEvaluation$evaluationStatistics$Metric),]
      result$performanceEvaluation$predictionDistribution$analysisId <- paste0('Analysis_',50+(2*i)-1)
      result$performanceEvaluation$thresholdSummary$analysisId <- paste0('Analysis_',50+(2*i)-1)
      
      result <- getSurvivialMetrics(plpResult = result, 
                                    recalibrate = F, 
                                    analysisId = paste0('Analysis_',50+(2*i)-1),
                                    model = 'combined')
      #=======================================
      
      
      
      if(!dir.exists(file.path(outputFolder,cdmDatabaseName))){
        dir.create(file.path(outputFolder,cdmDatabaseName))
      }
      ParallelLogger::logInfo("Saving results")
      PatientLevelPrediction::savePlpResult(result, file.path(outputFolder,cdmDatabaseName,paste0('Analysis_',50+(2*i)-1), 'plpResult'))
      saveRDS(table1, file.path(outputFolder,cdmDatabaseName,paste0('Analysis_',50+(2*i)-1), 'plpResult','table1.rds'))
      ParallelLogger::logInfo(paste0("Results saved to:",file.path(outputFolder,cdmDatabaseName,paste0('Analysis_',50+(2*i)-1))))
      
    }
    
    
    newSet <- c(
      targetId = -1,
      targetName = 'Persons who are statin-risk eligible',
      outcomeId = as.double(oids[i]),
      outcomeName = unique(settings$outcomeName[settings$outcomeId==oids[i]]),
      model = 'combined',
      modelSettingsId = 1, 
      analysisId = paste0('Analysis_',50+(2*i)-1),
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
    table1 <- NULL
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
        
        #table 1
        if(file.exists(file.path(outputFolder,cdmDatabaseName, aId, 'plpResult','table1.rds'))){
          tab1 <- readRDS(file.path(outputFolder,cdmDatabaseName, aId, 'plpResult','table1.rds'))
          
          if(!is.null(table1)){
            tab1 <- tab1 %>% dplyr::mutate(mean2 = mean, N2 = N, stdev2 = stdev) %>% dplyr::select(covariateName,mean2,N2,stdev2)
            table1 <- dplyr::full_join(table1, tab1, by = 'covariateName')
            table1[is.na(table1)] <- 0
            table1 <- table1 %>% dplyr::transmute(covariateName = covariateName,
                                                  mean = (mean*N +mean2*N2)/(N+N2),
                                                  stdev = sqrt(((N-1)*stdev^2 +(N2-1)*stdev2^2)/(N+N2-1) + N*N2*(mean-mean2)^2/((N+N2)*(N+N2-1))),
                                                  N = N+N2)
          } else{
            table1 <- tab1
          }
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
      saveRDS(table1, file.path(outputFolder,cdmDatabaseName,paste0('Analysis_',50+2*i), 'plpResult','table1.rds'))
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