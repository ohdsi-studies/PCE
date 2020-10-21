getAnalyses <- function(settings, outputFolder,cdmDatabaseName){
  
  cohorts <- system.file("settings", 'CohortsToCreate.csv', package = "PCE")
  cohorts <- read.csv(cohorts)
  
  if(is.null(settings)){
    cohortsSettings <- cohorts[cohorts$type == 'target', c('cohortId','name')]
    outcomeIds <- cohorts$cohortId[cohorts$type == 'outcome']
    cohortsSettings <- do.call("rbind", rep(list(cohortsSettings), length(outcomeIds)))
    cohortsSettings$outcomeId <- outcomeIds
    cohortsSettings$outcomeName <- cohorts$name[cohorts$type == 'outcome']
    colnames(cohortsSettings) <- c('targetId', 'targetName', 'outcomeId', 'outcomeName')
    
    settingLoc <- system.file("settings", package = "PCE")
    modelSettings <- data.frame(model = dir(settingLoc, pattern = '_model.csv'))
    modelSettings$modelSettingsId <- 1:nrow(modelSettings)
    analysesSettings <- merge(cohortsSettings, modelSettings)
  } else{
    
    #use data.frame(tId, oId and model) to create...
    settings <- settings[, c('tId', 'oId', 'model')]
    colnames(settings) <- c('targetId','outcomeId','model')
    
    settings <- merge(settings, cohorts[,c('cohortId','name')], by.x='targetId', by.y='cohortId')
    colnames(settings)[colnames(settings) == 'name'] <- 'targetName'
    settings <- merge(settings, cohorts[,c('cohortId','name')], by.x='outcomeId', by.y='cohortId')
    colnames(settings)[colnames(settings) == 'name'] <- 'outcomeName'
    settings <- settings[,c('targetId', 'targetName', 'outcomeId', 'outcomeName','model')]
    settings$modelSettingsId <- as.double(as.factor(settings$model))
    analysesSettings <- settings
    
  }
  analysesSettings$analysisId <- paste0('Analysis_', 1:nrow(analysesSettings))
  
  # adding extras for shiny
  analysesSettings$cohortName <- analysesSettings$targetName
  analysesSettings$devDatabase <- 'NA'
  analysesSettings$valDatabase <- cdmDatabaseName
  analysesSettings$modelSettingName <- analysesSettings$model
  analysesSettings$populationSettingId <- 1
  analysesSettings$covariateSettingId <- analysesSettings$modelSettingsId
  
  if(!dir.exists(file.path(outputFolder,cdmDatabaseName))){
    dir.create(file.path(outputFolder,cdmDatabaseName))
  }
  write.csv(analysesSettings, file.path(outputFolder,cdmDatabaseName, 'settings.csv'))
  return(analysesSettings)
}

getData <- function(connectionDetails,
                    cdmDatabaseSchema,
                    cdmDatabaseName,
                    cohortDatabaseSchema,
                    cohortTable,
                    cohortId,
                    outcomeId,
                    oracleTempSchema,
                    model,
                    standardCovariates,
                    firstExposureOnly,
                    sampleSize,
                    cdmVersion){
  
  
  pathToCustom <- system.file("settings", model, package = "PCE")
  varsToCreate <- utils::read.csv(pathToCustom)

  covSets <- list()
  if(!is.null(standardCovariates)){
    extra <- 1
  } else{
    extra <- 0
    if(nrow(varsToCreate[varsToCreate$type == 'standardCovariate',])!=0){
      warning('Standard covariates used but not set')
    }
  }
  length(covSets) <- nrow(varsToCreate)+extra 
  
  if(!is.null(standardCovariates)){
    covSets[[1]] <- standardCovariates
  }
  
  cohortVarsToCreate <- varsToCreate[varsToCreate$type == 'cohortCovariate',]
  if(nrow(cohortVarsToCreate)>0){
  for(i in 1:nrow(cohortVarsToCreate)){
    covSets[[extra+i]] <- createCohortCovariateSettings(covariateName = as.character(cohortVarsToCreate$cohortName[i]),
                                                        analysisId = cohortVarsToCreate$analysisId[i],
                                                        covariateId = cohortVarsToCreate$cohortId[i]*1000+cohortVarsToCreate$analysisId[i],
                                                      cohortDatabaseSchema = cohortDatabaseSchema,
                                                      cohortTable = cohortTable,
                                                      cohortId = cohortVarsToCreate$atlasId[i],
                                                      startDay=cohortVarsToCreate$startDay[i], 
                                                      endDay=cohortVarsToCreate$endDay[i],
                                                      count= ifelse(is.null(cohortVarsToCreate$count), F, cohortVarsToCreate$count[i]), 
                                                      ageInteraction = ifelse(is.null(cohortVarsToCreate$ageInteraction), F, cohortVarsToCreate$ageInteraction[i]),
                                                      lnAgeInteraction = ifelse(is.null(cohortVarsToCreate$lnAgeInteraction), F, cohortVarsToCreate$lnAgeInteraction[i])
                                                      
    )
  }
  }
  
  # add measurement covariates...
  measurementVarsToCreate <- varsToCreate[varsToCreate$type == 'measurementCovariate',]
  if(nrow(measurementVarsToCreate)>0){
  for(i in 1:nrow(measurementVarsToCreate)){
    pathToConcept <- system.file("settings", paste0(measurementVarsToCreate$covariateName[i],'_concepts.csv'), package = "PCE")
    conceptSet <- read.csv(pathToConcept)$x
    pathToScaleMap <- system.file("settings", paste0(measurementVarsToCreate$covariateName[i],'_scaleMap.rds'), package = "PCE")
    scaleMap <- readRDS(pathToScaleMap)
    
    covSets[[extra+nrow(cohortVarsToCreate)+i]] <- createMeasurementCovariateSettings(covariateName = measurementVarsToCreate$covariateName[i], 
                                                                                      analysisId = measurementVarsToCreate$analysisId[i],
                                                                                      conceptSet = conceptSet,
                                                                                      startDay = measurementVarsToCreate$startDay[i], 
                                                                                      endDay = measurementVarsToCreate$endDay[i], 
                                                                                      scaleMap = scaleMap, 
                                                                                      aggregateMethod = measurementVarsToCreate$aggregateMethod[i],
                                                                                      imputationValue = measurementVarsToCreate$imputationValue[i],
                                                                                      covariateId = measurementVarsToCreate$covariateId[i],
                                                                                      ageInteraction = ifelse(is.null(measurementVarsToCreate$ageInteraction), F, measurementVarsToCreate$ageInteraction[i]),
                                                                                      
                                                                                      lnAgeInteraction = ifelse(is.null(measurementVarsToCreate$lnAgeInteraction), F, measurementVarsToCreate$lnAgeInteraction[i]),
                                                                                      lnValue = ifelse(is.null(measurementVarsToCreate$lnValue), F, measurementVarsToCreate$lnValue[i])
                                                                                      
                                                                                      )
  }
  }
  
  # add age covariates...
  ageVarsToCreate <- varsToCreate[varsToCreate$type == 'ageCovariate',]
  if(nrow(ageVarsToCreate)>0){
    for(i in 1:nrow(ageVarsToCreate)){
      
      pathToAgeMap <- system.file("settings", paste0(paste0(gsub(' ', '_',gsub('\\)','_',gsub('\\(','_',ageVarsToCreate$covariateName[i])))),'_ageMap.rds'), package = "PCE")
      ageMap <- readRDS(pathToAgeMap)
      
      covSets[[extra+nrow(cohortVarsToCreate) +nrow(measurementVarsToCreate) +i]] <- createAgeCovariateSettings(covariateName = ageVarsToCreate$covariateName[i], 
                                                                                        analysisId = ageVarsToCreate$analysisId[i],
                                                                                        ageMap = ageMap, 
                                                                                        covariateId = ageVarsToCreate$covariateId[i]
                                                                                        
      )
    }
  }
  
  
  # add measurement cohort covariates...
  measurementCohortVarsToCreate <- varsToCreate[grep('measurementCohortCovariate',varsToCreate$type),]
  if(nrow(measurementCohortVarsToCreate)>0){
    for(i in 1:nrow(measurementCohortVarsToCreate)){
      pathToConcept <- system.file("settings", paste0(measurementCohortVarsToCreate$covariateName[i],'_concepts.csv'), package = "PCE")
      conceptSet <- read.csv(pathToConcept)$x
      pathToScaleMap <- system.file("settings", paste0(measurementCohortVarsToCreate$covariateName[i],'_scaleMap.rds'), package = "PCE")
      scaleMap <- readRDS(pathToScaleMap)
      
      covSets[[extra+nrow(cohortVarsToCreate) + nrow(measurementVarsToCreate) + nrow(ageVarsToCreate) +i]] <- createMeasurementCohortCovariateSettings(covariateName = measurementCohortVarsToCreate$covariateName[i], 
                                                                                        analysisId = measurementCohortVarsToCreate$analysisId[i],
                                                                                        cohortDatabaseSchema = cohortDatabaseSchema,
                                                                                        cohortTable = cohortTable,
                                                                                        cohortId = measurementCohortVarsToCreate$atlasId[i],
                                                                                        type = ifelse(length(grep('_in', measurementCohortVarsToCreate$type[i]))>0, 'in', 'out'),
                                                                                        conceptSet = conceptSet,
                                                                                        startDay = measurementCohortVarsToCreate$startDay[i], 
                                                                                        endDay = measurementCohortVarsToCreate$endDay[i], 
                                                                                        scaleMap = scaleMap, 
                                                                                        aggregateMethod = measurementCohortVarsToCreate$aggregateMethod[i],
                                                                                        imputationValue = measurementCohortVarsToCreate$imputationValue[i],
                                                                                        covariateId = measurementCohortVarsToCreate$covariateId[i],
                                                                                        ageInteraction = ifelse(is.null(measurementCohortVarsToCreate$ageInteraction), F, measurementCohortVarsToCreate$ageInteraction[i]),
                                                                                        
                                                                                        lnAgeInteraction = ifelse(is.null(measurementCohortVarsToCreate$lnAgeInteraction), F, measurementCohortVarsToCreate$lnAgeInteraction[i]),
                                                                                        lnValue = ifelse(is.null(measurementCohortVarsToCreate$lnValue), F, measurementCohortVarsToCreate$lnValue[i])
                                                                                        
      )
    }
  }
  
  
  result <- PatientLevelPrediction::getPlpData(connectionDetails = connectionDetails,
                                     cdmDatabaseSchema = cdmDatabaseSchema,
                                     oracleTempSchema = oracleTempSchema, 
                                     cohortId = cohortId, 
                                     outcomeIds = outcomeId, 
                                     cohortDatabaseSchema = cohortDatabaseSchema, 
                                     outcomeDatabaseSchema = cohortDatabaseSchema, 
                                     cohortTable = cohortTable, 
                                     outcomeTable = cohortTable, 
                                     cdmVersion = cdmVersion, 
                                     firstExposureOnly = firstExposureOnly, 
                                     sampleSize =  sampleSize, 
                                     covariateSettings = covSets)
  
  return(result)
  
}


getModel <- function(model = 'SimpleModel'){
  
  pathToCustom <- system.file("settings", model, package = "PCE")
  coefficients <- utils::read.csv(pathToCustom)
  coefficients <- coefficients[,colnames(coefficients)%in%c('covariateId','points')]
 
   return(coefficients)
}

predictExisting <- function(model){
  
  coefficients <- getModel(model)
  mapping <- getMap(gsub('_model.csv','',model))
  
  predict <- function(plpData, population){
    
    plpData$covariateData$coefficients <- coefficients
    on.exit(plpData$covariateData$coefficients <- NULL, add = TRUE)
    
    prediction <- plpData$covariateData$covariates %>% 
      dplyr::inner_join(plpData$covariateData$coefficients, by= 'covariateId') %>% 
      dplyr::mutate(values = covariateValue*points) %>%
      dplyr::group_by(rowId) %>%
      dplyr::summarise(value = sum(values, na.rm = TRUE)) %>%
      dplyr::select(rowId, value) %>% dplyr::collect() 
    
    prediction <- merge(population, prediction, by ="rowId", all.x = TRUE)
    prediction$value[is.na(prediction$value)] <- 0
    
    # add any final mapping here (e.g., add intercept and mapping)
    prediction$value <- mapping(prediction$value)
    
    # make sure every value is less than 1 for the evaluatation
    scaleVal <- max(prediction$value)
    if(scaleVal>1){
      prediction$value <- prediction$value/scaleVal
    }
    
    attr(prediction, "metaData") <- list(predictionType = 'binary', scale = scaleVal)
    
    return(prediction)
  }
  
  return(predict)
}



getSurvivialMetrics <- function(plpResult, recalibrate, analysisId, model){
  
  prediction <- plpResult$prediction
  
  # if you havent recalibrated use the original values
  if(!recalibrate){
    if(model == "pooled_female_non_black_model.csv"){
      lp <- log(log(1-prediction$value)/log(0.9665))- 29.18
      plpResult$prediction$value2year <- 1- 0.99619^exp(lp+29.18)
      plpResult$prediction$value3year <- 1- 0.99325^exp(lp+29.18)
      plpResult$prediction$value5year <- 1- 0.98898^exp(lp+29.18)
      plpResult$prediction$value10year <- plpResult$prediction$value
      
    }else if(model == "pooled_male_non_black_model.csv"){
      lp <- log(log(1-prediction$value)/log(0.9144)) + 61.18
      plpResult$prediction$value2year <- 1- 0.98659^exp(lp-61.18)
      plpResult$prediction$value3year <- 1- 0.97954^exp(lp-61.18)
      plpResult$prediction$value5year <- 1- 0.96254^exp(lp-61.18)
      plpResult$prediction$value10year <- plpResult$prediction$value
      
    }else if(model == "pooled_female_black_model.csv"){
      lp <- log(log(1-prediction$value)/log(0.9533))+86.61
      plpResult$prediction$value2year <- 1- 0.99357^exp(lp-86.61)
      plpResult$prediction$value3year <- 1- 0.98935^exp(lp-86.61)
      plpResult$prediction$value5year <- 1- 0.98194^exp(lp-86.61)
      plpResult$prediction$value10year <- plpResult$prediction$value
    } else{
      lp <- log(log(1-prediction$value)/log(0.8954)) + 19.54
      plpResult$prediction$value2year <- 1- 0.98682^exp(lp- 19.54)
      plpResult$prediction$value3year <- 1- 0.97846^exp(lp- 19.54)
      plpResult$prediction$value5year <- 1- 0.95726^exp(lp- 19.54)
      plpResult$prediction$value10year <- plpResult$prediction$value
    }
  }
  
  
  #t <- plpResult$prediction$survivalTime
  t <- apply(cbind(plpResult$prediction$daysToCohortEnd, plpResult$prediction$survivalTime), 1, min)
  y <- ifelse(plpResult$prediction$outcomeCount > 0, 1, 0)
  

  # now to calculate the metrics T 2/3/5/10 year
  for(yrs in c(2,3,5,10)){
    t_temp <- t
    y_temp <- y
    y_temp[t_temp>365*yrs] <- 0
    t_temp[t_temp>365*yrs] <- 365*yrs
    S<- survival::Surv(t_temp, y_temp) 
    p <- plpResult$prediction[,paste0('value',yrs,'year')]
    
    # outcome count per time period
    
    out <- summary(survival::survfit(survival::Surv(t_temp, y_temp) ~ 1), times = 365*yrs)
    
    extras <- rbind(c(analysisId = analysisId,
                      "validation",paste0("O_",yrs),sum(y_temp)),
                    c(analysisId = analysisId,
                      "validation",paste0("survival_",yrs),1-out$surv)
                    )
    plpResult$performanceEvaluation$evaluationStatistics <- rbind(plpResult$performanceEvaluation$evaluationStatistics,extras)
    
    # concordance
    ### Observed survival function object (truncated at prediction_horizon)
    conc <- survival::concordance(S~p, reverse=TRUE)
    c.se<-sqrt(conc$var)
    extras <- rbind(c(analysisId = analysisId,
                      "validation",paste0("c-Statistic_",yrs),round(conc$concordance,5)),
                    c(analysisId = analysisId,
                      "validation",paste0("c-Statistic_l95CI_",yrs),round(conc$concordance+stats::qnorm(.025)*c.se,3)),
                    c(analysisId = analysisId,
                      "validation",paste0("c-Statistic_u95CI_",yrs),round(conc$concordance+stats::qnorm(.975)*c.se,3))
    )
    plpResult$performanceEvaluation$evaluationStatistics <- rbind(plpResult$performanceEvaluation$evaluationStatistics,extras)
  
  
    ### E-stats
    w<-rms::val.surv(est.surv=1-p,S=S,
                     u=365*yrs, 
                     fun=function(pr)log(-log(pr)))
    e.mean<-mean(abs(w$actual - w$p))
    e.90<-stats::quantile((abs(w$actual - w$p)),0.9)
    
    extras <- rbind(c(analysisId = analysisId,
                      "validation",paste0("E-statistic_",yrs),e.mean),
                    c(analysisId = analysisId,
                      "validation",paste0("E90-statistic_",yrs),e.90)
    )
    plpResult$performanceEvaluation$evaluationStatistics <- rbind(plpResult$performanceEvaluation$evaluationStatistics,extras)
    
  }
  
  # add in calibration for 10-year survival 
  S<- survival::Surv(t, y) 
  groups<-Hmisc::cut2(plpResult$prediction$value,g=100)
  n.groups<-length(levels(groups))
  pred<-tapply(plpResult$prediction$value,groups,mean)
  obs.q<-NULL
  obs.lower.q<-NULL
  obs.upper.q<-NULL
  for (q in 1:n.groups){
    KM<-survival::survfit(S ~ 1,sub=groups==levels(groups)[q])
    obs.q<-c(obs.q,max(1-KM$surv))  # maximum OK because of prediction_horizon
    obs.lower.q<-c(obs.lower.q,obs.lower<-max(1-KM$upper))
    obs.upper.q<-c(obs.upper.q,obs.upper<-max(1-KM$lower))
  }
  plpResult$performanceEvaluation$calibrationSummary$observedSurvival <- obs.q
  plpResult$performanceEvaluation$calibrationSummary$observedSurvivalLB <- obs.lower.q
  plpResult$performanceEvaluation$calibrationSummary$observedSurvivalUB <- obs.upper.q
  
  
  return(plpResult)
}
