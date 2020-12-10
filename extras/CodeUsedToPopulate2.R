replaceName(packageLocation = getwd(), 
            packageName = 'PCE')

source("C:/Users/admin_jreps/Documents/SetBaseUrl.R")
#baseUrl <- Sys.getenv('baseUrl')

populatePackageCohorts(targetCohortIds = c(1325,1322,1326,1328,
                                           1358,1359,1360,1361),
                       targetCohortNames = c('Black Male Persons who are statin-risk eligible',
                                             'Black Female Persons who are statin-risk eligible',
                                             'Non-Black Male Persons who are statin-risk eligible',
                                             'Non-Black Female Persons who are statin-risk eligible',
                                             'Black Male Persons who are statin-risk eligible not censored at statin initiation',
                                             'Black Female Persons who are statin-risk eligible not censored at statin initiation',
                                             'Non-Black Male Persons who are statin-risk eligible not censored at statin initiation',
                                             'Non-Black Female Persons who are statin-risk eligible not censored at statin initiation'),
                       outcomeIds = c(1466,1357),#,1365,1366),
                       outcomeNames = c('first of AMI or ischemic stroke or death IP required',
                                        'first of AMI or ischemic stroke or death no IP required'
                                        #'first occurrence of atheroclerotic cardiovascular disease',
                                        #'first occurrence of atheroclerotic cardiovascular disease no IP required'
                                        ),
                       baseUrl = baseUrl)


# 1295 - hypertensive drugs
# 1286 - smoker
# 1362 - diabetes


populatePackageModels(modelname = 'pooled_male_non_black',
                      standardCovariates = NULL,
                      cohortCovariateSettings = list(baseUrl = baseUrl,
                                                     atlasCovariateIds = c(1362,1286,1286),
                                                     atlasCovariateNames = c('diabetes', 'smoking','smoking'),
                                                     analysisIds = c(456,456,455),
                                                     startDays = c(-9999,-730,-730),
                                                     endDays = c(-1,60,60),
                                                     points = c(0.658,7.837,-1.795),
                                                     count = rep(F, 3),
                                                     ageInteraction = c(F,F,F), 
                                                     lnAgeInteraction = c(F,F,T)) ,
                      
                      measurementCovariateSettings = list(names = c('Total_Cholesterol_mgdL', 'Total_Cholesterol_mgdL_age',
                                                                    'HDL-C_mgdL', 'HDL-C_mgdL_age'
                      ),
                      conceptSets = list(c(2212267,3015232,3019900,3027114,4008265,4190897,4198448,4260765,37393449,37397989,40484105,44791053,44809580),
                                         c(2212267,3015232,3019900,3027114,4008265,4190897,4198448,4260765,37393449,37397989,40484105,44791053,44809580),
                                         c(4076704,2212449,2212449,3003767,3007070,3007676,3011884,3013473,3015204,3016945,3022449,3023602,3023752,3024401,3030792,3032771,3033638,3034482,3040815,3053286,4005504,4008127,4011133,4019543,4041557,4041720,4042059,4042081,4055665,4076704,4101713,4195503,4198116,37208659,37208661,37392562,37392938,37394092,37394229,37394230,37398699,40757503,40765014,44789188,45768617,45768651,45768652,45768653,45768654,45771001,45772902),
                                         c(4076704,2212449,2212449,3003767,3007070,3007676,3011884,3013473,3015204,3016945,3022449,3023602,3023752,3024401,3030792,3032771,3033638,3034482,3040815,3053286,4005504,4008127,4011133,4019543,4041557,4041720,4042059,4042081,4055665,4076704,4101713,4195503,4198116,37208659,37208661,37392562,37392938,37394092,37394229,37394230,37398699,40757503,40765014,44789188,45768617,45768651,45768652,45768653,45768654,45771001,45772902)
                      ),
                      startDays = c(-1825, -1825,-1825, -1825),
                      endDays = c(0,0,0,0),
                      scaleMaps= list(function(x){ x = dplyr::mutate(x, rawValue = dplyr::case_when(unitConceptId == 8753 ~ rawValue*38.6, unitConceptId %in% c(8840,8954,9028 ) ~ rawValue, TRUE ~ 0)); x= dplyr::filter(x, rawValue >= 130 & rawValue <= 320 ); x = dplyr::mutate(x,valueAsNumber = log(rawValue)); return(x)},
                                      function(x){ x = dplyr::mutate(x, rawValue = dplyr::case_when(unitConceptId == 8753 ~ rawValue*38.6, unitConceptId %in% c(8840,8954,9028 ) ~ rawValue, TRUE ~ 0)); x= dplyr::filter(x, rawValue >= 130 & rawValue <= 320 ); x = dplyr::mutate(x,valueAsNumber = log(rawValue)*log(age)); return(x)},
                                      function(x){ x = dplyr::mutate(x, rawValue = dplyr::case_when(unitConceptId == 8753 ~ rawValue*38.6, unitConceptId %in% c(8840,8954,9028 ) ~ rawValue, TRUE ~ 0)); x= dplyr::filter(x, rawValue >= 20 & rawValue <= 100 ); x = dplyr::mutate(x,valueAsNumber = log(rawValue)); return(x)},
                                      function(x){ x = dplyr::mutate(x, rawValue = dplyr::case_when(unitConceptId == 8753 ~ rawValue*38.6, unitConceptId %in% c(8840,8954,9028 ) ~ rawValue, TRUE ~ 0)); x= dplyr::filter(x, rawValue >= 20 & rawValue <= 100 ); x = dplyr::mutate(x,valueAsNumber = log(rawValue)*log(age)); return(x)}), 
                      points = c(11.853,-2.664,-7.990,1.769),
                      aggregateMethods = c('recent','recent','recent', 'recent'),
                      imputationValues = c(150,150,50,50),
                      ageInteractions = c(F,F,F,F),
                      lnAgeInteractions = c(F,T,F,T),
                      lnValues = c(T,T,T,T),
                      measurementIds = c(1,2,3,4), 
                      analysisIds = c(457,457,457,457)
                      
                      
                      ),
                      
                      ageCovariateSettings = list(names = c('log(age)'),
                                                  ageMaps = list(function(x){return(log(x))}),
                                                  ageIds = 1,
                                                  analysisIds = c(458),
                                                  points = c(12.344)
                                                  
                      ),
                      
                      measurementCohortCovariateSettings = list(names = c('treated_Systolic_BP_mm_Hg','untreated_Systolic_BP_mm_Hg'),
                                                                atlasCovariateIds = c(1295,1295),
                                                                types = c('in', 'out'),
                                                                conceptSets = list(c(3004249,3009395,3018586,3028737,3035856,4152194,4153323,4161413,4197167,4217013,4232915,4248525,4292062,21492239,37396683,44789315,44806887,45769778),
                                                                                   c(3004249,3009395,3018586,3028737,3035856,4152194,4153323,4161413,4197167,4217013,4232915,4248525,4292062,21492239,37396683,44789315,44806887,45769778)
                                                                                   
                                                                ),
                                                                startDays = c(-365,-365),
                                                                endDays = c(0,0),
                                                                scaleMaps= list(function(x){ x = dplyr::filter(x, rawValue >= 90 & rawValue <= 200 ); return(x)},
                                                                                function(x){ x = dplyr::filter(x, rawValue >= 90 & rawValue <= 200 ); return(x)}
                                                                ),
                                                                points = c(1.797,1.764),
                                                                aggregateMethods = c('recent','recent'),
                                                                imputationValues = c(120,120),
                                                                ageInteractions = c(F,F),
                                                                lnAgeInteractions = c(F,F),
                                                                lnValues = c(T,T),
                                                                measurementIds = c(1,2), 
                                                                analysisIds = c(459,459),
                                                                baseUrl = baseUrl
                                                                
                                                                
                      ),
                      
                      finalMapping = function(x){ 1- 0.9144^exp(x-61.18)}
)



populatePackageModels(modelname = 'pooled_male_black',
                      standardCovariates = NULL,
                      cohortCovariateSettings = list(baseUrl = baseUrl,
                                                     atlasCovariateIds = c(1362,1286),
                                                     atlasCovariateNames = c('diabetes', 'smoking'),
                                                     analysisIds = c(456,456),
                                                     startDays = c(-9999,-730),
                                                     endDays = c(-1,60),
                                                     points = c(0.645,0.549),
                                                     count = rep(F, 2),
                                                     ageInteraction = c(F,F), 
                                                     lnAgeInteraction = c(F,F)) ,
                      
                      measurementCovariateSettings = list(names = c('Total_Cholesterol_mgdL',
                                                                    'HDL-C_mgdL_age'
                      ),
                      conceptSets = list(c(2212267,3015232,3019900,3027114,4008265,4190897,4198448,4260765,37393449,37397989,40484105,44791053,44809580),
                                         c(4076704,2212449,2212449,3003767,3007070,3007676,3011884,3013473,3015204,3016945,3022449,3023602,3023752,3024401,3030792,3032771,3033638,3034482,3040815,3053286,4005504,4008127,4011133,4019543,4041557,4041720,4042059,4042081,4055665,4076704,4101713,4195503,4198116,37208659,37208661,37392562,37392938,37394092,37394229,37394230,37398699,40757503,40765014,44789188,45768617,45768651,45768652,45768653,45768654,45771001,45772902)
                      ),
                      startDays = c(-1825, -1825),
                      endDays = c(0,0),
                      scaleMaps= list(function(x){ x = dplyr::mutate(x, rawValue = dplyr::case_when(unitConceptId == 8753 ~ rawValue*38.6, unitConceptId %in% c(8840,8954,9028 ) ~ rawValue, TRUE ~ 0)); x= dplyr::filter(x, rawValue >= 130 & rawValue <= 320 ); x = dplyr::mutate(x,valueAsNumber = log(rawValue)); return(x)},
                                      function(x){ x = dplyr::mutate(x, rawValue = dplyr::case_when(unitConceptId == 8753 ~ rawValue*38.6, unitConceptId %in% c(8840,8954,9028 ) ~ rawValue, TRUE ~ 0)); x= dplyr::filter(x, rawValue >= 20 & rawValue <= 100 ); x = dplyr::mutate(x,valueAsNumber = log(rawValue)*log(age)); return(x)}), 
                      points = c(0.302,-0.307),
                      aggregateMethods = c('recent','recent'),
                      imputationValues = c(150,50),
                      ageInteractions = c(F,F),
                      lnAgeInteractions = c(F,F),
                      lnValues = c(T,T),
                      measurementIds = c(1,2), 
                      analysisIds = c(457,457)
                      
                      
                      ),
                      
                      ageCovariateSettings = list(names = c('log(age)'),
                                                  ageMaps = list(function(x){return(log(x))}),
                                                  ageIds = 1,
                                                  analysisIds = c(458),
                                                  points = c(2.469)
                                                  
                      ),
                      
                      measurementCohortCovariateSettings = list(names = c('treated_Systolic_BP_mm_Hg','untreated_Systolic_BP_mm_Hg'),
                                                                atlasCovariateIds = c(1295,1295),
                                                                types = c('in', 'out'),
                                                                conceptSets = list(c(3004249,3009395,3018586,3028737,3035856,4152194,4153323,4161413,4197167,4217013,4232915,4248525,4292062,21492239,37396683,44789315,44806887,45769778),
                                                                                   c(3004249,3009395,3018586,3028737,3035856,4152194,4153323,4161413,4197167,4217013,4232915,4248525,4292062,21492239,37396683,44789315,44806887,45769778)
                                                                                   
                                                                ),
                                                                startDays = c(-365,-365),
                                                                endDays = c(0,0),
                                                                scaleMaps= list(function(x){ x = dplyr::filter(x, rawValue >= 90 & rawValue <= 200 ); return(x)},
                                                                                function(x){ x = dplyr::filter(x, rawValue >= 90 & rawValue <= 200 ); return(x)}
                                                                ),
                                                                points = c(1.916,1.809),
                                                                aggregateMethods = c('recent','recent'),
                                                                imputationValues = c(120,120),
                                                                ageInteractions = c(F,F),
                                                                lnAgeInteractions = c(F,F),
                                                                lnValues = c(T,T),
                                                                measurementIds = c(1,2), 
                                                                analysisIds = c(459,459),
                                                                baseUrl = baseUrl
                                                                
                                                                
                      ),
                      
                      finalMapping = function(x){ 1- 0.8954^exp(x-19.54)}
)



populatePackageModels(modelname = 'pooled_female_non_black',
                      standardCovariates = NULL,
                      cohortCovariateSettings = list(baseUrl = baseUrl,
                                                     atlasCovariateIds = c(1362,1286 ,1286 ),
                                                     atlasCovariateNames = c('diabetes', 'smoking','smoking'),
                                                     analysisIds = c(456,456,455),
                                                     startDays = c(-9999,-730,-730),
                                                     endDays = c(-1,60,60),
                                                     points = c(0.661,7.574,-1.665),
                                                     count = rep(F, 3),
                                                     ageInteraction = c(F,F,F), 
                                                     lnAgeInteraction = c(F,F,T)) ,
                      
                      measurementCovariateSettings = list(names = c('Total_Cholesterol_mgdL', 'Total_Cholesterol_mgdL_age',
                                                                    'HDL-C_mgdL', 'HDL-C_mgdL_age'
                      ),
                      conceptSets = list(c(2212267,3015232,3019900,3027114,4008265,4190897,4198448,4260765,37393449,37397989,40484105,44791053,44809580),
                                         c(2212267,3015232,3019900,3027114,4008265,4190897,4198448,4260765,37393449,37397989,40484105,44791053,44809580),
                                         c(4076704,2212449,2212449,3003767,3007070,3007676,3011884,3013473,3015204,3016945,3022449,3023602,3023752,3024401,3030792,3032771,3033638,3034482,3040815,3053286,4005504,4008127,4011133,4019543,4041557,4041720,4042059,4042081,4055665,4076704,4101713,4195503,4198116,37208659,37208661,37392562,37392938,37394092,37394229,37394230,37398699,40757503,40765014,44789188,45768617,45768651,45768652,45768653,45768654,45771001,45772902),
                                         c(4076704,2212449,2212449,3003767,3007070,3007676,3011884,3013473,3015204,3016945,3022449,3023602,3023752,3024401,3030792,3032771,3033638,3034482,3040815,3053286,4005504,4008127,4011133,4019543,4041557,4041720,4042059,4042081,4055665,4076704,4101713,4195503,4198116,37208659,37208661,37392562,37392938,37394092,37394229,37394230,37398699,40757503,40765014,44789188,45768617,45768651,45768652,45768653,45768654,45771001,45772902)
                      ),
                      startDays = c(-1825, -1825,-1825, -1825),
                      endDays = c(0,0,0,0),
                      scaleMaps= list(function(x){ x = dplyr::mutate(x, rawValue = dplyr::case_when(unitConceptId == 8753 ~ rawValue*38.6, unitConceptId %in% c(8840,8954,9028 ) ~ rawValue, TRUE ~ 0)); x= dplyr::filter(x, rawValue >= 130 & rawValue <= 320 ); x = dplyr::mutate(x,valueAsNumber = log(rawValue)); return(x)},
                                      function(x){ x = dplyr::mutate(x, rawValue = dplyr::case_when(unitConceptId == 8753 ~ rawValue*38.6, unitConceptId %in% c(8840,8954,9028 ) ~ rawValue, TRUE ~ 0)); x= dplyr::filter(x, rawValue >= 130 & rawValue <= 320 ); x = dplyr::mutate(x,valueAsNumber = log(rawValue)*log(age)); return(x)},
                                      function(x){ x = dplyr::mutate(x, rawValue = dplyr::case_when(unitConceptId == 8753 ~ rawValue*38.6, unitConceptId %in% c(8840,8954,9028 ) ~ rawValue, TRUE ~ 0)); x= dplyr::filter(x, rawValue >= 20 & rawValue <= 100 ); x = dplyr::mutate(x,valueAsNumber = log(rawValue)); return(x)},
                                      function(x){ x = dplyr::mutate(x, rawValue = dplyr::case_when(unitConceptId == 8753 ~ rawValue*38.6, unitConceptId %in% c(8840,8954,9028 ) ~ rawValue, TRUE ~ 0)); x= dplyr::filter(x, rawValue >= 20 & rawValue <= 100 ); x = dplyr::mutate(x,valueAsNumber = log(rawValue)*log(age)); return(x)}), 
                      points = c(13.540,-3.114,-13.578,3.149),
                      aggregateMethods = c('recent','recent','recent', 'recent'),
                      imputationValues = c(150,150,50,50),
                      ageInteractions = c(F,F,F,F),
                      lnAgeInteractions = c(F,T,F,T),
                      lnValues = c(T,T,T,T),
                      measurementIds = c(1,2,3,4), 
                      analysisIds = c(457,457,457,457)
                      
                      
                      ),
                      
                      ageCovariateSettings = list(names = c('log(age)', 'log(age)_squared'),
                                                  ageMaps = list(function(x){return(log(x))},
                                                                 function(x){return(log(x)^2)}),
                                                  ageIds = c(1,2),
                                                  analysisIds = c(458,458),
                                                  points = c(-29.799,4.884)
                                                  
                      ),
                      
                      measurementCohortCovariateSettings = list(names = c('treated_Systolic_BP_mm_Hg','untreated_Systolic_BP_mm_Hg'),
                                                                atlasCovariateIds = c(1295,1295),
                                                                types = c('in', 'out'),
                                                                conceptSets = list(c(3004249,3009395,3018586,3028737,3035856,4152194,4153323,4161413,4197167,4217013,4232915,4248525,4292062,21492239,37396683,44789315,44806887,45769778),
                                                                                   c(3004249,3009395,3018586,3028737,3035856,4152194,4153323,4161413,4197167,4217013,4232915,4248525,4292062,21492239,37396683,44789315,44806887,45769778)
                                                                                   
                                                                ),
                                                                startDays = c(-365,-365),
                                                                endDays = c(0,0),
                                                                scaleMaps= list(function(x){ x = dplyr::filter(x, rawValue >= 90 & rawValue <= 200 ); return(x)},
                                                                                function(x){ x = dplyr::filter(x, rawValue >= 90 & rawValue <= 200 ); return(x)}
                                                                ),
                                                                points = c(2.019,1.957),
                                                                aggregateMethods = c('recent','recent'),
                                                                imputationValues = c(120,120),
                                                                ageInteractions = c(F,F),
                                                                lnAgeInteractions = c(F,F),
                                                                lnValues = c(T,T),
                                                                measurementIds = c(1,2), 
                                                                analysisIds = c(459,459),
                                                                baseUrl = baseUrl
                                                                
                                                                
                      ),
                      
                      finalMapping = function(x){ 1- 0.9665^exp(x+29.18)}
)



populatePackageModels(modelname = 'pooled_female_black',
                      standardCovariates = NULL,
                      cohortCovariateSettings = list(baseUrl = baseUrl,
                                                     atlasCovariateIds = c(1362,1286 ),
                                                     atlasCovariateNames = c('diabetes', 'smoking'),
                                                     analysisIds = c(456,456),
                                                     startDays = c(-9999,-730),
                                                     endDays = c(-1,60),
                                                     points = c(0.874,0.691),
                                                     count = rep(F, 2),
                                                     ageInteraction = c(F,F), 
                                                     lnAgeInteraction = c(F,F)) ,
                      
                      measurementCovariateSettings = list(names = c('Total_Cholesterol_mgdL',
                                                                    'HDL-C_mgdL', 'HDL-C_mgdL_age'
                      ),
                      conceptSets = list(c(2212267,3015232,3019900,3027114,4008265,4190897,4198448,4260765,37393449,37397989,40484105,44791053,44809580),
                                         c(4076704,2212449,2212449,3003767,3007070,3007676,3011884,3013473,3015204,3016945,3022449,3023602,3023752,3024401,3030792,3032771,3033638,3034482,3040815,3053286,4005504,4008127,4011133,4019543,4041557,4041720,4042059,4042081,4055665,4076704,4101713,4195503,4198116,37208659,37208661,37392562,37392938,37394092,37394229,37394230,37398699,40757503,40765014,44789188,45768617,45768651,45768652,45768653,45768654,45771001,45772902),
                                         c(4076704,2212449,2212449,3003767,3007070,3007676,3011884,3013473,3015204,3016945,3022449,3023602,3023752,3024401,3030792,3032771,3033638,3034482,3040815,3053286,4005504,4008127,4011133,4019543,4041557,4041720,4042059,4042081,4055665,4076704,4101713,4195503,4198116,37208659,37208661,37392562,37392938,37394092,37394229,37394230,37398699,40757503,40765014,44789188,45768617,45768651,45768652,45768653,45768654,45771001,45772902)
                      ),
                      startDays = c(-1825,-1825, -1825),
                      endDays = c(0,0,0),
                      scaleMaps= list(function(x){ x = dplyr::mutate(x, rawValue = dplyr::case_when(unitConceptId == 8753 ~ rawValue*38.6, unitConceptId %in% c(8840,8954,9028 ) ~ rawValue, TRUE ~ 0)); x= dplyr::filter(x, rawValue >= 130 & rawValue <= 320 ); x = dplyr::mutate(x,valueAsNumber = log(rawValue)); return(x)},
                                      function(x){ x = dplyr::mutate(x, rawValue = dplyr::case_when(unitConceptId == 8753 ~ rawValue*38.6, unitConceptId %in% c(8840,8954,9028 ) ~ rawValue, TRUE ~ 0)); x= dplyr::filter(x, rawValue >= 20 & rawValue <= 100 ); x = dplyr::mutate(x,valueAsNumber = log(rawValue)); return(x)},
                                      function(x){ x = dplyr::mutate(x, rawValue = dplyr::case_when(unitConceptId == 8753 ~ rawValue*38.6, unitConceptId %in% c(8840,8954,9028 ) ~ rawValue, TRUE ~ 0)); x= dplyr::filter(x, rawValue >= 20 & rawValue <= 100 ); x = dplyr::mutate(x,valueAsNumber = log(rawValue)*log(age)); return(x)}), 
                      points = c(0.940,-18.920,4.475),
                      aggregateMethods = c('recent','recent','recent'),
                      imputationValues = c(150,50,50),
                      ageInteractions = c(F,F,F),
                      lnAgeInteractions = c(F,F,T),
                      lnValues = c(T,T,T),
                      measurementIds = c(1,2,3), 
                      analysisIds = c(457,457,457)
                      
                      
                      ),
                      
                      ageCovariateSettings = list(names = c('log(age)'),
                                                  ageMaps = list(function(x){return(log(x))}),
                                                  ageIds = c(1),
                                                  analysisIds = c(458),
                                                  points = c(17.114)
                                                  
                      ),
                      
                      measurementCohortCovariateSettings = list(names = c('treated_Systolic_BP_mm_Hg','untreated_Systolic_BP_mm_Hg',
                                                                          'treated_Systolic_BP_mm_Hg','untreated_Systolic_BP_mm_Hg'),
                                                                atlasCovariateIds = c(1295,1295,1295,1295),
                                                                types = c('in', 'out'),
                                                                conceptSets = list(c(3004249,3009395,3018586,3028737,3035856,4152194,4153323,4161413,4197167,4217013,4232915,4248525,4292062,21492239,37396683,44789315,44806887,45769778),
                                                                                   c(3004249,3009395,3018586,3028737,3035856,4152194,4153323,4161413,4197167,4217013,4232915,4248525,4292062,21492239,37396683,44789315,44806887,45769778),
                                                                                   c(3004249,3009395,3018586,3028737,3035856,4152194,4153323,4161413,4197167,4217013,4232915,4248525,4292062,21492239,37396683,44789315,44806887,45769778),
                                                                                   c(3004249,3009395,3018586,3028737,3035856,4152194,4153323,4161413,4197167,4217013,4232915,4248525,4292062,21492239,37396683,44789315,44806887,45769778)
                                                                                   
                                                                                   
                                                                ),
                                                                startDays = c(-365,-365,-365,-365),
                                                                endDays = c(0,0,0,0),
                                                                scaleMaps= list(function(x){ x = dplyr::filter(x, rawValue >= 90 & rawValue <= 200 ); return(x)},
                                                                                function(x){ x = dplyr::filter(x, rawValue >= 90 & rawValue <= 200 ); return(x)},
                                                                                function(x){ x = dplyr::filter(x, rawValue >= 90 & rawValue <= 200 ); return(x)},
                                                                                function(x){ x = dplyr::filter(x, rawValue >= 90 & rawValue <= 200 ); return(x)}
                                                                ),
                                                                points = c(29.291,27.820,-6.432,-6.087),
                                                                aggregateMethods = c('recent','recent','recent','recent'),
                                                                imputationValues = c(120,120,120,120),
                                                                ageInteractions = c(F,F,F,F),
                                                                lnAgeInteractions = c(F,F,T,T),
                                                                lnValues = c(T,T,T,T),
                                                                measurementIds = c(1,2,3,4), 
                                                                analysisIds = c(459,459,459,459),
                                                                baseUrl = baseUrl
                                                                
                                                                
                      ),
                      
                      finalMapping = function(x){ 1- 0.9533^exp(x-86.61)}
)
