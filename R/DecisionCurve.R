# taken from the dca package @ http://www.danieldsjoberg.com/dca/articles/survival-outcomes.html

stdca <- function (data, outcome, ttoutcome, timepoint, predictors, xstart = 0.01, 
                   xstop = 0.99, xby = 0.01, ymin = -0.05, probability = NULL, 
                   harm = NULL, graph = TRUE, intervention = FALSE, interventionper = 100, 
                   smooth = FALSE, loess.span = 0.1, cmprsk = FALSE) 
{
  data = data[stats::complete.cases(data[c(outcome, ttoutcome, 
                                           predictors)]), c(outcome, ttoutcome, predictors)]
  if ((length(data[!(data[outcome] == 0 | data[outcome] == 
                     1), outcome]) > 0) & cmprsk == FALSE) {
    stop("outcome must be coded as 0 and 1")
  }
  if (class(data) != "data.frame") {
    stop("Input data must be class data.frame")
  }
  if (xstart < 0 | xstart > 1) {
    stop("xstart must lie between 0 and 1")
  }
  if (xstop < 0 | xstop > 1) {
    stop("xstop must lie between 0 and 1")
  }
  if (xby <= 0 | xby >= 1) {
    stop("xby must lie between 0 and 1")
  }
  if (xstart >= xstop) {
    stop("xstop must be larger than xstart")
  }
  pred.n = length(predictors)
  if (length(probability) > 0 & pred.n != length(probability)) {
    stop("Number of probabilities specified must be the same as the number of predictors being checked.")
  }
  if (length(harm) > 0 & pred.n != length(harm)) {
    stop("Number of harms specified must be the same as the number of predictors being checked.")
  }
  if (length(harm) == 0) {
    harm = rep(0, pred.n)
  }
  if (length(probability) == 0) {
    probability = rep(TRUE, pred.n)
  }
  if (length(predictors[predictors == "all" | predictors == 
                        "none"])) {
    stop("Prediction names cannot be equal to all or none.")
  }
  for (m in 1:pred.n) {
    if (probability[m] != TRUE & probability[m] != FALSE) {
      stop("Each element of probability vector must be TRUE or FALSE")
    }
    if (probability[m] == TRUE & (max(data[predictors[m]]) > 
                                  1 | min(data[predictors[m]]) < 0)) {
      stop(paste(predictors[m], "must be between 0 and 1 OR sepcified as a non-probability in the probability option", 
                 sep = " "))
    }
    if (probability[m] == FALSE) {
      model = NULL
      pred = NULL
      model = survival::coxph(survival::Surv(data.matrix(data[ttoutcome]), 
                                             data.matrix(data[outcome])) ~ data.matrix(data[predictors[m]]))
      surv.data = data.frame(0)
      pred = data.frame(1 - c(summary(survival::survfit(model, 
                                                        newdata = surv.data), time = timepoint)$surv))
      names(pred) = predictors[m]
      data = cbind(data[names(data) != predictors[m]], 
                   pred)
      print(paste(predictors[m], "converted to a probability with Cox regression. Due to linearity and proportional hazards assumption, miscalibration may occur.", 
                  sep = " "))
    }
  }
  N = dim(data)[1]
  if (cmprsk == FALSE) {
    km.cuminc = survival::survfit(survival::Surv(data.matrix(data[ttoutcome]), #fixed missing survival::
                                       data.matrix(data[outcome])) ~ 1)
    pd = 1 - summary(km.cuminc, times = timepoint)$surv
  }
  else {
    cr.cuminc = cmprsk::cuminc(data[[ttoutcome]], data[[outcome]])
    pd = cmprsk::timepoints(cr.cuminc, times = timepoint)$est[1]
  }
  nb = data.frame(seq(from = xstart, to = xstop, by = xby))
  names(nb) = "threshold"
  interv = nb
  error = NULL
  nb["all"] = pd - (1 - pd) * nb$threshold/(1 - nb$threshold)
  nb["none"] = 0
  for (m in 1:pred.n) {
    nb[predictors[m]] = NA
    for (t in 1:length(nb$threshold)) {
      px = sum(data[predictors[m]] > nb$threshold[t])/N
      if (px == 0) {
        error = rbind(error, paste(predictors[m], ": No observations with risk greater than ", 
                                   nb$threshold[t] * 100, "%", sep = ""))
        break
      }
      else {
        if (cmprsk == FALSE) {
          km.cuminc = survival::survfit(survival::Surv(data.matrix(data[data[predictors[m]] > 
                                                                          nb$threshold[t], ttoutcome]), data.matrix(data[data[predictors[m]] > 
                                                                                                                           nb$threshold[t], outcome])) ~ 1)
          pdgivenx = (1 - summary(km.cuminc, times = timepoint)$surv)
          if (length(pdgivenx) == 0) {
            error = rbind(error, paste(predictors[m], 
                                       ": No observations with risk greater than ", 
                                       nb$threshold[t] * 100, "% that have followup through the timepoint selected", 
                                       sep = ""))
            break
          }
        }
        else {
          cr.cuminc = cmprsk::cuminc(data[[ttoutcome]][data[[predictors[m]]] > 
                                                         nb$threshold[t]], data[[outcome]][data[[predictors[m]]] > 
                                                                                             nb$threshold[t]])
          pdgivenx = cmprsk::timepoints(cr.cuminc, times = timepoint)$est[1]
          if (is.na(pdgivenx)) {
            error = rbind(error, paste(predictors[m], 
                                       ": No observations with risk greater than ", 
                                       nb$threshold[t] * 100, "% that have followup through the timepoint selected", 
                                       sep = ""))
            break
          }
        }
        nb[t, predictors[m]] = pdgivenx * px - (1 - pdgivenx) * 
          px * nb$threshold[t]/(1 - nb$threshold[t]) - 
          harm[m]
      }
    }
    interv[predictors[m]] = (nb[predictors[m]] - nb["all"]) * 
      interventionper/(interv$threshold/(1 - interv$threshold))
  }
  if (length(error) > 0) {
    print(paste(error, ", and therefore net benefit not calculable in this range.", 
                sep = ""))
  }
  for (m in 1:pred.n) {
    if (smooth == TRUE) {
      lws = stats::loess(data.matrix(nb[!is.na(nb[[predictors[m]]]), 
                                        predictors[m]]) ~ data.matrix(nb[!is.na(nb[[predictors[m]]]), 
                                                                         "threshold"]), span = loess.span)
      nb[!is.na(nb[[predictors[m]]]), paste(predictors[m], 
                                            "_sm", sep = "")] = lws$fitted
      lws = stats::loess(data.matrix(interv[!is.na(nb[[predictors[m]]]), 
                                            predictors[m]]) ~ data.matrix(interv[!is.na(nb[[predictors[m]]]), 
                                                                                 "threshold"]), span = loess.span)
      interv[!is.na(nb[[predictors[m]]]), paste(predictors[m], 
                                                "_sm", sep = "")] = lws$fitted
    }
  }
  if (graph == TRUE) {
    if (intervention == TRUE) {
      legendlabel <- NULL
      legendcolor <- NULL
      legendwidth <- NULL
      legendpattern <- NULL
      ymax = max(interv[predictors], na.rm = TRUE)
      plot(x = nb$threshold, y = nb$all, type = "n", 
           xlim = c(xstart, xstop), ylim = c(ymin, ymax), 
           xlab = "Threshold probability", ylab = paste("Net reduction in interventions per", 
                                                        interventionper, "patients"))
      for (m in 1:pred.n) {
        if (smooth == TRUE) {
          lines(interv$threshold, data.matrix(interv[paste(predictors[m], 
                                                           "_sm", sep = "")]), col = m, 
                lty = 2)
        }
        else {
          lines(interv$threshold, data.matrix(interv[predictors[m]]), 
                col = m, lty = 2)
        }
        legendlabel <- c(legendlabel, predictors[m])
        legendcolor <- c(legendcolor, m)
        legendwidth <- c(legendwidth, 1)
        legendpattern <- c(legendpattern, 2)
      }
    }
    else {
      legendlabel <- c("None", "All")
      legendcolor <- c(17, 8)
      legendwidth <- c(2, 2)
      legendpattern <- c(1, 1)
      ymax = max(nb[names(nb) != "threshold"], na.rm = TRUE)
      graphics::plot(x = nb$threshold, y = nb$all, type = "l", 
                     col = 8, lwd = 2, xlim = c(xstart, xstop), ylim = c(ymin, 
                                                                         ymax), xlab = "Threshold probability", 
                     ylab = "Net benefit")
      graphics::lines(x = nb$threshold, y = nb$none, lwd = 2)
      for (m in 1:pred.n) {
        if (smooth == TRUE) {
          lines(nb$threshold, data.matrix(nb[paste(predictors[m], 
                                                   "_sm", sep = "")]), col = m, 
                lty = 2)
        }
        else {
          lines(nb$threshold, data.matrix(nb[predictors[m]]), 
                col = m, lty = 2)
        }
        legendlabel <- c(legendlabel, predictors[m])
        legendcolor <- c(legendcolor, m)
        legendwidth <- c(legendwidth, 1)
        legendpattern <- c(legendpattern, 2)
      }
    }
    graphics::legend("topright", legendlabel, cex = 0.8, 
                     col = legendcolor, lwd = legendwidth, lty = legendpattern)
  }
  results = list()
  results$N = N
  results$predictors = data.frame(cbind(predictors, harm, probability))
  names(results$predictors) = c("predictor", "harm.applied", 
                                "probability")
  results$interventions.avoided.per = interventionper
  results$net.benefit = nb
  results$interventions.avoided = interv
  return(results)
}