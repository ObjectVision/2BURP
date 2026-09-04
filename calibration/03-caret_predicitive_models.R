####### Create predictive models in R with Caret########
# Adapted by: Pendula Ferdinand
# Further adapted by: Thijmen van der Wielen
# Last edited: 26.11.2024 by Thijmen

# load libraries
library(pacman)
p_load(
  caret,
  boot,
  MLmetrics,
  devtools,
  autoGLM,
  data.table,
  vroom,
  doParallel,
  tidyverse,
  varhandle,
  stargazer
)

print("Starting predictive models")

####################### Functions ######################
#' concatenate function for when you start to write in python and realize this is R.
#'
concat <- function(...) {
  paste(..., sep = "")
}


#' applies as.numeric column-wise to return a completely numeric matrix.
#'
#' @param mat matrix or data frame
#' @keywords data management
#' @export
#' @examples
#' as.numeric.matrix(data.frame(x))
as.numeric.matrix <- function(mat) {
  F <- function(x) {
    as.numeric(x)
  }
  return(apply(mat, 2, F))
}


#' Validation Summary function that calculates balanced loss metrics, suitable for Caret
#'
#' @param data Caret data object
#' @param lev Caret lev object
#' @param model Caret model object
#' @keywords validation, caret
#' @export
#' @examples
#' mod <- train(Class ~ .,
#'   data = subsetdat,
#'   method = "multinom",
#'   tuneLength = 5,
#'   metric = "BalancedLogLoss50",
#'   maximize = FALSE, #<--- minimize loss
#'   trControl = trainControl(
#'     summaryFunction = balancedSummary,
#'     classProbs = TRUE,
#'     method = "repeatedcv", number = folds, repeats = repeats
#'   )
#' )
#'
balancedSummary <- function(data, lev = NULL, model = NULL) {
  unfactor <- function(f) {
    if (is.factor(f)) {
      as.numeric(levels(f))[f]
    } else {
      f
    }
  }
  # Helper function that returns you wheter a variable is binary, or strictly in the sense that both 0 and 1 occur.
  is.binary <- function(x, na.rm = TRUE, strict = FALSE) {
    if (na.rm) {
      x <- x[!is.na(x)]
    }
    unique <- unique(x)
    if (!is.numeric(x) || any(is.na(x))) {
      return(FALSE)
    }
    if (!strict && length(unique) == 1) {
      return(unique == 0 || unique == 1)
    } else {
      return(!(any(as.integer(unique) != unique) || length(unique) >
        2 || min(x) != 0 || max(x) != 1))
    }
  }

  # False Negative Rate, all incorrect negative predictions as a fraction of all positives
  FNR <- function(y_true, y_pred) {
    y_true <- unfactor(y_true)
    y_pred <- unfactor(y_pred)
    if (!is.binary(y_pred) || !is.binary(y_true)) {
      warning("One of the supplied vectors is not a binary")
      return(NA)
    }
    return((y_true %*% (1 - y_pred)) / (y_true %*% y_true)) # (t(y_true)%*%(1- y_pred))/(t(y_true)%*%y_true)
  }

  # False Positive Rate, all incorrect positive predictions as a fraction of all negatives
  FPR <- function(y_true, y_pred) {
    y_true <- unfactor(y_true)
    y_pred <- unfactor(y_pred)
    if (!is.binary(y_pred) || !is.binary(y_true)) {
      warning("One of the supplied vectors is not a binary")
      return(NA)
    }
    return((((1 - y_true) %*% y_pred) / ((1 - y_true) %*% (1 - y_true))))
  }

  # Loss function La,  balanced error rates with weight w
  La <- function(y_true, y_pred, w = 0.5) {
    if (abs(w) > 1) {
      stop("w should be in [0,1]")
    }
    L_a <- w * FNR(y_true, y_pred) + (1 - w) * FPR(y_true, y_pred)
    return(L_a)
  }
  La2 <- function(y_true, y_pred, w = 0.5) {
    if (abs(w) > 1) {
      stop("w should be in [0,1]")
    }
    L_a <- sqrt(w * FNR(y_true, y_pred)^2 + (1 - w) * FPR(y_true, y_pred)^2)
    return(L_a)
  }
  # Loss function Lb, balanced LogLoss with class weight w
  # (w=0.5 equals balanced Log Loss, which equals standard Log Loss for a balanced outcome variable)
  Lb <- function(y_true, p_pred, w = 0.5) {
    y_true <- unfactor(y_true)
    if (abs(w) > 1) {
      stop("w should be in [0,1]")
    }
    if (0 %in% p_pred || 1 %in% p_pred) {
      warning("Probabilities equal to 0 or 1 occur")
      eps <- 1e-15
      p_pred <- pmax(pmin(p_pred, 1 - eps), eps)
    }
    # balanced by first calculating the class averages and taking a weighted average
    L_b <- -(w * ((y_true %*% log(p_pred)) / (y_true %*% y_true)) + (1 - w) * (((1 - y_true) %*% log(1 - p_pred)) / ((1 - y_true) %*% (1 - y_true))))

    # standard log loss (average loss per prediction)
    #     L_b <- - (y_true%*%log(p_pred)  +  (1-y_true)%*%log(1-p_pred) ) /(y_true%*%y_true+(1-y_true)%*%(1-y_true))
    return(L_b)
  }

  Accuracy <- function(y_pred, y_true) {
    Accuracy <- mean(y_true == y_pred)
    return(Accuracy)
  }

  lvls <- levels(data$obs)
  if (length(lvls) > 2) {} # some code that takes multiclass and converts to the relevant binary

  if (!all(levels(data[, "pred"]) == lvls)) {
    stop("levels of observed and predicted data do not match")
  }

  dataComplete <- data[complete.cases(data), ]
  probs <- as.matrix(dataComplete[, lev, drop = FALSE])

  p_pred <- dataComplete[, lev[2]] # as.matrix(dataComplete[, lev, drop = FALSE])[,1]#data[, lvls[1]]
  y_pred <- round(p_pred)
  y_true <- ifelse(dataComplete$obs == lev[1], 0, 1)

  fpr <- try(FPR(y_pred = y_pred, y_true = y_true), silent = TRUE)
  fnr <- try(FNR(y_pred = y_pred, y_true = y_true), silent = TRUE)
  BalancedErrorRate <- try(La(y_pred = y_pred, y_true = y_true), silent = TRUE)
  # wBalancedErrorRate = try(La(y_pred = y_pred, y_true = y_true, w=2/3), silent = TRUE)
  # wBalancedErrorRate2 = try(La(y_pred = y_pred, y_true = y_true, w=1/3), silent = TRUE)

  BalancedLogLoss <- try(Lb(y_true = y_true, p_pred = p_pred), silent = TRUE)
  # wBalancedLogLoss = try(Lb(y_true=y_true, p_pred=p_pred, w=2/3), silent = TRUE)
  # wBalancedLogLoss2 = try(Lb(y_true=y_true, p_pred=p_pred, w=1/3), silent = TRUE)

  logLoss <- try(MLmetrics::LogLoss(y_pred = p_pred, y_true = y_true), silent = TRUE)
  accuracy <- try(Accuracy(y_pred = y_pred, y_true = y_true), silent = TRUE)

  # f_y_true<-factor(y_true, levels=c(0,1)) # force assign both levels to ensure table overlap.
  # f_y_pred<-factor(y_pred, levels=c(0,1)) # not all standard llibraries do this!

  # confmat= confusionMatrix(f_y_pred, reference=f_y_true, positive="1")
  # False Positive Rate = 1 - True Positive Rate
  # spec_fpr = as.numeric(1-confmat$byClass["Specificity"])
  # False Negative Rate = 1 - True Negative Rate
  # sens_fnr = as.numeric(1-confmat$byClass["Sensitivity"])
  # or calculate from frequencies
  # conf_freq= try(data.frame(confmat$table)[,"Freq"]
  # conf_fpr = try(conf_freq[2]/sum(conf_freq[1:2])
  # conf_fnr = try(conf_freq[3]/sum(conf_freq[3:4])

  if (inherits(fpr, "try-error")) {
    fpr <- NA
  }
  if (inherits(fnr, "try-error")) {
    fnr <- NA
  }
  if (inherits(BalancedErrorRate, "try-error")) {
    BalancedErrorRate <- NA
  }
  # if (inherits(wBalancedErrorRate, "try-error")){wBalancedErrorRate<-NA}
  # if (inherits(wBalancedErrorRate2, "try-error")){wBalancedErrorRate2<-NA}
  if (inherits(BalancedLogLoss, "try-error")) {
    BalancedLogLoss <- NA
  }
  # if (inherits(wBalancedLogLoss, "try-error")){wBalancedLogLoss<-NA}
  # if (inherits(wBalancedLogLoss2, "try-error")){wBalancedLogLoss2<-NA}
  if (inherits(logLoss, "try-error")) {
    logLoss <- NA
  }
  if (inherits(accuracy, "try-error")) {
    accuracy <- NA
  }
  return(
    c(
      FNR = fnr,
      FPR = fpr,
      # BalancedErrorRate33 = wBalancedErrorRate2,
      # BalancedErrorRate50 = BalancedErrorRate,
      # BalancedErrorRate67 = wBalancedErrorRate,
      # BalancedLogLoss33 = wBalancedLogLoss2,
      BalancedLogLoss50 = BalancedLogLoss,
      # BalancedLogLoss67 = wBalancedLogLoss,
      Error = 1 - accuracy,
      LogLoss = logLoss
    )
  )
}

#' Balanced Log Loss
#'
#' @param y_true binary validation sample
#' @param p_pred continuous predictions
#' @param w class weight (0.5 is equal weight on both classes)
#' @keywords validation, caret
#' @export
#' @examples
#' mod <- train(Class ~ .,
#'   data = subsetdat,
#'   method = "multinom",
#'   tuneLength = 5,
#'   metric = "BalancedLogLoss50",
#'   maximize = FALSE, #<--- minimize loss
#'   trControl = trainControl(
#'     summaryFunction = balancedSummary,
#'     classProbs = TRUE,
#'     method = "repeatedcv", number = folds, repeats = repeats
#'   )
#' )
#'
Lb <- function(y_true, p_pred, w = 0.5) {
  y_true <- unfactor(y_true)
  if (abs(w) > 1) {
    stop("w should be in [0,1]")
  }
  if (0 %in% p_pred || 1 %in% p_pred) {
    warning("Probabilities equal to 0 or 1 occur")
    eps <- 1e-15
    p_pred <- pmax(pmin(p_pred, 1 - eps), eps)
  }
  # balanced by first calculating the class averages and taking a weighted average
  L_b <- -(w * ((y_true %*% log(p_pred)) / (y_true %*% y_true)) + (1 - w) * (((1 - y_true) %*% log(1 - p_pred)) / ((1 - y_true) %*% (1 - y_true))))

  # standard log loss (average loss per prediction)
  #     L_b <- - (y_true%*%log(p_pred)  +  (1-y_true)%*%log(1-p_pred) ) /(y_true%*%y_true+(1-y_true)%*%(1-y_true))
  return(L_b)
}



#' caret model object to fit penalized logistic regressions and probability balancing.
#' Allows tuning over alpha / beta as defined in the paper as well as lambda penalty, see glmnet.
#' Allows seamless integration with other caret functionality.
balanced_glmnet_new <- balanced_glmnet_new0 <- getModelInfo("glmnet", regex = FALSE)[[1]]
# This is Generalized:
balanced_glmnet_new$type <- c("Classification") # reset, only classification implemented. Could do hypertuning of the constant in regression though.
## Add the Constant as another tuning parameter
balanced_glmnet_new$parameters <- data.frame(
  parameter = c(as.character(balanced_glmnet_new0$parameters$parameter), "s", "Constant"), # ...2
  class = c(as.character(balanced_glmnet_new0$parameters$class), "numeric", "numeric"),
  label = c(as.character(balanced_glmnet_new0$parameters$label), "Probability Scaling", "Probability Constant")
)

balanced_glmnet_new$grid <- function(x, y, len = NULL, search = "grid") {
  default_grid <- expand.grid(alpha = 1, lambda = c(0, 10^seq(-1,
    -4,
    length = len - 1
  )))
  # maxcut = min(((table(y)/sum(table(y)))[2]+0.05), 1)*100
  crange <- (1:40) / 40 # (1:maxcut)/100#2
  srange <- (1:5) / 2
  grid <- data.frame(default_grid, s = expand.grid(data.frame(default_grid)[
    ,
    1
  ], s = srange, Constant = crange)[, "s"], Constant = expand.grid(data.frame(default_grid)[
    ,
    1
  ], s = srange, Constant = crange)[, "Constant"])
  grid
}

balanced_glmnet_new$loop <- function(grid) {
  library(plyr)
  nhypers <- ncol(grid) - 2
  hypernames <- colnames(grid)[1:nhypers]
  if (nhypers > 1) {
    gen.unique.models <- function(x) {
      paste(as.character(x), collapse = "")
    }
    unique.models <- apply(grid[, hypernames], 1, gen.unique.models)
  } else {
    unique.models <- grid[, hypernames]
  }
  loopgrid <- grid
  loopgrid$unique.models <- unique.models
  loop <- loopgrid[loopgrid$unique.models == unique.models, ][loopgrid[loopgrid$unique.models == unique.models, ]$Constant ==
    max(loopgrid$Constant) & loopgrid[loopgrid$unique.models ==
    unique.models, ]$s == max(loopgrid$s), ]
  submodels <- vector(mode = "list", length = nrow(loop))
  for (i in seq(along = loop$Constant)) {
    index <- which(loopgrid$unique.models == loopgrid$unique.models[i])
    constants <- grid[index, "Constant"]
    scales <- grid[index, "s"]
    submodels[[i]] <- data.frame(s = scales[paste0(
      constants,
      ".", scales
    ) != paste0(
      loop$Constant[i], ".",
      loop$s[i]
    )], Constant = constants[paste0(
      constants,
      ".", scales
    ) != paste0(
      loop$Constant[i], ".",
      loop$s[i]
    )])
  }
  list(
    loop = loop[, c(hypernames, "s", "Constant")],
    submodels = submodels
  )
}
balanced_glmnet_new$fit <- function(x, y, wts, param, lev, last, classProbs, ...) {
  w <- rep(1, length(y))
  Z <- cbind(x, as.numeric(y))
  z1 <- cumprod(apply(Z, 2L, max) + 1)
  Z1 <- apply(Z, 1L, function(x) sum(z1 * x))
  oZ <- order(Z1)
  Z2 <- !duplicated(Z1[oZ])
  oX <- (seq_along(Z1)[oZ])[Z2]
  x <- x[oX, , drop = FALSE]
  y <- if (is.matrix(y)) {
    y[oX, , drop = FALSE]
  } else {
    y[oX]
  }
  w <- diff(c(0, cumsum(w))[c(Z2, TRUE)])
  w[w > 1] <- mean(table(y)[1] / table(y)[2])
  numLev <- if (is.character(y) || is.factor(y)) {
    length(levels(y))
  } else {
    NA
  }
  theDots <- list(...)
  if (all(names(theDots) != "family")) {
    if (!is.na(numLev)) {
      fam <- ifelse(numLev > 2, "multinomial", "binomial")
    } else {
      fam <- "gaussian"
    }
    theDots$family <- fam
  }
  if (!is.null(wts)) {
    theDots$weights <- wts
  }
  if (!(class(x)[1] %in% c("matrix", "sparseMatrix"))) {
    x <- Matrix::as.matrix(x)
  }
  modelArgs <- c(list(
    x = x, y = y, alpha = param$alpha, type.logistic = "modified.Newton",
    weights = w, thresh = 1e-04
  ), theDots)
  out <- do.call(glmnet::glmnet, modelArgs)
  if (!is.na(param$lambda[1])) {
    out$lambdaOpt <- param$lambda[1]
  }
  out
}

balanced_glmnet_new$predict <- function(modelFit, newdata, submodels = NULL) {
  obsLevels <- if ("classnames" %in% names(modelFit)) {
    modelFit$classnames
  } else {
    NULL
  }
  probs <- predict(modelFit, Matrix::as.matrix(newdata),
    s = modelFit$lambdaOpt,
    type = "response"
  )
  probs <- as.vector(probs)
  probs <- as.data.frame(cbind(1 - probs, probs))
  colnames(probs) <- modelFit$obsLevels
  preds <- probs / rowSums(probs)
  tpredictnew <- function(Constant, s, out1 = preds) {
    bound <- function(x) {
      pmax(pmin(x, 1 - 1e-15), 1e-15)
    }
    cutoff <- Constant
    probs1 <- out1[, 2]
    probs1[probs1 <= cutoff] <- probs1[probs1 <= cutoff]^s *
      cutoff^(1 - s)
    probs1[probs1 > cutoff] <- 1 - (1 - probs1[probs1 > cutoff])^s *
      (1 - cutoff)^(1 - s)
    out1[, 2] <- bound(probs1)
    out1[, 1] <- bound(1 - probs1)
    out1
  }
  out <- round(tpredictnew(
    Constant = modelFit$tuneValue$Constant,
    s = modelFit$tuneValue$s, out1 = preds
  )[, 2])
  if (!is.null(submodels)) {
    tmp2 <- out
    out <- vector(mode = "list", length = length(submodels$Constant))
    out[[1]] <- tmp2
    for (i in seq(along = submodels$Constant)) {
      out[[i + 1]] <- round(tpredictnew(
        Constant = submodels$Constant[[i]],
        s = submodels$s[[i]]
      )[, 2])
    }
  }
  out
}

balanced_glmnet_new$prob <- function(modelFit, newdata, submodels = NULL) {
  obsLevels <- if ("classnames" %in% names(modelFit)) {
    modelFit$classnames
  } else {
    NULL
  }
  probs <- predict(modelFit, Matrix::as.matrix(newdata),
    s = modelFit$lambdaOpt,
    type = "response"
  )
  probs <- as.vector(probs)
  probs <- as.data.frame(cbind(1 - probs, probs))
  colnames(probs) <- modelFit$obsLevels
  preds <- probs / rowSums(probs)
  tpredictnew <- function(Constant, s = 1, out1 = preds) {
    bound <- function(x) {
      pmax(pmin(x, 1 - 1e-15), 1e-15)
    }
    cutoff <- Constant
    probs1 <- out1[, 2]
    probs1[probs1 <= cutoff] <- probs1[probs1 <= cutoff]^s *
      cutoff^(1 - s)
    probs1[probs1 > cutoff] <- 1 - (1 - probs1[probs1 > cutoff])^s *
      (1 - cutoff)^(1 - s)
    out1[, 2] <- bound(probs1)
    out1[, 1] <- bound(1 - probs1)
    out1
  }
  out <- tpredictnew(
    Constant = modelFit$tuneValue$Constant,
    s = modelFit$tuneValue$s, out1 = preds
  )
  if (!is.null(submodels)) {
    tmp2 <- out
    out <- vector(mode = "list", length = length(submodels$Constant))
    out[[1]] <- tmp2
    for (i in seq(along = submodels$Constant)) {
      out[[i + 1]] <- tpredictnew(
        Constant = submodels$Constant[[i]],
        s = submodels$s[[i]]
      )
    }
  }
  out
}





#' calculate a confusion matrix from FPR, FNR, number of negative class and number of positive class observations
#'
#' @param FPR false positive rate, numeric
#' @param FNR false negative rate, numeric
#' @param N number of negative class observations
#' @param P number of positive class observations
#' @param mode confusin matrix mode, see ?confusionMatrix defaults to calculating all additional metrics
#' @keywords validation
#' @export
#' @examples
#' genNadiasConfusionMatrix(.1, .2, 100, 200)
#'
genConfusionMatrix <- function(FPR, FNR, N = table(trainY)["zero"], P = table(trainY)["positive"], mode = "everything") {
  # N = N0 + N1
  # X = N0/N

  TPR <- 1 - FNR
  TNR <- 1 - FPR

  FN <- FNR * P
  FP <- FPR * N

  TP <- TPR * P
  TN <- TNR * N

  # n = N+P
  # X = N/n
  # A = round(as.numeric((1-FNR) * n * X))
  # B = round(as.numeric(FPR * n * (1-X)))
  # C = round(as.numeric(FNR * n * X ))
  # D = round(as.numeric((1- FPR) * n * (1-X)))

  lvs <- c("Critical", "nonCritical")
  truth <- factor(rep(lvs, times = c(2, 2)),
    levels = lvs
  )
  pred <- factor(
    c(
      rep(lvs, times = c(1, 1)),
      rep(lvs, times = c(1, 1))
    ),
    levels = lvs
  )

  xtab <- table(pred, truth)

  xtab[1, 1] <- TP
  xtab[1, 2] <- FP
  xtab[2, 1] <- FN
  xtab[2, 2] <- TN
  xtab <- round(xtab)

  # FPR = b/(b+d)
  # FNR = 1-(a/(a+c))
  # TPR = a/(a+c)
  # TNR = d/(b+d)
  conf_table <- confusionMatrix(xtab, mode = mode, positive = "Critical")
  suppressWarnings(conf_table$ byClass$FPR <- FPR)
  suppressWarnings(conf_table$ byClass$FNR <- FNR)
  conf_table
}


#' calculate a confusion matrix from a caret model object trained using a validation function that adds FNR and FPR columns to the cross-validated metrics table
#'
#' @param mod model object generated by using train function from caret
#' @param stat statistic used to select the optimal model configuration, passed on to getCVPerf()
#' @param mode confusin matrix mode, see ?confusionMatrix defaults to calculating all additional metrics
#' @keywords validation
#' @export
#' @examples
#' confusionMatrices67 <- lapply(modsList, function(x) {
#'   extractConfusionMatrix(x, stat = "BalancedErrorRate67")
#' })
#'
extractConfusionMatrix <- function(mod, stat = "BalancedLogLoss50", N = table(trainY)["zero"], P = table(trainY)["positive"], mode = "everything") {
  FPR <- as.numeric(getCVPerf(mod, stat)["FPR"])
  FNR <- as.numeric(getCVPerf(mod, stat)["FNR"])

  genConfusionMatrix(FPR = FPR, FNR = FNR, N = N, P = P, mode = mode)
}


#' Returns cross-validated performance metrics from a caret model.
#'
#' @param mod model fitted with caret::train
#' @param stat CV metrics will be returned at the minimized value of this statistic. Defaults to NULL in which case the metric supplied to the train call will be used.
#' @keywords validation
#' @export
#' @examples
#' getCVPerf(mod)
getCVPerf <- function(mod, stat = NULL, max.samples = FALSE) {
  is.boundary.solution <- function(theta, Theta) {
    if (theta == Theta[1] | theta == Theta[length(Theta)]) {
      TRUE
    } else {
      FALSE
    }
  }
  if (is.null(stat)) {
    stat <- mod$ metric
  }
  if (max.samples) {
    selectfrom <- mod$results[mod$results[, ncol(mod$results)] == max(mod$results[, ncol(mod$results)], na.rm = TRUE), ]
  } else {
    selectfrom <- mod$results
  }
  best <- selectfrom[which.min(selectfrom[, stat]), ]
  tuningpars <- as.character(mod$modelInfo$parameters$parameter)
  config <- best[tuningpars]
  boundarySolutions <- character()
  for (par in tuningpars) {
    if (length(unique(selectfrom[, par])) >= 3) {
      if (is.boundary.solution(config[par], unique(selectfrom[, par]))) {
        boundarySolutions[length(boundarySolutions) + 1] <- paste0("'", par, "'")
      }
    }
  }
  if (length(boundarySolutions) > 0) {
    warning(paste0("The following tuning parametershave been identified as boundary solutions: ", toString(boundarySolutions)))
  }
  best
}


#' clean a set of predictors
#'
#' @param X set of covariates
#' @param cutoff maximum allowable correlation between any two covariates
#' @param na.ignore ignore NA values, will use pair-wise complete observations if TRUE
#' @keywords data management, caret
#' @export
#' @examples
#' clean_X <- cleanMat(X)
#'
cleanMat <- function(X, cutoff = .85, na.ignore = FALSE) {
  if (na.ignore) {
    use <- "pairwise.complete.obs"
  } else {
    removes1 <- nearZeroVar(X)
    if (length(removes1) > 0) {
      X <- X[, -removes1]
    }
    removes2 <- findLinearCombos(X)$remove
    if (length(removes2) > 0) {
      X <- X[, -removes2]
    }
    use <- "everything"
  }
  removes3 <- findCorrelation(cor(X, use = use), cutoff = min(max(.90, cutoff + .075), .95), exact = FALSE)
  # drop removes
  if (length(removes3) > 0) {
    X <- X[, -removes3]
  }
  removes4 <- findCorrelation(cor(X, use = use), cutoff = cutoff, exact = TRUE)
  # drop removes
  if (length(removes4) > 0) {
    X <- X[, -removes4]
  }
  X
}

####################### Main Script ######################

# continent loop
for (continent in continents) {
  print(paste("Regressing", continent))

  # set seed for reproducibility
  set.seed(1)

  # load in data
  file <- paste0(folder_clean, continent, "/calibset_urban_", marker, ".csv")
  df <- fread(file, na.strings = c("NA", "N/A", "null"))
  df <- df[complete.cases(df), ]

  # remove variables with high correlation
  descrCor <- cor(df)
  write.csv(descrCor, paste0(folder_out, continent, "/Correlation_matrix_", marker, ".csv"))
  summary(descrCor[upper.tri(descrCor)])
  highlyCorDescr <- findCorrelation(descrCor, cutoff = .70)
  df_clean <- dplyr::select(df, -all_of(highlyCorDescr))

  # Make UrbanArea2010 a factor class, reversed levels so "positive" becomes the positive class
  df_clean <- cbind(df_clean[, -1], df_clean[, 1])
  cnames <- colnames(df_clean)
  cnames[length(cnames)] <- "Class"
  colnames(df_clean) <- cnames
  df_clean$Class <- factor(df_clean$Class, labels = c("zero", "positive"))
  # df_clean$Class <- factor(df_clean$Class, levels = rev(levels(df_clean$Class)))
  # commented out to not reverse the class



  # take a random sample and create training / testing data
  set.seed(1)
  randomSample <- createDataPartition(df_clean$Class, p = 0.5, list = FALSE)
  training <- df_clean[randomSample, ]
  testing <- df_clean[-randomSample, ]
  trainX <- dplyr::select(training, -c("Class"))
  trainY <- training$Class
  testX <- dplyr::select(testing, -c("Class"))
  testY <- testing$Class

  train_df <- cbind(Urban = trainY, trainX)


  # Define number of foldes
  folds <- 10
  repeats <- 1
  tuneLength <- 5

  print("Starting GLM")

  # Generalized Linear Model with downsampling
  model_glm_down <- caret::train(Urban ~ .,
    data = train_df,
    method = "multinom",
    metric = "BalancedLogLoss50",
    tuneLength = tuneLength,
    maximize = FALSE,
    trControl = trainControl(
      method = "repeatedcv",
      number = folds,
      repeats = repeats,
      returnResamp = "final",
      classProbs = TRUE,
      sampling = "down",
      summaryFunction = balancedSummary,
      # verboseIter = TRUE
    )
  )
 

  print("Finished GLM")

  #>>>>>>>> Model outputs

  # when optimizing for Balanced log loss
  BalancedLogLoss50 <- getCVPerf(model_glm_down, "BalancedLogLoss50")
  write.csv(BalancedLogLoss50, paste0(folder_out, continent, "/results/BalancedLogLoss50_", marker, ".csv"))

  # plot all possible FPR vs FNR combinations
  plot_fnr_vs_fpr <- function(mod) {
    hypname <- if ("decay" %in% colnames(mod$results)) {
      "decay"
    } else {
      "lambda"
    }
    plot(mod$results[, "FPR"], mod$results[, "FNR"], col = as.numeric(factor(mod$results[, hypname])), xlab = "FPR", ylab = "FNR", pch = 20)
  }

  plot_fnr_vs_fpr(model_glm_down)

  plot(model_glm_down)


  # make predictions for your test data
  test_glm_down_pred <- predict(model_glm_down, testX)


  # check the out-of-sample metrics

  # down sampled logit should have better balanced accuracy
  con_glm_down <- confusionMatrix(data = test_glm_down_pred, reference = testY, mode = "everything")

  write.csv(
    con_glm_down$byClass,
    paste0(folder_out, continent, "/results/byClass_glm_down_", marker, ".csv")
  )

  write.csv(
    con_glm_down$table,
    paste0(folder_out, continent, "/results/table_glm_down_", marker, ".csv")
  )

  # predict probabilities for your test data
  test_glm_down_prob <- predict(model_glm_down, testX, type = "prob")


  # check how good the predictions are with log loss (lower is better)
  # Lb(p_pred=test_glm_down_prob$positive, y_true=ifelse(testY=="zero", 0, 1))
  # Lb(p_pred=test_balanced_glm_prob$positive, y_true=as.numeric(testY)-1)

  LogLoss_table <- LogLoss(y_pred = test_glm_down_prob$positive, y_true = as.numeric(testY) - 1)
  write.csv(LogLoss_table, paste0(folder_out, continent, "/results/LogLoss_", marker, ".csv"))

  # check which variables are important
  glm_down_imp <- varImp(model_glm_down)
  plot(glm_down_imp)


  # Save models
  saveRDS(model_glm_down, paste0(folder_out, continent, "/results/model_glm_down_", marker, ".rds"))


  # open rds
  model_glm_down <- readRDS(paste0(folder_out, continent, "/results/model_glm_down_", marker, ".rds"))




  # calculate Z score and p-Value for the variables in the model
  z <- summary(model_glm_down)$coefficients / summary(model_glm_down)$standard.errors
  write.csv(z, paste0(folder_out, continent, "/results/z_", marker, ".csv"))


  # 2-tailed Wald z tests to test significance of coefficients
  p <- (1 - pnorm(abs(z), 0, 1)) * 2
  write.csv(p, paste0(folder_out, continent, "/results/p_", marker, ".csv"))



  # output regression coefficients
  write.csv(
    summary(model_glm_down)$coefficients,
    paste0(folder_out, continent, "/results/coefficients_", marker, ".csv")
  )
  write.csv(
    summary(model_glm_down)$standard.errors,
    paste0(folder_out, continent, "/results/standard_errors_", marker, ".csv")
  )

  stargazer(
    df_clean,
    type = "text",
    out = paste0(folder_out, continent, "/Descriptive_statistics_", marker, ".txt")
  )

  gc()
}

print("Finished predictive models")