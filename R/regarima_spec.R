#' Manage Outliers/Ramps in Specification
#'
#' Generic function to add outliers or Ramp regressors (\code{add_outlier()} and \code{add_ramp()})
#' to a specification or to remove them (\code{remove_outlier()} and \code{remove_ramp()}).
#'
#' @param x the specification.
#' @param type,date type and date of the outliers. Possible \code{type} are:
#' \code{"AO"} = additive, \code{"LS"} = level shift, \code{"TC"} = transitory change and
#' \code{"SO"} = seasonal outlier.
#' @param start,end dates of the ramp regressor.
#' @param name the name of the variable (to format print).
#' @param coef the coefficient. If equal to 0 the outliers/ramps are estimated.
#' @export
add_outlier <- function(x,
                        type,
                        date,
                        name = sprintf("%s (%s)", type, date),
                        coef = 0){
  UseMethod("add_outlier", x)
}
#' @export
add_outlier.default <- function(x,
                                type,
                                date,
                                name = sprintf("%s (%s)", type, date),
                                coef = 0){
  type = match.arg(toupper(type),
                   choices = c("AO", "TC", "LS", "SO"),
                   several.ok = TRUE)
  # data.frame to recycle arguments
  new_out <- data.frame(type, date, name, coef)
  new_out <- as.list(new_out)
  new_out <- mapply(rjd3modelling::createOutlier,
                    as.list(new_out)[[1]],
                    as.list(new_out)[[2]],
                    as.list(new_out)[[3]],
                    as.list(new_out)[[4]],
                    SIMPLIFY = FALSE)
  names(new_out) <- NULL
  x$regression$outliers <- c(x$regression$outliers,
                             new_out)
  all_out = t(simplify2array(x$regression$outliers)[c("pos","code"),])
  dupl_out <- duplicated(all_out,fromLast = TRUE)
  if(any(dupl_out)){
    warning("Duplicated outliers removed: last outliers kept")
    x$regression$outliers <- x$regression$outliers[!dupl_out]
  }
  x
}
#' @rdname add_outlier
#' @export
remove_outlier <- function(x,
                           type = NULL,
                           date = NULL,
                           name = NULL){
  UseMethod("remove_outlier", x)
}
#' @export
remove_outlier.default <- function(x,
                                type = NULL,
                                date = NULL,
                                name = NULL){
  if (is.null(x$regression$outliers))
    return (x)
  out_mat = simplify2array(x$regression$outliers)[c("code", "pos", "name"),, drop = FALSE]
  if (is.null(type)) {
    out_mat["code",] = ""
  } else {
    type = match.arg(toupper(type),
                     choices = c("AO", "TC", "LS", "SO"),
                     several.ok = TRUE)
  }
  if (is.null(date)) {
    out_mat["pos",] = ""
  }
  if (is.null(name)) {
    out_mat["name",] = ""
  }
  out_id = apply(out_mat,2, paste0, collapse = "")
  rm_out_id = rbind(type = type, date = date, name = name)
  if (is.null(rm_out_id))
    return (x)
  rm_out_id = apply(rm_out_id,2, paste0, collapse = "")

  remove_out = out_id %in% rm_out_id
  x$regression$outliers <- x$regression$outliers[!remove_out]
  if (length(x$regression$outliers) == 0) {
    x$regression["outliers"] = list(NULL)
  }
  x
}
#' @rdname add_outlier
#' @export
add_ramp <- function(x,
                     start,
                     end,
                     name = sprintf("rp.%s - %s", start, end),
                     coef = 0){
  UseMethod("add_ramp", x)
}
#' @export
add_ramp.default <- function(x,
                             start,
                             end,
                             name = sprintf("rp.%s - %s", start, end),
                             coef = 0){
  # data.frame to recycle arguments
  new_ramp <- data.frame(start, end, name, coef)
  new_ramp <- as.list(new_ramp)
  new_ramp <- mapply(rjd3modelling::createRamp,
                     as.list(new_ramp)[[1]],
                     as.list(new_ramp)[[2]],
                     as.list(new_ramp)[[3]],
                     as.list(new_ramp)[[4]],
                     SIMPLIFY = FALSE)
  names(new_ramp) <- NULL
  x$regression$ramps <- c(x$regression$ramps,
                          new_ramp)
  all_out = t(simplify2array(x$regression$ramps)[c("start", "end"),])
  dupl_out <- duplicated(all_out,fromLast = TRUE)
  if(any(dupl_out)){
    warning("Duplicated ramps removed")
    x$regression$ramps <- x$regression$ramps[!dupl_out]
  }
  x
}

#' @rdname add_outlier
#' @export
remove_ramp <- function(x,
                     start = NULL,
                     end = NULL,
                     name = NULL){
  UseMethod("remove_ramp", x)
}
#' @export
remove_ramp.default <- function(x,
                                start = NULL,
                                end = NULL,
                                name = NULL){
  if (is.null(x$regression$ramps))
    return (x)
  rp_mat = simplify2array(x$regression$ramps)[c("start", "end", "name"),, drop = FALSE]
  if (is.null(start)) {
    rp_mat["start",] = ""
  }
  if (is.null(end)) {
    rp_mat["end",] = ""
  }
  if (is.null(name)) {
    rp_mat["name",] = ""
  }
  rp_id = apply(rp_mat,2, paste0, collapse = "")
  rm_rp_id = rbind(start = start, end = end, name = name)
  if (is.null(rm_rp_id))
    return (x)
  rm_rp_id = apply(rm_rp_id,2, paste0, collapse = "")

  remove_rp = rp_id %in% rm_rp_id
  x$regression$ramps <- x$regression$ramps[!remove_rp]
  if (length(x$regression$ramps) == 0) {
    x$regression["ramps"] = list(NULL)
  }
  x
}

#' Set Basic Specification
#'
#' @inheritParams add_outlier
#'
#' @param type,d0,d1,n0,n1 parameters to specifiy the span of to be used.
#'
#' \code{d0} and \code{d1} characters in the format "YYYY-MM-DD" to specify first/last date of the span when \code{type} equals to \code{"From"}, \code{"To"} or \code{"Between"}.
#'
#' \code{n0} and \code{n1} numerics to specify the number of periods at the beginning/end of the series to be used for the span
#' (\code{type} equals to \code{"From"}, \code{"To"}) or to exclude (\code{type} equals to \code{"Excluding"}).
#'
#' @param preliminary.check a boolean to check the quality of the input series and exclude highly problematic ones
#' (e.g. the series with a number of identical observations and/or missing values above pre-specified threshold values).
#'
#' @param preprocessing (REGARIMA/X13 Specific) a boolean to enable/disable the pre-processing.
#'
#' @export
set_basic <- function(x,
                      type = c(NA, "All", "From", "To", "Between", "Last", "First", "Excluding"),
                      d0 = NULL,
                      d1 = NULL,
                      n0 = 0,
                      n1 = 0,
                      preliminary.check = NA,
                      preprocessing = NA){
  UseMethod("set_basic", x)
}
set_basic.default <- function(x,
                              type = c(NA, "All", "From", "To", "Between", "Last", "First", "Excluding"),
                              d0 = NULL,
                              d1 = NULL,
                              n0 = 0,
                              n1 = 0,
                              preliminary.check = NA,
                              preprocessing = NA){
  basic <- x$basic
  is_tramo <- inherits(x, "JD3_TRAMO_SPEC")

  basic$span <- set_span(basic$span,
                         type = type,
                         d0 = d0, d1 = d1,
                         n0 = n0, n1 = n1)
  if(!missing(preprocessing) & !is.na(preprocessing) & !is_tramo){
    basic$preprocessing <- preprocessing
  }
  if(!missing(preliminary.check) & !is.na(preliminary.check)){
    basic$preliminaryCheck <- preliminary.check
  }
  x$basic <- basic
  x
}
#' Set Estimate Specification
#'
#' @inheritParams set_basic
#'
#' @param tol a numeric, convergence tolerance. The absolute changes in the log-likelihood function
#' are compared to this value to check for the convergence of the estimation iterations.
#'
#' @param exact.ml (TRAMO specific) \code{logical}, the exact maximum likelihood estimation. If \code{TRUE}, the program performs an exact maximum likelihood estimation.
#' If \code{FASLE}, the Unconditional Least Squares method is used.
#'
#' @param unit.root.limit (TRAMO specific) \code{numeric}, the final unit root limit. The threshold value for the final unit root test
#' for identification of differencing orders. If the magnitude of an AR root for the final model is smaller than this number,
#'  then a unit root is assumed, the order of the AR polynomial is reduced by one and the appropriate order of the differencing
#'  (non-seasonal, seasonal) is increased.
#'
#' @export
set_estimate <- function(x,
                         type = c(NA, "All", "From", "To", "Between", "Last", "First", "Excluding"),
                         d0 = NULL,
                         d1 = NULL,
                         n0 = 0,
                         n1 = 0,
                         tol = NA,
                         # TRAMO SPECIFIC
                         exact.ml = NA,
                         unit.root.limit = NA){
  UseMethod("set_estimate", x)
}
#' @export
set_estimate.default <- function(x,
                                 type = c("All", "From", "To", "Between", "Last", "First", "Excluding"),
                                 d0 = NULL,
                                 d1 = NULL,
                                 n0 = 0,
                                 n1 = 0,
                                 tol = NA,
                                 # TRAMO SPECIFIC
                                 exact.ml = NA,
                                 unit.root.limit = NA){
  estimate <- x$estimate
  is_tramo <- inherits(x, "JD3_TRAMO_SPEC")
  estimate$span <- set_span(estimate$span,
                            type = type,
                            d0 = d0, d1 = d1,
                            n0 = n0, n1 = n1)
  if (!missing(tol) & !is.na(tol)) {
    estimate$tol <- tol
  }
  # TRAMO-SEATS SPECIFIC
  if (!missing(exact.ml) & !is.na(exact.ml) & is_tramo) {
    estimate$ml <- exact.ml
  }
  if (!missing(unit.root.limit) & !is.na(unit.root.limit) & is_tramo) {
    estimate$ubp <- unit.root.limit
  }
  # END TRAMO-SEATS SPECIFIC
  x$estimate <- estimate
  x
}
#' Set Outlier Specification
#'
#' @inheritParams set_basic
#' @param span.type,d0,d1,n0,n1 parameters to specifiy the span of to be used.
#'
#' \code{d0} and \code{d1} characters in the format "YYYY-MM-DD" to specify first/last date of the span when \code{type} equals to \code{"From"}, \code{"To"} or \code{"Between"}.
#'
#' \code{n0} and \code{n1} numerics to specify the number of periods at the beginning/end of the series to be used for the span
#' (\code{type} equals to \code{"From"}, \code{"To"}) or to exclude (\code{type} equals to \code{"Excluding"}).

#' @param outliers.type vector of characters of the outliers to be automatically detected. \code{"AO"} for additive outliers, \code{"TC"} for transitory changes
#' \code{"LS"} for level shifts and \code{"SO"} for seasonal outliers.
#' For example \code{outliers.type = c("AO", "LS")} to enable the detection of additive outliers and level shifts.
#' If \code{outliers.type = NULL} or \code{outliers.type = character()}, automatic detection of outliers is disabled.
#'
#' @param critical.value \code{numeric}. The entered critical value for the outliers' detection procedure.
#' If equal to 0 the critical value for the outliers' detection procedure is automatically determined
#' by the number of observations in the outlier detection time span.
#'
#'
#' @param tc.rate the rate of decay for the transitory change outlier.
#' @param method (REGARIMA/X13 Specific) determines how the program successively adds detected outliers to the model.
#' At present, only the \code{"AddOne"} method is supported.
#' @param maxiter (REGARIMA/X13 Specific) TODO
#' @param lsrun (REGARIMA/X13 Specific) TODO
#' @param eml.est (TRAMO Specific) \code{logical} for the exact likelihood estimation method. It controls the method applied for a parameter estimation
#' in the intermediate steps of the automatic detection and correction of outliers. If \code{TRUE}, an exact likelihood estimation method is used.
#' When \code{FALSE}, the fast Hannan-Rissanen method is used.
#' @export
set_outlier <- function(x,
                        span.type = c(NA, "All", "From", "To", "Between", "Last", "First", "Excluding"),
                        d0 = NULL,
                        d1 = NULL,
                        n0 = 0,
                        n1 = 0,
                        outliers.type = NA,
                        critical.value = NA,
                        tc.rate = NA,
                        # REGARIMA SPECIFIC
                        method = c(NA, "AddOne", "AddAll"),
                        maxiter = NA,
                        lsrun = NA,
                        # TRAMO SPECIFIC
                        eml.est = NA){
  UseMethod("set_outlier", x)
}
#' @export
set_outlier.default <- function(x,
                                span.type = c(NA, "All", "From", "To", "Between", "Last", "First", "Excluding"),
                                d0 = NULL,
                                d1 = NULL,
                                n0 = 0,
                                n1 = 0,
                                outliers.type = NA,
                                critical.value = NA,
                                tc.rate = NA,
                                # REGARIMA SPECIFIC
                                method = c(NA, "AddOne", "AddAll"),
                                maxiter = NA,
                                lsrun = NA,
                                # TRAMO SPECIFIC
                                eml.est = NA){
  outlier <- x$outlier
  outlier$outlier <- set_span(outlier$span,
                              type = span.type,
                              d0 = d0, d1 = d1,
                              n0 = n0, n1 = n1)
  # to set specific TRAMO/REGARIMA values
  is_tramo <- inherits(x, "JD3_TRAMO_SPEC")

  va_name <- ifelse(is_tramo, "va", "defva")
  tcr_name <- ifelse(is_tramo, "tcrate", "monthlytcrate")

  if(missing(critical.value) | is.na(critical.value)){
    critical.value <- outlier[[va_name]]
  }else{
    outlier[[va_name]] <- critical.value
  }
  if(is.null(outliers.type) || length(outliers.type) == 0){
    if (is_tramo) {
      outlier$enabled <- FALSE
    } else {
      outlier$outliers <- list()
    }
  }else if(!missing(outliers.type) && !all(is.na(outliers.type))){
    outliers.type = match.arg(toupper(outliers.type),
                              choices = c("AO", "LS", "TC", "SO"),
                              several.ok = TRUE)
    outliers.type = unique(outliers.type)
    if (is_tramo) {
      outlier$enabled <- TRUE
      for (out.name in c("ao", "ls", "ts", "so")) {
        outlier[[out.name]] <- out.name %in% tolower(outliers.type)
      }
    } else {
      outlier$outliers <- lapply(outliers.type, function(x){
        list(type = x, va = critical.value)
      })
    }
  }

  if (!is.na(tc.rate)) {
    outlier[[tcr_name]] <- tc.rate
  }
  if (is_tramo) {
    # TRAMO SPECIFIC PARAMETERS
    if (!is.na(eml.est) & is_tramo) {
      outlier$ml <- eml.est
    }
  } else {
    # REGARIMA SPECIFIC PARAMETERS
    if (!missing(method) && !is.null(method) && !all(is.na(method))) {
      method <- match.arg(toupper(method)[1],
                          choices = c("ADDONE", "ADDALL"))
      outlier$method <- method
    }
    if (!is.na(maxiter) ) {
      outlier$maxiter <- maxiter
    }
    if (!is.na(lsrun)) {
      outlier$lsrun <- lsrun
    }
  }
  x$outlier <- outlier
  x
}

#' Set Automodel Specification
#'
#' @inheritParams set_basic
#'
#' @param enabled \code{logical}. If \code{TRUE}, the automatic modelling of the ARIMA model is enabled.
#' If \code{FALSE}, the parameters of the ARIMA model can be specified.
#' @param acceptdefault \code{logical}. If \code{TRUE}, the default model (ARIMA(0,1,1)(0,1,1)) may be chosen in the first step
#' of the automatic model identification. If the Ljung-Box Q statistics for the residuals is acceptable, the default model is accepted
#' and no further attempt will be made to identify another model.
#' @param cancel \code{numeric}, the cancelation limit. If the difference in moduli of an AR and an MA roots (when estimating ARIMA(1,0,1)(1,0,1) models
#' in the second step of the automatic identification of the differencing orders) is smaller than the cancelation limit, the
#' @param ub1 \code{numeric}, the first unit root limit. It is the threshold value for the initial unit root test in the automatic differencing procedure.
#' When one of the roots in the estimation of the ARIMA(2,0,0)(1,0,0) plus mean model, performed in the first step of the automatic model identification procedure,
#' is larger than first unit root limit in modulus, it is set equal to unity.
#' @param ub2 \code{numeric}, the second unit root limit. When one of the roots in the estimation of the ARIMA(1,0,1)(1,0,1) plus mean model,
#' which is performed in the second step of the automatic model identification procedure, is larger than second unit root limit in modulus,
#' it is checked if there is a common factor in the corresponding AR and MA polynomials of the ARMA model that can be cancelled (see \code{automdl.cancel}).
#' If there is no cancellation, the AR root is set equal to unity (i.e. the differencing order changes).
#'
#' @param reducecv \code{numeric}, ReduceCV. The percentage by which the outlier critical value will be reduced
#' when an identified model is found to have a Ljung-Box statistic with an unacceptable confidence coefficient.
#' The parameter should be between 0 and 1, and will only be active when automatic outlier identification is enabled.
#' The reduced critical value will be set to (1-ReduceCV)xCV, where CV is the original critical value.
#'
#' @param ljungboxlimit \code{numeric}, the Ljung Box limit, setting the acceptance criterion for the confidence intervals of the Ljung-Box Q statistic.
#' If the LjungBox Q statistics for the residuals of a final model is greater than Ljung Box limit, then the model is rejected, the outlier critical value is reduced,
#' and model and outlier identification (if specified) is redone with a reduced value.
#'
#' @param tsig \code{numeric}, the arma limit. It is the threshold value for t-statistics of ARMA coefficients and the constant term used
#' for the final test of model parsimony. If the highest order ARMA coefficient has a t-value smaller than this value in magnitude, the order of the model is reduced.
#' If the constant term has a t-value smaller than the ARMA limit in magnitude, it is removed from the set of regressors.
#'
#' @param ubfinal (REGARIMA/X13 Specific) \code{numeric}, final unit root limit. The threshold value for the final unit root test.
#' If the magnitude of an AR root for the final model is smaller than the final unit root limit, then a unit root is assumed,
#' the order of the AR polynomial is reduced by one and the appropriate order of the differencing (non-seasonal, seasonal)
#' is increased. The parameter value should be greater than one.
#' @param checkmu (REGARIMA/X13 Specific) \code{logical}. TODO.
#' @param mixed (REGARIMA/X13 Specific) \code{logical}. This variable controls whether ARIMA models with non-seasonal AR and MA terms
#' or seasonal AR and MA terms will be considered in the automatic model identification procedure.
#' If \code{FALSE}, a model with AR and MA terms in both the seasonal and non-seasonal parts of the model can be acceptable,
#' provided there are no AR or MA terms in either the seasonal or non-seasonal terms.
#' @param fct (REGARIMA/X13 Specific) \code{numeric}. TODO.
#' @param balanced (REGARIMA/X13 Specific) \code{logical} If \code{TRUE}, the automatic model identification procedure will have a preference
#' for balanced models (i.e. models for which the order of the combined AR and differencing operator is equal to the order
#' of the combined MA operator).
#' @param amicompare (TRAMO Specific) \code{logical}. If {TRUE}, the program compares the model identified by the automatic procedure to the default model (ARIMA(0,1,1)(0,1,1))
#' and the model with the best fit is selected. Criteria considered are residual diagnostics, the model structure and the number of outliers.
#' @export
set_automodel <- function(x,
                          enabled = NA,
                          acceptdefault = NA,
                          cancel = NA,
                          ub1 = NA,
                          ub2 = NA,
                          reducecv = NA,
                          ljungboxlimit = NA,
                          tsig = NA,
                          # REGARIMA SPECIFIC
                          ubfinal = NA,
                          checkmu = NA,
                          mixed = NA,
                          fct = NA,
                          balanced = NA,
                          # TRAMO SPECIFIC
                          amicompare=NA){
  UseMethod("set_automodel", x)
}
#' @export
set_automodel.default <- function(x,
                                  enabled = NA,
                                  acceptdefault = NA,
                                  cancel = NA,
                                  ub1 = NA,
                                  ub2 = NA,
                                  reducecv = NA,
                                  ljungboxlimit = NA,
                                  tsig = NA,
                                  # REGARIMA SPECIFIC
                                  ubfinal = NA,
                                  checkmu = NA,
                                  mixed = NA,
                                  fct = NA,
                                  balanced = NA,
                                  # TRAMO SPECIFIC
                                  amicompare = NA){
  automodel <- x$automodel

  is_tramo <- inherits(x, "JD3_TRAMO_SPEC")
  reducecv_col <- ifelse(is_tramo, "pc", "predcv")
  lblim_col <- ifelse(is_tramo, "pcr", "ljungbox")
  if(!is.na(enabled) & is.logical(enabled)){
    automodel$enabled <- enabled
  }

  if(!is.na(ub1)){
    automodel$ub1 <- ub1
  }
  if(!is.na(ub2)){
    automodel$ub2 <- ub2
  }
  if(!is.na(cancel)){
    automodel$cancel <- cancel
  }
  if(!is.na(fct)){
    automodel$fct <- fct
  }
  if(!is.na(ljungboxlimit)){
    automodel[[lblim_col]] <- ljungboxlimit
  }
  if(!is.na(reducecv)){
    automodel[[reducecv_col]] <- reducecv
  }
  if(!is.na(acceptdefault) & is.logical(acceptdefault)){
    automodel$acceptdef <- acceptdefault
  }

  if(!is.na(tsig)){
    automodel$tsig <- tsig
  }
  if (is_tramo) {
    # TRAMO SPECIFIC
    if(!is.na(amicompare) & is.logical(amicompare)){
      automodel$amicompare <- amicompare
    }
  } else {
    # REGARIMA SPECIFIC
    if(!is.na(ubfinal)){
      automodel$ubfinal <- ubfinal
    }
    if(!is.na(checkmu) & is.logical(checkmu)){
      automodel$checkmu <- checkmu
    }
    if(!is.na(mixed) & is.logical(mixed)){
      automodel$mixed <- mixed
    }
    if(!is.na(balanced) & is.logical(balanced)){
      automodel$balanced <- balanced
    }
  }

  x$automodel <- automodel
  x
}
#' Set ARIMA Specification
#'
#' Function to set the ARIMA model when the automatic modelling is disabled.
#'
#' @inheritParams set_basic
#' @param mean to set the coefficient of the mean. If \code{mean = 0}, the mean is disabled.
#' @param mean.type a character defining the mean coefficient estimation procedure.
#' Possible procedures are: \code{"Undefined"} = no use of any user-defined input (i.e. coefficients are estimated),
#' \code{"Fixed"} = the coefficients are fixed at the value provided by the user,
#' \code{"Initial"} = the value defined by the user is used as the initial condition.
#'
#' @param p,d,q,bp,bd,bq to specify the order of the SARIMA model in the form ARIMA(p,d,q)(bp,bd,bd).
#' @param coef a vector providing the coefficients for the regular and seasonal AR and MA polynominals.
#' The vector length must be equal to the sum of the regular and seasonal AR and MA orders.
#' The coefficients shall be provided in the following order: regular AR (\emph{Phi}; \code{p} elements),
#' regular MA  (\emph{Theta}; \code{q} elements), seasonal AR (\emph{BPhi}; \code{bp} elements)
#' and seasonal MA (\emph{BTheta}; \code{bq} elements).
#' E.g.: \code{arima.coef=c(0.6,0.7)} with \code{p=1, q=0,bp=1} and \code{bq=0}.
#'
#' @param coef.type a vector defining the ARMA coefficients estimation procedure.
#' Possible procedures are: \code{"Undefined"} = no use of any user-defined input (i.e. coefficients are estimated),
#' \code{"Fixed"} = the coefficients are fixed at the value provided by the user,
#' \code{"Initial"} = the value defined by the user is used as the initial condition.
#'
#' @export
set_arima <- function(x,
                      mean = NA,
                      mean.type = c(NA, "Undefined", "Fixed", "Initial"),
                      p = NA,
                      d = NA,
                      q = NA,
                      bp = NA,
                      bd = NA,
                      bq = NA,
                      coef = NA,
                      coef.type = c(NA, "Undefined", "Fixed", "Initial")){
  UseMethod("set_arima", x)
}
#' @export
set_arima.default <- function(x,
                              mean = NA,
                              mean.type = c(NA, "Undefined", "Fixed", "Initial"),
                              p = NA,
                              d = NA,
                              q = NA,
                              bp = NA,
                              bd = NA,
                              bq = NA,
                              coef = NA,
                              coef.type = c(NA, "Undefined", "Fixed", "Initial")){
  arima <- x$arima
  if(x$automodel$enabled){
    warning("autmodel enabled: the parameters will not impact the final parameters")
  }
  if(!is.na(d)){
    arima$d <- d
  }
  if(!is.na(bd)){
    arima$bd <- bd
  }
  if(missing(coef.type) || is.null(coef.type)){
    coef.type <- "UNDEFINED"
  }else{
    coef.type <- match.arg(toupper(coef.type),
                           choices = c(NA, "UNDEFINED", "FIXED", "INITIAL"),
                           several.ok = TRUE)
    coef.type[is.na(coef.type)] <- "UNDEFINED"
  }
  if(missing(coef) || is.null(coef)){
    coef <- 0
  }else{
    coef[is.na(coef)] <- 0
  }

  if (any(!is.na(c(p, bp, q, bq)))) {
    np <- ifelse(is.na(p), 0, p)
    nbp <- ifelse(is.na(bp), 0, bp)
    nq <- ifelse(is.na(q), 0, q)
    nbq <- ifelse(is.na(bq), 0, bq)
    if (np + nq + nbp + nbq == 0) {
      arima_params <- NULL
    } else {
      arima_params <- data.frame(arima_order = c(rep("p", np),
                                                 rep("phi", nq),
                                                 rep("bp", nbp),
                                                 rep("bphi", nbq)),
                                 value = coef,
                                 type = coef.type)
      arima_params$value <- as.list(arima_params$value)
      arima_params$type <- as.list(arima_params$type)
    }


    if (!is.na(p)) {
      if (p == 0) {
        arima["phi"] <- NULL
      } else {
        arima$phi <- t(arima_params[1:p, c("value", "type")])
        colnames(arima$phi) <- NULL
        arima_params <- arima_params[-c(1:p),]
      }
    }
    if (!is.na(q)) {
      if (q == 0) {
        arima["theta"] <- NULL
      } else {
        arima$theta <- t(arima_params[1:q, c("value", "type")])
        colnames(arima$theta) <- NULL
        arima_params <- arima_params[-c(1:q),]
      }
    }
    if (!is.na(bp)) {
      if (bp == 0) {
        arima["bphi"] <- NULL
      } else {
        arima$bphi <- t(arima_params[1:bp, c("value", "type")])
        colnames(arima$bphi) <- NULL
        arima_params <- arima_params[-c(1:bp),]
      }
    }
    if (!is.na(bq)) {
      if (bq == 0) {
        arima["btheta"] <- NULL
      } else {
        arima$btheta <- t(arima_params[1:bq, c("value", "type")])
        colnames(arima$btheta) <- NULL
      }
    }
  }
  x$arima <- arima

  regression <- x$regression
  if (missing(mean.type) || any(is.na(mean.type))) {
    mean.type <- "UNDEFINED"
  } else {
    mean.type <- match.arg(toupper(mean.type)[1],
                           choices = c("UNDEFINED", "FIXED", "INITIAL"))
  }
  if (is.null(mean) || is.na(mean)) {
    regression["mean"] <- list(NULL)
  } else {
    regression$mean$value <- mean
    regression$mean$type <- mean.type
  }
  x$regression <- regression

  x
}


#' Set Trading-Days Specification
#'
#'
#' @inheritParams set_basic
#' @param option to specify the set of trading days regression variables:
#' \code{"TradingDays"} = six day-of-the-week regression variables;
#' \code{"WorkingDays"} = one working/non-working day contrast variable;
#' \code{"None"} = no correction for trading days and working days effects.
#' @param uservariable a vector of characters to specify the name of user-defined calendar regressors.
#' @param stocktd  a numeric indicating the day of the month when inventories and other stock are reported
#' (to denote the last day of the month, set the variable to 31).
#' When specified, automatically set \code{option = "None"}.
#'
#' @param test defines the pre-tests for the significance of the trading day regression variables
#' based on the AICC statistics: \code{"None"} = the trading day variables are not pre-tested and are included in the model;
#'
#' (REGARIMA/X-13 specific)
#'
#' \code{"Add"} = the trading day variables are not included in the initial regression model
#' but can be added to the RegARIMA model after the test;
#' \code{"Remove"} = the trading day variables belong to the initial regression model but can be removed from the RegARIMA model
#' after the test;
#'
#' (TRAMO specific)
#'
#' \code{"Separate_T"} = a t-test is applied to each trading day variable separately and the trading day variables are included in the RegArima model
#' if at least one t-statistic is greater than 2.6 or if two t-statistics are greater than 2.0 (in absolute terms);
#' \code{"Joint_F"} = a joint F-test of significance of all the trading day variables. The trading day effect is significant if the F statistic is greater than 0.95.
#'
#' @param autoadjust a logical indicating if the program corrects automatically for the leap year effect.
#' It is available when the transformation function is set to Auto.
#' @param coef vector of coefficients for the tranding-days regressors.
#'
#' @param automatic (TRAMO SPECIFIC) defines whether the calendar effects should be added to the model manually (\code{"Unused"}) or automatically.
#' During the automatic selection, the choice of the number of calendar variables can be based on the F-Test (\code{"FTest"}) or the Wald Test (\code{"WaldTest"});
#' the model with higher F value is chosen, provided that it is higher than \code{pftd}).
#' @param pftd (TRAMO SPECIFIC) \code{numeric}. The p-value used in the test specified by the automatic parameter (\code{tradingdays.mauto})
#' to assess the significance of the pre-tested calendar effects variables and whether they should be included in the RegArima model.
#'
#' @param leapyear a \code{character} to specify whether or not to include the leap-year effect in the model:
#' \code{"LeapYear"} = leap year effect; \code{"LengthOfPeriod"} = length of period (REGARIMA/X-13 specific), \code{"None"} = no effect included.
#'
#' @param leapyear.coef coefficient of the leapyear regressor.
#' @export
set_tradingdays<- function(x,
                           option = c(NA, "TradingDays", "WorkingDays", "None"),
                           uservariable = NA,
                           stocktd = NA,
                           test = c(NA, "None", "Remove", "Add", "Separate_T", "Joint_F"),
                           autoadjust = NA,
                           coef = NA,
                           # TRAMO SPECIFIC
                           automatic = c(NA, "Unused", "FTest", "WaldTest"),
                           pftd = NA,
                           # LEAP YEAR
                           leapyear = c(NA, "LeapYear", "LengthOfPeriod", "None"),
                           leapyear.coef = NA){
  UseMethod("set_tradingdays", x)
}

#' @export
set_tradingdays.default <- function(x,
                                    option = c(NA, "TradingDays", "WorkingDays", "None"),
                                    uservariable = NA,
                                    stocktd = NA,
                                    test = c(NA, "None", "Remove", "Add", "Separate_T", "Joint_F"),
                                    autoadjust = NA,
                                    coef = NA,
                                    # TRAMO SPECIFIC
                                    automatic = c(NA, "Unused", "FTest", "WaldTest"),
                                    pftd = NA,
                                    # LEAP YEAR
                                    leapyear = c(NA, "LeapYear", "LengthOfPeriod", "None"),
                                    leapyear.coef = NA){
  td <- x$regression$td

  is_tramo <- inherits(x, "JD3_TRAMO_SPEC")

  if(!missing(option) & !any(is.na(option))){
    option <- match.arg(toupper(option)[1],
                        choices = c("TRADINGDAYS", "WORKINGDAYS", "NONE"))
    td$td <- switch(option,
                    WORKINGDAYS = "TD2",
                    TRADINGDAYS = "TD7",
                    USERDEFINED = "TD_NONE",
                    NONE = "TD_NONE")
    td$users <- character()
  }
  if(!is.null(uservariable) &&
     !any(is.na(uservariable)) &&
     length(uservariable) > 0){
    td$td <- "TD_NONE"

    # delete the r. at the beginning of uservariable if supplied
    uservariable <- gsub("^r\\.", "", uservariable)
    uservariable <- sprintf("r.%s", uservariable)
    td$users <- uservariable
  }
  if(!missing(stocktd) && !is.na(stocktd)){
    td$users <- character()
    td$td <- "TD_NONE"
    td$w <- stocktd
  }
  if(!missing(autoadjust) && !is.na(autoadjust)){
    td$autoadjust <- autoadjust
  }

  if(!is.null(test) && !any(is.na(test))){
    if (is_tramo) {
      test <- match.arg(toupper(test)[1],
                        choices = c("SEPARATE_T", "JOINT_F", "NONE"))
      td$test <- sprintf("TEST_%s",
                         switch(test,
                                NONE = "NO",
                                test))
    }else{
      test <- match.arg(toupper(test)[1],
                        choices = c("REMOVE", "ADD", "NONE"))
      td$test <- switch(test,
                        NONE = "NO",
                        test)
    }
  }

  if (is_tramo) {

    if(!missing(automatic) & !any(is.na(automatic))){
      automatic <- match.arg(toupper(automatic)[1],
                             choices = c("UNUSED", "FTEST", "WALDTEST"))
      td$auto <- switch(automatic,
                        UNUSED = "AUTO_NO",
                        FTEST = "AUTO_FTEST",
                        WALDTEST = "AUTO_WALDTEST")
    }
    if(!missing(pftd) & !any(is.na(pftd))){
      td$ptest <- pftd
    }
  }

  if (!is.null(leapyear) && !any(is.na(leapyear))) {
    leapyear <- match.arg(toupper(leapyear),
                          choices = c("LEAPYEAR", "LENGTHOFPERIOD", "NONE"))
    if (leapyear != "LENGTHOFPERIOD" || (leapyear == "LENGTHOFPERIOD" & !is_tramo)) {
      # LENGTHOFPERIOD not available on TRAMO
      td$lp <- leapyear
    }
  }

  if(missing(coef) || is.null(coef)){
    # coef <- 0
  }else{
    warning("coef parameter not implemented")
  }
  if(missing(leapyear.coef) || is.null(leapyear.coef)){
    # coef <- 0
  }else{
    warning("leapyear.coef parameter not implemented")
  }
  td$tdcoefficients

  x$regression$td <- td
  x
}

#' Set Easter Specification
#'
#'
#' @inheritParams set_basic
#' @param enabled a logical indicating if the program considers the Easter effect in the model.
#'
#' @param julian a logical indicating if the program uses the Julian Easter (expressed in Gregorian calendar).
#'
#' @param duration a numeric indicating the duration of the Easter effect (length in days, between 1 and 20).
#'
#' @param test defines the pre-tests for the significance of the Easter effect based on the t-statistic
#' (the Easter effect is considered as significant if the t-statistic is greater than 1.96):
#' \code{"Add"} = the Easter effect variable is not included in the initial regression model but can be added
#' to the RegARIMA model after the test;
#' \code{"Remove"} = the Easter effect variable belongs to the initial regression model but can be removed
#' from the RegARIMA model after the test;
#' \code{"None"} = the Easter effect variable is not pre-tested and is included in the model.
#'
#' @param type (TRAMO specific) a \code{character} that specifies the presence and the length of the Easter effect:
#' \code{"Unused"} = the Easter effect is not considered; \code{"Standard"} = influences the period of \code{n} days strictly before Easter Sunday;
#' \code{"IncludeEaster"} = influences the entire period (\code{n}) up to and including Easter Sunday;
#' \code{"IncludeEasterMonday"} = influences the entire period (\code{n}) up to and including Easter Monday.
#' @export
set_easter<- function(x, enabled = NA,
                      julian = NA,
                      duration = NA,
                      test = c(NA, "Add", "Remove", "None"),
                      # TRAMO SPECIFIC
                      type = c(NA, "Unused", "Standard", "IncludeEaster", "IncludeEasterMonday")){
  UseMethod("set_easter", x)
}
#' @export
set_easter.default <- function(x, enabled = NA,
                               julian = NA,
                               duration = NA,
                               test = c(NA, "Add", "Remove", "None"),
                               # TRAMO SPECIFIC
                               type = c(NA, "Unused", "Standard", "IncludeEaster", "IncludeEasterMonday")){
  easter <- x$regression$easter

  # to set specific TRAMO/REGARIMA values
  is_tramo <- inherits(x, "JD3_TRAMO_SPEC")

  if(!is.null(test) && !any(is.na(test))){
    if (is_tramo) {
      if (!is.logical(test)) {
        test <- match.arg(toupper(test)[1],
                          choices = c("REMOVE", "ADD", "NONE")) != "NONE"
      }
      easter$test <- test
    } else {
      test <- match.arg(toupper(test)[1],
                        choices = c("REMOVE", "ADD", "NONE"))
      easter$test <- switch(test,
                            NONE = "NO",
                            test)
    }
  }
  if(!missing(enabled) && !is.na(enabled)){
    easter$type <- ifelse(enabled, "STANDARD", "UNUSED")
  }
  if (is_tramo && !is.null(type) && !any(is.na(type))) {
    # TRAMO SPECIFIC
    type <- match.arg(toupper(type)[1],
                      choices = c("UNUSED", "STANDARD", "INCLUDEEASTER", "INCLUDEEASTERMONDAY"))
    easter$type <- type
  }
  if(!missing(julian) && !is.na(julian)){
    if (is_tramo) {
      easter$julian <- julian
    } else {
      easter$type <- ifelse(julian, "JULIAN", easter$type)
    }
  }
  if(easter$type == "UNUSED"){
    if (is_tramo) {
      easter$test <- FALSE
    } else {
      easter$test <- "NO"
    }
  }
  if(!missing(duration) && !is.na(duration)){
    easter$duration <- duration
  }
  x$regression$easter <- easter
  x
}

#' Set Transform Specification
#'
#'
#' @inheritParams set_basic
#' @param fun the transformation of the input series: \code{"None"} = no transformation of the series;
#' \code{"Log"} = takes the log of the series; \code{"Auto"} = the program tests for the log-level specification.
#' @param adjust (REGARIMA/X-13 specific) pre-adjustment of the input series for the length of period or leap year effects:
#' \code{"None"} = no adjustment; \code{"LeapYear"} = leap year effect; \code{"LengthOfPeriod"} = length of period.
#' Modifications of this variable are taken into account only when \code{function = "Log"}.
#'
#' @param aicdiff (REGARIMA/X-13 specific)  a numeric defining the difference in AICC needed to accept no transformation when the automatic
#' transformation selection is chosen (considered only when \code{transform.function} is set to \code{"Auto"}).

#' @param fct (TRAMO specific) \code{numeric} controlling the bias in the log/level pre-test:
#' \code{ transform.fct }> 1 favours levels, \code{transform.fct}< 1 favours logs.
#' Considered only when \code{transform.function} is set to \code{"Auto"}.
#'
#' @export
set_transform<- function(x,
                         fun = c(NA, "Auto", "Log", "None"),
                         # REGARIMA SPECIFIC
                         adjust = c(NA, "None", "LeapYear", "LengthOfPeriod"),
                         aicdiff = NA,
                         # TRAMO SPECIFIC
                         fct = NA){
  UseMethod("set_transform", x)
}
#' @export
set_transform.default <- function(x,
                                  fun = c(NA, "Auto", "Log", "None"),
                                  # REGARIMA SPECIFIC
                                  adjust = c(NA, "None", "LeapYear", "LengthOfPeriod"),
                                  aicdiff = NA,
                                  # TRAMO SPECIFIC
                                  fct = NA){
  transform <- x$transform

  fun = match.arg(toupper(fun[1]),
                  c(NA, "AUTO", "LOG", "NONE"))
  # to set specific TRAMO/REGARIMA values
  is_tramo <- inherits(x, "JD3_TRAMO_SPEC")

  if(!is.na(fun)){
    transform$fn <- switch(fun,
                           "NONE" = "LEVEL",
                           fun)
  }
  if (is_tramo) {
    # TRAMO SPECIFIC PARAMETER
    if(!is.na(fct)){
      transform$fct <- fct
    }
  } else {
    # REGARIMA SPECIFIC PARAMETER
    adjust = match.arg(toupper(adjust[1]),
                       c(NA, "NONE", "LEAPYEAR", "LENGTHOFPERIOD"))
    if(!is.na(adjust)){
      transform$adjust = adjust
    }
    if(!is.na(aicdiff)){
      transform$aicdiff = aicdiff
    }
  }

  x$transform <- transform
  x
}
######### TODO

add_usrdefvar<- function(x,
                         variable,
                         variable.type,
                         variable.coef = 0){
  UseMethod("add_usrdefvar", x)
}
add_usrdefvar.JD3_REGARIMA_SPEC <- function(x,
                                            variable,
                                            variable.type,
                                            variable.coef = 0){
}
