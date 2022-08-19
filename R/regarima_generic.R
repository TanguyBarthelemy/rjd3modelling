# Method "JD3_REGARIMA_RSLTS" for the function coef
#' @importFrom stats coef df.residual logLik residuals vcov nobs
#' @export
coef.JD3_REGARIMA_RSLTS <- function(object, component = c("regression", "arima", "both"), ...){
  if (is.null(object))
    return(NULL)

  component <- match.arg(component)
  if (component == "regression") {
    regarima_coef_table(object)[,1]
  } else if (component == "arima") {
    sarima_coef_table(object$description$arima)$coef_table[,1]
  } else{
    c(sarima_coef_table(object$description$arima)$coef_table[,1],
      regarima_coef_table(object)[,1])
  }
}

# Method "JD3_REGARIMA_RSLTS" for the function logLik
#' @export
logLik.JD3_REGARIMA_RSLTS <- function(object, ...) {
  if (is.null(object) || is.null(object$estimation$likelihood$ll)) {
    res <- NA
  }else{
    res <- structure(object$estimation$likelihood$ll,
                     df = object$estimation$likelihood$nparams + 1,
                     nall = object$estimation$likelihood$nobs,
                     nobs = object$estimation$likelihood$neffectiveobs)
  }
  class(res) <- "logLik"
  res
}
#' @export
vcov.JD3_REGARIMA_RSLTS <- function(object, ...){
  if (is.null(object))
    return(NULL)
  object$estimation$bvar
}
#' @export
df.residual.JD3_REGARIMA_RSLTS <- function(object, ...){
  if (is.null(object))
    return(NULL)

  object$estimation$likelihood$neffectiveobs - object$estimation$likelihood$nparams
}
#' @export
nobs.JD3_REGARIMA_RSLTS <- function(object, ...){
  if (is.null(object))
    return(NULL)
  object$estimation$likelihood$neffectiveobs
}

#' @export
residuals.JD3_REGARIMA_RSLTS <- function(object, ...){
  if (is.null(object))
    return(NULL)

  object$estimation$res
}

