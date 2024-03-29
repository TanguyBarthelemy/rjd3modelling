#' @include protobuf.R jd3_r.R
NULL

#' Trading Days Test
#'
#' @param nyears \code{integer} that corresponds to number of periods number of periods starting from the end of the series:
#' in periods (positive value) or years (negative values).
#' By default (\code{nyears = 0}), the entire sample is used.
#' @param s a \code{ts} object that corresponds to the input time series to test.
#' @param model the model to use for the residuals. See details.
#'
#' @details
#' The function performs a residual seasonality test that is a joint F-Test on the coefficients of trading days regressors.
#' Several specifications can be used on the model:
#' \itemize{
#' \item \code{model = "WN"} the following model is used:
#' \deqn{
#' y_t - \bar y =\beta TD_t +  \varepsilon_t
#' }
#' \item \code{model = "D1"} (the default) the following model is used:
#' \deqn{
#' \Delta y_t - \overline{\Delta y} =\beta \Delta TD_t +  \varepsilon_t
#' }
#' \item \code{model = "DY"} the following model is used:
#' \deqn{
#' \Delta_s y_t - \overline{\Delta_s y} =\beta \Delta_s TD_t +  \varepsilon_t
#' }
#' \item \code{model = "DYD1"} the following model is used:
#' \deqn{
#' \Delta_s\Delta y_t - \overline{\Delta_s \Delta y} =\beta \Delta_s \Delta TD_t +  \varepsilon_t
#' }
#' \item \code{model = "AIRLINE"} the following model is used:
#' \deqn{
#' y_t =\beta TD_t +  \varepsilon_t \text{ with }\varepsilon_t \sim ARIMA(0,1,1)(0,1,1)
#' }
#' \item \code{model = "R011"} the following model is used:
#' \deqn{
#' y_t =\beta TD_t +  \varepsilon_t \text{ with }\varepsilon_t \sim ARIMA(0,1,1)(0,1,1)
#' }
#' \item \code{model = "R100"} the following model is used:
#' \deqn{
#' y_t =\alpha_0 + \alpha_1 y_{t-1} + \beta TD_t +  \varepsilon_t
#' }
#' }
#'
#' @examples
#' td.f(rjd3toolkit::ABS$X0.2.09.10.M)
#' @export
td.f<-function(s, model=c("D1", "DY", "DYD1", "WN", "AIRLINE", "R011", "R100"), nyears=0){
  model<-match.arg(model)
  jts<-rjd3toolkit::ts_r2jd(s)
  jtest<-.jcall("demetra/modelling/r/TradingDaysTests", "Ldemetra/stats/StatisticalTest;", "fTest",
                jts, model, as.integer(nyears))
  return (rjd3toolkit::jd2r_test(jtest))
}

#' Canova-Hansen Trading Days test
#'
#' @inheritParams td.f
#' @param differencing differencing lags.
#'
#' @return
#' @export
#'
#' @examples
td.ch<-function(s, differencing){
  jts<-rjd3toolkit::ts_r2jd(s)
  return (.jcall("demetra/modelling/r/TradingDaysTests", "[D", "chTest",
                jts, .jarray(as.integer(differencing))))
}
