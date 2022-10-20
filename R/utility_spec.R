#' Utility Functions For Specifications
#'
#' Utility functions to set specifications used in other packages
#'
#' @param start,end,name,coef,code,pos,id,lag0,lag1,regeffect  parameters.
#'
#' @name utility-spec
#' @rdname utility-spec
#' @export
createVariable<-function(id, name = NULL, lag0 = 0, lag1 = 0, coef = NULL, regeffect=c("Undefined", "Trend", "Seasonal", "Irregular", "Series", "SeasonallyAdjusted")){
  regeffect=match.arg(regeffect)
  if (is.null(name)) {
    name<-id
  }
  res = list(id=id, name=name, lags=rlags(lag0, lag1), coef = fixedParameters(coef), regeffect=regeffect)
  return (res)
}

#' @rdname utility-spec
#' @export
createRamp<-function(start, end, name = NULL, coef=NULL){
  res = list(name=name, start=start, end=end, coef = fixedParameter(coef))
  return (res)
}
#' @rdname utility-spec
#' @export
createOutlier<-function(code, pos, name = NULL, coef=NULL){
  res = list(name=name, pos=pos, code=code, coef = fixedParameter(coef))
  return (res)
}

fixedParameters<-function(coef){
  ncoef<-length(coef)
  if (ncoef == 0)return (NULL)
  l<-lapply(coef, function(v){list(value=v, type='FIXED')})
  return (l)
}

fixedParameter<-function(coef){
  if (is.null(coef)) return (NULL)
  if (coef == 0) return (NULL)
  return (list(value=coef, type='FIXED'))
}

