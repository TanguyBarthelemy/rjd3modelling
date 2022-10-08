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
sarima_coef_table <- function(x, cov = NULL, ndf = NULL,...){
  m <- x
  if (! is.null(m$phi)) p<-dim(m$phi)[2]else p<-0
  if (! is.null(m$theta)) q<-dim(m$theta)[2]else q<-0
  if (! is.null(m$bphi)) bp<-dim(m$bphi)[2]else bp<-0
  if (! is.null(m$btheta)) bq<-dim(m$btheta)[2]else bq<-0
  sarima_orders = list(p = p, d = m$d, q = q, bp = bp, bd = m$bd, bq = bq)
  names<-NULL
  if (p > 0){names=c(names,paste0("phi(", 1:p, ')')) }
  if (q > 0){names=c(names,paste0("theta(", 1:q, ')')) }
  if (bp > 0){names=c(names,paste0("bphi(", 1:bp, ')')) }
  if (bq > 0){names=c(names,paste0("btheta(", 1:bq,')')) }
  if (! is.null(names)){
    all<-t(cbind(m$phi, m$theta, m$bphi, m$btheta))
    fr<-as.data.frame(all, row.names = names)
    for(i in colnames(fr)){
      fr[,i] <- unlist(fr[,i])
    }
    if(!is.null(cov) & !is.null(ndf)){
      fr$pvalue <- fr$t <- fr$stde <- NA
      stde<-sqrt(diag(cov))
      sel<-fr$type=='ESTIMATED'
      t<-fr$value[sel]/stde
      pval<-2*pt(abs(t), ndf, lower.tail = F)
      fr$stde[sel]<-stde
      fr$t[sel]<-t
      fr$pvalue[sel]<-pval
      colnames(fr) <- c("Estimate", "Type", "Std. Error",
                        "T-stat", "Pr(>|t|)")
    }else{
      colnames(fr) <- c("Estimate", "Type")
    }
  }else{
    fr <- NULL
  }
  list(sarima_orders = sarima_orders,
       coef_table = fr)
}
regarima_coef_table <- function(x,...){
  q <- x
  if (length(q$description$variables)>0){
    regs<-do.call("rbind", lapply(q$description$variables, function(z){z$coeff}))
    xregs<-cbind(regs, stde=NA, t=NA, pvalue=NA)
    stde<-sqrt(diag(q$estimation$bvar))
    sel<-xregs$type=='ESTIMATED'
    t<-xregs$value[sel]/stde
    ndf<-q$estimation$likelihood$neffectiveobs-q$estimation$likelihood$nparams+1
    pval<-2*pt(abs(t), ndf, lower.tail = F)
    xregs$stde[sel]<-stde
    xregs$t[sel]<-t
    xregs$pvalue[sel]<-pval
    colnames(xregs) <- c("Estimate", "Type", "Std. Error",
                         "T-stat", "Pr(>|t|)")
    xregs
  }else{
    NULL
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

