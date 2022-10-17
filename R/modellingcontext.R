#' @import checkmate
#' @importFrom rjd3toolkit r2p_ts
#' @include calendars.R
NULL

#' Title
#'
#' @param source Source of the time series
#' @param id Id of the time series
#'
#' @return
#' @export
#'
#' @examples
tsmoniker<-function(source, id){
  return (structure(c(source=source, id=id), class=c('JD3_TSMONIKER')))
}

#' @export
#' @rdname jd3_utilities
.r2p_moniker<-function(r){
  p<-jd3.TsMoniker$new()
  p$source<-r['source']
  p$id<-r['id']
  return (p)
}

#' @export
#' @rdname jd3_utilities
.p2r_moniker<-function(p){
  if (is.null(p)) return (NULL)
  return (tsmoniker(p$source, p$id))
}

#' @export
#' @rdname jd3_utilities
.r2p_datasupplier<-function(name, r){
  p<-jd3.TsDataSuppliers$Item$new()
  p$name<-name
  if (is.ts(r)) p$data<-rjd3toolkit::r2p_ts(r)
  else if (is(r, 'JD3_TSMONIKER')) p$moniker<-.r2p_moniker(r)
  else return (NULL)
  return (p)
}

#' @export
#' @rdname jd3_utilities
.p2r_datasupplier<-function(p){
  if (p$has('moniker')) return (.p2r_moniker(p$moniker))
  if (p$has('data')) return (rjd3toolkit::p2r_ts(p$data))
  return (NULL)
}

#' @export
#' @rdname jd3_utilities
.r2p_datasuppliers<-function(r){
  if (! is.list(r)) stop("Suppliers should be a list")
  ns<-names(r)
  n<-length(ns)
  all<-lapply(1:n, function(z){.r2p_datasupplier(ns[z], r[[z]])})
  p<-jd3.TsDataSuppliers$new()
  p$items<-all
  return (p)
}

#' @export
#' @rdname jd3_utilities
.p2r_datasuppliers<-function(p){
  n<-length(p$items)
  if (n == 0){return (list())}
  l<-lapply(1:n, function(i){return(.p2r_datasupplier(p$items[[i]]))})
  ns<-sapply(1:n, function(i){return(p$items[[i]]$name)})
  names(l)<-ns
  return (l)
}

#' Title
#'
#' @param calendars Calendars
#' @param variables Variables
#'
#' @return
#' @export
#'
#' @examples
#' BE<-national_calendar(list(
#'     fixed_day(7,21),
#'     special_day('NEWYEAR'),
#'     special_day('CHRISTMAS'),
#'     special_day('MAYDAY'),
#'     special_day('EASTERMONDAY'),
#'     special_day('ASCENSION'),
#'     special_day('WHITMONDAY'),
#'     special_day('ASSUMPTION'),
#'     special_day('ALLSAINTSDAY'),
#'     special_day('ARMISTICE')))
#' FR<-national_calendar(list(
#'     fixed_day(5,8),
#'     fixed_day(7,14),
#'     special_day('NEWYEAR'),
#'     special_day('CHRISTMAS'),
#'     special_day('MAYDAY'),
#'     special_day('EASTERMONDAY'),
#'     special_day('ASCENSION'),
#'     special_day('WHITMONDAY'),
#'     special_day('ASSUMPTION'),
#'     special_day('ALLSAINTSDAY'),
#'     special_day('ARMISTICE')))
#' #simple list of ts
#' vars<-list(v1=rjd3toolkit::ABS$X0.2.09.10.M, v2=rjd3toolkit::ABS$X0.2.05.10.M)
#' MC<-modelling_context(calendars=list(BE=BE, FR=FR), variables<-vars)
modelling_context<-function(calendars=NULL, variables=NULL){
  if (is.null(calendars))calendars<-list()
  if (is.null(variables))variables<-list()
  if (! is.list(calendars)) stop("calendars should be a list of calendars")
  if (length(calendars)>0) if (length(calendars) != length(which(sapply(calendars,function(z) is(z, 'JD3_CALENDARDEFINITION'))))) stop("calendars should be a list of calendars")
  if (! is.list(variables)) stop("variables should be a list of vars")
  if (length(variables) != 0){
    # case of a simple ts dictionary
    if (! is.list(variables[[1]])){
      # Use 'r' as the name of the dictionary
      variables<-list(R=variables)
    }
  }

  return (list(calendars=calendars, variables=variables))
}


#' @export
#' @rdname jd3_utilities
.p2r_modellingcontext<-function(p){
  n<-length(p$calendars)
  lcal <- lvar <- NULL
  if (n > 0){
    lcal<-lapply(1:n, function(i){return(.p2r_calendardef(p$calendars[[i]]$value))})
    ns<-sapply(1:n, function(i){return(p$calendars[[i]]$key)})
    names(lcal)<-ns
  }
  n<-length(p$variables)
  if (n > 0){
    lvar<-lapply(1:n, function(i){return(.p2r_datasuppliers(p$variables[[i]]$value))})
    ns<-sapply(1:n, function(i){return(p$variables[[i]]$key)})
    names(lvar)<-ns
  }
  return (list(calendars=lcal, variables=lvar))
}

#' @export
#' @rdname jd3_utilities
.r2p_modellingcontext<-function(r){
  p<-jd3.ModellingContext$new()
  n<-length(r$calendars)
  if (n > 0){
    ns<-names(r$calendars)
    lcal<-lapply(1:n, function(i){
      entry<-jd3.ModellingContext$CalendarsEntry$new()
      entry$key<-ns[i]
      entry$value<-.r2p_calendardef(r$calendars[[i]])
      return(entry)
      })
    p$calendars<-lcal
  }
  n<-length(r$variables)
  if (n > 0){
    ns<-names(r$variables)
    lvar<-lapply(1:n, function(i){
      entry<-jd3.ModellingContext$VariablesEntry$new()
      entry$key<-ns[i]
      entry$value<-.r2p_datasuppliers(r$variables[[i]])
      return(entry)
      })
    p$variables=lvar
  }
  return (p)
}

