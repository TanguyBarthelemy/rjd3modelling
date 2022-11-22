#' @import checkmate
#' @importFrom rjd3toolkit r2p_ts
#' @include calendars.R
NULL

JD3_DYNAMICTS<-'JD3_DYNAMICTS'
JD3_TSMONIKER<-'JD3_TSMONIKER'

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
  return (structure(c(source=source, id=id), class=c(JD3_TSMONIKER)))
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
  else if (is(r, JD3_DYNAMICTS)) p$dynamic_data<-.r2p_dynamic_ts(r)
  else return (NULL)
  return (p)
}

dynamicTs<-function(moniker, data){
  return (structure(list(moniker=moniker, data=data), class=c(JD3_DYNAMICTS)))
}

.p2r_dynamic_ts<-function(p){
  if (is.null(p)) return (NULL)
  s<-rjd3toolkit::p2r_ts(p$current)
  m<-.p2r_moniker(p$moniker)
  return (dynamicTs(m, s))
}

.r2p_dynamic_ts<-function(r){
  p<-jd3.DynamicTsData$new()
  p$current<-rjd3toolkit:: r2p_ts(r$data)
  p$moniker<-.r2p_moniker(r$moniker)
  return (p)
}

#' @export
#' @rdname jd3_utilities
.p2r_datasupplier<-function(p){
  if (p$has('dynamic_data')) return (.p2r_dynamic_ts(p$dynamic_data))
  if (p$has('data')) return (rjd3toolkit::p2r_ts(p$data))
  return (NULL)
}

#' @export
#' @rdname jd3_utilities
.r2p_datasuppliers<-function(r){
  if (! is.list(r)) stop("Suppliers should be a list")
  ns<-names(r)
  if (is.null(ns))
    stop("All the variables of the list should be named")
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

#' Create context
#'
#' @param calendars list of calendars.
#' @param variables list of variables.
#'
#' @return list of calendars and variables
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
      variables<-list(r=variables)
    }
  }

  return (list(calendars=calendars, variables=variables))
}


#' @export
#' @rdname jd3_utilities
.p2r_context<-function(p){
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
.r2p_context<-function(r){
  p<-jd3.ModellingContext$new()
  n<-length(r$calendars)
  if (n > 0){
    ns<-names(r$calendars)
    # To take into account empty calendars
    length_cal <- sapply(r$calendars, length)

    lcal<-lapply((1:n)[length_cal!=0], function(i){
      entry<-jd3.ModellingContext$CalendarsEntry$new()
      entry$key<-ns[i]
      entry$value<-.r2p_calendardef(r$calendars[[i]])
      return(entry)
      })
    if (length(lcal) > 0) {
      p$calendars<-lcal
    }
  }
  n<-length(r$variables)
  if (n > 0){
    ns<-names(r$variables)
    length_var <- sapply(r$variables, length)
    lvar<-lapply((1:n)[length_var!=0], function(i){
      entry<-jd3.ModellingContext$VariablesEntry$new()
      entry$key<-ns[i]
      entry$value<-.r2p_datasuppliers(r$variables[[i]])
      return(entry)
      })
    if (length(lvar) > 0) {
      p$variables=lvar
    }
  }
  return (p)
}

#' @export
#' @rdname jd3_utilities
.p2jd_context<-function(p){
  bytes<-p$serialize(NULL)
  jcal <- .jcall("demetra/util/r/Modelling", "Ldemetra/timeseries/regression/ModellingContext;",
                "of",
                bytes)
  return (jcal)
}

#' @export
#' @rdname jd3_utilities
.jd2p_context<-function(jd){
  bytes<-.jcall("demetra/util/r/Modelling", "[B", "toBuffer", jd)
  p<-RProtoBuf::read(jd3.ModellingContext, bytes)
  return (p)
}


#' @export
#' @rdname jd3_utilities
context2dict <- function(x) {
  .jcall("demetra/util/r/Dictionary",
        "Ldemetra/util/r/Dictionary;",
        "fromContext",
        x)
}

#' @export
#' @rdname jd3_utilities
.jd2r_modellingcontext<-function(jcontext){
  p<-.jd2p_context(jcontext)
  return (.p2r_context(p))
}

#' @export
#' @rdname jd3_utilities
.r2jd_modellingcontext<-function(r){
  p<-.r2p_context(r)
  return (.p2jd_context(p))
}

