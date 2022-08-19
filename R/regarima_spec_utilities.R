set_span <- function(x,
                     type = c("All", "From", "To", "Between", "Last", "First", "Excluding"),
                     d0 = NULL,
                     d1 = NULL,
                     n0 = 0,
                     n1 = 0){
  if(!missing(type) && !is.null(type) && !is.na(type[1])){
    type <- match.arg(toupper(type),
                      choices = c("ALL", "FROM", "TO", "BETWEEN", "LAST", "FIRST", "EXCLUDING"))
    if (type == "ALL") {
      x$type <- type
      x$d1 <- x$d1 <- NULL
      x$n0 <- x$n1 <- 0
    } else if (type == "FROM"){
      if(is.null(d0)){
        warning("d0 parameter must be defined")
      }else{
        x$type <- type
        x$d0 <- d0
        x$d1 <- NULL
        x$n0 <- x$n1 <- 0
      }
    } else if (type == "TO"){
      if(is.na(d1)){
        warning("d1 parameter must be defined")
      }else{
        x$type <- type
        x$d1 <- d1
        x$d0 <- NULL
        x$n0 <- x$n1 <- 0
      }
    } else if (type=="BETWEEN"){
      if(is.na(d0) | is.na(d1)){
        warning("d0 and d1 parameters must be defined")
      }else{
        x$type <- type
        x$d0 <- d0
        x$d1 <- d1
        x$n0 <- x$n1 <- 0
      }
    } else if (type=="FIRST"){
      if(is.na(n0)){
        warning("n0 parameter must be defined")
      }else{
        x$type <- type
        x$d0 <- x$d1 <- NULL
        x$n0 <- n0
        x$n1 <- 0
      }
    } else if (type=="LAST"){
      if(is.na(n1)){
        warning("n1 parameter must be defined")
      }else{
        x$type <- type
        x$d0 <- x$d1 <- NULL
        x$n0 <- 0
        x$n1 <- n1
      }
    } else if (type=="EXCLUDING"){
      if(is.na(n0) | is.na(n1)){
        warning("n0 and n1 parameters must be defined")
      }else{
        x$type <- type
        x$d0 <- x$d1 <- NULL
        x$n0 <- n0
        x$n1 <- n1
      }
    }
  }
  x
}
