#' @import checkmate
#' @importFrom methods is
#' @importFrom stats frequency is.ts start
#' @include protobuf.R jd3_r.R
NULL

HOLIDAY='JD3_HOLIDAY'
FIXEDDAY='JD3_FIXEDDAY'
FIXEDWEEKDAY='JD3_FIXEDWEEKDAY'
EASTERDAY='JD3_EASTERDAY'
SPECIALDAY='JD3_SPECIALDAY'
SINGLEDAY='JD3_SINGLEDAY'

#' Create a calendar
#'
#'
#'
#' @examples
#' belgiumCalendar<-calendar.new()
#' calendar.fixedday(belgiumCalendar, 7, 21)
#' calendar.holiday(belgiumCalendar, "NEWYEAR")
#' calendar.holiday(belgiumCalendar, "CHRISTMAS")
#' calendar.holiday(belgiumCalendar, "MAYDAY")
#' calendar.holiday(belgiumCalendar, "EASTERMONDAY")
#' calendar.holiday(belgiumCalendar, "WHITMONDAY")
#' calendar.holiday(belgiumCalendar, "ASSUMPTION")
#' calendar.holiday(belgiumCalendar, "ALLSAINTSDAY")
#' calendar.holiday(belgiumCalendar, "ARMISTICE")
#' calendar.singledate(belgiumCalendar, "2022-05-13")
#' M<-td(12, c(1980,1), 120, groups = c(1,1,1,1,2,3,0), contrasts = FALSE)
#'
#' H<-htd(belgiumCalendar, 12, c(1980,1), 120, groups = c(1,1,1,1,1,2,0), contrasts = FALSE)
#'
#' MC<-td(4, c(1980,1), 120, groups = c(1,1,1,1,1,2,0), contrasts = TRUE)
#' HC<-htd(belgiumCalendar, 4, c(1980,1), 120, groups = c(1,1,1,1,1,2,0), contrasts = TRUE)
#'
#' C12<-longTermMean(belgiumCalendar, 12)
#' C4<-longTermMean(belgiumCalendar, 4)
#'
#' C12bis<-longTermMean(belgiumCalendar, 12, c(1,1,1,1,1,2,0))
#' C4bis<-longTermMean(belgiumCalendar, 4, c(1,1,1,1,1,2,0))
#'
#' print(C12)
#' print(C12bis)
#' @export
calendar.new<-function(){
  return (jd3.Calendar$new())
}

#' @export
#' @rdname jd3_utilities
.r2p_validityPeriod<-function(start, end){
  vp<-jd3.ValidityPeriod$new()
  if (is.null(start)) {
    pstart=rjd3toolkit::DATE_MIN
  }else{
    pstart=rjd3toolkit::parseDate(start)
  }
  if (is.null(end)){
    pend=rjd3toolkit::DATE_MAX
  }else{
    pend=rjd3toolkit::parseDate(end)
  }
  vp$start<-pstart
  vp$end<-pend
  return (vp)
}


#' @export
#' @rdname jd3_utilities
.p2r_validityPeriod<-function(vp){
  pstart<-vp$start
  if (pstart == rjd3toolkit::DATE_MIN)
    start<-NULL
  else
    start<-as.Date(sprintf("%04i-%02i-%02i", pstart$year, pstart$month, pstart$day))

  pend<-vp$end
  if (pend == rjd3toolkit::DATE_MAX)
    end<-NULL
  else
    end<-as.Date(sprintf("%04i-%02i-%02i", pend$year, pend$month, pend$day))
  if (is.null(start) && is.null(end))
    return (NULL)
  else
    return (list(start=start, end=end))
}

#' @importFrom stats is.mts ts
length_ts <- function(s){
  if(is.mts(s)){
    nrow(s)
  }else{
    length(s)
  }
}
#' Add fixed day to a calendar
#'
#' @param calendar The calendar.
#' @param month,day the month and the day of the fixed day to add.
#' @param weight weight associated to the holiday.
#' @param start,end Validity period of the holiday in the format \code{"YYYY-MM-DD"}.
#'
#'
#' @examples
#' calendar <-calendar.new()
#' calendar.fixedday(calendar, 1, 1) # add New-Year
#' calendar.fixedday(calendar, 12, 25) # add Christmas
#' @export
calendar.fixedday<-function(calendar, month, day, weight=1, start=NULL, end=NULL){
  fd<-jd3.FixedDay$new()
  fd$month<-month
  fd$day<-day
  fd$weight<-weight
  fd$validity<-.r2p_validityPeriod(start, end)
  n<-1+length(calendar$fixed_days)
  calendar$fixed_days[[n]]<-fd
}

#' @export
fixed_day<-function(month, day, weight=1, validity=NULL){
  return (structure(list(month=month, day=day, weight=weight, validity=validity), class=c(FIXEDDAY, HOLIDAY)))
}

#' @export
#' @rdname jd3_utilities
.p2r_fixedday<-function(p){
  return (structure(list(month=p$month, day=p$day, weight=p$weight, validity=.p2r_validityPeriod(p$validity)), class=FIXEDDAY))
}

#' @export
#' @rdname jd3_utilities
.r2p_fixedday<-function(r){
  fd<-jd3.FixedDay$new()
  fd$month<-r$month
  fd$day<-r$day
  fd$weight<-r$weight
  if (is.null(r$validity))
    fd$validity<-.r2p_validityPeriod(NULL, NULL)
  else
    fd$validity<-.r2p_validityPeriod(r$validity$start, r$validity$end)

  return (fd)
}

#' Add fixed week day to a calendar
#'
#' @inheritParams calendar.fixedday
#' @param month the month of the day to add: from 1 (January) to 12 (December).
#' @param week the number of the week of the day to add: from 1 (first week of the month) to 5.
#' @param dayofweek the day of the week: from 1 (Monday) to 7 (Sunday).
#'
#'
#' @export
calendar.fixedweekday<-function(calendar, month, week,
                                dayofweek, weight=1, start=NULL, end=NULL){
  fd<-jd3.FixedWeekDay$new()
  fd$month<-month
  fd$position <- week
  fd$weekday <- dayofweek
  fd$weight<-weight
  fd$validity<-.r2p_validityPeriod(start, end)
  n<-1+length(calendar$fixed_week_days)
  calendar$fixed_week_days[[n]]<-fd
}

#' @export
fixed_week_day<-function(month, week, dayofweek, weight=1, validity=NULL){
  return (structure(list(month=month, week=week, dayofweek=dayofweek, weight=weight, validity=validity), class=c(FIXEDWEEKDAY, HOLIDAY)))
}

#' @export
#' @rdname jd3_utilities
.p2r_fixedweekday<-function(p){
  return (fixed_week_day(p$month, week=p$position, dayofweek=p$weekday, weight=p$weight, validity=.p2r_validityPeriod(p$validity)))
}

#' @export
#' @rdname jd3_utilities
.r2p_fixedweekday<-function(r){
  fd<-jd3.FixedWeekDay$new()
  fd$month<-r$month
  fd$position <- r$week
  fd$weekday <- r$dayofweek
  fd$weight<-r$weight
  if (is.null(r$validity))
    fd$validity<-.r2p_validityPeriod(NULL, NULL)
  else
    fd$validity<-.r2p_validityPeriod(r$validity$start, r$validity$end)
  return (fd)
}

#' Add Easter related day to a calendar
#'
#' @inheritParams calendar.fixedday
#' @param offset The position of the holiday in relation to the Easter Sunday, measured in days (can be positive or negative).
#' @param julian boolean indicating if julian calendar must be used.
#'
#' @export
#' @examples
#' calendar <- calendar.new()
#' calendar.easter(calendar, 1) # add Easter Monday
#' calendar.easter(calendar, -2) # add Easter Good Friday
calendar.easter<-function(calendar, offset, julian=FALSE, weight=1, start=NULL, end=NULL){
  ed<-jd3.EasterRelatedDay$new()
  ed$offset<-offset
  ed$julian<-julian
  ed$weight<-weight
  ed$validity<-.r2p_validityPeriod(start, end)
  n<-1+length(calendar$easter_related_days)
  calendar$easter_related_days[[n]]<-ed
}

#' @export
easter_day<-function(offset, julian=FALSE, weight=1, validity=NULL){
  return (structure(list(offset=offset, julian=julian, weight=weight, validity=validity), class=c(EASTERDAY, HOLIDAY)))
}

#' @export
#' @rdname jd3_utilities
.p2r_easterday<-function(p){
  return (easter_day(p$offset, p$julian, p$weight, .p2r_validityPeriod(p$validity)))
}

#' @export
#' @rdname jd3_utilities
.r2p_easterday<-function(r){
  fd<-jd3.EasterRelatedDay$new()
  fd$offset<-r$offset
  fd$julian<-r$julian
  fd$weight<-r$weight
  if (is.null(r$validity))
    fd$validity<-.r2p_validityPeriod(NULL, NULL)
  else
    fd$validity<-.r2p_validityPeriod(r$validity$start, r$validity$end)
  return (fd)
}


#' Add Single Date to a Calendar
#'
#' @inheritParams calendar.fixedday
#' @param date the date of the holiday in the format `"YYYY-MM-DD"`.
#'
#' @export
#'
#' @examples
calendar.singledate<-function(calendar, date, weight=1){
  sd<-jd3.SingleDate$new()
  sd$date<-rjd3toolkit::parseDate(date)
  sd$weight<-weight
  n<-1+length(calendar$single_dates)
  calendar$single_dates[[n]]<-sd
}

#' @export
single_day<-function(date, weight=1){
  return (structure(list(date=date, weight=weight), class=c(SINGLEDAY, HOLIDAY)))
}

#' @export
#' @rdname jd3_utilities
.p2r_singleday<-function(p){
  return (single_day(rjd3toolkit::p2r_date(p$date), p$weight))
}

#' @export
#' @rdname jd3_utilities
.r2p_singleday<-function(r){
  sd<-jd3.SingleDate$new()
  sd$date<-rjd3toolkit::parseDate(r$date)
  sd$weight<-r$weight
  return (sd)
}

#' Add specific holiday to a calendar
#'
#' @inheritParams calendar.easter
#' @param offset The position of the holiday in relation to the selected pre-specified holiday measured in days (can be positive or negative).
#' By default \code{offset = 0}.
#' @param event the event to add (see details).
#'
#' @details Possible values :
#'
#' \tabular{ll}{
#' NEWYEAR        \tab Fixed holiday, falls on January, 1.                                                  \cr
#' SHROVEMONDAY   \tab Moving holiday, falls on Monday before Ash Wednesday (48 days before Easter Sunday). \cr
#' SHROVETUESDAY  \tab Moving holiday, falls on Tuesday before Ash Wednesday (47 days before Easter Sunday).\cr
#' ASHWEDNESDAY   \tab Moving holiday, occurring 46 days before Easter Sunday.                              \cr
#' MAUNDYTHURSDAY \tab Moving holiday, falls on the Thursday before Easter.                                 \cr
#' GOODFRIDAY     \tab Moving holiday, falls on the Friday before Easter.                                   \cr
#' EASTER         \tab Moving holiday, varies between March, 22 and April, 25.                              \cr
#' EASTERMONDAY   \tab Moving holiday, falls on the day after Easter.                                       \cr
#' ASCENSION      \tab Moving holiday, celebrated on Thursday, 39 days after Easter.                        \cr
#' PENTECOST      \tab Moving holiday, celebrated 49 days after Easter Sunday.                              \cr
#' WHITMONDAY     \tab Moving holiday, falling on the day after Pentecost.                                  \cr
#' CORPUSCHRISTI  \tab Moving holiday, celebrated 60 days after Easter Sunday.                              \cr
#' JULIANEASTER   \tab                                                                                      \cr
#' MAYDAY         \tab Fixed holiday, falls on May, 1.                                                      \cr
#' ASSUMPTION     \tab Fixed holiday, falls on August, 15.                                                  \cr
#' HALLOWEEN      \tab Fixed holiday, falls on October, 31.                                                 \cr
#' ALLSAINTSDAY    \tab Fixed holiday, falls on November, 1.                                                 \cr
#' ARMISTICE      \tab Fixed holiday, falls on November, 11.                                                \cr
#' CHRISTMAS      \tab Fixed holiday, falls on December, 25.
#' }
#'
#'
#' @export
#' @examples
#' calendar <- calendar.new()
#' calendar.holiday(calendar, "EASTERMONDAY") # add Easter Monday
calendar.holiday<-function(calendar, event, offset=0, weight=1, start=NULL, end=NULL){
  pd<-jd3.PrespecifiedHoliday$new()
  pd$event<-rjd3toolkit::enum_of(jd3.CalendarEvent, event, "HOLIDAY")
  pd$offset<-offset
  pd$weight<-weight
  pd$validity<-.r2p_validityPeriod(start, end)
  n<-1+length(calendar$prespecified_holidays)
  calendar$prespecified_holidays[[n]]<-pd
}

#' @export
special_day<-function(event, offset=0, weight=1, validity=NULL){
  return (structure(list(event=event, offset=offset, weight=weight, validity=validity), class=c(SPECIALDAY, HOLIDAY)))
}

#' @export
#' @rdname jd3_utilities
.p2r_specialday<-function(p){
  return (special_day(rjd3toolkit::enum_extract(jd3.CalendarEvent, p$event), p$offset, p$weight, .p2r_validityPeriod(p$validity)))
}

#' @export
#' @rdname jd3_utilities
.r2p_specialday<-function(r){
  pd<-jd3.PrespecifiedHoliday$new()
  pd$event<-rjd3toolkit::enum_of(jd3.CalendarEvent, r$event, "HOLIDAY")
  pd$offset<-r$offset
  pd$weight<-r$weight
  if (is.null(r$validity))
    pd$validity<-.r2p_validityPeriod(NULL, NULL)
  else
    pd$validity<-.r2p_validityPeriod(r$validity$start, r$validity$end)
  return (pd)
}

p2jd_calendar<-function(pcalendar){
  bytes<-pcalendar$serialize(NULL)
  jcal<-.jcall("demetra/calendar/r/Calendars", "Ldemetra/timeseries/calendars/Calendar;",
               "calendarOf", bytes)
  return (jcal)
}

group_names <- function(x, contrasts = TRUE){
  if(!is.matrix(x))
    return(x)
  col_names <- seq_len(ncol(x)) - !contrasts #if !constrast then it starts from 0
  colnames(x) <- sprintf("group-%i", col_names)
  x
}

#' Usual trading days variables
#'
#' @param frequency Annual frequency (divisor of 12).
#' @param start,length First date (array with the first year and the first period)
#' (for instance `c(1980, 1)`) and number of periods of the output variables. Can also be provided with the `s` argument
#' @param s time series used to get the dates for the trading days variables. If supplied the
#' parameters `frequency`, `start` and `length` are ignored.
#' @param groups Groups of days. The length of the array must be 7. It indicates to what group each week day
#' belongs. The first item corresponds to Mondays and the last one to Sundays. The group used for contrasts (usually Sundays) is identified by 0.
#' The other groups are identified by 1, 2,... n (<= 6). For instance, usual trading days are defined by c(1,2,3,4,5,6,0),
#' week days by c(1,1,1,1,1,0,0), week days, Saturdays, Sundays by c(1,1,1,1,1,2,0) etc...
#' @param contrasts If true, the variables are defined by contrasts with the 0-group. Otherwise, raw number of days are provided
#'
#' @return The variables corresponding to each group, starting with the 0-group (\code{contrasts = FALSE})
#' or the 1-group (\code{contrasts = TRUE}).
#' @export
#'
#' @examples
td<-function(frequency, start, length, s, groups=c(1,2,3,4,5,6,0), contrasts=TRUE){
  if (!missing(s) && is.ts(s)) {
    frequency = stats::frequency(s)
    start = stats::start(s)
    length = length_ts(s)
  }
  jdom<-rjd3toolkit::tsdomain_r2jd(frequency, start[1], start[2], length)
  igroups<-as.integer(groups)
  jm<-.jcall("demetra/modelling/r/Variables", "Ldemetra/math/matrices/Matrix;",
             "td", jdom, igroups, contrasts)
  data <- rjd3toolkit::matrix_jd2r(jm)
  data <- group_names(data, contrasts = contrasts)
  return (ts(data, start = start, frequency = frequency))
}


#' Calendar specific trading days variables
#'
#' @inheritParams td
#' @param calendar The calendar.
#' @param holiday Day for holidays (holidays are considered as that day). 1 for Monday... 7 for Sunday. Doesn't necessary belong to the 0-group.
#' @param meanCorrection boolean indicating if the regressors are corrected for long-term term.
#' By default the correction is done if \code{contrasts = TRUE}.
#'
#' @return The variables corresponding to each group, starting with the 0-group (\code{contrasts = FALSE})
#' or the 1-group (\code{contrasts = TRUE}).
#' @export
#'
#' @examples
htd<-function(calendar,frequency, start, length, s, groups=c(1,2,3,4,5,6,0), holiday=7, contrasts=TRUE,
              meanCorrection = contrasts){
  if (!missing(s) && is.ts(s)) {
    frequency = stats::frequency(s)
    start = stats::start(s)
    length = length_ts(s)
  }
  jdom<-rjd3toolkit::tsdomain_r2jd(frequency, start[1], start[2], length)
  jcal<-p2jd_calendar(calendar)
  jm<-.jcall("demetra/modelling/r/Variables", "Ldemetra/math/matrices/Matrix;",
              "htd", jcal, jdom, as.integer(groups), as.integer(holiday), contrasts, meanCorrection)
  return <- rjd3toolkit::matrix_jd2r(jm)
  return <- group_names(return, contrasts = contrasts)
  return (ts(return, start = start, frequency = frequency))
}


#' Gets the days corresponding to the holidays
#'
#' @param calendar The calendar
#' @param start First day of the calendar in the format \code{"YYYY-MM-DD"}.
#' @param length Length of the calendar.
#' @param nonworking Indexes of non working days (Monday=1, Sunday=7).
#' @param type Adjustment type when a holiday falls a week-end: \code{"NextWorkingDay"},
#' \code{"PreviousWorkingDay"},
#' \code{"Skip"} (holidays corresponding to non working days are simply skipped in the matrix),
#' \code{"All"} (holidays are always put in the matrix, even if they correspond to a non working day).
#' @param single boolean indication if a single variable (`TRUE`) should be return or a matrix (`FALSE`, the default) containing the different holidays in separate columns.
#' @returns A matrix where each column is associated to a holiday (in the order of creation of the holiday) and each row to a date.
#'
#' @examples
#' belgiumCalendar<-calendar.new()
#' calendar.fixedday(belgiumCalendar, 7, 21)
#' calendar.holiday(belgiumCalendar, "NEWYEAR")
#' calendar.holiday(belgiumCalendar, "CHRISTMAS")
#' calendar.holiday(belgiumCalendar, "CHRISTMAS", offset=1, weight=.5)
#' calendar.holiday(belgiumCalendar, "MAYDAY")
#' calendar.holiday(belgiumCalendar, "EASTERMONDAY")
#' calendar.holiday(belgiumCalendar, "WHITMONDAY")
#' calendar.holiday(belgiumCalendar, "ASSUMPTION")
#' calendar.holiday(belgiumCalendar, "ALLSAINTSDAY")
#' calendar.holiday(belgiumCalendar, "ARMISTICE")
#' q<-holidays(belgiumCalendar, "2021-01-01", 365.25*10, type="NextWorkingDay")
#' plot(apply(q,1, max))
#' @export
holidays<-function(calendar, start, length, nonworking=c(6,7), type=c("Skip", "All", "NextWorkingDay", "PreviousWorkingDay"), single=FALSE){
  type<-match.arg(type)
  jcal<-p2jd_calendar(calendar)
  jm<-.jcall("demetra/calendar/r/Calendars", "Ldemetra/math/matrices/Matrix;",
             "holidays", jcal, as.character(start), as.integer(length), .jarray(as.integer(nonworking)), type,  as.logical(single))
  res <- rjd3toolkit::matrix_jd2r(jm)
  rownames(res) <- as.character(seq(as.Date(start), length.out = nrow(res), by="days"))
  return (res)

}

#' Long-term means of a calendar
#'
#' @inheritParams htd
#'
#' @return The long term means corresponding to each group/period, starting with the 0-group.
#' @export
#'
#' @examples
longTermMean<-function(calendar,frequency,groups=c(1,2,3,4,5,6,0), holiday=7){
  jcal<-p2jd_calendar(calendar)
  jm<-.jcall("demetra/calendar/r/Calendars", "Ldemetra/math/matrices/Matrix;",
             "longTermMean", jcal, as.integer(frequency), as.integer(groups), as.integer(holiday))
  res <- rjd3toolkit::matrix_jd2r(jm)
  return (group_names(res, contrasts = FALSE))
}

#' Compute Easter days between two years
#'
#' @param year0,year1 years.
#' @inheritParams calendar.easter
#'
#' @export
#'
#' @examples
#' easter.dates(2020, 2021)
easter.dates<-function(year0, year1, julian = FALSE){
  dates<-.jcall("demetra/calendar/r/Calendars", "[S", "easter", as.integer(year0), as.integer(year1), as.logical(julian))
  return (sapply(dates, as.Date))
}

#' Stock Trading days
#'
#' @inheritParams td
#' @param w indicates day of the month when inventories and other stock are reported (to denote the last day of the month enter 31).
#' @export
stock.td<-function(frequency, start, length, s, w = 31){
  if (!missing(s) && is.ts(s)) {
    frequency = stats::frequency(s)
    start = stats::start(s)
    length = length_ts(s)
  }
  jdom <- rjd3toolkit::tsdomain_r2jd(frequency, start[1], start[2], length)
  jm<-.jcall("demetra/modelling/r/Variables", "Ldemetra/math/matrices/Matrix;", "stockTradingDays", jdom, as.integer(w))
  data <- rjd3toolkit::matrix_jd2r(jm)
  colnames(data) <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  return (ts(data, frequency = frequency, start= start))
}

#' @export
#' @rdname jd3_utilities
.p2r_calendar<-function(p){
  return (structure(
    c(lapply(p$fixed_days, function(z) .p2r_fixedday(z)),
      lapply(p$fixed_week_days, function(z) .p2r_fixedweekday(z)),
      lapply(p$easter_related_days, function(z) .p2r_easterday(z)),
      lapply(p$prespecified_holidays, function(z) .p2r_specialday(z)),
      lapply(p$single_dates, function(z) .p2r_singleday(z))
  ), class=c('JD3_CALENDAR', 'JD3_CALENDARDEFINITION')))
}

#' @export
#' @rdname jd3_utilities
.r2p_calendar<-function(r){
  p<-jd3.Calendar$new()
  #select fixed days
  sel<-which(sapply(r,function(z) is(z, FIXEDDAY)))
  p$fixed_days<-lapply(r[sel], function(z) .r2p_fixedday(z))
  #select fixed week days
  sel<-which(sapply(r,function(z) is(z, FIXEDDAY)))
  p$fixed_week_days<-lapply(r[sel], function(z) .r2p_fixedweekday(z))
  # select easter days
  sel<-which(sapply(r,function(z) is(z, EASTERDAY)))
  p$easter_related_days<-lapply(r[sel], function(z) .r2p_easterday(z))
  # select special days
  sel<-which(sapply(r,function(z) is(z, SPECIALDAY)))
  p$prespecified_holidays<-lapply(r[sel], function(z) .r2p_specialday(z))
  # select single days
  sel<-which(sapply(r,function(z) is(z, SINGLEDAY)))
  p$single_dates<-lapply(r[sel], function(z) .r2p_singleday(z))
  return (p)
}

#' Title
#'
#' @param calendar1
#' @param calendar2
#' @param break_date
#'
#' @return
#' @export
#'
#' @examples
chained_calendar<-function(calendar1, calendar2, break_date){
  return (structure(list(
    calendar1=calendar1,
    calendar2=calendar2,
    break_date=break_date
  )), class=c('JD3_CHAINEDCALENDAR', 'JD3_CALENDARDEFINITION'))
}

#' @export
#' @rdname jd3_utilities
.p2r_chainedcalendar<-function(p){
  return (chained_calendar(p$calendar1, p$calendar2, rjd3toolkit::p2r_date(p$break_date)))
}

#' @export
#' @rdname jd3_utilities
.r2p_chainedcalendar<-function(r){
  pc<-jd3.ChainedCalendar$new()
  pc$calendar1<-.r2p_calendardef(r$calendar1)
  pc$calendar2<-.r2p_calendardef(r$calendar2)
  pc$break_date<-rjd3toolkit::parseDate(r$break_date)
  return (pc)
}

#' Title
#'
#' @param calendars
#' @param weights
#'
#' @return
#' @export
#'
#' @examples
weighted_calendar<-function(calendars, weights){
  checkmate::assertNames(calendars)
  checkmate::assertNumeric(weights)
  if (length(calendars) != length(weights)) stop("Calendars and weights should have the same length")

  return (structure(list(calendars=calendars, weights=weights)), class=c('JD3_WEIGHTEDCALENDAR', 'JD3_CALENDARDEFINITION'))
}

#' @export
#' @rdname jd3_utilities
.p2r_wcalendar<-function(p){
  calendars<-sapply(p, function(item){return (item$calendar)})
  weights<-sapply(p, function(item){return (item$weights)})
  return (weighted_calendar(calendars, weights))

}

#' @export
#' @rdname jd3_utilities
.r2p_wcalendar<-function(r){
  n<-length(r$calendars)
  p$items<-lapply(1:n, function(i){return (list(calendar=r$calendars[i], weight=r$weights[i]))})
}

#' @export
#' @rdname jd3_utilities
.p2r_calendardef<-function(p){
  if (p$has('calendar')) return (.p2r_calendar(p$calendar))
  if (p$has('chained_calendar')) return (.p2r_chainedcalendar(p$chained_calendar))
  if (p$has('weighted_calendar')) return (.p2r_wcalendar(p$weighted_calendar))
  return (NULL)
}

#' @export
#' @rdname jd3_utilities
.r2p_calendardef<-function(r){
  p<-jd3.CalendarDefinition$new()
  if (is(r, 'JD3_CALENDAR')){p$calendar<-.r2p_calendar(r)}
  else if (is(r, 'JD3_CHAINEDCALENDAR')){p$calendar<-.r2p_chainedcalendar(r)}
  else if (is(r, 'JD3_WEIGHTEDCALENDAR')){p$calendar<-.r2p_wcalendar(r)}
  return (p)
}


national_calendar<-function(days){

}



