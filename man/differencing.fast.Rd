% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/differencing.R
\name{differencing.fast}
\alias{differencing.fast}
\title{Automatic differencing}
\usage{
differencing.fast(data, period, mad = TRUE, centile = 90, k = 1.2)
}
\arguments{
\item{data}{Series being differenced.}

\item{period}{Period considered in the automatic differencing.}

\item{mad}{Use of MAD in the computation of the variance (true by default).}

\item{centile}{Percentage of the data used for computing the variance (90 by default).}

\item{k}{tolerance in the decrease of the variance. The algorithm stops if the new varance is higher than k*the old variance.}
}
\value{
Stationary transformation
\itemize{
\item ddata: data after differencing
\item mean: mean correction
\item differences:
\itemize{
\item lag: ddata(t)=data(t)-data(t-lag)
\item order: order of the differencing
}
}
}
\description{
The series is differentiated till its variance is decreasing.
}
\examples{
z<-rjd3modelling::differencing.fast(log(rjd3toolkit::ABS$X0.2.09.10.M),12)

}
