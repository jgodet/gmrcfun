% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cs01.r
\name{cs01}
\alias{cs01}
\title{Normalize data to 0-1 range}
\usage{
cs01(X, mini = min(X), maxi = max(X))
}
\arguments{
\item{X}{numerical vector}

\item{min}{either min of X or user define value}

\item{max}{either min of X or user define value}
}
\value{
numeric()
}
\description{
Normalize data to 0-1 range
}
\examples{
X <- runif(n = 20, min = 1,max = 100)
cs01(X)
cs01(X,0,150)
}
\author{
JuG
}
