% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/slice_artifact.r
\name{as.slice_artifact}
\alias{as.slice_artifact}
\alias{as.slice_artifact.data.frame}
\title{Coerce To A \code{slice_artifact}}
\usage{
as.slice_artifact(x, ...)

\method{as.slice_artifact}{data.frame}(x, index_column, ...)
}
\arguments{
\item{x}{object to be coerced into \code{slice_artifact}}

\item{...}{specific arguments to each method}

\item{index_column}{column to be used as slice index}
}
\value{
an \code{slice_artifact} generated from \code{x}
}
\description{
Coerces \code{x} an object to \code{slice_artifact} if possible
}
\details{
Currently only data.frames and data.tables can be coerced to \code{slice_artifact}
}
\section{Methods (by class)}{
\itemize{
\item \code{as.slice_artifact(data.frame)}: 

}}
