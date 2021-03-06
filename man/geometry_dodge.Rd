% Generated by roxygen2 (4.0.2): do not edit by hand
\name{geometry_dodge}
\alias{geometry_dodge}
\title{Dodge objects on next to one another.}
\usage{
geometry_dodge(geom)
}
\arguments{
\item{geom}{A geometry data frame.}
}
\description{
Currently only implemented for rects.
}
\examples{
bar_ex \%>\% plot()
bar_ex \%>\% geometry_stack() \%>\% plot()
bar_ex \%>\% geometry_dodge() \%>\% plot()
}

