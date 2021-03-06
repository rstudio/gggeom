% Generated by roxygen2 (4.0.2): do not edit by hand
\name{compute_dp_distance}
\alias{compute_dp_distance}
\title{Compute the Douglas-Peucker distance for paths.}
\usage{
compute_dp_distance(data, x_var, y_var)
}
\arguments{
\item{data}{A data frame like object.}

\item{x_var,y_var}{Formulas specifying either variable names or
expressions to use as x and y positions.}
}
\value{
A data frame with columns:
 \item{x_,y_}{Position}
 \item{distance_}{Distance between point and sub-line}
}
\description{
Douglas-Peucker is a recursive line simplification algorithm. It starts by
defining a line from the first to the last point, and then finds the
point that is furthest from the line. It then recursively breaks up the
into two pieces around the furthest point, and finds the furthest point
from those sublines. See
\url{http://en.wikipedia.org/wiki/Ramer-Douglas-Peucker_algorithm} for
more details.
}
\details{
Note that this function does not do any simplification - it just adds an
additional column the measures the distaince between each point and it
subline. Filtering on this column will perform simplification.
}
\examples{
x <- 1:10
y <- x * 2
df <- data.frame(x, y)

# For a straight line, can remove all points except first and last
compute_dp_distance(df, ~x, ~y)
}
\seealso{
\code{\link{geometry_simplify}()} for an function that works on
  a geometry and does simplification given tolerance or percentage of
  points to keep.
}

