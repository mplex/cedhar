\name{clv}
\alias{clv}
\title{
Cleansing vector text
}
\description{
Internal function for cleansing and re-encoding text in vector format.
}
\usage{
clv(x, level = 1, case, chr.rm, na.rm = FALSE, space)
}
\arguments{
  \item{x}{
    a vector with text for cleansing
}
  \item{level}{
    optional clean level, either \code{0} for no-cleansing, default \code{1} to \code{3} (see in \code{\link{cln}})
}
  \item{case}{
   case for text \code{1} for 1st uppercase, code{2} for lowercase, code{3} for uppercase
}
  \item{chr.rm}{
    characters to remove
}
  \item{na.rm}{
   remove entries with NA data?
}
  \item{space}{
    optional remove extra spaces,\code{1} double to single spaces/space at end, \code{2} remove also spaces around \code{ / }, \code{ - },  and \code{ = }
}
}
\details{
This is an internal function used in \code{\link{cln}} for cleansing text in vector format.
}
\value{
A vector with text cleansed.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Antonio Rivero Ostoic
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{cln}}, \code{\link{cs}}}
\examples{
# clean Greek characters
clv("Caesar?*+")
}
\keyword{ utilities }
\keyword{ manip }
