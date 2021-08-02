\name{simil}
\alias{simil}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Similarity between (column) vectors
}
\description{
A function to compute the Similarity between vectors, which can arise from columns in a data frame or list entries.
}
\usage{
simil(x, type = c("sm", "ja", "ra"), vars, uniq, diag.incl)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
	A list or a data frame 
}
  \item{type}{
   Type format of data frame. Currently either \code{"long"} or \code{"wide"} (\code{"narrow"} not yet implemented)
}
  \item{vars}{
	Column(s) in \code{x} representing attributes (vector)
}
  \item{uniq}{
	Only unique elements? (optional and logical)
}
  \item{diag.incl}{
	include entries in matrix diagonal? (optional and logical) 
}
}
\details{
At this point, the ID column in the input represents the labels of the nodes. 
In case that an ID column does not exists, then the first column is taken provided that there are not duplicated entry names. 
}
\value{
A valued matrix with similarities among units by simple matching.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Antonio Rivero Ostoic
}
\note{
Other similarity measures will be added in the near future.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{edhw}}, \code{\link{prex}}, \code{\link{get.edh}}
}
\examples{
# TBD 

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ metrics }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
