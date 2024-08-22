\name{cs}
\alias{cs}
\title{
Change Case in a Text
}
\description{
A function to change letters to uppercase and lowercase in a given text.
}
\usage{
cs(xz, level = 1, case = 0, na.rm = FALSE)
}
\arguments{
  \item{xz}{
  input text
}
  \item{level}{
    optional clean level, either \code{0} for no-clean, default \code{1} to most strict \code{9} (see \code{\link{cln}})
}
  \item{case}{
  change case: 
  
\itemize{ 
       \item \code{1} first letter uppercase and rest lowercase 
       \item \code{2} all letters lowercase
       \item \code{3} all letters uppercase 
       }
}
  \item{na.rm}{
   remove entries with NA data? (optional and logical)
}
}
\details{
This is an internal function to change letters to uppercase and lowercase in a text for
argument \code{'case'} in function \code{\link{cln}}, and it inherits values from this function for \code{'level'}, 
and \code{'na.rm'}.
}
\value{
Input text with defined case.
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

\seealso{
\code{\link{cln}}
}
\examples{
cs("Caesar?*+", level=2, case=3)
}
\keyword{ manip }
\keyword{ utilities }