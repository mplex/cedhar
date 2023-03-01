\name{cln}
\alias{cln}
\title{
Cleansing and re-encoding a vector, list or dataframe
}
\description{
A wrapper function to re-encode Greek and Latin extended characters and to remove symbols from text.
}
\usage{
cln(x, level = 1, chr.rm, na.rm, case, repl)
}
\arguments{
  \item{x}{
    a vector, list or dataframe with text for cleansing
}
  \item{level}{
    optional clean level, either \code{0} for no-cleansing, default \code{1} to \code{3} (see details)
}
  \item{chr.rm}{
    characters to remove (optional)
}
  \item{na.rm}{
   remove entries with NA data? (optional and logical)
}
  \item{case}{
   case for text \code{1} for 1st uppercase, code{2} for lowercase, code{3} for uppercase (optional)
}
  \item{repl}{
   data frame with text to replace (optional)
}
}
\details{
This function is meant to re-encode Greek (and other) characters in the \code{EDH} set given either as list format, 
vector, or a dataframe produced with function \code{\link{edhw}} for example. 

When \code{level} is \code{0} only the input is returned. Use level \code{2} for text in Latin adn Greek mixed, and 
level \code{3}  to force an extra iteration in the cleansing.
in the re-encoding, to remove extra spaces, or what is in \code{chr.rm} at the end of a record when clean what is invoked. 

With \code{repl}, is possible to replace a list of text in two columns, for `text to replace' and for `text that replaces'. 
}
\value{
Depending on the input, a vector or dataframe with text cleansed.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Antonio Rivero Ostoic
}
\note{
This function is meant for the \code{EDH} dataset. 
}

\section{Warning }{
Re-encoding tests where made on a Windows platform with \emph{locale} \code{Danish_Denmark.utf8}; see \code{?Sys.setlocale()} 
to eventually change this setting.
}

\seealso{
\code{\link{edhw}}, \code{\link{get.edh}}, \code{\link{edhwpd}}, \code{\link{cs}}, \code{\link{clv}}
}
\examples{
# clean Greek characters
cln("Caesar?*+", chr.rm=c("*","+"))
}
\keyword{ utilities }
\keyword{ manip }
