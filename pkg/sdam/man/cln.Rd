\name{cln}
\alias{cln}
\title{
Cleansing and re-encoding a text in vector, list, or dataframe.
}
\description{
A wrapper function to re-encode Greek and Latin extended characters and to remove symbols and spaces from text.
}
\usage{
cln(x, level = 1, chr.rm, na.rm, case, repl, space)
}
\arguments{
  \item{x}{
    a vector, list or dataframe with text for cleansing
}
  \item{level}{
    clean level (optional):
      
      \code{0} no-cleansing 
      
      \code{1} default, basic Greek and Latin extended
      
      \code{2} mixed text in Greek and Latin extended
      
      \code{3} force an extra iteration (see details)
}
  \item{chr.rm}{
    characters to remove (optional)
}
  \item{na.rm}{
   remove entries with NA data? (optional and logical)
}
  \item{case}{
   case for text (optional): 
   
   \code{1} lowercase text with 1st letter uppercase
   
   \code{2} lowercase text
   
   \code{3} uppercase text
}
  \item{repl}{
   data frame with text to replace (optional)
}
  \item{space}{
    remove extra spaces (optional):
      
      \code{1} double spaces to a single space; remove space at end of text
      
      \code{2} remove also spaces around \code{ / }, \code{ - },  and \code{ = }
}
}
\details{
This function is meant to re-encode Greek (and other) characters in the \code{EDH} set given either as list format, 
vector, or a dataframe produced with function \code{\link{edhw}} for example. 

Level \code{1} is the default for the re-encoding, and to remove extra spaces. 
Use level \code{2} for text in Latin and Greek mixed, and 
level \code{3} is to force an extra iteration in the cleansing with the same level, which is sometimes needed for ancient Greek. 
Different levels cleansing can be combined by pipeing this function. %running 
For \code{level} equals \code{0} only the input is returned. 

Ad hoc removals from the text are specified in \code{chr.rm}. % at the end of a record when clean what is invoked. 

With \code{repl}, is possible to replace a list of text in two columns, for `text to replace' and for `text that replaces'. 

Argument \code{space} for removing extra spaces in the input text.
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

Re-starting the \code{R} console may be needed for combination of Latin extended 2.
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
