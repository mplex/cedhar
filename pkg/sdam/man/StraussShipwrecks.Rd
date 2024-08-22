\name{StraussShipwrecks}
\alias{StraussShipwrecks}
\title{
Ancient Roman Shipwrecks Dataset
}
\description{
Sets of data of ancient wrecks in the Mediterranean Basin up to 1500 AD.
}
%\usage{
%system.file("extdata", "StraussShipwrecks.csv", package = "sdam")
%}
\format{
File \code{StraussShipwrecks.csv} is a comma separated data frame with variables: 

\code{"Wreck ID"}, \code{"Parker Number"}, \code{"Region"}, \code{"Min depth"}, \code{"Period"}, 
\code{"Latest date"}, \code{"Probability"}, \code{"Reference"}, \code{"Marble"}, \code{"Blocks"}, 
\code{"Hull remains"}, \code{"Estimated tonnage"}, \code{"Strauss ID"}, \code{"Sea area"}, 
\code{"Latitude"}, \code{"Max depth"}, \code{"Dating"}, \code{"Date range"}, \code{"Place of origin"}, 
\code{"Comments"}, \code{"Columns etc"}, \code{"Marble type"}, \code{"Shipboard paraphernalia"}, 
\code{"Amphora type"}, \code{"Name"}, \code{"Country"}, \code{"Longitude"}, \code{"Depth"}, 
\code{"Earliest date"}, \code{"Mid point of date range"}, \code{"Place of destination"}, 
\code{"Amphorae"}, \code{"Sarcophagi"}, \code{"Other cargo"}, \code{"Ship equipment"}.

File \code{ShipwreckSeas.txt} is a tab separated data frame with \code{PLACE} and \code{CODE} variables 
for aggregation sea regions in the Mediterranean Basin.
}
\source{
Strauss, J. (2013). Shipwrecks Database. Version 1.0. Accessed (07-12-2021) from \cr 
oxrep.classics.ox.ac.uk/databases/shipwrecks_database/

Online repository for Diplomacy information at \cr http://www.dipwiki.com/images/5/5c/AncientMed9_Names.gif
}
\references{
Strauss, J. \emph{Roman Cargoes: Underwater Evidence from the East} (London 2007)
}
\seealso{
\code{\link{retn}}, \code{\link{rpmp}}, \code{\link{rp}}.
}
\examples{
# usage
system.file("extdata", "StraussShipwrecks.csv", package = "sdam")
system.file("extdata", "ShipwreckSeas.txt", package = "sdam")
}
\keyword{datasets}
