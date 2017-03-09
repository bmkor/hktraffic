\name{plotRoads}
\alias{plotRoads}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
To view some road segments in Hong Kong via leaflet.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotRoads(roads = getRoads())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{roads}{
%%     ~~Describe \code{roads} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (roads = getRoads())
{
    if (require("leaflet", character.only = T, quietly = T)) {
        roads$dashPattern <- as.character(c(1, c("15, 10, 5"))[as.numeric(roads$route_type)])
        pale <- c("yellow", "orange", "blue", "brown")
        dPal <- leaflet::colorFactor(pale, domain = roads$district)
        leaflet(data = roads) \%>\% addTiles(group = "OSM (default)") \%>\%
            addProviderTiles(providers$Stamen.Toner, group = "Toner") \%>\%
            addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") \%>\%
            addPolylines(color = ~dPal(district), dashArray = ~dashPattern,
                popup = ~paste0("route type: ", route_type),
                label = ~route) \%>\% addLegend(position = "bottomright",
            pal = dPal, values = ~district, title = "District",
            labels = ~district) \%>\% addLayersControl(baseGroups = c("OSM (default)",
            "Toner", "Toner Lite"), options = layersControlOptions(collapsed = FALSE))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
