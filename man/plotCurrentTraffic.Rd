\name{plotCurrentTraffic}
\alias{plotCurrentTraffic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
To view current traffic speed of some road segments in Hong Kong.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotCurrentTraffic(trafficURL = "http://resource.data.one.gov.hk/td/speedmap.xml")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{trafficURL}{
%%     ~~Describe \code{trafficURL} here~~
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
function (trafficURL = "http://resource.data.one.gov.hk/td/speedmap.xml")
{
    if (require("leaflet", character.only = T, quietly = T) &
        require("XML", character.only = T, quietly = T)) {
        tdata <- XML::xmlParse(trafficURL)
        tdata <- XML::xmlToDataFrame(tdata, stringsAsFactors = F)
        tdata$ROAD_SATURATION_LEVEL <- gsub("TRAFFIC ", "", tdata$ROAD_SATURATION_LEVEL)
        tdata$ROAD_SATURATION_LEVEL <- factor(tdata$ROAD_SATURATION_LEVEL)
        tdata$CAPTURE_DATE <- strptime(tdata$CAPTURE_DATE, format = "\%Y-\%m-\%dT\%H:\%M:\%S")
        pale <- c("green", "yellow", "red")
        names(pale) <- c("GOOD", "AVERAGE", "BAD")
        pale <- as.vector(pale[levels(tdata$ROAD_SATURATION_LEVEL)])
        pal <- leaflet::colorFactor(pale, domain = tdata$ROAD_SATURATION_LEVEL)
        roads = getRoads()
        roads$dashPattern <- as.character(c(1, c("15, 10, 5"))[as.numeric(roads$route_type)])
        roads <- subset(roads, route \%in\% tdata$LINK_ID)
        roads@data <- cbind(roads@data, tdata[match(roads$route,
            data$LINK_ID), ])
        lastCaptureTime <- max(unique(tdata$CAPTURE_DATE))
        lastCaptureTime <- strftime(lastCaptureTime, format = "\%H:\%M, \%d \%b")
        leaflet(data = roads) \%>\% addTiles(group = "OSM (default)") \%>\%
            addProviderTiles(providers$Stamen.Toner, group = "Toner") \%>\%
            addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") \%>\%
            addPolylines(color = ~pal(ROAD_SATURATION_LEVEL),
                dashArray = ~dashPattern, popup = ~paste0("average speed: ",
                  TRAFFIC_SPEED, " km/h"), label = ~route) \%>\%
            addLegend(position = "bottomright", pal = pal, values = ~ROAD_SATURATION_LEVEL,
                title = paste0("Road Saturation Level at ", lastCaptureTime),
                labels = ~ROAD_SATURATION_LEVEL) \%>\% addLayersControl(baseGroups = c("OSM (default)",
            "Toner", "Toner Lite"), options = layersControlOptions(collapsed = FALSE))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
