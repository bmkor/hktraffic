tmp<-tempfile()
roadMetaData<-"http://theme.gov.hk/en/theme/psi/datasets/tsm_dataspec.pdf"
download.file(roadMetaData,dest=tmp)

roadMetaData<-pdftools::pdf_text(tmp)
roadMetaData<-roadMetaData[-1]
head(roadMetaData)

res<-sapply(roadMetaData, function(x){
  stmp<-unlist(strsplit(x,"\\n"))[-(1:2)]
  gsub("^\\s","",gsub(" ROAD", "_ROAD",gsub(" ROUTE", "_ROUTE",gsub("\\s+"," ",stmp))))
})
names(res)<-1:length(res)
res<-c(paste("route","start","start_E","start_N","end","end_E","end_N","district","route_type",sep=" "),as.vector(unlist(res)))
write(res,tmp)
roadMetaData<-read.csv(tmp,sep=" ",stringsAsFactors = TRUE)
head(roadMetaData)
rm(res)
rm(tmp)

######## validity checking
which(roadMetaData$route != paste0(roadMetaData$start,"-",roadMetaData$end)) #93 not same
roadMetaData[93,] #7891, 7881?
which(roadMetaData$start == 7891) ## empty
which(roadMetaData$end == 7891) ## 94, 95
roadMetaData[c(94,95),] #7891 836701.9 815918.1
roadMetaData[c(93),] #7881 should be 7891
roadMetaData$start[93] <- 7891
roadMetaData[93,] #7891
rownames(roadMetaData) <- roadMetaData$route
head(roadMetaData)

######## create road segments
require(sp)
roads<-apply(roadMetaData,1,function(r){
  m<-matrix(as.numeric(c(r[3],r[4],
                         r[6],r[7])), nrow = 2, byrow =T)
  m<-data.frame(m)
  sp::coordinates(m) <- ~X1+X2
  sp::SpatialLines(list(sp::Lines(list(sp::Line(m)), r[1])),sp::CRS("+init=epsg:2326"))
})
roads<-do.call(rbind,roads)
roads<-sp::SpatialLinesDataFrame(roads,roadMetaData[,c("route","district","route_type")])
roads<-sp::spTransform(roads,sp::CRS("+init=epsg:4326"))

rm(roadMetaData)


