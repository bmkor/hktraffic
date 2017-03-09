plotRoads<-function(roads=getRoads()){
  if(require("leaflet",character.only = T,quietly =T)){
    roads$dashPattern<-as.character(c(1,c("15, 10, 5"))[as.numeric(roads$route_type)])
    pale<-c("yellow","orange","blue","brown")
    dPal<-leaflet::colorFactor(pale,domain=roads$district)
    leaflet(data=roads) %>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
      addPolylines(color=~dPal(district),dashArray=~dashPattern,
                   popup=~paste0("route type: ",route_type), label = ~route) %>%
      addLegend(position="bottomright", pal = dPal,
                values = ~district, title = "District",
                labels=~district) %>%
      addLayersControl(
        baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
        options = layersControlOptions(collapsed = FALSE)
      )
  }
}
