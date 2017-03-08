require(leaflet)

leaflet() %>% addTiles() %>%
  addPolylines(data=roads,popup=~route)
