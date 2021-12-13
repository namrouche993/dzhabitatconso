#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import leaflet
#' @import leaflet.providers
#' @import leaflet.extras
#' @import tidyverse
#' @importFrom utils head
#' @importFrom graphics barplot
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic
  output$mtable1<-renderTable({
    head(dzhabitatconso::donnees_consomation,2)
  })

  output$m333<-renderTable({
    head(dzhabitatconso::donnees_encours,2)
  })

  output$m4441<-renderTable({
    head(dd,2)
  })

  output$m666<-renderTable({
    head(dco21)
  })

  output$plot1<-renderPlot({
    barplot(1:10, col = blues_pal(seq(0,1,length.out=10)))
  })


  output$leaflet1<-renderLeaflet({
    leaflet(dzhabitatconso::algeria)%>%
      setView(lng = 1.63333 , lat = 28.3667, zoom = 5)%>%
      addProviderTiles("OpenStreetMap.BZH") %>%
      setMapWidgetStyle(list(background= "#ffffff")) %>%
      addPolygons()
  })

  output$algeriawilayas<-renderText({
    dzhabitatconso::algeria@data$wilayas
  })

  output$leaflet_mapdz<-renderLeaflet({
    dzhabitatconso::mapdz %>%
      addPolygons()
  })


  output$leaflet_mapdz2<-renderLeaflet({
    mapdz2 %>%
      addPolygons()
  })



  output$leaflet_mapdz3<-renderLeaflet({
    dzhabitatconso::mapdz3 %>%
      addPolygons()
  })



}
