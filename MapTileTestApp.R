library(sf)
library(scales)
library(shiny)
library(leaflet)
library(leafgl)
library(data.table)
library(shinyWidgets)
library(shinyjs)

feas <- fread("~/CommonTables/Feasibility_v11_21.csv")
feas <- feas[,.(BGC,SS_NoSpace,Spp,Feasible)]
spp.choose <- sort(unique(feas$Spp))

suitcols <- data.table(Suit = c(1,2,3),Col = c("#42CF20","#ECCD22","#EC0E0E"))

bcgov_tileserver <- "http://142.93.157.218/data/tiles/{z}/{x}/{y}.pbf"
bcgov_tilelayer <- "BECMap"
load("subzones_colours_ref.rda")
colnames(subzones_colours_ref) <- c("BGC","Col")
plugins <- {
  list(vgplugin = 
         htmltools::htmlDependency(
           name = "leaflet.vectorgrid",
           version = "1.3.0",
           src = system.file("htmlwidgets", package = "bccciss"),
           script = "lfx-vgrid-prod.js"
         ),
       sliderplugin = htmltools::htmlDependency(
         name = "leaflet.slider",
         version = "1.0.0",
         stylesheet = "lfx-slider.css",
         src = system.file("htmlwidgets", package = "bccciss"),
         script = "lfx-slider.js"
       )
  )
}
registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}
js <- paste0('window.LeafletWidget.methods.addGridTiles = function(BGC,Colour) {
      var subzoneColors = {};
      BGC.forEach((bec,i) => {
        const col = Colour[i];
        subzoneColors[bec] = col;
      });
      var map = this;
      L.bec_layer_opacity = 1;
      
      var vectorTileOptions=function(layerName, layerId, activ,
                                     lfPane, colorMap, prop, id) {
        return {
          vectorTileLayerName: layerName,
          interactive: true, // makes it able to trigger js events like click
          vectorTileLayerStyles: {
            [layerId]: function(properties, zoom) {
              return {
                weight: 0,
                fillColor: colorMap[properties[prop]],
                fill: true,
                fillOpacity: L.bec_layer_opacity
              }
            }
          },
          pane : lfPane,
          getFeatureId: function(f) {
            return f.properties[id];
          }
        }
        
      };
      
      var subzLayer = L.vectorGrid.protobuf(
        "', bcgov_tileserver, '",
        vectorTileOptions("bec_subz", "', bcgov_tilelayer, '", true,
                          "tilePane", subzoneColors, "MAP_LABEL", "OBJECTID")
      )
      
      this.layerManager.addLayer(subzLayer, "tile", "bec_subz", "Subzones Variants")
      subzLayer.on("click", function(e){
        Shiny.setInputValue("bgc_click",e.layer.properties.MAP_LABEL);
      });
      subzLayer.bindTooltip(function(e) {
        return e.properties.MAP_LABEL
      }, {sticky: true, textsize: "10px", opacity: 1})
    }'
  )

leafletjs <-  tags$head(
  tags$script(HTML(
    js
  ))
)

addVectorGridTilesDev <- function(map) {
  map <- registerPlugin(map, plugins$vgplugin)
  map <- registerPlugin(map, plugins$sliderplugin)
  map
}

# Define UI for application that draws a histogram
ui <- navbarPage("Species Feasibility",
                 tabPanel("Leaflet Update inplace",
                          useShinyjs(),
                          column(2,
                                 pickerInput("sppPick",
                                             label = "Select Tree Species",
                                             choices = spp.choose,
                                             selected = "Pl",
                                             multiple = F),
                                 awesomeRadio("type",
                                              label = "Select Summary",
                                              choices = c("P/A","Max Suit"),
                                              selected =  "P/A")
                          ),
                          column(10,
                                 leafletjs,
                                 shinycssloaders::withSpinner(
                                   leafletOutput("map", height = "700px"),
                                   type = 5
                                 ),
                                 textOutput("textout")
                                 
                          )
                 )
)


server <- function(input, output) {
  showLog()
  prepDat <- reactive({
    feasMax <- feas[Spp == input$sppPick & Feasible %in% c(1,2,3),
                    .(SuitMax = min(Feasible)), by = BGC]
    feasMax[,Col := fifelse(is.na(SuitMax),"#BABABA","#19CD21")]
  })
  
  
  output$map <- renderLeaflet({
    leaflet() %>%
      setView(lng = -122.77222, lat = 51.2665, zoom = 7) %>%
      addVectorGridTilesDev()
  })
  
  observe({
    dat <- prepDat()
    #browser()
    leafletProxy("map",data = dat) %>%
      invokeMethod(data = dat, method = "addGridTiles", dat$BGC, dat$Col)
  })
  
  observeEvent(input$bgc_click,{
    output$textout <- renderText({
      input$bgc_click
    })
    print("click!")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
