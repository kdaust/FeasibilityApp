## Source at start of FeasibilityApp
## Kiri Daust

bcgov_tileserver <- "http://142.93.157.218/data/tiles/{z}/{x}/{y}.pbf"
bcgov_tilelayer <- "BECMap"

plugins <- {list(vgplugin = 
         htmltools::htmlDependency(
           name = "leaflet.vectorgrid",
           version = "1.3.0",
           src = "htmlwidgets",
           script = "lfx-vgrid-prod.js"
         ),
       sliderplugin = htmltools::htmlDependency(
         name = "leaflet.slider",
         version = "1.0.0",
         stylesheet = "lfx-slider.css",
         src = "htmlwidgets",
         script = "lfx-slider.js"
       )
  )
}
registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}
jscode <- paste0('window.LeafletWidget.methods.addGridTiles = function(BGC,Colour) {
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
    jscode
  ))
)

addVectorGridTilesDev <- function(map) {
  map <- registerPlugin(map, plugins$vgplugin)
  map <- registerPlugin(map, plugins$sliderplugin)
  map
}
