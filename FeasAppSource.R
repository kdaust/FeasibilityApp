## Source at start of FeasibilityApp
## Kiri Daust

bcgov_tileserver <- "http://142.93.157.218/data/tiles/{z}/{x}/{y}.pbf"
bcgov_tilelayer <- "BECMap"
load("subzones_colours_ref.rda")
colnames(subzones_colours_ref) <- c("BGC","Col")

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
jscode_feas <- paste0('window.LeafletWidget.methods.addGridTiles = function(BGC,Colour,Lab) {
      var subzoneColors = {};
      var tooltipLabs = {};
      BGC.forEach((bec,i) => {
        const col = Colour[i];
        const label = Lab[i];
        subzoneColors[bec] = col;
        tooltipLabs[bec] = label;
      });
      
      var map = this;
      
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
                fillOpacity: 1
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
        vectorTileOptions("bec_feas", "', bcgov_tilelayer, '", true,
                          "tilePane", subzoneColors, "MAP_LABEL", "OBJECTID")
      )
      
      this.layerManager.addLayer(subzLayer, "tile", "bec_feas", "Feasibility")
      subzLayer.on("click", function(e){
        Shiny.setInputValue("bgc_click",e.layer.properties.MAP_LABEL);
      });
      
      var highlight
		  var clearHighlight = function() {
		  	if (highlight) {
		  		subzLayer.resetFeatureStyle(highlight);
		  	}
		  	highlight = null;
		  }
		  
		  subzLayer.on("click", function(e) {
        if (e.layer.properties) {
          var properties = e.layer.properties
  			  highlight = properties.OBJECTID
  			  var style = {
            weight: 1,
            color: "#fc036f",
            fillColor: subzoneColors[properties.MAP_LABEL],
            fillOpacity: 1,
            fill: true
          }
          subzLayer.setFeatureStyle(properties.OBJECTID, style);
        }
      })
      subzLayer.on("mouseout", function(e) {
        clearHighlight();
      })
		  
      subzLayer.bindTooltip(function(e) {
        return tooltipLabs[e.properties.MAP_LABEL]
      }, {sticky: true, textsize: "10px", opacity: 1});
      subzLayer.bringToFront();
    }'
)

leafletjs_feas <-  tags$head(
  tags$script(HTML(
    jscode_feas
  ))
)

# addVectorGridTilesDev <- function(map) {
#   map <- registerPlugin(map, plugins$vgplugin)
#   map <- registerPlugin(map, plugins$sliderplugin)
#   map
# }

addBGCTiles <- function(map) {
  map <- registerPlugin(map, plugins$vgplugin)
  map <- registerPlugin(map, plugins$sliderplugin)
  map <- htmlwidgets::onRender(map, paste0('
    function(el, x, data) {
      ', paste0("var subzoneColors = {", paste0("'", subzones_colours_ref$BGC, "':'", subzones_colours_ref$Col,"'", collapse = ","), "}"), '
      
      L.bec_layer_opacity = 0.2
      
      var vectorTileOptions=function(layerName, layerId, activ,
                             lfPane, colorMap, prop, id) {
        return {
          vectorTileLayerName: layerName,
          interactive: activ, // makes it able to trigger js events like click
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
        vectorTileOptions("bec_map", "', bcgov_tilelayer, '", false,
                          "tilePane", subzoneColors, "MAP_LABEL", "OBJECTID")
      )
      this.layerManager.addLayer(subzLayer, "tile", "bec_map", "BGCs");
      
      updateOpacity = function(value) {
        L.bec_layer_opacity = parseFloat(value);
      }
      
      var opacityslider = L.control.slider(updateOpacity, {
        id:"opacity_slider",
        orientation:"horizontal",
        position:"bottomleft",
        logo:\'<img src="www/opacity.svg" />\',
        max:1,
        step:0.01,
        syncSlider:true,
        size:"250px",
        // Starting opacity value for bec map layers
        value:0.2,
        showValue:true
      })
      
      opacityslider.addTo(this)
    }'
  ))
  map
}
