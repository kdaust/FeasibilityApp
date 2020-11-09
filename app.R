library(shiny)
library(mapdeck)
library(sf)
library(data.table)
library(shinyWidgets)

wna_zone <- st_read(dsn = "BC_VSmall.gpkg")
wna_zone <- st_transform(wna_zone, 4326) %>% st_cast("POLYGON")
wna_zone <- as.data.table(wna_zone)
tempCols <- data.table(BGC = unique(wna_zone$BGC))
tempCols[,Col := rainbow(nrow(tempCols), alpha = 0.2)]
wna <- st_read(dsn = "Tiled_WNA.gpkg")
grd <- st_read(dsn = "WNA_TilesOutlines.gpkg")

wna <- st_transform(wna, 4326) %>% st_cast("POLYGON")
wna <- as.data.table(wna)
feas <- fread("Feasibility_v11_21.csv")
feas <- feas[,.(BGC,SS_NoSpace,Spp,Feasible)]
spp.choose <- sort(unique(feas$Spp))
suitcols <- data.table(Suit = c(1,2,3),Col = c("#443e3d","#736e6e","#a29f9e"))#c("#42CF20FF","#ECCD22FF","#EC0E0EFF")
renderedTiles <- vector("numeric")
set_token("pk.eyJ1Ijoia2lyaWRhdXN0IiwiYSI6ImNraDJjOTNxNzBucm0ycWxxbTlrOHY5OTEifQ.GybbrNS0kJ3VZ_lGCpXwMA")

# Define UI for application that draws a histogram
ui <- navbarPage("Species Feasibility",
                 tabPanel("LeafGL",
                          column(2,
                                 pickerInput("sppPick",
                                             label = "Select Tree Species",
                                             choices = spp.choose,
                                             selected = "Pl"),
                                 awesomeRadio("type",
                                              label = "Select Summary",
                                              choices = c("P/A","Max Suit"),
                                              selected =  "P/A")
                                 ),
                          column(10,
                                 h2("Map rendered with mapdeck"),
                                     mapdeckOutput("map",height = "700px")
                                 )
                          )
                 )


server <- function(input, output) {
    
    prepDat <- reactive({
        feasMax <- feas[Spp == input$sppPick & Feasible %in% c(1,2,3),
                        .(SuitMax = min(Feasible)), by = BGC]
        temp <- feasMax[wna, on = "BGC"]
        temp[tempCols, BGC_Col := i.Col, on = "BGC"]
        if(input$type == "P/A"){

            temp[,Col := fifelse(is.na(SuitMax),BGC_Col,"#443e3d")]##19CD21FF"

        }else{
            temp[suitcols, Col := i.Col, on = c(SuitMax = "Suit")]
            temp[is.na(Col), Col := BGC_Col]
        }
        st_as_sf(temp)
    })
    
    prepZone <- reactive({
        feasMax <- feas[Spp == input$sppPick & Feasible %in% c(1,2,3),
                        .(SuitMax = min(Feasible)), by = BGC]
        temp <- feasMax[wna_zone, on = "BGC"]
        temp[tempCols, BGC_Col := i.Col, on = "BGC"]
        if(input$type == "P/A"){

            temp[,Col := fifelse(is.na(SuitMax),BGC_Col,"#443e3d")]##19CD21FF"

        }else{
            temp[suitcols, Col := i.Col, on = c(SuitMax = "Suit")]
            temp[is.na(Col), Col := BGC_Col]
        }
        st_as_sf(temp)
    })
    
    getDat <- reactive({
        bounds <- input$map_view_change
        if(bounds$viewBounds$north - bounds$viewBounds$south < 1.2){
            dat <- prepDat()
            bnds <- st_bbox(c(xmin = bounds$viewBounds$west, xmax = bounds$viewBounds$east, 
                              ymax = bounds$viewBounds$north,ymin = bounds$viewBounds$south), crs = st_crs(4326))
            bnds <- st_as_sfc(bnds)
            t1 <- st_intersects(grd, bnds)
            t1 <- as.data.frame(t1)
            dat <- dat[dat$ID %in% t1$row.id,]
            dat <- dat[!dat$ID %in% renderedTiles,]
            dat <- dat[!is.na(dat$SuitMax),]
            if(nrow(dat) < 1) dat <- NULL
            dat
        }else{
            NULL
        }
    })
    
    output$map <- renderMapdeck({
       # dat <- prepZone()
        mapdeck() %>%
            mapdeck_view(location = c(-124.72,54.56), zoom = 4)
    })
    
    observeEvent({input$sppPick
        input$type},{
            dat <- prepZone()
            mapdeck_update(map_id = "map") %>%
                #clear_polygon(layer_id = "zone") %>%
                add_polygon(dat,
                            layer_id = "zone",
                            fill_colour = "Col",
                            tooltip = "BGC",
                            auto_highlight = F,
                            focus_layer = F,
                            update_view = F
                )

        })
    
    observeEvent({input$sppPick 
        input$type},{
            mapdeck_update(map_id = "map") %>%
                clear_polygon(layer_id = "polys")
        })
    
    observeEvent({input$sppPick
        input$type
        input$map_view_change},{
            dat <- getDat()
            if(!is.null(dat)){
                renderedTiles <- c(renderedTiles, unique(dat$ID))
                mapdeck_update(map_id = "map") %>%
                    add_polygon(dat,
                                layer_id = "polys",
                                fill_colour = "Col",
                                tooltip = "BGC",
                                auto_highlight = F,
                                focus_layer = F,
                                update_view = F
                    )
            }
            
        })
    
    # output$map <- renderLeaflet({
    #     dat <- st_as_sf(wna)
    #     
    #     leaflet(data = dat) %>%
    #         addPolygons(data = dat,
    #                     layerId = ~ BGC,
    #                     label = ~ BGC,
    #                     weight = 1)
    # })
    # 
    # observeEvent({input$sppPick
    #     input$type},
    #     {dat2 <- prepDat()
    #     leafletProxy("map",data = dat2) %>%
    #         setShapeStyle(layerId = ~BGC, fillColor = dat2[["Col"]], color = dat2[["Col"]])
    #     })
    
    # ##leaflet glify
    # output$map2 <- renderLeaflet({
    #     dat <- prepDat()
    #     leaflet(data = dat) %>%
    #         addGlPolygons(data = dat,
    #                     layerId = ~BGC,
    #                     color = dat$Col)
    # })

}

# Run the application 
shinyApp(ui = ui, server = server)
