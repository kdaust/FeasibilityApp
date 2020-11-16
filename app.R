library(shiny)
library(mapdeck)
library(sf)
library(data.table)
library(shinyWidgets)
library(shinycssloaders)
library(ggplot2)
library(ggiraph)
library(scales)
library(rhandsontable)
library(shinyalert)
library(RPostgreSQL)

##connect to database
###Read in climate summary data
drv <- dbDriver("PostgreSQL")
sapply(dbListConnections(drv), dbDisconnect)
con <- dbConnect(drv, user = "postgres", password = "Kiriliny41", host = "smithersresearch.ca", 
                 port = 5432, dbname = "feasibility_update")

##data for edatopic grid
grd1x <- seq(1.5,4.5,1)
grd1y <- seq(1.5,7.5,1)
rects <- data.table(xmin = rep(c(0.5,3.5), each = 5),
                    xmax = rep(c(3.5,5.5), each = 5),
                    ymin = rep(c(0.5,1.5,3.5,5.5,7.5),2),
                    ymax = rep(c(1.5,3.5,5.5,7.5,8.5),2))
ids <- 1:10
labs <- c("SHD-P","SAG/HG-P","Sm/m-P","Sx/x-P","Vx-P","SHD-R","SAG/HG-R","Sm/m-R","Sx/x-R","Vx-R")
idDat <- expand.grid(SMR = 0:7, SNR = c("A","B","C","D","E"))
idDat <- as.data.table(idDat)
setorder(idDat,SMR,SNR)
idDat[,ID := c(5,5,5,10,10,4,4,4,9,9,4,4,4,9,9,3,3,3,8,8,3,3,3,8,8,2,2,2,7,7,2,2,2,7,7,1,1,1,6,6)]
idDat[,Edatopic := paste0(SNR,SMR)]
cols <- fread("WNAv12_HexColours.csv")
setnames(cols, c("BGC","Col"))
alpha <- "4D"
cols[,Col := paste0(Col,alpha)]
grRamp <- colorRamp(c("#443e3dFF","#a29f9eFF"),alpha = T) ##colour ramp for gray values

##setup species picker
treelist <- fread("Tree_List_2020.csv")
treelist <- treelist[Bad != "x",.(TreeCode,Group)]
sppList <- list()
for(nm in unique(treelist$Group)){
    temp <- treelist[Group == nm, TreeCode]
    sppList[[nm]] <- temp
}

wna_zone <- st_read(dsn = "BC_VSmall.gpkg")
wna_zone <- st_transform(wna_zone, 4326) %>% st_cast("POLYGON")
wna_zone <- as.data.table(wna_zone)
wna_zone[cols, BGC_Col := i.Col, on = "BGC"]
wna_small <- st_as_sf(wna_zone)
wna <- st_read(dsn = "Tiled_WNA.gpkg")
grd <- st_read(dsn = "WNA_TilesOutlines.gpkg")

wna <- st_transform(wna, 4326) %>% st_cast("POLYGON")
wna <- as.data.table(wna)
wna[cols, BGC_Col := i.Col, on = "BGC"]
feas <- fread("Feasibility_v11_22.csv")
feas <- feas[,.(BGC,SS_NoSpace,Spp,Feasible)]
setnames(feas, old = "Spp",new = "SppSplit")
feas[,Spp := SppSplit]
feas[SppSplit %in% c("Fdi","Fdc"),Spp := "Fd"]
feas[SppSplit %in% c("Pli","Plc"),Spp := "Pl"]
feas[SppSplit %in% c("Se","Sw","Sxw","Sxl","Sxs","Ss"),Spp := "Sx"]
feas[SppSplit %in% c("Pyi","Pyc"),Spp := "Py"]
feasOrig <- feas

eda <- fread("Edatopic_v11_20.csv")
eda <- eda[is.na(Special),.(BGC,SS_NoSpace,Edatopic)]
#spp.choose <- sort(unique(feas$Spp))
suitcols <- data.table(Suit = c(1,2,3),Col = c("#443e3dFF","#736e6eFF","#a29f9eFF"))#c("#42CF20FF","#ECCD22FF","#EC0E0EFF")
renderedTiles <- vector("numeric")
set_token("pk.eyJ1Ijoia2lyaWRhdXN0IiwiYSI6ImNraDJjOTNxNzBucm0ycWxxbTlrOHY5OTEifQ.GybbrNS0kJ3VZ_lGCpXwMA")

# Define UI for application that draws a histogram
ui <- navbarPage("Species Feasibility",
                 tabPanel("BC Map",
                          useShinyalert(),
                          fluidPage(
                                  column(3,
                                         pickerInput("sppPick",
                                                     label = "Select Tree Species",
                                                     choices = sppList,
                                                     selected = "Pl"),
                                         awesomeRadio("type",
                                                      label = "Select Summary",
                                                      choices = c("P/A","Max Suit"),
                                                      selected =  "P/A"),
                                         actionBttn("updatedfeas","Show Updated Feasibility Values"),
                                         h4("Select Edatopic Space: \n"),
                                         girafeOutput("edaplot", width = "400px"),
                                         verbatimTextOutput("info")
                                  ),
                                  column(9,
                                         withSpinner(
                                             mapdeckOutput("map", height = 700),
                                             type = 6
                                         ),
                                         br(),
                                         h2("Suitability data for selected polygon"),
                                         h4("You can edit the feasibility values. When you click submit, 
                                            the updated values will be sent to a database"),
                                         rHandsontableOutput("hotTab"),
                                         actionBttn("submitdat", label = "Submit!")
                                         
                                  )
                              
                              )
                          )
                          
                 )


server <- function(input, output) {
    
    ##base BGC map
    output$map <- renderMapdeck({
        dat <- wna_small
        mapdeck() %>%
            mapdeck_view(location = c(-124.72,54.56), zoom = 4) %>%
            add_polygon(dat,
                        layer_id = "bgcmap",
                        fill_colour = "BGC_Col",
                        tooltip = "BGC",
                        auto_highlight = F,
                        focus_layer = F,
                        update_view = F)
    })
    
    observeEvent({c(
        input$sppPick,
        input$type,
        input$edaplot_selected,
        input$updatedfeas)
    },{
        dat <- mergeSmallDat()
        if(!is.null(dat)){
            mapdeck_update(map_id = "map") %>%
                #clear_polygon(layer_id = "zone") %>%
                add_polygon(dat,
                            layer_id = "zone",
                            fill_colour = "Col",
                            tooltip = "Lab",
                            auto_highlight = F,
                            focus_layer = F,
                            update_view = F
                )
        }
    }, priority = 5)
    
    observeEvent({
        c(input$sppPick, 
        input$type,
        input$edaplot_selected,
        input$updatedfeas)},{
            mapdeck_update(map_id = "map") %>%
                clear_polygon(layer_id = "polys")
        }, priority = 10)
    
    observeEvent({
        c(input$sppPick,
          input$type,
          input$map_view_change,
          input$edaplot_selected,
          input$updatedfeas)
    },{
        dat <- getTiles()
        if(!is.null(dat)){
            renderedTiles <- c(renderedTiles, unique(dat$ID))
            mapdeck_update(map_id = "map") %>%
                add_polygon(dat,
                            layer_id = "polys",
                            fill_colour = "Col",
                            tooltip = "Lab",
                            auto_highlight = F,
                            focus_layer = F,
                            update_view = F
                )
        }
        
    }, priority = 2)
    
    observeEvent(input$updatedfeas,{
        if(input$updatedfeas %% 2 == 1){
            newDat <- dbGetQuery(con, "SELECT * FROM edatope_updates")
            newDat <- as.data.table(newDat)
            newDat <- newDat[,.(unit,sppsplit,new)]
            setnames(newDat, c("SS_NoSpace","SppSplit","NewFeas"))
            temp <- newDat[feasOrig, on = c("SS_NoSpace","SppSplit")]
            temp[!is.na(NewFeas), Feasible := NewFeas]
            temp[,NewFeas := NULL]
            feas <<- temp
        }else{
            feas <<- feasOrig
        }

    }, priority = 20)
    
    prepDatSimple <- reactive({
        feasMax <- feas[Spp == input$sppPick & Feasible %in% c(1,2,3),
                        .(SuitMax = min(Feasible)), by = .(BGC,SppSplit)]
        if(input$type == "P/A"){
            if(length(unique(feasMax$SppSplit)) > 1){
                feasMax[,SppSplit := as.numeric(as.factor(SppSplit))]
                tempCol <- grRamp(rescale(feasMax$SppSplit,to = c(0,0.6)))
                feasMax[,Col := rgb(tempCol[,1],tempCol[,2],tempCol[,3],tempCol[,4], maxColorValue = 255)]
            }else{
                feasMax[,Col := "#443e3dFF"]
            }
            
        }else{
            feasMax[suitcols, Col := i.Col, on = c(SuitMax = "Suit")]
        }
        feasMax[,Lab := BGC]
        feasMax[,.(BGC,Col,Lab)]
    })
    
    prepEdaDat <- reactive({
        id <- as.numeric(input$edaplot_selected)
        idSub <- idDat[ID == id,.(ID,Edatopic)]
        edaSub <- eda[idSub, on = "Edatopic"]
        feasSub <- feas[Spp == input$sppPick & Feasible %in% c(1,2,3),]
        feasSub <- feasSub[SS_NoSpace %chin% edaSub$SS_NoSpace,]
        feasSub[,Lab := paste0(SS_NoSpace,": ", Feasible)]
        feasSum <- feasSub[,.(FeasVal = mean(Feasible), Lab = paste(Lab, collapse = "<br>")), by = BGC]
        tempCol <- grRamp(rescale(feasSum$FeasVal,to = c(0,1)))
        feasSum[,Col := rgb(tempCol[,1],tempCol[,2],tempCol[,3],tempCol[,4], maxColorValue = 255)]
        feasSum[,.(BGC,Col,Lab)]
    })
    
    mergeBigDat <- reactive({
        if(is.null(input$edaplot_selected)){
            feasDat <- prepDatSimple()
        }else{
            feasDat <- prepEdaDat()
        }
        temp <- wna[feasDat, on = "BGC"]
        temp <- temp[!is.na(BGC_Col),]
        st_as_sf(temp)
    })
    
    mergeSmallDat <- reactive({
        if(is.null(input$edaplot_selected)){
            feasDat <- prepDatSimple()
        }else{
            feasDat <- prepEdaDat()
        }
        temp <- wna_zone[feasDat, on = "BGC"]
        temp <- temp[!is.na(BGC_Col),]
        if(nrow(temp) == 0){
            shinyalert("Oh oh!","There are no suitable locations for this selection!", type = "error")
            return(NULL)
        }else{
            st_as_sf(temp)
        }
        
    })
    
    getTiles <- reactive({
        bounds <- input$map_view_change
        if(!is.null(bounds) && bounds$viewBounds$north - bounds$viewBounds$south < 1.2){
            dat <- mergeBigDat()
            bnds <- st_bbox(c(xmin = bounds$viewBounds$west, xmax = bounds$viewBounds$east, 
                              ymax = bounds$viewBounds$north,ymin = bounds$viewBounds$south), crs = st_crs(4326))
            bnds <- st_as_sfc(bnds)
            t1 <- st_intersects(grd, bnds)
            t1 <- as.data.frame(t1)
            dat <- dat[dat$ID %in% t1$row.id,]
            dat <- dat[!dat$ID %in% renderedTiles,]
            dat <- dat[!is.na(dat$Lab),]
            if(nrow(dat) < 1) dat <- NULL
            dat
        }else{
            NULL
        }
    })
    
    prepTable <- reactive({
        event <- input$map_polygon_click
        temp <- regmatches(event,regexpr("tooltip.{5}[[:upper:]]*[[:lower:]]*[[:digit:]]?",event))
        unit <- gsub("tooltip.{3}","",temp)
        if(is.null(input$edaplot_selected)){
            feasMax <- feas[BGC == unit & Feasible %in% c(1,2,3),
                            .(SuitMax = min(Feasible)), by = .(Spp,BGC)]
            feasMax <- feasMax[Spp != "X",]
            tabOut <- data.table::dcast(feasMax, BGC ~ Spp, value.var = "SuitMax")
        }else{
            id <- as.numeric(input$edaplot_selected)
            idSub <- idDat[ID == id,.(ID,Edatopic)]
            edaSub <- eda[idSub, on = "Edatopic"]
            edaSub <- edaSub[BGC == unit,]
            dat <- feas[SS_NoSpace %in% edaSub$SS_NoSpace & Feasible %in% c(1,2,3),]
            tabOut <- data.table::dcast(dat, SS_NoSpace ~ SppSplit, value.var = "Feasible")
            setnames(tabOut, old = c("SS_NoSpace"), new = c("BGC"))
        }
        tabOut
    })
    
    observeEvent({c(input$map_polygon_click, input$edaplot_selected)},{
        if(!is.null(input$map_polygon_click)){
            dat <- prepTable()
            output$hotTab <- renderRHandsontable({
                rhandsontable(dat)
            })
        }
    })
    
    observeEvent(input$submitdat,{
        shinyalert("Enter your initials:", type = "input", inputId = "initials", callbackR = sendToDb)
    })
        
    sendToDb <- function(nme){
        dat <- as.data.table(hot_to_r(input$hotTab))
        if(!is.null(input$edaplot_selected)){
            dat <- melt(dat, id.vars = "BGC", value.name = "Feas_New", variable.name = "Spp")
            setnames(dat, old = c("BGC","Spp"), new = c("SS_NoSpace","SppSplit"))
            datComb <- feas[dat, on = c("SS_NoSpace","SppSplit")]
            datComb[,Spp := NULL]
            datComb <- datComb[!is.na(Feas_New),]
            datComb <- datComb[Feasible != Feas_New,]
            datComb[,Modifier := nme]
            setnames(datComb, c("bgc","unit","sppsplit","feasible","new","modifier"))
            dbWriteTable(con, name = "edatope_updates", value = datComb, row.names = F, append = T)
            shinyalert("Thank you!","Your updates have been recorded", type = "info", inputId = "dbmessage")
        }
    }

    output$edaplot <- renderGirafe({
        gg <- ggplot()+
            geom_blank()+
            scale_y_discrete(limits = c("7","6","5","4","3","2","1","0"))+
            scale_x_discrete(limits = c("A","B","C","D","E"))+
            geom_rect_interactive(data = rects, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, 
                                                    tooltip = labs, data_id = ids), 
                                  fill = "grey", col = "purple")+
            geom_hline(aes(yintercept = grd1y), linetype = "dashed")+
            geom_vline(aes(xintercept = grd1x), linetype = "dashed")+
            theme_bw(base_size = 10)+
            theme(panel.grid.major = element_blank())+
            labs(x = "SNR", y = "SMR")+
            coord_fixed()
        
        girafe(ggobj = gg,
               options = list(opts_selection(type = "single")))
    })
    
    observeEvent(input$map_polygon_click, {
        event <- input$map_polygon_click
        temp <- regmatches(event,regexpr("tooltip.{5}[[:upper:]]*[[:lower:]]*[[:digit:]]?",event))
        unit <- gsub("tooltip.{3}","",temp)
        output$temp <- renderText({
            paste0("Current unit: ", unit)
        })
    })
    
    output$info <- renderText({
        paste0("You have selected: ", labs[as.numeric(input$edaplot_selected)])
    })
    
    onStop(function() {
        dbDisconnect(conn = con)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)



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
