###Kiri Daust
###Nov 2020

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
library(shinyjs)

##connect to database
###Read in climate summary data
drv <- dbDriver("PostgreSQL")
sapply(dbListConnections(drv), dbDisconnect)
con <- dbConnect(drv, user = "postgres", password = "Kiriliny41", host = "68.183.199.104", 
                 port = 5432, dbname = "spp_feas")
#con <- dbConnect(drv, user = "postgres", password = "Kiriliny41", host = "smithersresearch.ca", port = 5432, dbname = "feasibility_update") ## server use


##data for edatopic grid
grd1x <- seq(1.5,4.5,1)
grd1y <- seq(1.5,7.5,1)
rects <- data.table(xmin = rep(c(0.5,3.5), each = 5),
                    xmax = rep(c(3.5,5.5), each = 5),
                    ymin = rep(c(0.5,1.5,3.5,5.5,7.5),2),
                    ymax = rep(c(1.5,3.5,5.5,7.5,8.5),2))
ids <- 1:10
labs <- c("SHD-P","SHG/HG-P","SM/M-P","SX/X-P","VX-P","SHD-R","SHG/HG-R","SM/M-R","SX/X-R","VX-R")
idDat <- expand.grid(SMR = 0:7, SNR = c("A","B","C","D","E"))
idDat <- as.data.table(idDat)
setorder(idDat,SMR,SNR)
idDat[,ID := c(5,5,5,10,10,4,4,4,9,9,4,4,4,9,9,3,3,3,8,8,3,3,3,8,8,2,2,2,7,7,2,2,2,7,7,1,1,1,6,6)]
idDat[,edatopic := paste0(SNR,SMR)]
cols <- fread("./inputs/WNAv12_HexColours.csv")
setnames(cols, c("bgc","Col"))
alpha <- "4D"
cols[,Col := paste0(Col,alpha)]
edaMaxCol <- "#00fa00ff"
edaMinCol <- "#fa0000ff"
grRamp <- colorRamp(c(edaMaxCol,edaMinCol),alpha = T) ##colour ramp for gray values
grRamp2 <- colorRamp(c("#443e3dFF","#c0c0c0ff"),alpha = T) ##colour ramp for gray values

##setup species picker
treelist <- fread("./inputs/Tree_List_2020.csv")
treelist <- treelist[Bad != "x",.(TreeCode,Group)]
treelist <- rbind(treelist,data.table(TreeCode = "None",Group = "Conifer_BC"))
sppList <- list()
for(nm in c("Conifer_BC","Broadleaf_BC","Conifer_Native","Broadleaf_Native")){
    temp <- treelist[Group == nm, TreeCode]
    sppList[[nm]] <- temp
}
allSppNames <- dbGetQuery(con,"select distinct sppsplit from feasorig")[,1]
bc_init <- st_read(dsn = "./inputs/BC_Init.gpkg")
colnames(bc_init)[1] <- "bgc"
bc_init <- as.data.table(bc_init)
bc_init[cols, BGC_Col := i.Col, on = "bgc"]
wna_init <- st_read(dsn = "./inputs/WNA_Small_Tiled.gpkg")
colnames(wna_init)[1] <- "bgc"
wna_init <- as.data.table(wna_init)
wna_init[cols, BGC_Col := i.Col, on = "bgc"]
wna_med <- st_read(dsn = "./inputs/WNA_Tiled_12_BC.gpkg")
colnames(wna_med)[1] <- "bgc"
wna_med <- as.data.table(wna_med)
wna_med[cols, BGC_Col := i.Col, on = "bgc"]
grd_big <- st_read(dsn = "./inputs/WNA_GrdID_900.gpkg") %>% st_transform(4326)

##max suitability colours
eda <- dbGetQuery(con,"select * from eda")
eda <- as.data.table(eda)
suitcols <- data.table(Suit = c(1,2,3),Col = c("#443e3dFF","#736e6eFF","#a29f9eFF"))#c("#42CF20FF","#ECCD22FF","#EC0E0EFF")
##climatic suitability colours
zonalOpt <- "#3e6837ff"
wetOpt <- data.table(feasible = c(1,2,3), Col = c("#c24f00ff","#cd804bff","#fbbd92ff"))
splitOpt <- "#df00a9ff"
dryOpt <- data.table(feasible = c(1,2,3), Col = c("#000aa3ff","#565edeff","#8b8fdbff"))

##legends
leg <- legend_element(
    variables = c("Climatic Optimum","Wet Site Optimum","Dry Site Optimum","Bimodal Feasibility","Off-site Addition","Removed from CFRG")
    , colours = c(zonalOpt, wetOpt$Col[1], dryOpt$Col[1],splitOpt,"#fbff00ff","#8300ffff")
    , colour_type = "fill"
    , variable_type = "category"
    , title = "Climatic Feasibility"
)
climaticLeg <- mapdeck_legend(leg)
leg <- legend_element(
    variables = c("Feas 1","Feas 2","Feas 3")
    , colours = suitcols$Col
    , colour_type = "fill"
    , variable_type = "category"
    , title = "Best Feasibility"
)
maxSuitLeg <- mapdeck_legend(leg)
leg <- legend_element(
    variables = c("Poor Feasibility","Good Feasibility")
    , colours = c(edaMinCol,edaMaxCol)
    , colour_type = "fill"
    , variable_type = "gradient",
    title = "Edatopic Feasibility"
)
edaLeg <- mapdeck_legend(leg)

set_token("pk.eyJ1Ijoia2lyaWRhdXN0IiwiYSI6ImNraDJjOTNxNzBucm0ycWxxbTlrOHY5OTEifQ.GybbrNS0kJ3VZ_lGCpXwMA")

instr <- tagList(
    p("To use this tool:"),
    tags$ol(
        tags$li("Select a species code to view its feasibility on the map."),
        tags$li("Select type of map: A) Presence/Absence, B) Climatic Suitability. C) Suitability in select edatopic space 
                (click again to return to summarised view)."),
        tags$li("Click on a BGC polygon to display feasibility ratings in table format")),
    tags$hr(),
    p("Note: The values in the table can be updated and
                                           submitted to a database unless Climatic Suitability is selected. 
                                           To view updated feasibility toggle the Updated Feasibility Button"),
    p("There are several other options available:"),
    tags$ol(
        tags$li("By default the tool shows only BC. Toggle WNA to see rating across western north america"),
        tags$li("By default the tool does not show the BGC map. Shift slider to show colour-themed BGC"),
        tags$li("Show locations of actual tree species collections/observations in the dataset")),
    tags$hr()
)

# Define UI for application that draws a histogram
ui <- navbarPage("Species Feasibility",theme = "css/bcgov.css",
                 tabPanel("Map",
                          useShinyalert(),
                          useShinyjs(),
                          fluidPage(
                                  column(3,
                                         h3("Spatial Maps of Tree Feasibility Ratings by BGC"),
                                         actionButton("showinstr","Click To Show Instructions"),
                                         h4("Select A Tree Species"),
                                         pickerInput("sppPick",
                                                     label = "",
                                                     choices = sppList,
                                                     selected = "Ba"),                                         
                                         h4("Summary type"),
                                         awesomeRadio("type",
                                                      label = "Select Summary by Subzone",
                                                      choices = c("Presence/Absence","Climatic Suitability"),
                                                      selected =  "Presence/Absence"),
                                         h4("Or Select Edatopic Space: \n"),
                                         girafeOutput("edaplot"),                                         
                                         h3("Options"),
                                         awesomeRadio("wnaORbc",
                                                      label = "Select BC or all of WNA",
                                                      choices = c("BC","WNA"),
                                                      inline = T,
                                                      selected = "BC"),

                                         switchInput("bgcLayer",label = "Show BGC",value = F,labelWidth = "80px"),
                                         switchInput("updatedfeas","Updated Feas",value = F, labelWidth = "100px"),
                                         switchInput("showtrees",label = "Show Plots", value = F, labelWidth = "100px")
                                         
                                  ),
                                  column(9,
                                         withSpinner(
                                             mapdeckOutput("map", height = 700),
                                             type = 6
                                         ),
                                         br(),
                                         h3("Suitability data for selected polygon:"),
                                         p("Edit the feasibility values here. When you click submit, 
                                            the updated values will be sent to a database. If you are looking
                                           at updated values, they will be shown with a pink background on the table."),
                                         textOutput("tableInfo"),
                                         fluidRow(
                                             uiOutput("tableBGC"),
                                             rHandsontableOutput("hot"),
                                             hidden(actionBttn("submitdat", label = "Submit Changes!")),
                                             hidden(actionBttn("addspp","Add Species"))
                                         )
                                  )
                              
                              )
                          )
                          
                 )


server <- function(input, output) {
    globalFeas <- reactiveValues(dat = "feasible")
    globalRendered <- reactiveValues(med = vector("numeric"), big = vector("numeric"))
    globalPoly <- reactiveValues(Small = bc_init, Big = wna_med[WNABC == "BC",])
    globalLocation <- reactiveValues(loc = c(-124.72,54.56), zoom = 4.5)
    globalLeg <- reactiveValues(Legend = climaticLeg)
    
    observeEvent(input$showinstr,{
        shinyalert(title = "Instructions",html = T,text = instr)
    })
    
    testCanAdd <- function(){
        if(input$type == "Presence/Absence" & is.null(input$edaplot_selected)){
            return(TRUE)
        }
        return(FALSE)
    }
    
    # testCanRemove <- function(){
    #     if(input$type == "Presence/Absence" & is.null(input$edaplot_selected)){
    #         event <- input$map_polygon_click
    #         if(!is.null(event)){
    #             return(TRUE)
    #         }
    #     }
    #     return(FALSE)
    # }
    
    observeEvent({c(input$type,input$map_polygon_click,input$edaplot_selected)},{
        toggle(id = "addspp", condition = testCanAdd())
    })
    
    observe({
        toggle(id = "submitdat", condition = input$type != "Climatic Suitability")
    })
    
    observeEvent(input$wnaORbc,{
        if(input$wnaORbc == "WNA"){
            globalPoly$Small <- wna_init
            globalPoly$Big <- wna_med
            globalLocation$loc = c(-116.97,49)
            globalLocation$zoom = 3.5
        }else{
            globalPoly$Small <- bc_init
            globalPoly$Big <- wna_med[WNABC == "BC",]
            globalLocation$loc = c(-124.72,54.56)
            globalLocation$zoom = 4.5
        }
    }, priority = 50)
    
    ##this is the column name in the database
    observeEvent(input$updatedfeas,{
        print("Updating feasibility")
        if(input$updatedfeas){
            globalFeas$dat <- "newfeas"
        }else{
            globalFeas$dat <- "feasible"
        }
        
    }, priority = 20)
    
    ##base BGC map -- done
    output$map <- renderMapdeck({
        dat <- st_as_sf(globalPoly$Small)
        mapdeck() %>%
            mapdeck_view(location = globalLocation$loc, zoom = globalLocation$zoom) %>%
            add_polygon(dat,
                        layer_id = "bgcmap",
                        fill_colour = "BGC_Col",
                        tooltip = "bgc",
                        auto_highlight = F,
                        focus_layer = F,
                        update_view = F)
    })
    
    observeEvent({c(input$bgcLayer,input$wnaORbc)},{
        if(!input$bgcLayer){
            print("removing bgc")
            dat <- st_as_sf(globalPoly$Small)
            mapdeck_update(map_id = "map") %>%
                add_polygon(dat,
                            layer_id = "bgcmap",
                            fill_colour = "purple",
                            fill_opacity = 0,
                            tooltip = "bgc",
                            auto_highlight = F,
                            focus_layer = F,
                            update_view = F)
        }else{
            dat <- st_as_sf(globalPoly$Small)
            mapdeck_update(map_id = "map") %>%
                mapdeck_view(location = globalLocation$loc, zoom = globalLocation$zoom) %>%
                add_polygon(dat,
                            layer_id = "bgcmap",
                            fill_colour = "BGC_Col",
                            tooltip = "bgc",
                            auto_highlight = F,
                            focus_layer = F,
                            update_view = F)
        }
    }, priority = 23)
    
    observeEvent({c(input$showtrees,
                    input$sppPick,
                    input$wnaORbc)},{
        if(input$showtrees){
            sppName <- substr(input$sppPick,1,2)
            if(input$wnaORbc == "BC"){
                QRY <- paste0("select spp,plotnum,geometry from plotdata where spp = '",sppName,"' and region = 'BC'")
            }else{
                QRY <- paste0("select spp,plotnum,geometry from plotdata where spp = '",sppName,"'")
            }
            
            dat <- st_read(con,query = QRY)
            if(nrow(dat) > 1){
                mapdeck_update(map_id = "map") %>%
                    clear_scatterplot(layer_id = "tree_points") %>%
                    clear_scatterplot(layer_id = "tree_points2") %>%
                    add_sf(data = dat,
                            layer_id = "tree_points",
                            fill_colour = "black",
                           tooltip = "plotnum",
                            radius = 1,
                            radius_min_pixels = 5,
                            radius_max_pixels = 10,
                            focus_layer = F,
                            update_view = F)
            }
        }else{
            mapdeck_update(map_id = "map") %>%
                clear_scatterplot(layer_id = "tree_points") %>%
                clear_scatterplot(layer_id = "tree_points2")
        }
    })
    
    observeEvent({c(
        input$sppPick,
        input$type,
        input$edaplot_selected,
        input$updatedfeas,
        input$wnaORbc)
    },{
    dat <- mergeSmallDat()
    print("Rendering map")
    if(!is.null(dat)){
        mapdeck_update(map_id = "map") %>%
            add_polygon(dat,
                        layer_id = "zone",
                        fill_colour = "Col",
                        tooltip = "Lab",
                        auto_highlight = F,
                        focus_layer = F,
                        update_view = F,
                        legend = globalLeg$Legend
            )
    }else{
        mapdeck_update(map_id = "map") %>%
            clear_polygon(layer_id = "zone")
    }

}, priority = 15)
    
    observeEvent({c(
        input$map_view_change)
    },{
        bounds <- input$map_view_change
        if(!is.null(bounds)){
            if(bounds$viewBounds$north - bounds$viewBounds$south > 1 & bounds$viewBounds$north - bounds$viewBounds$south < 2){
                dat <- mergeSmallDat()
                print("Rendering map")
                if(!is.null(dat)){
                    mapdeck_update(map_id = "map") %>%
                        add_polygon(dat,
                                    layer_id = "zone",
                                    fill_colour = "Col",
                                    tooltip = "Lab",
                                    auto_highlight = F,
                                    focus_layer = F,
                                    update_view = F
                        )
                }
            }
        } 
            
    }, priority = 15)
    
    ##decide which to update
    observeEvent({c(
        input$sppPick,
        input$type,
        input$edaplot_selected,
        input$updatedfeas,
        input$map_view_change)},{
            
            bounds <- input$map_view_change
            if(!is.null(bounds) && bounds$viewBounds$north - bounds$viewBounds$south < 2.5){
                if(bounds$viewBounds$north - bounds$viewBounds$south > 0.2){
                    dat <- getTiles_Med()
                    if(!is.null(dat)){
                        globalRendered$med <- c(globalRendered$med, unique(dat$ID))
                        #print("Rendering medium")
                        mapdeck_update(map_id = "map") %>%
                            add_polygon(dat,
                                        layer_id = "tiles_med",
                                        fill_colour = "Col",
                                        tooltip = "Lab",
                                        auto_highlight = F,
                                        focus_layer = F,
                                        update_view = F
                            )
                        if(input$showtrees){
                            sppName <- substr(input$sppPick,1,2)
                            if(input$wnaORbc == "BC"){
                                QRY <- paste0("select spp,plotnum,geometry from plotdata where spp = '",sppName,"' and region = 'BC'")
                            }else{
                                QRY <- paste0("select spp,plotnum,geometry from plotdata where spp = '",sppName,"'")
                            }
                            dat <- st_read(con,query = QRY)
                            if(nrow(dat) > 1){
                                mapdeck_update(map_id = "map") %>%
                                    add_sf(data = dat,
                                           layer_id = "tree_points2",
                                           fill_colour = "black",
                                           tooltip = "plotnum",
                                           radius = 1,
                                           radius_min_pixels = 5,
                                           radius_max_pixels = 10,
                                           focus_layer = F,
                                           update_view = F)
                            }
                        }
                    }
                }else{
                    # dat <- getTiles_Big()
                    # if(!is.null(dat)){
                    #     globalRendered$big <- c(globalRendered$big, unique(dat$ID))
                    #     #print("Rendering big")
                    #     mapdeck_update(map_id = "map") %>%
                    #         add_polygon(dat,
                    #                     layer_id = "tiles_big",
                    #                     fill_colour = "Col",
                    #                     tooltip = "Lab",
                    #                     auto_highlight = F,
                    #                     focus_layer = F,
                    #                     update_view = F
                    #         )
                    # }
                }
            }
        })

    
    observeEvent({
        c(input$sppPick, 
        input$type,
        input$edaplot_selected,
        input$updatedfeas,
        input$wnaORbc)},{
            globalRendered$med <- vector("numeric")
            globalRendered$big <- vector("numeric")
            mapdeck_update(map_id = "map") %>%
                clear_polygon(layer_id = "tiles_big") %>%
                clear_polygon(layer_id = "tiles_med")
        }, priority = 25)
    
    # observeEvent(input$edaplot_selected,{
    #     if(is.null(input$edaplot_selected)){
    #         disable("submitdat")
    #     }else{
    #         enable("submitdat")
    #     }
    # })
    
    ###calculate climatic suitability colours
    prepClimSuit <- reactive({
        # feas <- globalFeas$dat
        # tempFeas <- feas[Spp == substr(input$sppPick,1,2) & Feasible %in% c(1,2,3),]
        QRY <- paste0("select bgc,ss_nospace,sppsplit,spp,",globalFeas$dat,
                      " from feasorig where spp = '",substr(input$sppPick,1,2),
                      "' and ",globalFeas$dat," in (1,2,3,4,5)")
        feas <- as.data.table(dbGetQuery(con, QRY))
        setnames(feas, old = globalFeas$dat, new = "feasible")
        tempFeas <- feas[feasible %in% c(1,2,3),]
        minDist <- tempFeas[,.SD[feasible == min(feasible, na.rm = T)],by = .(bgc)]
        abUnits <- minDist[grep("[[:alpha:]] */[[:alpha:]]+$",ss_nospace),]
        noAb <- minDist[!grepl("[[:alpha:]] */[[:alpha:]]+$",ss_nospace),]
        abUnits <- eda[abUnits, on = "ss_nospace"] ##merge
        abUnits <- abUnits[,.(Temp = if(any(grepl("C4",edatopic))) paste0(ss_nospace,"_01") else ss_nospace, feasible = feasible[1]),
                           by = .(bgc,ss_nospace,sppsplit,spp)]
        abUnits[,ss_nospace := NULL]
        setnames(abUnits,old = "Temp",new = "ss_nospace")
        minDist <- rbind(noAb,abUnits)
        minDist[,ID := if(any(grepl("01", ss_nospace)) & feasible[1] == 1) T else F, by = .(bgc)]
        green <- minDist[(ID),]
        green <- green[,.(Col = zonalOpt), by = .(bgc)]
        
        minDist <- minDist[ID == F,]
        minDist[,ID := if(any(grepl("01", ss_nospace))) T else F, by = .(bgc)]
        blue <- minDist[(ID),]
        blue <- blue[,.(feasible = min(feasible)), by = .(bgc)]
        
        minDist <- minDist[ID == F,]
        minEda <- eda[minDist, on = "ss_nospace"]
        minEda <- minEda[,.(AvgEda = mean(smr)), by = .(bgc,ss_nospace,feasible)]
        minEda <- minEda[,.(Col = fifelse(all(AvgEda >= 3.5),"WET",
                                          fifelse(all(AvgEda < 3.5), "DRY", splitOpt)), feasible = min(feasible)), by = .(bgc)]
        temp <- minEda[Col == "DRY",]
        temp[,Col := NULL]
        blue <- rbind(blue,temp)
        red <- minEda[Col == "WET",]
        minEda <- minEda[!Col %in% c("WET","DRY"),.(bgc,Col)]
        blue[dryOpt, Col := i.Col, on = "feasible"]
        red[wetOpt, Col := i.Col, on = "feasible"]
        blue[,feasible := NULL]
        red[,feasible := NULL]
        climSuit <- rbind(green,blue,red,minEda)
        climSuit <- climSuit[!is.na(bgc),]
        
        tf2 <- feas[feasible %in% c(4,5),.(SuitMax = min(feasible)), by = .(bgc)]
        if(nrow(tf2) > 0){
            tf2[SuitMax == 4,Col := "#fbff00ff"]
            tf2[SuitMax == 5,Col := "#8300ffff"]
            tf2 <- tf2[,.(bgc,Col)]
            climSuit <- rbind(climSuit, tf2)
        }
        return(climSuit)
    })
    
    ##Prepare BGC colour table for non-edatopic
    prepDatSimple <- reactive({
        QRY <- paste0("select bgc,ss_nospace,sppsplit,spp,",globalFeas$dat,
                      " from feasorig where spp = '",substr(input$sppPick,1,2),
                      "' and ",globalFeas$dat," in (1,2,3,4,5)")
        d1 <- tryCatch({
            dbGetQuery(con, QRY)
        },
        error = function(e){
            invisible(lapply(dbListConnections(PostgreSQL()), dbDisconnect))
            con <<- dbConnect(drv, user = "postgres", password = "Kiriliny41", host = "68.183.199.104", 
                             port = 5432, dbname = "spp_feas")
            dat <- dbGetQuery(con, QRY)
            return(dat)
        })
        if(nrow(d1) == 0){
            shinyalert(title = "Oops!",text = "There are no data for that species",
                       type = "error",showConfirmButton = T)
            QRY <- paste0("select bgc,ss_nospace,sppsplit,spp,",globalFeas$dat,
                          " from feasorig where spp = 'Sx' and ",globalFeas$dat," in (1,2,3,4,5)")
            d1 <- dbGetQuery(con, QRY)
        }
        feas <- as.data.table(d1)
        setnames(feas, old = globalFeas$dat, new = "feasible")
        feasMax <- feas[,.(SuitMax = min(feasible)), by = .(bgc,sppsplit)]
        if(input$type == "Presence/Absence"){
            if(length(unique(feasMax$sppsplit)) > 1){
                feasMax[,SppNum := as.numeric(as.factor(sppsplit))]
                tempCol <- grRamp2(rescale(feasMax$SppNum,to = c(0,0.6)))
                feasMax[,Col := rgb(tempCol[,1],tempCol[,2],tempCol[,3],tempCol[,4], maxColorValue = 255)]
                temp <- unique(feasMax[,.(sppsplit,Col)])
                
                leg <- legend_element(
                    variables = c(temp$sppsplit,"Added","Removed")
                    , colours = c(temp$Col,"#fbff00ff","#8300ffff")
                    , colour_type = "fill"
                    , variable_type = "category",
                    title = "Presence/Absence"
                )
                PALeg <- mapdeck_legend(leg)
                globalLeg$Legend <- PALeg
                
                feasMax[SuitMax == 4,Col := "#fbff00ff"]
                feasMax[SuitMax == 5,Col := "#8300ffff"]
            }else{
                feasMax[,Col := "#443e3dFF"]
                feasMax[SuitMax == 4,Col := "#fbff00ff"]
                feasMax[SuitMax == 5,Col := "#8300ffff"]
                
                leg <- legend_element(
                    variables = c(feasMax$sppsplit[1],"Added","Removed")
                    , colours = c("#443e3dFF","#fbff00ff","#8300ffff")
                    , colour_type = "fill"
                    , variable_type = "category",
                    title = "Presence/Absence"
                )
                PALeg <- mapdeck_legend(leg)
                globalLeg$Legend <- PALeg
            }
            
        }else if(input$type == "Max Suit"){
            feasMax[suitcols, Col := i.Col, on = c(SuitMax = "Suit")]
            globalLeg$Legend <- maxSuitLeg
        }else{
            feasMax <- prepClimSuit()
            globalLeg$Legend <- climaticLeg
        }
        feasMax[,Lab := bgc]
        feasMax[,.(bgc,Col,Lab)]

    })
    
    ##Prepare BGC colours for edatopic option
    prepEdaDat <- reactive({
        QRY <- paste0("select bgc,ss_nospace,sppsplit,spp,",globalFeas$dat,
                      " from feasorig where spp = '",substr(input$sppPick,1,2),
                      "' and ",globalFeas$dat," in (1,2,3,4)")
        feas <- as.data.table(dbGetQuery(con, QRY))
        setnames(feas, old = globalFeas$dat, new = "feasible")        
        globalLeg$Legend <- edaLeg
        id <- as.numeric(input$edaplot_selected)
        idSub <- idDat[ID == id,.(ID,edatopic)]
        edaSub <- eda[idSub, on = "edatopic"]
        feasSub <- feas[ss_nospace %chin% edaSub$ss_nospace,]
        feasSub[,Lab := paste0(ss_nospace,": ", feasible)]
        feasSum <- feasSub[,.(FeasVal = mean(feasible), Lab = paste(Lab, collapse = "<br>")), by = bgc]
        tempCol <- grRamp(rescale(feasSum$FeasVal,to = c(0,1)))
        feasSum[,Col := rgb(tempCol[,1],tempCol[,2],tempCol[,3],tempCol[,4], maxColorValue = 255)]
        feasSum[,.(bgc,Col,Lab)]
    })
    
    ##join colours to small map
    mergeSmallDat <- reactive({
        #browser()
        if(input$sppPick == "None"){
            return(NULL)
        }
        if(is.null(input$edaplot_selected)){
            feasDat <- prepDatSimple()
        }else{
            feasDat <- prepEdaDat()
        }
        temp <- globalPoly$Small[feasDat, on = "bgc"]
        temp <- temp[!is.na(BGC_Col),]
        if(nrow(temp) == 0){
            shinyalert("Oh oh!","There are no suitable locations for this selection! :(", type = "error")
            return(NULL)
        }else{
            temp <- temp[!ID %in% c(globalRendered$med,globalRendered$big),]
            st_as_sf(temp)
        }
        
    })
    
    ##join colours to big map
    mergeMedDat <- function(tid){
        if(input$sppPick == "None"){
            return(NULL)
        }
        if(is.null(input$edaplot_selected)){
            feasDat <- prepDatSimple()
        }else{
            feasDat <- prepEdaDat()
        }
        temp <- globalPoly$Big[ID %in% tid,]
        temp <- temp[feasDat, on = "bgc"]
        temp <- temp[!is.na(BGC_Col),]
        st_as_sf(temp)
    }
    
    ##extract tiles based on current view
    getTiles_Med <- reactive({
        bounds <- input$map_view_change
        bnds <- st_bbox(c(xmin = bounds$viewBounds$west, xmax = bounds$viewBounds$east, 
                          ymax = bounds$viewBounds$north,ymin = bounds$viewBounds$south), crs = st_crs(4326))
        bnds <- st_as_sfc(bnds)
        t1 <- st_intersects(grd_big, bnds) %>% as.data.frame()
        ids <- unique(c(t1$row.id,globalRendered$med))
        dat <- mergeMedDat(tid = ids)
        if(is.null(dat) || nrow(dat) < 1) dat <- NULL
        dat
    })
    
    # #join colours to big map
    # mergeBigDat <- function(tid){
    #     if(is.null(input$edaplot_selected)){
    #         feasDat <- prepDatSimple()
    #     }else{
    #         feasDat <- prepEdaDat()
    #     }
    #     temp <- wna_big[ID %in% tid,]
    #     temp <- temp[feasDat, on = "BGC"]
    #     temp <- temp[!is.na(BGC_Col),]
    #     st_as_sf(temp)
    # }
    # 
    # ##extract tiles based on current view
    # getTiles_Big <- reactive({
    #     bounds <- input$map_view_change
    #     bnds <- st_bbox(c(xmin = bounds$viewBounds$west, xmax = bounds$viewBounds$east, 
    #                       ymax = bounds$viewBounds$north,ymin = bounds$viewBounds$south), crs = st_crs(4326))
    #     bnds <- st_as_sfc(bnds)
    #     t1 <- st_intersects(grd_big, bnds) %>% as.data.frame()
    #     ids <- unique(c(t1$row.id,globalRendered$big))
    #     dat <- mergeBigDat(tid = ids)
    #     if(nrow(dat) < 1) dat <- NULL
    #     dat
    # })
    
    output$tableBGC <- renderUI({
        event <- input$map_polygon_click
        if(!is.null(event)){
            temp <- regmatches(event,regexpr("tooltip.{5}[[:upper:]]*[[:lower:]]*[[:digit:]]?_?[[:upper:]]{,2}",event))
            unit <- gsub("tooltip.{3}","",temp)
            tagList(
                h3(paste0("Feasibility for ",unit)),
                br()
            )
        }
    })
    
    ##prepare suitability table when polygon clicked
    prepTable <- reactive({
        event <- input$map_polygon_click
        temp <- regmatches(event,regexpr("tooltip.{5}[[:upper:]]*[[:lower:]]*[[:digit:]]?_?[[:upper:]]{,2}",event))
        unit <- gsub("tooltip.{3}","",temp)
        print(unit)
        #feas <- globalFeas$dat
        idx_row <- NULL
        idx_col <- NULL
        QRY <- paste0("select bgc,ss_nospace,sppsplit,spp,",globalFeas$dat,
                      " from feasorig where bgc = '",unit,"' and ",globalFeas$dat," in (1,2,3,4)")
        feas <- as.data.table(dbGetQuery(con, QRY))
        setnames(feas, old = globalFeas$dat, new = "feasible")   
        if(is.null(input$edaplot_selected)){
            if(input$type == "Climatic Suitability"){
                tempFeas <- feas[feasible %in% c(1,2,3),]
                tempEda <- eda[tempFeas, on = "ss_nospace"]
                tempEda <- tempEda[!is.na(smr),]
                tempEda <- tempEda[,.(AvgSMR = mean(smr)), by = .(ss_nospace,feasible,sppsplit)]
                tempEda[,SSType := fifelse(grepl("01",ss_nospace),"Zonal",fifelse(AvgSMR <= 3.5,"Dry",
                                                                                  fifelse(AvgSMR > 4.1,"Wet","??")))]
                tabOut <- dcast(tempEda, SSType ~ sppsplit, value.var = "feasible", fun.aggregate = min)
                tabOut[tabOut == 0] <- NA
            }else{
                feasSub <- feas[sppsplit != "X",]
                tabOut <- data.table::dcast(feasSub, ss_nospace ~ sppsplit,fun.aggregate = mean, value.var = "feasible")
                tabOut[,lapply(.SD,as.integer),.SDcols = -"ss_nospace"]
            }
        }else{
            id <- as.numeric(input$edaplot_selected)
            idSub <- idDat[ID == id,.(ID,edatopic)]
            edaSub <- eda[idSub, on = "edatopic"]
            edaSub <- edaSub[bgc == unit,]
            dat <- feas[ss_nospace %in% edaSub$ss_nospace & feasible %in% c(1,2,3,4),]
            tabOut <- data.table::dcast(dat, ss_nospace ~ sppsplit, value.var = "feasible", fun.aggregate = mean)
            tabOut[,lapply(.SD,as.integer),.SDcols = -"ss_nospace"]
            if(input$updatedfeas){
                QRY <- paste0("select ss_nospace,sppsplit,feasible from feasorig where bgc = '",
                              unit,"' and feasible in (1,2,3,4)")
                feasOrig <- as.data.table(dbGetQuery(con, QRY))
                dat2 <- feasOrig[ss_nospace %in% edaSub$ss_nospace,]
                setnames(dat2, old = "feasible", new = "FeasOld")
                comp <- merge(dat,dat2,on = c("ss_nospace","sppsplit"),all = T)
                comp[,Same := (feasible == FeasOld) & !is.na(feasible) & !is.na(FeasOld)]
                tabOrig <- data.table::dcast(comp, ss_nospace ~ sppsplit, value.var = "Same",fun.aggregate = function(x){x[1]})
                idx <- which(tabOrig == F, arr.ind = T)
                idx_row <- unname(idx[,1] - 1)
                idx_col <- unname(idx[,2] - 1)
            }
            
        }
        spp <- colnames(tabOut)
        spp[spp %in% c("Se","Sw","Sxw")] <- "Sx"
        spp[spp %in% c("Sxl","Sxs","Ss")] <- "Ss"        
        spp <- substr(spp, 1,2)
        sppCurr <- substr(input$sppPick,1,2)
        if(sppCurr %in% spp){
            sppIdx <- which(spp == sppCurr) - 1
        }else{
            sppIdx <- NULL
        }
        list(dat = tabOut, rIdx = idx_row, cIdx = idx_col, sppCol = sppIdx)
    })
    
    ##render suitability table, colour updated cells
    observeEvent({c(input$map_polygon_click, 
                    input$edaplot_selected,
                    input$sppPick,
                    input$updatedfeas)},{
        if(!is.null(input$map_polygon_click)){
            output$hot <- renderRHandsontable({
                temp <- prepTable()
                dat <- temp$dat
                #browser()
                rhandsontable(data = dat,col_highlight = temp$cIdx,
                              row_highlight = temp$rIdx, spp_highlight = temp$sppCol) %>%
                    hot_cols(format = "0", renderer = "
                function(instance, td, row, col, prop, value, cellProperties) {
                Handsontable.renderers.NumericRenderer.apply(this, arguments);
                if (instance.params) {
                    hcols = instance.params.col_highlight
                    hcols = hcols instanceof Array ? hcols : [hcols]
                    hrows = instance.params.row_highlight
                    hrows = hrows instanceof Array ? hrows : [hrows]
                    hspp = instance.params.spp_highlight
                    hspp = hspp instanceof Array ? hspp : [hspp]
                }
                
                var i;
                for(i = 0; i < 100; i++){
                    if (instance.params && (col === hcols[i] && row === hrows[i])) {
                      td.style.background = 'yellow';
                    }
                    if(instance.params && col === hspp[i]){
                        td.style.background = 'lightgreen';
                    }
                }
                    
            }
                             ")
            })
        }
    })
    
    ##ask for initials and call sendToDb
    observeEvent(input$submitdat,{
        shinyalert("Enter your initials:", type = "input", inputId = "initials", callbackR = sendToDb)
    })
        
    ##compile and send updates to database
    sendToDb <- function(nme){
        dat <- as.data.table(hot_to_r(input$hot))
        dat <- melt(dat, id.vars = "ss_nospace", value.name = "newfeas", variable.name = "sppsplit")
        dat[,mod := nme]
        dbWriteTable(con, "temp_update", dat, overwrite = T)
        dbExecute(con,"UPDATE feasorig 
                  SET newfeas = temp_update.newfeas,
                  mod = temp_update.mod
                  FROM temp_update
                  WHERE feasorig.ss_nospace = temp_update.ss_nospace
                  AND feasorig.sppsplit = temp_update.sppsplit")
        dbExecute(con,"UPDATE feasorig
                  SET newfeas = 5
                  WHERE newfeas IS NULL
                  AND feasible IS NOT NULL")
        shinyalert("Thank you!","Your updates have been recorded", type = "info", inputId = "dbmessage")

    }
    
    output$hot_add <- renderRHandsontable({
        if(!is.null(input$map_polygon_click)){
            # spp <- substr(input$sppPick,1,2)
            # spp2 <- dbGetQuery(con,paste0("select distinct sppsplit from feasorig where spp = '",spp,"'"))[,1]
            event <- input$map_polygon_click
            temp <- regmatches(event,regexpr("tooltip.{5}[[:upper:]]*[[:lower:]]*[[:digit:]]?_?[[:upper:]]{,2}",event))
            unit <- gsub("tooltip.{3}","",temp)
            edaSub <- unique(eda[bgc == unit,.(bgc,ss_nospace)])
            temp <- data.table(ss_nospace = edaSub$ss_nospace, newfeas = NA_integer_)
            rhandsontable(data = temp)
        }
    })
    
    observeEvent(input$addspp,{
        shinyalert(html = T,
                   text = tagList(
                       h4("Select a species, add feasibility, then click submit"),
                       pickerInput("sppPickAdd",
                                   label = "",
                                   choices = allSppNames,
                                   selected = "Ba"), 
                       fluidRow(column(6,rHandsontableOutput("hot_add")),
                                column(6,textInput("addsppMod",label = "Enter your initials:"))),
                       
                   ),
                   callbackR = addSppToDb,
                   showCancelButton = T,
                   showConfirmButton = T)
    })
    
    addSppToDb <- function(x){
        if(x){
            dat <- hot_to_r(input$hot_add)
            dat <- as.data.table(dat)
            dat2 <- data.table(bgc = gsub("/[[:digit:]]*","", dat$ss_nospace),
                               ss_nospace = dat$ss_nospace,
                               sppsplit = input$sppPickAdd,
                               feasible = NA,spp = substr(input$sppPickAdd,1,2), newfeas = dat$newfeas, mod = input$addsppMod)
            
            dat2 <- dat2[!is.na(newfeas),]
            dbWriteTable(con, name = "feasorig", value = dat2, append = T,row.names = F)
            shinyalert("Thank you!","Your updates have been recorded", type = "info", inputId = "dbmessage")
        }
    }
    
    # output$hot_delete <- renderRHandsontable({
    #     if(!is.null(input$map_polygon_click)){
    #         event <- input$map_polygon_click
    #         temp <- regmatches(event,regexpr("tooltip.{5}[[:upper:]]*[[:lower:]]*[[:digit:]]?_?[[:upper:]]{,2}",event))
    #         unit <- gsub("tooltip.{3}","",temp)
    #         QRY <- paste0("select ss_nospace,sppsplit,feasible from feasorig where bgc = '",
    #                       unit,"' and feasible in (1,2,3,4)")
    #         feas <- as.data.table(dbGetQuery(con, QRY))
    #         feasMax <- feas[,.(SuitMax = min(feasible)), by = .(sppsplit)]
    #         feasMax <- feasMax[sppsplit != "X",]
    #         nSpp <- length(unique(feasMax$sppsplit))
    #         temp <- matrix(data = rep(T,nSpp),nrow = 1, ncol = nSpp, byrow = T)
    #         colnames(temp) <- unique(feasMax$sppsplit)
    #         rhandsontable(data = temp)
    #     }
    # })
    # 
    # observeEvent(input$removespp,{
    #     shinyalert(html = T,
    #                text = tagList(
    #                    h4("Uncheck a species to remove"),
    #                    rHandsontableOutput("hot_delete"),
    #                    textInput("removesppMod",label = "Enter your initials:")
    #                ),
    #                callbackR = removeSppToDb,
    #                showCancelButton = T,
    #                showConfirmButton = T,
    #                size = "m")
    # })
    # 
    # removeSppToDb <- function(x){
    #     if(x){
    #         event <- input$map_polygon_click
    #         temp <- regmatches(event,regexpr("tooltip.{5}[[:upper:]]*[[:lower:]]*[[:digit:]]?_?[[:upper:]]{,2}",event))
    #         unit <- gsub("tooltip.{3}","",temp)
    #         dat <- hot_to_r(input$hot_delete)
    #         toRemove <- colnames(dat)[dat[1,] == F]
    #         dbExecute(con,paste0("UPDATE feasorig
    #                   SET newfeas = 5,
    #                   mod = '",input$removesppMod,
    #                              "' WHERE feasorig.bgc = '",unit,
    #                              "' AND feasorig.sppsplit IN ('",
    #                              paste(toRemove, collapse = "','"),"')"))
    # 
    #         shinyalert("Thank you!","Your updates have been recorded", type = "info", inputId = "dbmessage")
    #     }
    # }

    ##render interactive edatopic grid
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
               options = list(opts_selection(type = "single")),
               width_svg = 4, height_svg = 7)
    })
    
    # observeEvent(input$map_polygon_click, {
    #     event <- input$map_polygon_click
    #     temp <- regmatches(event,regexpr("tooltip.{5}[[:upper:]]*[[:lower:]]*[[:digit:]]?",event))
    #     unit <- gsub("tooltip.{3}","",temp)
    #     output$temp <- renderText({
    #         paste0("Current unit: ", unit)
    #     })
    # })
    
    ##table caption
    output$tableInfo <- renderText({
        if(is.null(input$edaplot_selected)){
            if(input$type == "Climatic Suitability"){
                "Zonal feasibility and maximum feasibility in wetter and drier sites"
            }else{
                "Maximum feasibility by subzone"
            }
        }else{
            "Feasibility for each site series overlapping selected edatopic area"
        }
    })
    
    onStop(function() {
        dbDisconnect(conn = con)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
