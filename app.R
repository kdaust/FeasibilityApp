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
edaMaxCol <- "#2fa631ff"
edaMinCol <- "#c91a1aff"
grRamp <- colorRamp(c(edaMaxCol,edaMinCol),alpha = T) ##colour ramp for gray values

##setup species picker
treelist <- fread("Tree_List_2020.csv")
treelist <- treelist[Bad != "x",.(TreeCode,Group)]
sppList <- list()
for(nm in c("Conifer_BC","Broadleaf_BC","Conifer_Native","Broadleaf_Native")){
    temp <- treelist[Group == nm, TreeCode]
    sppList[[nm]] <- temp
}
bc_init <- st_read(dsn = "BC_Init.gpkg")
bc_init <- as.data.table(bc_init)
bc_init[cols, BGC_Col := i.Col, on = "BGC"]
wna_init <- st_read(dsn = "WNA_Small_Tiled.gpkg")
wna_init <- as.data.table(wna_init)
wna_init[cols, BGC_Col := i.Col, on = "BGC"]
wna_med <- st_read(dsn = "WNA_Tiled_12_BC.gpkg")
wna_med <- as.data.table(wna_med)
wna_med[cols, BGC_Col := i.Col, on = "BGC"]
grd_big <- st_read(dsn = "WNA_GrdID_900.gpkg") %>% st_transform(4326)
#wna_big <- st_read(dsn = "WNA_Tiled_900.gpkg")
#wna_big <- as.data.table(wna_big)
#wna_big[cols, BGC_Col := i.Col, on = "BGC"]

feas <- fread("Feasibility_v11_22.csv")
feas <- feas[,.(BGC,SS_NoSpace,Spp,Feasible)]
setnames(feas, old = "Spp",new = "SppSplit")
feas[,Spp := SppSplit]
feas[SppSplit %in% c("Fdi","Fdc"),Spp := "Fd"]
feas[SppSplit %in% c("Pli","Plc"),Spp := "Pl"]
feas[SppSplit %in% c("Se","Sw","Sxw","Sxl","Sxs","Ss"),Spp := "Sx"]
feas[SppSplit %in% c("Pyi","Pyc"),Spp := "Py"]
setkey(feas,BGC,SppSplit)
feasOrig <- feas

eda <- fread("Edatopic_v11_20.csv")
eda <- eda[is.na(Special),.(BGC,SS_NoSpace,Edatopic)]
eda[,SMR := as.numeric(gsub("[[:alpha:]]","", Edatopic))]
treeLocs <- fread("TreeSppLocations.csv")
treeLocs <- treeLocs[,.(Spp,Latitude,Longitude,`Plot Number`)]
##max suitability colours
suitcols <- data.table(Suit = c(1,2,3),Col = c("#443e3dFF","#736e6eFF","#a29f9eFF"))#c("#42CF20FF","#ECCD22FF","#EC0E0EFF")
##climatic suitability colours
zonalOpt <- "#3e6837ff"
wetOpt <- data.table(Feasible = c(1,2,3), Col = c("#c24f00ff","#cd804bff","#fbbd92ff"))
splitOpt <- "#df00a9ff"
dryOpt <- data.table(Feasible = c(1,2,3), Col = c("#000aa3ff","#565edeff","#8b8fdbff"))

##legends
leg <- legend_element(
    variables = c("No Restrictions","Dry Limited","Wet Limited","Split Feasibility","Added","Removed")
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
    , title = "Max Feasibility"
)
maxSuitLeg <- mapdeck_legend(leg)
leg <- legend_element(
    variables = c("Low Feasibility","Best Feasibility")
    , colours = c(edaMinCol,edaMaxCol)
    , colour_type = "fill"
    , variable_type = "gradient",
    title = "Edatopic Feasibility"
)
edaLeg <- mapdeck_legend(leg)

set_token("pk.eyJ1Ijoia2lyaWRhdXN0IiwiYSI6ImNraDJjOTNxNzBucm0ycWxxbTlrOHY5OTEifQ.GybbrNS0kJ3VZ_lGCpXwMA")

# Define UI for application that draws a histogram
ui <- navbarPage("Species Feasibility",
                 tabPanel("WNA Map",
                          useShinyalert(),
                          useShinyjs(),
                          fluidPage(
                                  column(3,
                                         h2("Welcome to the tree feasibility investigation tool!"),
                                         p("Select a species code to investigate its feasibility on the map. You can select different
                                           summaries by subzone/variant, or select an edatopic position on the chart below to show
                                           feasibility in all site series in that space (unclick a selected edatope to return to summaries).
                                           Click on a map polygon to show the feasibilities in a table format, which you can update and
                                           submit to the database. To view updated feasibility instead of original feasibility, click the button
                                           (and click again to return to original feasibility)."),
                                         br(),
                                         h2("Options"),
                                         awesomeRadio("wnaORbc",
                                                      label = "Select BC or all of WNA",
                                                      choices = c("BC","WNA"),
                                                      inline = T,
                                                      selected = "BC"),
                                         pickerInput("sppPick",
                                                     label = "Select Tree Species",
                                                     choices = sppList,
                                                     selected = "Pl (All)"),
                                         switchInput("bgcLayer",label = "Show BGC",value = T,labelWidth = "80px"),
                                         switchInput("updatedfeas","Updated Feas",value = F, labelWidth = "100px"),
                                         switchInput("showtrees",label = "Show Plots", value = F, labelWidth = "100px"),
                                         br(),
                                         h2("Summary type"),
                                         awesomeRadio("type",
                                                      label = "Select Summary by Subzone",
                                                      choices = c("Climatic Suit","P/A"),
                                                      selected =  "Climatic Suit"),
                                         h4("Or Select Edatopic Space: \n"),
                                         girafeOutput("edaplot", width = "400px")
                                  ),
                                  column(9,
                                         withSpinner(
                                             mapdeckOutput("map", height = 700),
                                             type = 6
                                         ),
                                         br(),
                                         h2("Suitability data for selected polygon:"),
                                         p("Edit the feasibility values here. When you click submit, 
                                            the updated values will be sent to a database. If you are looking
                                           at updated values, they will be shown with a pink background on the table."),
                                         textOutput("tableInfo"),
                                         fluidRow(
                                             rHandsontableOutput("hot"),
                                             hidden(actionBttn("submitdat", label = "Submit!")),
                                             hidden(actionBttn("addspp","Add Current Species")),
                                             hidden(actionBttn("removespp","Remove Species"))
                                         )
                                  )
                              
                              )
                          )
                          
                 )


server <- function(input, output) {
    globalFeas <- reactiveValues(dat = feasOrig)
    globalRendered <- reactiveValues(med = vector("numeric"), big = vector("numeric"))
    globalPoly <- reactiveValues(Small = bc_init, Big = wna_med[WNABC == "BC",])
    globalLeg <- reactiveValues(Legend = climaticLeg)
    
    testCanAdd <- function(){
        if(input$type == "P/A" & is.null(input$edaplot_selected)){
            event <- input$map_polygon_click
            if(!is.null(event)){
                temp <- regmatches(event,regexpr("tooltip.{5}[[:upper:]]*[[:lower:]]*[[:digit:]]?_?[[:upper:]]{,2}",event))
                unit <- gsub("tooltip.{3}","",temp)
                feas <- globalFeas$dat
                spp <- unique(feas[BGC == unit,Spp])
                if(!substr(input$sppPick,1,2) %in% spp){
                    return(TRUE)
                }
            }
        }
        return(FALSE)
    }
    
    testCanRemove <- function(){
        if(input$type == "P/A" & is.null(input$edaplot_selected)){
            event <- input$map_polygon_click
            if(!is.null(event)){
                return(TRUE)
            }
        }
        return(FALSE)
    }
    
    observeEvent({c(input$type,input$map_polygon_click,input$edaplot_selected)},{
        toggle(id = "addspp", condition = testCanAdd())
        toggle(id = "removespp", condition = testCanRemove())
    })
    
    observe({
        toggle(id = "submitdat", condition = !is.null(input$edaplot_selected))
    })
    
    observeEvent(input$wnaORbc,{
        if(input$wnaORbc == "WNA"){
            globalPoly$Small <- wna_init
            globalPoly$Big <- wna_med
        }else{
            globalPoly$Small <- bc_init
            globalPoly$Big <- wna_med[WNABC == "BC",]
        }
    }, priority = 50)
    
    ##update global feasibility values
    observeEvent(input$updatedfeas,{
        print("Updating feasibility")
        if(input$updatedfeas){
            newDat <- dbGetQuery(con, "SELECT * FROM edatope_updates")
            newDat <- as.data.table(newDat)
            newDat <- newDat[,.(unit,sppsplit,new)]
            setnames(newDat, c("SS_NoSpace","SppSplit","NewFeas"))
            temp <- merge(feasOrig,newDat, by = c("SS_NoSpace","SppSplit"), all = T)
            temp[!is.na(NewFeas), Feasible := NewFeas]
            temp[,NewFeas := NULL]
            temp[is.na(Spp),Spp := SppSplit]
            temp[Spp %in% c("Fdi","Fdc"),Spp := "Fd"]
            temp[Spp %in% c("Pli","Plc"),Spp := "Pl"]
            temp[Spp %in% c("Se","Sw","Sxw","Sxl","Sxs","Ss"),Spp := "Sx"]
            temp[Spp %in% c("Pyi","Pyc"),Spp := "Py"]
            temp[is.na(BGC),BGC := gsub("/.*","",SS_NoSpace)]
            setcolorder(temp, colnames(feasOrig))
            globalFeas$dat <- temp
        }else{
            globalFeas$dat <- feasOrig
        }
        
    }, priority = 20)
    
    ##base BGC map -- done
    output$map <- renderMapdeck({
        dat <- st_as_sf(globalPoly$Small)
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
    
    observeEvent(input$bgcLayer,{
        if(!input$bgcLayer){
            print("removing bgc")
            dat <- st_as_sf(globalPoly$Small)
            mapdeck_update(map_id = "map") %>%
                add_polygon(dat,
                            layer_id = "bgcmap",
                            fill_colour = "purple",
                            fill_opacity = 0,
                            tooltip = "BGC",
                            auto_highlight = F,
                            focus_layer = F,
                            update_view = F)
        }else{
            dat <- st_as_sf(globalPoly$Small)
            mapdeck_update(map_id = "map") %>%
                mapdeck_view(location = c(-124.72,54.56), zoom = 4) %>%
                add_polygon(dat,
                            layer_id = "bgcmap",
                            fill_colour = "BGC_Col",
                            tooltip = "BGC",
                            auto_highlight = F,
                            focus_layer = F,
                            update_view = F)
        }
    })
    
    observeEvent(input$showtrees,{
        if(input$showtrees){
            dat <- treeLocs[Spp == substr(input$sppPick,1,2),]
            if(nrow(dat) > 1){
                mapdeck_update(map_id = "map") %>%
                    add_scatterplot(data = dat,
                                    layer_id = "tree_points",
                                    lon = "Longitude",
                                    lat = "Latitude",
                                    fill_colour = "black",
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
                            dat <- treeLocs[Spp == substr(input$sppPick,1,2),]
                            if(nrow(dat) > 1){
                                mapdeck_update(map_id = "map") %>%
                                    add_scatterplot(data = dat,
                                                    layer_id = "tree_points2",
                                                    lon = "Longitude",
                                                    lat = "Latitude",
                                                    fill_colour = "black",
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
        feas <- globalFeas$dat
        tempFeas <- feas[Spp == substr(input$sppPick,1,2) & Feasible %in% c(1,2,3),]
        minDist <- tempFeas[,.SD[Feasible == min(Feasible, na.rm = T)],by = .(BGC)]
        abUnits <- minDist[grep("[[:alpha:]] */[[:alpha:]]+$",SS_NoSpace),]
        noAb <- minDist[!grepl("[[:alpha:]] */[[:alpha:]]+$",SS_NoSpace),]
        abUnits <- eda[abUnits, on = "SS_NoSpace"]
        abUnits <- abUnits[,.(Temp = if(any(grepl("C4",Edatopic))) paste0(SS_NoSpace,"_01") else SS_NoSpace, Feasible = Feasible[1]),
                           by = .(BGC,SS_NoSpace,SppSplit,Spp)]
        abUnits[,SS_NoSpace := NULL]
        setnames(abUnits,old = "Temp",new = "SS_NoSpace")
        minDist <- rbind(noAb,abUnits)
        minDist[,ID := if(any(grepl("01", SS_NoSpace)) & Feasible[1] == 1) T else F, by = .(BGC)]
        green <- minDist[(ID),]
        green <- green[,.(Col = zonalOpt), by = .(BGC)]
        
        minDist <- minDist[ID == F,]
        minDist[,ID := if(any(grepl("01", SS_NoSpace))) T else F, by = .(BGC)]
        blue <- minDist[(ID),]
        blue <- blue[,.(Feasible = min(Feasible)), by = .(BGC)]
        
        minDist <- minDist[ID == F,]
        minEda <- eda[minDist, on = "SS_NoSpace"]
        minEda <- minEda[,.(AvgEda = mean(SMR)), by = .(BGC,SS_NoSpace,Feasible)]
        minEda <- minEda[,.(Col = fifelse(all(AvgEda >= 3.5),"WET",
                                          fifelse(all(AvgEda < 3.5), "DRY", splitOpt)), Feasible = min(Feasible)), by = .(BGC)]
        temp <- minEda[Col == "DRY",]
        temp[,Col := NULL]
        blue <- rbind(blue,temp)
        red <- minEda[Col == "WET",]
        minEda <- minEda[!Col %in% c("WET","DRY"),.(BGC,Col)]
        blue[dryOpt, Col := i.Col, on = "Feasible"]
        red[wetOpt, Col := i.Col, on = "Feasible"]
        blue[,Feasible := NULL]
        red[,Feasible := NULL]
        climSuit <- rbind(green,blue,red,minEda)
        climSuit <- climSuit[!is.na(BGC),]
        
        tf2 <- feas[Spp == substr(input$sppPick,1,2) & Feasible %in% c(4,5),
                    .(SuitMax = min(Feasible)), by = .(BGC)]
        if(nrow(tf2) > 0){
            tf2[SuitMax == 4,Col := "#fbff00ff"]
            tf2[SuitMax == 5,Col := "#8300ffff"]
            tf2 <- tf2[,.(BGC,Col)]
            climSuit <- rbind(climSuit, tf2)
        }
        return(climSuit)
    })
    
    ##Prepare BGC colour table for non-edatopic
    prepDatSimple <- reactive({
        feas <- globalFeas$dat
        feasMax <- feas[Spp == substr(input$sppPick,1,2) & Feasible %in% c(1,2,3,4,5),
                        .(SuitMax = min(Feasible)), by = .(BGC,SppSplit)]
        if(input$type == "P/A"){
            if(length(unique(feasMax$SppSplit)) > 1){
                feasMax[,SppNum := as.numeric(as.factor(SppSplit))]
                tempCol <- grRamp(rescale(feasMax$SppNum,to = c(0,0.6)))
                feasMax[,Col := rgb(tempCol[,1],tempCol[,2],tempCol[,3],tempCol[,4], maxColorValue = 255)]
                temp <- unique(feasMax[,.(SppSplit,Col)])
                
                leg <- legend_element(
                    variables = c(temp$SppSplit,"Added","Removed")
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
                    variables = c(feasMax$SppSplit[1],"Added","Removed")
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
        feasMax[,Lab := BGC]
        feasMax[,.(BGC,Col,Lab)]
    })
    
    ##Prepare BGC colours for edatopic option
    prepEdaDat <- reactive({
        feas <- globalFeas$dat
        globalLeg$Legend <- edaLeg
        id <- as.numeric(input$edaplot_selected)
        idSub <- idDat[ID == id,.(ID,Edatopic)]
        edaSub <- eda[idSub, on = "Edatopic"]
        feasSub <- feas[Spp == substr(input$sppPick,1,2) & Feasible %in% c(1,2,3),]
        feasSub <- feasSub[SS_NoSpace %chin% edaSub$SS_NoSpace,]
        feasSub[,Lab := paste0(SS_NoSpace,": ", Feasible)]
        feasSum <- feasSub[,.(FeasVal = mean(Feasible), Lab = paste(Lab, collapse = "<br>")), by = BGC]
        tempCol <- grRamp(rescale(feasSum$FeasVal,to = c(0,1)))
        feasSum[,Col := rgb(tempCol[,1],tempCol[,2],tempCol[,3],tempCol[,4], maxColorValue = 255)]
        feasSum[,.(BGC,Col,Lab)]
    })
    
    ##join colours to small map
    mergeSmallDat <- reactive({
        #browser()
        if(is.null(input$edaplot_selected)){
            feasDat <- prepDatSimple()
        }else{
            feasDat <- prepEdaDat()
        }
        temp <- globalPoly$Small[feasDat, on = "BGC"]
        temp <- temp[!is.na(BGC_Col),]
        if(nrow(temp) == 0){
            shinyalert("Oh oh!","There are no suitable locations for this selection!", type = "error")
            return(NULL)
        }else{
            temp <- temp[!ID %in% c(globalRendered$med,globalRendered$big),]
            st_as_sf(temp)
        }
        
    })
    
    ##join colours to big map
    mergeMedDat <- function(tid){
        if(is.null(input$edaplot_selected)){
            feasDat <- prepDatSimple()
        }else{
            feasDat <- prepEdaDat()
        }
        temp <- globalPoly$Big[ID %in% tid,]
        temp <- temp[feasDat, on = "BGC"]
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
        if(nrow(dat) < 1) dat <- NULL
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
    
    ##prepare suitability table when polygon clicked
    prepTable <- reactive({
        event <- input$map_polygon_click
        temp <- regmatches(event,regexpr("tooltip.{5}[[:upper:]]*[[:lower:]]*[[:digit:]]?_?[[:upper:]]{,2}",event))
        unit <- gsub("tooltip.{3}","",temp)
        #browser()
        print(unit)
        feas <- globalFeas$dat
        idx_row <- NULL
        idx_col <- NULL
        if(is.null(input$edaplot_selected)){
            if(input$type == "Climatic Suit"){
                tempFeas <- feas[BGC == unit & Feasible %in% c(1,2,3),]
                tempEda <- eda[tempFeas, on = "SS_NoSpace"]
                tempEda <- tempEda[!is.na(SMR),]
                tempEda <- tempEda[,.(AvgSMR = mean(SMR)), by = .(SS_NoSpace,Feasible,SppSplit)]
                tempEda[,SSType := fifelse(AvgSMR < 3.5,"Dry",
                                           fifelse(AvgSMR > 4.1,"Wet","Zonal"))]
                tabOut <- dcast(tempEda, SSType ~ SppSplit, value.var = "Feasible", fun.aggregate = min)
                tabOut[tabOut == 0] <- NA
            }else{
                feasMax <- feas[BGC == unit & Feasible %in% c(1,2,3,4),
                                .(SuitMax = min(Feasible)), by = .(SppSplit,BGC)]
                feasMax <- feasMax[SppSplit != "X",]
                tabOut <- data.table::dcast(feasMax, BGC ~ SppSplit, value.var = "SuitMax")
            }
        }else{
            id <- as.numeric(input$edaplot_selected)
            idSub <- idDat[ID == id,.(ID,Edatopic)]
            edaSub <- eda[idSub, on = "Edatopic"]
            edaSub <- edaSub[BGC == unit,]
            dat <- feas[SS_NoSpace %in% edaSub$SS_NoSpace & Feasible %in% c(1,2,3),]
            tabOut <- data.table::dcast(dat, SS_NoSpace ~ SppSplit, value.var = "Feasible", fun.aggregate = mean)
            setnames(tabOut, old = c("SS_NoSpace"), new = c("BGC"))
            if(input$updatedfeas){
                dat2 <- feasOrig[SS_NoSpace %in% edaSub$SS_NoSpace & Feasible %in% c(1,2,3),]
                dat2 <- dat2[,.(SS_NoSpace,SppSplit,Feasible)]
                setnames(dat2, old = "Feasible", new = "FeasOld")
                comp <- merge(dat,dat2,on = c("SS_NoSpace","SppSplit"),all = T)
                comp[,Same := (Feasible == FeasOld) & !is.na(Feasible) & !is.na(FeasOld)]
                tabOrig <- data.table::dcast(comp, SS_NoSpace ~ SppSplit, value.var = "Same",fun.aggregate = function(x){x[1]})
                idx <- which(tabOrig == F, arr.ind = T)
                idx_row <- unname(idx[,1] - 1)
                idx_col <- unname(idx[,2] - 1)
            }
            
        }
        spp <- colnames(tabOut)
        spp[spp %in% c("Se","Sw","Sxw","Sxl","Sxs","Ss")] <- "Sx"
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
                rhandsontable(data = dat,col_highlight = temp$cIdx, row_highlight = temp$rIdx, spp_highlight = temp$sppCol) %>%
                    hot_cols(renderer = "
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
        if(!is.null(input$edaplot_selected)){
            feas <- globalFeas$dat
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
    
    output$hot_add <- renderRHandsontable({
        if(!is.null(input$map_polygon_click)){
            spp <- substr(input$sppPick,1,2)
            spp2 <- unique(feasOrig[Spp == spp, SppSplit])
            event <- input$map_polygon_click
            temp <- regmatches(event,regexpr("tooltip.{5}[[:upper:]]*[[:lower:]]*[[:digit:]]?_?[[:upper:]]{,2}",event))
            unit <- gsub("tooltip.{3}","",temp)
            edaSub <- unique(eda[BGC == unit,.(BGC,SS_NoSpace)])
            temp <- as.data.table(expand.grid(edaSub$SS_NoSpace,spp2))
            temp[,Val := NA_integer_]
            outTab <- dcast(temp, Var1 ~ Var2, value.var = "Val")
            setnames(outTab, old = "Var1",new = "SS_NoSpace")
            rhandsontable(data = outTab)
        }
    })
    
    observeEvent(input$addspp,{
        shinyalert(html = T,
                   text = tagList(
                       h4("Add species by site unit, then click submit"),
                       rHandsontableOutput("hot_add"),
                       textInput("addsppMod",label = "Enter your initials:")
                   ),
                   callbackR = addSppToDb,
                   showCancelButton = T,
                   showConfirmButton = T)
    })
    
    addSppToDb <- function(x){
        if(x){
            dat <- hot_to_r(input$hot_add)
            dat <- as.data.table(dat)
            feas <- globalFeas$dat
            dat <- melt(dat, id.vars = "SS_NoSpace", value.name = "Feas_New", variable.name = "Spp")
            setnames(dat, old = c("Spp"), new = c("SppSplit"))
            datComb <- feas[dat, on = c("SS_NoSpace","SppSplit")]
            datComb[,Spp := NULL]
            datComb <- datComb[!is.na(Feas_New),]
            datComb[,Modifier := input$addsppMod]
            datComb[,BGC := gsub("/.*","",SS_NoSpace)]
            setnames(datComb, c("bgc","unit","sppsplit","feasible","new","modifier"))
            dbWriteTable(con, name = "edatope_updates", value = datComb, row.names = F, append = T)
            shinyalert("Thank you!","Your updates have been recorded", type = "info", inputId = "dbmessage")
        }
    }
    
    output$hot_delete <- renderRHandsontable({
        if(!is.null(input$map_polygon_click)){
            feas <- globalFeas$dat
            event <- input$map_polygon_click
            temp <- regmatches(event,regexpr("tooltip.{5}[[:upper:]]*[[:lower:]]*[[:digit:]]?_?[[:upper:]]{,2}",event))
            unit <- gsub("tooltip.{3}","",temp)
            feasMax <- feas[BGC == unit & Feasible %in% c(1,2,3,4),
                            .(SuitMax = min(Feasible)), by = .(SppSplit,BGC)]
            feasMax <- feasMax[SppSplit != "X",]
            nSpp <- length(unique(feasMax$SppSplit))
            temp <- matrix(data = rep(T,nSpp),nrow = 1, ncol = nSpp, byrow = T)
            colnames(temp) <- unique(feasMax$SppSplit)
            rhandsontable(data = temp)
        }
    })
    
    observeEvent(input$removespp,{
        shinyalert(html = T,
                   text = tagList(
                       h4("Uncheck a species to remove"),
                       rHandsontableOutput("hot_delete"),
                       textInput("removesppMod",label = "Enter your initials:")
                   ),
                   callbackR = removeSppToDb,
                   showCancelButton = T,
                   showConfirmButton = T,
                   size = "m")
    })
    
    removeSppToDb <- function(x){
        if(x){
            event <- input$map_polygon_click
            temp <- regmatches(event,regexpr("tooltip.{5}[[:upper:]]*[[:lower:]]*[[:digit:]]?_?[[:upper:]]{,2}",event))
            unit <- gsub("tooltip.{3}","",temp)
            dat <- hot_to_r(input$hot_delete)
            toRemove <- colnames(dat)[dat[1,] == F]
            temp <- feas[BGC == unit & SppSplit %in% toRemove,]
            temp[,new := 5]
            temp[,Modifier := input$removesppMod]
            temp[,Spp := NULL]
            setnames(temp, c("bgc","unit","sppsplit","feasible","new","modifier"))
            dbWriteTable(con, name = "edatope_updates", value = temp, row.names = F, append = T)
            shinyalert("Thank you!","Your updates have been recorded", type = "info", inputId = "dbmessage")
        }
    }

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
               options = list(opts_selection(type = "single")))
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
            if(input$type == "Climatic Suit"){
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
