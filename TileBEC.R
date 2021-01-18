library(rmapshaper)
library(sf)
library(data.table)
library(tidyverse)

wna <- st_read(dsn = "WNA_BGC_v12_12Oct2020.gpkg")
wna <- wna[,c("BGC","State","geom")]
wna_s1 <- ms_simplify(wna, keep = 0.1, sys = T)
wna_s1 <- st_buffer(wna_s1, dist = 0)
wna_outline <- st_read(dsn = "WNA_State_Boundaries.gpkg")
wna_outline <- ms_simplify(wna_outline, keep = 0.1)
wna_outline$State[wna_outline$State != "BC"] <- "AB"
wna_outline <- wna_outline %>%
  group_by(State) %>%
  summarise(geom = st_union(geom))

bc_init <- st_read(file.choose())
bc_init <- st_buffer(bc_init, dist = 0)
grd <- st_read(file.choose())
bc_s2 <- st_intersection(bc_init,grd)
inter2 <- st_transform(bc_s2,4326) %>% 
  st_cast("MULTIPOLYGON") %>% st_cast("POLYGON")
st_write(inter2, dsn = "BC_Init.gpkg")

BCOutline <- wna_outline[wna_outline$State == "BC",]
BCOutline <- ms_simplify(BCOutline,keep = 0.25)
wna_out2 <- st_union(wna_outline)
wna_out2 <- ms_simplify(wna_out2,keep = 0.5)
wna_out2 <- st_buffer(wna_out2, dist = 0)
wna_s2 <- st_intersection(wna_s1,wna_out2)

grd <- st_make_grid(wna, n = c(30,30))
grd <- st_as_sf(data.frame(ID = 1:900, geom = grd))
inter <- st_intersection(wna_s2,grd)
inter2 <- st_transform(wna_s2,4326) %>% 
  st_cast("MULTIPOLYGON") %>% st_cast("POLYGON")
st_write(inter2, dsn = "WNA_Tiled_900.gpkg")
st_write(grd, dsn = "WNA_GrdID_900.gpkg")

wna_s2 <- st_read(file.choose())
wna_s2 <- st_transform(wna_s2, 3005) %>% st_buffer(dist = 0)
colnames(wnaOut)[1] <- "WNABC"
wna_s2 <- st_intersection(wna_s2, wnaOut)
bc_s2 <- st_intersection(wna_s2, wnaOut)

wna_s3 <- ms_simplify(wna_s2, keep = 0.4, sys = T)
grd2 <- st_read("./FeasibilityApp/FeasibilityWNA/WNA_GrdID_900.gpkg")
#grd2 <- st_as_sf(data.frame(ID = 1:12, geom = grd2))
wna_s3 <- st_buffer(wna_s3, dist = 0)
inter3 <- st_intersection(wna_s3, grd2)
inter3 <- st_transform(wna_s2,4326) %>% 
  st_cast("MULTIPOLYGON") %>% st_cast("POLYGON")
st_write(inter3, dsn = "WNA_Tiled_12_BC.gpkg")
st_write(grd2, dsn = "WNA_GrdID_12.gpkg")

wna_s4 <- ms_simplify(wna_s4, keep = 0.4, sys = T)
wna_s2 <- st_read(file.choose())
wna_s4 <- st_transform(wna_s2,4326) %>% 
st_cast("POLYGON")
st_write(wna_s4, dsn = "WNA_Tiled_12_BC.gpkg")
