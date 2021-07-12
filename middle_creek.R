library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(plotly)
library(corrgram)
library(corrplot)
library(sp)
library(sf)
library(leaflet)
library(tidyverse)
library(stringr)
library(here)
library(widgetframe)
library(htmltools)
library(htmlwidgets)
library(tmap)
library(rgdal)

##################################################################################
## File/Shapefile Upload, Preparation, and Transformations

mc.all <- st_read("All Parcels to be Flooded.shp")
mc.select <- select(mc.all, -c(OWNER))
mc.attributes <- read_csv("MiddleCreekAtributes.csv")
mc.merge <- merge(mc.attributes, mc.select, by="PARCEL")
middle.creek.spatial <- st_as_sf(mc.merge)
middle.creek.project <- st_transform(middle.creek.spatial, "+proj=longlat +datum=WGS84")

study_area <- st_read("study_area.shp")
study_area.project <- st_transform(study_area, "+proj=longlat +datum=WGS84")

flood.zones <- st_read("FloodMerge.shp")
flood.zones.project <- st_transform(flood.zones, "+proj=longlat +datum=WGS84")

levees <- st_read("Levee_Routes.shp")
levees.project <- st_transform(levees, "+proj=longlat +datum=WGS84")
goodline <- st_zm(levees.project, drop = T, what = "ZM")


ma.levees <- st_read("ma-17.shp")
ma.attributes <- read.csv("Ma_17_Attributes.csv")
ma.merge <- merge(ma.levees, ma.attributes, by="Id")
ma.spatial <- st_as_sf(ma.merge)
ma.project <- st_transform(ma.spatial, "+proj=longlat +datum=WGS84")


##################################################################################
##color palettes
bins <- c(0, 0.5, 1.1)
bins1 <- c(0, 0.5, 1.1, 2.1)

color.trial <- read_csv("MiddleCreekColorTrial.csv")

pal <- colorBin(c("#B8F3FF", "#00AFBB", "#C8B6FF"), domain = middle.creek.project$Priority_Parcel, bins = bins1)
pal1 <- colorBin(c("#B8F3FF", "#00AFBB", "#C9A227"), domain = middle.creek.project$Partial_Parcel_Acquisition, 
                 bins = bins1)
pal2 <- colorBin(c("#B8F3FF", "#00AFBB"), domain = middle.creek.project$Purchased,bins = bins)
pal3 <- colorFactor(palette = c('#FF5D8F','#F94144', '#577590', '#F8961E', '#83E377','#55A630'), 
                    domain = flood.zones.project$FLD_ZONE)
pal4 <- colorBin(c("#FFD6FF", "#7AE582"), domain = middle.creek.project$Consultant_Number, bins = bins)
pal5 <- colorBin(c("#B8F3FF", "#00AFBB", "#DD1D0E"), domain = color.trial$Purchased ,bins = bins1)
##################################################################################
## Leaflet Map
middle.creek <- 
  middle.creek.project %>%
  mutate(popup = str_c("<strong>", OWNER, "<strong>",
                       "<br/>",
                       "APN #: ", APN,
                       "<br/>",
                       "Is Priority Parcel? ", Is_Priority_Parcel,
                       "<br/>",
                       "Is Purchased? ", Is_Purchased)%>%
           map(htmltools::HTML))%>%
  mutate(popup1 = str_c("<strong>", OWNER, "<strong>",
                        "<br/>",
                        "APN #: ", APN,
                        "<br/>",
                        "Is Partial Parcel? ", Is_Partial_Parcel,
                        "<br/>",
                        "Is Purchased? ", Is_Purchased)%>%
           map(htmltools::HTML))%>%
  mutate(popup2 = str_c("<strong>", OWNER, "<strong>",
                        "<br/>",
                        "APN #: ", APN,
                        "<br/>",
                        "Is Purchased? ", Is_Purchased,
                        "<br/>",
                        "Year Purchased - ", Year_Purchased,
                        "<br/>",
                        "Parcel in Acquisition Process? ", Is_In_Acquisition_Process)%>%
           map(htmltools::HTML))%>%
  mutate(popup3 = str_c("<strong>", OWNER, "<strong>",
                        "<br/>",
                        "APN #: ", APN,
                        "<br/>",
                        "Consultant: ", Consultant)%>%
           map(htmltools::HTML))%>%
  leaflet()%>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
  addProviderTiles(providers$Esri.DeLorme, group = "Steet Map") %>%
  addMiniMap(
    position = "topright",
    tiles = providers$Esri.NatGeoWorldMap,
    toggleDisplay = TRUE)%>%
  addPolygons(data = study_area.project,
              color = "#FCF300",
              weight = 2,
              smoothFactor = 0.5,
              opacity = 1.0,
              fillOpacity = 0,
              group = "Study Area")%>%
  addPolygons(label = ~popup,
              fillColor = ~pal(Priority_Parcel),
              color = "#444444",
              weight = 1,
              smoothFactor = 0.5,
              opacity = 1.0,
              fillOpacity = 0.80,
              highlightOptions = highlightOptions(color = "black",
                                                  weight = 2,
                                                  bringToFront = TRUE),
              group = "Priority Parcels")%>%
  addPolygons(label = ~popup1,
              fillColor = ~pal1(Partial_Parcel_Acquisition),
              color = "#444444",
              weight = 1,
              smoothFactor = 0.5,
              opacity = 1.0,
              fillOpacity = 0.80,
              highlightOptions = highlightOptions(color = "black",
                                                  weight = 2,
                                                  bringToFront = TRUE),
              group = "Partial Parcel Acquisitions")%>%
  addPolygons(label = ~popup2,
              fillColor = ~pal5(Purchased),
              color = "#444444",
              weight = 1,
              smoothFactor = 0.5,
              opacity = 1.0,
              fillOpacity = 0.80,
              highlightOptions = highlightOptions(color = "black",
                                                  weight = 2,
                                                  bringToFront = TRUE),
              group = "Purchased as of 5/25/21")%>%
  addPolygons(label = ~popup3,
              fillColor = ~pal4(Consultant_Number),
              color = "#444444",
              weight = 1,
              smoothFactor = 0.5,
              opacity = 1.0,
              fillOpacity = 0.80,
              highlightOptions = highlightOptions(color = "black",
                                                  weight = 2,
                                                  bringToFront = TRUE),
              group = "Consultant Parcels")%>%
  addPolylines(data = goodline,
               label = ~UnitName,
               color = '#7B2CBF',
               weight = 5,
               group = "District Levees")%>%
  addPolylines(data = ma.project,
               label = ~Name,
               color = '#7B2CBF',
               weight = 5,
               group = "MA-17 Levees")%>%
  addPolygons(data = flood.zones.project,
              label = ~FLD_ZONE,
              fillColor = ~pal3(FLD_ZONE),
              color = "#444444",
              weight = 1,
              smoothFactor = 0.5,
              opacity = 1.0,
              fillOpacity = 0.65,
              group = "FEMA Flood Zones")%>%
  addLegend(pal = pal3,
            values = ~flood.zones.project$FLD_ZONE,
            opacity = 0.7,
            labels = c("A","AE","AO", "0.2 PCT ANNUAL CHANCE FLOOD HAZARD", "D", "X"),
            title = "FEMA Flood Zones",
            position = "bottomright")%>%
  addLayersControl(
    position = "topright",
    baseGroups = c("Imagery","Street Map"),
    overlayGroups = c("Purchased as of 5/25/21", "Partial Parcel Acquisitions",  "Priority Parcels", 
                      "Consultant Parcels", "FEMA Flood Zones", "District Levees", "MA-17 Levees", 
                      "Study Area"),
    options = layersControlOptions(collapsed = FALSE))%>%
  hideGroup(c("Partial Parcel Acquisitions", "Priority Parcels", "Consultant Parcels", 
              "FEMA Flood Zones", "District Levees", "MA-17 Levees")) #%>%
middle.creek %>%
  setView(lng = -122.891142,
          lat = 39.126292,
          zoom = 13)

#######################################################################################
## Middle Creek Summary Stats
summary(middle.creek.project)

mc.filter.purchased <- filter(middle.creek.project, Is_Purchased == "Yes")
mc.filter.not.purchased <- filter(middle.creek.project, Is_Purchased == "No")

purchased.acres <- sum(mc.filter.purchased$ACRES)
not.purchased.acres <- sum(mc.filter.not.purchased$ACRES)
total.acres <-purchased.acres + not.purchased.acres #1890.603 Acres
percentage.purchased <- purchased.acres/total.acres * 100
print(percentage.purchased)
