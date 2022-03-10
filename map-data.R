library(maps)
library(sf)
library(tools)

sf_use_s2(FALSE)

var.maxt <- tidync("data/MAXT.nc")
var.mint <- tidync("data/MINT.nc")
var.prec <- tidync("data/PREC.nc")

#######################################################################################################
####################################         BASE MAP DATA         #################################### 
#######################################################################################################
# This map data is from ggplot2. Map data can easily be replaced with raster maps from Google Maps, 
# Statem, or OpenStreetMaps using the ggmaps package. The maps can be skinned with roads, terrain,
# buildings, etc. This may be useful depending on what our research question will be.
map <- st_as_sf(map("world", plot = FALSE, fill = TRUE))
map <- cbind(map, st_coordinates(st_centroid(map)))
map$ID <- toTitleCase(as.vector(map$ID))
map$ID <- ifelse(map$ID == "USA", "", map$ID)

us.states <- map_data("state")
us.states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
us.states <- cbind(us.states, st_coordinates(st_centroid(us.states)))
us.states$ID <- toTitleCase(as.vector(us.states$ID))

world.map <- st_as_sf(map("world", plot = FALSE, fill = TRUE))
world.map <- cbind(map, st_coordinates(st_centroid(map)))

base.map <- c(geom_sf(data = world.map, fill = NA),
              geom_text(data = map, aes(X, Y, label = ID), size = 2),
              geom_sf(data = us.states, fill = NA),
              geom_text(data = us.states, aes(X, Y, label = ID), size = 2),
              coord_sf(xlim = c(min(var.maxt$transforms$lon$lon) - 360,
                                max(var.maxt$transforms$lon$lon) - 360),
                       ylim = c(min(var.maxt$transforms$lat$lat),
                                max(var.maxt$transforms$lat$lat)),
                       expand = FALSE))
