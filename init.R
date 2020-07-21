# install.packages("CASdatasets", repos = "http://dutangc.free.fr/pub/RRepos/", type="source")
library(tidyverse)

require(MASS)
library(CASdatasets)
?CASdatasets

data(freMTPL2freq)
str(freMTPL2freq)

# library(GADMTools)
# map <- gadm_sp_loadCountries(c("FRA"),level=2,basefile ="./")
# gadm_plot(map)
# 
# library(sf)
# belgium_shape_sf <- st_read('./FRA_adm/FRA_adm1.shp', quiet = TRUE)
# belgium_shape_sf <- st_transform(belgium_shape_sf, "+proj=longlat +datum=WGS84")
# class(belgium_shape_sf)
# ## [1] "sf"         "data.frame"
# belgium_shape_sf %>% as_tibble() %>% select(-geometry) %>% slice(1:3) 
# ggplot(belgium_shape_sf) +
#   geom_sf() +
#   ggtitle("Welcome to Belgium!") +
#   theme_bw()
# 
# france_shape_sf <- st_read('./FRA_adm/FRA_adm4.shp', quiet = TRUE)
# france_shape_sf <- st_transform(france_shape_sf, "+proj=longlat +ellps=WGS84 +datum=WGS84" )
# class(france_shape_sf)
# ## [1] "sf"         "data.frame"
# belgium_shape_sf %>% as_tibble() %>% select(-geometry) %>% slice(1:3) 
# ggplot(belgium_shape_sf) +
#   geom_sf() +
#   ggtitle("Welcome to Belgium!") +
#   theme_bw()
# 
# france_shape_sf_10km <- st_read('./France_shapefile/fr_10km.shp', quiet = TRUE)
# france_shape_sf_10km <- st_transform(france_shape_sf_10km, "+proj=longlat +datum=WGS84")
# 
# france <- st_read("./codes_postaux_V5/codes_postaux_region.shp")
# france<- st_transform(france, "+proj=longlat +ellps=WGS84 +datum=WGS84" )
# 
# island_sf <- st_read("./ISL_adm/ISL_adm2.shp")
# 
# 
# library(rgdal)
# my_shape <- readOGR("./FRA_adm/FRA_adm1.shp", encoding = "UTF-8", use_iconv = TRUE)
# my_shape <- spTransform(my_shape, CRS("+proj=longlat +datum=WGS84"))
# my_shape$id <- row.names(my_shape)
# unique(my_shape@data$NAME_1)
# class(my_shape)
# myDF <- fortify(my_shape)
# ggplot(data = myDF, aes(x = long, y = lat, group = group)) + geom_path()
# 
# # fin exposure per shapefile
# post_expo <- freMTPL2freq %>% group_by(Region) %>% summarize(num = sum(ClaimNb), total_expo = sum(Exposure)) 
# post_expo


# my_shape@data <- left_join(my_shape@data, post_expo, by = c("NAME_1" = "Region"))
# # athuga missing NA values vegna france letters
# my_shape@data$freq <- my_shape@data$num/my_shape@data$total_expo
# 
# # Convert to data frame
# shape_f <- fortify(my_shape)
# shape_f <- left_join(shape_f, my_shape@data)
# 
# 
# plot.eda.map <- ggplot(shape_f, 
#                        aes(long, lat, group = group)) + 
#   geom_polygon(aes(fill = freq), 
#                size = 0.1)
# plot.eda.map <- plot.eda.map + theme_bw() + 
#   labs(fill = "Relative\nfrequency") 
# plot.eda.map
# 
# ggplot(shape_f) +
#   geom_sf(aes(fill = freq), colour = NA) +
#   ggtitle("MTPL claim frequency data") +
#   scale_fill_gradient(low = "#99CCFF", 
#                       high = "#003366") 


france_shape_sf <- st_read('./FRA_adm/FRA_adm1.shp', quiet = TRUE)
france_shape_sf <- st_transform(france_shape_sf, "+proj=longlat +datum=WGS84")
ggplot(france_shape_sf) +
  geom_sf() +
  ggtitle("Welcome to Belgium!") +
  theme_bw()

# 
# iceland_sf <- st_read('./ISL_adm/ISL_adm2.shp')
# iceland_sf <- st_transform(iceland_sf, "+proj=longlat +datum=WGS84")
# ggplot(iceland_sf) +
#   geom_sf() +
#   ggtitle("Welcome to Iceland!") +
#   theme_bw()
# 
# mapview::mapview(iceland_sf)



post_expo <- freMTPL2freq %>% group_by(Region) %>% summarize(num = sum(ClaimNb), total_expo = sum(Exposure)) 
post_expo

# laga postocde names
unique(freMTPL2freq$Region)
unique(france_shape_sf$NAME_1)
france_shape_sf$NAME_1 <- as.character(france_shape_sf$NAME_1)
france_shape_sf$NAME_1[france_shape_sf$NAME_1 == "Rhône-Alpes"] <- "Rhone-Alpes"
france_shape_sf$NAME_1[france_shape_sf$NAME_1 == "Île-de-France"] <- "Ile-de-France"
france_shape_sf$NAME_1[france_shape_sf$NAME_1 == "Franche-Comté"] <- "Franche-Comte"
france_shape_sf$NAME_1[france_shape_sf$NAME_1 == "Midi-Pyrénées"] <- "Midi-Pyrenees"
france_shape_sf$NAME_1[france_shape_sf$NAME_1 == "Provence-Alpes-Côte d'Azur"] <- "Provence-Alpes-Cotes-D'Azur"
france_shape_sf$NAME_1[france_shape_sf$NAME_1 == "Pays de la Loire"] <- "Pays-de-la-Loire"

france_shape_sf <- left_join(france_shape_sf, 
                              post_expo, 
                              by = c("NAME_1" = "Region"))

france_shape_sf$freq <- france_shape_sf$num/france_shape_sf$total_expo

ggplot(france_shape_sf) +
  geom_sf(aes(fill = freq), 
          colour = "black", size = 0.1) +
  ggtitle("MTPL claim frequency data") + 
  labs(fill = "Relative\nexposure") +
  theme_bw()























