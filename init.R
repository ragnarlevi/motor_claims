install.packages("CASdatasets", repos = "http://dutangc.free.fr/pub/RRepos/", type="source")
library(tidyverse)

require(MASS)
library(CASdatasets)
?CASdatasets

data(freMTPL2freq)
str(freMTPL2freq)

library(GADMTools)
map <- gadm_sp_loadCountries(c("FRA"),level=2,basefile ="./")
gadm_plot(map)

library(sf)


france_shape_sf <- st_read('./FRA_adm/FRA_adm4.shp', quiet = TRUE)
france_shape_sf <- st_transform(france_shape_sf, "+proj=longlat +ellps=WGS84 +datum=WGS84" )
class(france_shape_sf)
## [1] "sf"         "data.frame"
belgium_shape_sf %>% as_tibble() %>% select(-geometry) %>% slice(1:3) 
ggplot(belgium_shape_sf) +
  geom_sf() +
  ggtitle("Welcome to Belgium!") +
  theme_bw()

france_shape_sf_10km <- st_read('./France_shapefile/fr_10km.shp', quiet = TRUE)
france_shape_sf_10km <- st_transform(france_shape_sf_10km, "+proj=longlat +datum=WGS84")

france <- st_read("./codes_postaux_V5/codes_postaux_region.shp")
france<- st_transform(france, "+proj=longlat +ellps=WGS84 +datum=WGS84" )

island_sf <- st_read("./ISL_adm/ISL_adm2.shp")


library(rgdal)
my_shape <- readOGR("./FRA_adm/FRA_adm1.shp", encoding = "UTF-8", use_iconv = TRUE)
my_shape <- spTransform(my_shape, CRS("+proj=longlat +datum=WGS84"))
my_shape$id <- row.names(my_shape)
unique(my_shape@data$NAME_1)
class(my_shape)
myDF <- fortify(my_shape)
ggplot(data = myDF, aes(x = long, y = lat, group = group)) + geom_path()

# fin exposure per shapefile
post_expo <- freMTPL2freq %>% group_by(Region) %>% summarize(num = sum(ClaimNb), total_expo = sum(Exposure)) 
post_expo


my_shape@data <- left_join(my_shape@data, post_expo, by = c("NAME_1" = "Region"))
# athuga missing NA values vegna france letters
my_shape@data$freq <- my_shape@data$num/my_shape@data$total_expo

# Convert to data frame
shape_f <- fortify(my_shape)
shape_f <- left_join(shape_f, my_shape@data)


plot.eda.map <- ggplot(shape_f, 
                       aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = freq), 
               size = 0.1)
plot.eda.map <- plot.eda.map + theme_bw() + 
  labs(fill = "Relative\nfrequency") 
plot.eda.map

ggplot(shape_f) +
  geom_sf(aes(fill = freq), colour = NA) +
  ggtitle("MTPL claim frequency data") +
  scale_fill_gradient(low = "#99CCFF", 
                      high = "#003366") 




