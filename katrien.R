library(tidyverse)

mtpl_orig <- read.table('data.txt', 
                        header = TRUE)
mtpl_orig <- as_tibble(mtpl_orig)

str(mtpl_orig)



mtpl_orig %>% slice(1:3) %>% select(-LONG, -LAT) 



mtpl <- mtpl_orig %>%
  # rename all columns 
  rename_all(function(.name) {
    .name %>% 
      # replace all names with the lowercase versions
      tolower 
  })
mtpl <- rename(mtpl, expo = exp)

head(mtpl)

mean(mtpl$nclaims)
sum(mtpl$nclaims)/sum(mtpl$expo)
mtpl %>% summarize(emp_freq = sum(nclaims) / sum(expo))



m <- sum(mtpl$nclaims)/sum(mtpl$expo)
m
## [1] 0.1393352
var <- sum((mtpl$nclaims - m * mtpl$expo)^2)/sum(mtpl$expo)
var

mtpl %>% 
  group_by(sex) %>% 
  summarize(emp_freq = sum(nclaims) / sum(expo))

KULbg <- "#116E8A"
g <- ggplot(mtpl, aes(nclaims)) + theme_bw() + 
  geom_bar(col = KULbg, fill = KULbg) + 
  labs(y = "Abs frequency") +
  ggtitle("MTPL - number of claims")
g


KULbg <- "#116E8A"
g <- ggplot(mtpl, aes(nclaims)) + theme_bw() + 
  geom_bar(aes(weight = expo), col = KULbg, 
           fill = KULbg) + 
  labs(y = "Abs freq (in exposure)") +
  ggtitle("MTPL - number of claims")
g

# sums to one?
g <- ggplot(mtpl, aes(nclaims)) + theme_bw()
g + geom_bar(aes(y = (..count..)/sum(..count..)), 
             col = KULbg, fill = KULbg) + 
  labs(y = "Relative frequency") +
  ggtitle("MTPL - relative number of claims")


# BM
g <- ggplot(mtpl, aes(bm)) + theme_bw()
g + geom_histogram(binwidth = 1, col = KULbg, 
                   fill = KULbg, alpha = .5)

g <- ggplot(mtpl, aes(bm)) + theme_bw()
g + geom_histogram(aes(y = (..count..)/sum(..count..)), 
                   binwidth = 1, col = KULbg, 
                   fill = KULbg, alpha = .5) + 
  labs(y = "Relative frequency")

# AGEPH
ggplot(data = mtpl, aes(ageph)) + theme_bw() + 
  geom_histogram(binwidth = 2, col = KULbg, 
                 fill = KULbg, alpha = .5) +
  labs(y = "Absolute frequency") +
  ggtitle("MTPL - age policyholder")

mtpl %>% 
  group_by(ageph) %>% 
  summarize(tot_claims = sum(nclaims), 
            tot_expo = sum(expo), tot_obs = n())


# Spatial data, rgal

library(rgdal)
readShapefile = function(){
  belgium_shape <- readOGR(dsn = path.expand(paste(getwd(), "./PE-pricing-analytics-master/scripts/shape file Belgie postcodes", sep = "")), 
                           layer = "npc96_region_Project1")
  belgium_shape <- spTransform(belgium_shape, CRS("+proj=longlat +datum=WGS84"))
  belgium_shape$id <- row.names(belgium_shape)
  return(belgium_shape)
}

belgium_shape = readShapefile()
class(belgium_shape)
str(belgium_shape@data)

# plot belgium
plot.eda.map <- ggplot(belgium_shape, 
                       aes(long, lat, 
                           group = group)) + 
  geom_polygon(fill = NA, colour = "black", 
               size = 0.1) + 
  theme_bw()
plot.eda.map
require(mapview)
mapview(belgium_shape)

# Disadvantage no df

# Spatial data, sf gives a df

library(sf)
belgium_shape_sf <- st_read('./PE-pricing-analytics-master/scripts/shape file Belgie postcodes/npc96_region_Project1.shp', quiet = TRUE)
belgium_shape_sf <- st_transform(belgium_shape_sf, "+proj=longlat +datum=WGS84")
class(belgium_shape_sf)
## [1] "sf"         "data.frame"
belgium_shape_sf %>% as_tibble() %>% select(-geometry) %>% slice(1:3) 
ggplot(belgium_shape_sf) +
  geom_sf() +
  ggtitle("Welcome to Belgium!") +
  theme_bw()

library(tmap)
# qtm(belgium_shape_sf) # does not work
# shapefile slightly corrupted!
# slightly smooth the shapefile
simple_shp <- st_simplify(belgium_shape_sf, 
                          dTolerance = 0.00001)
# and plot
qtm(simple_shp)
tm_shape(simple_shp) +
  tm_borders(col = KULbg, lwd = 0.5) +
  tm_layout(main.title = 'Welcome to Belgium!', 
            legend.outside = TRUE, frame = FALSE)


post_expo <- mtpl %>% group_by(pc) %>% summarize(num = n(), total_expo = sum(expo)) 
post_expo %>% slice(1:5) 

# 
belgium_shape@data <- left_join(belgium_shape@data, post_expo, by = c("POSTCODE" = "pc"))

belgium_shape@data$freq <- belgium_shape@data$total_expo/belgium_shape@data$Shape_Area
# transform continous
belgium_shape@data$freq_class <- cut(belgium_shape@data$freq, 
                                     breaks = quantile(belgium_shape@data$freq, c(0,0.2,0.8,1), na.rm = TRUE), 
                                     right = FALSE, include.lowest = TRUE, labels = c("low","average","high"))

# Convert to data frame
belgium_shape_f <- fortify(belgium_shape)
belgium_shape_f <- left_join(belgium_shape_f, 
                             belgium_shape@data)


plot.eda.map <- ggplot(belgium_shape_f, 
                       aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = belgium_shape_f$freq_class), 
               colour = "black", size = 0.1)
plot.eda.map <- plot.eda.map + theme_bw() + 
  labs(fill = "Relative\nfrequency") + 
  scale_fill_brewer(palette = "Blues", 
                    na.value = "white")
plot.eda.map


# Additinoally we can do this with the sf package
head(belgium_shape_sf)
head(post_expo)
belgium_shape_sf <- left_join(belgium_shape_sf, 
                              post_expo, 
                              by = c("POSTCODE" = "pc"))

# relative exposure
belgium_shape_sf$freq <- belgium_shape_sf$total_expo/belgium_shape_sf$Shape_Area
belgium_shape_sf$freq_class <- cut(belgium_shape_sf$freq, 
                                   breaks = quantile(belgium_shape_sf$freq, c(0,0.2,0.8,1), na.rm = TRUE),
                                   right = FALSE, include.lowest = TRUE, 
                                   labels = c("low", "average", "high"))
ggplot(belgium_shape_sf) +
  geom_sf(aes(fill = freq_class), 
          colour = "black", size = 0.1) +
  ggtitle("MTPL claim frequency data") + 
  labs(fill = "Relative\nexposure") +
  scale_fill_brewer(palette = "Blues", 
                    na.value = "white") + 
  theme_bw()

library(tmap)
# slightly smooth the shapefile
belgium_shape_sf <- st_simplify(belgium_shape_sf, 
                                dTolerance = 0.00001)
# and plot
tm_shape(belgium_shape_sf) + 
  tm_borders(col = "black") + 
  tm_fill(col = "freq_class", 
          style = "cont", palette = "Blues", 
          colorNA = "white")
tmap_leaflet(last_map())

# Postal code level

post_expo <- mtpl %>% group_by(pc) %>% summarise(num = n(), total_expo = sum(expo))
post_expo %>% slice(1:5)

belgium_shape_sf <- left_join(belgium_shape_sf, 
                              post_expo, 
                              by = c("POSTCODE" = "pc"))

belgium_shape_sf$freq <- 
  belgium_shape_sf$total_expo/belgium_shape_sf$Shape_Area

# Transform frequency to binned
belgium_shape_sf$freq_class <- cut(belgium_shape_sf$freq, 
                                   breaks = quantile(belgium_shape_sf$freq, c(0,0.2,0.8,1), na.rm = TRUE),
                                   right = FALSE, include.lowest = TRUE, 
                                   labels = c("low", "average", "high"))
ggplot(belgium_shape_sf) +
  geom_sf(aes(fill = freq_class), 
          colour = "black", size = 0.1) +
  ggtitle("MTPL claim frequency data") + 
  labs(fill = "Relative\nexposure") +
  scale_fill_brewer(palette = "Blues", 
                    na.value = "white") + 
  theme_bw()

library(tmap)
# slightly smooth the shapefile
belgium_shape_sf <- st_simplify(belgium_shape_sf, 
                                dTolerance = 0.00001)
# and plot
tm_shape(belgium_shape_sf) + 
  tm_borders(col = "black") + 
  tm_fill(col = "freq_class", 
          style = "cont", palette = "Blues", 
          colorNA = "white")


