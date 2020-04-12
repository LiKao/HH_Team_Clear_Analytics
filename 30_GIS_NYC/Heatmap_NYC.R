rm(list = ls())

library(tidyverse)
library(sf)
library(ggmap)
library(rgdal)

dists <- st_read("ZIP_CODE_040114.shp")

df.cases <- read.csv("tests-by-zcta.csv", stringsAsFactors = F) %>%
  mutate(MODZCTA = as.character(MODZCTA))

dists <- dists %>% left_join(df.cases, by = c("ZIPCODE" = "MODZCTA"))

dists$Positive <- ifelse(is.na(dists$Positive),0,dists$Positive)

ggplot(dists, aes(fill=Positive)) + geom_sf() + scale_fill_gradientn(colors=c("white","yellow","red"))


NYCmap = get_googlemap( c(lon = -73.95, lat = 40.7), zoom = 10,
                        style = c(feature = "all", element = "labels", visibility = "off"),
                        maptype = "satellite")
ggmap(NYCmap)

NYCZips <- readOGR("./NYCZips/") %>% spTransform(CRS("+proj=longlat +datum=WGS84"))
ggplot(data = NYCZips, aes(x = long, y = lat, group = group)) + geom_path()

# convert to a format in which case data can be merged in

# Rasters the shape data into a single dataframe and joins case data
df.cases.rastered  <- NYCZips %>%
  fortify(region = "ZIPCODE") %>%
  left_join(NYCZips@data, by = c("id" = "ZIPCODE")) %>%
  left_join(df.cases, by=c("id" = "MODZCTA"))

cases.max <- max(df.cases.rastered$Positive, na.rm = T)
df.cases.rastered$alpha <- ifelse(is.na(df.cases.rastered$Positive),0,df.cases.rastered$Positive/cases.max+0.3)


ggplot(df.cases, aes(long, lat, group=group, fill=Positive)) +
  geom_polygon() +
  scale_fill_gradientn(colors=c("white","yellow","red"))


ggmap(NYCmap) +
  geom_polygon(data=df.cases.rastered, aes(long, lat, group=group, fill=Positive, alpha=alpha)) +
  scale_fill_gradientn(colors=c("yellow","red")) +
  scale_alpha_continuous(range=c(0,0.7)) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_continuous(name="",breaks = NULL) +
  scale_y_continuous(name="",breaks = NULL)

# retrieve the movement path
path <- readOGR("./path/","doc-line") %>% spTransform(CRS("+proj=longlat +datum=WGS84"))

ggmap(NYCmap) +
  geom_polygon(data=df.cases.rastered, aes(long, lat, group=group, fill=Positive, alpha=alpha)) +
  geom_path(data=path, aes(long, lat), color="blue", width=3) +
  scale_fill_gradientn(colors=c("yellow","red")) +
  scale_alpha_continuous(range=c(0,0.7)) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_continuous(name="",breaks = NULL) +
  scale_y_continuous(name="",breaks = NULL)


# Detailed view of the path

NYCmap.path = get_googlemap( c(lon = -74, lat = 40.75), zoom = 12,
                             style = c(feature = "all", element = "labels", visibility = "off"),
                             maptype = "satellite")
ggmap(NYCmap.path)

ggmap(NYCmap.path) +
  geom_polygon(data=df.cases.rastered, aes(long, lat, group=group, fill=Positive, alpha=alpha)) +
  scale_fill_gradientn(colors=c("yellow","red")) +
  scale_alpha_continuous(range=c(0,0.7)) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_continuous(name="",breaks = NULL) +
  scale_y_continuous(name="",breaks = NULL) +
  coord_equal(xlim=c(-74.1,-73.9), ylim=c(40.68,40.82))

ggmap(NYCmap.path) +
  geom_polygon(data=df.cases.rastered, aes(long, lat, group=group, fill=Positive, alpha=alpha)) +
  geom_path(data=path, aes(long, lat), color="blue", size=1.5) +
  scale_fill_gradientn(colors=c("yellow","red")) +
  scale_alpha_continuous(range=c(0,0.7)) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_continuous(name="",breaks = NULL) +
  scale_y_continuous(name="",breaks = NULL) +
  coord_equal(xlim=c(-74.1,-73.9), ylim=c(40.68,40.82))
