rm(list = ls())

library(tidyverse)
library(sf)
library(ggmap)
library(rgdal)

# Some configuration parameters

IMAGE_DIR      <- "images/"
NYC_CORD       <- c(lon = -73.95, lat = 40.7)
NYC_ZOOM       <- 10
NYC_PATH_ZOOM  <- 12
BBOX_FACTOR    <- 2.5
MAP_STYLE      <- c(feature = "all", element = "labels", visibility = "off")
MAP_TYPE       <- "satellite"
HEATMAP_COLORS <- c("yellow","red")
ALPHA_MIN      <- 0.3
ALPHA_MAX      <- 0.7

# There must be a file called `.api_key` with the google maps API key
# to retrieve maps from google
API_KEY <- read_file(".api_key") %>% str_trim()
register_google(API_KEY)


### Just the district data without overlay
dists <- st_read("NYCZips/ZIP_CODE_040114.shp")

df.cases <- read.csv("tests-by-zcta.csv", stringsAsFactors = F) %>%
  mutate(MODZCTA = as.character(MODZCTA))

dists <- dists %>% left_join(df.cases, by = c("ZIPCODE" = "MODZCTA"))
# Impute missing data as zero
dists$Positive <- ifelse(is.na(dists$Positive),0,dists$Positive)

ggplot(dists, aes(fill=Positive)) + geom_sf() + scale_fill_gradientn(colors = HEATMAP_COLORS)

### Google map overlay

NYCmap = get_googlemap( NYC_CORD, zoom = NYC_ZOOM, style = MAP_STYLE, maptype = MAP_TYPE)
ggmap(NYCmap)

# Read Zipcode data in a format that can be overlayed
NYCZips <- readOGR("./NYCZips/") %>% spTransform(CRS("+proj=longlat +datum=WGS84"))
ggplot(data = NYCZips, aes(x = long, y = lat, group = group)) + geom_path()

# convert to a format in which case data can be merged in

# Rasters the shape data into a single dataframe and joins case data
df.cases.rastered  <- NYCZips %>%
  fortify(region = "ZIPCODE") %>%
  left_join(NYCZips@data, by = c("id" = "ZIPCODE")) %>%
  left_join(df.cases, by=c("id" = "MODZCTA"))

cases.max <- max(df.cases.rastered$Positive, na.rm = T)
df.cases.rastered$alpha <- ifelse(is.na(df.cases.rastered$Positive),0,(1-ALPHA_MIN)*df.cases.rastered$Positive/cases.max+ALPHA_MIN)

# Just the heatmap. Save as PNG so we can retain an alpha channel
ggplot(df.cases.rastered, aes(long, lat, group = group, fill = Positive, alpha = alpha)) +
  geom_polygon() +
  scale_fill_gradientn(colors = HEATMAP_COLORS) +
  scale_alpha_continuous(range=c(0, ALPHA_MAX)) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_continuous(name="",breaks = NULL) +
  scale_y_continuous(name="",breaks = NULL)
ggsave(paste0(IMAGE_DIR,"Heatmap_NYC_Full_No_Overlay_With_Alpha.png"))


# Full heatmap with overlay
ggmap(NYCmap) +
  geom_polygon(data=df.cases.rastered, aes(long, lat, group=group, fill=Positive, alpha=alpha)) +
  scale_fill_gradientn(colors = HEATMAP_COLORS) +
  scale_alpha_continuous(range=c(0, ALPHA_MAX)) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_continuous(name="",breaks = NULL) +
  scale_y_continuous(name="",breaks = NULL)
ggsave(paste0(IMAGE_DIR, "Heatmap_NYC_Full_No_Path.jpg"))

# retrieve the movement path
path <- readOGR("./path/","polylines9") %>% spTransform(CRS("+proj=longlat +datum=WGS84"))

bbox <- list(
  xlim = c(
    min(path@lines[[1]]@Lines[[1]]@coords[,1]),
    max(path@lines[[1]]@Lines[[1]]@coords[,1])),
  ylim = c(
    min(path@lines[[1]]@Lines[[1]]@coords[,2]),
    max(path@lines[[1]]@Lines[[1]]@coords[,2]))
  )

path.center <- c(mean(bbox$xlim),mean(bbox$ylim))

# stretch the bounding box to the required size
bbox <- list(
  xlim = c(
    path.center[1]-(path.center[1]-bbox$xlim[1])*BBOX_FACTOR,
    path.center[1]+(bbox$xlim[2]-path.center[1])*BBOX_FACTOR),
  ylim = c(
    path.center[2]-(path.center[2]-bbox$ylim[1])*BBOX_FACTOR,
    path.center[2]+(bbox$ylim[2]-path.center[2])*BBOX_FACTOR
  )
)



# Full heatmap with overlay and movement path
ggmap(NYCmap) +
  geom_polygon(data=df.cases.rastered, aes(long, lat, group=group, fill=Positive, alpha=alpha)) +
  geom_path(data=path, aes(long, lat), color="blue") +
  scale_fill_gradientn(colors=HEATMAP_COLORS) +
  scale_alpha_continuous(range=c(0, ALPHA_MAX)) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_continuous(name="",breaks = NULL) +
  scale_y_continuous(name="",breaks = NULL)
ggsave(paste0(IMAGE_DIR,"Heatmap_NYC_Full_With_Path.jpg"))

# Path only for overlay
ggplot(data=path, aes(long, lat), color="blue", width=3) +
  geom_path() +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_continuous(name="",breaks = NULL) +
  scale_y_continuous(name="",breaks = NULL) +
  coord_equal(xlim = bbox$xlim, ylim = bbox$ylim)
ggsave(paste0(IMAGE_DIR,"Path_only.png"))

# Detailed view of the path

NYCmap.path = get_googlemap( path.center, zoom = NYC_PATH_ZOOM, style = MAP_STYLE, maptype = MAP_TYPE)
ggmap(NYCmap.path)

# Detailed heatmap data without overlay, PNG for alpha channel
ggplot(df.cases.rastered, aes(long, lat, group = group, fill = Positive, alpha = alpha)) +
  geom_polygon() +
  scale_fill_gradientn(colors = HEATMAP_COLORS) +
  scale_alpha_continuous(range=c(0, ALPHA_MAX)) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_continuous(name="",breaks = NULL) +
  scale_y_continuous(name="",breaks = NULL) +
  coord_equal(xlim = bbox$xlim, ylim = bbox$ylim)
ggsave(paste0(IMAGE_DIR,"Heatmap_NYC_Detailed_No_Overlay_With_Alpha.png"))

# Detailed heatmap with overlay
ggmap(NYCmap.path) +
  geom_polygon(data=df.cases.rastered, aes(long, lat, group=group, fill=Positive, alpha=alpha)) +
  scale_fill_gradientn(colors = HEATMAP_COLORS) +
  scale_alpha_continuous(range=c(0, ALPHA_MAX)) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_continuous(name="",breaks = NULL) +
  scale_y_continuous(name="",breaks = NULL) +
  coord_equal(xlim = bbox$xlim, ylim = bbox$ylim)
ggsave(paste0(IMAGE_DIR,"Heatmap_NYC_Detailed_No_Path.jpg"))

# Detailed heatmap with overlay and path
ggmap(NYCmap.path) +
  geom_polygon(data=df.cases.rastered, aes(long, lat, group=group, fill=Positive, alpha=alpha)) +
  geom_path(data=path, aes(long, lat), color="blue", size=1) +
  scale_fill_gradientn(colors = HEATMAP_COLORS) +
  scale_alpha_continuous(range=c(0, ALPHA_MAX)) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_continuous(name="",breaks = NULL) +
  scale_y_continuous(name="",breaks = NULL) +
  coord_equal(xlim = bbox$xlim, ylim = bbox$ylim)
ggsave(paste0(IMAGE_DIR,"Heatmap_NYC_Detailed_With_Path.jpg"))