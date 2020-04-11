rm(list = ls())

library(tidyverse)
library(sf)
dists <- st_read("ZIP_CODE_040114.shp")

df.cases <- read.csv("tests-by-zcta.csv", stringsAsFactors = F) %>%
  mutate(MODZCTA = as.character(MODZCTA))

dists <- dists %>% left_join(df.cases, by = c("ZIPCODE" = "MODZCTA"))

ggplot(dists, aes(fill=Positive)) + geom_sf() + scale_fill_gradientn(colors=c("red","yellow","white"))
