rm(list = ls())

library(tidyverse)
library(sf)
dists <- st_read("bezirksgrenzen.shp")

df.cases <- read.csv2("bezirkstabelle.csv", stringsAsFactors = F) %>%
  mutate(Bezirk = gsub("\xf6","ö",Bezirk))

dists <- dists %>% left_join(df.cases, by = c("Gemeinde_n" = "Bezirk"))

ggplot(dists, aes(fill=Fallzahl)) + geom_sf() + scale_fill_gradientn(colors=c("red","yellow","white"))
