

library(dplyr)
library(purrr)
library(tibble)

#read in gps file
gps = read.csv("datasets/bat temps/global_site_GPS.csv")


#try daymetr
library(daymetr)
head(gps)

library(tidyverse)
gps2 = gps %>%
  select(site, latitude, longitude)

write.csv(gps2, "gps2.csv", row.names = FALSE)

x = download_daymet_batch(file_location = 'gps2.csv',
                      start = 2012,
                      end = 2015,
                      internal = TRUE)

str(x)
x[[96]]$site


x2 <- x$data |>
  mutate(
    tmean = (tmax..deg.c. + tmin..deg.c.)/2,
    date = as.Date(paste(year, yday, sep = "-"), "%Y-%j")
  )

#only US data - but gives time range an much higher resolution
#VERRRRY difficult to extract - need to use function below

daymet_tbl <- x %>%
  keep(~ is.list(.x) && "data" %in% names(.x)) %>%   # keep only valid entries
  map_dfr(function(x) {
    as_tibble(x$data) %>%
      mutate(
        site = x$site,
        latitude = x$latitude,
        longitude = x$longitude,
        altitude = x$altitude
      )
  })

View(daymet_tbl)

#use geodata and terra
library(geodata)
library(terra)

#this is using data from 1970-2000
wc <- worldclim_global(var = "bio", res = 10)
pts <- vect(gps2, geom = c("longitude", "latitude"), crs = "EPSG:4326")
vals <- terra::extract(wc, pts)

clim <- bind_cols(gps2, as_tibble(vals)) %>%
  select(-ID)

View(clim)

#variables are here:
#bio1 - annual mean temp averaged over 30 years; 1979-2000
#bio5 - max temp of warmest month
#bio6 - min temp of coldest month




