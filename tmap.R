# demographics and housing tenure for DeKal County
# created: December 15, 2017
# last updated: March 22, 2018
# author: Taylor
# tidycensus 

library(tidycensus)
library(tidyverse)
library(tigris)
library(viridis)
library(sf)
library(magrittr)
library(rgdal)
library(tmap)
library(tmaptools)
library(ggplot2)
library(janitor)
library(sp)
library(maptools)
library(htmlwidgets)
library(leaflet)
options(tigris_use_cache = TRUE)

#th_api_acs <- '45544f0d114cfaa037a5566745d18bb8d4778cfa'

# the available variables
#load_variables(year = 2016, dataset = "acs5") %>% View
#load_variables(year = 2010, dataset = "acs5") %>% View

fips_codes %>% View

######
county10 <- get_acs(geography = "tract",
                    variables = c("B19013_001E",
                                  'B25001_001E',
                                  'B25002_002E',
                                  'B25002_003E',
                                  'B25003_001E',
                                  'B25003_002E',
                                  'B25003_003E',
                                  'B02001_001E',
                                  'B02001_002E',
                                  'B02001_003E'),
                    county = "Hart",
                    state = "GA",
                    survey = "acs5",
                    year = 2010,
                    output = 'wide') %>%
  rename(hhincome10 = "B19013_001E",
         tothu10 = 'B25001_001E',
         totocc10 = 'B25002_002E',
         totvac10 = 'B25002_003E',
         tottenure10 = 'B25003_001E',
         ownocc10 = 'B25003_002E',
         rentocc10 = 'B25003_003E',
         tpop10 = 'B02001_001E',
         white10 = 'B02001_002E',
         black10 = 'B02001_003E')  %>%
  mutate(hopct10 = round(100 * (ownocc10/tothu10),1),
         rntpct10 = round(100 * (rentocc10/tothu10),1),
         occpct10 = round(100 * (totocc10/tothu10), 1),
         vacpct10 = round(100 * (totvac10/tothu10), 1),
         whtpct10 = round(100 * (white10/tpop10), 1),
         blkpct10 = round(100 * (black10/tpop10), 1))


# race, housing tenure, housing occupancy
county16 <- get_acs(geography = "tract", 
                    variables = c("B19013_001E",'B25001_001E','B25002_002E',
                                  'B25002_003E','B25003_001E','B25003_002E',
                                  'B25003_003E',  'B02001_001E',
                                  'B02001_002E',
                                  'B02001_003E',
                                  'B17001_001E',
                                  'B17001_002E'),
                    county = "Hart",
                    state = "GA",
                    survey = "acs5",
                    geometry = TRUE,
                    output = 'wide') %>%
  rename(hhincome16 = "B19013_001E",
         tothu16 = 'B25001_001E',
         totocc16 = 'B25002_002E',
         totvac16 = 'B25002_003E',
         tottenure16 = 'B25003_001E',
         ownocc16 = 'B25003_002E',
         rentocc16 = 'B25003_003E',
         tpop16 = 'B02001_001E',
         white16 = 'B02001_002E',
         black16 = 'B02001_003E',
         tpov16 = 'B17001_001E',
         ipov16 = 'B17001_002E') %>%
  mutate(hopct16 = round(100 * (ownocc16/tothu16),1),
         rntpct16 = round(100 * rentocc16/tothu16),1,
         occpct16 = round(100 * totocc16/tothu16), 1,
         vacpct16 = round(100 * totvac16/tothu16), 1,
         whtpct16 = round(100 * (white16/tpop16), 1),
         blkpct16 = round(100 * (black16/tpop16), 1),
         povrt16 = round(100 * (ipov16/tpov16),1))

# now, Join 2010 "tbl_df" to 2015 "sf"
hart <- merge(county16,county10, by = 'GEOID')
class(hart)

# map with tmap
tm_shape(county16) +
  tm_polygons("blkpct16")
tmap_mode("view")

# EDA
ggplot(county16, mapping = aes(x = blkpct16, y = hopct16)) +
  geom_point() +
  geom_smooth()

ggplot(county16) +
  geom_point(mapping = aes(x = blkpct16, y = povrt16))

ggplot(county16, mapping = aes(x = blkpct16, y = hhincome16)) +
  geom_point() +
  geom_smooth()

ggplot(county16, mapping = aes(x = blkpct16, y = hhincome16, size = hopct16, alpha = 0.5)) +
  geom_point()

ggplot(county16) +
  geom_point(mapping = aes(x = blkpct16, y = vacpct16))

ggplot(county16, mapping = aes(x = blkpct16, y = hopct16, size = vacpct16, alpha = 0.5)) +
  geom_point()

# tmap
tm_shape(hart) +
  tm_polygons(c("hopct10", "hopct16"), 
              style=c("pretty", "pretty"),
              palette=list("RdYlGn", "Purples"),
              auto.palette.mapping=FALSE,
              title=c("Homeowner Pct 2010", "Homeowner Pct 2015")) +
  tm_style_grey()

# stupid example, but this is how you put dot density map
tm_shape(hart) +
  tm_bubbles(size=c("hopct10", "hopct16"), title.size="Homeownership") +
  tm_facets(free.scales=FALSE) +
  tm_layout(panel.labels=c("2010", "2015"))

tm_shape(hart) +
  tm_borders() +
  tm_bubbles("hopct10", "blue", border.col = "black", border.lwd=1, 
             size.lim = c(0, 11e7), sizes.legend = c(60, 70, 90, 100), 
             title.size="Metropolitan Population") +
  tm_layout()
