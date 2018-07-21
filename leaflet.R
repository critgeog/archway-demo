# create Census demo for Hart County
# updated: June 25, 2018
# created by Taylor Hafley

library(tidyverse)
library(tidycensus)
options(tigris_use_cache = TRUE)
options(tigris_class = "sf")
library(viridis)
library(sf)
library(devtools)
dev_mode(on=T)
install_github("hadley/ggplot2")
# use dev ggplot2 now
# when finished do:
dev_mode(on=F)  #an
library(ggplot2)

archway <- tibble(county = c("Candler", "Colquitt", "Grady", "Hart", "Pulaski","Spalding",
                             "McDuffie","Washington","Clayton","Glynn","Habersham",
                             "Sumter","Whitfield"),
                  seat = c("Mettler", "Moultrie", "Cairo", "Hartwell", "Hawkinsville",
                           "Griffin","Thomson", "Sandersville", "Jonesboro", "Brunswick",
                           "Clarkseville", "Americus", "Dalton"),
                  active = factor(c("Y","Y","Y","Y","Y","Y",
                                    "Y","Y","N","N","N",
                                    "N","N")))



hart <- get_acs(state = "GA", county = "Hart", geography = "tract", 
                variables = "B19013_001", geometry = TRUE)

hart %>%
  ggplot(aes(fill = estimate, color = estimate)) + 
  geom_sf() + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis(option = "cividis") + 
  scale_color_viridis(option = "cividis")

?viridis

archway_demo <- get_acs(geography = "tract", 
                    variables = c("B19013_001E",'B25001_001E','B25002_002E',
                                  'B25002_003E','B25003_001E','B25003_002E',
                                  'B25003_003E',  'B02001_001E',
                                  'B02001_002E',
                                  'B02001_003E',
                                  'B17001_001E',
                                  'B17001_002E'),
                    county = archway$county,
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
         povrt16 = round(100 * (ipov16/tpov16),1)) %>%
  st_transform(4326)

# create leaflet map for all Archway counties, multiple variables 

bins <- c(0, 30000, 40000, 50000, 60000, 70000, 80000, 250000)
pala_inc <- colorBin("plasma", hart1$B19013_001E, bins = bins)

bins_pov <- c(0, 10, 15, 20, 30, 40, 80)
pala_pov <- colorBin("Greens", hart1$pct_pov, bins = bins_pov)
pala_pov2 <- colorBin("Greens", archway_demo$povrt16, bins = bins_pov)

bins_own <- c(0, 40, 50, 60, 70, 80)
pala_own <- colorBin("PuOr", archway_demo$hopct16, bins = bins_own)

bins_occ <- c(0, 50, 80, 85, 90, 100)
pala_occ <- colorBin("BuGn", archway_demo$occpct16, bins = bins_occ)
fivenum(archway_demo$whtpct16)

#pala_vac <- colorBin("Greens", archway_demo$vacpct16, bins = bins_pov)

bins_blk <- c(0, 10, 35, 60, 80, 100)
pala_blk <- colorBin("YlOrRd", archway_demo$blkpct16, bins = bins_blk)

bins_wht <- c(0, 20, 45, 60, 85, 100)
pala_wht <- colorBin("BrBG", archway_demo$whtpct16, bins = bins_wht)


leaflet_archway <- leaflet() %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark") %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Carto") %>%
  addPolygons(data = archway_demo, stroke = FALSE, smoothFactor = 0.2, 
              color = ~pala_inc(hhincome16), 
              label = ~as.character(hhincome16), 
              fillOpacity = .7, 
              group = "hhincome") %>%
  addPolygons(data = archway_demo, stroke = FALSE, smoothFactor = 0.2,
              color = ~pala_pov2(povrt16), 
              label = ~as.character(povrt16),
              fillOpacity = .7, 
              group = "poverty") %>%
  addPolygons(data = archway_demo, stroke = FALSE, smoothFactor = 0.2,
              color = ~pala_own(hopct16), 
              label = ~as.character(hopct16),
              fillOpacity = .7, 
              group = "owner") %>%
  addPolygons(data = archway_demo, stroke = FALSE, smoothFactor = 0.2,
              color = ~pala_occ(occpct16), 
              label = ~as.character(occpct16),
              fillOpacity = .7, 
              group = "occupied") %>%
  addPolygons(data = archway_demo, stroke = FALSE, smoothFactor = 0.2,
              color = ~pala_blk(blkpct16), 
              label = ~as.character(blkpct16),
              fillOpacity = .85, 
              group = "black") %>%
  addPolygons(data = archway_demo, stroke = FALSE, smoothFactor = 0.2,
              color = ~pala_wht(whtpct16), 
              label = ~as.character(whtpct16),
              fillOpacity = .7, 
              group = "white") %>%
  addLegend(pal = pala_inc, values = archway_demo$hhincome16, 
            title = "Med. HH Income ($)",
            group = "hhincome") %>%
  addLegend(pal = pala_pov2, values = archway_demo$povrt16,
            title = "Poverty Rate",
            group = 'poverty') %>%
  addLegend(pal = pala_own, values = archway_demo$hopct16,
            title = "Homeownership Rate",
            group = 'owner') %>%
  addLegend(pal = pala_occ, values = archway_demo$occpct16, 
            title = "Occupancy Rate",
            group = "occupied") %>%
  addLegend(pal = pala_blk, values = archway_demo$blkpct16, 
            title = "Percent Black",
            group = "black") %>%
  addLegend(pal = pala_wht, values = archway_demo$whtpct16, 
            title = "Percent white",
            group = "white") %>%
# Layers control
addLayersControl(
  baseGroups = c("Dark","OSM (default)","Carto"),
  overlayGroups = c("hhincome", "poverty","owner", "occupied","black","white"),
  position = 'topleft',
  options = layersControlOptions(collapsed = FALSE)
)%>%
  hideGroup(c("poverty","owner","occupied","black","white"))


leaflet_archway







# interactive leaflet map.

# create leaflet map for Hart County, multiple variables to select on and off in 
hart1 <- get_acs(geography = "tract", 
                 variables = c("B19013_001",'B17001_001E','B17001_002E'),
                 state = "GA",
                 county = archway$county,
                 output = "wide",
                 geometry = TRUE) %>%
  mutate(pct_pov = B17001_002E/B17001_001E) %>%
  st_transform(4326)



# import 2000 race data using tidycensus
gaBG <- get_decennial(geography = "block group", variables = dec_vars00sf3, 
                      state = "GA", county = cnty, year = 2000,
                      output = 'wide',
                      geometry = TRUE) %>%
  mutate(SqKM_BG = as.numeric(st_area(geometry)) / 1e6) %>%
  mutate(ahpi = asian + hawaiian,
         oth2 = other + twomore)

#hart2 <- get_acs(geography = "tract", 
#               variables = c(hhincome = "B19013_001"), 
#               state = "GA", 
#               geometry = TRUE) %>%
#  st_transform(4326)
