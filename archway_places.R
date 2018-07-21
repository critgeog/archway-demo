
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

library(tigris)
us_states <- states()
plot(us_states)
rd <- primary_roads("GA", "Clarke")
ga_cities <- places("GA",county = "Hartwell")

rd <- primary_roads()
?places

hart <- get_acs(state = "GA", county = "Hart", geography = "county", 
                variables = "B19013_001", geometry = TRUE)

hartwell = ga_cities %>%
    filter(NAME == 'Hartwell')


hartwell <- hartwell %>%
  st_transform(32616)
hart <- hart %>%
  st_transform(32616)

tm_shape(hart) +
  tm_polygons() +
  tm_shape(hartwell) +
  tm_borders(col = 'red') +
  tm_shape(rd) +
  tm_lines(col = 'black')





huc10 <- 
  tm_shape(bg_class10_huc10) +
  tm_fill('class10', legend.show = FALSE, palette = race_mm_col3)+
  tm_add_legend(type = c("fill"), labels = lbl3, col = leg_col3, 
                title = "2010 seg & diversity\nby HUC10 Watershed") +
  tm_shape(rd) + 
  tm_lines(col = "black") +
  tm_compass(type = "arrow", size = 4, position = c(0.82, 0.08)) +
  tm_scale_bar(breaks = c(0,20), size = 1.1, position= c(0.8, 0.0)) +
  tm_legend(position = c(0.025, 0.05),
            bg.color = "white",
            frame = TRUE,
            legend.text.size = .9,
            legend.title.size = 1.1) + 
  tm_layout(frame = FALSE, 
            outer.margins=c(0,0,0,0), 
            inner.margins=c(0,0,0,0), asp=0)
huc10
