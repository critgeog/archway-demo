
library(tidyverse)
library(tidycensus)
options(tigris_use_cache = TRUE)
options(tigris_class = "sf")
library(ggplot2)
library(multiscales)
library(colorspace)
??multiscales
??bivariate_scale

install.packages("colorspace", repos = "http://R-Forge.R-project.org")
devtools::install_github("clauswilke/multiscales")

View(v16)
#Enter the variables and geographies below
census_title <- c("Median Household Income by County:\n Coefficient of Variation")
census_var <- c("B25077_001E")
census_geog <- c("county")
census_state <- c("fl")
acs_data <- get_acs(geography = census_geog, variables = census_var, state = census_state, 
                    output = "wide", geometry = TRUE)

acs_data
#Make more readable column names
acs_data <- acs_data %>% rename(HV_est = B25077_001E ,
                                HV_moe = B25077_001M)

#Calculate the SE, CV for future reference
acs_data <- acs_data %>% mutate(se = HV_moe/1.645,
                                cv = (se/HV_est)*100)




ggplot(acs_data, aes(fill = zip(HV_est/1000, HV_moe/HV_est))) +
  geom_sf(color = "gray30", size = 0.2) +
  coord_sf(xlim = c(-88, -79.8), ylim = c(24.1, 31.2), datum = NA) +
  bivariate_scale("fill",
                  pal_vsup_carto(palette = "Sunset", rev = TRUE),
                  name = c("median house\nvalues ($1K)", "uncertainty"),
                  limits = list(c(0,400), c(0, 0.4)),
                  breaks = list(waiver(), c(0.05,0.15, 0.25, 0.35)),
                  labels = list(waiver(), scales::percent),
                  guide = "colourbox") +
  theme_void() +
  theme(
    legend.key.size = grid::unit(0.8, "cm"),
    legend.title.align = 0.5,
    legend.justification = c(0, 0),
    legend.position = c(0.1, 0.2)
  )

acs_data
