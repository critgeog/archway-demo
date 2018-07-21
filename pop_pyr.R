library(tidycensus)
library(tidyverse)
library(tigris)
library(sf)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)
library(stringr)
# census_api_key("YOUR KEY HERE")
library(geofacet)
library(extrafont)

rent <- get_acs(geography = "tract", variables = "DP04_0134", 
                state = c("GA"), county = archway$county, geometry = TRUE)
rent

cnty <- counties(cb = TRUE, state = 'GA') %>%
  filter(NAME %in% archway$county) %>%
  select(county_name = NAME)

cnty_rent <- st_join(rent, cnty, join = st_within, 
                   left = FALSE) 

ggplot(cnty_rent, aes(x = estimate)) + 
  geom_histogram() + 
  facet_wrap(~county_name)

library(viridis)

ggplot(cnty_rent, aes(fill = estimate, color = estimate)) + 
  geom_sf() + 
  facet_wrap(~county_name, nrow = 3) + 
  theme_minimal() + 
  theme(aspect.ratio = 1) + 
  scale_fill_viridis() + 
  scale_color_viridis()
?coord_sf

age <- get_decennial(geography = "county", state = "GA", county = archway$county,
                     table = "P012", summary_var = "P0010001") %>%
  mutate(variable = str_replace(variable, "P01200", "")) %>%
  filter(!variable %in% c("01", "02", "26")) %>%
  arrange(NAME, variable)

head(age)

agegroups <- c("0-4", "5-9", "10-14", "15-19", "15-19", "20-24", "20-24", 
               "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", 
               "55-59", "60-64", "60-64", "65-69", "65-69", "70-74", "75-79", 
               "80-84", "85+")

agesex <- c(paste("Male", agegroups), 
            paste("Female", agegroups))

age$group <- rep(agesex, length(unique(age$NAME)))

age2 <- age %>%
  group_by(NAME, group) %>%
  mutate(group_est = sum(value)) %>%
  distinct(NAME, group, .keep_all = TRUE) %>%
  ungroup() %>%
  mutate(percent = 100 * (group_est / summary_value)) %>%
  select(name = NAME, group, percent) %>%
  separate(group, into = c("sex", "age"), sep = " ") %>%
  mutate(age = factor(age, levels = unique(age)), 
         percent = ifelse(sex == "Female", percent, -percent)) 

head(age2)

xlabs = c("0-4" = "0-4", "5-9" = "", "10-14" = "", "15-19" = "", "20-24" = "", 
          "25-29" = "", "30-34" = "", "35-39" = "", "40-44" = "", "45-49" = "", 
          "50-54" = "", "55-59" = "", "60-64" = "", "65-69" = "", "70-74" = "", 
          "75-79" = "", "80-84" = "", "85+" = "85+")

ggplot(data = age2, aes(x = age, y = percent, fill = sex)) +
  geom_bar(stat = "identity", width = 1) + 
  scale_y_continuous(breaks=c(-5, 0, 5),labels=c("5%", "0%", "5%")) + 
  coord_flip() + 
  theme_minimal(base_family = "Tahoma") + 
  scale_x_discrete(labels = xlabs) + 
  scale_fill_manual(values = c("red", "navy")) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        strip.text.x = element_text(size = 6)) + 
  labs(x = "", y = "", fill = "", 
       title = "Population Pyramids of Archway counties", 
       caption = "Data source: 2010 US Census, tidycensus R package.  Chart by Taylor Hafley.") +
  facet_wrap(~name)











