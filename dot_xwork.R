
# https://tarakc02.github.io/dot-density/

v16 <- load_variables(2016, "acs5", cache = TRUE)
v16 %>%
  mutate(table = str_extract(name, "^.+_")) %>%
  filter(str_detect(concept, "EDUCATIONAL ATTAINMENT")) %>%
  select(table, concept) %>% distinct %>% print(n = Inf)

acs <- get_acs("tract", table = "B15003", cache_table = TRUE,
               geometry = TRUE, state = "06", county = "001",
               year = 2016, output = "tidy")
acs

acs <- acs %>%
  mutate(
    id = str_extract(variable, "[0-9]{3}$") %>% as.integer
  ) %>%
  # variable 1 is the "total", which is just the sum of the others
  filter(id > 1) %>%
  mutate(education =case_when(
    id %>% between(2, 16) ~ "No HS diploma",
    id %>% between(17, 21) ~ "HS, no Bachelors",
    id == 22 ~ "Bachelors",
    id > 22 ~ "Post-Bachelors"
  )) %>% 
  group_by(GEOID, education) %>% 
  summarise(estimate = sum(estimate))

acs

acs_split <- acs %>%
  filter(estimate > 50) %>%
  split(.$education)

generate_samples <- function(data)suppressMessages(st_sample(data, size = round(data$estimate / 100)))

points <- map(acs_split, generate_samples)

points <- imap(points,
               ~st_sf(data_frame(education = rep(.y, length(.x))),
                      geometry = .x))

points <- do.call(rbind, points)

points <- points %>% 
  group_by(education) %>% 
  summarise()

points <- points %>%
  mutate(education = factor(
    education,
    levels = c("No HS diploma", "HS, no Bachelors",
               "Bachelors", "Post-Bachelors")))

# view how many points are in each layer
points <- points %>% 
  mutate(n_points = map_int(geometry, nrow))

# setting theme options
theme_set(theme_minimal() +
            theme(panel.grid.major = element_line(size = 0),
                  plot.background = element_rect(fill = "#fdfdfd",
                                                 colour = NA),
                  axis.title = element_blank(),
                  text = element_text(family = "Roboto Condensed"),
                  axis.text = element_blank(),
                  legend.position = "bottom"))


p1 <- ggplot() +
  geom_sf(data = mhi_tract, aes(fill = estimate)) + 
  coord_sf(datum = NA)
  
  
ggplot() + 
  geom_sf(data = points, 
          aes(fill = education),
          size = .1) + 
  scale_color_brewer(type = "div", palette = 4) + 
  scale_fill_brewer(type = "div", palette = 4)

water <- tigris::area_water("06", "001")
towns <- tigris::county_subdivisions("06", county = "001")
alameda_roads <- tigris::roads("06", "001")
ca_county <- tigris::counties(state = "06")
alameda <- ca_county %>% filter(COUNTYFP == "001")

# create town labels by finding the centroid of each town
# ggplot's label functions work better with X/Y dataframes rather 
# than sf objects
town_labels <- towns %>% select(NAME) %>%
  mutate(center = st_centroid(geometry)) %>%
  as.tibble %>%
  mutate(center = map(center, ~st_coordinates(.) %>%
                        as_data_frame)) %>%
  select(NAME, center) %>% unnest()
