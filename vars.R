
library(tidyverse)
library(tidycensus)
#library(tigris)
options(tigris_use_cache = TRUE)
library(sf)
library(ggplot2)

# used Jerry's github tutorial as a reference
# https://github.com/jshannon75/awp_tidycensus/blob/master/ACS_walkthrough.md

v16 <- load_variables(2016, "acs5", cache = TRUE)
#View(v16)


# https://raw.githubusercontent.com/jshannon75/awp_tidycensus/master/ACStracts_2016_metadata.csv

metadata<- read_csv('https://raw.githubusercontent.com/jshannon75/awp_tidycensus/master/ACStracts_2016_metadata.csv')

metadata_head<-metadata[1:10,]
knitr::kable(metadata_head)

acs_vars<-metadata$variable
df <- get_acs(geography = "tract", state="GA",
              variable = acs_vars, geometry=TRUE)

df_geom<-df %>%
  select(GEOID,NAME,geometry)
df_geom<-unique(df_geom)

#Use the code below if working with the downloaded data through tidycensus
df_data<-df
st_geometry(df_data)<-NULL

df_data<-df_data %>%
  left_join(metadata) %>%
  group_by(GEOID,var_group) %>%
  summarise(
    est=sum(estimate),
    moe=round(sqrt(sum(moe^2)),0)
  )

df_data_head<-df_data[1:10,]
knitr::kable(df_data_head)

metadata
metadata_group<-metadata %>%
  select(var_group,var_normalize)

census_data<-df_data %>%
  left_join(metadata_group) 

census_data<-unique(census_data)

census_data_head<-census_data[1:10,]
knitr::kable(census_data_head)


#Select just the normalizing variables 
normal_vars<-census_data %>%
  filter(var_normalize=="99999") %>%
  select(GEOID,var_group,est) %>%
  rename("var_normalize"=var_group,
         "normal_est"=est)

#Join normalizing variables back and create a percentage
census_data_pct <- census_data %>%
  filter(var_normalize!="99999") %>%
  left_join(normal_vars) %>%
  filter(normal_est>0) %>% #Get rid of tracts that have 0 for population
  mutate(est_pct=round(est/normal_est*100,2),
         moe_pct=round(moe/normal_est*100,2)) %>%
  select(-var_normalize,-normal_est)

census_data_pct_head<-census_data_pct[1:10,]
knitr::kable(census_data_pct_head)


census_data_pct_only<-census_data_pct %>%
  select(GEOID,var_group,est_pct,moe_pct) %>%
  mutate(var_group=paste(var_group,"_p",sep="")) %>% #Add "_p" on the end of the variable name
  rename("est"=est_pct,
         "moe"=moe_pct)

census_data_est_only<-census_data_pct %>%
  select(GEOID,var_group,est,moe)

census_data_all<-rbind(census_data_est_only,census_data_pct_only) %>%
  rename("var"=var_group) %>%
  gather(est:moe,key="var_type",value="value") 

census_data_all_head<-census_data_all[1:10,]
knitr::kable(census_data_all_head)

census_data_all_wide<-census_data_all %>%
  unite("var_c","var":"var_type") %>%
  spread(var_c,value)

census_data_all_wide_head<-census_data_all_wide[1:10,1:7]
knitr::kable(census_data_all_wide_head)

census_data_filter<-census_data_all %>%
  filter(var=="Rent30_p" & var_type=="est") 

census_data_filter_sf<-left_join(df_geom,census_data_filter)

library(tmap)
tm_shape(census_data_filter_sf) +
  tm_polygons("value",style="quantile",border.alpha=0)+
  tm_legend(outside=TRUE)


# this is ~ 2 hours of work. finish it in your next sitting.

# need to do: be able to sort by county
# need to join archway communities by county, 
# be able to filter by (df_arch %>% filter(archway = 'Y', county = 'Hart')

# check for the variables needed that Angel requested.
# identify those variables in a df. 
# do any wrangling necessary to have that date prepped
# make some visualizations to test data wrangling
# 