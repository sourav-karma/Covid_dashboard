#install.packages("rmarkdown")
#install.packages("flexdashboard")
setwd("F:/R_directory/assignment8")
library(tidyverse)
library(ggthemes) 
library(plotly)
library(sf)
library(leaflet)
library(gganimate)
library(tmap)

covid <- read_delim("F:/R_directory/assignment8/opendata_covid19_test_county_all.csv")
#view(covid)
covid %>% 
  distinct(ResultValue, .keep_all = FALSE)

covid_p = dplyr::filter(covid, ResultValue %in% c("P"))

covid_p %>% 
  distinct(ResultValue, .keep_all = FALSE)

#glimpse(covid_p)

ggplot()+
  geom_line(data = covid_p, aes(x= StatisticsDate, y = DailyCases), size= .25)+
  facet_wrap(vars(County), scale = "free_y")


#removing maakong
covid_no_maakkond <- covid_p %>% 
  mutate(County = gsub(" maakond", "", County))

#covid_no_maakkond
#plotting with 
ggplot()+
  geom_line(data = covid_no_maakkond, aes(x= StatisticsDate, y = DailyCases), size= .25)+
  facet_wrap(vars(County), scale = "free_y")

#filtering NA
covid_test_county_all <- covid_no_maakkond %>% 
  filter(!is.na(County))

#interactive map
gg_cov_cases <- ggplot()+
  geom_line(data = covid_test_county_all, aes(x = StatisticsDate, y = DailyCases), size = .25)+
  facet_wrap(vars(County), scale = "free_y", ncol = 3) 

plotly::ggplotly(gg_cov_cases)



###mapp

covid19_test_county_all_latest <- covid_test_county_all %>% 
  select(CountyEHAK, DailyCases, StatisticsDate) %>% 
  filter(StatisticsDate == max(StatisticsDate))


download.file("https://geoportaal.maaamet.ee/docs/haldus_asustus/maakond_shp.zip", destfile="maakond_shp.zip")
#maakond means county!
unzip("maakond_shp.zip")


# import shp as simple feature:
counties <- st_read("maakond_20211201.shp") 
# data structure:
glimpse(counties)

counties <- counties %>% 
  st_simplify(preserveTopology = TRUE, dTolerance = 200) %>% 
  st_cast("MULTIPOLYGON") # defines the type of geometry after simplification

# structure:
glimpse(counties)

#join
covid19_test_county_all_latest_sf <- left_join(counties, covid19_test_county_all_latest, by = c("MKOOD" = "CountyEHAK"))


covid19_test_county_all_latest_sf_cntr <- covid19_test_county_all_latest_sf %>% 
  st_centroid()

gg_covid19_map <- ggplot()+
  theme_void()+
  geom_sf(data = covid19_test_county_all_latest_sf, aes(fill = DailyCases))+
  geom_sf_text(data = covid19_test_county_all_latest_sf_cntr, aes(label = DailyCases))+
  scale_fill_gradientn(colours = c("forestgreen", "grey70", "orange", "red"))

gg_covid19_map

library(leaflet)

# polygons:
covid19_test_county_all_latest_sf_4326 <- covid19_test_county_all_latest_sf %>% 
  st_transform(4326)

# labels:
covid19_test_county_all_latest_sf_cntr_4326 <- covid19_test_county_all_latest_sf_cntr %>% 
  st_transform(4326)



tmap_mode("view")

tm_shape(covid19_test_county_all_latest_sf_4326)+
  tm_polygons(col = "DailyCases", 
              style = "pretty",
              palette = "Reds",
              alpha = .7)+
tm_shape(covid19_test_county_all_latest_sf_cntr_4326)+
 tm_text(text = "DailyCases",
          bg.color = "grey",
          shadow = T)
###1st one is done
#####last two week difference
#covid_new <- covid_test_county_all %>% 
 # select(CountyEHAK, DailyCases, StatisticsDate) %>% 
  #filter(StatisticsDate == max(StatisticsDate))
# Install the package
#install.packages("lubridate")
# Load the package
library(lubridate)

#glimpse(covid19_test_county_all_latest_sf_cntr_4326)

covid_last_week <-  covid_test_county_all%>%
  select(StatisticsDate, County, CountyEHAK, ResultValue, DailyCases, TotalCases) %>%
  filter(StatisticsDate >= as.Date("2021-12-23") & StatisticsDate <= as.Date("2021-12-29"))

#view(covid_last_week)
glimpse(covid_test_county_all)

#covid19_test_county_all_latest <- covid_test_county_all %>% 
  #select(CountyEHAK, DailyCases, StatisticsDate) %>% 
  #filter(StatisticsDate == max(StatisticsDate))

#library(dplyr)
last_week_sum <- covid_last_week %>% 
  group_by(CountyEHAK) %>% 
  summarise(DailyCases = sum(DailyCases))


#view(last_week_sum)

#sapply(counties, class)
#sapply(covid19_test_county_all_latest, class)


#join
last_week_sum_sf <- left_join(counties, last_week_sum, by = c("MKOOD" = "CountyEHAK"))

last_week_sum_sf_cntr <- last_week_sum_sf %>% 
  st_centroid()



last_week_sum_sf_4326 <- last_week_sum_sf %>% 
  st_transform(4326)

# labels:
last_week_sum_sf_cntr_4326 <- last_week_sum_sf_cntr %>% 
  st_transform(4326)




tmap_mode("view")

b <- tm_shape(last_week_sum_sf_4326)+
  tm_polygons(col = "DailyCases", 
              style = "pretty",
              palette = "Reds",
              alpha = .7)+
  tm_shape(last_week_sum_sf_cntr_4326 )+
  tm_text(text = "DailyCases",
          bg.color = "grey",
          shadow = T)
b
##
gg_covid19_last_week <- ggplot()+
  theme_void()+
  geom_sf(data = last_week_sum_sf, aes(fill = DailyCases))+
  geom_sf_text(data = last_week_sum_sf_cntr, aes(label = DailyCases))+
  scale_fill_gradientn(colours = c("forestgreen", "grey70", "orange", "red"))

gg_covid19_last_week
####week before the last week

covid_week_before_last_week <-  covid_test_county_all%>%
  select(StatisticsDate, County, CountyEHAK, ResultValue, DailyCases, TotalCases) %>%
  filter(StatisticsDate >= as.Date("2021-12-16") & StatisticsDate <= as.Date("2021-12-22"))

#group by ehak code
week_before_last_week_sum <- covid_week_before_last_week %>% 
  group_by(CountyEHAK) %>% 
  summarise(DailyCases = sum(DailyCases))


week_before_last_week_sum_sf <- left_join(counties, week_before_last_week_sum, by = c("MKOOD" = "CountyEHAK"))

week_before_last_week_sum_sf_cntr <- week_before_last_week_sum_sf %>% 
  st_centroid()



week_before_last_week_sum_sf_4326 <- week_before_last_week_sum_sf %>% 
  st_transform(4326)

# labels:
week_before_last_week_sum_sf_cntr_4326 <- week_before_last_week_sum_sf_cntr %>% 
  st_transform(4326)



#mapping the week before last week
tmap_mode("view")

a <- tm_shape(week_before_last_week_sum_sf_4326)+
  tm_polygons(col = "DailyCases", 
              style = "pretty",
              palette = "Reds",
              alpha = .7)+
  tm_shape(week_before_last_week_sum_sf_cntr_4326 )+
  tm_text(text = "DailyCases",
          bg.color = "grey",
          shadow = T)
a
##gg map instead of tmap
gg_covid19_map_week_before_last_week <- ggplot()+
  theme_void()+
  geom_sf(data = week_before_last_week_sum_sf, aes(fill = DailyCases))+
  geom_sf_text(data = week_before_last_week_sum_sf_cntr, aes(label = DailyCases))+
  scale_fill_gradientn(colours = c("forestgreen", "grey70", "orange", "red"))

gg_covid19_map_week_before_last_week


#gg_covid19_last_week + gg_covid19_map_week_before_last_week + plot_layout(ncol = 2, widths = c(1.5,1))


library(ggmap)
library(gridExtra)

grid.arrange(gg_covid19_last_week, gg_covid19_map_week_before_last_week , nrow = 2)



tmap_arrange(a, b, nrow = 2) # worked



##### play with vaccination 

vaccine <- read_delim("F:/R_directory/assignment8/opendata_covid19_vaccination_location_county.csv")
#view(vaccine)
#covid %>% 
  #distinct(ResultValue, .keep_all = FALSE)

fully_vaccinated = dplyr::filter(vaccine, MeasurementType %in% c("FullyVaccinated"))

#covid_p %>% 
  #distinct(ResultValue, .keep_all = FALSE)

glimpse(fully_vaccinated)

#ggplot()+
  #geom_line(data = covid_p, aes(x= StatisticsDate, y = DailyCases), size= .25)+
  #facet_wrap(vars(County), scale = "free_y")


#removing maakong
fully_vaccinated_no_maakkond <- fully_vaccinated %>% 
  mutate(LocationCounty = gsub(" maakond", "", LocationCounty))

#covid_no_maakkond
#plotting with 
#ggplot()+
 # geom_line(data = covid_no_maakkond, aes(x= StatisticsDate, y = DailyCases), size= .25)+
  #facet_wrap(vars(County), scale = "free_y")

#filtering NA
fully_vaccinated_no_maakkond_na <- fully_vaccinated_no_maakkond %>% 
  filter(!is.na(LocationCounty))

#interactive map
gg_fully_vaccinated <- ggplot()+
  geom_line(data = fully_vaccinated_no_maakkond_na, aes(x = StatisticsDate, y = PopulationCoverage), size = .25)+
  facet_wrap(vars(LocationCounty), scale = "free_y", ncol = 3) 

plotly::ggplotly(gg_fully_vaccinated)

#one chart for all
vaccination_coverage <- ggplot(data=fully_vaccinated_no_maakkond_na, aes(x=StatisticsDate, y=PopulationCoverage, group = LocationCounty, colour = LocationCounty))+
  geom_line()
plotly::ggplotly(vaccination_coverage)

###mapping vaccination
#glimpse(fully_vaccinated_no_maakkond_na)

fully_vaccinated_bylast_date <- fully_vaccinated_no_maakkond_na %>% 
  select(LocationCountyEHAK, PopulationCoverage, StatisticsDate) %>% 
  filter(StatisticsDate == max(StatisticsDate))

#join
fully_vaccinated_no_maakkond_na_sf <- left_join(counties, fully_vaccinated_bylast_date, by = c("MKOOD" = "LocationCountyEHAK"))

#glimpse(fully_vaccinated_no_maakkond_na_sf)

fully_vaccinated_no_maakkond_na_sf_cntr <- fully_vaccinated_no_maakkond_na_sf %>% 
  st_centroid()

gg_vaccination_map <- ggplot()+
  theme_void()+
  geom_sf(data = fully_vaccinated_no_maakkond_na_sf, aes(fill = PopulationCoverage))+
  geom_sf_text(data = fully_vaccinated_no_maakkond_na_sf_cntr, aes(label = PopulationCoverage))+
  scale_fill_gradientn(colours = c("forestgreen", "grey70", "orange", "red"))
gg_vaccination_map


#### positive covid by age

positive <- read_delim("F:/R_directory/assignment8/opendata_covid19_test_results.csv")

positive_by_county = dplyr::filter(positive, ResultValue %in% c("P"))

#removing maakong
positive_by_county_no_maakond <- positive_by_county %>% 
  mutate(County = gsub(" maakond", "", County))

#filtering NA
positive_by_county_no_maakond_na <- positive_by_county_no_maakond %>% 
  filter(!is.na(County))

#glimpse(positive_by_county_no_maakond_na)



age_group <-  positive_by_county_no_maakond_na%>%
  select(AgeGroup, StatisticsDate, County, ResultValue) %>%
  filter(StatisticsDate >= as.Date("2021-12-23") & StatisticsDate <= as.Date("2021-12-29"))

#view(covid_last_week)
#glimpse(age_group)

age_group_1 <- age_group %>% 
  mutate(ResultValue = gsub("P", as.numeric(1), ResultValue))

#glimpse(age_group_1)


#convert charecter to integer
age_group_1$ResultValue <- as.integer(as.character(age_group_1$ResultValue))

sapply(age_group_1, class)

#library(dplyr)
age_group_summary <- age_group_1 %>% 
  group_by(AgeGroup,StatisticsDate) %>% 
  summarise(ResultValue = sum(ResultValue))

#view(age_group_summary)


#dfplot <- age_group_summary %>% gather(ResultValue, AgeGroup, -StatisticsDate)
#ggplot(dfplot, mapping = aes(x = StatisticsDate, y = ResultValue, color = AgeGroup) ) + geom_line()

ggplot(data = age_group_summary, aes(x = StatisticsDate , y = AgeGroup, fill = ResultValue)) + geom_col()


#view(last_week_sum)

#join
#try_join_age <- left_join(age_group, age_group_summary, by = c("AgeGroup" = "AgeGroup"))
#glimpse(try_join_age)

#view(try_join_age)

library(ggplot2)
library(hrbrthemes)
library(plotly)

strip <- ggplot(age_group_summary, aes(StatisticsDate, AgeGroup, fill = ResultValue))+ geom_tile()

ggplotly(strip)


#####plotting hospitalised male female on 31st dec

hos <- read.csv("F:/R_directory/assignment8/new.csv")
#view(hos)
#ggplot(data = hos, aes(x =AgeGroup , y = Gender, PatientCount)) + geom_col()

ggplot(hos,aes(AgeGroup,PatientCount,fill=Gender))+
  geom_bar(stat="identity",position="dodge")+
  scale_x_discrete(guide = guide_axis(angle = 90))

#hos_sorted <- arrange(hos, AgeGroup)

#ggplot(hos_sorted,aes(AgeGroup,PatientCount,fill=Gender))+
  #geom_bar(stat="identity",position="dodge")+
  #scale_x_discrete(guide = guide_axis(angle = 90))
