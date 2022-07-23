#=====================================#
#      technology adoption            #
#         07-22-2022                  #
#=====================================#


#libraries --------
library(tidyverse)
library(countrycode)
library(tidytext)
library(scales)
library(tidymodels)
library(ggrepel)
theme_set(theme_classic())


# Load Dataset --------
technology <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-19/technology.csv')

data <- technology %>%
  rename(category_sector = category,
         group_sector = group,
         variable_name = variable,
         date_year = year) %>%
  select(iso3c, variable_name,
         category_sector, group_sector, date_year, value)


## EDA -------

data_countries_iso3c <- map_data("world") %>%
  tibble() %>%
  mutate(iso3c = countrycode(region,
                             destination = "iso3c",
                             origin = "country.name")) %>%
  select(region, iso3c) %>%
  distinct()


regions<-data %>%
  left_join(data_countries_iso3c, by =c('iso3c')) %>%
  filter(date_year==max(date_year)) %>%
  select(region) %>%
  distinct() %>%
  pull() 
  


data1<-data %>%
  left_join(data_countries_iso3c, by =c('iso3c')) %>%
  filter(region %in%regions) %>%
  ungroup()


top15<-data1 %>%
  group_by(region) %>%
  count(sort = T) %>%
  filter(!is.na(region)) %>%
  head(15) %>%
  select(region) %>%
  pull()

## Final Frame -----

db<-data1 %>%
  filter(region %in% top15)


db %>%
  filter(date_year>1980,
         group_sector %in% c('Production','Consumption','Non-Tech')) %>%
  group_by(region,group_sector, decade = (date_year%/%10)*10) %>%
  summarize(median_val=median(value),
            p25 = quantile(value)[2],
            p75 = quantile(value)[4]) %>%
  ggplot(aes(decade,
             median_val, 
             color=region))+
  geom_vline(xintercept = 2010, 
             color = 'gray', 
             linetype = 'dashed')+
  geom_line()+
  scale_y_continuous(labels = comma)+
  facet_wrap(~group_sector, scales = 'free_y')
  


## Model -----


tidy_adoption <-db %>%
  filter(date_year>1970,
         group_sector!='Creation') %>%
  group_by(region,group_sector, decade = (date_year%/%10)*10) %>%
  summarize(median_val=median(value))


tidy_reg <- tidy_adoption %>%
  nest(median_vals = c(decade, median_val)) %>%
  mutate(model = map(median_vals, ~ lm(median_val ~ decade, data = .x)))


## Explore results --- 
slopes <- tidy_reg %>%
  mutate(coefs = map(model, tidy)) %>%
  unnest(coefs) %>%
  filter(term == "decade") %>%
  mutate(p.value = p.adjust(p.value))

slopes %>%
  ggplot(aes(estimate, p.value, label = region)) +
  geom_vline(
    xintercept = 0, 
    lty = 2,
    size = 1.5, 
    alpha = 0.7, 
    color = "gray50") +
  geom_point(aes(color = group_sector), 
             alpha = 0.8, 
             size = 2.5, 
             show.legend = FALSE) +
  scale_y_log10()+
  facet_wrap(~group_sector) +
  geom_text_repel(size = 3) 

# The futher to the right a country is, the larger 
# tech adoption over this time
