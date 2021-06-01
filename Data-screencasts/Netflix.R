#==========================#
#         Netflix          #
#       Tidytuesday        #
#       2021-05-31         #
#==========================#

#Code for quick exploration and modeling of 
#https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-04-20/readme.md
# IDE : Visual Studio Code 

# Libraries -------
library(tidyverse)
library(tidytext)
library(stringr)
library(scales)
library(widyr)
library(plotly)
library(snakecase)
library(tidylo)
library(ggrepel)
library(janitor)

## Load Dataset -----
netflix_titles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-20/netflix_titles.csv')


## Exploratory Data Analysis --------

netflix_titles%>%
  count(type, sort = T)%>%
  mutate(perc=round(n/sum(n),2)*100)

# Percentaje Movies =70
# Percentaje Tv Shows =30
# Non balanced Data

netflix_titles%>%
  filter(!is.na(director))%>%
  count(director,type,sort = T)%>%
  top_n(10)%>%
  ungroup()%>%
  mutate(director=fct_reorder(director,n))%>%
  ggplot(aes(n,director))+
  geom_col()+
  labs(title = 'Relation Director with the movies Projects',
       y='',
       x='# of Projects')

netflix_titles%>%
  ggplot(aes(release_year))+
  geom_histogram()

#With the histogram we can explan some colaborative filters in the years for create some
# Heuristiscts for this EDA

netflix_titles%>%
  filter(release_year>=1980)%>%
  ggplot(aes(release_year))+
  geom_histogram()

# Not is a good idea


## Other alternative for visualizations
netflix_titles%>%
  ggplot(aes(release_year, fill=type))+
  geom_histogram(binwidth = 5)

netflix_titles%>%
  count(decade=10*(release_year %/%10),type)%>%
  ggplot(aes(decade,n,color=type))+
  geom_line()+
  geom_point()+
  geom_vline(xintercept = 2000,linetype='dashed', col='gray')+
  geom_vline(xintercept = 2020,linetype='dashed', col='gray')+
  labs(title = 'Relation Between Movies and TV Programs',
       subtitle = 'Per Decade',
       y='Total')

# Between 2000 and 2020 There is a differentiating behavior between the genres until they converge in a new dynamic
  

ggplotly(netflix_titles%>%
           filter(!is.na(country))%>%
           separate(country,c('primary_country','others'),sep = ',',extra = 'merge')%>%
           group_by(type)%>%
           count(primary_country,decade=10*(release_year %/%10))%>%
           ggplot(aes(decade,n,color=primary_country))+
           geom_line(show.legend = F)+
           facet_wrap(~type, ncol=1, scales = 'free_y')+
           labs(title = 'Countries vs TV Shows/Movies',
                subtitle = 'Time Series',
                y=' ')+
           theme(legend.position  ='none')
)
  
## Tidytext format for Visualization ------

Total_genres<-netflix_titles%>%
  separate(country,c('primary_country','others'),sep = ',',extra = 'merge')%>%
  filter(!is.na(primary_country))%>%
  separate_rows(listed_in,sep = ', ')%>%
  mutate(primary_country=factor(primary_country))%>%
  group_by(primary_country,listed_in)%>%
  summarize(Total=sum(n()))%>%#most efficent format
  ungroup()
  


top10<-Total_genres%>%
  count(primary_country,sort = T)%>%
  top_n(10)


Total_genres%>%
  filter(primary_country %in% top10$primary_country)%>%
  #filter(primary_country=='Argentina')%>%
  group_by(primary_country)%>%
  top_n(10,wt = Total)%>%
  ungroup()%>%
  mutate(listed_in=reorder_within(listed_in,Total,primary_country))%>%
  ggplot(aes(Total,listed_in,fill=primary_country))+
  geom_col(show.legend = F)+
  scale_y_reordered()+
  facet_wrap(~primary_country,scales = 'free')+
  labs(title = 'Top 10 Genres per Countries',
       y='')

# In this Image We can find soime clusters by countries.


## Clusters by Area ----
Total_genres_decade<-netflix_titles%>%
  separate(country,c('primary_country','others'),sep = ',',extra = 'merge')%>%
  filter(!is.na(primary_country))%>%
  separate_rows(listed_in,sep = ', ')%>%
  mutate(primary_country=factor(primary_country))%>%
  group_by(primary_country,listed_in,decade=10*(release_year %/%10))%>%
  summarize(Total=sum(n()))%>%#most efficent format
  ungroup()

Total_genres_decade%>%
  filter(!is.na(decade),
         primary_country %in% top10$primary_country,
         decade>=1990)%>%
  group_by(primary_country,decade)%>%
  top_n(10)%>%
  ggplot(aes(decade,Total,fill=listed_in))+
  geom_area()+
  facet_wrap(~primary_country, scales = 'free')

# I love this Graph!!!

netflix_titles%>%
  filter(!is.na(country))%>%
  count(country=fct_lump(country,10),
        type,
        sort=TRUE)%>%
  mutate(country=fct_reorder(country,n))%>%
  ggplot(aes(n,country,fill=type))+
  geom_col()


## Basic NLP ----



words_by_genres<-netflix_titles%>%
  separate_rows(listed_in,sep = ', |& ')%>%
  mutate(listed_in=str_trim(listed_in))%>%
  unnest_tokens(word,description)%>%
  anti_join(stop_words, by='word')%>%
  count(listed_in,word,sort=TRUE)%>%
  ungroup()

total_words<-netflix_titles%>%
  separate_rows(listed_in,sep = ', |& ')%>%
  mutate(listed_in=str_trim(listed_in))%>%
  unnest_tokens(word,description)%>%
  anti_join(stop_words, by='word')%>%
  count(word,sort=TRUE)%>%
  ungroup()

total_relation<-words_by_genres%>%
  rename(total_word_genre=n)%>%
  left_join(total_words%>%
              rename(total_word=n),by='word')

## create tf_idf model with bayes

genres_bind_odds<-total_relation%>%
  bind_log_odds(listed_in,word,total_word_genre)%>%
  filter(total_word_genre>10)%>%
  arrange(-log_odds_weighted)


genres_bind_odds%>%
  group_by(listed_in)%>%
  top_n(10)%>%
  ungroup()%>%
  mutate(word=reorder_within(word,log_odds_weighted,listed_in))%>%
  ggplot(aes(log_odds_weighted,word,fill=listed_in))+
  geom_col(show.legend = F)+
  scale_y_reordered()+
  facet_wrap(~listed_in,scales = 'free_y')

## Interesting ..... Maybe we can make zoom to International Movies comparative with Dramas 

genres_bind_odds%>%
  count(listed_in, sort=T)%>%
  print(n=40)
  
genres_bind_odds%>%
  filter(listed_in %in% c('Dramas','International Movies'))%>%
  mutate(log_odds_weighted=abs(log_odds_weighted))%>%
  spread(listed_in,log_odds_weighted,fill=0)%>%
  filter(Dramas!=0,
         `International Movies`!=0)%>%
  ggplot(aes(Dramas,`International Movies`))+
  geom_abline(color = "red") +
  geom_point()+
  geom_text(aes(label=word),vjust=1,hjust=1)+
  scale_x_log10()+
  scale_y_log10()
  
