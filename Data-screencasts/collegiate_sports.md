Collegiate sports
================

``` r
library(tidyverse)
library(tidytext)
library(scales)
theme_set(theme_classic())
```

``` r
sports <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-29/sports.csv')
```

``` r
sports %>%
  group_by(sports) %>%
  count(sort = T)
```

    ## # A tibble: 38 × 2
    ## # Groups:   sports [38]
    ##    sports                         n
    ##    <chr>                      <int>
    ##  1 Basketball                 10000
    ##  2 Volleyball                  9122
    ##  3 Soccer                      8647
    ##  4 Baseball                    8644
    ##  5 Softball                    8560
    ##  6 Golf                        7060
    ##  7 Tennis                      6418
    ##  8 Football                    5310
    ##  9 Track and Field, X-Country  4923
    ## 10 All Track Combined          4870
    ## # … with 28 more rows

``` r
sports<-sports %>%
  filter(!str_detect(sports,'Track and Field'),
         !str_detect(sports,'All Track Combined'))
```

``` r
sports %>%
  select(partic_men,partic_women,sports) %>%
  mutate(partic_men=str_replace_na(partic_men,0),
         partic_women=str_replace_na(partic_women,0),
         partic_men=as.numeric(partic_men),
         partic_women=as.numeric(partic_women)) %>%
  mutate(total_participation=(partic_women+partic_men),
         pct_women=partic_women/total_participation,
         pct_men = 1- pct_women) %>%
  filter(total_participation>0) %>%
  ungroup() %>%
  ggplot(aes(pct_men,pct_women,color=sports))+
  geom_point(aes(size=total_participation), 
             alpha=0.4)+
  scale_y_continuous(labels = percent)+
  scale_x_continuous(labels = percent)+
  labs(title = 'Sport Profile by gender',
       y='Women Sport',
       x='Men Sport',
       color='',
       size='Total participation')+
  theme(plot.title = element_text(hjust = 0.5))
```

![](collegiate_sports_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
sports %>%
  group_by(year,sports) %>%
  summarize(avg_exp=mean(total_exp_menwomen)) %>%
  na.omit()%>%
  ungroup() %>%
  mutate(sports=fct_reorder(sports,avg_exp))%>%
  ggplot(aes(avg_exp,sports, color=sports,fill=sports))+
  geom_boxplot(alpha=0.5, show.legend = FALSE)+
  scale_x_continuous(labels = comma)
```

![](collegiate_sports_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

## Dimensionality reduction + clustering

``` r
library(widyr)
sports %>%
  group_by(year,sports) %>%
  summarize(avg_exp=mean(total_exp_menwomen)) %>%
  na.omit()%>%
  ungroup() %>%
  pairwise_cor(sports,year,avg_exp, sort=TRUE, upper=FALSE) %>%
  filter(item1=='Football') %>%
  mutate(item2=fct_reorder(item2,correlation)) %>%
  ggplot(aes(correlation,item2, fill=correlation>0))+
  geom_col(show.legend = FALSE)+
  scale_x_continuous(labels = percent)+
  labs(title = 'Correlation with Football sport',
       subtitle = 'PCA Iteraction',
       y='')+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
```

![](collegiate_sports_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
top5<-sports %>%
  group_by(sports) %>%
  count(sort = T)%>%
  head()%>%
  select(sports)


sports %>%
  group_by(year,sports) %>%
  summarize(avg_exp=mean(total_exp_menwomen)) %>%
  na.omit()%>%
  ungroup() %>%
  pairwise_cor(sports,year,avg_exp, sort=TRUE, upper=FALSE) %>%
  filter(item1 %in% top5$sports) %>%
  mutate(item2=reorder_within(item2,correlation,item1)) %>%
  ggplot(aes(correlation,item2, fill=correlation>0))+
  geom_col(show.legend = FALSE)+
  scale_y_reordered()+
  facet_wrap(~item1, scales = 'free_y')+
  scale_x_continuous(labels = percent)+
  labs(title = 'Correlation  sport',
       subtitle = 'PCA Iteraction',
       y='')+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
```

![](collegiate_sports_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
sport_clusters<-sports %>%
  group_by(year,sports) %>%
  summarize(avg_exp=mean(total_exp_menwomen)) %>%
  na.omit()%>%
  ungroup() %>%
  widely_svd(sports,year,avg_exp) %>%
  widely_kmeans(sports,dimension,value ,k=4)


sports %>%
  group_by(year,sports) %>%
  summarize(avg_exp=mean(total_exp_menwomen)) %>%
  filter(!is.na(avg_exp))%>%
  group_by(sports) %>%
  summarize(avg_exp=mean(avg_exp))%>%
  inner_join(sport_clusters)%>%
  mutate(sports=fct_reorder(sports,avg_exp)) %>%
  ungroup()%>%
  ggplot(aes(avg_exp,sports, color=cluster))+
  geom_point(aes(size=avg_exp),
             show.legend = FALSE)+
  labs(title = 'Describing sports through clustering')+
  theme(plot.title = element_text(hjust = 0.5))
```

![](collegiate_sports_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
sports %>%
  select(partic_men,partic_women,sports) %>%
  inner_join(sport_clusters)%>%
  mutate(partic_men=str_replace_na(partic_men,0),
         partic_women=str_replace_na(partic_women,0),
         partic_men=as.numeric(partic_men),
         partic_women=as.numeric(partic_women)) %>%
  mutate(total_participation=(partic_women+partic_men),
         pct_women=partic_women/total_participation,
         pct_men = 1- pct_women) %>%
  filter(total_participation>0) %>%
  ungroup() %>%
  mutate(cluster=paste0('Cluster ',cluster)) %>%
  ggplot(aes(pct_men,pct_women,color=cluster))+
  geom_point(aes(size=total_participation), 
             alpha=0.4)+
  scale_y_continuous(labels = percent)+
  scale_x_continuous(labels = percent)+
  facet_wrap(~cluster)+
  labs(title = 'Sport Profile by gender and clusters',
       y='Women Sport',
       x='Men Sport',
       color='',
       size='Total participation')+
  theme(plot.title = element_text(hjust = 0.5))
```

![](collegiate_sports_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
sports %>%
  group_by(year,sports) %>%
  summarize(avg_exp=mean(total_exp_menwomen)) %>%
  na.omit()%>%
  ungroup() %>%
  widely_svd(sports,year,avg_exp) %>%
  filter(dimension>1) %>%
  mutate(dimension = paste0("PC",dimension))%>%
  group_by(dimension) %>%
  top_n(5,abs(value))%>%
  mutate(sports=reorder_within(sports,value,dimension))%>%
  ggplot(aes(value,sports,fill = value >0))+
  geom_col(show.legend = FALSE)+
  scale_y_reordered()+
  facet_wrap(~dimension, scales = 'free_y')+
  labs(x = 'Principal component  value',
       y = 'Sports',
       title = 'What are the source of variation in sports?')+
  theme(plot.title = element_text(hjust = 0.5))
```

![](collegiate_sports_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->