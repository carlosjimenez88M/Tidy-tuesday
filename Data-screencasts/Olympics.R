#================================#
#         Tidytuesday #31        #
#          The Olympics          #
#           2021-08-20          #
#================================#


## libraries ---------------
library(tidyverse)
library(tidytext)
library(infer)
library(scales)
theme_set(theme_bw())

## Data ---------

olympics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv')

## EDA -----------

olympics %>%
    mutate(sex = ifelse(sex == 'M','Male','Female')) %>%
    group_by(sex) %>%
    count(name= 'Total', sort = T)  %>%
    ungroup() %>%
    mutate(pct = (Total/sum(Total)* 100)) 


#  sex     Total   pct
#  <chr>   <int> <dbl>
#1 Male   196594  72.5
#2 Female  74522  27.5



# Some little Feature Engineering
olympics<- olympics %>%
    mutate(sex = ifelse(sex == 'M','Male','Female')) 

olympics %>%
    group_by(sex,sport) %>%
    count(sort = T, name = 'Total') %>%
    pivot_wider(names_from = sex, values_from = Total) %>%
    mutate(diff = Male - Female,
           Total = Male + Female) %>%
    ungroup() %>%
    mutate(sport = fct_reorder(sport,Total, sum)) %>%
    ungroup() %>%
    top_n(30, Total) %>% 
    ggplot(aes(y = sport, x = Total)) +
    geom_segment(aes(x = Female,
                    xend = Male,
                    y = sport,
                    yend = sport),
                    linetype = 'twodash',
                    color = '#787474')+
    geom_point(aes(Female, size = Female),
                  shape = 20,
                  fill = 'white', 
                  stroke = 1,
                  color = 'orange', 
                  show.legend = FALSE)+
    geom_point(aes(Male, size = Male),
                  shape = 20,
                  fill = 'white', 
                  stroke = 1,
                  color = '#48705c',
                  show.legend = FALSE) +
    annotate(geom = 'text',
            x = 22000,
            y = 5 ,
            label = 'Female',
            color = 'Orange') +
    annotate(geom = 'text',
            x = 21500,
            y = 6,
            label = 'Male',
            color = '#48705c') + 
    labs(title = 'Differences between sport by gender: Top 30',
        y = '',
        x = '# of Athletes')

# Volleyball is the sport with more gender equality 

avg_gender_age<-olympics %>%
    filter(!is.na(sex),
           !is.na(age)) %>%
    group_by(sex) %>%
    summarize(avg_age = mean(age))

olympics %>%
    left_join(avg_gender_age, by = 'sex') %>%
    ggplot(aes(age,fill = sex)) +
    geom_vline(aes(xintercept = avg_age))+
    geom_histogram(alpha = .4) +
    facet_wrap(~sex, scales = 'free_y') +
    scale_x_continuous(breaks = seq(from =0, 
                                    to =100, 
                                    by = 5)) +
    labs(title = 'Age Distribution by Gender')

# Woman are more younger than men



olympics %>%
    filter(!is.na(medal)) %>%
    group_by(sex,sport,medal) %>%
    count(sort = T) %>%
    ungroup() %>%
    mutate(pct = n/sum(n)) %>%
    filter(pct >= .003) %>%
    mutate(sex = factor(sex),
          sport = reorder_within(sport,pct,sex)) %>%
    ggplot(aes(pct,sport,fill = medal)) +
    geom_col(alpha = .7) +
    scale_y_reordered() +
    geom_text(aes(label = paste0(round(pct*100,1),'%')), 
    position = position_stack(vjust = .5), size =2)+
    facet_wrap(~sex, scales = 'free')

## Well is necesary chane this graph

olympics %>%
    filter(!is.na(medal)) %>%
    group_by(sex,sport,medal) %>%
    count(sort = T) %>%
    group_by(sex) %>%
    mutate(pct = n/sum(n)) %>%
    filter(pct >= 0.01) %>%
    mutate(sport = reorder_within(sport,pct,sex)) %>%
    ggplot(aes(medal,sport,fill = pct))+
    geom_tile()+
    geom_text(aes(label = paste0(round(pct*100,1),'%')))+
    scale_fill_gradient(high = 'red',low = 'pink',labels = percent)+
    facet_wrap(~sex, scales = 'free')+
    scale_y_reordered()



### Infer model ----

olympics %>%
    filter(!is.na(medal),
        !is.na(sex),
        !is.na(age),
        sport == 'Swimming')  %>%
        specify(medal ~ sex ) %>%
        hypothesize(null = 'independence') %>%
        generate(reps = 5, type = "permute") %>%
        calculate("diff in means",order = c('Female','Male'))
        
