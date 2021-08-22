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
library(corrplot)
library(broom)
library(ggpubr)
library(car)
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



### Experimental Design model ----

olympics %>%
    na.omit()%>%
    select(c(age,height,weight)) %>%
    cor()


#              age     height    weight
# age    1.00000000 0.09594238 0.1617503
# height 0.09594238 1.00000000 0.8018308
# weight 0.16175032 0.80183082 1.0000000
        
olympics %>%
    na.omit()%>%
    select(c(age,height,weight)) %>%
    cor() %>%
    corrplot(method = 'pie',
    title = 'Principal Correlations',
    order = 'hclust') 

fit <- olympics %>%
       na.omit()%>%
       select(c(age,height,weight)) %>%
       lm(weight ~ ., data = .)

fit %>%
    plot()


 

fit <- olympics %>%
       na.omit()%>%
       select(c(age,height,weight,sex)) %>% 
       lm(weight ~ age + height + sex, data = .)

Experimental <- olympics %>%
                na.omit()%>%
                select(c(age,height,weight,sex))

Experimental$predicted <- predict(fit)
Experimental$residuals <- residuals(fit)

quartz()
Experimental %>%
sample_n(150) %>%
    ggplot(aes(age,weight, color = sex)) +
    geom_segment(aes(xend = age,
                 yend = predicted),
                 color = 'gray') +
    geom_point(alpha = .2) +
    geom_point(aes(y = predicted), shape = 1)+
    facet_wrap(~sex, scales = 'free')

# Its seems that 'Male' have more correlation  with the weigth and age

Experimental %>%
    ggplot(aes(height,
    weight, 
    color = sex)) +
    geom_segment(aes(xend = height,
                 yend = predicted),
                 color = 'gray') +
    geom_point(alpha = .2) +
    geom_point(aes(y = predicted), shape = 2, color = 'black')+
    facet_wrap(~sex, scales = 'free')

# The heigth and age (combination) are weak explains 
Experimental %>%
    ggplot(aes(height,
    weight, 
    color = sex)) +
    geom_smooth(method = 'lm',
                se = FALSE,
                color = 'gray')+
    geom_segment(aes(xend = height,
                 yend = predicted),
                 color = 'purple') +
    geom_point() +
    geom_point(aes(y = predicted), shape = 1, color = 'black')+
    facet_wrap(~sex, scales = 'free')


## Residuals problems
set.seed(12345)
olympics %>%
       na.omit() %>% 
       distinct(name,.keep_all = T) %>%
       lm(age ~ height + weight, data = .) %>%
       augment() %>%
       sample_n(150) %>% 
       ggplot(aes(weight,age)) +
           geom_smooth(method = 'lm',
                se = FALSE,
                color = 'gray')+
    geom_segment(aes(xend = weight,
                 yend = .fitted),
                 color = 'red') +
       geom_point() +
       geom_point(aes (x=weight,
                  y = .fitted), shape = 1)

# is possible that problem (predicted age) have heterocesaticy behavior?? 
set.seed(1234)
Female_BH <- olympics %>%
    filter(sex == 'Female') %>%
       na.omit() %>% 
       distinct(name,.keep_all = T) %>%
       lm(age ~ height + weight, data = .) %>%
       augment() %>%
       sample_n(150) %>% 
       ggplot(aes(weight,age)) +
           geom_smooth(method = 'lm',
                se = FALSE,
                color = 'gray')+
    geom_segment(aes(xend = weight,
                 yend = .fitted),
                 color = 'red') +
       geom_point() +
       geom_point(aes (x=weight,
                  y = .fitted), shape = 1)+
    expand_limits(x = 0) +
    labs(title = 'Female predict Model (Age) with adj Values')


set.seed(1234)
Male_Bh<-olympics %>%
    filter(sex != 'Female') %>%
       na.omit() %>% 
       distinct(name,.keep_all = T) %>%
       lm(age ~ height + weight, data = .) %>%
       augment() %>%
       sample_n(150) %>% 
       ggplot(aes(weight,age)) +
           geom_smooth(method = 'lm',
                se = FALSE,
                color = 'gray')+
    geom_segment(aes(xend = weight,
                 yend = .fitted),
                 color = 'red') +
       geom_point() +
       geom_point(aes (x=weight,
                  y = .fitted), shape = 1) +
       expand_limits(x = 0) +
       labs(title = 'Male predict Model (Age) with adj Values')



ggarrange(Female_BH,Male_Bh)

# Yes!!!! the gender generate heterosedasticy, but .....

## A Test ------

model <- olympics %>%
      select(c(age , height , weight)) %>%
       na.omit() %>% 
       distinct(.keep_all = T) %>%
       lm(age ~ height + weight, data = .) 

data <- olympics %>%
      select(c(age , height , weight, sex, sport)) %>%
       na.omit() %>% 
       distinct(.keep_all = T)      

data$residuals <- residuals(model)


lmtest::bptest(model)  # Breusch-Pagan test

#        studentized Breusch-Pagan test

# data:  model
# BP = 399.75, df = 2, p-value < 2.2e-16

car::ncvTest(model) 

# Non-constant Variance Score Test 
# Variance formula: ~ fitted.values 
# Chisquare = 178.0335, Df = 1, p = < 2.22e-16


# the test confirms heteroscedasticity presence!!!

# Last test -----

data %>%
    ggplot(aes(height,residuals)) +
    geom_point(col = 'gray') +
    geom_abline(slope = 0)

var.func <- lm(residuals^2 ~ height, data = data)
summary(var.func)

qchisq(.95, df = 1)


a<-lmtest::bptest(model)

BP = a$statistic %>% .[[1]]

BP > qchisq(.95, df = 1)

# we reject the null hypothesis and conclude that heterocedasticity is present.

## Inferencial Test ------
# The Age of a athletes is independent for sport ??

F_hat <- data %>% 
  specify(age ~ sport) %>%
  calculate(stat = "F")

null_f_distn_theoretical <- data %>%
   specify(age ~ sport) %>%
   hypothesize(null = "independence") %>%
   calculate(stat = "F")



visualize(null_f_distn_theoretical, method = "theoretical") +
shade_p_value(obs_stat = F_hat, direction = "greater")

# reject Null Hypothesis : Age has effects in sport selections