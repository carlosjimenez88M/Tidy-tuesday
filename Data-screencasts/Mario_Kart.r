#==========================#
#         Mario Kart       #
#       Tidytuesday        #
#       2021-05-31         #
#==========================#

#Code for quick exploration and modeling of 
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-05-25/readme.md
# IDE : Visual Studio Code 


## Libraries ------
library(tidyverse)
library(rpart)
library(tidytext)
library(tidymodels)
library(scales)
library(lubridate)
library(lime)
#install.packages('randomForest')
library(iml)

## Load Data ----

records <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/records.csv')
drivers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/drivers.csv')


## EDA ----

records%>%
    head()

# Feature Engineering format for time period
records<-records%>%
    select(-time_period)%>%
    mutate(time=time/60)

records%>%
    #mutate(record_duration=as.integer(record_duration))%>%
    ggplot(aes(time,record_duration,color=factor(record_duration)))+
    geom_point(aes(size=record_duration), alpha=.1)+
    guides(color=FALSE)

# A lot of Correlation between Time and Records !!
# Maybe is possible some correlation with the records type

records%>%
    ggplot(aes(type,record_duration,color=as.factor(type)))+
    geom_jitter(alpha=.1)


# (type record) There is not evidence that explaind records
    
records%>%
    ggplot(aes(type,time,color=as.factor(type)))+
    geom_jitter(alpha=.1)
# YEs!!!!! THe evidence says that : Type of records and time (in mins) hace highlest correlations!!!


records%>%
    ggplot(aes(time,record_duration,color=as.factor(type)))+
    geom_point(alpha=0.1,aes(size=time))+
    geom_smooth()+
    guides(color=FALSE)


## Trends ????

records%>%
    group_by(date=year(date),type)%>%
    summarize(time=sum(time))%>%
    ggplot(aes(date,time,color=factor(type)))+
    geom_line()

# I think that ... maybe this problems is a GAM relations!!


## Build a Model
set.seed(123)
mario_split <- records %>%
  select(shortcut, track, type, date, time) %>%
  mutate_if(is.character, factor) %>%
  initial_split(strata = shortcut)

mario_train <- training(mario_split)
mario_test <- testing(mario_split)

set.seed(234)
mario_folds <- bootstraps(mario_train, strata = shortcut)
tree_spec <- decision_tree(
  cost_complexity = tune(),
  tree_depth = tune()
) %>%
  set_engine("rpart") %>%
  set_mode("classification")

tree_grid <- grid_regular(cost_complexity(), tree_depth(), levels = 7)
mario_wf <- workflow() %>%
  add_model(tree_spec) %>%
  add_formula(shortcut ~ .)

doParallel::registerDoParallel()

tree_res <- tune_grid(
  mario_wf,
  resamples = mario_folds,
  grid = tree_grid,
  control = control_grid(save_pred = TRUE)
)
## Select the model
show_best(tree_res, metric = "accuracy")
# cost_complexity tree_depth .metric  .estimator  mean     n std_err .config    

#1   0.00316                5 accuracy binary     0.736    25 0.00483 Preprocess…
#2   0.0000000001           5 accuracy binary     0.736    25 0.00515 Preprocess…
#3   0.00000000316          5 accuracy binary     0.736    25 0.00515 Preprocess…
#4   0.0000001              5 accuracy binary     0.736    25 0.00515 Preprocess…
#5   0.00000316             5 accuracy binary     0.736    25 0.00515 Preprocess…

autoplot(tree_res)

collect_predictions(tree_res) %>%
  group_by(id) %>%
  roc_curve(shortcut, .pred_No) %>%
  autoplot() +
  theme(legend.position = "none")

choose_tree <- select_best(tree_res, metric = "accuracy")
final_res <- mario_wf %>%
  finalize_workflow(choose_tree) %>%
  last_fit(mario_split)

collect_metrics(final_res)
final_fitted <- final_res$.workflow[[1]]

### LIME

