
#### Libraries
library(tidymodels)
library(tidyverse)
library(baguette)
tidymodels_prefer()

# Load the data 
train <- read_csv("train.csv")


#### Preprocessing

## Recipe with original features
recipe_original <- recipe(Listening_Time_minutes ~ .,data = train) %>%
  
  # Remove id for the modeling process
  add_role(id,new_role = "id") %>%
  
  # Impute missing data with bag trees
  step_impute_bag(all_predictors()) %>%
  
  # Remove Near zero variance features
  step_nzv(all_predictors()) %>%
  
  # Transform all numeric features with Yeo-Jonhson
  step_YeoJohnson(all_numeric_predictors()) %>%
  
  # Scale all numeric features
  step_scale(all_numeric_predictors()) %>%
  
  # Center all numeic features
  step_center(all_numeric_predictors()) %>%
  
  # Dummy encode all categorical featrues
  step_dummy(all_nominal_predictors())

## Recipe with engineered features
recipe_engineered <- recipe(Listening_Time_minutes ~ .,data = train) %>%
  
  # Remove id for the modeling process
  add_role(id,new_role = "id") %>%
  
  # Impute missing data with bag trees
  step_impute_bag(all_predictors()) %>%
  
  # Add new features
  step_mutate(
    
    ## Features from Episode_Title
    Episode_Stage = case_when(
      Episode_Title <= 10 ~"New",
      Episode_Title <= 50 ~"Established",
      TRUE ~ "Legacy"),
    
    # Special Numbers
    Is_Milestone = case_when(
      Episode_Title%% 25 == 0 ~ "Special", # Episodes like 25, 50, 75...
      Episode_Title%% 50 == 0 ~ "Special", # Episodes like 50, 100, 150...
      Episode_Title%% 10 == 0 & Episode_Title <= 40 ~ "Special",
      TRUE ~ "Regular"),
    
    # Combined Popularity
    Total_Popularity = Host_Popularity_percentage + Guest_Popularity_percentage,
    
    ## Podcast_Quality
    
    # Convert Sentiment into numeric
    Sentiment_score = case_when(
      Episode_Sentiment == "Positive"~ 100,
      Episode_Sentiment == "Neutral" ~ 50,
      Episode_Sentiment == "Negative" ~ 0),
    
    # Make a Podcast Quality features as a weighted sum of popularity and sentiment
    Podcast_Quality = 
      0.3 * Host_Popularity_percentage +
      0.3 * Guest_Popularity_percentage +
      0.4 * Sentiment_score,
    
    # Podcast Quality Binned
    Podcast_Quality_Group = case_when(
      Podcast_Quality >= 80 ~ "Good_Episode",
      Podcast_Quality >= 50 ~ "Mid_Episode",
      TRUE ~ "Bad_Episode"),
      
      # Add Density 
      add_density = Episode_Length_minutes / (Number_of_Ads + 1),
    
    # Genre Grouping 
    Meta_genre = case_when(
      Genre %in% c("Education", "Health", "Lifestyle", "Technology", "True Crime")~"Informative",
      Genre %in% c("Comedy", "Music","Sports") ~ "Entertainment",
      Genre %in% c("Business", "News") ~ "Serious"),
    
    # Publication Day Group 
    Publication_Day_Group = case_when(
      Publication_Day %in% c("Tuesday", "Wednesday") ~ "Prime_Tu_W",
      Publication_Day %in% c("Sunday", "Monday", "Saturday") ~ "Steady_S_M_S",
      Publication_Day %in% c("Thursday", "Friday") ~ "Low_Th_F"),
    
    # Publication Time Group
    Publication_Time_Group = case_when(
      Publication_Time == "Night" ~ "Night",
      Publication_Time %in% c("Morning", "Evening") ~ "Middle_Day",
      Publication_Time == "Afternoon" ~ "Afternoon")
    ) %>%
  
  # Remove the original Variables
  step_rm(
    c("Episode_Length_minutes","Publication_Day","Number_of_Ads","Genre","Publication_Time",
      "Episode_Sentiment","Episode_Title","Host_Popularity_percentage","Guest_Popularity_percentage",
      "Podcast_Quality"))%>%
  
  # Remove Near zero variance features
  step_nzv(all_predictors()) %>%
  
  # Transform all numeric features with Yeo-Jonhson
  step_YeoJohnson(all_numeric_predictors()) %>%
  
  # Scale all numeric features
  step_scale(all_numeric_predictors()) %>%
  
  # Center all numeic features
  step_center(all_numeric_predictors()) %>%
  
  # Dummy encode all categorical featrues
  step_dummy(all_nominal_predictors())

#### Model Selection ####

## Null model 
null_model <- null_model()%>%
  set_mode(mode = "regression")%>%
  set_engine("parsnip")

## XGB 
xgb_model <- boost_tree(
  mtry = tune(),
  trees = tune(),
  min_n = tune(),
  sample_size = tune(),
  learn_rate = tune(),
  tree_depth = tune())%>%
  set_mode("regression")%>%
  set_engine("xgboost")

## Multilayer perceptron nnet
mlp_model <- mlp(
  hidden_units = tune(),
  penalty = tune(),
  epochs = tune(),
  learn_rate = tune())%>%
  set_mode("regression")%>%
  set_engine("nnet")

## Bagged MARS
mars_model <- bag_mars(
  num_terms = tune(),
  prod_degree = tune())%>%
  set_mode("regression")%>%
  set_engine("earth")

## Set a Workflow set for light-tuning the models
light_tuning_set <- workflow_set(
  preproc = list(engineered = recipe_engineered,original = recipe_original),
  models = list(bag_mars = mars_model,xgb = xgb_model,mlp = mlp_model,null = null_model),
  cross = TRUE)

# Take a sample 10% for faster tunning
light_tune_sample <- train %>% slice_sample(prop = 0.10)
small_cv <- vfold_cv(v = 5,strata = Listening_Time_minutes,data = light_tune_sample)

## Tune the light tunning set via tune_race_anova 
tune_race_anova <- workflow_map(
  object = light_tuning_set,
  fn = "tune_race_anova",
  resamples = small_cv,
  seed = 123,
  verbose = TRUE)
  
  
  
