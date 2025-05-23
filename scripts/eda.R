
#### Libraries ####
library(tidyverse)
library(corrplot)
library(patchwork)
library(ggmosaic)
library(ggridges)

# Load the data 
train <- read.csv("Data/train.csv",stringsAsFactors = TRUE)

# Remove id 
eda <- train %>% 
  na.omit()%>%
  select(-id) %>%
  mutate(
    Episode_Title = as.numeric(substr(x = Episode_Title,start = 8,stop = 12))
  )

## Add New features ##
eda <- eda %>%
  mutate(
    
    ## Features from Episode_Title
    # Modify Episode_Title to be binned 
    Episode_Stage = case_when(
      Episode_Title <= 10 ~"New",
      Episode_Title <= 50 ~"Established",
      TRUE ~ "Legacy"),
    
    # Special Numbers
    Is_Milestone = case_when(
      Episode_Title%% 25 == 0 ~ "Special", # Episodes like 25, 50, 75...
      Episode_Title%% 50 == 0 ~ "Special", # Episodes like 50, 100, 150...
      Episode_Title%% 10 == 0 & Episode_Title <= 40 ~ "Special",
      TRUE ~ "Regular"
    ),
    
    ## Popularity Features 
    # Combine Popularity
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
      TRUE ~ "Bad_Episode"
    ),
    # Add Density 
    add_density = Episode_Length_minutes / (Number_of_Ads + 1),
    
    # Genre Grouping 
    Meta_genre = case_when(
      Genre %in% c("Education", "Health", "Lifestyle", "Technology", "True Crime")~"Informative",
      Genre %in% c("Comedy", "Music","Sports") ~ "Entertainment",
      Genre %in% c("Business", "News") ~ "Serious"),
    
    # Publication Day Group 
    Publication_Day_Group = case_when(
      Publication_Day %in% c("Tuesday", "Wednesday") ~ "Prime",
      Publication_Day %in% c("Sunday", "Monday", "Saturday") ~ "Steady",
      Publication_Day %in% c("Thursday", "Friday") ~ "Low"
    ),
    
    # Publication Time Group
    Publication_Time_Group = case_when(
      Publication_Time == "Night" ~ "Peak",
      Publication_Time %in% c("Morning", "Evening") ~ "Normal",
      Publication_Time == "Afternoon" ~ "Off_Peak"
    )
  )%>%
  # Convert Char into Factors
  mutate(across(where(is.character), as.factor))


#### Univariate Analysis ####

# Check the distributions of all numerical features
eda %>%
  select_if(is.numeric) %>%
  pivot_longer(cols = everything(),names_to = "Feature",values_to = "Value") %>%
ggplot(aes(x = Value,y = Feature,fill = Feature))+
  geom_boxplot(na.rm = TRUE,outlier.color = "red4")+
  geom_violin(alpha = 0.2,fill = "grey50")+
  scale_fill_viridis_d(option = "A",begin = 0.4,end = 0.7)+
  facet_wrap(~Feature,scales = "free")+
  theme_minimal()+
  labs(
    title = "Numerical Distributions",
    fill = "Features",
    y = "Features"
  )+
  theme(
    title = element_text(size = 20,face = "bold"),
    legend.position = "inside",
    legend.position.inside = c(0.8,0.2),
    legend.key.size = unit(1.2,"cm")
  )

# Check the distributions of all categorical features
eda %>%
  select(-Podcast_Name) %>%
  select_if(is.factor) %>%
  pivot_longer(cols = everything(), names_to = "Feature", values_to = "Value") %>%
  mutate(Value = fct_infreq(Value)) %>% 
  ggplot(aes(x = Value)) +
  geom_bar(fill = "#69b3a2") +
  facet_wrap(~ Feature, scales = "free", ncol = 3) +
  theme_minimal() +
  labs(
    title = "Categorical Distributions",
    x = "",
    y = "Count"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    title = element_text(size = 15)
  )

#### Bivariate Analysis ####

## Cor Matrix
eda %>%
  select_if(is.numeric)%>%
  na.omit()%>%
  cor()%>%
  corrplot(
    method = "color",type = "full",order = "hclust",addCoef.col = TRUE)

## Bivariate Analysis 1-3 Func 1 Scatter plot 2 Correlation 3 Regression
num_biv_func <- function(df,col1,col2){
  
  # Handle NAs
  df <- df %>% na.omit()
  
  # Cor 
  cor <- round(cor(df[[col1]],df[[col2]]),3)
  
  # Regression Analysis with 2 degree polynomial
  poly_reg <- lm(formula = df[[col2]] ~ poly(df[[col1]],2,raw = TRUE),data = df)
  summary_poly <- summary(poly_reg)
  
  # Take the R sqrt 
  r_sqrt <- round(summary_poly$r.squared,2)
  
  # Density plots
  density <- ggplot(data = df,aes(x = df[[col1]],y = df[[col2]]))+
    geom_density2d_filled(show.legend = FALSE)+
    theme_minimal()+
    labs(
      title = "Density Plot",
      x = col1,
      y = col2,
    )
  
  # Line Plot
  line_plot <- ggplot(data = df,aes(x = df[[col1]],y = df[[col2]]))+
    geom_smooth(method = "gam",colour = "green")+
    theme_minimal()+
    labs(
      title = "Line Plot Gam Smoothing",
      caption = paste0("Correlation ~ ",cor,""),
      x = col1,
      y = col2
    )+
    theme(
      plot.caption = element_text(size = 10,hjust = 0,vjust = 58)
    )
  
  # Plot the Polynomial
  poly_plot <- ggplot(data = df,aes(x = df[[col1]],y = df[[col2]]))+
    geom_smooth(method = "lm",formula = y ~ poly(x, 3, raw = TRUE),colour = "red2")+
    theme_minimal()+
    labs(
      title = "2th Degree Polynomial",
      x = col1,
      y = " ",
      caption = paste0("R Sqrt ~ ",r_sqrt,"")
    )+
    theme(
      plot.caption = element_text(size = 10,hjust = 0,vjust = 58)
    )
  
  # Combine the plots
  final_plot <- (line_plot + poly_plot)/density & 
    plot_annotation( 
      title = "Bivariate Analysis 1-3")
  
  return(final_plot)
}

# Execute the func
num_biv_func(df = eda,col1 = "Episode_Length_minutes",col2 = "Listening_Time_minutes")
num_biv_func(df = eda,col1 = "Host_Popularity_percentage",col2 = "Listening_Time_minutes")
num_biv_func(df = eda,col1 = "Guest_Popularity_percentage",col2 = "Listening_Time_minutes")
num_biv_func(df = eda,col1 = "Number_of_Ads",col2 = "Listening_Time_minutes")
num_biv_func(df = eda,col1 = "Number_of_Ads",col2 = "Host_Popularity_percentage")
num_biv_func(df = eda,col1 = "add_density",col2 = "Listening_Time_minutes")
num_biv_func(df = eda,col1 = "Total_Popularity",col2 = "Listening_Time_minutes")
num_biv_func(df = eda,col1 = "Podcast_Quality",col2 = "Listening_Time_minutes")

## Bivariate Analysis for Categorical Features 1 Mosaic plot 2 Heatmap of Counts
biv_cat_func <- function(df, col1, col2) {
  
  # Handle missing values
  df <- na.omit(df[c(col1, col2)])
  
  # Run a chi-sq test
  chisq_test <- chisq.test(df[[col1]],df[[col2]])
  
  # Mosaic plot
  mosaic <- ggplot(df) +
  geom_mosaic(aes(weight = 1,x = product(!!sym(col1)),fill = !!sym(col2))) +
      scale_fill_viridis_d(option = "A") + 
      labs(
        title = "Mosaic Plot",
        x = col1,                         
        y = "Proportion",               
        fill = col2                       
      )+
      theme_minimal()
    
  # Heatmap of Counts
  heatmap <- df %>%
    count(.data[[col1]], .data[[col2]]) %>%
    ggplot(aes(x = .data[[col1]], y = .data[[col2]], fill = n)) +
    geom_tile() +
    scale_fill_viridis_c(option = "A") +
    labs(title = "Heatmap of Counts", x = col1, y = col2, fill = "Count") +
    theme_minimal()
  
  # Combine plots 
  final_plot <- mosaic / heatmap
  # Return
  return(list(
    plot = final_plot,
    chi_sq = chisq_test))
  }

# Execute the func
biv_cat_func(df = eda,col1 = "Genre",col2 = "Episode_Sentiment")
biv_cat_func(df = eda,col1 = "Episode_Title",col2 = "Episode_Sentiment")
biv_cat_func(df = eda,col1 = "Podcast_Name",col2 = "Episode_Sentiment")
biv_cat_func(df = eda,col1 = "Publication_Time",col2 = "Episode_Sentiment")
biv_cat_func(df = eda,col1 = "Genre",col2 = "Publication_Time")
biv_cat_func(df = eda,col1 = "Episode_Sentiment",col2 = "Is_Milestone")
biv_cat_func(df = eda,col1 = "Episode_Sentiment",col2 = "Meta_genre")

## Bivariate Analysis for Categorical ~ Numerical  
biv_cat_num_func <- function(df,num,cat){
  
  ## Pairwise Wilcox Test
  wilcox_test <- pairwise.wilcox.test(df[[num]],g = df[[cat]], p.adjust.method = "bonferroni")

  # Median Bar chart 
  bar_chart <- 
    ggplot(df, aes(x = fct_reorder(.f = df[[cat]],.x =df[[num]],.fun = median), y = df[[num]],fill = df[[cat]])) +
    stat_summary(fun = median, geom = "bar") +
    scale_fill_viridis_d(option = "G")+
    theme_minimal()+
    coord_flip()+
    labs(
      x = " ",
      y = num)+
    theme(
      legend.position = "none")
  
  # Boxplot 
  boxplot <- 
    ggplot(data = df,aes(x = df[[num]],fill = df[[cat]]))+
    geom_boxplot()+
    scale_fill_viridis_d(option = "G")+
    theme_minimal()+
    labs(
      y = " ",
      x = num,
      fill = cat)
  
  # Ridges plot
  ridge_plot <- 
    ggplot(data = df,aes(x = df[[num]],y = df[[cat]],fill = after_stat(x)))+
    geom_density_ridges_gradient() +
    scale_fill_viridis_c(option = "G")+
    theme_minimal()+
    labs(
      x = num,
      fill = num,
      y = " "
    )
  
  # Combine the plots 
  final_plot <- (bar_chart+boxplot)/ridge_plot & plot_annotation(
    title = paste0("",num," Feature by ",cat,"")
  )
  # Return
  return(list(
    plot = final_plot,
    wilcox_test = wilcox_test
  ))
}

# Execute  Bivariate Analysis for Categorical ~ Numerical 
biv_cat_num_func(df = eda,num = "Listening_Time_minutes",cat = "Publication_Time")
biv_cat_num_func(df = eda,num = "Listening_Time_minutes",cat = "Publication_Day")
biv_cat_num_func(df = eda,num = "Listening_Time_minutes",cat = "Genre")
biv_cat_num_func(df = eda,num = "Listening_Time_minutes",cat = "Episode_Sentiment")
biv_cat_num_func(df = eda,num = "Listening_Time_minutes",cat = "Podcast_Name")
biv_cat_num_func(df = eda,num = "Listening_Time_minutes",cat = "Is_Milestone")
biv_cat_num_func(df = eda,num = "Listening_Time_minutes",cat = "Meta_genre")
biv_cat_num_func(df = eda,num = "Listening_Time_minutes",cat = "Podcast_Quality_Group")
biv_cat_num_func(df = eda,num = "Listening_Time_minutes",cat = "Publication_Day_Group")
biv_cat_num_func(df = eda,num = "Listening_Time_minutes",cat = "Publication_Day_Group")




