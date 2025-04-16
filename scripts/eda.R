
#### Libraries ####
library(tidyverse)
library(corrplot)
library(patchwork)

# Load the data 
train <- read.csv("Data/train.csv",stringsAsFactors = TRUE)

# Remove id 
eda <- train %>% select(-id)

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
  select(-Podcast_Name, -Episode_Title) %>%
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
num_biv_func(df = eda,col1 = "Number_of_Ads",col2 = "Guest_Popularity_percentage")
