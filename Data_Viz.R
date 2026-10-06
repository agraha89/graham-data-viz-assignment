#Set-up
library(tidyverse)
library(gt)
library(psych)
library(readxl)
library(dplyr)
library(janitor)
library(ggplot2)
library(scales)
library(patchwork)

Top_Movies <- Data_Viz_Assignment_Dataset |> 
  select(Year:Genre)
COVID_Removed_List <- Top_Movies |> 
  filter(Year != "2020", Year != "2021")


## Graph 1: Revenue (Y) by Year (X)
  ## Scientific --> Scatter plot
Graph_1_Scientific <- ggplot(data = Top_Movies, aes(x = Year, y = `Inflation-Adjusted Box Office`))+
  geom_jitter(alpha = .4)+
  geom_smooth(method = "lm", color = "black", se = TRUE) +
  theme_minimal()+
  labs(title = str_wrap("Inflation-Adjusted Box Office Revenue for the 5 Highest Grossing Films of 2006-2025"), x = "Year", y = "Inflation-Adjusted Worldwide Box Office (USD)")+
  scale_y_continuous(labels = label_dollar(scale_cut = cut_short_scale()))+
  theme((text = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.5))

year_by_revenue_correlation <- cor.test(Top_Movies$Year, Top_Movies$`Inflation-Adjusted Box Office`, method = "pearson")

COVID_removed_year_by_revenue_correlation <- cor.test(COVID_Removed_List$Year, COVID_Removed_List$`Inflation-Adjusted Box Office`, method = "pearson")

  ## Accessible --> Bar graph
Graph_1_Accessible <- ggplot(data = Top_Movies, aes(x = Year, y = `Inflation-Adjusted Box Office`))+
  stat_summary(fun = mean,
    geom = "line",
    aes(group = 1),
    colour = "red",
    linewidth = 1.2) +
  stat_summary(fun = mean,
    geom = "point",
    colour = "red",
    fill = "red",
    shape = 21,
    size = 3) +
  labs(title = "How Much Do the Biggest Movies Really Make?",
    subtitle = "Average worldwide box office of each year's five biggest films, adjusted for inflation",
    x = "Year",
    y = "Average worldwide box office (USD)") +
  scale_y_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  scale_x_continuous(breaks = seq(2006, 2025, 2)) +
  theme_classic() +
  theme(plot.background = element_rect(fill = "white"),
    panel.background = element_rect(fill = "white"),
    text = element_text(colour = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "black"),
    axis.text = element_text(colour = "black"),
    axis.title = element_text(colour = "black"),
    plot.title = element_text(size = 18, face = "bold"))

## Graph 2: Revenue (Y) by Critic and Audience Ratings (X) --> Although both critic and audience ratings are related to box office revenue, these ratings aren't very good at explaining box office turnout. Instead, we need to look at the factors that put butts into seats in the first place.
  ## Scientific --> Two-panel scatter plot
  ## Accessible --> Double line graph




## Graph 3: Critic and Audience Ratings (Y) by Genre (X)
  ## Scientific --> Dumbbell plot
  ## Accessible --> Double bar graph




## Graph 4: Revenue (Y) by Well-known IP (X)
  ## Scientific --> Box plot with jittered points
  ## Accessible --> Bar chart with points
