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
library(gganimate)
library(gifski)
library(readr)
library(ggiraph)
library(ggExtra)
library(plotly)


Data_Viz_Assignment_Dataset <- read_xlsx("Data Viz Assignment Dataset.xlsx")

Top_Movies <- Data_Viz_Assignment_Dataset |> 
  select(Year:Genre) |> 
  filter(Year != "NA") |> 
  mutate(Year = as.numeric(Year)) |> 
  mutate(`90-Day Rotten Tomatoes critic score` = `90-Day Rotten Tomatoes critic score`*100) |> 
  mutate(`90-Day Rotten Tomatoes audience score` = `90-Day Rotten Tomatoes audience score`*100)

COVID_Removed_List <- Top_Movies |> 
  filter(!Year %in% c(2020, 2021))

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
    axis.line = element_line(color = "black", linewidth = 0.5)) +
annotate("label",
    x = 2006,
    y = 2500000000,
    label = "r = -.03
Not including pandemic years, r = .06",
    fill = "white",
    colour = "black",
    label.size = 0,
    size = 4,
    hjust = 0) 

Year_Ratings <- Top_Movies |>
  pivot_longer(cols = c(`90-Day Rotten Tomatoes audience score`, `90-Day Rotten Tomatoes critic score`),
               names_to = "Rating Type",
               values_to = "Rating")

Graph_1_Scientific_Rating <- ggplot(data = Year_Ratings, aes(x = Year, y = Rating, colour = `Rating Type`)) +
  geom_point(na.rm = TRUE) +
  geom_smooth(method = "lm",
              se = FALSE,
              fullrange = TRUE,
              linewidth = 1.2,
              na.rm = TRUE) +
  labs(title = "Rotten Tomatoes Ratings Have Steadily Increased Year-by-Year",
       x = "Year",
       y = "Rotten Tomatoes Rating (%)") +
  scale_y_continuous(limits = c(18, 100),
                     breaks = seq(20, 100, 20)) +
  scale_x_continuous(limits = c(2006, 2025),
                     breaks = seq(2006, 2025, 5)) +
  scale_colour_grey(start = 0.2, end = 0.7) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  theme(text = element_text(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black", linewidth = 0.5))+
  annotate("label",
           x = 2015,
           y = 30,
           label = "Critic r = .26
Audience r = .48 ",
           fill = "white",
           colour = "black",
           label.size = 0,
           size = 4,
           hjust = 0)

year_by_critic <- cor.test(Top_Movies$Year, Top_Movies$`90-Day Rotten Tomatoes critic score`, method = "pearson")

year_by_audience<- cor.test(Top_Movies$Year, Top_Movies$`90-Day Rotten Tomatoes audience score`, method = "pearson")

year_by_revenue_correlation <- cor.test(Top_Movies$Year, Top_Movies$`Inflation-Adjusted Box Office`, method = "pearson")

COVID_removed_year_by_revenue_correlation <- cor.test(COVID_Removed_List$Year, COVID_Removed_List$`Inflation-Adjusted Box Office`, method = "pearson")

  ## Accessible --> Line graph
Graph_1_Accessible <- ggplot(data = Top_Movies, aes(x = Year, y = `Inflation-Adjusted Box Office`))+
  stat_summary(aes(group = 1),
    fun = mean,
    geom = "line",
    colour = "red3",
    linewidth = 1.2) +
  stat_summary(aes(group = 1),
    fun = mean,
    geom = "point",
    colour = "orangered4",
    fill = "orangered3",
    shape = 21,
    size = 3) +
  annotate("segment",
    x = 2016,
    y = 750000000,
    xend = 2019.7,
    yend = 540000000,
    colour = "orangered3",
    linewidth = 0.8,
    arrow = arrow(length = unit(0.15, "inches"))) +
  annotate("label",
    x = 2016,
    y = 750000000,
    label = "COVID-19 pandemic",
    fill = "lemonchiffon2",
    colour = "black",
    label.size = 0,
    size = 4) +
  labs(title = "How Much Do the Biggest Movies Really Make?",
    subtitle = "Average worldwide box office for the five biggest films of 2006-2025, adjusted for inflation",
    x = "Year",
    y = "Average worldwide box office (USD)") +
  scale_y_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  scale_x_continuous(breaks = seq(2006, 2025, 2)) +
  theme_classic() +
  theme(plot.background = element_rect(fill = "lemonchiffon3"),
    panel.background = element_rect(fill = "lemonchiffon3"),
    text = element_text(colour = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "black"),
    axis.text = element_text(colour = "black"),
    axis.title = element_text(colour = "black"),
    plot.title = element_text(size = 18, face = "bold"))

  animated_plot <- Graph_1_Accessible + 
  transition_reveal(Year)

animate(animated_plot, renderer = gifski_renderer())

## Year by Ratings

Graph_1_Accessible_Rating <- ggplot(data = Year_Ratings, aes(x = Year, y = Rating, colour = `Rating Type`, group = `Rating Type`)) +
  stat_summary(fun = mean,
               geom = "line",
               linewidth = 1.2) +
  stat_summary(fun = mean,
               geom = "point",
               shape = 21,
               size = 3) +
  labs(title = "How Much Do the Biggest Movies Really Make?",
       subtitle = "Average Rotten Tomatoes Ratings for the five biggest films of 2006-2025, adjusted for inflation",
       x = "Year",
       y = "Rotten Tomatoes Ratings (%))") +
  scale_y_continuous(limits = c(18, 100),
                     breaks = seq(20, 100, 20)) +
  scale_x_continuous(breaks = seq(2006, 2025, 2)) +
  scale_colour_manual(values = c(
      "90-Day Rotten Tomatoes audience score" = "orangered3",
      "90-Day Rotten Tomatoes critic score" = "steelblue4"),
    labels = c("90-Day Rotten Tomatoes audience score" = "Audience",
      "90-Day Rotten Tomatoes critic score" = "Critics")) +
  theme_classic() +
  theme(plot.background = element_rect(fill = "lemonchiffon3"),
        panel.background = element_rect(fill = "lemonchiffon3"),
        text = element_text(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "black"),
        axis.text = element_text(colour = "black"),
        axis.title = element_text(colour = "black"),
        plot.title = element_text(size = 18, face = "bold"))

animated_plot <- Graph_1_Accessible_Rating + 
  transition_reveal(Year)

animate(animated_plot,
        renderer = gifski_renderer())

## Graph 2: Revenue (Y) by Critic and Audience Ratings (X) --> Although both critic and audience ratings are related to box office revenue, these ratings aren't very good at explaining box office turnout. Instead, we need to look at the factors that put butts into seats in the first place.
  ## Scientific --> Scatter plot disaggregated by rating type
Ratings_Revenue <- COVID_Removed_List |>
pivot_longer(cols = c(`90-Day Rotten Tomatoes audience score`, `90-Day Rotten Tomatoes critic score`),
  names_to = "Rating Type",
  values_to = "Rating")

Graph_2_Scientific <- ggplot(data = Ratings_Revenue, aes(x = Rating, y = `Inflation-Adjusted Box Office`, colour = `Rating Type`)) +
  geom_point(na.rm = TRUE) +
  geom_smooth(method = "lm",
  se = FALSE,
  fullrange = TRUE,
  linewidth = 1.2,
  na.rm = TRUE) +
labs(title = "The Financial Success of BlockBusters Have Little To Do With Their Critic and Audience Ratings",
  x = "Rating (%)",
  y = "Box Office Revenue (USD)") +
scale_x_continuous(limits = c(18, 100),
  breaks = seq(20, 100, 20)) +
scale_y_continuous(labels = label_dollar(
  scale_cut = cut_short_scale())) +
scale_colour_grey(start = 0.2, end = 0.7) +
theme_minimal() +
  theme(legend.position = "bottom") +
  theme(text = element_text(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black", linewidth = 0.5))+
annotate("label",
  x = 20,
  y = 2500000000,
  label = "Critic r = .29
Audience r = .18 ",
  fill = "white",
  colour = "black",
  label.size = 0,
  size = 4,
  hjust = 0) 
  
rating_by_revenue_correlation <- cor.test(COVID_Removed_List$`90-Day Rotten Tomatoes critic score`, COVID_Removed_List$`Inflation-Adjusted Box Office`, method = "pearson")

rating_by_audience_correlation <- cor.test(COVID_Removed_List$`90-Day Rotten Tomatoes audience score`, COVID_Removed_List$`Inflation-Adjusted Box Office`, method = "pearson")

critic_by_audience_correlation <- cor.test(Top_Movies$`90-Day Rotten Tomatoes audience score`, Top_Movies$`90-Day Rotten Tomatoes critic score`, method = "pearson")

## Accessible --> Patched scatter plots
Graph_2_Accessible_Audience <- ggplot(data = COVID_Removed_List, aes(x = `90-Day Rotten Tomatoes audience score`, y = `Inflation-Adjusted Box Office`)) +
  geom_point_interactive(aes(tooltip = `Movie Title`, data_id = `Movie Title`), na.rm = TRUE, colour = "orangered3", size = 3) +
  scale_x_continuous(limits = c(18, 100),
      breaks = seq(20, 100, 20)) +
  scale_y_continuous(labels = label_dollar(
    scale_cut = cut_short_scale())) +
  labs(x = "Audience Rating (%)",
       y = "Box Office Revenue (USD)") +
  theme_classic() +
  theme(text = element_text(colour = "black"),
        plot.background = element_rect(fill = "lemonchiffon3"),
        panel.background = element_rect(fill = "lemonchiffon3"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(colour = "black"),
        axis.line = element_line(color = "black", linewidth = 0.5))

Graph_2_Accessible_Audience <- Graph_2_Accessible_Audience +
  geom_smooth_interactive(method = "lm",
                          se = FALSE,
                          fullrange = TRUE,
                          linewidth = 1.2,
                          na.rm = TRUE,
                          colour = "orangered4")

Graph_2_Accessible_Critic <- ggplot(data = COVID_Removed_List, aes(x = `90-Day Rotten Tomatoes critic score`, y = `Inflation-Adjusted Box Office`)) +
  geom_point_interactive(aes(tooltip = `Movie Title`, data_id = `Movie Title`), na.rm = TRUE, colour = "orangered3", size = 3) +
  scale_x_continuous(limits = c(18, 100),
       breaks = seq(20, 100, 20)) +
  scale_y_continuous(labels = label_dollar(
       scale_cut = cut_short_scale())) +
  labs(x = "Critic Rating (%)",
       y = "Box Office Revenue (USD)") +
  theme_classic() +
  theme(text = element_text(colour = "black"),
       plot.background = element_rect(fill = "lemonchiffon3"),
       panel.background = element_rect(fill = "lemonchiffon3"),
       panel.grid.major.x = element_blank(),
       panel.grid.minor = element_blank(),
       panel.grid.major.y = element_line(colour = "black"),
       axis.line = element_line(color = "black", linewidth = 0.5)) 

Graph_2_Accessible_Critic <- Graph_2_Accessible_Critic +
  geom_smooth_interactive(method = "lm",
                          se = FALSE,
                          fullrange = TRUE,
                          linewidth = 1.2,
                          na.rm = TRUE,
                          colour = "orangered4")

Combined_Graph_2 <- Graph_2_Accessible_Audience + Graph_2_Accessible_Critic + 
  plot_annotation((title = "Better Ratings Don't Guarantee Higher Profits"),
  theme = theme(plot.title = element_text(size = 18, face = "bold", hjust = 0, colour = "black"),
    plot.background = element_rect(fill = "lemonchiffon3", colour = NA)))

girafe(ggobj = Combined_Graph_2,
  width_svg = 12, height_svg = 6,
  options = list(opts_hover(css = "fill:black;stroke:black;cursor:pointer;"),
    opts_tooltip(css = "background-color:white;color:black;padding:5px;border-radius:3px;")))


  ## Graph 3: Critic and Audience Ratings (Y) by Genre (X)
### This is further complicated by the influence of genre on both Rotten Tomatoes Ratings and Box Office Revenue. In particular, films that primarily fell under the genre of "Adventure" were simultaneously less likely to receive higher ratings from both critics and audiences (with wider discrepancies), yet no less successful at the box office. Ultimately, it's clear that what defines a "successful" film has more to do with one's definition of success (i.e., ratings, revenue, etc.) than any single characteristic of a film.
  ## Scientific --> Dumbbell plot

Ratings_by_Genre <- Top_Movies |> 
  group_by(Genre) |> 
  summarise(Audience = mean(`90-Day Rotten Tomatoes audience score`, na.rm = TRUE), Critic = mean(`90-Day Rotten Tomatoes critic score`, na.rm = TRUE), 'Box Office' = mean(`Inflation-Adjusted Box Office`, na.rm = TRUE), count = n()) |> 
  filter(count > 5) 

Graph_3_Scientific_Ratings <- ggplot(Ratings_by_Genre) +
  geom_segment(aes(x = Audience, xend = Critic, y = Genre, yend = Genre), colour = "darkgrey") +
  geom_point(aes(x = Audience, y = Genre, colour = "Audience"), 
    size = 3) +
  geom_point(aes(x = Critic, y = Genre, colour = "Critic"), 
    size = 3) +
  scale_colour_manual(name = "Rating Type",
    values = c("Audience" = "black", "Critic" = "gray70")) +
  labs(x = "Rotten Tomatoes Ratings") +
  theme_minimal() +
  theme(legend.position = "bottom",
    text = element_text(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.text = element_text(size = 10))

Key_Genres <- COVID_Removed_List |> 
  filter(Genre == "Action" | Genre == "Adventure" | Genre == "Fantasy" | Genre == "Kids & Family" | Genre == "Sci-Fi") 

Graph_3_Scientific_Box_Office <- ggplot(Key_Genres, aes(x = Genre, y = `Inflation-Adjusted Box Office`)) +
  geom_violin(trim = TRUE, fill = "gray95", color = "black", alpha = 0.4) +
  geom_jitter(alpha = 0.4) +
  stat_summary(fun = "mean", 
               geom = "crossbar", 
               width = 0.5,
               colour = "black",
               linewidth = 0.4) +
  scale_y_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  theme_minimal() +
  labs(y = "Average worldwide box office (USD)") +
  theme(text = element_text(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black", linewidth = 0.5))

Graph_3_Scientific <- Graph_3_Scientific_Ratings / Graph_3_Scientific_Box_Office +
  plot_annotation((title = "Genre Differentially Influences Ratings and Box Office Revenue"),
  theme = theme(plot.title = element_text(size = 18, face = "bold", hjust = 0, colour = "black")))

  ## Accessible --> Double bar graph

Year_Ratings_Genre <- Year_Ratings |> 
  filter(Genre == "Action" | Genre == "Adventure" | Genre == "Fantasy" | Genre == "Kids & Family" | Genre == "Sci-Fi")

Graph_3_Accessible_Ratings <- ggplot(Year_Ratings_Genre, aes(x = Genre, y = Rating, fill = `Rating Type`)) +
  stat_summary(aes(group = `Rating Type`),
    fun = mean,
    geom = "col",
    position = position_dodge(width = 0.85),
    width = 0.7,
    na.rm = TRUE) +
  geom_point_interactive(aes(fill = `Rating Type`, tooltip = `Movie Title`, data_id = `Movie Title`),
    shape = 21,
    stroke = 0.5,
    colour = "black",
    position = position_dodge(width = 0.8),
    alpha = 0.6,
    size = 2) +
  scale_y_continuous(limits = c(0, 100),
    breaks = seq(0, 100, 20)) +
  scale_fill_manual(values = c("90-Day Rotten Tomatoes critic score	" = "orangered4",
      "90-Day Rotten Tomatoes audience score" = "grey23")) +
  labs(x = "Genre",
    y = "Rotten Tomatoes Score (%)",
    fill = "Rating Type") +
  theme_classic() +
  theme(text = element_text(colour = "black"),
        plot.background = element_rect(fill = "lemonchiffon3"),
        panel.background = element_rect(fill = "lemonchiffon3"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(colour = "black"),
        axis.line = element_line(color = "black", linewidth = 0.5),
        legend.background = element_rect(fill = "lemonchiffon3", colour = NA),
        legend.position = "bottom")

Graph_3_Accessible_Box_Office <- ggplot(Ratings_by_Genre, aes(x = Genre, y = `Box Office`)) +
  geom_col(color = "black", fill = "orangered4") +
  scale_y_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  theme_minimal() +
  labs(y = "Average worldwide box office (USD)") +
  theme_classic() +
          theme(text = element_text(colour = "black"),
                plot.background = element_rect(fill = "lemonchiffon3"),
                panel.background = element_rect(fill = "lemonchiffon3"),
                panel.grid.major.x = element_blank(),
                panel.grid.minor = element_blank(),
                panel.grid.major.y = element_line(colour = "black"),
                axis.line = element_line(color = "black", linewidth = 0.5)) 

Combined_Graph_3 <- Graph_3_Accessible_Ratings / Graph_3_Accessible_Box_Office + 
  plot_annotation((title = "Adventure Movies Tank in Critic Ratings but Score at the Box Office"),
                  theme = theme(plot.title = element_text(size = 18, face = "bold", hjust = 0, colour = "black"),
                                plot.background = element_rect(fill = "lemonchiffon3", colour = NA)))

girafe(ggobj = Combined_Graph_3,
       width_svg = 10, height_svg = 12,
       options = list(opts_hover(css = "fill:black;stroke:black;cursor:pointer;"),
                      opts_tooltip(css = "background-color:white;color:black;padding:5px;border-radius:3px;")))

