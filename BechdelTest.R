#### Attempt #1 ####

#### All Unfinished ####

#### Looking at the Relationship of Budget, IMDB Rating, and Bechdel Score for 1970-1995 ####

#### Packages ####
  
  library(tidytuesdayR)
  library(tidyverse)
  library(plotly)
  library(viridis)
  library(ggthemes)
  library(scales)
  library(devtools)

#### Explanation of Data ####

  tuesdata <- tidytuesdayR::tt_load('2021-03-09')
  tuesdata 

#### Import Data ####

  raw_bechdel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/raw_bechdel.csv')
  view(raw_bechdel)
  movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/movies.csv')
  view(movies)

#### Subsetting Data ####

  ## imdb_id, year (1970-2000), title, rating 
  bechdel_subset <- data.frame(raw_bechdel$imdb_id, raw_bechdel$year, raw_bechdel$title, raw_bechdel$rating)
  names(bechdel_subset) <- c("IMDB_ID", "Year", "Title", "Bechdel_Rating")
  bechdel_subset <- bechdel_subset %>% filter(Year >= 1970)
  bechdel_subset <- bechdel_subset %>% filter(Year <= 1995)
  view(bechdel_subset)
  colSums(is.na(bechdel_subset)) ## no missing data 
  
  ## imdb_id, year, budget_2013, imdb_rating 
  movies_subset <- data.frame(movies$imdb_id, movies$year, movies$budget_2013, movies$imdb_rating)
  names(movies_subset) <- c("IMDB_ID", "Year" ,"Budget_2013", "IMDB_Rating")
  movies_subset <- movies_subset %>% filter(Year >= 1970)
  movies_subset <- movies_subset %>% filter(Year <= 1995)
  movies_subset <- movies_subset %>% filter(Budget_2013 <= 300000000)
  colSums(is.na(movies_subset))  
  movies_subset <- na.omit(movies_subset) ## 202 missing IMDB Ratings, removed rows
  view(movies_subset)

#### Combining Data Frames #### 
  
  ## Including only movies that are in both data frames
  data <- bechdel_subset %>% 
  inner_join(movies_subset, by = c("IMDB_ID" = "IMDB_ID", "Year" = "Year"))
  view(data)  
  
#### Visualization ####
  
  figure <- data %>%
  ## Preparing text for interactive graph 
    mutate(text = paste("Title: ", Title, "\nYear: ", Year, 
    "\nBechdel Score: ", Bechdel_Rating, sep="")) %>%
  ## ggplot
    ggplot(aes(x=Budget_2013, y=IMDB_Rating, size = Bechdel_Rating, 
    color = Bechdel_Rating, text=text)) +
    geom_point(alpha=0.7) +
    scale_x_continuous(labels = comma) +
    theme_fivethirtyeight()
  figure
  
  ## Interactive Figure
  
  figure_interactive <- ggplotly(figure, tooltip="text")
  figure_interactive


#### Attempt #2 ####
  
#### Looking at the Proportion of Bechdel Scores by Year ####
  
#### Packages ####
  
  library(tidyverse)
  library(ggridges)
  library(magick)
  library(ggimage)
  library(pacman)
  library(grid)
  
#### Explanation of Data ####

  tuesdata <- tidytuesdayR::tt_load('2021-03-09')
  tuesdata 

#### Import Data ####

  raw_bechdel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/raw_bechdel.csv')
  view(raw_bechdel)
  movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-09/movies.csv')
  view(movies)

#### Subsetting Data ####

  ## imdb_id, year (1970-2000), title, rating 
  bechdel_subset <- data.frame(raw_bechdel$imdb_id, raw_bechdel$year, raw_bechdel$title, raw_bechdel$rating)
  names(bechdel_subset) <- c("IMDB_ID", "Year", "Title", "Bechdel_Rating")
  bechdel_subset <- bechdel_subset %>% filter(Year >= 1970)
  bechdel_subset <- bechdel_subset %>% filter(Year <= 2013)
  view(bechdel_subset)
  colSums(is.na(bechdel_subset)) ## no missing data 

  ## imdb_id, year, budget_2013, imdb_rating 
  movies_subset <- data.frame(movies$imdb_id, movies$year, movies$budget_2013, movies$imdb_rating, movies$rated)
  names(movies_subset) <- c("IMDB_ID", "Year" ,"Budget_2013", "IMDB_Rating", "Rated")
  movies_subset <- movies_subset %>% filter(Year >= 1970)
  movies_subset <- movies_subset %>% filter(Year <= 2013)
  movies_subset <- filter(movies_subset, Rated != "N/A")
  movies_subset <- filter(movies_subset, Rated != "TV-PG")
  movies_subset <- filter(movies_subset, Rated != "TV-14")
  movies_subset <- movies_subset %>% mutate(Rated = recode(Rated, "Unrated" = "Not Rated")) ## Combining "Not Rated" and "Unrated" into one group 
  colSums(is.na(movies_subset))  
  movies_subset <- na.omit(movies_subset) ## 202 missing IMDB Ratings, removed rows
  view(movies_subset)

#### Combining Data Frames #### 

  ## Including only movies that are in both data frames
  data <- bechdel_subset %>% 
    inner_join(movies_subset, by = c("IMDB_ID" = "IMDB_ID", "Year" = "Year"))
  view(data)  

#### Visualization ####

  data$Bechdel_Rating <- as.numeric(data$Bechdel_Rating)
  data$Rated <- factor(data$Rated, levels = c("X", "NC-17", "R", "PG-13", "PG", "G", "Not Rated"))
  
  colorpalette <- c("#0032AB", "#6028A7", "#8C159F", "#AF0093", "#CB0084", "#E10072", "#F10060")
    ## Generated on https://twitter.com/palitra_color/status/1178163019542609921
  
  ## Image background 
  image <- jpeg::readJPEG("WOMEN,THE1939_00309850_1399x1104_091020081242.jpg")
  
  figure <- ggplot(data, aes(x = Bechdel_Rating, y = Rated, fill = Rated)) +
  geom_density_ridges(show.legend = FALSE, bandwidth = 0.20, jittered_points = TRUE,
                      position = position_points_jitter(width = 0.5, height = 0),
                      point_shape = '|', point_size = 3, point_alpha = 0.7, alpha = 0.7) +
  theme_ridges(center_axis_labels = TRUE) +
  scale_y_discrete(limits = c("Not Rated", "G", "PG", "PG-13", "R", "NC-17", "X")) +
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = colorpalette) +
  xlim(-0.5, 3.7) +
  xlab("Bechdel Score") + ylab("Motion Picture Association Film Rating") +
    theme(plot.background = element_rect(fill = "#e6cce1"), panel.background = element_rect(fill = "#e6cce1"))
  
  figure
  
  
  

    



