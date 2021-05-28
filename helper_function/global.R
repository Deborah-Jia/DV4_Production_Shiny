
# get the data ------------------------------------------------------------
library(shiny)
library(ggplot2)
library(ggiraph)
library(cowplot)
library(lubridate)
library(data.table)
library(plotly)
library(gapminder)
library(viridis)
library(hrbrthemes)

nobel <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv")
publication <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winner_all_pubs.csv")

win_country <- nobel[!is.na(birth_country), .N, by = list(birth_country,prize_year)][order(birth_country)][, cumu_n := cumsum(N), by = birth_country] 

country_formatted <- win_country %>%
  group_by(prize_year) %>% # The 'first' method makes it possible to have non-integer ranks while sliding
  mutate(rank = rank(-cumu_n, ties.method = 'first'), Value_lbl = paste0(" ", cumu_n)) %>%
  group_by(birth_country) %>%
  filter(rank <=5) %>%
  ungroup()



# helper function ---------------------------------------------------------
# plot year-category bar chart
year_category_bar <- function(date_start, date_end, categories) {
  p <- nobel[prize_year >= date_start & prize_year <= date_end & category %in% categories ]%>% 
    ggplot(aes(prize_year)) + 
    geom_histogram(aes(fill = category)) +
    labs(caption = "Histogram of Prize-awarding Year by Category", x= 'Year', y= 'Number of Awardees', fill= 'Category') +
    theme(legend.position = "right", plot.caption = element_text(hjust = 0, face= "italic"),
          plot.caption.position =  "plot") +
    scale_fill_brewer(palette = "PiYG") +
    theme_minimal()
  
  return(p)
}

# plot country medals line chart
country_line <- function(country){
  win_country[ birth_country %in% country] %>%
             ggplot( aes(prize_year, cumu_n)) +
             geom_line() +
             scale_color_viridis(discrete = TRUE) +
             ggtitle("Accumulative medals in the previous 120 years") +
             theme_ipsum() +
             ylab("Medal count")
}

# print country summary for one country
country_medal_info <- function(country) {
  country_info <- nobel[birth_country %in% country][, .(n= .N),by= category][order(-n)]$category[1]
  country_n <- nobel[birth_country %in% country][, .(n= .N),by= category][order(-n)]$n[1]
  print(paste0(country, "'s advatageous category is ", country_info, "; till 2016, it has ", country_n, " medals in this field."  ))
}


name_year_table  <- function(ticker, start_date, end_date) {
  
  data <- publication[pub_year >= start_date & pub_year <= end_date & laureate_name %in% ticker]
  return(data)
}



