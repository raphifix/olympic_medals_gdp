rm(list = ls())
library(dplyr)
library(plotly)
library(rvest)
library(stringr)
library(zoo)

medal_count <-
  "https://en.wikipedia.org/wiki/2016_Summer_Olympics_medal_table" %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/table[2]') %>%
  html_table(fill = T) %>% 
  data.frame() %>% 
  select(-contains('rank')) %>% 
  mutate(NOC = str_trim(sapply(strsplit(NOC, '\\('), `[[`, 1)))

gdp_pc <-
  "https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(PPP)_per_capita" %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/table') %>%
  html_table(fill = T) %>% 
  data.frame() %>% 
  select(X2, X3)
gdp_pc <- gdp_pc[-c(1,2),]
gdp_pc$indic <- NA
gdp_pc$indic[which(gdp_pc$X3 == 'Int$')] <- seq(1, length(which(gdp_pc$X3 == 'Int$')))
gdp_pc$indic <- na.locf(gdp_pc$indic)
gdp_pc <- gdp_pc[which(gdp_pc$X3 != 'Int$'), ]
gdp_pc$X3 <- as.numeric(gsub(',', '', gdp_pc$X3))
gdp_pc$X2 <- str_trim(gdp_pc$X2)

population <- 
  "https://en.wikipedia.org/wiki/List_of_countries_by_population_(United_Nations)" %>% 
  read_html() %>% 
  html_node(xpath = '//*[@id="mw-content-text"]/table') %>% 
  html_table() %>% 
  select('country' = contains('Country'),
         'pop' = contains('2016'),
         contains('region')) %>% 
  mutate(pop = as.numeric(gsub(',', '', pop)),
         country = str_trim(country))

combined_dat <-
  gdp_pc %>%
  filter(indic == 1) %>%
  inner_join(medal_count, by = c('X2' = 'NOC')) %>% 
  inner_join(population, by = c('X2' = 'country'))

plot_ly(data = combined_dat, x = X3, y = Total, text = X2, size = pop, color = `UN continental
region[1]`, mode = "markers")

plot_ly(data = combined_dat, x = pop, y = X3, text = X2, size = Total, color = `UN continental
region[1]`, mode = "markers")