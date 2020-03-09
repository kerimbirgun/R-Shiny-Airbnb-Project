library(rsconnect)
library(shiny)
library(maps)
library(mapproj)
library(leaflet)
library(dplyr)
library(ggplot2)
library(shinyWidgets)
library(shinythemes)
library(shinyEffects)
library(readr)

data = read.csv('Airbnb_Shiny.csv')

data2 = unique(data %>% select(Neighbourhood))
data2 = as.data.frame(data2);glimpse(data2)
data2$Neighbourhood = as.character(data2$Neighbourhood)
data$Neighbourhood = as.character(data$Neighbourhood)

