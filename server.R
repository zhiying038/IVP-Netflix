library(dplyr)
library(shiny)
library(ggplot2)
library(DT)

##################
# DATA WRANGLING #
##################

netflix <- read.csv(file="netflix_titles.csv", na.strings=c("NA", ""), stringsAsFactors=F)

# Tidy and enrich dataframes
netflix <- subset(netflix, select=-c(show_id))
netflix$date_added <- as.Date(netflix$date_added, format="%B %d, %Y")
netflix <- distinct(netflix, title, country, type, release_year, .keep_all=TRUE)

################
# SERVER LOGIC #
################
shinyServer(function(input, output) {})