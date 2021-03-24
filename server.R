library(dplyr)
library(shiny)
library(ggplot2)
library(DT)
library("rnaturalearth")
library("rnaturalearthdata")
library("sf")

##################
# DATA WRANGLING #
##################

originalNetflix <- read.csv(file="netflix_titles.csv", na.strings=c("NA", ""), stringsAsFactors=F)

# Tidy and enrich dataframes
originalNetflix <- subset(originalNetflix, select=-c(show_id))
originalNetflix$date_added <- as.Date(originalNetflix$date_added, format="%B %d, %Y")
originalNetflix <- distinct(originalNetflix, title, country, type, release_year, .keep_all=TRUE)

###################
# DATA PROCESSING #
###################

# Top 5 country
topCountry <- na.omit(originalNetflix) %>% group_by(country) %>% summarise(count=n()) %>% arrange(desc(count)) %>% top_n(5)

# Growth in content
totalContent <- originalNetflix %>% group_by(date_added) %>% summarise(addedToday=n()) %>%
  mutate(total_content=cumsum(addedToday), type="Total Contents")
contentByDate <- originalNetflix %>% group_by(date_added, type) %>% summarise(addedToday=n()) %>% ungroup() %>%
  group_by(type) %>% mutate(total_content=cumsum(addedToday))
fullDataContent <- rbind(as.data.frame(totalContent), as.data.frame(contentByDate))
names(fullDataContent)[4] <- "Type"

# Total movies
totalMovies <- na.omit(originalNetflix) %>% group_by(type) %>% summarise(count=n())
movie <- filter(totalMovies, type == "Movie")
show <- filter(totalMovies, type == "TV Show")

# World Map
world <- ne_countries(scale = "medium", returnclass = "sf")
world$continent[world$continent == "Seven seas (open ocean)"] <- "Africa"
unique_country = unique(originalNetflix$country)

################
# SERVER LOGIC #
################

shinyServer(function(input, output) {
  # Top country
  output$topCountryBar <- renderPlot({
    country <- ggplot(topCountry, aes(x=reorder(country, -count), y=count, fill=country)) +
      geom_bar(stat="identity", show.legend=FALSE) + 
      scale_fill_hue(c=40) +
      labs(x="Country", y="Total Contents", title="Top Countries By Amount of Content Produced") +
      theme(axis.text=element_text(size=12), 
            plot.title=element_text(size=18, margin=margin(0,0,20,0), hjust=0.5),
            axis.title.x=element_text(margin=margin(20,0,0,0), size=15),
            plot.margin=unit(c(25,15,25,15), "pt"),
            axis.title.y=element_text(size=15, margin=margin(0,20,0,0)))
    print(country)
  })
  
  # Content growth
  output$contentGrowth <- renderPlot({
    growth <- ggplot(fullDataContent, aes(x=date_added, y=total_content)) + 
      geom_line(aes(linetype=Type, color=Type), size=1) +
      labs(x="Date", y="Number of Contents", title="Content Growth Each Year") +
      theme(plot.title=element_text(size=18, margin=margin(0,0,20,0), hjust=0.5), 
            axis.title.x=element_text(margin=margin(20,0,0,0), size=15),
            axis.title.y=element_text(size=15, margin=margin(0,20,0,0)),
            plot.margin=unit(c(25,15,25,15), "pt"), axis.text=element_text(size=12), 
            legend.title=element_text(size=15), legend.text=element_text(size=12), legend.position="top")
    print(growth)
  })
  
  # Total movies
  output$totalMoviesBox <- renderInfoBox({
    infoBox(
      "Total Movies", movie$count, icon=icon("film"),
      color = "red"
    )
  })
  
  # Total shows
  output$totalShowsBox <- renderInfoBox({
    infoBox(
      "Total TV Shows", show$count, icon=icon("video"),
      color="red"
    )
  })
  
  # Total contents
  output$totalContentBox <- renderInfoBox({
    infoBox(
      "Total Contents", show$count + movie$count, icon=icon("globe-americas"), color="red"
    )
  })
  
  # Total Countries
  output$totalCountries <- renderInfoBox({
    infoBox(
      "Total Countries", length(unique_country), icon=icon("flag"), color="red"
    )
  })
  
  # Total Continents
  output$totalContinent <- renderInfoBox({
    infoBox(
      "Total Continents", "6", icon=icon("globe"), color="red"
    )
  })
  
  # Map
  output$mapCountry <- renderPlot({
	map <- ggplot(world[world$name %in% unique_country,],aes(fill = continent)) + 
	  geom_sf(color = "black") +
	  labs(title="Countries with content available on Netflix ") +
	  theme(plot.title=element_text(size=18, margin=margin(0,0,20,0), hjust=0.5))
	print(map)  
  })
})
