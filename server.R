library(dplyr)
library(shiny)
library(ggplot2)
library(DT)

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
topCountry <- na.omit(originalNetflix) %>% group_by(country, type) %>% summarise(count=n())
totalByCountry <- aggregate(topCountry$count, by=list(country=topCountry$country), FUN=sum) %>% arrange(desc(x)) %>% top_n(5)
names(totalByCountry)[2] <- "count"
totalByCountry <- totalByCountry[order(desc(totalByCountry$count)),]

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

################
# SERVER LOGIC #
################

shinyServer(function(input, output) {
  # Top country
  output$topCountryBar <- renderPlot({
    country <- ggplot(totalByCountry, aes(x=reorder(country, -count), y=count)) +
      geom_bar(stat="identity", color="skyblue", fill="steelblue") + 
      labs(x="Country", y="Total Contents", title="Top Countries By Amount of Content Produced") +
      theme(axis.text=element_text(size=12), axis.title=element_text(size=15), plot.title=element_text(size=16))
    print(country)
  })
  
  # Content growth
  output$contentGrowth <- renderPlot({
    growth <- ggplot(fullDataContent, aes(x=date_added, y=total_content)) + 
      geom_line(aes(linetype=Type, color=Type), size=1) +
      labs(x="Date", y="Number of Contents", title="Content Growth Each Year") +
      theme(plot.title=element_text(size=16), axis.title=element_text(size=15), axis.text=element_text(size=12), 
            legend.title=element_text(size=15), legend.text=element_text(size=12))
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
})
