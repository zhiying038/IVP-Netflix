library(dplyr)
library(shiny)
library(ggplot2)
library(DT)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(plotly)

##################
# DATA WRANGLING #
##################

originalNetflix <- read.csv(file="netflix_titles.csv", na.strings=c("NA", ""), stringsAsFactors=F)
movieNetflix <- read.csv(file="movie_dataset.csv")
iso <- read.csv(file="iso.csv")

# Tidy and enrich dataframes
originalNetflix <- subset(originalNetflix, select=-c(show_id))
originalNetflix$date_added <- as.Date(originalNetflix$date_added, format="%B %d, %Y")
originalNetflix <- distinct(originalNetflix, title, country, type, release_year, .keep_all=TRUE)

createLink <- function(val) {
  sprintf('<a href="%s" target="_blank" class="btn btn-danger">Trailer Link</a>',val)
}
originalNetflix$trailer_link <- createLink(originalNetflix$trailer_link)

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
unique_country = sort(unique(originalNetflix$country))

################
# SERVER LOGIC #
################

shinyServer(function(input, output) {
  # Overview Tab
  # Top country
  output$topCountryBar <- renderPlotly({
    country <- ggplot(topCountry, aes(x=reorder(country, -count), y=count, fill=country, text=paste("Number of Contents: ", count))) +
      geom_bar(stat="identity", show.legend=FALSE) + 
      scale_fill_hue(c=40) +
      labs(x="Country", y="Total Contents", title="Top Countries By Amount of Content Produced") +
      theme(axis.text=element_text(size=10),
            legend.position="none",
            plot.title=element_text(size=13, hjust=0.5),
            axis.title.x=element_text(size=12),
            axis.title.y=element_text(size=12))
   ggplotly(country, tooltip=c("text"))
  })
  
  # Content growth
  output$contentGrowth <- renderPlotly({
    growth <- ggplot(fullDataContent, aes(x=date_added, y=total_content, group=Type,
                                          text=paste("Type: ", Type, "<br>Date Added: ", date_added, "<br>Total Contents: ", total_content))) + 
      geom_line(aes(linetype=Type, color=Type)) +
      labs(x="Date", y="Number of Contents", title="Content Growth Each Year") +
      theme(plot.title=element_text(size=13, hjust=0.5), 
            legend.title=element_blank(),
            axis.title.x=element_text(size=12),
            axis.title.y=element_text(size=12),
            axis.text=element_text(size=10))
    ggplotly(growth, tooltip=c("text"))
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
  
  # Total Year
  output$totalYear <- renderInfoBox({
    infoBox(
      "Total Year Involved", "97", icon=icon("history"), color="red"
    )
  })
  
  # Map
  output$mapCountry <- renderPlotly({
  	map <- ggplot(world[world$name %in% unique_country,], aes(fill = continent)) + 
  	  geom_sf(color="black") +
  	  labs(title="Countries with Content Available on Netflix") +
  	  theme(plot.title=element_text(size=13, hjust=0.5),
  	        legend.title=element_blank())
  	ggplotly(map) 
  })
  
  # Movie Tab
  # Movie Map
  output$mapMovie <- renderPlotly({
	movie_filtered <-
    movieNetflix %>%
    filter(type == input$typeInput,
		   release_year >= input$yearInput[1],
           release_year <= input$yearInput[2]
    )
	
	if (input$countryInput != "All"){
	movie_filtered <- movie_filtered %>% filter(country == input$countryInput)}
	
	if (input$genreInput != "All"){
	movie_filtered <- movie_filtered %>% filter(listed_in == input$genreInput)}
	
	movie_filtered <- movie_filtered %>% count(country)
	movie_filtered <- merge(movie_filtered,iso,by.x="country",by.y = "country")
			
  map <- plot_ly(movie_filtered, type='choropleth', locations=movie_filtered$code, z=movie_filtered$n, colorscale="tealgrn")
	map <- map %>% layout(
    mapbox = list(
      style = 'open-street-map',
      zoom =2.5,
      center = list(lon = -88, lat = 34))) 
	
  	ggplotly(map) 
  })

   #Table
   output$movieResults <- renderDataTable({
  	 movie_filtered <-
  	  movieNetflix %>%
  	  filter(type == input$typeInput,
  		release_year >= input$yearInput[1],
  		release_year <= input$yearInput[2])
		   
  	 if (input$countryInput != "All") {
  	  movie_filtered <- movie_filtered %>% filter(country == input$countryInput)
  	 }
  		
  	 if (input$genreInput != "All") {
  	  movie_filtered <- movie_filtered %>% filter(listed_in == input$genreInput)
  	 }
  	
  	 movie_filtered <- subset(movie_filtered, select=-c(listed_in))
  	 movie_filtered <- unique(movie_filtered)
  	 movie_filtered <- movie_filtered %>% rename(
  	   "Country" = country,
  	   "Type" = type,
  	   "Title" = title,
  	   "Release Year" = release_year,
  	   "Description" = description
  	 )
  	 
  	 movie_filtered
	 })
	
  # Unique Country
  output$countryOutput <- renderUI({
  selectInput("countryInput", "Country",
              append("All", unique_country, after = 1),
              selected = "All")})
			  
  # Unique Genre
  output$genreOutput <- renderUI({
  selectInput("genreInput", "Genre",
			  append("All", unique(movieNetflix$listed_in), after = 1),
              selected = "All")})
			  
	
  # Rating Tab
  # Unique Continent
  output$continentOutput <- renderUI({
  selectInput("continentInput", "Continent",
			  append("All", unique(originalNetflix$continent), after = 1),
              selected = "All")})
			  
  # Unique Rating			  
  output$ratingOutput <- renderUI({
  selectInput("ratingInput", "Rating",
			  append("All", unique(originalNetflix$rating), after = 1),
              selected = "All")})
			  
  # Search table
  output$netflixTable <- renderDataTable(
    originalNetflix[,-c(2,6,9)], filter="top",
    extensions=c("Buttons"),
    colnames=c("ID", "Country", "Title", "Director", "Cast", "Release Year", "Rating", "Genres", "Description", "Trailer Link", "Continent"),
    options=list(
      autoWidth=TRUE, pageLength=10, scrollX=TRUE,
      dom="Bfrtip",
      buttons=c("copy", "csv", "excel", "pdf", "print"),
      columnDefs=list(
        list(width="150px",targets=c(2,3,4,5,7,8,9,10)))
    ),
	  escape = FALSE
  )
})
