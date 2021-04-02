library(dplyr)
library(shiny)
library(ggplot2)
library(DT)
library(sf)
library(plotly)
library(shinyjs)
library(rnaturalearth)

##################
# DATA WRANGLING #
##################

originalNetflix <- read.csv(file="netflix_titles.csv", na.strings=c("NA", ""), stringsAsFactors=F)
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

# New column for age group based on rating
genres <- strsplit(originalNetflix$listed_in, split=", ")
netflixListedIn <- data.frame(type=rep(originalNetflix$type, sapply(genres, length)),
							  country=rep(originalNetflix$country, sapply(genres, length)),
                              listed_in=unlist(genres),
							  release_year=rep(originalNetflix$release_year, sapply(genres, length)),
							  continent=rep(originalNetflix$continent, sapply(genres, length)),
                              rating=rep(originalNetflix$rating, sapply(genres, length)))

netflixListedIn$age_group[(netflixListedIn$rating == "TV-PG") | (netflixListedIn$rating == "TV-Y7-FV") | (netflixListedIn$rating == "TV-Y7") |
                       (netflixListedIn$rating == "PG")] <- "Older Kids"
netflixListedIn$age_group[(netflixListedIn$rating == "TV-MA") | (netflixListedIn$rating == "R") | (netflixListedIn$rating == "NR") |
                       (netflixListedIn$rating == "UR") | (netflixListedIn$rating == "NC-17")] <- "Adults"
netflixListedIn$age_group[(netflixListedIn$rating == "TV-14") | (netflixListedIn$rating == "PG-13")] <- "Teens"
netflixListedIn$age_group[(netflixListedIn$rating == "TV-Y") | (netflixListedIn$rating == "TV-G") | (netflixListedIn$rating == "G")] <- "Kids"


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
                                          text=paste("Type: ", Type, 
                                                     "<br>Date Added: ", date_added, 
                                                     "<br>Total Contents: ", total_content))) + 
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
  
	
  # Unique Country
  output$countryOutput <- renderUI({
    selectInput("countryInput", "Country",
              append("All", unique_country, after = 1),
              selected = "All")})

  # Initialize a variable to count how many times "Next" is clicked.
  values <- reactiveValues(data = 1)

  # Control "Next" button
  observeEvent(input$btnN, {
	if (values$data < 4){
		values$data = values$data + 1
	}}
  )
  
  # Control "Back" button 
  observeEvent(input$btnB, {
	if (values$data != 1){
		values$data = values$data - 1
	}}
  )
  
  observe(
	if (values$data == 1){
		disable('btnB')
		enable('btnN')
	}
	else if (values$data == 4){
		enable("btnB")
		disable('btnN')
	}
	else{
		enable('btnB')
		enable('btnN')
	}
  )

  # Determine which filter display
  output$filter <- renderUI({
	if (values$data == 0) {
		 return()
		 
	  } else if (values$data == 1) {
	  
	    #Age Group
		selectInput("ageGroupInput", "Age Group",
                append("All", unique(netflixListedIn$age_group), after=1),
                selected="All")
				
	  }else if (values$data == 2){
	  
	    bubbleChart <- netflixListedIn
		 
	     if (input$ageGroupInput != "All") {
		  bubbleChart <- netflixListedIn %>% filter(age_group == input$ageGroupInput)
		}
	  
		#Genre
		selectInput("genreInput", "Genre",
			  append("All", unique(bubbleChart$listed_in), after = 1),
              selected = "All")  
			  
	  }else if (values$data == 3){
	  
	    countryMap <- netflixListedIn
		 
	    if (input$ageGroupInput != "All") {
		  countryMap <- netflixListedIn %>% filter(age_group == input$ageGroupInput)
		}
		
		if (input$genreInput != "All") {
		  countryMap <- netflixListedIn %>% filter(listed_in == input$genreInput)
		}
	  
		#Release Year
		sliderInput("countryInput", label = "Slider Range", min = min(countryMap$release_year), max = max(countryMap$release_year), value = c(min(countryMap$release_year), max(countryMap$release_year)))  
	  }
  }
  )
	
  # Determine which graph display
  output$graph <- renderPlotly({
	  if (values$data == 0) {
		 return()
	  } else if (values$data == 1) {
	  
	    # Genre Bar Graph
		genresBar <- netflixListedIn
    
		if (input$ageGroupInput != "All") {
		  genresBar <- netflixListedIn %>% filter(age_group == input$ageGroupInput)
		}
		genresBarS <- na.omit(genresBar) %>% count(listed_in) %>% top_n(5)
		
		newBar <- ggplot(genresBarS, aes(x=listed_in, y=n, fill=listed_in, text=paste("Number of Contents: ", n, "<br>Genre: ", listed_in))) + 
		  geom_bar(stat="identity", show.legend=FALSE) +
		  scale_fill_brewer(palette="Dark2") +
		  labs(x="Genres", y="Number of Contents Contents", title="Most Popular Genres Based on Age Group") +
		  theme(axis.text=element_text(size=10),
				legend.position="none",
				plot.title=element_text(size=13, hjust=0.5),
				axis.title.x=element_text(size=12),
				axis.title.y=element_text(size=12))
		
		ggplotly(newBar, tooltip=c("text"))
	  } else if (values$data == 2) {
	  
	     bubbleChart <- netflixListedIn
		 
	     if (input$ageGroupInput != "All") {
		  bubbleChart <- netflixListedIn %>% filter(age_group == input$ageGroupInput)
		}
			
		 if (input$genreInput != "All"){
			bubbleChart <- bubbleChart %>% filter(listed_in == input$genreInput)}
			
		 bubbleChart <- bubbleChart %>% count(release_year,country,continent)	
		 
		 bubble <- ggplot(bubbleChart , aes(x=release_year, y=n, size = n,color = continent,text = paste("Country: ", bubbleChart$country, "\nNo of show: ", bubbleChart$n,sep="")))+ 
				scale_size(name="Continent") +
				geom_point(alpha=0.7)+
				ylab("No. of Movie") +
				xlab("Release Year")
		 	
		 ggplotly(bubble, tooltip = "text")	
		 
	  } else if (values$data == 3) {
	  
	     countryMap <- netflixListedIn
		 
	     if (input$ageGroupInput != "All") {
		  countryMap <- netflixListedIn %>% filter(age_group == input$ageGroupInput)
		}
		
		 if (input$genreInput != "All"){
			countryMap <- countryMap %>% filter(listed_in == input$genreInput)}
			
		 countryMap <- countryMap %>% filter(release_year >= input$countryInput[1])
		 countryMap <- countryMap %>% filter(release_year <= input$countryInput[2])
			
		 countryMap <- countryMap %>% count(country)	
		 countryMap <- merge(countryMap,iso,by.x="country",by.y = "country")	
		 map <- plot_ly(countryMap, type='choropleth', locations=countryMap$code, z=countryMap$n, colorscale="tealgrn")	
		 	
		 ggplotly(map)	
	  } else if (values$data == 4) {
		 dataTableOutput("select4")
	  }
  }) 
	
  # Search table
  output$netflixTable <- renderDataTable(
    originalNetflix[,-c(2,6,13)], filter="top",
    colnames=c("ID", "Country", "Title", "Director", "Cast", "Release Year", "Rating", "Duration", "Genres", "Description", "Trailer Link"),
    options=list(
      autoWidth=TRUE, pageLength=10, scrollX=TRUE,
      processing=FALSE,
      columnDefs=list(
        list(width="150px",targets=c(1,2,4,8,9)),
        list(width="100px", targets=3),
        list(width="80px", targets=10),
        list(width="50px", targets=c(5,7)),
        list(width="5px", targets=0),
        list(className="dt-center", targets=c(0,5,6,7))
      )
    ),
	  escape = FALSE
  )
})
