library(dplyr)
library(shiny)
library(ggplot2)
library(DT)
library(sf)
library(plotly)
library(shinyjs)
library(maps)
library(rnaturalearth)

##################
# DATA WRANGLING #
##################

originalNetflix <- read.csv(file="netflix_titles.csv", na.strings=c("NA", ""), stringsAsFactors=F)
map <- read.csv(file="map.csv")

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
  							  title=rep(originalNetflix$title, sapply(genres, length)),
  							  continent=rep(originalNetflix$continent, sapply(genres, length)),
                  rating=rep(originalNetflix$rating, sapply(genres, length)),
  							  description=rep(originalNetflix$description, sapply(genres, length))
							  )

netflixListedIn$age_group[(netflixListedIn$rating == "TV-PG") | (netflixListedIn$rating == "TV-Y7-FV") | (netflixListedIn$rating == "TV-Y7") |
                       (netflixListedIn$rating == "PG")] <- "Older Kids"
netflixListedIn$age_group[(netflixListedIn$rating == "TV-MA") | (netflixListedIn$rating == "R") | (netflixListedIn$rating == "NR") |
                       (netflixListedIn$rating == "UR") | (netflixListedIn$rating == "NC-17")] <- "Adults"
netflixListedIn$age_group[(netflixListedIn$rating == "TV-14") | (netflixListedIn$rating == "PG-13")] <- "Teens"
netflixListedIn$age_group[(netflixListedIn$rating == "TV-Y") | (netflixListedIn$rating == "TV-G") | (netflixListedIn$rating == "G")] <- "Kids"


################
# SERVER LOGIC #
################

shinyServer(function(input, output, session) {
  # Overview Tab
  # Top country
  output$topCountryBar <- renderPlotly({
    country <- ggplot(topCountry, aes(x=reorder(country, -count), y=count, fill=country, text=paste("Number of Contents: ", count))) +
      geom_bar(stat="identity", show.legend=FALSE) + 
      scale_fill_hue(c=40) +
      labs(x="Country", y="Total Contents", title="Countries By Content Produced") +
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
	
	
  observeEvent(input$help,introjs(session, options = list("showBullets"="false", "showProgress"="true", "showStepNumbers"="false","nextLabel"="Next","prevLabel"="Prev","skipLabel"="Skip")))
	
  # Initialize a variable to count how many times "Next" is clicked.
  values <- reactiveValues(data = 1)

  # Control "Next" button
  observeEvent(input$btnN, {
	if (values$data < 5){
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
	  hide("btnB")
		show('btnN')
	}
	else if (values$data == 5){
		show("btnB")
		hide("btnN")
	}
	else{
		show('btnB')
		show('btnN')
	}
  )

  # Determine which filter display
  output$filter <- renderUI({
	if (values$data == 0) {
		 return()
	  } else if (values$data == 1) {
	    #Age Group
		  selectInput("ageGroupInput", "Select Age Group",
                append("All", unique(netflixListedIn$age_group), after=1),
                selected="All")
	  } else if (values$data == 2) {
	    typePie <- netflixListedIn
	    
	    if (input$ageGroupInput != "All") {
	      typePie <- typePie %>% filter(age_group == input$ageGroupInput)
	    }
	    
	    radioButtons("typeInput", "Select Type", choices=list("Movie" = "Movie", "TV Show" = "TV Show"), inline=TRUE)
	  } else if (values$data == 3) {
	    bubbleMap <- netflixListedIn
		 
	    if (input$ageGroupInput != "All") {
		    bubbleMap <- bubbleMap %>% filter(age_group == input$ageGroupInput)
	    }
	  
  		#Genre
  		selectInput("genreInput", "Select Genre",
  			  append("All", unique(bubbleMap$listed_in), after = 1),
                selected = "All")  
	  } else if (values$data == 4) {
	    lineGraph <- netflixListedIn
		 
	    if (input$ageGroupInput != "All") {
		    lineGraph <- lineGraph %>% filter(age_group == input$ageGroupInput)
		  }
		
  		if (input$genreInput != "All") {
  		  lineGraph <- lineGraph %>% filter(listed_in == input$genreInput)
  		}
	  
		  #Release Year
		  sliderInput("yearInput", label = "Select Range of Release Year", min = min(lineGraph$release_year), 
		              max = max(lineGraph$release_year), value = c(min(lineGraph$release_year), max(lineGraph$release_year)),
		              format="####")  
		
	  } else if (values$data == 5) {
	    tableResult <- netflixListedIn
		 
	    if (input$ageGroupInput != "All") {
		    tableResult <- tableResult %>% filter(age_group == input$ageGroupInput)
		  }
		
  		if (input$genreInput != "All") {
  		  tableResult <- tableResult %>% filter(listed_in == input$genreInput)
  		}
	  
	    tableResult <- tableResult %>% filter(release_year >= input$yearInput[1])
		  tableResult <- tableResult %>% filter(release_year <= input$yearInput[2])
		 
  		#Release Year
  		selectInput("countryInput", "Select Country",
                  append("All", unique(tableResult$country), after=1),
                  selected="All")
  	  }
    }
  )
  
  # Output type selection
  output$typeSelection <- renderText({
    if (values$data == 2) {
      return(paste("Selected Type: ", input$typeInput))
    } else {
      return()
    }
  })
	
  # Determine which graph display
  output$graph <- renderPlotly({
	  if (values$data == 0) {
		 return()
	  } else if (values$data == 1) {
	    # Genre Bar Graph
		  genresBar <- netflixListedIn
    
		  req(input$ageGroupInput)
  		if (input$ageGroupInput != "All") {
  		  genresBar <- netflixListedIn %>% filter(age_group == input$ageGroupInput)
  		}
		  genresBarS <- na.omit(genresBar) %>% count(listed_in) %>% top_n(5)
		
  		newBar <- ggplot(genresBarS, aes(x=reorder(listed_in, -n), y=n, fill=listed_in, text=paste("Number of Contents: ", n, "<br>Genre: ", listed_in))) + 
  		  geom_bar(stat="identity", show.legend=FALSE) +
  		  scale_fill_brewer(palette="Dark2") +
  		  labs(x="Genres", y="Number of Contents", title="Most Popular Genres Based on Age Group") +
  		  theme(axis.text=element_text(size=10),
  				legend.position="none",
  				plot.title=element_text(size=13, hjust=0.5),
  				axis.title.x=element_text(size=12),
  				axis.title.y=element_text(size=12))
		
		  ggplotly(newBar, tooltip=c("text"), height=450) %>% layout(margin=list(l = 50, r = 50, b = 20, t = 50))
	  } else if (values$data == 2) {
	    typePie <- netflixListedIn
	    
	    req(input$ageGroupInput)
	    if (input$ageGroupInput != "All") {
	      typePie <- typePie %>% filter(age_group == input$ageGroupInput)
	    }
	    
	    pie <- na.omit(typePie) %>% count(type)

	    newPie <- plot_ly(pie, labels=~type, values=~n, type="pie", height=450,marker = list(colors = c('#DD4B39', '#39CBDD')))
	    newPie <- newPie %>% layout(title="Proportion of Movies and TV Shows", margin=list(l = 50, r = 50, b = 20, t = 50))
	  } else if (values$data == 3) {
      bubbleMap <- netflixListedIn
		 
      req(input$ageGroupInput)
      if (input$ageGroupInput != "All") {
       bubbleMap <- bubbleMap %>% filter(age_group == input$ageGroupInput)
      }
      
      req(input$typeInput)
      if (input$typeInput == "Movie") {
        bubbleMap <- bubbleMap %>% filter(type == "Movie")
      } else {
        bubbleMap <- bubbleMap %>% filter(type == "TV Show")
      }
			
      req(input$genreInput)
	    if (input$genreInput != "All"){
	     bubbleMap <- bubbleMap %>% filter(listed_in == input$genreInput)
	    } 
		 
  	  bubbleMap <- bubbleMap %>% count(country, continent)
  	  colnames(bubbleMap)[3] <- "shows"
	 
  	  # get the world map
  	  world <- map_data("world")
	 
	    bubbleMap <- merge(bubbleMap,map,by.x="country",by.y = "country")
	 
  	  newBubble <- ggplot() +
    		geom_polygon(data = world, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
    		geom_point(data=bubbleMap, aes(x=longitude, y=latitude, size=shows, color = continent,
    		                               text = paste("country: ", bubbleMap$country)))+
    		theme(legend.title=element_blank(), plot.title = element_text(hjust = 0.5))+
    		labs(y="Latitude", x = "Longitude")+
    		ggtitle("Number of Shows Available In Each Country")
  	  
  	  ggplotly(newBubble, height=450) %>% layout(margin=list(l = 50, r = 50, b = 20, t = 50))
	  } else if (values$data == 4) {
	    lineGraph <- netflixListedIn
	    
	    req(input$ageGroupInput)
	    if (input$ageGroupInput != "All") {
		    lineGraph <- lineGraph %>% filter(age_group == input$ageGroupInput)
	    }
	    
	    req(input$typeInput)
	    if (input$typeInput == "Movie") {
	      lineGraph <- lineGraph %>% filter(type == "Movie")
	    } else {
	      lineGraph <- lineGraph %>% filter(type == "TV Show")
	    }
	    
	    req(input$genreInput)
	    if (input$genreInput != "All"){
	      lineGraph <- lineGraph %>% filter(listed_in == input$genreInput)
	    }
			
	    req(input$yearInput[1], input$yearInput[2])
      lineGraph <- lineGraph %>% filter(release_year >= input$yearInput[1])
      lineGraph <- lineGraph %>% filter(release_year <= input$yearInput[2])
			
      lineGraph <- lineGraph %>% count(release_year, continent)	
      lineGraph <- ggplot(data = lineGraph, aes(x=release_year, y=n, group=continent, text=paste("Continent: ", continent,
                                                                                                 "<br>Release Year: ", release_year,
                                                                                                 "<br>Count: ", n))) + 
        geom_line(aes(color = continent)) + 
       labs(x = "Release Year", y = "Number of Contents") + 
       ggtitle("Number of Contents Over the Years") + 
       theme(plot.title = element_text(hjust = 0.5), legend.title=element_blank())
		 	
		  ggplotly(lineGraph, height=450, tooltip=c("text")) %>% layout(margin=list(l = 50, r = 50, b = 20, t = 50))	
	  } else if (values$data == 5) {
		  tableResult <- netflixListedIn
		 
		  req(input$ageGroupInput)
	    if (input$ageGroupInput != "All") {
		   tableResult <- tableResult %>% filter(age_group == input$ageGroupInput)
	    }
		  
		  req(input$typeInput)
		  if (input$typeInput == "Movie") {
		    tableResult <- tableResult %>% filter(type == "Movie")
		  } else {
		    tableResult <- tableResult %>% filter(type == "TV Show")
		  }
		 
		  req(input$genreInput)
		  if (input$genreInput != "All") {
		   tableResult <- tableResult %>% filter(listed_in == input$genreInput)
		  }
		 
		  tableResult <- tableResult %>% filter(release_year >= input$yearInput[1])
		  tableResult <- tableResult %>% filter(release_year <= input$yearInput[2])
		
		  req(input$countryInput)
  	  if (input$countryInput != "All") {
  		 tableResult <- tableResult %>% filter(country == input$countryInput)
  	  }
		
		  tableResult <- unique(select(tableResult, title, description))
		
		  table <- plot_ly(type="table", height=450,
		                 header=list(
		                   values=c("<b>Title</b>", "<b>Description</b>"),
		                   line=list(color = '#506784'),
						   fill = list(color = c("#DD4B39", "#DD4B39")),
						   font = list(family = "Arial", size = 14, color = "white")
		                 ), 
		                 cells=list(
		                   values=unname(tableResult),
		                   align=c("center", "left"),
		                   height=30
		                )) %>% layout(margin=list(l = 50, r = 50, b = 20, t = 50))
		  ggplotly(table)
	  }
  }) 
	
  # Search table
  output$netflixTable <- DT::renderDataTable(
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