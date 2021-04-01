library(shinythemes)
library(shinycssloaders)
library(shinydashboard)
library(plotly)

###########
# LOAD UI #
###########

shinyUI(dashboardPage(
  skin="red",
  dashboardHeader(title="Netflix Movies and TV Shows", titleWidth=300),
  dashboardSidebar(
    width=300,
    sidebarMenu(
      menuItem("Overview", tabName="overview", icon=icon("chart-line")),
  	  menuItem("Movies/TV Shows", tabName="movies", icon=icon("video")),
  	  menuItem("Rating", tabName="rating", icon=icon("star-half-alt")),
  	  menuItem("Genre", tabName="genre", icon=icon("th-list")),
      menuItem("Recommendation", tabName="recommendation", icon=icon("heart")),
      menuItem("All Movies/TV Shows", tabName="search", icon=icon("search"))
    )
  ),
  dashboardBody(
	  tags$head(tags$link(rel = "shortcut icon", href = "favicon.jpg")),
    tabItems(
      tabItem(
        tabName="overview",
        HTML('<center><img src="netflix-logo.png" width="400"></center>'),
        br(),
        h2("One of the Most Popular Content Platform in the World", style="text-align: center;"),
        br(),
        fluidRow(
          withSpinner(infoBoxOutput("totalMoviesBox"), color="red"),
          withSpinner(infoBoxOutput("totalShowsBox"), color="red"),
          withSpinner(infoBoxOutput("totalContentBox"), color="red")
        ),
		    br(),
        fluidRow(
          withSpinner(infoBoxOutput("totalCountries"), color="red"),
          withSpinner(infoBoxOutput("totalContinent"), color="red"),
		      withSpinner(infoBoxOutput("totalYear"), color="red")
        ),
        br(),
        fluidRow(
          column(6, plotlyOutput(outputId="topCountryBar")),
          column(6, plotlyOutput(outputId="contentGrowth"))
        )
      ),
	  tabItem(
		tabName="movies",
		fluidRow(
      column(7, withSpinner(plotlyOutput(outputId="mapMovie"),color = 'red')),
		  column(5,
			  box(title = "Filter", width = NULL, status = 'warning',
  				radioButtons("typeInput", "Type", choices = c("Movie", "TV Show"), selected = "Movie"),
  				uiOutput("countryOutput"),
  				uiOutput("genreOutput"),
  				sliderInput("yearInput", "Release Year", 1925, 2021, c(1925, 2021))))
      ),
		  br(),
      fluidRow(
        column(12, withSpinner(dataTableOutput("movieResults"), color="red"))
      )
	  ),
	  tabItem(
	    tabName="rating",
  		fluidRow(
  		  column(3,
  			box(title = "Filter", width = NULL, status = 'warning',
  				uiOutput("continentOutput"),
  				uiOutput("ratingOutput")))
  		)
  	),
		tabItem(
		  tabName="recommendation",
		  fluidRow(
		    column(7, withSpinner(plotlyOutput(outputId="genresBarChart"), color='red')),
		    column(5,
		           box(title = "Filter", width = NULL, status = 'warning',
		               uiOutput("ageGroupOutput")
		  )))
		),
		tabItem(tabName="search", withSpinner(dataTableOutput(outputId="netflixTable"), color="red")))
  )
))