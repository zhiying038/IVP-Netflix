library(shinythemes)
library(shinycssloaders)
library(shinydashboard)
library(plotly)
library(rintrojs)
library(shinyLP)
library(shinyjs)
library(shinyWidgets)

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
        h2("One of the Most Popular Content Platforms in the World", style="text-align: center;"),
        br(),
		fluidPage(
		  HTML('<center><iframe width="560" height="315" src="https://www.youtube.com/embed/XL6zNexyt8o" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; 
		       gyroscope; picture-in-picture" allowfullscreen></iframe></center>')
		),
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
		  tabName="recommendation",
		  fluidPage(
		    introjsUI(),
        tags$div(actionButton("help", "Take a Quick Tour",icon("question-circle"), style="color: #fff; 
                              background-color: #DD4B39")),
		    br(),
  		  shinyjs::useShinyjs(),
  		  fluidRow(
  		    column(12, 
  		      introBox(box(width = NULL, status="danger", uiOutput("filter"), textOutput(outputId="typeSelection")),
			      data.step = 1, 
            data.intro = "Start by selecting your desired choice from the options provided.")
  		    )
		  ),
		  br(),
		  fluidRow(
		    introBox(column(12, withSpinner(plotlyOutput(outputId="graph"), color='red')),
			  data.step = 2, 
        data.intro = "Your selection will reflect in the graph which shows you the movies or TV shows that may suit you.")
		  ),
		  fluidRow(
		    column(12,
		    tags$div(style="margin-top: 6em; text-align: center;",
		    introBox(actionGroupButtons(inputIds=c("btnB", "btnN"), labels=list("Previous", "Next"), status="danger", size="lg", fullwidth=TRUE),
			  data.step = 3, 
        data.intro = "Confirm your selection by clicking 'Next'. Go back a step to edit your selection by clicking 'Previous'.")
			))
		))),
		tabItem(tabName="search", withSpinner(DT::dataTableOutput(outputId="netflixTable"), color="red")))
  )
))