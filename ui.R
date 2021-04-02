library(shinythemes)
library(shinycssloaders)
library(shinydashboard)
library(plotly)
library(rintrojs)
library(shinyLP)
library(shinyjs)

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
        h2("One of the Most Popular Content Platform in the World", style="text-align: center;"),
        br(),
		fluidPage(
		tags$video(src = "video.mp4", type = "video/mp4", autoplay = TRUE, controls = TRUE, width = "1225px"),
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
		  shinyjs::useShinyjs(),
		  fluidRow(
			withSpinner(plotlyOutput(outputId="graph"), color='red')
		  ),
		  br(),
		  fluidRow(
			box(title = "Filter", width = NULL, status = 'warning', 
			uiOutput("filter"),
			)
		  ),
		  fluidRow(
			tags$div(class = "wrap",
				div(class = 'center', style="display: inline-block;vertical-align:top; width: 622px;",actionButton("btnB", label = "Back", width= "612px", height= "40px", style="color: #fff; background-color: #DD4B39; border-color: #ac0e28")),
				div(class = 'center', style="display: inline-block;vertical-align:top; width: 622px;",actionButton("btnN", label = "Next", width= "612px", height= "40px", style="color: #fff; background-color: #DD4B39; border-color: #ac0e28"))
			)
		  )
		)),
		tabItem(tabName="search", withSpinner(dataTableOutput(outputId="netflixTable"), color="red")))
  )
))