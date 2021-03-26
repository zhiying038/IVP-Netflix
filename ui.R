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
      menuItem("Search", tabName="search", icon=icon("search"))
    )
  ),
  dashboardBody(
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
          withSpinner(infoBoxOutput("totalContinent"), color="red")
        ),
        br(),
        fluidRow(
          column(6, plotlyOutput(outputId="topCountryBar")),
          column(6, plotlyOutput(outputId="contentGrowth"))
        ),
		    br(),
		    br(),
		    fluidRow(
		      column(12, plotlyOutput(outputId="mapCountry"))
		    )
      ),
      tabItem(tabName="search", withSpinner(dataTableOutput(outputId="netflixTable"), color="red"))
    )
  )
))