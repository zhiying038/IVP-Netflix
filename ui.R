library(shinythemes)
library(shinycssloaders)
library(shinydashboard)

###########
# LOAD UI #
###########

shinyUI(dashboardPage(
  skin="red",
  dashboardHeader(title="Netflix Movies and TV Shows", titleWidth=300),
  dashboardSidebar(
    width=300,
    sidebarMenu(
      menuItem("Overview", tabName="overview", icon=icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName="overview",
        HTML('<center><img src="netflix-logo.png" width="400"></center>'),
        br(),
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
          column(6, plotOutput(outputId="topCountryBar")),
          column(6, plotOutput(outputId="contentGrowth"))
        )
      )
    )
  )
))