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
        h4("Netflix, one of the most popular content platform in media industry.", style="text-align: center;"),
        br(),
        fluidRow(
          infoBoxOutput("totalMoviesBox"),
          infoBoxOutput("totalShowsBox"),
          infoBoxOutput("totalContentBox")
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