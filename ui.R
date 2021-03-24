library(shinythemes)
library(shinycssloaders)

###########
# LOAD UI #
###########

shinyUI(fluidPage(
  navbarPage("Netflix Movies and TV Shows", theme=shinytheme("simplex"),
             tabPanel("Overview", fluid=TRUE, icon=icon("globe-americas"),
                      fluidRow(
                        column(6, withSpinner(plotOutput(outputId="topCountryBar"), color="red")),
                        column(6, withSpinner(plotOutput(outputId="contentGrowth"), color="red"))
                      ))
  
)))