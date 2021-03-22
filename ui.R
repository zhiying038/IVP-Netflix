library(shinythemes)
library(shinycssloaders)

###########
# LOAD UI #
###########

shinyUI(fluidPage(
  navbarPage("Netflix Movies and TV Shows", theme=shinytheme("lumen"))
))