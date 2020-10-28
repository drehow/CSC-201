#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
data("USArrests")
USArrests$State <- row.names(USArrests)
vars <- setdiff(names(USArrests), "State")
library(shiny)

# Define UI for application that draws a histogram
shinyUI(pageWithSidebar(
    headerPanel('k-means clustering'),
    sidebarPanel(
        selectInput('xcol', 'X Variable', vars),
        selectInput('ycol', 'Y Variable', vars, selected = vars[[2]]),
        numericInput('clusters', 'Cluster count', 3, min = 1, max = 9)
    ),
    mainPanel(
        plotOutput('plot1')
    )
))
