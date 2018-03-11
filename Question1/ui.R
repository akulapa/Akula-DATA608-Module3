#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
# References
# - https://deanattali.com/blog/building-shiny-apps-tutorial/
# - https://shiny.rstudio.com/gallery/selectize-vs-select.html


library(shiny)
library(DT)
library(plotly)

shinyUI(fluidPage(
  titlePanel("CDC Mortality 1999 - 2010"),
  fluidRow(
    column(3, wellPanel(
      uiOutput("yearOutput"),
      br(),
      uiOutput("causeOutput"),
      br(),
      radioButtons("rankInput", "Rank Type",
                   choices = c("Min. Rank", "Dense Rank"),
                   selected = "Min. Rank"),
      br(),
      radioButtons("orderInput", "Mortality Order",
                   choices = c("Low To High", "High To Low"),
                   selected = "High To Low")
      )
    ),
    column(8,
      tabsetPanel(  
                  tabPanel("Data",
                    dataTableOutput("results")),
                  tabPanel("GGPlot - Bar Chart",
                    plotOutput("plot1")),
                  tabPanel("Plotly - Bar Chart",
                    plotlyOutput("plot3")),
                  tabPanel("Plotly - Bubble",
                      plotlyOutput("plot4")
                  
                  )
      )
    )
  )
)

)

