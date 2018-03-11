#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
# - https://deanattali.com/blog/building-shiny-apps-tutorial/
# - https://shiny.rstudio.com/gallery/selectize-vs-select.html

library(shiny)
library(DT)
library(plotly)

shinyUI(fluidPage(
  titlePanel("CDC Mortality 1999 - 2010"),
      fluidRow(
        column(3, wellPanel(
          br(),
          uiOutput("causeOutput"),
          br(),
          p("Use Ctrl+Click to select more than one state"),
          br(),
          uiOutput("statesOutput"),
          br(),
          submitButton("Generate")
        )
      ),
      column(8,
             tabsetPanel(  
               tabPanel("Data",
                        dataTableOutput("results")),
               tabPanel("GGPlot - Line Graph",
                        plotOutput("plot1")),
               tabPanel("Plotly - Line Graph-1",
                        plotlyOutput("plot3")),
               tabPanel("Plotly - Line Graph-2",
                        plotlyOutput("plot4"))
             )
             
      )
    )
  )
)

