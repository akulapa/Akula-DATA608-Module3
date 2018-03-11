#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(RColorBrewer)


State.Names<-data.frame(State=state.abb,StateName=state.name, stringsAsFactors = F)
cdc.df <- read.csv("cleaned-cdc-mortality-1999-2010-2.csv", header= TRUE, stringsAsFactors = F)
colnames(cdc.df)[1] <- "Cause"
colnames(cdc.df)[6] <- "Mortality"
cdc.df <- cdc.df %>% left_join(State.Names) %>% select(Cause, StateName, Year, Deaths, Population, Mortality)
colnames(cdc.df)[2] <- "State"
cdc.df <- cdc.df %>%
  mutate(State = if_else(is.na(State), 'Washington DC', State))

cdc.df <- data.frame(cdc.df)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$yearOutput <- renderUI({
    selectInput("yearInput", "Year",
                sort(unique(cdc.df$Year)),
                selected = "2010")
  })

  output$stateOutput <- renderUI({
    selectInput("stateInput", "State",
              sort(unique(cdc.df$State)),
              selected = "AK")
  })

  output$causeOutput <- renderUI({
    selectInput("causeInput", "Cause",
                sort(unique(cdc.df$Cause)),
                selected = "Certain infectious and parasitic diseases")
  })
  
  filtered <- reactive({
    if (is.null(input$yearInput)) {
      return(NULL)
    }
    if (is.null(input$causeInput)) {
      return(NULL)
    }
    if (is.null(input$rankInput)) {
      return(NULL)
    }
    if (is.null(input$orderInput)) {
      return(NULL)
    }

    if (input$rankInput=='Min. Rank'){
      if (input$orderInput=="Low To High"){
        cdc.df %>%
          filter(Year == input$yearInput,
                 Cause == input$causeInput) %>% 
          arrange(State) %>% 
          mutate(Rank = min_rank(Mortality)) %>% 
          select(Rank, Cause, State, Year, Deaths, Population, Mortality)
      }
      else {
        cdc.df %>%
          filter(Year == input$yearInput,
                 Cause == input$causeInput) %>% 
          arrange(State) %>% 
          mutate(Rank = min_rank(desc(Mortality))) %>% 
          select(Rank, Cause, State, Year, Deaths, Population, Mortality)
      }
    }
    else {
      if (input$orderInput=="Low To High"){
        cdc.df %>%
          filter(Year == input$yearInput,
                 Cause == input$causeInput) %>% 
          arrange(State) %>% 
          mutate(Rank = dense_rank(Mortality)) %>% 
          select(Rank, Cause, State, Year, Deaths, Population, Mortality)
      }
      else {
        cdc.df %>%
          filter(Year == input$yearInput,
                 Cause == input$causeInput) %>% 
          arrange(State) %>% 
          mutate(Rank = dense_rank(desc(Mortality))) %>% 
          select(Rank, Cause, State, Year, Deaths, Population, Mortality)
      }
    }
  })
  
  output$plot1 <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    if (nrow(filtered())==0) {
      return()
    }
    
    g <- ggplot(filtered(), aes(x=reorder(State, State), y=Mortality)) +
      geom_bar(stat="identity", fill="steelblue")+
      geom_text(aes(label=Rank), vjust=1.6, color="white", size=2.5) +
      geom_text(aes(label=Mortality), vjust=-0.5, color="red", size=3.5) +
      labs(title=paste0("Crude Mortality Rate Accross All States - ", input$yearInput), 
           subtitle=paste0("Cause: ",input$causeInput, " - ", input$orderInput),
           caption="Source: CDC WONDER",
           x="State",
           y="Mortality Rate Per 100,000 Population") +
      theme(axis.text.x = element_text(size  = 10, angle = 45, hjust = 1, vjust = 1))
    g
  })


  output$plot3 <- renderPlotly({
    m <- list(b=120, t=50) # l = left; r = right; t = top; b = bottom
    
    p <- plot_ly(filtered(), x = ~reorder(State, State), y = ~Mortality, type = 'bar', colors = "steelblue", hoverinfo = 'text', text = ~paste('State: ', State, 
                                                                                                                  '<br> Mortality: ', Mortality, 
                                                                                                                  '<br> Rank: ', Rank)) %>% 
            layout(title = paste0("Crude Mortality Rate Accross All States - ", input$yearInput,'<br>','Cause: ',input$causeInput),
                   xaxis = list(title = paste("State - ", input$orderInput), showgrid = TRUE, tickangle = -45, tickfont=list(size=8)),
                   yaxis = list(title = "Mortality Rate Per 100,000 Population", showgrid = TRUE),
                   margin=m, titlefont=list(size=11))
    p
  })
  
  
  output$plot4 <- renderPlotly({
    m <- list(b=120, t=50) # l = left; r = right; t = top; b = bottom
    p <- plot_ly(filtered(), x = ~State, y = ~Mortality, type = 'scatter', mode = 'markers', color = ~Rank, colors = brewer.pal(11, "Dark2"),
               marker = list(size = 10, opacity = 0.5), hoverinfo = 'text', text = ~paste('State: ', State, 
                                                                                          '<br> Mortality: ', Mortality, 
                                                                                          '<br> Rank: ', Rank), name=" ")
    
    p <- p %>%
      layout(title = paste0("Crude Mortality Rate Accross All States - ", input$yearInput,'<br>','Cause: ',input$causeInput),
             xaxis = list(title = paste("State - ", input$orderInput), showgrid = TRUE, tickangle = -45, tickfont=list(size=8)),
             yaxis = list(title = "Mortality Rate Per 100,000 Population", showgrid = TRUE),
             margin=m, titlefont=list(size=11))
  
    p
  })
  
  output$results <- renderDataTable({
    if (is.null(filtered())) {
      return()
    }
    datatable(filtered(), class = 'cell-border stripe', options = list(searching = FALSE), rownames= FALSE)
    
  })
  
})
