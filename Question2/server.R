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

National.Avg <- cdc.df %>%
  group_by(Cause, Year) %>%
  summarize(Mortality = round(sum(Deaths) * 100000/ sum(Population),1))

National.Avg$State = 'National Avg.'
National.Avg$Order = 0

National.Avg <- National.Avg %>% select(Cause, Year, State, Mortality, Order)
National.Avg <- data.frame(National.Avg)

States.df <- cdc.df %>% mutate(Order = 1) %>% select(Cause, Year, State, Mortality, Order)

Years.df <- rbind(National.Avg, States.df)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$causeOutput <- renderUI({
    selectInput("causeInput", "Cause",
                sort(unique(cdc.df$Cause)),
                selected = "Certain infectious and parasitic diseases")
  })

  output$statesOutput <- renderUI({selectInput('statesInput', 'State', sort(unique(cdc.df$State)), multiple=TRUE, selectize=FALSE) })
  
  
  filtered <- reactive({
    if (is.null(input$causeInput)) {
      return(NULL)
    }
    if (is.null(input$statesInput)) {
      return(NULL)
    }

    Years.df %>%
      filter(State %in% c(input$statesInput,'National Avg.'),
             Cause == input$causeInput) %>% 
      arrange(Order, State) %>% 
      select(Cause, Year, State, Mortality)
    
  })

  filteredY <- reactive({
    Year.sp <- data.frame(filtered())
    State.Years.Avg <- Year.sp %>% spread(State, Mortality)
    State.Years.Avg <- State.Years.Avg %>% arrange(Year)
  })
  
  output$plot1 <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    if (nrow(filtered())==0) {
      return()
    }
    
    g <- ggplot(filtered(), aes(x=Year, y=Mortality, group=State, color=State)) +
          geom_line() + geom_point() +
          labs(title=paste0("Mortality Rate States Vs. National Average"), 
            subtitle=paste0("Cause: ",input$causeInput),
            caption="Source: CDC WONDER",
            x="Year",
            y="Mortality Rate Per 100,000 Population") +
        theme_minimal()
    g
  })
  
  output$plot3 <- renderPlotly({
    if (is.null(filtered())) {
      return()
    }
    if (nrow(filtered())==0) {
      return()
    }
    
    p <- plot_ly(source = "source") %>% 
      add_lines(data = filtered(), x=~Year, y=~Mortality, color=~State, mode="lines+markers",
                hoverinfo = 'text', text = ~ifelse(State != 'National Avg.',
                                                    paste('Year: ',Year,'<br>State: ', State,'<br>Mortality: ', Mortality), 
                                                    paste('Year: ',Year,'<br>',State, '<br>Mortality: ', Mortality))) %>% 
     layout(title = paste("Mortality Rate States Vs. National Average",'<br>','Cause: ',input$causeInput),
            xaxis = list(title = "Years", showgrid = TRUE, tickangle = 0, tickfont=list(size=9)),
            yaxis = list(title = "Mortality Rate Per 100,000 Population", showgrid = TRUE), titlefont=list(size=11))
    
    
    #p <- plot_ly(filtered(), x=~Year, y=~Mortality, type="scatter",color=~State, mode="lines+markers")
    p
  })
  
  output$plot4 <- renderPlotly({
    if (is.null(filtered())) {
      return()
    }
    if (nrow(filtered())==0) {
      return()
    }
    
    p <- plot_ly(source = "source") %>% 
      add_trace(data = filtered(), x=~Year, y=~Mortality, color=~State, mode="lines+markers",
                hoverinfo = 'text', text = ~ifelse(State != 'National Avg.',                                                                   
                                                   paste('Year: ',Year,'<br>State: ', State,'<br>Mortality: ', Mortality),
                                                   paste('Year: ',Year,'<br>',State, '<br>Mortality: ', Mortality))) %>% 
      layout(title = paste("Mortality Rate States Vs. National Average",'<br>','Cause: ',input$causeInput),
             xaxis = list(title = "Years", showgrid = TRUE, tickangle = 0, tickfont=list(size=9)),
             yaxis = list(title = "Mortality Rate Per 100,000 Population", showgrid = TRUE), titlefont=list(size=11))
    
    p
  })

  output$results <- renderDataTable({
    if (is.null(filtered())) {
      return()
    }
    datatable(filteredY(), class = 'cell-border stripe', options = list(searching = FALSE), rownames= FALSE)
    
  })
  
})
