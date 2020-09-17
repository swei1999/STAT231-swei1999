#Sean Wei, PS 3B

library(fivethirtyeight)
library(shinythemes)
library(shinybusy)
library(tidyverse)
library(ggrepel)
library(kableExtra)
library(dplyr)
library(tidyr)
library(stringr)
#Convert data to tidy for later
candy_rankings_tidy <- candy_rankings %>%
  pivot_longer(-c(competitorname, sugarpercent, pricepercent, winpercent), 
               names_to = "characteristics", values_to = "present") %>%
  mutate(present = as.logical(present)) %>%
  arrange(competitorname)

data(candy_rankings)

# Define Predictor Variable Choices
predictors <- as.list(names(candy_rankings)[11:12])
predictor_names <- c("Sugar Percent"
                    , "Price Percent")
names(predictors) <- predictor_names

# ui 
ui <- fluidPage(
  
  # CSS
  tags$head(
    tags$style(HTML("
      h1, h3 {
        color: orange;
        text-align: center;
      }
    "))
  ),
  
  HTML("<h1>Halloween Candy Power Rankings</h1>
        <h3>by Sean Wei</h3>"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput(inputId = "predictor", 
                  label = "Choose a predictor variable of interest for the scatterplot:",
                  choices = predictors, 
                  selected = "Sugar Percent")
    ),
    
    mainPanel(
      #let users know things might take a while to load
      add_busy_spinner(spin = "double-bounce"),
      tabsetPanel(type = "tabs", 
          tabPanel("Scatterplot", 
            HTML("<p>The scatterplot below is interactive - try hovering over the points to see the candy brand!</p>"), 
            plotOutput(outputId = "scatter", hover = hoverOpts(id = "plot_hover", delay = 0))),
          
          tabPanel("Rankings", 
            HTML("<p>This table explains how often a candy of a given type won its matchups against other brands."), 
            tableOutput(outputId = "table")),
          
          tabPanel("Candy Characteristics", 
            HTML("<p>Curious to see the different possible characteristics of your favorite candy? Use this table to explore your favorites.</p>"),
            fluidRow(
              selectInput(
                inputId = "characteristic",
                label = "Characteristic",
                choices = c(
                  "ALL",
                  unique(candy_rankings_tidy$characteristics)
                ),
                selected = "chocolate"
              ),
              fluidRow(
                DT::dataTableOutput("table2")
              )
            )
          )
        )
    )
  ),
  uiOutput("dynamic")
  
)

# server
server <- function(input, output){
  
  output$scatter <- renderPlot({
    ggplot(data = candy_rankings, aes_string(x = input$predictor, y = "winpercent")) +
      geom_point() + 
      labs(x = names(predictors)[predictors == input$predictor]
           , y = "Win Percentage") + 
      ggtitle("Win Percentage Based on Sugar % or Price %")
  })
  
  output$dynamic <- renderUI({
    req(input$plot_hover) 
    textOutput("vals")
  })
  
  output$vals <- renderPrint({
    hover <- input$plot_hover 
    y <- nearPoints(candy_rankings, input$plot_hover)["competitorname"]
    req(nrow(y) != 0)
    print(y$competitorname)
  })
  
  output$table <- function() {
    candy_rankings %>%
      select(competitorname, winpercent) %>%
      arrange(desc(winpercent)) %>%
      rename(Candy = competitorname, `Win Percentage` = winpercent) %>%
      knitr::kable("html") %>%
      kable_styling(bootstrap_options = "striped")
  }
  
  datasetcandy <- reactive({  # Filter data based on selections
    data <- candy_rankings_tidy %>%
      select(-c(winpercent, sugarpercent, pricepercent)) %>%
      filter(present == TRUE)
    req(input$characteristic) # wait until there's a selection
    #If there is a filter
    if (input$characteristic != "ALL") {
      data <- data %>%
        filter(characteristics == input$characteristic)
    }
    #If selection is ALL
    else {
      data <- data
    }
  })
  
  output$table2 <- DT::renderDataTable(DT::datatable(datasetcandy()))
  
}

# call to shinyApp
shinyApp(ui = ui, server = server)
