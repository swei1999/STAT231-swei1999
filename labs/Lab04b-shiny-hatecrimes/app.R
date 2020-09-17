library(fivethirtyeight)
library(shinythemes)
library(tidyverse)

# get "division" info (New England, Pacific, South Atlantic, etc.) for each state 
hate_crimes2 <- hate_crimes %>%
  left_join(fivethirtyeight::state_info, by = c("state", "state_abbrev"))

# define vectors for choice values and labels 
# can then refer to them in server as well (not just in defining widgets)
# for selectInput, needs to be named list
x_choices <- as.list(names(hate_crimes2)[3:11])
x_choice_names <- c("Median household income, 2016"
                    , "Share of the pop that in unemployed, 9/2016"
                    , "Share of the pop that lives in metropolitan areas, 2015"
                    , "Share of adults 25+ with a high-school degree, 2009"
                    , "Share of the pop that are not US citizens, 2015"
                    , "Share of white residents who are living in poverty, 2015"
                    , "Gini index, 2015"
                    , "Share of the pop that is not white, 2015"
                    , "Share of 2016 US presidential votes who voted for Donald Trump")
names(x_choices) <- x_choice_names

# for radio button, can be separate (have choiceValues and choiceNames options, 
# rather than just choices)
y_choice_values = names(hate_crimes2)[12:13]
y_choice_names <- c("Hate crimes per 100,000 pop, SPLC, Nov 9-18,2016"
                    , "Average annual hate crimes per 100,000 population, FBI, 2010-2015")

# for checkboxGroupInput (only have "choices" option, but these labels are fine)
div_choices <- (hate_crimes2 %>%
    count(division))$division

# for selectInput choices, needs to be named list
st_choices <- as.list(c("None", hate_crimes2$state_abbrev))
names(st_choices) <- c("None", hate_crimes2$state)
 
 
# ui 
ui <- fluidPage(
  
  h1("Hate Crimes in the United States"),
  
  sidebarLayout(
    sidebarPanel(
      
       selectInput(inputId = "x"
                  , label = "Choose a predictor variable of interest:"
                  , choices = x_choices
                  , selected = "gini_index"),
      radioButtons(inputId = "y"
                   , label = "Choose an outcome variable of interest:"
                   , choiceValues = y_choice_values
                   , choiceNames = y_choice_names
                   , selected = "hate_crimes_per_100k_splc"),
      checkboxGroupInput(inputId = "div"
                    , label = "Include divisions:"
                    , choices = div_choices
                    , selected = div_choices
                    , inline = TRUE),
      selectInput(inputId = "id_st"
                  , label = "Identify a state in the scatterplot:"
                  , choices = st_choices
                  , selected = "MA")
    ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs"
                  , tabPanel("Histogram of the outcome"
                             , plotOutput(outputId = "hist"))
                  , tabPanel("Scatterplot", plotOutput(outputId = "scatter"))
                  , tabPanel("Table", tableOutput(outputId = "table"))
      )
    )
  )
)

# server
server <- function(input,output){
  
  use_data <- reactive({
    data <- filter(hate_crimes2, division %in% input$div)
  })
    
  output$hist <- renderPlot({
    ggplot(data = use_data(), aes_string(x = input$y)) +
      geom_histogram(color = "#00215c", fill = "#00215c", alpha = 0.7) +
      labs(x = y_choice_names[y_choice_values == input$y]
           , y = "Number of States")
  })
  
  output$scatter <- renderPlot({
    ggplot(data = use_data(), aes_string(x = input$x, y = input$y)) +
        geom_point() +
        labs(x = names(x_choices)[x_choices == input$x]
             , y = y_choice_names[y_choice_values == input$y]) +
        geom_label(data = filter(hate_crimes2, state_abbrev == input$id_st)
                   , aes(label = state_abbrev))
  })
  
  output$table <- renderTable({
    dplyr::select(use_data(), state, input$x, input$y)
  })
}

# call to shinyApp
shinyApp(ui = ui, server = server)


# Your turn.  Copy this code as a template into a new app.R file (WITHIN A FOLDER
# named something different than your other Shiny app folders).  Then, either 
# (1) update this template to still explore the hate_crimes dataset, but with
#     different app functionality (e.g. different widgets, variables, layout, theme...); 
#   OR
# (2) use this as a template to create a Shiny app for a different dataset:
#      either mad_men (tv performers and their post-show career), 
#             ncaa_w_bball_tourney (women's NCAA div 1 basketball tournament, 1982-2018), 
#             nfl_suspensions (NFL suspensions, 1946-2014), 
#             or candy_rankings (candy characteristics and popularity)
#      these four datasets are also part of the fivethirtyeight package
#      and their variable definitions are included in pdfs posted to the Moodle course page
