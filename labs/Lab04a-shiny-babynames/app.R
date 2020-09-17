# Q: "Is there a way to change the name(s) being illustrated, 
#      either in the code or through a more interactive plot?"
# A: Yes!
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# data
library(babynames)
babynames_dat <- babynames::babynames

# create choices vector for name choices
# too many names! app will be sloooooow
#name_choices <- babynames_dat %>% 
#  count(name) %>%
#  select(name)

# subset on names of students in this course so doesn't crash ...
name_choices <- str_to_title(c("katharine"
                            # section 1
                            , "aditi", "ava", "bella", "braedon"
                            , "cat", "chris", "damien", "jack"
                            , "john", "karen", "lauren", "leah"
                            , "lillian", "matt", "michael", "mythili"
                            , "nathan", "pedro", "sam"
                            , "thai", "will", "zach"
                            # section 2
                            , "alex", "angelica", "charlie", "chris"
                            , "elizabeth", "grace", "jamie", "majd"
                            , "michael", "molly", "rodrigo", "sean"
                            , "steedman", "tamer"))
name_choices


# Define UI for application that creates a line plot for a given name
ui <- fluidPage(
   
   # Application title
   titlePanel("Babies Named Sean Over Time"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        # Select Name
        selectInput(inputId = "nm", 
                    label = "Name:",
                    choices = name_choices, 
                    selected = "Sean"),
        # Choose Sex
        radioButtons(inputId = "sx", 
                    label = "Sex:",
                    choices = c("M", "F"), 
                    selected = "M")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     
     dat <- babynames_dat %>%
       filter(name %in% input$nm & sex == input$sx) %>%
       group_by(name, year) %>%
       summarize(total = sum(n))
     
     ggplot(data = dat, aes(x = year, y = total)) +
       geom_line(color = "#0095b6") + 
       labs(x = "Year", y = "Total number of births with this name"
            , title = paste("Babies Named", paste(input$nm)))
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

