# Sean Wei - PS 3A

# Instructions
# - copy the code from the `02-two-outputs.R` file (in the `two-outputs` code chunk below)
# - add a `textInput` widget that allows the user to change the title of the histogram 
# (following code in `01-two-inputs.R`).  Update the code in the server() function appropriately.  
# Run the app to make sure it works as you expect.
# - update the layout of the app to use a `navlistPanel` structure 
# (following the code in `06-navlist.R`).  Hint: put `navlistPanel` around the output objects only.

library(shiny)

ui <- fluidPage(
  sliderInput(inputId = "num", 
              label = "Choose a number", 
              value = 25, min = 1, max = 100),
  textInput(inputId = "title", 
            label = "Write a Title",
            value = "Histogram of Random Normal Values"),
  navlistPanel(
    tabPanel(title = "Histogram",
             plotOutput("hist")
    ),
    tabPanel(title = "Summary", 
             verbatimTextOutput("stats")
    )
  )
)

server <- function(input, output) {
  output$hist <- renderPlot({
    hist(rnorm(input$num), main=input$title, xlab = "Normal Values")
  })
  output$stats <- renderPrint({
    summary(rnorm(input$num))
  })
}

shinyApp(ui = ui, server = server)