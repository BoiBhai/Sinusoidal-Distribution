#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("s",
                        "Choose s:",
                        min = 0.1,
                        max = 10,
                        value = 30),
            sliderInput("k",
                        "Choose k:",
                        min = 0.1,
                        max = 10,
                        value = 30)
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
        # generate bins based on input$bins from ui.R
        s = input$s
        k = input$k
        # draw the histogram with the specified number of bins
        curve(dsinustd(x, s,k), xlim=c(0,1))
        curve(g(x, s,k), col='red', add=T)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
