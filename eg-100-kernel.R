library(shiny)

ui <- fillPage(
  padding = 10,
  sidebarLayout(
    sidebarPanel(
      sliderInput("h", "h", 0, 10, 0.2, 0.1),
      sliderInput("s", "s", 0, 10, 1, 0.1),
      sliderInput("k", "k", 0, 10, 1, 0.1)
    ),
    mainPanel(plotOutput("p"))
  )
)

server <- function(input, output) {
  data1 <- rexp(1000)
  
  sinu.kernel <- function(data, h, s, k) {
    n <- length(data)
    a_vals <- data - h
    d_val <- 2 * h
    kernel <- function(x) {
      sum(vapply(a_vals, function(a) dsinu(x, a, d_val, s, k), numeric(1))) / n
    }
    return(Vectorize(kernel))
  }
  
  output$p <- renderPlot({
    plot(density(data1), ylim=c(0, 1))
    kde1 <- sinu.kernel(data1, input$h, input$s, input$k)
    curve(dexp, add = TRUE, col = 'green')
    curve(kde1, add = TRUE, col = "red")
  })
}

shinyApp(ui, server)