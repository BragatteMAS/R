library(shiny)

ui <- fluidPage(
  titlePanel("Aplicativo Shiny Simples"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("n", "Número de pontos:", min = 1, max = 100, value = 50)
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {
  output$plot <- renderPlot({
    plot(runif(input$n), main = "Gráfico Aleatório")
  })
}

shinyApp(ui = ui, server = server)
