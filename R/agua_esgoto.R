#
# Painel Investimento Água Esgoto
#

moduloDemografico <- function(id) {
  ns <- NS(id)
  sidebarLayout(sidebarPanel(titlePanel("Opções"),
                             fluidRow(
                               sliderInput(
                                 ns("ano"),
                                 "size",
                                 min = 1,
                                 max = 100,
                                 value = 10
                               )
                             )),
                mainPanel(plotOutput(ns("plot1"))),)
}

getData <- function(n) {
  1:n
}

aguaEsgotoServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$plot1 <- renderPlot({
      plot(getData(input$ano))
    })
  })

}
