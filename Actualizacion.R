library(shiny)
library(report)

ui <- fluidPage(
  titlePanel("Análisis de Peso Neto en Adultos Mayores"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("archivo", "Sube tu archivo CSV", accept = c(".csv")),
      selectInput("analisis", "Selecciona el tipo de análisis:",
                  choices = c("Prueba t", "ANOVA")),
      helpText("Variables esperadas: Peso, Edad y Sexo.")
    ),
    
    mainPanel(
      uiOutput("salidaUI")  
    )
  )
)

server <- function(input, output) {
  
  
  datos <- reactive({
    req(input$archivo)
    read.csv(input$archivo$datapath, stringsAsFactors = TRUE)
  })
  
  
  output$salidaUI <- renderUI({
    req(input$archivo)
    tagList(
      h4("Resumen estadístico"),
      verbatimTextOutput("resumen"),
      h4("Resultado del análisis"),
      verbatimTextOutput("resultado"),
      h4("Gráfico"),
      plotOutput("grafico"),
      h4("Interpretación con 'report'"),
      verbatimTextOutput("interpretacion")
    )
  })
  
  
  output$resumen <- renderPrint({
    req(datos())
    summary(datos())
  })
  
  
  output$resultado <- renderPrint({
    req(datos())
    data <- datos()
    
    if (input$analisis == "Prueba t") {
      t.test(Peso ~ Sexo, data = data)
    } else {
      data$EdadRango <- cut(data$Edad, breaks = c(59, 69, 79, 99),
                            labels = c("60-69", "70-79", "80+"))
      summary(aov(Peso ~ EdadRango, data = data))
    }
  })
  
  
  output$grafico <- renderPlot({
    req(datos())
    data <- datos()
    
    if (input$analisis == "Prueba t") {
      boxplot(Peso ~ Sexo, data = data, col = c("#FFA07A", "#87CEFA"),
              main = "Peso por Sexo", ylab = "Peso Neto (kg)")
    } else {
      data$EdadRango <- cut(data$Edad, breaks = c(59, 69, 79, 99),
                            labels = c("60-69", "70-79", "80+"))
      boxplot(Peso ~ EdadRango, data = data, col = "#90EE90",
              main = "Peso por Rangos de Edad", ylab = "Peso Neto (kg)")
    }
  })
  
  
  output$interpretacion <- renderPrint({
    req(datos())
    data <- datos()
    
    if (input$analisis == "Prueba t") {
      modelo <- t.test(Peso ~ Sexo, data = data)
    } else {
      data$EdadRango <- cut(data$Edad, breaks = c(59, 69, 79, 99),
                            labels = c("60-69", "70-79", "80+"))
      modelo <- aov(Peso ~ EdadRango, data = data)
    }
    
    cat(report(modelo))
  })
}

shinyApp(ui = ui, server = server)

