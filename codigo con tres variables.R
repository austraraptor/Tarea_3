#Actitudad 3
#Con el fin de realizar esta actividad se escogió una base de datos de una encuesta de vigilancia realizada entre los
#años 2017-2018 el cual se trata de un seguimiento de la nutrición entre adolescentes y adultos mayores.
#Pero para la relizacion de esta actrividad solo se trabajara con 30 adultos mayores hombres y mujeres de entre 60 a 97 años siendo el peso la variable cuantitativa dependiente, edad variable cuantitatva continua y el sexo la variable categorica
#El objetivo identificar diferencias internas conforme al sexo y explorar posibles relaciones en funcion a la edad

#en los cuales se utilizaran la prueba T y ANOVA. el primero se usara para ver si existe una diferencia de peso entre hombre y mujeres
#y agora se usara para se vera si se presentan variaciones de peso significativas a medida que avanza la edad
library(shiny)

#prueba de hipotesis
#HO<-A=B
#HA<-A≠B
#Datos
#peso <- c(47.2,63.1,73.7,86.5,58.4,55.3,60.7,90.9,49.7,90.9,77.9,56.5,80.8,65,68.5,39.1,50.9,56.9,73.4,67.2,59.1,54.7,59.4,0,60.4,79.8,67.3,64.8,74.3,49.4)
#edad <- c(73,71,67,64,60,72,79,64,68,65,71,66,61,70,68,77,64,62,64,60,63,97,63,65,81,72,71,60,77,64)
#sexo <- c("Mujer","Hombre","Mujer","Hombre","Hombre","Mujer","Mujer","Hombre","Mujer","Hombre",
          #"Hombre","Mujer","Hombre","Hombre","Hombre","Mujer","Hombre","Hombre","Hombre","Hombre",
          #"Hombre","Mujer","Hombre","Mujer","Hombre","Hombre","Mujer","Mujer","Mujer","Mujer")

#grado de significancia
#α=05

#codigo
ui <- fluidPage(
  titlePanel("Análisis de Peso Neto en Adultos Mayores"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("archivo", "Sube tu archivo CSV",
                accept = c(".csv")),
      selectInput("analisis", "Selecciona el tipo de análisis:",
                  choices = c("Prueba t", "ANOVA")),
      helpText("variables: Peso, Edad y Sexo.")
    ),
    mainPanel(
      verbatimTextOutput("resultado"),
      plotOutput("grafico")
    )
  )
)

server <- function(input, output) {
  
  datos <- reactive({
    req(input$archivo)
    read.csv(input$archivo$datapath, stringsAsFactors = TRUE)
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
              main = "Peso", ylab = "Peso Neto (kg)")
    } else {
      data$EdadRango <- cut(data$Edad, breaks = c(59, 69, 79, 99),
                            labels = c("60-69", "70-79", "80+"))
      boxplot(Peso ~ EdadRango, data = data, col = "#90EE90",
              main = "Peso por Rangos de Edad", ylab = "Peso Neto (kg)")
    }
  })
}

shinyApp(ui = ui, server = server)

#interpretacion
