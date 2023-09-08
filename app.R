library(shiny)
library(dplyr)
library(ggplot2)

# Carga los datos (reemplaza "tus_datos.csv" con el nombre de tu archivo)
datos <- read.csv("data/conurbano_sur.csv")

# Ordenar los datos por la columna "votos" de manera descendente (de mayor a menor)
datos <- datos %>% arrange(desc(votos))

ui <- fluidPage(
  # Estilo CSS personalizado
  tags$head(tags$style(
    HTML(
      "
      body {
        font-family: Arial, sans-serif;
        background-color: #f0f7ff; /* Fondo celeste claro */
        margin: 1;
        padding: 1;
      }
      .container {
        max-width: 1000px;
        margin: 0 auto;
        padding: 20px;
        background-color: #ffffff;
        border: 1px solid #ccc;
        border-radius: 5px;
        box-shadow: 0 2px 5px rgba(0, 0, 0, 0.1);
      }
      .title {
        font-size: 24px;
        font-weight: bold;
        margin-bottom: 20px;
        color: #333;
      }
      .description {
        font-size: 16px;
        margin-bottom: 20px;
        color: #666;
      }
      .content {
        padding: 20px;
      }
      .footer {
        text-align: center;
        margin-top: 20px;
        padding: 10px;
                font-size: 12px;
        border-top: 1px solid #ccc;
      }
      .input-label {
        font-weight: bold;
      }
      "
    )
  )),
  
  # Contenedor principal
  div(class = "container",
      
      # Título principal
      div(class = "title", "Monitor de Resultados Electorales PASO 2023"),
      
      # Instrucciones de uso de la app
      div(class = "content-body",
          p("El monitor de Resultados Electorales ofrece la posibilidad de realizar un análisis 
            personalizado de las Elecciones PASO 2023 en la categoría de Presidente, 
            específicamente en los circuitos y municipios del Conurbano Sur de la 
            Provincia de Buenos Aires. La información se presenta de manera gráfica 
            para facilitar la exploración y comparación de la performance de las 
            diferentes fuerzas políticas. Cada visual cuenta con una línea punteada que 
            representa los promedios generales por circuito para cada fuerza política, 
            ofreciendo una visión general del desempeño medio en cada circuito.")
      ),
      
      # Contenido de la app
      div(class = "panel-body",
          # Inputs en dos hileras por dos filas
          div(
            div(
              class = "col-md-6",
              style = "text-align: center; border-radius: 8px; font-size: 100%; padding-left: 8em;",
              div(class = "input-field", selectInput("municipio", "Seleccioná un Municipio del menú", 
                                                     choices = unique(datos$municipio),
                                                     selected = "Lanús"))
            ),
            div(
              class = "col-md-6",
              style = "text-align: center; border-radius: 8px; font-size: 100%; padding-left: 8em;",
              div(class = "input-field", selectizeInput("circuitos", "Filtrá por Circuito Electoral", choices = NULL, multiple = TRUE))
            )
          ),
          div(
            div(
              class = "col-md-6",
              style = "text-align: center; border-radius: 8px; font-size: 100%; padding-left: 8em;",
              div(class = "input-field", selectInput("agrupacion", "Elegí las agrupaciones políticas a comparar", 
                                                     choices = unique(datos$nombreAgrupacion),
                                                     selected = c("Union Por La Patria", "La Libertad Avanza"),
                                                     multiple = TRUE))
            ),
            div(
              class = "col-md-6",
              style = "text-align: center; border-radius: 8px; font-size: 100%; padding-left: 8em;",
              div(class = "input-field", uiOutput("slider_porcentaje"))
            )
          ),
          
          # Gráfico de Municipios
          div(class = "col-md-12",
              plotOutput("grafico_municipios"))
      ),
      
      # Pie de página
      div(class = "footer",
          p("Un desarrollo abierto de", a(href="https://linktr.ee/pedroorden?utm_source=linktree_admin_share","Pedro Damián Orden"), 
            ". Fuente de los datos", a(href="https://gitlab.com/lid.datos/lid-elecciones","aquí.")),
            p("Acceder al", a(href="https://github.com/pedroorden/paso2023","código de la app."))
  
  
  )
  ))




server <- function(input, output, session) {
  
  observe({
    circuitos_disponibles <- unique(datos$circuito[datos$municipio == input$municipio])
    updateSelectizeInput(session, "circuitos", choices = circuitos_disponibles, selected = circuitos_disponibles)
  })
  
  observe({
    filtro <- filter(
      datos,
      municipio == input$municipio &
        circuito %in% input$circuitos &
        nombreAgrupacion %in% input$agrupacion
    )
    
    if (!is.null(filtro$votosPorcentaje)) {
      min_porcentaje <- min(filtro$votosPorcentaje)
      max_porcentaje <- max(filtro$votosPorcentaje)
      updateSliderInput(session, "rango_porcentaje", min = min_porcentaje, max = max_porcentaje, value = c(min_porcentaje, max_porcentaje))
    }
  })
  
  output$slider_porcentaje <- renderUI({
    req(input$municipio, input$circuitos, input$agrupacion)
    
    filtro <- filter(
      datos,
      municipio == input$municipio &
        circuito %in% input$circuitos &
        nombreAgrupacion %in% input$agrupacion
    )
    
    if (!is.null(filtro$votosPorcentaje)) {
      min_porcentaje <- min(filtro$votosPorcentaje)
      max_porcentaje <- max(filtro$votosPorcentaje)
      
      sliderInput("rango_porcentaje", "Segmentá por el rango % votos:",
                  min = min_porcentaje, max = max_porcentaje, value = c(min_porcentaje, max_porcentaje))
    }
  })
  
  # ... (código anterior)
  
  output$grafico_municipios <- renderPlot({
    tryCatch({
      filtro <- filter(
        datos,
        municipio == input$municipio &
          circuito %in% input$circuitos &
          nombreAgrupacion %in% input$agrupacion &
          votosPorcentaje >= input$rango_porcentaje[1] &
          votosPorcentaje <= input$rango_porcentaje[2]
      )
      
      if (nrow(filtro) == 0) {
        # Si no hay datos válidos para facetar, muestra un mensaje informativo en lugar del gráfico
        text(0.5, 0.5, "Cargando", cex = 1.2)
      } else {
        promedios_por_agrupacion <- datos %>%
          group_by(circuito, nombreAgrupacion) %>%
          summarize(promedio = mean(votosPorcentaje))
        
        promedios_por_municipio <- datos %>%
          filter(
            municipio == input$municipio &
              circuito %in% input$circuitos &
              nombreAgrupacion %in% input$agrupacion
          ) %>%
          group_by(municipio, nombreAgrupacion) %>%
          summarize(promedio = mean(votosPorcentaje))
        
        ggplot(filtro, aes(x = circuito, y = votosPorcentaje, fill = factor(circuito))) +
          geom_bar(stat = "identity", show.legend = FALSE) +
          geom_text(aes(label = paste0(round(votosPorcentaje, 1), "%")),
                    position = position_dodge(0.9),
                    vjust = -0.5,
                    size = 3.6) +
          geom_hline(data = promedios_por_municipio, aes(yintercept = promedio, label = paste0(round(promedio, 1), "%")), 
                     color = "tomato", linetype = "dashed", 
                     size = 0.7,
                     alpha = 0.6
          ) +
          scale_fill_manual(values = c("#66c2a5", "#fc7d62", "#8da2cb", "#e95ac3", "#e9d110", "#e4c2a5", "#ac5d62", "#3da0cb", "#a19ae0", "#a6d854", "#66c2a5", "#fc7d62", "#1da2cb", "#a95ac3", "#e3d990", "#a4c2a5", "#1da2cb", "#a99ae0", "#a1d111", "#16c2a5", "#fc9d62", "#1da2cb", "#e17ac3", "#e1d990", "#e4c2a5", "#ac5d62", "#3da0cb", "#a19ae0", "#a6d854")) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                strip.text = element_text(face = "bold",
                                          size = 10)) +
          labs(x = "Circuito", y = "Porcentaje de Votos") +
          facet_wrap(~nombreAgrupacion, scales = "free", ncol = 2) +
          ylim(0, 100)
      }
    }, error = function(e) {
      
    }, silent = TRUE)
  })
  
}

shinyApp(ui, server)
