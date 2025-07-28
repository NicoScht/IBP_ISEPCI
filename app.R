# Load packages ----------------------------------------------------------------

library(shiny)
library(bslib)
library(ggplot2)
library(scales)
library(tidyverse)
library(googlesheets4)
library(DT)
library(shinyWidgets)

# Load data --------------------------------------------------------------------

gs4_deauth()  # Disable authentication (since the sheet is public)
data <- read_sheet("https://docs.google.com/spreadsheets/d/1j7pNVYhwCbqlTP2Qnl4FqjjYPKh-KiHTOy_gjPxwAHA/edit?usp=sharing")
indec <- read_sheet("https://docs.google.com/spreadsheets/d/1dz3WGgw3Sr6dZkFD43DFacWDXsCr2W5Dec76c5GzZpU/edit?usp=sharing")
data <- left_join(data, indec, by = "periodo")
min_date <- min(data$periodo)
max_date <- max(data$periodo)
distritos <- levels(factor(data$Distrito))

# Define UI --------------------------------------------------------------------

ui <- page_sidebar(
  title = "Índice Barrial de Precios - ISEPCI",
  theme = bs_theme(preset = "pulse"),
  sidebar = sidebar(
    
    uiOutput("selector_fechas")
    ,
    
    
    selectInput(inputId = "distrito_seleccionado",
                label = "Seleccionar distrito:",
                choices = distritos,
                selected = "Conurbano",
                multiple = FALSE, selectize = TRUE),
    
    checkboxGroupInput(inputId = 'opcionales',
                       label = 'Métricas opcionales:',
                       choices = c('Almacén' = 'almacen',
                                   'Verdulería' = 'verduleria',
                                   'Carnicería' = 'carniceria',
                                   "Comparar con índice nacional del INDEC" = "cba_indec"),
                       selected = NULL),
    
    downloadBttn(outputId = 'descarga',
                 label = 'Descargar datos seleccionados',
                 style = 'simple',
                 size = 'sm'),
    
    helpText("La descarga es en formato .CSV")
  ),
  
  navset_card_underline(
    nav_panel("Variaciones", plotOutput(outputId = 'grafico_1',
                                        hover = "hover_1"), 
              verbatimTextOutput("grafico_info")),
    
    nav_panel("Canastas", plotOutput('grafico_2', hover = 'hover_2'),
              verbatimTextOutput("grafico_info_2")
    )),
  
  
  card(
    h5(textOutput("titulo_tabla_1")),
    DT::DTOutput(outputId = 'tabla_1')
  )
  
)


# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  
  output$selector_fechas <- renderUI({
    req(input$distrito_seleccionado)
    
    fechas_distrito <- data %>%
      filter(Distrito == input$distrito_seleccionado) %>%
      pull(periodo) %>%
      as.Date()  # Convertir a Date
    
    airDatepickerInput(
      inputId = "periodo_seleccionado",
      label = NULL,
      range = TRUE,
      minDate = min(fechas_distrito),
      maxDate = max(fechas_distrito),
      value = c(min(fechas_distrito), max(fechas_distrito)),
      separator = " al ",
      autoClose = TRUE,
      dateFormat = "MM-yyyy",
      view = "months",
      minView = "months",
      language = "es"
    )
  })
  
  dato_filtrado <- reactive({
    req(input$periodo_seleccionado)
    req(input$distrito_seleccionado)
    
    base <- data %>% 
      filter(
        periodo >= (input$periodo_seleccionado[1]) &
          periodo <= (input$periodo_seleccionado[2]),
        Distrito == input$distrito_seleccionado) %>%
      
      mutate(
        periodo = format(ymd(periodo), "%m-%Y"),  # Format 'periodo' as "M-YYYY"
        IBP = as.numeric(sapply(IBP, function(x) ifelse(is.null(x), NA, x))),
        var_almacen = as.numeric(sapply(var_almacen, function(x) ifelse(is.null(x), NA, x))),
        var_verduleria = as.numeric(sapply(var_verduleria, function(x) ifelse(is.null(x), NA, x))),
        var_carniceria = as.numeric(sapply(var_carniceria, function(x) ifelse(is.null(x), NA, x))),
        var_cba_indec = as.numeric(sapply(var_cba_indec, function(x) ifelse(is.null(x), NA, x))),
        
        almacen = as.numeric(sapply(almacen, function(x) ifelse(is.null(x), NA, x))),
        verduleria = as.numeric(sapply(verduleria, function(x) ifelse(is.null(x), NA, x))),
        carniceria = as.numeric(sapply(carniceria, function(x) ifelse(is.null(x), NA, x))),
        cba_indec = as.numeric(sapply(cba_indec, function(x) ifelse(is.null(x), NA, x))),
        
        # Ensure CBA is numeric# Convert list to numeric safely
      ) %>%
      drop_na(IBP)  # Remove NA values  
    
    # Base columns always shown
    columnas_basicas <- c("Distrito", "periodo", "CBA", "IBP")
    
    # Add optional metrics, if selected
    if (!is.null(input$opcionales) && length(input$opcionales) > 0) {
      columnas_opcionales <- unlist(lapply(input$opcionales, function(x) {
        c(x, paste0("var_", x))
      }))
      columnas_basicas <- c(columnas_basicas, columnas_opcionales)
    }
    
    # Only keep existing columns
    columnas_basicas <- intersect(columnas_basicas, colnames(base))
    
    base %>% select(all_of(columnas_basicas))
  })
  
  
  output$titulo_tabla_1 <- renderText({
    req(input$periodo_seleccionado, input$distrito_seleccionado)  # Ensure inputs exist
    
    paste(
      "Tabla de IBP mensual para", input$distrito_seleccionado, 
      "desde", format(input$periodo_seleccionado[1], "%m-%Y"),
      "hasta", format(input$periodo_seleccionado[2], "%m-%Y")
    )
  })
  
  
  output$tabla_1 <- DT::renderDataTable({
    df <- dato_filtrado()
    
    
    if (nrow(df) == 0) {
      return(DT::datatable(data.frame(Mensaje = "No hay datos para mostrar."),
                           options = list(dom = 't'), rownames = FALSE))
    }
    
    # ---- Rename columns for display ----
    colnames(df) <- colnames(df) %>%
      str_replace_all("IBP", "IBP (%)") %>%
      str_replace_all("CBA", "CBA ($)") %>%
      str_replace_all("^almacen$", "Almacén ($)") %>%
      str_replace_all("^verduleria$", "Verdulería ($)") %>%
      str_replace_all("^carniceria$", "Carnicería ($)") %>%
      str_replace_all("var_almacen", "Almacén (%)") %>%
      str_replace_all("var_verduleria", "Verdulería (%)") %>%
      str_replace_all("var_carniceria", "Carnicería (%)") %>%
      str_replace_all("var_cba_indec", "CBA INDEC (%)") %>%
      str_replace_all("^cba_indec$", "CBA INDEC ($)")
    
    # ---- Detect column types for formatting ----
    cols_currency <- grep("\\(\\$\\)", colnames(df), value = TRUE)
    cols_percent <- grep("\\(\\%\\)", colnames(df), value = TRUE)
    
    # ---- Build datatable with formatting ----
    datatable_object <- DT::datatable(df, options = list(pageLength = 10), rownames = FALSE) %>%
      DT::formatCurrency(columns = cols_currency, currency = "$", digits = 0) %>%
      DT::formatPercentage(columns = cols_percent, digits = 2)
    
    # ---- Optional: Add color to IBP column ----
    if ("IBP (%)" %in% colnames(df)) {
      datatable_object <- datatable_object %>%
        DT::formatStyle(
          columns = "IBP (%)",
          color = DT::styleInterval(0, c("green", "red")),
          fontWeight = 'bold'
        )
    }
    
    return(datatable_object)
  })
  
  
  
  output$grafico_1 <- renderPlot({
    filtered_data <- dato_filtrado() %>%
      mutate(
        periodo = as.Date(paste0("01-", periodo), format = "%d-%m-%Y")
      )
    
    porcentaje_cols <- c("IBP")
    if (!is.null(input$opcionales) && length(input$opcionales) > 0) {
      porcentaje_cols <- c(porcentaje_cols, paste0("var_", input$opcionales))
    }
    
    porcentaje_cols <- intersect(porcentaje_cols, colnames(filtered_data))
    
    plot_data <- filtered_data %>% 
      select(periodo, all_of(porcentaje_cols)) %>% 
      pivot_longer(-periodo, names_to = "variable", values_to = 'valor') %>%
      mutate(
        variable = recode(variable,
                          IBP = "IBP",
                          var_almacen = "Almacén (%)",
                          var_verduleria = "Verdulería (%)",
                          var_carniceria = "Carnicería (%)",
                          var_cba_indec = "CBA INDEC (%)"
        )
      )
    
    # Define consistent colors
    colores <- c(
      "IBP" = "red",
      "Almacén (%)" = "#00FFFF",
      "Verdulería (%)" = "#7CFC00",
      "Carnicería (%)" = "#CD7F32",
      "CBA INDEC (%)" = "blue"
    )
    
    ggplot(data = plot_data, aes(x = periodo, y = valor * 100, color = variable)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      geom_hline(yintercept = 0, color = "black", linewidth = 1, linetype = "solid") +
      
      
      labs(
        title = paste("Evolución porcentual mensual en", input$distrito_seleccionado), 
        x = "Periodo", 
        y = "Variación (%)",
        color = 'Indicador'
      ) +
      scale_y_continuous(labels = scales::percent_format(scale = 1, accuracy = 0.01)) +
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
      scale_color_manual(values = colores) +  # ✅ Correct usage
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$grafico_info <- renderText({
    # Ensure there is hover data
    
    periodo_hover <- as.Date(input$hover_1$x, origin = "1970-01-01")  # Convert numeric date to Date
    ibp_hover <- as.numeric(input$hover_1$y) / 100  # Convert IBP back to original scale
    
    paste0("Distrito: ", input$distrito_seleccionado,
           "\nPeríodo: ", format(periodo_hover, "%b %Y"),  # Format date as "MMM YYYY"
           "\nIBP: ", scales::percent(ibp_hover, accuracy = 0.01)  # Format IBP as percentage
    )
  })
  
  output$grafico_2 <- renderPlot({
    filtered_data_2 <- dato_filtrado() %>%
      mutate(
        periodo = as.Date(paste0("01-", periodo), format = "%d-%m-%Y"),
        CBA = as.numeric(gsub(",", ".", gsub("[^0-9,.-]", "", CBA)))  # Clean and convert to numeric
      )
    
    currency_cols <- c("CBA")
    if (!is.null(input$opcionales) && length(input$opcionales) > 0) {
      currency_cols <- c(currency_cols, input$opcionales)  # Optional metrics (almacen, verduleria, carniceria)
    }
    
    currency_cols <- intersect(currency_cols, colnames(filtered_data_2))
    
    # Pivot long for plotting
    plot_data <- filtered_data_2 %>%
      select(periodo, all_of(currency_cols)) %>%
      pivot_longer(-periodo, names_to = "variable", values_to = "valor")
    
    # Friendly labels
    plot_data$variable <- recode(plot_data$variable,
                                 CBA = "CBA",
                                 almacen = "Almacén (ARS)",
                                 verduleria = "Verdulería (ARS)",
                                 carniceria = "Carnicería (ARS)",
                                 cba_indec = "CBA INDEC (ARS)")
    
    # Plot
    ggplot(data = plot_data, aes(x = periodo, y = valor, color = variable)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(
        title = paste("Evolución de precios por categoría en", input$distrito_seleccionado),
        x = "Periodo",
        y = "Precio (ARS)",
        color = "Indicador"
      ) +
      scale_y_continuous(labels = scales::dollar_format(prefix = "$", big.mark = ".", decimal.mark = ",")) +
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
      theme_minimal() +
      scale_color_manual(
        values = c(
          "CBA" = "red",
          "Almacén (ARS)" = "#00FFFF",
          "Verdulería (ARS)" = "#7CFC00",
          "Carnicería (ARS)" = "#CD7F32",
          "CBA INDEC (ARS)" = "blue"
        )
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$grafico_info_2 <- renderText({
    periodo_hover <- as.Date(input$hover_2$x, origin = "1970-01-01")
    cba_hover <- as.numeric(input$hover_2$y)
    
    paste0(
      "Distrito: ", input$distrito_seleccionado,
      "\nPeríodo: ", format(periodo_hover, "%b %Y"),
      "\nCBA: ", scales::dollar(cba_hover, prefix = "$", big.mark = ".", decimal.mark = ",")
    )
  })
  
  output$descarga <- downloadHandler(
    filename = function() {
      paste0(
        "IBP_", input$distrito_seleccionado,
        "_del_", input$periodo_seleccionado[1],
        "_al_", input$periodo_seleccionado[2],
        ".csv"
      )
    },
    
    content = function(file) {
      base <- dato_filtrado()
      
      # Base columns always included
      columnas_exportar <- c("periodo", "Distrito", "CBA", "IBP")
      
      # Add optional metric columns if selected
      if (!is.null(input$opcionales) && length(input$opcionales) > 0) {
        columnas_opcionales <- unlist(lapply(input$opcionales, function(x) {
          c(x, paste0("var_", x))
        }))
        columnas_exportar <- c(columnas_exportar, columnas_opcionales)
      }
      
      # Filter to only available columns in data
      columnas_exportar <- intersect(columnas_exportar, colnames(base))
      
      base %>%
        select(all_of(columnas_exportar)) %>%
        write_csv(file)
    }
  )
  
  
  
}
# Create the Shiny app object --------------------------------------------------

shinyApp(ui = ui, server = server)


#Pasos a seguir
# Revisar botones tabla. Ordenan mal (fecha), o tiran error (CBA)
# 1: Cambiar la tabla por una que me permita ver todos los meses
# 2. Permitir elegir entre Excel o CSV o PDF
# 4: Añadir comparación con otra provincia
# 5. Cabmiar fecha por un slider?
# 6. Cambiar color de IBP por algo más escalar
# 7: Añadir mapa?

