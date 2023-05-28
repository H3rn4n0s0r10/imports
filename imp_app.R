library(shiny)
library(tidyverse)
library(dplyr)
library(shinydashboard)
library(kableExtra)
library(DT)
library(leaflet)
library(leaflet.extras)
library(rworldxtra)
library(raster)
library(sf)

imports <- read.csv("C:/Users/hosorio/OneDrive - U.S. Tsubaki Holdings, Inc/Desktop/My Documents/Personal/R/Imports/5year_imports2.csv", nrows = 1000000)
depar <- st_read("C:/Users/hosorio/OneDrive - U.S. Tsubaki Holdings, Inc/Desktop/My Documents/Personal/R/Imports/MGN2022_DPTO_POLITICO/MGN_DPTO_POLITICO.shp")



imports <- imports %>%
  rename(clase_de_import = calse_de_import)

imports$Razon_social_del_importador <- as.factor(imports$Razon_social_del_importador)
imports$Posicion_arancelaria <- as.factor(imports$Posicion_arancelaria)

cod_aran_search <- function(cod_aran) {
  data <- imports %>%
    dplyr::select(
      Fecha_de_proceso, Posicion_arancelaria, Descripcion_aran, Porcentaje_de_arancel, Lugar_de_ingreso, Aduana,
      Numero_de_identificación_Tributaria, Razon_social_del_importador,
      Actividad_economica, Pais_de_origen, Pais_de_procedencia, Pais_de_compra,
      departametno_destino_final, metodo_de_transporte, Peso_bruto_en_kilos,
      Peso_neto_en_kilos, Cantidad_de_unidades, Codigo_de_unidad,
      Valor_FOB_dolares_de_la_mercancia, Fletes, Valor_CIF_dolares_de_la_mercancia,
      Valor_CIF_pesos_de_la_mercancia, Impuesto_a_las_ventas, Otros_derechos, Valor_aduana,
      Valor_ajuste, Base_IVA, Total_IVA_y_otros_gastos, total_costo_imp, clase_de_import, Codigo_regimen,
      codigo_de_acuerdo, tipo_de_imp
    ) %>%
    filter(Posicion_arancelaria == cod_aran)
  
  return(data)
}

nit_imp <- function(nit) {
  data <- imports %>%
    dplyr::select(
      Fecha_de_proceso, Posicion_arancelaria, Descripcion_aran, Porcentaje_de_arancel, Lugar_de_ingreso, Aduana,
      Numero_de_identificación_Tributaria, Razon_social_del_importador,
      Actividad_economica, Pais_de_origen, Pais_de_procedencia, Pais_de_compra,
      departametno_destino_final, metodo_de_transporte, Peso_bruto_en_kilos,
      Peso_neto_en_kilos, Cantidad_de_unidades, Codigo_de_unidad,
      Valor_FOB_dolares_de_la_mercancia, Fletes, Valor_CIF_dolares_de_la_mercancia,
      Valor_CIF_pesos_de_la_mercancia, Impuesto_a_las_ventas, Otros_derechos, Valor_aduana,
      Valor_ajuste, Base_IVA, Total_IVA_y_otros_gastos, total_costo_imp, clase_de_import, Codigo_regimen,
      codigo_de_acuerdo, tipo_de_imp
    ) %>%
    filter(Numero_de_identificación_Tributaria == nit)
  
  return(data)
}

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Importaciones"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Codigo Arancelario", tabName = "codigo_arancel", icon = icon("globe"))
    ),
    sidebarSearchForm(
      textId = "searchText",
      buttonId = "searchButton_cod_arancel",
      label = "Codigo Arancelario",
      icon = icon("search")
    ),
    sidebarMenu(
      menuItem("Datos de Mercado", tabName = "datos_de_mercado", icon = icon("dashboard")),
      menuItem("NIT", tabName = "numero_identifica_tri", icon = icon("dashboard"))
    ),
    sidebarSearchForm(
      textId = "searchText_nit",
      buttonId = "searchButton_nit",
      label = "Numero Identificacion Tributaria",
      icon = icon("search")
    ),
    sidebarMenu(
      menuItem("Datos de importador", tabName = "datos_importador", icon = icon("user"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "codigo_arancel",
        dataTableOutput("filteredData"),
        downloadButton("exportButton", "Export Table")
      ),
      tabItem(
        tabName = "datos_de_mercado",
        fluidRow(
          valueBoxOutput('valuebox_total_mercado')
        ),
        
        fluidRow(
          box(
            title = "Top 5 Importadores por Monto FOB USD",
            dataTableOutput('top_5_importadores_monto')
          ),
          box(
            title = "Top 5 Importadores por frecuencia",
            dataTableOutput("top_5_imp_frec")
          )
        ),
        
        fluidRow(
          box(
            title = "Top 5 Departamentos de Destino",
            dataTableOutput('top_3_dep_dest')
          ),
          box(
            title = "Mapa Colombia",
            leafletOutput('mapa')
          )
        )
      ),
      tabItem(tabName = "numero_identifica_tri"),
      tabItem(tabName = "datos_importador")
    )
  )
)



# Define server logic
server <- function(input, output) {
  
  filteredData <- reactive({
    cod_aran <- input$searchText
    nit <- input$searchText_nit
    
    if (!is.null(cod_aran) && cod_aran != "") {
      cod_aran_search(cod_aran)
    } else if (!is.null(nit) && nit != "") {
      nit_imp(nit)
    } else {
      NULL
    }
  })
  
  top_destino <- function() {
    filtered <- filteredData()
    top_destino <- filtered %>%
      count(departametno_destino_final) %>%
      drop_na(departametno_destino_final) %>% 
      top_n(5) %>%
      arrange(desc(n))
    
    return(top_destino)
  }
  
  # Define the top_destino function
  
  output$filteredData <- renderDataTable({
    filteredData()
  })
  
  output$valuebox_total_mercado <- renderValueBox({
    filtered <- filteredData()
    total_mercado <- sum(filtered$Valor_FOB_dolares_de_la_mercancia, na.rm = TRUE) %>% 
      scales::dollar_format()
    
    valueBox(
      "Tamaño de Mercado",
      total_mercado,
      color = "green",
      icon = icon("globe")
    )
  })
  
  output$top_5_importadores_monto <- renderDataTable({
    filtered <- filteredData()
    top_importador <- filtered %>%
      group_by(Razon_social_del_importador) %>%
      summarise(total_valor = sum(Valor_FOB_dolares_de_la_mercancia, na.rm = TRUE)) %>%
      top_n(5, total_valor) %>%
      arrange(desc(total_valor))
    
    kable(top_importador, "html") %>%
      kable_styling()
    
    top_importador
  })
  
  
  output$top_5_imp_frec <- renderDataTable({
    filtered <- filteredData()
    top_5_frec <- filtered %>% 
      count(Razon_social_del_importador) %>% 
      top_n(5) %>% 
      arrange(desc(n))
    
    datatable(top_5_frec, options = list(paging = FALSE))
  })
  
  output$top_3_dep_dest <- renderDataTable({
    top_destino_data <- top_destino()
    top_destino_data
  })
  
  output$mapa <- renderLeaflet({
    mapa <- leaflet() %>%
      addTiles() %>%
      setView(lng = -74, lat = 4, zoom = 6)
    
    if (!is.null(depar)) {
      top_destino_data <- top_destino()
      
      # Transform depar to WGS84 datum
      depar_wgs84 <- st_transform(depar, "+proj=longlat +datum=WGS84")
      
      # Join the top_destino table with the shapefile based on matching names
      joined_data <- left_join(depar_wgs84, top_destino_data, by = c("DPTO_CCDGO" = "departametno_destino_final"))
      
      # Determine the maximum value for coloring
      max_value <- max(top_destino_data$n)
      
      # Iterate over the joined data and add polygons with conditional coloring
      for (i in 1:nrow(joined_data)) {
        color <- ifelse(is.na(joined_data$n[i]), "gray", colorNumeric(palette = "Blues", domain = c(0, max_value))(joined_data$n[i]))
        mapa <- mapa %>% addPolygons(data = joined_data[i, ], color = color, weight = 1)
      }
    }
    
    mapa
  })
  
  output$exportButton <- downloadHandler(
    filename = function() {
      paste("filtered_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filteredData(), file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
