# Funções de módulo de Meio Ambiente - Estadual
# Função de UI
meio_ambiente_pa_ui <- function(id) {
  fluidPage(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "estilo.css")
    ),
    # Lista de Navegação lateral----
    div(
      class = "navbar_ambiente",
      navbarPage(
        tags$b("Meio Ambiente - Pará"),
        navbarMenu(
          "Indicadores",
          # 1 - Desflorestamento Acumulado (km²)----
          tabPanel(
            "Desflorestamento Acumulado (km²)",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Desflorestamento Acumulado (km²)"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "mab1ano"),
                  label = "Ano",
                  choices = sort(unique(mab1[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "mab1ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(mab1[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Desflorestamento Acumulado (km²)----
              box(
                title = textOutput(NS(id, "mab1txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "mab1map"), height = "590px"),
                  type = 8,
                  color = "#00a65a",
                  size = 0.5
                ),
                footer = list(
                  tags$h6(tags$b("Fonte:"), "INPE/PRODES"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                )
              ),
              ## Tabela - Desflorestamento Acumulado (km²)----
              box(
                title = textOutput(NS(id, "mab1txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "mab1tab"), height = "400px"),
                  type = 8,
                  color = "#00a65a",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "INPE/PRODES"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "mab1_1"))
                )
              ),
              ## Gráfico de linha - Desflorestamento Acumulado (km²)----
              box(
                title = textOutput(NS(id, "mab1txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "mab1graf")),
                  type = 8,
                  color = "#00a65a",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "INPE/PRODES"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "mab1_2"))
                )
              )
            )
          ),
          # 2 - Incremento (Desflorestamento km²)----
          tabPanel(
            "Incremento (Desflorestamento km²)",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Incremento (Desflorestamento km²)"),
              tags$div(
                class = "seletor1",
                # select Ano
                pickerInput(
                  inputId = NS(id, "mab2ano"),
                  label = "Ano",
                  choices = sort(unique(mab2[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                # select R.I
                pickerInput(
                  inputId = NS(id, "mab2ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(mab2[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Incremento (Desflorestamento km²)----
              box(
                title = textOutput(NS(id, "mab2txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "mab2map"), height = "590px"),
                  type = 8,
                  color = "#00a65a",
                  size = 0.5
                ),
                footer = list(
                  tags$h6(tags$b("Fonte:"), "INPE/PRODES"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                )
              ),
              ## Tabela - Incremento (Desflorestamento km²)----
              box(
                title = textOutput(NS(id, "mab2txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "mab2tab"), height = "400px"),
                  type = 8,
                  color = "#00a65a",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "INPE/PRODES"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "mab2_1"))
                )
              ),
              ## Gráfico de linha - Incremento (Desflorestamento km²)----
              box(
                title = textOutput(NS(id, "mab2txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "mab2graf")),
                  type = 8,
                  color = "#00a65a",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "INPE/PRODES"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "mab2_2"))
                )
              )
            )
          ),
          # 3 - Área de Floresta (km²) e Hidrografia 2020 (km²)----
          ## 3-1 - Área de Floresta (km²)----
          tabPanel(
            "Área de Floresta (km²)",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Área de Floresta (km²)"),
              tags$div(
                class = "seletor1",
                # select Ano
                pickerInput(
                  inputId = NS(id, "mab3ano1"),
                  label = "Ano",
                  choices = sort(unique(mab3[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                # select R.I
                pickerInput(
                  inputId = NS(id, "mab3ri1"),
                  label = "Pará/Região de Integração",
                  choices = unique(mab3[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Área de Floresta (km²)----
              box(
                title = textOutput(NS(id, "mab3txt11")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "mab3map1"), height = "590px"),
                  type = 8,
                  color = "#00a65a",
                  size = 0.5
                ),
                footer = list(
                  tags$h6(tags$b("Fonte:"), "INPE/ Queimadas"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                )
              ),
              ## Tabela - Área de Floresta (km²)----
              box(
                title = textOutput(NS(id, "mab3txt12")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "mab3tab1"), height = "400px"),
                  type = 8,
                  color = "#00a65a",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "INPE/ Queimadas"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "mab3_11"))
                )
              ),
              ## Gráfico de linha - Área de Floresta (km²)--------
              box(
                title = textOutput(NS(id, "mab3txt13")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "mab3graf1")),
                  type = 8,
                  color = "#00a65a",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "INPE/ Queimadas"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "mab3_21"))
                )
              )
            )
          ),
          ## 3-2 - Hidrografia (km²)----  
          tabPanel(
            "Hidrografia (km²)",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Hidrografia (km²)"),
              tags$div(
                class = "seletor1",
                # select Ano
                pickerInput(
                  inputId = NS(id, "mab3ano2"),
                  label = "Ano",
                  choices = mab3 %>% filter(categoria == "Hidrografia (km²)") %>% 
                    pull(ano) %>% unique() %>% sort(decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                # select R.I
                pickerInput(
                  inputId = NS(id, "mab3ri2"),
                  label = "Pará/Região de Integração",
                  choices = unique(mab3[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Hidrografia (km²)----
              box(
                title = textOutput(NS(id, "mab3txt21")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "mab3map2"), height = "590px"),
                  type = 8,
                  color = "#00a65a",
                  size = 0.5
                ),
                footer = list(
                  tags$h6(tags$b("Fonte:"), "INPE/ Queimadas"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                )
              ),
              ## Tabela - Hidrografia (km²)----
              box(
                title = textOutput(NS(id, "mab3txt22")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "mab3tab2"), height = "400px"),
                  type = 8,
                  color = "#00a65a",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "INPE/ Queimadas"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "mab3_12"))
                )
              ),
              ## Gráfico de linha - Hidrografia (km²)--------
              box(
                title = textOutput(NS(id, "mab3txt23")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "mab3graf2")),
                  type = 8,
                  color = "#00a65a",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "INPE/ Queimadas"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "mab3_22"))
                )
              )
            )
          ),
          
          # 4 - Focos de Calor----
          tabPanel(
            "Focos de Calor",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Focos de Calor"),
              tags$div(
                class = "seletor1",
                # select Ano
                pickerInput(
                  inputId = NS(id, "mab4ano"),
                  label = "Ano",
                  choices = sort(unique(mab4[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                # select R.I
                pickerInput(
                  inputId = NS(id, "mab4ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(mab4[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Focos de Calor----
              box(
                title = textOutput(NS(id, "mab4txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "mab4map"), height = "590px"),
                  type = 8,
                  color = "#00a65a",
                  size = 0.5
                ),
                footer = list(
                  tags$h6(tags$b("Fonte:"), "INPE/ Queimadas"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                )
              ),
              ## Tabela - Focos de Calor----
              box(
                title = textOutput(NS(id, "mab4txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "mab4tab"), height = "400px"),
                  type = 8,
                  color = "#00a65a",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "INPE/ Queimadas"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "mab4_1"))
                )
              ),
              ## Gráfico de linha - Focos de Calor----
              box(
                title = textOutput(NS(id, "mab4txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "mab4graf")),
                  type = 8,
                  color = "#00a65a",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "INPE/ Queimadas"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "mab4_2"))
                )
              )
            )
          )
        )
      )
    )
  )
}
# Função do modulo servidor
meio_ambiente_pa_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    #TÍTULOS----
    # 1 - Desflorestamento Acumulado (km²)----
    ## Mapa - Desflorestamento Acumulado (km²)----
    t11 <- reactive({
      if (input$mab1ri == "Pará") {
        paste0("Desflorestamento Acumulado (km²), Pará - ",
               input$mab1ano) 
      }else{
        paste0("Desflorestamento Acumulado (km²), Região de Integração ",
               input$mab1ri," - ",
               input$mab1ano) 
      }
    })
    ## Tabela - Desflorestamento Acumulado (km²)----
    t12 <- reactive({
      if (input$mab1ri == "Pará") {
        paste0("Desflorestamento Acumulado (km²) dos Municípios, Pará - ",
               input$mab1ano) 
      }else{
        paste0("Desflorestamento Acumulado (km²) dos Municípios, Região de Integração ",
               input$mab1ri," - ",
               input$mab1ano) 
      }
    })
    
    ## Gráfico de linha - Desflorestamento Acumulado (km²)----
    t13 <- reactive({
      paste0("Desflorestamento Acumulado (km²), Pará - ",
             min(mab1$ano)," a ",max(mab1$ano))
    })
    
    # 2 - Incremento (Desflorestamento km²)----
    ## Mapa - Incremento (Desflorestamento km²)----
    t21 <- reactive({
      if (input$mab2ri == "Pará") {
        paste0("Incremento (Desflorestamento km²), Pará - ",
               input$mab2ano) 
      }else{
        paste0("Incremento (Desflorestamento km²), Região de Integração ",
               input$mab2ri," - ",
               input$mab2ano) 
      }
    })
    ## Tabela - Incremento (Desflorestamento km²)----
    t22 <- reactive({
      if (input$mab2ri == "Pará") {
        paste0("Incremento (Desflorestamento km²) dos Municípios, Pará - ",
               input$mab2ano) 
      }else{
        paste0("Incremento (Desflorestamento km²) dos Municípios, Região de Integração ",
               input$mab2ri," - ",
               input$mab2ano) 
      }
    })
    
    ## Gráfico de linha - Incremento (Desflorestamento km²)----
    t23 <- reactive({
      paste0("Incremento (Desflorestamento km²), Pará - ",
             min(mab2$ano)," a ",max(mab2$ano))
    })
    
    # 3 - Área de Floresta (km²) e Hidrografia 2020 (km²)----
    ## 3-1 - Área de Floresta (km²)----
    ## Mapa - Focos de Calor----
    t311 <- reactive({
      if (input$mab3ri1 == "Pará") {
        paste0("Área de Floresta (km²), Pará - ",
               input$mab3ano1) 
      }else{
        paste0("Área de Floresta (km²), Região de Integração ",
               input$mab3ri1," - ",
               input$mab3ano1) 
      }
    })
    ## Tabela - Focos de Calor----
    t312 <- reactive({
      if (input$mab3ri1 == "Pará") {
        paste0("Área de Floresta (km²) dos Municípios, Pará - ",
               input$mab3ano1) 
      }else{
        paste0("Área de Floresta (km²) dos Municípios, Região de Integração ",
               input$mab3ri1," - ",
               input$mab3ano1) 
      }
    })
    
    ## Gráfico de linha - Focos de Calor----
    t313 <- reactive({
      paste0("Área de Floresta (km²), Pará - ",
             min(mab3$ano)," a ",max(mab3$ano))
    })
    ## 3-2 - Hidrografia (km²)----
    ## Mapa - Hidrografia (km²)----
    t321 <- reactive({
      if (input$mab3ri2 == "Pará") {
        paste0("Hidrografia (km²), Pará - ",
               input$mab3ano2) 
      }else{
        paste0("Hidrografia (km²), Região de Integração ",
               input$mab3ri2," - ",
               input$mab3ano2) 
      }
    })
    ## Tabela - Hidrografia (km²)----
    t322 <- reactive({
      if (input$mab3ri2 == "Pará") {
        paste0("Hidrografia (km²) dos Municípios, Pará - ",
               input$mab3ano2) 
      }else{
        paste0("Hidrografia (km²)dos Municípios, Região de Integração ",
               input$mab3ri2," - ",
               input$mab3ano2) 
      }
    })
    
    ## Gráfico de linha - Focos de Calor----
    t323 <- reactive({
      x <- mab3 %>% filter(categoria == "Hidrografia (km²)")
      paste0("Hidrografia (km²), Pará - ", min(x$ano)," a ",max(x$ano))
    })
    
    # 4 - Focos de Calor----
    ## Mapa - Focos de Calor----
    t41 <- reactive({
      if (input$mab4ri == "Pará") {
        paste0("Focos de Calor, Pará - ",
               input$mab4ano) 
      }else{
        paste0("Focos de Calor, Região de Integração ",
               input$mab4ri," - ",
               input$mab4ano) 
      }
    })
    ## Tabela - Focos de Calor----
    t42 <- reactive({
      if (input$mab4ri == "Pará") {
        paste0("Focos de Calor dos Municípios, Pará - ",
               input$mab4ano) 
      }else{
        paste0("Focos de Calor dos Municípios, Região de Integração ",
               input$mab4ri," - ",
               input$mab4ano) 
      }
    })
    
    ## Gráfico de linha - Focos de Calor----
    t43 <- reactive({
      paste0("Focos de Calor, Pará - ",
             min(mab4$ano)," a ",max(mab4$ano))
    })
    
    #VISUALIZAÇÃO----
    # 1 - Desflorestamento Acumulado (km²)----
    ## Mapa - Desflorestamento Acumulado (km²)----
    output$mab1txt1 <- renderText({
      t11()
    })
    output$mab1map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$mab1ri == "Pará") {
        df <- mab1 %>%
          filter(localidade != "Pará", ano == input$mab1ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x$valor[x$valor == 0] <- NA
      } else {
        df <- mab1 %>%
          filter(localidade != "Pará", ano == input$mab1ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$mab1ri)
        x$valor[x$valor == 0] <- NA
      }
      
      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }
      
      pal <- colorBin(c("#35aa00", "#7aca00", "#cfee00", "#ffff00", "#eba712", "#ff0000"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Área (km²):</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
            format(round(x$valor, digits = 0), big.mark = ".", decimal.mark = ",")
          )
        ) %>% lapply(htmltools::HTML)
      
      # Mapas com leafleft
      leaflet(x, options = leafletOptions(minZoom = 0, maxZoom = 15)) %>%
        addTiles() %>%
        addPolygons(
          weight = 2,
          opacity = 1,
          color = "black",
          fillOpacity = 1,
          fillColor = ~ pal(valor),
          dashArray = 1,
          smoothFactor = 1.5,
          highlightOptions =
            highlightOptions(
              weight = 3,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.5,
              bringToFront = TRUE
            ),
          label = conteudo,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"
          )
        ) %>%
        addLegend(
          pal = pal,
          values = ~valor,
          opacity = 0.7,
          title = "Acumulado (km²)",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 0)
        )
    })
    ## Tabela - Desflorestamento Acumulado (km²)----
    output$mab1txt2 <- renderText({
      t12()
    })
    
    output$mab1tab <- renderReactable({
      if (input$mab1ri == "Pará") {
        x <- mab1 %>%
          filter(localidade != "Pará", ano == input$mab1ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- mab1 %>%
          filter(localidade != "Pará", ano == input$mab1ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$mab1ri)
      }
      x %>% reactable(
        defaultPageSize = 10,
        striped = FALSE,
        highlight = TRUE,
        bordered = TRUE,
        outlined = TRUE,
        resizable = TRUE,
        showSortable = TRUE,
        pagination = F,
        columns = list(
          ri = colDef(name = "Região de Integração"),
          localidade = colDef(name = "Municípios"),
          valor = colDef(
            name = "Desflorestamento",
            format = colFormat(
              separators = T,
              locales = "pt-BR"
            )
          )
        ),
        defaultColDef = colDef(
          footerStyle = list(fontWeight = "bold"),
          headerStyle = list(background = "#f7f7f8")
        ),
        language = reactableLang(
          noData = "Sem informação",
          pageInfo = "{rowStart} a {rowEnd} de {rows} linhas",
          pagePrevious = "Anterior",
          pageNext = "Próximo",
          pagePreviousLabel = "Anterior",
          pageNextLabel = "Proximo"
        )
      )
    })
    ## Gráfico de linha - Desflorestamento Acumulado (km²)----
    output$mab1txt3 <- renderText({
      t13()
    })
    output$mab1graf <- renderEcharts4r({
      mab1 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#35aa00",
          name = "Área(km²)",
          legend = F,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
          itemStyle = list(barBorderRadius = 5)
        ) %>%
        e_tooltip(
          trigger = "axis",
          formatter = e_tooltip_pointer_formatter("decimal", digits = 0, locale = "pt-BR"),
          axisPointer = list(type = "shadow")
        ) %>%
        e_x_axis(
          axisLabel = list(show = T, fontSize = 11),
          name = "Ano",
          splitLine = list(show = T),
          nameTextStyle = list(fontWeight = "bold", fontSize = 14)
        ) %>%
        e_y_axis(
          name = "Área(km²)",
          nameTextStyle = list(fontWeight = "bold", fontSize = 14),
          scale = T,
          axisLabel = list(
            formatter = htmlwidgets::JS("
              function (value, index) {
              return value.toLocaleString('pt-BR', { minimumFractionDigits: 0, maximumFractionDigits: 0 });
              }
            ")
          )
        ) %>%
        e_locale("pt-Br") %>%
        e_datazoom(toolbox = F, fillerColor = "#E5F5F9") %>%
        e_grid(show = T)
    })
    # 2 - Incremento (Desflorestamento km²)----
    ## Mapa - Incremento (Desflorestamento km²)----
    output$mab2txt1 <- renderText({
      t21()
    })
    
    output$mab2map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$mab2ri == "Pará") {
        df <- mab2 %>%
          filter(localidade != "Pará", ano == input$mab2ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        # x$valor[x$valor == 0] <- NA
      } else {
        df <- mab2 %>%
          filter(localidade != "Pará", ano == input$mab2ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$mab2ri)
        # x$valor[x$valor == 0] <- NA
      }
      
      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }
      
      pal <- colorBin(c("#35aa00", "#7aca00", "#cfee00", "#ffff00", "#eba712", "#ff0000"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Incremento:</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
            format(round(x$valor, digits = 2), big.mark = ".", decimal.mark = ",")
          )
        ) %>% lapply(htmltools::HTML)
      
      # Mapas com leafleft
      leaflet(x, options = leafletOptions(minZoom = 0, maxZoom = 15)) %>%
        addTiles() %>%
        addPolygons(
          weight = 2,
          opacity = 1,
          color = "black",
          fillOpacity = 1,
          fillColor = ~ pal(valor),
          dashArray = 1,
          smoothFactor = 1.5,
          highlightOptions =
            highlightOptions(
              weight = 3,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.5,
              bringToFront = TRUE
            ),
          label = conteudo,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"
          )
        ) %>%
        addLegend(
          pal = pal,
          values = ~valor,
          opacity = 0.7,
          title = "Incremento",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 2)
        )
    })
    ## Tabela - Incremento (Desflorestamento km²)----
    output$mab2txt2 <- renderText({
      t22()
    })
    
    output$mab2tab <- renderReactable({
      if (input$mab2ri == "Pará") {
        x <- mab2 %>%
          filter(localidade != "Pará", ano == input$mab2ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- mab2 %>%
          filter(localidade != "Pará", ano == input$mab2ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$mab2ri)
      }
      x %>% reactable(
        defaultPageSize = 10,
        striped = FALSE,
        highlight = TRUE,
        bordered = TRUE,
        outlined = TRUE,
        resizable = TRUE,
        showSortable = TRUE,
        pagination = F,
        columns = list(
          ri = colDef(name = "Região de Integração"),
          localidade = colDef(name = "Municípios"),
          valor = colDef(
            name = "Incremento",
            format = colFormat(
              separators = T,
              digits = 2,
              locales = "pt-BR"
            )
          )
        ),
        defaultColDef = colDef(
          footerStyle = list(fontWeight = "bold"),
          headerStyle = list(background = "#f7f7f8")
        ),
        language = reactableLang(
          noData = "Sem informação",
          pageInfo = "{rowStart} a {rowEnd} de {rows} linhas",
          pagePrevious = "Anterior",
          pageNext = "Próximo",
          pagePreviousLabel = "Anterior",
          pageNextLabel = "Proximo"
        )
      )
    })
    ## Gráfico de linha - Incremento (Desflorestamento km²)----
    output$mab2txt3 <- renderText({
      t23()
    })
    output$mab2graf <- renderEcharts4r({
      mab2 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#35aa00",
          name = "Incremento",
          legend = F,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
          itemStyle = list(barBorderRadius = 5)
        ) %>%
        e_tooltip(
          trigger = "axis",
          formatter = e_tooltip_pointer_formatter("decimal", digits = 2, locale = "pt-BR"),
          axisPointer = list(type = "shadow")
        ) %>%
        e_x_axis(
          axisLabel = list(show = T, fontSize = 11),
          name = "Ano",
          splitLine = list(show = T),
          nameTextStyle = list(fontWeight = "bold", fontSize = 14)
        ) %>%
        e_y_axis(
          name = "Área(km²)",
          nameTextStyle = list(fontWeight = "bold", fontSize = 14),
          scale = T,
          axisLabel = list(
            formatter = htmlwidgets::JS("
              function (value, index) {
              return value.toLocaleString('pt-BR', 
              { minimumFractionDigits: 2, maximumFractionDigits: 2 });
              }
            ")
          )
        ) %>%
        e_locale("pt-Br") %>%
        e_datazoom(toolbox = F, fillerColor = "#E5F5F9") %>%
        e_grid(show = T)
    })
    
    # 3 - Área de Floresta (km²)----
    ## 3-1 - Área de Floresta (km²)----
    ## Mapa - Área de Floresta (km²)----
    output$mab3txt11 <- renderText({
      t311()
    })
    output$mab3map1 <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$mab3ri1 == "Pará") {
        df <- mab3 %>%
          filter(localidade != "Pará", 
                 ano == input$mab3ano1,
                 categoria == "Área de Floresta (km²)") %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- mab3 %>%
          filter(localidade != "Pará", ano == input$mab3ano1,
                 categoria == "Área de Floresta (km²)") %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$mab3ri1)
      }
      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }
      pal <- colorBin(c("#e9ffbe", "#9ff595", "#98e600", "#70a800", "#267300"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Área (km²):</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
            format(round(x$valor, digits = 0), big.mark = ".", decimal.mark = ",")
          )
        ) %>% lapply(htmltools::HTML)
      # Mapas com leafleft
      leaflet(x, options = leafletOptions(minZoom = 0, maxZoom = 15)) %>%
        addTiles() %>%
        addPolygons(
          weight = 2,
          opacity = 1,
          color = "black",
          fillOpacity = 1,
          fillColor = ~ pal(valor),
          dashArray = 1,
          smoothFactor = 1.5,
          highlightOptions =
            highlightOptions(
              weight = 3,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.5,
              bringToFront = TRUE
            ),
          label = conteudo,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"
          )
        ) %>%
        addLegend(
          pal = pal,
          values = ~valor,
          opacity = 0.7,
          title = "Área (km²)",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 0)
        )
    })
    
    ## Tabela - Área de Floresta (km²)----
    output$mab3txt12 <- renderText({
      t312()
    })
    output$mab3tab1 <- renderReactable({
      if (input$mab3ri1 == "Pará") {
        x <- mab3 %>%
          filter(localidade != "Pará", 
                 ano == input$mab3ano1,
                 categoria == "Área de Floresta (km²)") %>%
          select(ri, localidade, valor)
      } else {
        x <- mab3 %>%
          filter(localidade != "Pará", 
                 ano == input$mab3ano1,
                 categoria == "Área de Floresta (km²)") %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$mab3ri1)
      }
      x %>% reactable(
        defaultPageSize = 10,
        striped = FALSE,
        highlight = TRUE,
        bordered = TRUE,
        outlined = TRUE,
        resizable = TRUE,
        showSortable = TRUE,
        pagination = F,
        columns = list(
          ri = colDef(name = "Região de Integração"),
          localidade = colDef(name = "Municípios"),
          valor = colDef(
            name = "Área (km²)",
            format = colFormat(
              separators = T,
              locales = "pt-BR"
            )
          )
        ),
        defaultColDef = colDef(
          footerStyle = list(fontWeight = "bold"),
          headerStyle = list(background = "#f7f7f8")
        ),
        language = reactableLang(
          noData = "Sem informação",
          pageInfo = "{rowStart} a {rowEnd} de {rows} linhas",
          pagePrevious = "Anterior",
          pageNext = "Próximo",
          pagePreviousLabel = "Anterior",
          pageNextLabel = "Proximo"
        )
      )
    })
    
    ## Gráfico - Área de Floresta (km²)----
    output$mab3txt13 <- renderText({
      t313()
    })
    output$mab3graf1 <- renderEcharts4r({
      mab3 %>%
        filter(localidade == "Pará", categoria == "Área de Floresta (km²)") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#35aa00",
          name = "Área (km²)",
          legend = F,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
          itemStyle = list(barBorderRadius = 5)
        ) %>%
        e_tooltip(
          trigger = "axis",
          formatter = e_tooltip_pointer_formatter("decimal", digits = 0, locale = "pt-BR"),
          axisPointer = list(type = "shadow")
        ) %>%
        e_x_axis(
          axisLabel = list(show = T, fontSize = 11),
          name = "Ano",
          splitLine = list(show = T),
          nameTextStyle = list(fontWeight = "bold", fontSize = 14)
        ) %>%
        e_y_axis(
          name = "Área(km²)",
          nameTextStyle = list(fontWeight = "bold", fontSize = 14),
          scale = T,
          axisLabel = list(
            formatter = htmlwidgets::JS(
              "
              function (value, index) {
              return value.toLocaleString('pt-BR', { minimumFractionDigits: 0, maximumFractionDigits: 0 });
              }
            "
            )
          )
        ) %>%
        e_locale("pt-Br") %>%
        e_datazoom(toolbox = F, fillerColor = "#E5F5F9") %>%
        e_grid(show = T)
    })
    
    
    ## 3-2 - Hidrografia (km²)----
    ## Mapa - Hidrografia (km²)----
    output$mab3txt21 <- renderText({
      t321()
    })
    output$mab3map2 <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$mab3ri2 == "Pará") {
        df <- mab3 %>%
          filter(localidade != "Pará", 
                 ano == input$mab3ano2,
                 categoria == "Hidrografia (km²)") %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- mab3 %>%
          filter(localidade != "Pará", 
                 ano == input$mab3ano2,
                 categoria == "Hidrografia (km²)") %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$mab3ri2)
      }
      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }
      pal <- colorBin(c("#acffff", "#6fd4f2", "#32a9e5", "#5070eb", "#0343d6"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Hidrografia (km²):</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
            format(round(x$valor, digits = 0), big.mark = ".", decimal.mark = ",")
          )
        ) %>% lapply(htmltools::HTML)
      
      # Mapas com leafleft
      leaflet(x, options = leafletOptions(minZoom = 0, maxZoom = 15)) %>%
        addTiles() %>%
        addPolygons(
          weight = 2,
          opacity = 1,
          color = "black",
          fillOpacity = 1,
          fillColor = ~ pal(valor),
          dashArray = 1,
          smoothFactor = 1.5,
          highlightOptions =
            highlightOptions(
              weight = 3,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.5,
              bringToFront = TRUE
            ),
          label = conteudo,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"
          )
        ) %>%
        addLegend(
          pal = pal,
          values = ~valor,
          opacity = 0.7,
          title = "Hidrografia (km²)",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 0)
        )
    })
    
    ## Tabela - Hidrografia (km²)----
    output$mab3txt22 <- renderText({
      t322()
    })
    output$mab3tab2 <- renderReactable({
      if (input$mab3ri2 == "Pará") {
        x <- mab3 %>%
          filter(localidade != "Pará",
                 ano == input$mab3ano2,
                 categoria == "Hidrografia (km²)") %>%
          select(ri, localidade, valor)
      } else {
        x <- mab3 %>%
          filter(localidade != "Pará",
                 ano == input$mab3ano2,
                 categoria == "Hidrografia (km²)") %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$mab3ri2)
      }
      x %>% reactable(
        defaultPageSize = 10,
        striped = FALSE,
        highlight = TRUE,
        bordered = TRUE,
        outlined = TRUE,
        resizable = TRUE,
        showSortable = TRUE,
        pagination = F,
        columns = list(
          ri = colDef(name = "Região de Integração"),
          localidade = colDef(name = "Municípios"),
          valor = colDef(
            name = "Hidrografia (km²)",
            format = colFormat(
              separators = T,
              locales = "pt-BR"
            )
          )
        ),
        defaultColDef = colDef(
          footerStyle = list(fontWeight = "bold"),
          headerStyle = list(background = "#f7f7f8")
        ),
        language = reactableLang(
          noData = "Sem informação",
          pageInfo = "{rowStart} a {rowEnd} de {rows} linhas",
          pagePrevious = "Anterior",
          pageNext = "Próximo",
          pagePreviousLabel = "Anterior",
          pageNextLabel = "Proximo"
        )
      )
    })
    
    ## Gráfico - Hidrografia (km²)----
    output$mab3txt23 <- renderText({
      t323()
    })
    output$mab3graf2 <- renderEcharts4r({
      mab3 %>%
        filter(localidade == "Pará", categoria == "Hidrografia (km²)") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#32a9e5",
          name = "Hidrografia (km²)",
          legend = F,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
          itemStyle = list(barBorderRadius = 5)
        ) %>%
        e_tooltip(
          trigger = "axis",
          formatter = e_tooltip_pointer_formatter("decimal", digits = 0, locale = "pt-BR"),
          axisPointer = list(type = "shadow")
        ) %>%
        e_x_axis(
          axisLabel = list(show = T, fontSize = 11),
          name = "Ano",
          splitLine = list(show = T),
          nameTextStyle = list(fontWeight = "bold", fontSize = 14)
        ) %>%
        e_y_axis(
          name = "Área(km²)",
          nameTextStyle = list(fontWeight = "bold", fontSize = 14),
          scale = T,
          axisLabel = list(
            formatter = htmlwidgets::JS(
              "
              function (value, index) {
              return value.toLocaleString('pt-BR', { minimumFractionDigits: 0, maximumFractionDigits: 0 });
              }
            "
            )
          )
        ) %>%
        e_locale("pt-Br") %>%
        e_datazoom(toolbox = F, fillerColor = "#E5F5F9") %>%
        e_grid(show = T)
    })
    
    # 4 - Focos de Calor----
    ## Mapa - Focos de Calor----
    output$mab4txt1 <- renderText({
      t41()
    })
    output$mab4map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$mab4ri == "Pará") {
        df <- mab4 %>%
          filter(localidade != "Pará", ano == input$mab4ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        # x$valor[x$valor == 0] <- NA
      } else {
        df <- mab4 %>%
          filter(localidade != "Pará", ano == input$mab4ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$mab4ri)
        # x$valor[x$valor == 0] <- NA
      }
      
      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }
      
      pal <- colorBin(c("#F5F500", "#f6b900", "#f67b01", "#f53b00", "#f50100"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Focos de Calor:</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
            format(round(x$valor, digits = 0), big.mark = ".", decimal.mark = ",")
          )
        ) %>% lapply(htmltools::HTML)
      
      # Mapas com leafleft
      leaflet(x, options = leafletOptions(minZoom = 0, maxZoom = 15)) %>%
        addTiles() %>%
        addPolygons(
          weight = 2,
          opacity = 1,
          color = "black",
          fillOpacity = 1,
          fillColor = ~ pal(valor),
          dashArray = 1,
          smoothFactor = 1.5,
          highlightOptions =
            highlightOptions(
              weight = 3,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.5,
              bringToFront = TRUE
            ),
          label = conteudo,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"
          )
        ) %>%
        addLegend(
          pal = pal,
          values = ~valor,
          opacity = 0.7,
          title = "Focos de Calor",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 2)
        )
    })
    ## Tabela - Focos de Calor----
    output$mab4txt2 <- renderText({
      t42()
    })
    output$mab4tab <- renderReactable({
      if (input$mab4ri == "Pará") {
        x <- mab4 %>%
          filter(localidade != "Pará", ano == input$mab4ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- mab4 %>%
          filter(localidade != "Pará", ano == input$mab4ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$mab4ri)
      }
      x %>% reactable(
        defaultPageSize = 10,
        striped = FALSE,
        highlight = TRUE,
        bordered = TRUE,
        outlined = TRUE,
        resizable = TRUE,
        showSortable = TRUE,
        pagination = F,
        columns = list(
          ri = colDef(name = "Região de Integração"),
          localidade = colDef(name = "Municípios"),
          valor = colDef(
            name = "Focos de Calor",
            format = colFormat(
              separators = T,
              locales = "pt-BR"
            )
          )
        ),
        defaultColDef = colDef(
          footerStyle = list(fontWeight = "bold"),
          headerStyle = list(background = "#f7f7f8")
        ),
        language = reactableLang(
          noData = "Sem informação",
          pageInfo = "{rowStart} a {rowEnd} de {rows} linhas",
          pagePrevious = "Anterior",
          pageNext = "Próximo",
          pagePreviousLabel = "Anterior",
          pageNextLabel = "Proximo"
        )
      )
    })
    ## Gráfico de linha - Focos de Calor----
    output$mab4txt3 <- renderText({
      t43()
    })
    output$mab4graf <- renderEcharts4r({
      mab4 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#f50100",
          name = "Focos de Calor",
          legend = F,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
          itemStyle = list(barBorderRadius = 5)
        ) %>%
        e_tooltip(
          trigger = "axis",
          formatter = e_tooltip_pointer_formatter("decimal", digits = 0, locale = "pt-BR"),
          axisPointer = list(type = "shadow")
        ) %>%
        e_x_axis(
          axisLabel = list(show = T, fontSize = 11),
          name = "Ano",
          splitLine = list(show = T),
          nameTextStyle = list(fontWeight = "bold", fontSize = 14)
        ) %>%
        e_y_axis(
          name = "Área(km²)",
          nameTextStyle = list(fontWeight = "bold", fontSize = 14),
          scale = T,
          axisLabel = list(
            formatter = htmlwidgets::JS("
              function (value, index) {
              return value.toLocaleString('pt-BR', { minimumFractionDigits: 0, maximumFractionDigits: 0 });
              }
            ")
          )
        ) %>%
        e_locale("pt-Br") %>%
        e_datazoom(toolbox = F, fillerColor = "#E5F5F9") %>%
        e_grid(show = T)
    })
    #DOWNLOADS----
    # 1 - Desflorestamento Acumulado (km²)----
    ## Tabela - Desflorestamento Acumulado (km²)----
    # Filtra os dados
    mab1_1 <- reactive({
      if (input$mab1ri == "Pará") {
        x <- mab1 %>%
          filter(localidade != "Pará", ano == input$mab1ano) 
      } else {
        x <- mab1 %>%
          filter(localidade != "Pará", 
                 ano == input$mab1ano,
                 ri == input$mab1ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(mab1_1(), {
      downset_Server("mab1_1", mab1_1(), t12())
    })
    ## Gráfico de linha - Desflorestamento Acumulado (km²)----
    # Filtra os dados
    mab1_2 <- reactive({
      mab1 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(mab1_2(), {
      downset_Server("mab1_2", mab1_2(), t13())
    })
    
    # 2 - Incremento (Desflorestamento km²)----
    ## Tabela - Incremento (Desflorestamento km²)----
    # Filtra os dados
    mab2_1 <- reactive({
      if (input$mab2ri == "Pará") {
        x <- mab2 %>%
          filter(localidade != "Pará", ano == input$mab2ano)
      } else {
        x <- mab2 %>%
          filter(localidade != "Pará",
                 ano == input$mab2ano,
                 ri == input$mab2ri)
      }  
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(mab2_1(), {
      downset_Server("mab2_1", mab2_1(), t22())
    })
    ## Gráfico de linha - Incremento (Desflorestamento km²)----
    # Filtra os dados
    mab2_2 <- reactive({
      mab2 %>%
        filter(localidade == "Pará")  
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(mab2_2(), {
      downset_Server("mab2_2", mab2_2(), t23())
    })
    
    # 3 - Área de Floresta (km²) e Hidrografia (km²)----
    # 3-1 -Área de Floresta (km²)----
    ## Tabela - Área de Floresta (km²)----
    # Filtra os dados
    mab3_11 <- reactive({
      if (input$mab3ri1 == "Pará") {
        x <- mab3 %>%
          filter(localidade != "Pará", 
                 ano == input$mab3ano1,
                 categoria == "Área de Floresta (km²)")
      } else {
        x <- mab3 %>%
          filter(localidade != "Pará", 
                 ano == input$mab3ano1,
                 categoria == "Área de Floresta (km²)")
        x <- x %>% filter(ri == input$mab3ri1)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(mab3_11(), {
      downset_Server("mab3_11", mab3_11(), t312())
    })
    ## Gráfico de linha - Área de Floresta (km²)----
    # Filtra os dados
    mab3_13 <- reactive({
      mab3 %>%
        filter(localidade == "Pará", categoria == "Área de Floresta (km²)")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(mab3_13(), {
      downset_Server("mab3_21", mab3_13(), t313())
    })
    
    #3-2 -Hidrografia (km²)----
    ## Tabela - Hidrografia (km²)----
    # Filtra os dados
    mab3_21 <- reactive({
      if (input$mab3ri2 == "Pará") {
        x <- mab3 %>%
          filter(localidade != "Pará",
                 ano == input$mab3ano2,
                 categoria == "Hidrografia (km²)")
      } else {
        x <- mab3 %>%
          filter(localidade != "Pará",
                 ano == input$mab3ano2,
                 categoria == "Hidrografia (km²)")
        x <- x %>% filter(ri == input$mab3ri2)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(mab3_21(), {
      downset_Server("mab3_21", mab3_21(), t322())
    })
    ## Gráfico de linha - Hidrografia (km²)----
    # Filtra os dados
    mab3_22 <- reactive({
      mab3 %>%
        filter(localidade == "Pará", categoria == "Hidrografia (km²)")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(mab3_22(), {
      downset_Server("mab3_22", mab3_22(), t323())
    })
    
    # 4 - Focos de Calor----
    ## Tabela - Focos de Calor----
    # Filtra os dados
    mab4_1 <- reactive({
      if (input$mab4ri == "Pará") {
        x <- mab4 %>%
          filter(localidade != "Pará", ano == input$mab4ano) 
      } else {
        x <- mab4 %>%
          filter(localidade != "Pará", 
                 ano == input$mab4ano,
                 ri == input$mab4ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(mab4_1(), {
      downset_Server("mab4_1", mab4_1(), t42())
    })
    ## Gráfico de linha - Focos de Calor----
    # Filtra os dados
    mab4_2 <- reactive({
      mab4 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(mab4_2(), {
      downset_Server("mab4_2", mab4_2(), t43())
    })
    
  })
}

# Play do Módulo
# ui <- dashboardPage(
#   header = dashboardHeader(),
#   sidebar = dashboardSidebar(),
#   body = dashboardBody(fluidPage(
#     meio_ambiente_pa_ui("meio_ambiente_pa")
#   ))
# )
# 
# server <- function(input, output) {
#   meio_ambiente_pa_Server("meio_ambiente_pa")
# }
# 
# shinyApp(ui, server)
