# Funções de módulo de Economia - Balança Comercial  - Estadual
# Função de UI
economia_bc_pa_ui <- function(id) {
  fluidPage(
    # CAMINHO DO ARQUIVO CSS----
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "estilo.css")
    ),
    # Lista de Navegação lateral----
    div(
      class = "navbar_economia",
      navbarPage(
        tags$b("Balança Comercial - Pará"),
        navbarMenu(
          tags$b("Escolha um Indicador"),
          # 1 - Balança Comercial - Exportação----
          tabPanel(
            "Exportação",
            panel(
              ## Controle----
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "bc1ano"),
                  label = "Ano",
                  choices = sort(unique(bc1[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "bc1ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(bc1[["ri"]]),
                  width = "200px"
                )
              )
            ),
            ## Mapa - Balança Comercial - Exportação----
            fluidRow(
              box(
                title = textOutput(NS(id, "bc1txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "bc1map"), height = "600px"),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  tags$h6(tags$b("Fonte:"), "MDIC - Comexstat"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                )
              ),
              ## Tabela - Balança Comercial - Exportação----
              box(
                title = textOutput(NS(id, "bc1txt2")),
                collapsible = T,
                collapsed = F,
                width = 12,
                headerBorder = T,
                solidHeader = F,
                withSpinner(
                  reactableOutput(NS(id, "bc1tab"), height = "400px"),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "MDIC - Alice Web"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "bc1_1"))
                )
              )
            ),
            ## Gráfico de Barras Balança Comercial - Exportação----
            fluidRow(
              box(
                title = textOutput(NS(id, "bc1txt3")),
                collapsible = T,
                collapsed = F,
                width = 12,
                headerBorder = T,
                solidHeader = F,
                withSpinner(
                  echarts4rOutput(NS(id, "bc1graf")),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "MDIC - Alice Web"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "bc1_2"))
                )
              )
            )
          ),
          # 2 - Balança Comercial - Importação----
          tabPanel(
            "Importação",
            panel(
              ## Controle----
              tags$div(
                class = "seletor1",
                # Selecionar
                pickerInput(
                  inputId = NS(id, "bc2ano"),
                  label = "Ano",
                  choices = sort(unique(bc2[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                # select R.I
                pickerInput(
                  inputId = NS(id, "bc2ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(bc2[["ri"]]),
                  width = "200px"
                )
              )
            ),

            ## Mapa - Balança Comercial - Importação----
            fluidRow(
              box(
                title = textOutput(NS(id, "bc2txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "bc2map"), height = "600px"),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  tags$h6(tags$b("Fonte:"), "MDIC - Alice Web"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                )
              ),
              ## Tabela - Balança Comercial - Importação----

              box(
                title = textOutput(NS(id, "bc2txt2")),
                collapsible = T,
                collapsed = F,
                width = 12,
                headerBorder = T,
                solidHeader = F,
                withSpinner(
                  reactableOutput(NS(id, "bc2tab"), height = "400px"),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "MDIC - Alice Web"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "bc2_1"))
                )
              )
            ),
            ## Gráfico de Barras Balança Comercial - Importação----
            fluidRow(
              box(
                title = textOutput(NS(id, "bc2txt3")),
                collapsible = T,
                collapsed = F,
                width = 12,
                headerBorder = T,
                solidHeader = F,
                withSpinner(
                  echarts4rOutput(NS(id, "bc2graf")),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "MDIC - Alice Web"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "bc2_2"))
                )
              )
            )
          ),
          # 3 - Saldo da Balança Comercial----
          tabPanel(
            "Saldo da Balança Comercial",
            panel(
              ## Controle----
              tags$div(
                class = "seletor1",
                # Selecionar
                pickerInput(
                  inputId = NS(id, "bc3ano"),
                  label = "Ano",
                  choices = sort(unique(bc3[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                # select R.I
                pickerInput(
                  inputId = NS(id, "bc3ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(bc3[["ri"]]),
                  width = "200px"
                )
              )
            ),

            ## Mapa - Saldo da Balança Comercial----
            fluidRow(
              box(
                title = textOutput(NS(id, "bc3txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "bc3map"), height = "600px"),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  tags$h6(tags$b("Fonte:"), "MDIC - Alice Web"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                  tags$h6(tags$b("Nota:"), 
                  "O total do Pará não equivale a soma dos municípios.
                   O critério para as exportações por UF considera o 
                   estado PRODUTOR da mercadoria.
                   Já para exportações do município, leva-se em conta 
                   o DOMICÍLIO FISCAL da empresa exportadora.")
                )
              ),
              ## Tabela - Saldo da Balança Comercial----
              box(
                title = textOutput(NS(id, "bc3txt2")),
                collapsible = T,
                collapsed = F,
                width = 12,
                headerBorder = T,
                solidHeader = F,
                withSpinner(
                  reactableOutput(NS(id, "bc3tab"), height = "400px"),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "MDIC - Alice Web"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                    tags$h6(tags$b("Nota:"), 
                    "O total do Pará não equivale a soma dos municípios.
                     O critério para as exportações por UF considera o 
                     estado PRODUTOR da mercadoria.
                     Já para exportações do município, leva-se em conta 
                    o DOMICÍLIO FISCAL da empresa exportadora.")
                  ),
                  downset_ui(NS(id, "bc3_1"))
                )
              )
            ),
            ## Gráfico de Barras Saldo da Balança Comercial----
            fluidRow(
              box(
                title = textOutput(NS(id, "bc3txt3")),
                collapsible = T,
                collapsed = F,
                width = 12,
                headerBorder = T,
                solidHeader = F,
                withSpinner(
                  echarts4rOutput(NS(id, "bc3graf")),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "MDIC - Alice Web"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                    tags$h6(tags$b("Nota:"), 
                    " O total do Pará não equivale a soma dos municípios.
                      O critério para as exportações por UF considera o 
                      estado PRODUTOR da mercadoria.
                      Já para exportações do município, leva-se em conta 
                      o DOMICÍLIO FISCAL da empresa exportadora.")
                  ),
                  downset_ui(NS(id, "bc3_2"))
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
economia_bc_pa_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    #TÍTULOS----
    # 1 - Balança Comercial - Exportação----
    ## Mapa - Balança Comercial - Exportação----
    t11 <- reactive({
      if (input$bc1ri == "Pará") {
        paste0("Balança Comercial - Exportação, ", 
               input$bc1ri, " - ", input$bc1ano)
      } else {
        paste0("Balança Comercial - Exportação, Região de Integrtação ", 
               input$bc1ri," - ", input$bc1ano)
      }
    })

    ## Tabela - Balança Comercial - Exportação----
    t12 <- reactive({
      if (input$bc1ri == "Pará") {
        paste0("Balança Comercial - Exportação dos Municípios, ", 
               input$bc1ri," - ", input$bc1ano)
      } else {
        paste0("Balança Comercial - Exportação dos Municípios, Região de Integração ", 
               input$bc1ri, " - ", input$bc1ano)
      }
    })

    ## Gráfico de Barras Balança Comercial - Exportação----
    t13 <- reactive({
      paste0("Balança Comercial - Exportação, Pará - ", 
             min(bc1$ano), " a ", max(bc1$ano))
    })
    # 2 - Balança Comercial - Importação----
    ## Mapa - Balança Comercial - Importação----
    t21 <- reactive({
      if (input$bc2ri == "Pará") {
        paste0("Balança Comercial - Importação, ", 
               input$bc2ri, " - ", input$bc2ano)
      } else {
        paste0("Balança Comercial - Importação, Região de Integrtação ", 
               input$bc2ri, " - ", input$bc2ano)
      }
    })
    ## Tabela - Balança Comercial - Importação----
    t22 <- reactive({
      if (input$bc2ri == "Pará") {
        paste0("Balança Comercial - Importação dos Municípios, ", 
               input$bc2ri, " - ", input$bc2ano)
      } else {
        paste0("Balança Comercial - Importação dos Municípios, Região de Integração ", 
               input$bc2ri, " - ", input$bc2ano)
      }
    })

    ## Gráfico de Barras Balança Comercial - Importação----
    t23 <- reactive({
      paste0("Balança Comercial - Importação, Pará - ",
             min(bc2$ano), " a ", max(bc2$ano))
    })
    # 3 - Saldo da Balança Comercial----
    ## Mapa - Saldo da Balança Comercial----
    t31 <- reactive({
      if (input$bc3ri == "Pará") {
        paste0("Saldo da Balança Comercial, ",
               input$bc3ri, " - ", input$bc3ano)
      } else {
        paste0("Saldo da Balança Comercial, Região de Integrtação ",
               input$bc3ri, " -", input$bc3ano)
      }
    })
    
    ## Tabela - Saldo da Balança Comercial----
    t32 <- reactive({
      if (input$bc3ri == "Pará") {
        paste0("Saldo da Balança Comercial dos Municípios, ",
               input$bc3ri, " - ", input$bc3ano)
      } else {
        paste0("Saldo da Balança Comercial dos Municípios, Região de Integração ",
               input$bc3ri, " -", input$bc3ano)
      }
    })
    
    ## Gráfico de Barras - Saldo da Balança Comercial----
    t33 <- reactive({
      paste0("Saldo da Balança Comercial, Pará - ",
             min(bc3$ano), " a ", max(bc3$ano))
    })

    #VISUALIZAÇÃO----
    # 1 - Balança Comercial - Exportação----
    ## Mapa - Balança Comercial - Exportação----
    output$bc1txt1 <- renderText({
    t11()  
    })
    output$bc1map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$bc1ri == "Pará") {
        df <- bc1 %>%
          filter(localidade != "Pará", ano == input$bc1ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- bc1 %>%
          filter(localidade != "Pará", ano == input$bc1ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$bc1ri)
      }

      z <- x$valor[x$valor != 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }

      pal <- colorBin(c("#ffff81", "#fad254", "#f2a92a", "#ae520b", "#6b0000"), 
                      domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Valores FOB US$:</b> %s ",
          x$name_muni,
          ifelse(is.na(x$valor), "Sem Exportação", 
                 format(x$valor, big.mark = ".", 
                        decimal.mark = ","))
        ) %>% lapply(htmltools::HTML)

      # Mapas com leafleft
      leaflet(x,
        options =
          leafletOptions(
            minZoom = 0,
            maxZoom = 15
          )
      ) %>%
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
          title = "Valores FOB US$",
          position = "bottomright",
          na.label = "Sem Exportação", 
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 2)
        )
    })

    ## Tabela - Balança Comercial - Exportação----
    output$bc1txt2 <- renderText({
    t12()  
    })
    output$bc1tab <- renderReactable({
      if (input$bc1ri == "Pará") {
        x <- bc1 %>%
          filter(localidade != "Pará", ano == input$bc1ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- bc1 %>%
          filter(localidade != "Pará", ano == input$bc1ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$bc1ri)
      }
      x$valor[x$valor == 0] <- NA
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
            name = "Valores FOB US$",
            na = "-",
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

    ## Gráfico de Barras Balança Comercial - Exportação----
    output$bc1txt3 <- renderText({
    t13()  
    })
    output$bc1graf <- renderEcharts4r({
      bc1 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#f2c94e",
          name = "Valores FOB US$",
          legend = F,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
          itemStyle = list(barBorderRadius = 5)
        ) %>%
        e_tooltip(
          trigger = "axis",
          formatter =
            e_tooltip_pointer_formatter("decimal", digits = 0, locale = "pt-BR"),
          axisPointer = list(type = "shadow")
        ) %>%
        e_x_axis(
          axisLabel = list(show = T, fontSize = 11),
          splitLine = list(show = T),
          name = "Ano",
          splitLine = list(show = T),
          nameTextStyle = list(fontWeight = "bold", fontSize = 14)
        ) %>%
        e_y_axis(
          name = "Valores FOB US$",
          nameTextStyle =
            list(fontWeight = "bold", fontSize = 14),
          scale = T,
          axisLabel = list(
            formatter = htmlwidgets::JS("
              function (value, index) {
              return value.toLocaleString('pt-BR', 
              { minimumFractionDigits: 0, maximumFractionDigits: 0 });
              }
            ")
          )
        ) %>%
        e_locale("pt-Br") %>%
        e_datazoom(toolbox = F, fillerColor = "#E5F5F9") %>%
        e_grid(show = T)
    })
    # 2 - Balança Comercial - Importação----
    ## Mapa - Balança Comercial - Importação----
    output$bc2txt1 <- renderText({
    t21()  
    })
    output$bc2map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$bc2ri == "Pará") {
        df <- bc2 %>%
          filter(localidade != "Pará", ano == input$bc2ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- bc2 %>%
          filter(localidade != "Pará", ano == input$bc2ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$bc2ri)
      }

      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }

      pal <- colorBin(c("#ffff81", "#fad254", "#f2a92a", "#ae520b", "#6b0000"), 
                      domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Valores FOB US$:</b> %s ",
          x$name_muni,
          ifelse(is.na(x$valor), "Sem Importação", 
                 format(x$valor, big.mark = ".", 
                        decimal.mark = ","))
        ) %>% lapply(htmltools::HTML)

      # Mapas com leafleft
      leaflet(x,
        options =
          leafletOptions(
            minZoom = 0,
            maxZoom = 15
          )
      ) %>%
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
          title = "Valores FOB US$",
          position = "bottomright",
          na.label = "Sem Importação",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 2)
        )
    })
    ## Tabela - Balança Comercial - Importação----
    output$bc2txt2 <- renderText({
    t22()  
    })
    output$bc2tab <- renderReactable({
      if (input$bc2ri == "Pará") {
        x <- bc2 %>%
          filter(localidade != "Pará", ano == input$bc2ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- bc2 %>%
          filter(localidade != "Pará", ano == input$bc2ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$bc2ri)
      }
      x$valor[x$valor == 0] <- NA
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
            name = "Valores FOB US$",
            na = "-",
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

    ## Gráfico de Barras Balança Comercial - Importação----
    output$bc2txt3 <- renderText({
    t23()  
    })
    output$bc2graf <- renderEcharts4r({
      bc2 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#f2c94e",
          name = "Valores FOB US$",
          legend = F,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
          itemStyle = list(barBorderRadius = 5)
        ) %>%
        e_tooltip(
          trigger = "axis",
          formatter =
            e_tooltip_pointer_formatter("decimal", digits = 0, locale = "pt-BR"),
          axisPointer = list(type = "shadow")
        ) %>%
        e_x_axis(
          axisLabel = list(show = T, fontSize = 11),
          splitLine = list(show = T),
          name = "Ano",
          splitLine = list(show = T),
          nameTextStyle = list(fontWeight = "bold", fontSize = 14)
        ) %>%
        e_y_axis(
          name = "Valores FOB US$",
          nameTextStyle =
            list(fontWeight = "bold", fontSize = 14),
          scale = T,
          axisLabel = list(
            formatter = htmlwidgets::JS("
              function (value, index) {
              return value.toLocaleString('pt-BR', 
              { minimumFractionDigits: 0, maximumFractionDigits: 0 });
              }
            ")
          )
        ) %>%
        e_locale("pt-Br") %>%
        e_datazoom(toolbox = F, fillerColor = "#E5F5F9") %>%
        e_grid(show = T)
    })
    # 3 - Saldo da Balança Comercial----
    ## Mapa - Saldo da Balança Comercial----
    output$bc3txt1 <- renderText({
    t31()  
    })

    output$bc3map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$bc3ri == "Pará") {
        df <- bc3 %>%
          filter(localidade != "Pará", ano == input$bc3ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- bc3 %>%
          filter(localidade != "Pará", ano == input$bc3ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$bc3ri)
      }

      z <- x$valor[x$valor != 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }

      pal <- colorBin(c("#ffff81", "#fad254", "#f2a92a", "#ae520b", "#6b0000"),
                      domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Valores FOB US$:</b> %s ",
          x$name_muni,
          ifelse(is.na(x$valor), "Sem Saldo", 
                 format(x$valor, big.mark = ".", 
                        decimal.mark = ","))
        ) %>% lapply(htmltools::HTML)
      # Mapas com leafleft
      leaflet(x,
        options =
          leafletOptions(
            minZoom = 0,
            maxZoom = 15
          )
      ) %>%
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
          title = "Valores FOB US$",
          position = "bottomright",
          na.label = "Sem Saldo",
          labFormat = labelFormat_decimal(big.mark = ".",
                                          decimal.mark = ",", digits = 2)
        )
    })
    ## Tabela - Saldo da Balança Comercial----
    output$bc3txt2 <- renderText({
    t32()  
    })
    output$bc3tab <- renderReactable({
      if (input$bc3ri == "Pará") {
        x <- bc3 %>%
          filter(localidade != "Pará", ano == input$bc3ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- bc3 %>%
          filter(localidade != "Pará", ano == input$bc3ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$bc3ri)
      }
      x$valor[x$valor == 0] <- NA
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
            name = "Valores FOB US$",
            na = "-",
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
    ## Gráfico de Barras - Saldo da Balança Comercial----
    output$bc3txt3 <- renderText({
    t33()  
    })
    output$bc3graf <- renderEcharts4r({
      bc3 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#f2c94e",
          name = "Valores FOB US$",
          legend = F,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
          itemStyle = list(barBorderRadius = 5)
        ) %>%
        e_tooltip(
          trigger = "axis",
          formatter =
            e_tooltip_pointer_formatter("decimal", digits = 0, locale = "pt-BR"),
          axisPointer = list(type = "shadow")
        ) %>%
        e_x_axis(
          axisLabel = list(show = T, fontSize = 11),
          splitLine = list(show = T),
          name = "Ano",
          splitLine = list(show = T),
          nameTextStyle = list(fontWeight = "bold", fontSize = 14)
        ) %>%
        e_y_axis(
          name = "Valores FOB US$",
          nameTextStyle =
            list(fontWeight = "bold", fontSize = 14),
          scale = T,
          axisLabel = list(
            formatter = htmlwidgets::JS("
              function (value, index) {
              return value.toLocaleString('pt-BR',
              { minimumFractionDigits: 0, maximumFractionDigits: 0 });
              }
            ")
          )
        ) %>%
        e_locale("pt-Br") %>%
        e_datazoom(toolbox = F, fillerColor = "#E5F5F9") %>%
        e_grid(show = T)
    })

    # DOWNLOADS----
    ## 1 - Balança Comercial - Exportação----
    ## Tabela - Balança Comercial - Exportação----
    # Filtra os dados
    bc1_1 <- reactive({
      if (input$bc1ri == "Pará") {
        x <- bc1 %>%
          filter(localidade != "Pará", ano == input$bc1ano)
      } else {
        x <- bc1 %>%
          filter(localidade != "Pará",
                 ano == input$bc1ano,
                 ri == input$bc1ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(bc1_1(), {
     t12()
      downset_Server("bc1_1", bc1_1(), t12())
    })
    ## Gráfico de Barras Balança Comercial - Exportação----
    # Filtra os dados
    bc1_2 <- reactive({
      bc1 %>% filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(bc1_2(), {
      t13()
      downset_Server("bc1_2", bc1_2(), t13())
    })

    ## 2 - Balança Comercial - Importação----
    ## Tabela - Balança Comercial - Importação----
    # Filtra os dados
    bc2_1 <- reactive({
      if (input$bc2ri == "Pará") {
        x <- bc2 %>%
          filter(localidade != "Pará",ano == input$bc2ano)
      } else {
        x <- bc2 %>%
          filter(localidade != "Pará",
                 ano == input$bc2ano,
                 ri == input$bc2ri) 
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(bc2_1(), {
      t22()
      downset_Server("bc2_1", bc2_1(), t22())
    })
    ## Gráfico de Barras Balança Comercial - Importação----
    # Filtra os dados
    bc2_2 <- reactive({
      bc2 %>% filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(bc2_2(), {
      t23()
      downset_Server("bc2_2", bc2_2(), t23())
    })

    ## 3 - Saldo da Balança Comercial----
    ## Tabela - Saldo da Balança Comercial----
    # Filtra os dados
    bc3_1 <- reactive({
      if (input$bc3ri == "Pará") {
        x <- bc3 %>%
          filter(localidade != "Pará", ano == input$bc3ano)
          
      } else {
        x <- bc3 %>%
          filter(localidade != "Pará", 
                 ano == input$bc3ano,
                 ri == input$bc3ri) 
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(bc3_1(), {
      t32()
      downset_Server("bc3_1", bc3_1(), t32())
    })
    ## Gráfico de Barras Saldo da Balança Comercial----
    # Filtra os dados
    bc3_2 <- reactive({
      bc3 %>% filter(localidade == "Pará")
      
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(bc3_2(), {
      t33()
      downset_Server("bc3_2", bc3_2(), t33())
    })
  })
}

# # Play do Módulo
# ui <- dashboardPage(
#   header = dashboardHeader(),
#   sidebar = dashboardSidebar(),
#   body = dashboardBody(fluidPage(economia_bc_pa_ui("economia_bc_pa")))
# )
# 
# server <- function(input, output) {
#   economia_bc_pa_Server("economia_bc_pa")
# }
# 
# shinyApp(ui, server)
