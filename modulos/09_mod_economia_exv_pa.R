# Funções de módulo de Economia - Extração Vegetal - Estadual
# Função de UI
economia_exv_pa_ui <- function(id) {
  fluidPage(# CAMINHO DO ARQUIVO CSS----
            tags$head(
              tags$link(rel = "stylesheet", type = "text/css", href = "estilo.css")
            ),
            # Lista de Navegação lateral----
            div(class = "navbar_economia",
                navbarPage(
                  tags$b("Extração Vegetal - Pará"),
                  navbarMenu(
                    "Indicadores",
                    # 1 - Quantidade Produzida na Extração Vegetal, por Tipo de Produto Extrativo----
                    tabPanel(
                      "Quantidade Produzida na Extração Vegetal, por Tipo de Produto Extrativo",
                      panel(
                        ## Controle----
                        heading =
                          h4(style = "margin-bottom: 2px;margin-top: 2px;
                          font-weight: bold;color: #6D6D6D;",
                             "Quantidade Produzida na Extração Vegetal, por Tipo de Produto Extrativo"),
                        tags$div(
                          class = "seletor1",
                          pickerInput(
                            inputId = NS(id, "exv1ano"),
                            label = "Ano",
                            choices = sort(unique(exv1[["ano"]]), decreasing = T),
                            width = "100px"
                          )
                        ),
                        tags$div(
                          class = "seletor2",
                          pickerInput(
                            inputId = NS(id, "exv1ri"),
                            label = "Pará/Região de Integração",
                            choices = unique(exv1[["ri"]]),
                            width = "200px"
                          )
                        ),
                        tags$div(
                          class = "seletor2",
                          # select R.I
                          pickerInput(
                            inputId = NS(id, "exv1cat"),
                            label = "Produto",
                            choices = unique(exv1[["categoria"]]),
                            width = "200px"
                          )
                        )
                      ),
                      fluidRow(
                        ## Mapa - Quantidade Produzida na Extração Vegetal, por Tipo de Produto Extrativo----
                        box(
                          title = textOutput(NS(id, "exv1txt1")),
                          collapsible = T,
                          collapsed = F,
                          headerBorder = T,
                          width = 12,
                          withSpinner(
                            leafletOutput(NS(id, "exv1map"), height = "600px"),
                            type = 8,
                            color = "#f2c94e",
                            size = 0.5
                          ),
                          footer = list(tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                                        tags$h6(tags$b("Elaboração:"), "FAPESPA"))
                        ),
                        ## Tabela - Quantidade Produzida na Extração Vegetal, por Tipo de Produto Extrativo----
                        box(
                          title = textOutput(NS(id, "exv1txt2")),
                          collapsible = T,
                          collapsed = F,
                          width = 12,
                          headerBorder = T,
                          solidHeader = F,
                          withSpinner(
                            reactableOutput(NS(id, "exv1tab"), height = "400px"),
                            type = 8,
                            color = "#f2c94e",
                            size = 0.5
                          ),
                          footer = list(column(
                            11,
                            tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA")
                          ),
                          downset_ui(NS(id, "exv1_1")))
                        )
                      ),
                      fluidRow(
                        ## Gráfico - Quantidade Produzida na Extração Vegetal, por Tipo de Produto Extrativo----
                        box(
                          title = textOutput(NS(id, "exv1txt3")),
                          collapsible = T,
                          collapsed = F,
                          width = 12,
                          headerBorder = T,
                          solidHeader = F,
                          withSpinner(
                            echarts4rOutput(NS(id, "exv1graf")),
                            type = 8,
                            color = "#f2c94e",
                            size = 0.5
                          ),
                          footer = list(column(
                            11,
                            tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA")
                          ),
                          downset_ui(NS(id, "exv1_2")))
                        ),
                        ## Tabela - Quantidade Produzida na Extração Vegetal, por Tipo de Produto Extrativo----
                        box(
                          title = textOutput(NS(id, "exv1txt4")),
                          collapsible = T,
                          collapsed = F,
                          width = 12,
                          headerBorder = T,
                          solidHeader = F,
                          withSpinner(
                            reactableOutput(NS(id, "exv1tabcat"), height = "600px"),
                            type = 8,
                            color = "#f2c94e",
                            size = 0.5
                          ),
                          footer = list(column(
                            11,
                            tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA")
                          ),
                          downset_ui(NS(id, "exv1_3")))
                        )
                      )
                    ),
                    # 2 - Valor (Mil Reais) da Produção na Extração Vegetal, por Tipo de Produto Extrativo----
                    tabPanel(
                      "Valor (Mil Reais) da Produção na Extração Vegetal, por Tipo de Produto Extrativo",
                      panel(
                        ## Controle----
                        heading =
                          h4(style = "margin-bottom: 2px;margin-top: 2px;
                          font-weight: bold;color: #6D6D6D;",
                             "Valor (Mil Reais) da Produção na Extração Vegetal, por Tipo de Produto Extrativo"),
                        tags$div(
                          class = "seletor1",
                          pickerInput(
                            inputId = NS(id, "exv2ano"),
                            label = "Ano",
                            choices = sort(unique(exv2[["ano"]]), decreasing = T),
                            width = "100px"
                          )
                        ),
                        tags$div(
                          class = "seletor2",
                          pickerInput(
                            inputId = NS(id, "exv2ri"),
                            label = "Pará/Região de Integração",
                            choices = unique(exv2[["ri"]]),
                            width = "200px"
                          )
                        ),
                        tags$div(
                          class = "seletor2",
                          pickerInput(
                            inputId = NS(id, "exv2cat"),
                            label = "Produto",
                            choices = unique(exv2[["categoria"]]),
                            width = "200px"
                          )
                        )
                      ),
                      fluidRow(
                        ## Mapa - Valor (Mil Reais) da Produção na Extração Vegetal, por Tipo de Produto Extrativo----
                        box(
                          title = textOutput(NS(id, "exv2txt1")),
                          collapsible = T,
                          collapsed = F,
                          headerBorder = T,
                          width = 12,
                          withSpinner(
                            leafletOutput(NS(id, "exv2map"), height = "600px"),
                            type = 8,
                            color = "#f2c94e",
                            size = 0.5
                          ),
                          footer = list(tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                                        tags$h6(tags$b("Elaboração:"), "FAPESPA"))
                        ),
                        ## Tabela - Valor (Mil Reais) da Produção na Extração Vegetal, por Tipo de Produto Extrativo----
                        box(
                          title = textOutput(NS(id, "exv2txt2")),
                          collapsible = T,
                          collapsed = F,
                          width = 12,
                          headerBorder = T,
                          solidHeader = F,
                          withSpinner(
                            reactableOutput(NS(id, "exv2tab"), height = "400px"),
                            type = 8,
                            color = "#f2c94e",
                            size = 0.5
                          ),
                          footer = list(column(
                            11,
                            tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA")
                          ),
                          downset_ui(NS(id, "exv2_1")))
                        )
                      ),
                      fluidRow(
                        ## Gráfico - Valor (Mil Reais) da Produção na Extração Vegetal, por Tipo de Produto Extrativo----
                        box(
                          title = textOutput(NS(id, "exv2txt3")),
                          collapsible = T,
                          collapsed = F,
                          width = 12,
                          headerBorder = T,
                          solidHeader = F,
                          withSpinner(
                            echarts4rOutput(NS(id, "exv2graf")),
                            type = 8,
                            color = "#f2c94e",
                            size = 0.5
                          ),
                          footer = list(column(
                            11,
                            tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA")
                          ),
                          downset_ui(NS(id, "exv2_2")))
                        ),
                        ## Tabela - Valor (Mil Reais) da Produção na Extração Vegetal, por Tipo de Produto Extrativo----
                        box(
                          title = textOutput(NS(id, "exv2txt4")),
                          collapsible = T,
                          collapsed = F,
                          width = 12,
                          headerBorder = T,
                          solidHeader = F,
                          withSpinner(
                            reactableOutput(NS(id, "exv2tabcat"), height = "600px"),
                            type = 8,
                            color = "#f2c94e",
                            size = 0.5
                          ),
                          footer = list(column(
                            11,
                            tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA")
                          ),
                          downset_ui(NS(id, "exv2_3")))
                        )
                      )
                    )
                  )
                )))
}
# Função do modulo servidor
economia_exv_pa_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    #TÍTULOS----
    # 1 - Quantidade Produzida na Extração Vegetal, por Tipo de Produto Extrativo----
    ## Mapa - Quantidade Produzida na Extração Vegetal, por Tipo de Produto Extrativo----
    t11 <- reactive({
      if (input$exv1ri == "Pará") {
        paste0(
          "Quantidade Produzida na Extração Vegetal de ",
          input$exv1cat,
          ", ",
          input$exv1ri,
          " - ",
          input$exv1ano
        )
      } else {
        paste0(
          "Quantidade Produzida na Extração Vegetal de ",
          input$exv1cat,
          ", Região de Integração ",
          input$exv1ri,
          " - ",
          input$exv1ano
        )
      }
    })
    ## Tabela - Quantidade Produzida na Extração Vegetal, por Tipo de Produto Extrativo----
    t12 <- reactive({
      if (input$exv1ri == "Pará") {
        paste0(
          "Quantidade Produzida na Extração Vegetal de ",
          input$exv1cat,
          " dos Municípios, ",
          input$exv1ri,
          " - ",
          input$exv1ano
        )
      } else {
        paste0(
          "Quantidade Produzida na Extração Vegetal de ",
          input$exv1cat,
          " dos Municípios, Região de Integração ",
          input$exv1ri,
          " - ",
          input$exv1ano
        )
      }
    })
    ## Gráfico - Quantidade Produzida na Extração Vegetal, por Tipo de Produto Extrativo----
    t13 <- reactive({
      paste0(
        "Quantidade Produzida na Extração Vegetal de ",
        input$exv1cat,
        ", Pará",
        " - ",
        min(exv1$ano),
        " a ",
        max(exv1$ano)
      )
    })
    ## Tabela - Quantidade Produzida na Extração Vegetal, por Tipo de Produto Extrativo----
    t14 <- reactive({
      paste0(
        "Quantidade Produzida na Extração Vegetal, por Tipo de Produto Extrativo, Pará - ",
        min(exv1$ano),
        " a ",
        max(exv1$ano)
      )
    })
    # 2 - Valor (Mil Reais) da Produção na Extração Vegetal, por Tipo de Produto Extrativo----
    ## Mapa - Área Destinada à Colheita (Hectares)----
    t21 <- reactive({
      if (input$exv2ri == "Pará") {
        paste0(
          "Valor (Mil Reais) da Produção na Extração Vegetal de ",
          input$exv2cat,
          ", ",
          input$exv2ri,
          " - ",
          input$exv2ano
        )
      } else {
        paste0(
          "Valor (Mil Reais) da Produção na Extração Vegetal de ",
          input$exv2cat,
          ", Região de Integração ",
          input$exv2ri,
          " - ",
          input$exv2ano
        )
      }
    })
    ## Tabela - Valor (Mil Reais) da Produção na Extração Vegetal, por Tipo de Produto Extrativo----
    t22 <- reactive({
      if (input$exv2ri == "Pará") {
        paste0(
          "Valor (Mil Reais) da Produção na Extração Vegetal de ",
          input$exv2cat,
          " dos Municípios, ",
          input$exv2ri,
          " - ",
          input$exv2ano
        )
      } else {
        paste0(
          "Valor (Mil Reais) da Produção na Extração Vegetal de ",
          input$exv2cat,
          " dos Municípios, Região de Integração ",
          input$exv2ri,
          " - ",
          input$exv2ano
        )
      }
    })
    ## Gráfico - Valor (Mil Reais) da Produção na Extração Vegetal, por Tipo de Produto Extrativo----
    t23 <- reactive({
      paste0(
        "Valor (Mil Reais) da Produção na Extração Vegetal de ",
        input$exv2cat,
        ", Pará ",
        " - ",
        min(exv2$ano),
        " a ",
        max(exv2$ano)
      )
    })
    ## Tabela - Valor (Mil Reais) da Produção na Extração Vegetal, por Tipo de Produto Extrativo----
    t24 <- reactive({
      paste0(
        "Tabela - Valor (Mil Reais) da Produção na Extração Vegetal, por Tipo de Produto Extrativo, Pará - ",
        input$exv2ano
      )
    })
    #VISUALIZAÇÃO----
    # 1 - Quantidade Produzida na Extração Vegetal, por Tipo de Produto Extrativo----
    ## Mapa - Área Destinada à Colheita (Hectares)----
    output$exv1txt1 <- renderText({
      t11()
    })
    output$exv1map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$exv1ri == "Pará") {
        df <- exv1 %>%
          filter(localidade != "Pará",
                 ano == input$exv1ano,
                 categoria == input$exv1cat) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- exv1 %>%
          filter(localidade != "Pará",
                 ano == input$exv1ano,
                 categoria == input$exv1cat) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$exv1ri)
      }
      
      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }
      
      pal <-
        colorBin(
          c("#ffff81", "#fad254", "#f2c94e", "#ae520b", "#6b0000"),
          domain = x$valor,
          bins = bins
        )
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>%s:</b> %s ",
          x$name_muni,
          input$exv1cat,
          ifelse(
            is.na(x$valor),
            "Sem Produção",
            format(x$valor, big.mark = ".", decimal.mark = ",")
          )
        ) %>% lapply(htmltools::HTML)
      
      # Mapas com leafleft
      leaflet(x,
              options =
                leafletOptions(minZoom = 0,
                               maxZoom = 15)) %>%
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
          labelOptions =
            labelOptions(
              style =
                list("font-weight" = "normal",
                     padding = "3px 8px"),
              textsize = "15px",
              direction = "auto"
            )
        ) %>%
        addLegend(
          pal = pal,
          values = ~ valor,
          opacity = 0.7,
          title = input$exv1cat,
          position = "bottomright",
          na.label = "Sem Produção",
          labFormat = labelFormat_decimal(
            big.mark = ".",
            decimal.mark = ",",
            digits = 0
          )
        )
    })
    ## Tabela - Quantidade Produzida na Extração Vegetal, por Tipo de Produto Extrativo----
    output$exv1txt2 <- renderText({
      t12()
    })
    # Renderização da base exv1
    output$exv1tab <- renderReactable({
      if (input$exv1ri == "Pará") {
        x <- exv1 %>%
          filter(localidade != "Pará",
                 ano == input$exv1ano,
                 categoria == input$exv1cat) %>%
          select(ri, localidade, valor)
      } else {
        x <- exv1 %>%
          filter(localidade != "Pará",
                 ano == input$exv1ano,
                 categoria == input$exv1cat) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$exv1ri)
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
            name = input$exv1cat,
            na = "-",
            format = colFormat(separators = T,
                               locales = "pt-BR")
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
    ## Gráfico - Quantidade Produzida na Extração Vegetal, por Tipo de Produto Extrativo----
    output$exv1txt3 <- renderText({
      t13()
    })
    output$exv1graf <- renderEcharts4r({
      exv1 %>%
        filter(categoria == input$exv1cat, localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#f2c94e",
          name = input$exv1cat,
          legend = F,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
          itemStyle = list(barBorderRadius = 5)
        ) %>%
        e_tooltip(
          trigger = "axis",
          formatter = e_tooltip_pointer_formatter("decimal", digits = 0, locale = "pt-Br"),
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
          name = input$exv1cat,
          nameTextStyle =
            list(fontWeight = "bold", fontSize = 14),
          scale = T,
          axisLabel = list(
            formatter = htmlwidgets::JS(
              "
              function (value, index) {
              return value.toLocaleString('pt-BR',
              { minimumFractionDigits: 0, maximumFractionDigits: 0 });
              }
            "
            )
          )
        ) %>%
        e_locale("pt-Br") %>%
        e_datazoom(toolbox = F, fillerColor = "#E5F5F9") %>%
        e_grid(show = T)
    })
    ## Tabela - Quantidade Produzida na Extração Vegetal, por Tipo de Produto Extrativo----
    output$exv1txt4 <- renderText({
      t14()
    })
    output$exv1tabcat <- renderReactable({
      x <- exv1 %>%
        filter(localidade == "Pará") %>%
        select(categoria, ano, valor) %>%
        pivot_wider(names_from = ano, values_from = valor)
      x %>% reactable(
        defaultPageSize = 10,
        striped = FALSE,
        highlight = TRUE,
        bordered = TRUE,
        outlined = TRUE,
        resizable = TRUE,
        showSortable = TRUE,
        pagination = F,
        columns = list(categoria = colDef(name = "Produtos", width = 250)),
        defaultColDef = colDef(
          footerStyle = list(fontWeight = "bold"),
          headerStyle = list(background = "#f7f7f8"),
          format = colFormat(separators = T, locales = "pt-BR")
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
    # 2 - Valor (Mil Reais) da Produção na Extração Vegetal, por Tipo de Produto Extrativo----
    ## Mapa - Área Destinada à Colheita (Hectares)----
    output$exv2txt1 <- renderText({
      t21()
    })
    output$exv2map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$exv2ri == "Pará") {
        df <- exv2 %>%
          filter(localidade != "Pará",
                 ano == input$exv2ano,
                 categoria == input$exv2cat) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- exv2 %>%
          filter(localidade != "Pará",
                 ano == input$exv2ano,
                 categoria == input$exv2cat) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$exv2ri)
      }
      
      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }
      
      pal <-
        colorBin(
          c("#ffff81", "#fad254", "#f2c94e", "#ae520b", "#6b0000"),
          domain = x$valor,
          bins = bins
        )
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Valor (Mil reais):</b> %s ",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Sem Produção",
            format(x$valor, big.mark = ".", decimal.mark = ",")
          )
        ) %>% lapply(htmltools::HTML)
      # Mapas com leafleft
      leaflet(x,
              options =
                leafletOptions(minZoom = 0,
                               maxZoom = 15)) %>%
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
          values = ~ valor,
          opacity = 0.7,
          title = paste0("Valor (Mil Reais)"),
          position = "bottomright",
          na.label = "Sem Produção",
          labFormat = labelFormat_decimal(
            big.mark = ".",
            decimal.mark = ",",
            digits = 2
          )
        )
    })
    ## Tabela - Valor (Mil Reais) da Produção na Extração Vegetal, por Tipo de Produto Extrativo----
    output$exv2txt2 <- renderText({
      t22()
    })
    # Renderização da base exv2
    output$exv2tab <- renderReactable({
      if (input$exv2ri == "Pará") {
        x <- exv2 %>%
          filter(localidade != "Pará",
                 ano == input$exv2ano,
                 categoria == input$exv2cat) %>%
          select(ri, localidade, valor)
      } else {
        x <- exv2 %>%
          filter(localidade != "Pará",
                 ano == input$exv2ano,
                 categoria == input$exv2cat) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$exv2ri)
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
            name = paste0(input$exv2cat, " Valor (Mil Reais)"),
            na = "-",
            format = colFormat(separators = T,
                               locales = "pt-BR")
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
    ## Gráfico - Valor (Mil Reais) da Produção na Extração Vegetal, por Tipo de Produto Extrativo----
    output$exv2txt3 <- renderText({
      t23()
    })
    output$exv2graf <- renderEcharts4r({
      exv2 %>%
        filter(categoria == input$exv2cat, localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#f2c94e",
          name = paste0("Valor (Mil Reais)"),
          legend = F,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
          itemStyle = list(barBorderRadius = 5)
        ) %>%
        e_tooltip(
          trigger = "axis",
          formatter = e_tooltip_pointer_formatter("decimal", digits = 0, locale = "pt-Br"),
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
          name = "Valor (Mil Reais)",
          nameTextStyle =
            list(fontWeight = "bold", fontSize = 14),
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
    ## Tabela - Valor (Mil Reais) da Produção na Extração Vegetal, por Tipo de Produto Extrativo----
    output$exv2txt4 <- renderText({
      t24()
    })
    output$exv2tabcat <- renderReactable({
      x <- exv2 %>%
        filter(localidade == "Pará", ano == input$exv2ano) %>%
        select(categoria, valor) %>%
        arrange(desc(valor)) %>%
        mutate(Percentual = (valor / sum(valor)) * 100)
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
          categoria = colDef(name = "Produtos"),
          valor = colDef(
            width = 200,
            name = "Valor (Mil Reais)",
            format = colFormat(separators = T,
                               locales = "pt-BR")
          ),
          Percentual = colDef(
            name = "Percentual(%)",
            format = colFormat(
              digits = 2,
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
    # DOWNLOADS----
    ## 1 - Quantidade Produzida na Extração Vegetal, por Tipo de Produto Extrativo----
    ## Tabela - Quantidade Produzida na Extração Vegetal, por Tipo de Produto Extrativo----
    # Filtra os dados
    exv1_1 <- reactive({
      if (input$exv1ri == "Pará") {
        x <- exv1 %>%
          filter(localidade != "Pará",
                 ano == input$exv1ano,
                 categoria == input$exv1cat)
      } else {
        x <- exv1 %>%
          filter(
            localidade != "Pará",
            ano == input$exv1ano,
            categoria == input$exv1cat,
            ri == input$exv1ri
          )
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(exv1_1(), {
      t12()
      downset_Server("exv1_1", exv1_1(), t12())
    })
    ## Gráfico de linha - Quantidade Produzida na Extração Vegetal, por Tipo de Produto Extrativo----
    # Filtra os dados
    exv1_2 <- reactive({
      exv1 %>%
        filter(categoria == input$exv1cat, localidade == "Pará")
      
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(exv1_2(), {
      t13()
      downset_Server("exv1_2", exv1_2(), t13())
    })
    ## Tabela - Quantidade Produzida na Extração Vegetal, por Tipo de Produto Extrativo----
    # Filtra os dados
    exv1_3 <- reactive({
      x <- exv1 %>%
        filter(localidade == "Pará") %>%
        pivot_wider(names_from = ano, values_from = valor)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(exv1_3(), {
      t14()
      downset_Server("exv1_3", exv1_3(), t14())
    })
    
    ## 2 - Valor (Mil Reais) da Produção na Extração Vegetal, por Tipo de Produto Extrativo----
    ## Tabela - Valor (Mil Reais) da Produção na Extração Vegetal, por Tipo de Produto Extrativo----
    # Filtra os dados
    exv2_1 <- reactive({
      if (input$exv2ri == "Pará") {
        x <- exv2 %>%
          filter(localidade != "Pará",
                 ano == input$exv2ano,
                 categoria == input$exv2cat)
      } else {
        x <- exv2 %>%
          filter(
            localidade != "Pará",
            ano == input$exv2ano,
            categoria == input$exv2cat,
            ri == input$exv2ri
          )
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(exv2_1(), {
      t22()
      downset_Server("exv2_1", exv2_1(), t22())
    })
    ## Gráfico - Valor (Mil Reais) da Produção na Extração Vegetal, por Tipo de Produto Extrativo----
    # Filtra os dados
    exv2_2 <- reactive({
      exv2 %>%
        filter(categoria == input$exv2cat, localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(exv2_2(), {
      t23()
      downset_Server("exv2_2", exv2_2(), t23())
    })
    ## Tabela - Valor (Mil Reais) da Produção na Extração Vegetal, por Tipo de Produto Extrativo----
    # Filtra os dados
    exv2_3 <- reactive({
      x <- exv2 %>%
        filter(localidade == "Pará", ano == input$exv2ano) %>%
        arrange(desc(valor)) %>%
        mutate(Percentual = (valor / sum(valor)) * 100)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(exv2_3(), {
      t24()
      downset_Server("exv2_3", exv2_3(), t24())
    })
  })
}

# Play do Módulo
# ui <- dashboardPage(header = dashboardHeader(),
#                     sidebar = dashboardSidebar(),
#                     body = dashboardBody(fluidPage(economia_exv_pa_ui("economia_exv_pa"))))
# 
# 
# server <- function(input, output) {
#   economia_exv_pa_Server("economia_exv_pa")
# }
# 
# shinyApp(ui, server)
