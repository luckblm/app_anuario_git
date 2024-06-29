# Funções de módulo de Economia - Extração Vegetal - Municipal
# Função de UI
economia_exv_mp_ui <- function(id) {
  fluidPage(# CAMINHO DO ARQUIVO CSS----
            tags$head(
              tags$link(rel = "stylesheet", type = "text/css", href = "estilo.css")
            ),
            # Lista de Navegação lateral----
            div(class = "navbar_economia",
                navbarPage(
                  tags$b("Extração Vegetal - Municípios"),
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
                          class = "seletor3",
                          pickerInput(
                            inputId = NS(id, "exv1cat"),
                            label = "Produto",
                            choices = unique(exv1[["categoria"]]),
                            width = "250px"
                          )
                        ),
                        tags$div(
                          class = "seletor2",
                          pickerInput(
                            inputId = NS(id, "exv1muni"),
                            label = "Municípios",
                            choices = NULL,
                            width = "200px"
                          )
                        )
                      ),
                      fluidRow(
                        ## Gráfico - Quantidade Produzida na Extração Vegetal, por Tipo de Produto Extrativo----
                        box(
                          title = textOutput(NS(id, "exv1txt1")),
                          collapsible = T,
                          collapsed = F,
                          headerBorder = T,
                          width = 12,
                          # Comparar municípios
                          pickerInput(
                            inputId = NS(id, "exv1municomp"),
                            label = "Comparar Município",
                            choices = NULL,
                            width = "200px",
                            options = list(`none-selected-text` = "Selecione um município")
                          ),
                          withSpinner(
                            echarts4rOutput(NS(id, "exv1graf")),
                            type = 8,
                            color = "#f2c94e",
                            size = 0.5
                          ),
                          footer = list(column(
                            11,
                            tags$h6(tags$b("Fonte:"), "IBGE/PEVS"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA")
                          ),
                          downset_ui(NS(id, "exv1_1")))
                        )
                      ),
                      fluidRow(
                        ## Tabela - Quantidade Produzida na Extração Vegetal, por Tipo de Produto Extrativo por produto----
                        box(
                          title = textOutput(NS(id, "exv1txt2")),
                          collapsible = T,
                          collapsed = F,
                          headerBorder = T,
                          width = 12,
                          withSpinner(
                            reactableOutput(NS(id, "exv1tab")),
                            type = 8,
                            color = "#f2c94e",
                            size = 0.5
                          ),
                          footer = list(column(
                            11,
                            tags$h6(tags$b("Fonte:"), "IBGE/PEVS"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA")
                          ),
                          downset_ui(NS(id, "exv1_2")))
                        )
                      )
                    ),
                    # 2 - Valor (Mil Reais) da Produção na Extração Vegetal, por Tipo de Produto Extrativo----
                    tabPanel(
                      "Valor (Mil Reais) da Produção na Extração Vegetal, por Tipo de Produto Extrativo",
                      panel(## Controle----
                            heading =
                              h4(style = "margin-bottom: 2px;margin-top: 2px;
                              font-weight: bold;color: #6D6D6D;",
                                 "Valor (Mil Reais) da Produção na Extração Vegetal, por Tipo de Produto Extrativo"),
                            tags$div(
                              class = "seletor2",
                              pickerInput(
                                inputId = NS(id, "exv2muni"),
                                label = "Municípios",
                                choices = exv2 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                                width = "200px"
                              )
                            )),
                      fluidRow(
                        ## Gráfico - Valor (Mil Reais) da Produção na Extração Vegetal, por Tipo de Produto Extrativo----
                        box(
                          title = textOutput(NS(id, "exv2txt1")),
                          collapsible = T,
                          collapsed = F,
                          headerBorder = T,
                          width = 12,
                          pickerInput(
                            inputId = NS(id, "exv2ano"),
                            label = "Ano",
                            choices = sort(unique(exv2[["ano"]]), decreasing = T),
                            width = "100px"
                          ),
                          withSpinner(
                            echarts4rOutput(NS(id, "exv2graf"), height = "600px"),
                            type = 8,
                            color = "#f2c94e",
                            size = 0.5
                          ),
                          footer = list(column(
                            11,
                            tags$h6(tags$b("Fonte:"), "IBGE/PEVS"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA")
                          ),
                          downset_ui(NS(id, "exv2_1")))
                        ),
                        ## Gráfico - Valor (Mil Reais) da Produção na Extração Vegetal, por Tipo de Produto Extrativo - Total----
                        box(
                          title = textOutput(NS(id, "exv2txt2")),
                          collapsible = T,
                          collapsed = F,
                          headerBorder = T,
                          width = 12,
                          pickerInput(
                            inputId = NS(id, "exv2municomp"),
                            label = "Comparar Município",
                            choices = NULL,
                            width = "200px",
                            options = list(`none-selected-text` = "Selecione um município")
                          ),
                          withSpinner(
                            echarts4rOutput(NS(id, "exv2graf1")),
                            type = 8,
                            color = "#f2c94e",
                            size = 0.5
                          ),
                          footer = list(column(
                            11,
                            tags$h6(tags$b("Fonte:"), "IBGE/PEVS"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA")
                          ),
                          downset_ui(NS(id, "exv2_2")))
                        )
                      ),
                      fluidRow(
                        ## Tabela - Valor (Mil Reais) da Produção na Extração Vegetal, por Tipo de Produto Extrativo por produto----
                        box(
                          title = textOutput(NS(id, "exv2txt3")),
                          collapsible = T,
                          collapsed = F,
                          headerBorder = T,
                          width = 12,
                          withSpinner(
                            reactableOutput(NS(id, "exv2tab"), height = "400px"),
                            type = 8,
                            color = "#f2c94e",
                            size = 0.5
                          ),
                          footer = list(column(
                            11,
                            tags$h6(tags$b("Fonte:"), "IBGE/PEVS"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA")
                          ),
                          downset_ui(NS(id, "exv2_3")))
                        ),
                        ## Tabela - Valor (Mil Reais) da Produção na Extração Vegetal, por Tipo de Produto Extrativo Total por municipio----
                        box(
                          title = textOutput(NS(id, "exv2txt4")),
                          collapsible = T,
                          collapsed = F,
                          headerBorder = T,
                          width = 12,
                          pickerInput(
                            inputId = NS(id, "exv2ano2"),
                            label = "Ano",
                            choices = sort(unique(exv2[["ano"]]), decreasing = T),
                            width = "100px"
                          ),
                          withSpinner(
                            reactableOutput(NS(id, "exv2tab1"),height = "400px"),
                            type = 8,
                            color = "#f2c94e",
                            size = 0.5
                          ),
                          footer = list(column(
                            11,
                            tags$h6(tags$b("Fonte:"), "IBGE/PEVS"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA")
                          ),
                          downset_ui(NS(id, "exv2_4")))
                        )
                      )
                    )
                  )
                )))
}
# Função do modulo servidor
economia_exv_mp_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    #TÍTULO----
    # 1 - Quantidade Produzida na Extração Vegetal, por Tipo de Produto Extrativo----
    ## Gráfico - Quantidade Produzida na Extração Vegetal, por Tipo de Produto Extrativo (Hectares)----
    t11 <- reactive({
      req(input$exv1municomp)
      if (input$exv1municomp == "Seleconar Município") {
        paste0(
          "Quantidade Produzida na Extração Vegetal de ",
          input$exv1cat,
          " - ",
          input$exv1muni,
          " - ",
          min(exv1$ano),
          " a ",
          max(exv1$ano)
        )
      } else {
        paste0(
          "Quantidade Produzida na Extração Vegetal de ",
          input$exv1cat,
          ", ",
          input$exv1muni,
          " x ",
          input$exv1municomp,
          " - ",
          min(exv1$ano),
          " a ",
          max(exv1$ano)
        )
      }
    })
    
    ## Tabela - Quantidade Produzida na Extração Vegetal, por Tipo de Produto Extrativo por produto----
    t12 <- reactive({
      paste0(
        "Quantidade Produzida na Extração Vegetal por Tipo de Produto, ",
        input$exv1muni,
        " - ",
        min(exv1$ano),
        " a ",
        max(exv1$ano)
      )
    })
    # 2 - Valor (Mil Reais) da Produção na Extração Vegetal, por Tipo de Produto Extrativo----
    
    ## Gráfico Barras - Valor (Mil Reais) da Produção na Extração Vegetal, por Tipo de Produto Extrativo ----
    t21 <- reactive({
      paste0(
        "Valor (Mil Reais) da Produção na Extração Vegetal, por Tipo de Produto Extrativo, ",
        input$exv2muni,
        " - ",
        input$exv2ano
      )
    })

    ## Gráfico - Valor (Mil Reais) da Produção na Extração Vegetal, por Tipo de Produto Extrativo  Total----
    t22 <- reactive({
      req(input$exv2municomp)
      if (input$exv2municomp == "Seleconar Município") {
        paste0(
          "Valor Total (Mil Reais) da Produção na Extração Vegetal, ",
          input$exv2muni,
          " - ",
          min(exv2$ano),
          " a ",
          max(exv2$ano)
        )
      } else {
        paste0(
          "Valor Total (Mil Reais) da Produçãona Extração Vegetal, ",
          input$exv2muni,
          " x ",
          input$exv2municomp,
          " - ",
          min(exv2$ano),
          " a ",
          max(exv2$ano)
        )
      }
    })
    
    ## Tabela - Valor (Mil Reais) da Produção na Extração Vegetal, por Tipo de Produto Extrativo por produto----
    t23 <- reactive({
      paste0(
        "Valor (Mil Reais) da Produção na Extração Vegetal, por Tipo de Produto Extrativo, ",
        input$exv2muni,
        " - ",
        min(exv2$ano),
        " a ",
        max(exv2$ano)
      )
    })
    ## Tabela - Valor (Mil Reais) da Produção na Extração Vegetal, por Tipo de Produto Extrativo por produto----
    t24 <- reactive({
      ri <- exv2 %>%
        filter(localidade == input$exv2muni) %>%
        select(ri) %>%
        pull() %>%
        unique()
      paste0(
        "Valor Total (Mil Reais) da Produção na Extração Vegetal dos Municípios, Região de Integração ",
        ri,
        " - ",
        input$exv2ano2
      )
    })

    #VISUALIZAÇÃO----
    # 1 - Quantidade Produzida na Extração Vegetal, por Tipo de Produto Extrativo----
    ## Controle
    exv1loc <- reactive({
      exv1 %>%
        filter(categoria == input$exv1cat,
               localidade != "Pará",
               valor > 0) %>%
        select(localidade)
    })
    observeEvent(exv1loc(), {
      choices <- unique(pull(exv1loc()))
      updatePickerInput(inputId = "exv1muni", choices = choices, session)
    })
    exv1locomp <- reactive({
      req(input$exv1muni)
      x <- exv1 %>%
        filter(categoria == input$exv1cat,
               localidade != "Pará",
               valor > 0) %>%
        select(localidade)
      x <- x %>% filter(localidade != input$exv1muni)
    })
    observeEvent(exv1locomp(), {
      choices <- unique(pull(exv1locomp()))
      updatePickerInput(
        inputId = "exv1municomp",
        choices = c("Seleconar Município", choices),
        session
      )
    })
    ## Gráfico - Quantidade Produzida na Extração Vegetal, por Tipo de Produto Extrativo (Hectares)----
    output$exv1txt1 <- renderText({
      t11()
    })
    
    output$exv1graf <- renderEcharts4r({
      req(input$exv1municomp)
      if (input$exv1municomp == "Seleconar Município") {
        a <- exv1 %>%
          filter(localidade == input$exv1muni,
                 categoria == input$exv1cat)
        a %>%
          e_charts(x = ano) %>%
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
              return value.toLocaleString('pt-BR', { minimumFractionDigits: 0, maximumFractionDigits: 0 });
              }
            "
              )
            )
          ) %>%
          e_locale("pt-Br") %>%
          e_datazoom(toolbox = F, fillerColor = "#E5F5F9") %>%
          e_grid(show = T)
      } else {
        a <- exv1 %>%
          filter(localidade == input$exv1muni,
                 categoria == input$exv1cat)
        b <- exv1 %>%
          filter(localidade == input$exv1municomp,
                 categoria == input$exv1cat)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$exv1muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$exv1municomp,
            legend = T,
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
              return value.toLocaleString('pt-BR', { minimumFractionDigits: 0, maximumFractionDigits: 0 });
              }
            "
              )
            )
          ) %>%
          e_locale("pt-Br") %>%
          e_datazoom(toolbox = F, fillerColor = "#E5F5F9") %>%
          e_grid(show = T)
      }
    })
    
    ## Tabela - Quantidade Produzida na Extração Vegetal, por Tipo de Produto Extrativo por produto----
    output$exv1txt2 <- renderText({
    t12()  
    })
    output$exv1tab <- renderReactable({
      req(input$exv1muni)
      x <- exv1 %>%
        filter(localidade == input$exv1muni) %>%
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
          na = "0",
          headerStyle = list(background = "#f7f7f8"),
          format = colFormat(separators = T)
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
    
    ## Gráfico Barras - Valor (Mil Reais) da Produção na Extração Vegetal, por Tipo de Produto Extrativo ----
    output$exv2txt1 <- renderText({
    t21()  
    })
    output$exv2graf <- renderEcharts4r({
      a <- exv2 %>%
        filter(localidade == input$exv2muni, ano == input$exv2ano) %>%
        arrange(valor)
      a %>%
        e_charts(categoria) %>%
        e_bar(
          serie = valor,
          color = "#f2c94e",
          name = "Valor em (Mil Reais)",
          legend = F,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
          itemStyle = list(barBorderRadius = 0)
        ) %>%
        e_labels(
          position = "right",
          fontWeight = "bold",
          formatter = htmlwidgets::JS(
            glue::glue(
              "function(params) {return Intl.NumberFormat('pt-BR', { style: 'decimal'}).format(params.value[{{0}}]);}",
              .open = "{{",
              .close = "}}"
            )
          )
        ) %>%
        e_y_axis(
          name = " Valor(em mil reais)",
          nameTextStyle =
            list(
              fontWeight = "bold",
              padding = c(30, 0, 0, 0),
              fontSize = 14
            ),
          scale = T,
          splitNumber = 15,
          nameLocation = "middle",
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
        e_grid(
          show = T,
          width = "80%",
          height = "80%",
          left = "10%"
        ) %>%
        e_flip_coords()
    })
    
    ## Gráfico - Valor (Mil Reais) da Produção na Extração Vegetal, por Tipo de Produto Extrativo  Total----
    # Comparador_________________________________________________
    exv2locomp <- reactive({
      req(input$exv2muni)
      x <- exv2 %>%
        filter(localidade != "Pará", valor > 0) %>%
        select(localidade)
      x <- x %>% filter(localidade != input$exv2muni)
    })
    observeEvent(exv2locomp(), {
      choices <- unique(pull(exv2locomp()))
      updatePickerInput(
        inputId = "exv2municomp",
        choices = c("Seleconar Município", choices),
        session
      )
    })
    # ____________________________________________________________
    output$exv2txt2 <- renderText({
    t22()  
    })
    
    output$exv2graf1 <- renderEcharts4r({
      req(input$exv2municomp)
      if (input$exv2municomp == "Seleconar Município") {
        a <- exv2 %>%
          filter(localidade == input$exv2muni) %>%
          group_by(ano) %>%
          summarise(valor = sum(valor))
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f2c94e",
            name = "Valor em (Mil Reais)",
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
            name = "Ano",
            splitLine = list(show = T),
            nameTextStyle = list(fontWeight = "bold", fontSize = 14)
          ) %>%
          e_y_axis(
            name = "Valor em (Mil Reais)",
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
      } else {
        a <- exv2 %>%
          filter(localidade == input$exv2muni) %>%
          group_by(ano) %>%
          summarise(valor = sum(valor))
        b <- exv2 %>%
          filter(localidade == input$exv2municomp) %>%
          group_by(ano) %>%
          summarise(valor = sum(valor))
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$exv2muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$exv2municomp,
            legend = T,
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
            name = "Ano",
            splitLine = list(show = T),
            nameTextStyle = list(fontWeight = "bold", fontSize = 14)
          ) %>%
          e_y_axis(
            name = "Valor em (Mil Reais)",
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
      }
    })
    ## Tabela - Valor (Mil Reais) da Produção na Extração Vegetal, por Tipo de Produto Extrativo por produto----
    output$exv2txt3 <- renderText({
    t23()  
    })
    output$exv2tab <- renderReactable({
      req(input$exv2muni)
      x <- exv2 %>%
        filter(localidade == input$exv2muni) %>%
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
        columns = list(
          categoria = colDef(name = "Produtos", width = 200)),
        defaultColDef = colDef(
          footerStyle = list(fontWeight = "bold"),
          na = "0",
          headerStyle = list(background = "#f7f7f8"),
          format = colFormat(separators = T)
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
    ## Tabela - Valor (Mil Reais) da Produção na Extração Vegetal, por Tipo de Produto Extrativo por produto----
    output$exv2txt4 <- renderText({
    t24()  
    })
    output$exv2tab1 <- renderReactable({
      req(input$exv2muni)
      ris <- exv2 %>%
        filter(localidade == input$exv2muni) %>%
        select(ri) %>%
        pull()
      x <- exv2 %>%
        filter(ano == input$exv2ano2,
               localidade != "Pará",
               ri == ris) %>%
        group_by(ri, localidade) %>%
        summarise(valor = sum(valor), .groups = "drop")
      x <- x %>% arrange(desc(valor))
      x <- x %>%
        mutate(posicao = row.names(.)) %>%
        select(posicao, ri, localidade, valor)
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
          localidade = colDef(name = "Múnicípios"),
          valor = colDef(
            name = "Valor Total(em Mil Reais)",
            format = colFormat(separators = T),
            cell = data_bars(
              x,
              text_position = "above",
              box_shadow = TRUE,
              align_bars = "right",
              number_fmt = scales::number_format(big.mark = ".",
                                                 decimal.mark = ",")
            )
          ),
          posicao = colDef(name = "nº", width = 50)
        ),
        defaultColDef = colDef(
          footerStyle = list(fontWeight = "bold"),
          na = "0",
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
    # DOWNLOAD----
    ## 1 - Quantidade Produzida na Extração Vegetal, por Tipo de Produto Extrativo----
    ## Gráfico de linha - Quantidade Produzida na Extração Vegetal, por Tipo de Produto Extrativo----
    # Filtra os dados
    exv1_1 <- reactive({
      req(input$exv1municomp)
      if (input$exv1municomp == "Seleconar Município") {
        a <- exv1 %>%
          filter(localidade == input$exv1muni,
                 categoria == input$exv1cat)
      } else{
        a <- exv1 %>%
          filter(localidade == input$exv1muni,
                 categoria == input$exv1cat)
        b <- exv1 %>%
          filter(localidade == input$exv1municomp,
                 categoria == input$exv1cat)
        df <- rbind(a, b)
      }
      
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(exv1_1(), {
      t11()
      downset_Server("exv1_1", exv1_1(), t11())
    })
    ## Tabela - Quantidade Produzida na Extração Vegetal, por Tipo de Produto Extrativo por produto----
    # Filtra os dados
    exv1_2 <- reactive({
      req(input$exv1muni)
      x <- exv1 %>%
        filter(localidade == input$exv1muni) %>%
        pivot_wider(names_from = ano, values_from = valor)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(exv1_2(), {
      t12()
      downset_Server("exv1_2", exv1_2(), t12())
    })
    
    ## 2 - Valor (Mil Reais) da Produção na Extração Vegetal, por Tipo de Produto Extrativo----
    ## Gráfico1 de Barras - Valor (Mil Reais) da Produção na Extração Vegetal, por Tipo de Produto Extrativo----
    # Filtra os dados
    exv2_1 <- reactive({
      a <- exv2 %>%
        filter(localidade == input$exv2muni, ano == input$exv2ano) %>%
        arrange(valor)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(exv2_1(), {
      t21()
      downset_Server("exv2_1", exv2_1(), t21())
    })
    ## Gráfico2 - Valor (Mil Reais) total da Produção na Extração Vegetal, por Tipo de Produto Extrativo - Total----
    # Filtra os dados
    exv2_2 <- reactive({
      req(input$exv2municomp)
      if (input$exv2municomp == "Seleconar Município") {
        a <- exv2 %>%
          filter(localidade == input$exv2muni) %>%
          group_by(
            tematica,
            subtema,
            indicador,
            ri,
            localidade,
            ano
          ) %>%
          summarise(valor = sum(valor),
                    .groups = "drop")
      } else {
        a <- exv2 %>%
          filter(localidade == input$exv2muni) %>%
          group_by(
            tematica,
            subtema,
            indicador,
            ri,
            localidade,
            ano
          ) %>%
          summarise(valor = sum(valor),
                    .groups = "drop")
        b <- exv2 %>%
          filter(localidade == input$exv2municomp) %>%
          group_by(
            tematica,
            subtema,
            indicador,
            ri,
            localidade,
            ano
          ) %>%
          summarise(valor = sum(valor),
                    .groups = "drop")
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(exv2_2(), {
      t22()
      downset_Server("exv2_2", exv2_2(), t22())
    })
    ## Tabela1 - Valor (Mil Reais) da Produção na Extração Vegetal, por Tipo de Produto Extrativo por produto----
    # Filtra os dados
    exv2_3 <- reactive({
      req(input$exv2muni)
      x <- exv2 %>%
        filter(localidade == input$exv2muni) %>%
        pivot_wider(names_from = ano, values_from = valor)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(exv2_3(), {
      t23()
      downset_Server("exv2_3", exv2_3(), t23())
    })
    ## Tabela2 - Valor (Mil Reais) da Produção na Extração Vegetal, por Tipo de Produto Extrativo Total por municipio----
    # Filtra os dados
    exv2_4 <- reactive({
      req(input$exv2muni)
      ris <- exv2 %>%
        filter(localidade == input$exv2muni) %>%
        select(ri) %>%
        pull()
      x <- exv2 %>%
        filter(ano == input$exv2ano2,
               localidade != "Pará",
               ri == ris) %>%
        group_by(
          tematica,
          subtema,
          indicador,
          ri,
          localidade,
          ano
        ) %>%
        summarise(valor = sum(valor),
                  .groups = "drop") %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(exv2_4(), {
      t24()
      downset_Server("exv2_4", exv2_4(), t24())
    })
  })
}
# # Play do Módulo
# ui <- dashboardPage(header = dashboardHeader(),
#                     sidebar = dashboardSidebar(),
#                     body = dashboardBody(fluidPage(economia_exv_mp_ui("economia_exv_mp"))))
# 
# 
# server <- function(input, output) {
#   economia_exv_mp_Server("economia_exv_mp")
# }
# 
# shinyApp(ui, server)
