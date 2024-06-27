# Funções de módulo de Meio Ambiente - Municipal
# Função de UI
meio_ambiente_mp_ui <- function(id) {
  fluidPage(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "estilo.css")
    ),
    # Lista de Navegação lateral----
    div(
      class = "navbar_ambiente",
      navbarPage(
        tags$b("Meio Ambiente - Municípios"),
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
                  inputId = NS(id, "mab1muni"),
                  label = "Município",
                  choices = mab1 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico de linha - Desflorestamento Acumulado (km²)----
              box(
                title = textOutput(NS(id, "mab1txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                # Comparar municípios
                pickerInput(
                  inputId = NS(id, "mab1municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "mab1graf")),
                  type = 8,
                  color = "#35aa00",
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
              )
            ),
            fluidRow(
              ## Tabela - Desflorestamento Acumulado (km²)----
              box(
                title = textOutput(NS(id, "mab1txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "mab1ano"),
                  label = "Ano",
                  choices = sort(unique(mab1[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "mab1tab2"),height = "400px"),
                  type = 8,
                  color = "#35aa00",
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
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "mab2muni"),
                  label = "Município",
                  choices = mab2 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico de linha - Incremento (Desflorestamento km²)----
              box(
                title = textOutput(NS(id, "mab2txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                # Comparar municípios
                pickerInput(
                  inputId = NS(id, "mab2municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "mab2graf")),
                  type = 8,
                  color = "#35aa00",
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
              )
            ),
            fluidRow(
              ## Tabela - Incremento (Desflorestamento km²)----
              box(
                title = textOutput(NS(id, "mab2txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "mab2ano"),
                  label = "Ano",
                  choices = sort(unique(mab2[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "mab2tab2"),height = "400px"),
                  type = 8,
                  color = "#35aa00",
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
          # 3 - Área de Floresta (km²) e Hidrografia (km²)----
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
              pickerInput(
                inputId = NS(id, "mab3muniA"),
                label = "Município",
                choices = mab3 %>% 
                  filter(localidade != "Pará",
                         categoria == "Área de Floresta (km²)") %>% 
                  pull(localidade) %>% unique(),
                width = "200px"
              )
            )
          ),
          fluidRow(
            ## Gráfico de linha - Área de Floresta (km²)----
            box(
              title = textOutput(NS(id, "mab3txt1A")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              # Comparar municípios
              pickerInput(
                inputId = NS(id, "mab3municompA"),
                label = "Comparar Município",
                choices = NULL,
                width = "200px",
                options = list(`none-selected-text` = "Selecione um município")
              ),
              withSpinner(
                echarts4rOutput(NS(id, "mab3grafA")),
                type = 8,
                color = "#35aa00",
                size = 0.5
              ),
              footer = list(
                column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INPE/PRODES"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "mab3_1A"))
              )
            )
          ),
          fluidRow(
            ## Tabela - Área de Floresta (km²)----
            box(
              title = textOutput(NS(id, "mab3txt2A")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              pickerInput(
                inputId = NS(id, "mab3anoA"),
                label = "Ano",
                choices = mab3 %>% filter(categoria == "Área de Floresta (km²)") %>% 
                  pull(ano) %>% unique() %>% sort(decreasing = T),
                width = "100px"
              ),
              withSpinner(
                reactableOutput(NS(id, "mab3tabA"),height = "400px"),
                type = 8,
                color = "#35aa00",
                size = 0.5
              ),
              footer = list(
                column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INPE/PRODES"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "mab3_2A"))
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
              pickerInput(
                inputId = NS(id, "mab3muniH"),
                label = "Município",
                choices = mab3 %>% 
                  filter(localidade != "Pará",
                         categoria == "Hidrografia (km²)") %>% 
                  pull(localidade) %>% unique(),
                width = "200px"
              )
            )
          ),
          fluidRow(
            ## Gráfico de linha - Hidrografia (km²)----
            box(
              title = textOutput(NS(id, "mab3txt1H")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              # Comparar municípios
              pickerInput(
                inputId = NS(id, "mab3municompH"),
                label = "Comparar Município",
                choices = NULL,
                width = "200px",
                options = list(`none-selected-text` = "Selecione um município")
              ),
              withSpinner(
                echarts4rOutput(NS(id, "mab3grafH")),
                type = 8,
                color = "#35aa00",
                size = 0.5
              ),
              footer = list(
                column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INPE/PRODES"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "mab3_1H"))
              )
            )
          ),
          fluidRow(
            ## Tabela - Hidrografia (km²)----
            box(
              title = textOutput(NS(id, "mab3txt2H")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              pickerInput(
                inputId = NS(id, "mab3anoH"),
                label = "Ano",
                choices = mab3 %>% filter(categoria == "Hidrografia (km²)") %>% 
                  pull(ano) %>% unique() %>% sort(decreasing = T),
                width = "100px"
              ),
              withSpinner(
                reactableOutput(NS(id, "mab3tabH"),height = "400px"),
                type = 8,
                color = "#35aa00",
                size = 0.5
              ),
              footer = list(
                column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INPE/PRODES"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "mab3_2H"))
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
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "mab4muni"),
                  label = "Município",
                  choices = mab4 %>% 
                    filter(localidade != "Pará") %>% 
                    pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico de linha - Focos de Calor----
              box(
                title = textOutput(NS(id, "mab4txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                # Comparar municípios
                pickerInput(
                  inputId = NS(id, "mab4municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "mab4graf")),
                  type = 8,
                  color = "#35aa00",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "INPE/PRODES"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "mab4_1"))
                )
              )
            ),
            fluidRow(
              ## Tabela - Focos de Calor----
              box(
                title = textOutput(NS(id, "mab4txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "mab4ano"),
                  label = "Ano",
                  choices = sort(unique(mab4[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "mab4tab2"),height = "400px"),
                  type = 8,
                  color = "#35aa00",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "INPE/PRODES"),
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
meio_ambiente_mp_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    #TÍTULOS----
    # 1 - Desflorestamento Acumulado (km²)----
    ## Gráfico de linha - Desflorestamento Acumulado (km²)----
    t11 <- reactive({
      req(input$mab1municomp)
      if (input$mab1municomp == "Selecione um município") {
        paste0("Desflorestamento Acumulado (km²), ",
               input$mab1muni, " - ", min(mab1$ano), " a ", max(mab1$ano))
      } else {
        paste0("Desflorestamento Acumulado (km²), ",
               input$mab1muni, " x ", input$mab1municomp, " - ", min(mab1$ano), " a ", max(mab1$ano))
      }
    })

    ## Tabela - Desflorestamento Acumulado (km²) da mesma Região de Integração----
    t12 <- reactive({
      ri <- mab1 %>%
        filter(ano == input$mab1ano, localidade == input$mab1muni) %>%
        pull(ri)
      paste0("Desflorestamento Acumulado (km²) por municípios, Região de Integração ", ri, " - ", input$mab1ano)
    })
    
    # 2 - Incremento (Desflorestamento km²)----
    ## Gráfico de linha - Incremento (Desflorestamento km²)----
    t21 <- reactive({
      req(input$mab2municomp)
      if (input$mab2municomp == "Selecione um município") {
        paste0("Incremento (Desflorestamento km²), ",
               input$mab2muni, " - ", min(mab2$ano), " a ", max(mab2$ano))
      } else {
        paste0("Incremento (Desflorestamento km²), ",
               input$mab2muni, " x ", input$mab2municomp, " - ", min(mab2$ano), " a ", max(mab2$ano))
      }
    })

    ## Tabela - Incremento (Desflorestamento km²) da mesma Região de Integração----
    t22 <- reactive({
      ri <- mab2 %>%
        filter(ano == input$mab2ano, localidade == input$mab2muni) %>%
        pull(ri)
      paste0("Incremento (Desflorestamento km²) por municípios, Região de Integração ", ri, " - ", input$mab2ano)
    })
    
    # 3 - Área de Floresta (km²) e Hidrografia (km²)----
    ## 3-1 Área de Floresta (km²)----
    ## Gráfico de linha - Área de Floresta (km²)----
    t31A <- reactive({
      db <- mab3 %>% filter(categoria == "Área de Floresta (km²)")
      req(input$mab3municompA)
      if (input$mab3municompA == "Selecione um município") {
        paste0("Área de Floresta (km²), ",
               input$mab3muniA, " - ", min(db$ano), " a ", max(db$ano))
      } else {
        paste0("Área de Floresta (km²), ",
               input$mab3muniA, " x ", input$mab3municompA, " - ", min(db$ano), " a ", max(db$ano))
      }
    })
    
    ## Tabela - Área de Floresta (km²) da mesma Região de Integração----
    t32A <- reactive({
      ri <- mab3 %>%
        filter(ano == input$mab3anoA, localidade == input$mab3muniA,categoria == "Área de Floresta (km²)") %>%
        pull(ri) %>% unique()
      paste0("Área de Floresta (km²) por municípios, Região de Integração do", ri, " - ", input$mab3anoA)
    })
    ## 3-2 - Hidrografia (km²)----
    ## Gráfico de linha - Hidrografia (km²)----
    t31H <- reactive({
      db <- mab3 %>% filter(categoria == "Hidrografia (km²)")
      req(input$mab3municompH)
      if (input$mab3municompH == "Selecione um município") {
        paste0("Hidrografia (km²), ",
               input$mab3muniH, " - ", min(db$ano), " a ", max(db$ano))
      } else {
        paste0("Hidrografia (km²), ",
               input$mab3muniH, " x ", input$mab3municompH, " - ", min(db$ano), " a ", max(db$ano))
      }
    })
    
    ## Tabela - Hidrografia (km²) da mesma Região de Integração----
    t32H <- reactive({
      ri <- mab3 %>%
        filter(ano == input$mab3anoH, localidade == input$mab3muniH,categoria == "Hidrografia (km²)") %>%
        pull(ri) %>% unique()
      paste0("Hidrografia (km²) por municípios, Região de Integração do", ri, " - ", input$mab3anoH)
    })
   
    # 4 - Focos de Calor----
    ## Gráfico de linha - Focos de Calor----
    t41 <- reactive({
      req(input$mab4municomp)
      if (input$mab4municomp == "Selecione um município") {
        paste0("Focos de Calor, ",
               input$mab4muni, " - ", min(mab4$ano), " - ", max(mab4$ano))
      } else {
        paste0("Focos de Calor, ",
               input$mab4muni, " x ", input$mab4municomp, " - ", min(mab4$ano), " - ", max(mab4$ano))
      }
    })

    ## Tabela - Focos de Calor da mesma Região de Integração----
    t42 <- reactive({
      ri <- mab4 %>%
        filter(ano == input$mab4ano, localidade == input$mab4muni) %>%
        pull(ri)
      paste0("Focos de Calor por municípios, Região de Integração ", ri, " - ", input$mab4ano)
    })

    #VISUALIZAÇÃO----
    # 1 - Desflorestamento Acumulado (km²)----
    ## Gráfico de linha - Desflorestamento Acumulado (km²)----
    # Atualização da entrada
    mab1comp <- reactive({
      input$mab1muni
    })
    observeEvent(mab1comp(), {
      x <- mab1 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != mab1comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "mab1municomp", choices = c("Selecione um município", choices), session)
    })

    output$mab1txt1 <- renderText({
      t11()
    })

    output$mab1graf <- renderEcharts4r({
      req(input$mab1municomp)
      if (input$mab1municomp == "Selecione um município") {
        a <- mab1 %>% filter(localidade == input$mab1muni)
        a %>%
          e_charts(x = ano) %>%
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
            formatter = e_tooltip_pointer_formatter("decimal", digits = 2, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Ano",
            splitLine = list(show = T),
            nameTextStyle = list(fontWeight = "bold", fontSize = 14)
          ) %>%
          e_y_axis(
            name = "Área (km²)",
            nameTextStyle = list(fontWeight = "bold", fontSize = 14),
            scale = T,
            axisLabel = list(
              formatter = htmlwidgets::JS("
              function (value, index) {
              return value.toLocaleString('pt-BR', { minimumFractionDigits: 2, maximumFractionDigits: 2 });
              }
            ")
            )
          ) %>%
          e_locale("pt-Br") %>%
          e_datazoom(toolbox = F, fillerColor = "#E5F5F9") %>%
          e_grid(show = T)
      } else {
        a <- mab1 %>% filter(localidade == input$mab1muni)
        b <- mab1 %>% filter(localidade == input$mab1municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$mab1muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$mab1municomp,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 2, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Ano",
            splitLine = list(show = T),
            nameTextStyle = list(fontWeight = "bold", fontSize = 14)
          ) %>%
          e_y_axis(
            name = "Área (km²)",
            nameTextStyle = list(fontWeight = "bold", fontSize = 14),
            scale = T,
            axisLabel = list(
              formatter = htmlwidgets::JS("
              function (value, index) {
              return value.toLocaleString('pt-BR', { minimumFractionDigits: 2, maximumFractionDigits: 2 });
              }
            ")
            )
          ) %>%
          e_locale("pt-Br") %>%
          e_datazoom(toolbox = F, fillerColor = "#E5F5F9") %>%
          e_grid(show = T)
      }
    })
    ## Tabela - Desflorestamento Acumulado (km²) da mesma Região de Integração----
    output$mab1txt2 <- renderText({
      t12()
    })
    output$mab1tab2 <- renderReactable({
      ris <- mab1 %>%
        filter(ano == input$mab1ano, localidade == input$mab1muni) %>%
        pull(ri)
      x <- mab1 %>% filter(ano == input$mab1ano, localidade != "Pará")
      x <- x %>%
        filter(ri == ris) %>%
        arrange(desc(valor)) %>%
        mutate(posicao = row.names(.)) %>%
        select(posicao, ri, localidade, valor)
      x %>% reactable(
        defaultSorted = list(valor = "desc"),
        defaultPageSize = 10,
        striped = FALSE,
        highlight = TRUE,
        bordered = TRUE,
        outlined = TRUE,
        resizable = TRUE,
        showSortable = TRUE,
        pagination = F,
        columns = list(
          posicao = colDef(name = "nº", width = 50),
          ri = colDef(name = "Região de Integração"),
          localidade = colDef(name = "Municípios"),
          Ano = colDef(align = "center"),
          valor = colDef(
            name = "Área (km²)",
            format = colFormat(
              digits = 4,
              separators = T,
              locales = "pt-BR"
            ),
            cell = data_bars(x,
              text_position = "above",
              box_shadow = TRUE,
              align_bars = "right",
              number_fmt = scales::number_format(accuracy = 0.01, decimal.mark = ",", big.mark = ".")
            )
          )
        ),
        defaultColDef = colDef(headerStyle = list(background = "#f7f7f8")),
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
    # 2 - Incremento (Desflorestamento km²)----
    ## Gráfico de linha - Incremento (Desflorestamento km²)----
    # Atualização da entrada
    mab2comp <- reactive({
      input$mab2muni
    })
    observeEvent(mab2comp(), {
      x <- mab2 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != mab2comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "mab2municomp", choices = c("Selecione um município", choices), session)
    })

    output$mab2txt1 <- renderText({
      t21()
    })

    output$mab2graf <- renderEcharts4r({
      req(input$mab2municomp)
      if (input$mab2municomp == "Selecione um município") {
        a <- mab2 %>% filter(localidade == input$mab2muni)
        a %>%
          e_charts(x = ano) %>%
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
            formatter = e_tooltip_pointer_formatter("decimal", digits = 2, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Ano",
            splitLine = list(show = T),
            nameTextStyle = list(fontWeight = "bold", fontSize = 14)
          ) %>%
          e_y_axis(
            name = "Área (km²)",
            nameTextStyle = list(fontWeight = "bold", fontSize = 14),
            scale = T,
            axisLabel = list(
              formatter = htmlwidgets::JS("
              function (value, index) {
              return value.toLocaleString('pt-BR', { minimumFractionDigits: 2, maximumFractionDigits: 2 });
              }
            ")
            )
          ) %>%
          e_locale("pt-Br") %>%
          e_datazoom(toolbox = F, fillerColor = "#E5F5F9") %>%
          e_grid(show = T)
      } else {
        a <- mab2 %>% filter(localidade == input$mab2muni)
        b <- mab2 %>% filter(localidade == input$mab2municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$mab2muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$mab2municomp,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 2, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Ano",
            splitLine = list(show = T),
            nameTextStyle = list(fontWeight = "bold", fontSize = 14)
          ) %>%
          e_y_axis(
            name = "Área (km²)",
            nameTextStyle = list(fontWeight = "bold", fontSize = 14),
            scale = T,
            axisLabel = list(
              formatter = htmlwidgets::JS("
              function (value, index) {
              return value.toLocaleString('pt-BR', { minimumFractionDigits: 2, maximumFractionDigits: 2 });
              }
            ")
            )
          ) %>%
          e_locale("pt-Br") %>%
          e_datazoom(toolbox = F, fillerColor = "#E5F5F9") %>%
          e_grid(show = T)
      }
    })
    ## Tabela - Incremento (Desflorestamento km²) da mesma Região de Integração----
    output$mab2txt2 <- renderText({
      t22()
    })
    output$mab2tab2 <- renderReactable({
      ris <- mab2 %>%
        filter(ano == input$mab2ano, localidade == input$mab2muni) %>%
        pull(ri)
      x <- mab2 %>% filter(ano == input$mab2ano, localidade != "Pará")
      x <- x %>%
        filter(ri == ris) %>%
        arrange(desc(valor)) %>%
        mutate(posicao = row.names(.)) %>%
        select(posicao, ri, localidade, valor)
      x %>% reactable(
        defaultSorted = list(valor = "desc"),
        defaultPageSize = 10,
        striped = FALSE,
        highlight = TRUE,
        bordered = TRUE,
        outlined = TRUE,
        resizable = TRUE,
        showSortable = TRUE,
        pagination = F,
        columns = list(
          posicao = colDef(name = "nº", width = 50),
          ri = colDef(name = "Região de Integração"),
          localidade = colDef(name = "Municípios"),
          Ano = colDef(align = "center"),
          valor = colDef(
            name = "Área (km²)",
            format = colFormat(
              digits = 4,
              separators = T,
              locales = "pt-BR"
            ),
            cell = data_bars(x,
              text_position = "above",
              box_shadow = TRUE,
              align_bars = "right",
              number_fmt = scales::number_format(accuracy = 0.01, decimal.mark = ",", big.mark = ".")
            )
          )
        ),
        defaultColDef = colDef(headerStyle = list(background = "#f7f7f8")),
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
    
    # 3 - Área de Floresta (km²) e Hidrografia (km²)----
    ## 3-1 - Área de Floresta (km²)----
    ## Gráfico de linha - Área de Floresta (km²)----
    # Atualização da entrada
    mab3compA <- reactive({
      input$mab3muniA
    })
    observeEvent(mab3compA(), {
      x <- mab3 %>% filter(localidade != "Pará",categoria == "Área de Floresta (km²)")
      x <- x %>% filter(localidade != mab3compA())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "mab3municompA", choices = c("Selecione um município", choices), session)
    })
    
    output$mab3txt1A <- renderText({
      t31A()
    })
    
    output$mab3grafA <- renderEcharts4r({
      req(input$mab3municompA)
      if (input$mab3municompA == "Selecione um município") {
        a <- mab3 %>% filter(localidade == input$mab3muniA,categoria == "Área de Floresta (km²)")
        a %>%
          e_charts(x = ano) %>%
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
            formatter = e_tooltip_pointer_formatter("decimal", digits = 2, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Ano",
            splitLine = list(show = T),
            nameTextStyle = list(fontWeight = "bold", fontSize = 14)
          ) %>%
          e_y_axis(
            name = "Área (km²)",
            nameTextStyle = list(fontWeight = "bold", fontSize = 14),
            scale = T,
            axisLabel = list(
              formatter = htmlwidgets::JS("
              function (value, index) {
              return value.toLocaleString('pt-BR', { minimumFractionDigits: 2, maximumFractionDigits: 2 });
              }
            ")
            )
          ) %>%
          e_locale("pt-Br") %>%
          e_datazoom(toolbox = F, fillerColor = "#E5F5F9") %>%
          e_grid(show = T)
      } else {
        a <- mab3 %>% filter(localidade == input$mab3muniA,categoria == "Área de Floresta (km²)")
        b <- mab3 %>% filter(localidade == input$mab3municompA,categoria == "Área de Floresta (km²)")
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$mab3muniA,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$mab3municompA,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 2, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Ano",
            splitLine = list(show = T),
            nameTextStyle = list(fontWeight = "bold", fontSize = 14)
          ) %>%
          e_y_axis(
            name = "Área (km²)",
            nameTextStyle = list(fontWeight = "bold", fontSize = 14),
            scale = T,
            axisLabel = list(
              formatter = htmlwidgets::JS("
              function (value, index) {
              return value.toLocaleString('pt-BR', { minimumFractionDigits: 2, maximumFractionDigits: 2 });
              }
            ")
            )
          ) %>%
          e_locale("pt-Br") %>%
          e_datazoom(toolbox = F, fillerColor = "#E5F5F9") %>%
          e_grid(show = T)
      }
    })
    ## Tabela - Área de Floresta (km²) da mesma Região de Integração----
    output$mab3txt2A <- renderText({
      t32A()
    })
    output$mab3tabA <- renderReactable({
      ris <- mab3 %>%
        filter(ano == input$mab3anoA, localidade == input$mab3muniA) %>%
        pull(ri)
      x <- mab3 %>% 
        filter(ano == input$mab3anoA, 
               localidade != "Pará",
               categoria == "Área de Floresta (km²)")
      x <- x %>%
        filter(ri == ris) %>%
        arrange(desc(valor)) %>%
        mutate(posicao = row.names(.)) %>%
        select(posicao, ri, localidade, valor)
      x %>% reactable(
        defaultSorted = list(valor = "desc"),
        defaultPageSize = 10,
        striped = FALSE,
        highlight = TRUE,
        bordered = TRUE,
        outlined = TRUE,
        resizable = TRUE,
        showSortable = TRUE,
        pagination = F,
        columns = list(
          posicao = colDef(name = "nº", width = 50),
          ri = colDef(name = "Região de Integração"),
          localidade = colDef(name = "Municípios"),
          Ano = colDef(align = "center"),
          valor = colDef(
            name = "Área (km²)",
            format = colFormat(
              digits = 4,
              separators = T,
              locales = "pt-BR"
            ),
            cell = data_bars(x,
                             text_position = "above",
                             box_shadow = TRUE,
                             align_bars = "right",
                             number_fmt = scales::number_format(accuracy = 0.01, decimal.mark = ",", big.mark = ".")
            )
          )
        ),
        defaultColDef = colDef(headerStyle = list(background = "#f7f7f8")),
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
    ## 3-2 - Hidrografia (km²)----
    ## Gráfico de linha - Hidrografia (km²)----
    # Atualização da entrada
    mab3compH <- reactive({
      input$mab3muniH
    })
    observeEvent(mab3compH(), {
      x <- mab3 %>% filter(localidade != "Pará",categoria == "Hidrografia (km²)")
      x <- x %>% filter(localidade != mab3compH())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "mab3municompH", choices = c("Selecione um município", choices), session)
    })
    
    output$mab3txt1H <- renderText({
      t31H()
    })
    
    output$mab3grafH <- renderEcharts4r({
      req(input$mab3municompH)
      if (input$mab3municompH == "Selecione um município") {
        a <- mab3 %>% filter(localidade == input$mab3muniH,categoria == "Hidrografia (km²)")
        a %>%
          e_charts(x = ano) %>%
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
            formatter = e_tooltip_pointer_formatter("decimal", digits = 2, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Ano",
            splitLine = list(show = T),
            nameTextStyle = list(fontWeight = "bold", fontSize = 14)
          ) %>%
          e_y_axis(
            name = "Área (km²)",
            nameTextStyle = list(fontWeight = "bold", fontSize = 14),
            scale = T,
            axisLabel = list(
              formatter = htmlwidgets::JS("
              function (value, index) {
              return value.toLocaleString('pt-BR', { minimumFractionDigits: 2, maximumFractionDigits: 2 });
              }
            ")
            )
          ) %>%
          e_locale("pt-Br") %>%
          e_datazoom(toolbox = F, fillerColor = "#E5F5F9") %>%
          e_grid(show = T)
      } else {
        a <- mab3 %>% filter(localidade == input$mab3muniH,categoria == "Hidrografia (km²)")
        b <- mab3 %>% filter(localidade == input$mab3municompH,categoria == "Hidrografia (km²)")
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$mab3muniH,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$mab3municompH,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 2, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Ano",
            splitLine = list(show = T),
            nameTextStyle = list(fontWeight = "bold", fontSize = 14)
          ) %>%
          e_y_axis(
            name = "Área (km²)",
            nameTextStyle = list(fontWeight = "bold", fontSize = 14),
            scale = T,
            axisLabel = list(
              formatter = htmlwidgets::JS("
              function (value, index) {
              return value.toLocaleString('pt-BR', { minimumFractionDigits: 2, maximumFractionDigits: 2 });
              }
            ")
            )
          ) %>%
          e_locale("pt-Br") %>%
          e_datazoom(toolbox = F, fillerColor = "#E5F5F9") %>%
          e_grid(show = T)
      }
    })
    ## Tabela - Hidrografia (km²) da mesma Região de Integração----
    output$mab3txt2H <- renderText({
      t32H()
    })
    output$mab3tabH <- renderReactable({
      ris <- mab3 %>%
        filter(ano == input$mab3anoH, localidade == input$mab3muniH) %>%
        pull(ri)
      x <- mab3 %>% 
        filter(ano == input$mab3anoH, 
               localidade != "Pará",
               categoria == "Hidrografia (km²)")
      x <- x %>%
        filter(ri == ris) %>%
        arrange(desc(valor)) %>%
        mutate(posicao = row.names(.)) %>%
        select(posicao, ri, localidade, valor)
      x %>% reactable(
        defaultSorted = list(valor = "desc"),
        defaultPageSize = 10,
        striped = FALSE,
        highlight = TRUE,
        bordered = TRUE,
        outlined = TRUE,
        resizable = TRUE,
        showSortable = TRUE,
        pagination = F,
        columns = list(
          posicao = colDef(name = "nº", width = 50),
          ri = colDef(name = "Região de Integração"),
          localidade = colDef(name = "Municípios"),
          Ano = colDef(align = "center"),
          valor = colDef(
            name = "Área (km²)",
            format = colFormat(
              digits = 4,
              separators = T,
              locales = "pt-BR"
            ),
            cell = data_bars(x,
                             text_position = "above",
                             box_shadow = TRUE,
                             align_bars = "right",
                             number_fmt = scales::number_format(accuracy = 0.01, decimal.mark = ",", big.mark = ".")
            )
          )
        ),
        defaultColDef = colDef(headerStyle = list(background = "#f7f7f8")),
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

    # 4 - Focos de Calor----
    ## Gráfico de linha - Focos de Calor----
    # Atualização da entrada
    mab4comp <- reactive({
      input$mab4muni
    })
    observeEvent(mab4comp(), {
      x <- mab4 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != mab4comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "mab4municomp", choices = c("Selecione um município", choices), session)
    })

    output$mab4txt1 <- renderText({
      t41()
    })

    output$mab4graf <- renderEcharts4r({
      req(input$mab4municomp)
      if (input$mab4municomp == "Selecione um município") {
        a <- mab4 %>% filter(localidade == input$mab4muni)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#35aa00",
            name = "Quantidade",
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
            name = "Quantidade",
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
      } else {
        a <- mab4 %>% filter(localidade == input$mab4muni)
        b <- mab4 %>% filter(localidade == input$mab4municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$mab4muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$mab4municomp,
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
            name = "Quantidade",
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
      }
    })
    ## Tabela - Focos de Calor da mesma Região de Integração----
    output$mab4txt2 <- renderText({
      t42()
    })
    output$mab4tab2 <- renderReactable({
      ris <- mab4 %>%
        filter(ano == input$mab4ano, localidade == input$mab4muni) %>%
        pull(ri)
      x <- mab4 %>% filter(ano == input$mab4ano, localidade != "Pará")
      x <- x %>%
        filter(ri == ris) %>%
        arrange(desc(valor)) %>%
        mutate(posicao = row.names(.)) %>%
        select(posicao, ri, localidade, valor)
      x %>% reactable(
        defaultSorted = list(valor = "desc"),
        defaultPageSize = 10,
        striped = FALSE,
        highlight = TRUE,
        bordered = TRUE,
        outlined = TRUE,
        resizable = TRUE,
        showSortable = TRUE,
        pagination = F,
        columns = list(
          posicao = colDef(name = "nº", width = 50),
          ri = colDef(name = "Região de Integração"),
          localidade = colDef(name = "Municípios"),
          Ano = colDef(align = "center"),
          valor = colDef(
            name = "Nº Focos",
            format = colFormat(
              digits = 4,
              separators = T,
              locales = "pt-BR"
            ),
            cell = data_bars(x,
              text_position = "above",
              box_shadow = TRUE,
              align_bars = "right",
              number_fmt = scales::number_format(decimal.mark = ",", big.mark = ".")
            )
          )
        ),
        defaultColDef = colDef(headerStyle = list(background = "#f7f7f8")),
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
  #   #DOWNLOADS----
    # 1 - Desflorestamento Acumulado (km²)----
    ## - Gráfico de linha - Desflorestamento Acumulado (km²)----
    # Filtra os dados
    mab1_1 <- reactive({
      req(input$mab1municomp)
      if (input$mab1municomp == "Selecione um município") {
        a <- mab1 %>% filter(localidade == input$mab1muni)
        } else {
        a <- mab1 %>% filter(localidade == input$mab1muni)
        b <- mab1 %>% filter(localidade == input$mab1municomp)
        df <- rbind(a,b)}
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(mab1_1(), {
      downset_Server("mab1_1", mab1_1(), t11())
    })
    ## Tabela - Desflorestamento Acumulado (km²)----
    # Filtra os dados
    mab1_2 <- reactive({
      ri <- mab1 %>%
        filter(ano == input$mab1ano, localidade == input$mab1muni) %>%
        pull(ri)
      x <- mab1 %>%
        filter(ano == input$mab1ano,
               localidade != "Pará",
               ri == ri
               ) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(mab1_2(), {
      downset_Server("mab1_2", mab1_2(), t12())
    })

    # 2 - Incremento (Desflorestamento km²)----
    ## Gráfico de linha - Incremento (Desflorestamento km²)----
    # Filtra os dados
    mab2_1 <- reactive({
      req(input$mab2municomp)
      if (input$mab2municomp == "Selecione um município") {
        a <- mab2 %>% filter(localidade == input$mab2muni)
        } else {
        a <- mab2 %>% filter(localidade == input$mab2muni)
        b <- mab2 %>% filter(localidade == input$mab2municomp)
        df <- rbind(a,b)}
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(mab2_1(), {
      downset_Server("mab2_1", mab2_1(), t21())
    })
    ## Tabela - Incremento (Desflorestamento km²)----
    # Filtra os dados
    mab2_2 <- reactive({
      ri <- mab2 %>%
        filter(ano == input$mab2ano, localidade == input$mab2muni) %>%
        pull(ri)
      x <- mab2 %>%
        filter(ano == input$mab2ano,
               localidade != "Pará",
               ri == ri
               ) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(mab2_2(), {
      downset_Server("mab2_2", mab2_2(), t22())
    })

    # 3 - Área de Floresta (km²)----
    # 3-1 Área de Floresta (km²)----
    ## Gráfico de linha - Área de Floresta (km²)----
    # Filtra os dados
    mab3_1A <- reactive({
      req(input$mab3municompA)
      if (input$mab3municompA == "Selecione um município") {
        a <- mab3 %>% filter(localidade == input$mab3muniA,categoria == "Área de Floresta (km²)")
      } else {
        a <- mab3 %>% filter(localidade == input$mab3muniA,categoria == "Área de Floresta (km²)")
        b <- mab3 %>% filter(localidade == input$mab3municompA,categoria == "Área de Floresta (km²)")
        df <- rbind(a,b)}
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(mab3_1A(), {
      downset_Server("mab3_1A", mab3_1A(), t31A())
    })
    ## Tabela - Área de Floresta (km²)----
    # Filtra os dados
    mab3_2A <- reactive({
      ris <- mab3 %>%
        filter(ano == input$mab3anoA, localidade == input$mab3muniA) %>%
        pull(ri)
      x <- mab3 %>%
        filter(ano == input$mab3anoA,
               localidade != "Pará",
               ri == ris
        ) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(mab3_2A(), {
      downset_Server("mab3_2A", mab3_2A(), t32A())
    })
    # 3-2 Hidrografia (km²)----
    ## Gráfico de linha - Hidrografia (km²)----
    # Filtra os dados
    mab3_1H <- reactive({
      req(input$mab3municompH)
      if (input$mab3municompH == "Selecione um município") {
        a <- mab3 %>% filter(localidade == input$mab3muniH,categoria == "Hidrografia (km²)")
      } else {
        a <- mab3 %>% filter(localidade == input$mab3muniH,categoria == "Hidrografia (km²)")
        b <- mab3 %>% filter(localidade == input$mab3municompH,categoria == "Hidrografia (km²)")
        df <- rbind(a,b)}
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(mab3_1H(), {
      downset_Server("mab3_1H", mab3_1H(), t31H())
    })
    ## Tabela - Hidrografia (km²)----
    # Filtra os dados
    mab3_2H <- reactive({
      ris <- mab3 %>%
        filter(ano == input$mab3anoH, localidade == input$mab3muniH) %>%
        pull(ri)
      x <- mab3 %>%
        filter(ano == input$mab3anoH,
               localidade != "Pará",
               ri == ris
        ) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(mab3_2H(), {
      downset_Server("mab3_2H", mab3_2H(), t32H())
    })

    # 4 - Focos de Calor----
    ## Gráfico de linha - Focos de Calor----
    # Filtra os dados
    mab4_1 <- reactive({
      req(input$mab4municomp)
      if (input$mab4municomp == "Selecione um município") {
        a <- mab4 %>% filter(localidade == input$mab4muni)
        } else {
        a <- mab4 %>% filter(localidade == input$mab4muni)
        b <- mab4 %>% filter(localidade == input$mab4municomp)
        df <- rbind(a,b)}
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(mab4_1(), {
      downset_Server("mab4_1", mab4_1(), t41())
    })
    ## Tabela - Focos de Calor----
    # Filtra os dados
    mab4_2 <- reactive({
      ris <- mab4 %>%
        filter(ano == input$mab4ano, localidade == input$mab4muni) %>%
        pull(ri)
      x <- mab4 %>%
        filter(ano == input$mab4ano,
               localidade != "Pará",
               ri == ris
               ) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(mab4_2(), {
      downset_Server("mab4_2", mab4_2(), t42())
    })

    })
}

# Play do Módulo
# ui <- dashboardPage(
#   header = dashboardHeader(),
#   sidebar = dashboardSidebar(),
#   body = dashboardBody(fluidPage(
#     meio_ambiente_mp_ui("meio_ambiente_mp")
#   ))
# )
# 
# 
# server <- function(input, output) {
#   meio_ambiente_mp_Server("meio_ambiente_mp")
# }
# 
# shinyApp(ui, server)


