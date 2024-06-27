# Funções de módulo de Economia - Balança Comercial  - Municipal
# Função de UI
economia_bc_mp_ui <- function(id) {
  fluidPage(# CAMINHO DO ARQUIVO CSS----
            tags$head(
              tags$link(rel = "stylesheet", type = "text/css", href = "estilo.css")
            ),
            # Lista de Navegação lateral----
            div(class = "navbar_economia", navbarPage(
              tags$b("Balança Comercial - Municípios"),
              navbarMenu(
                "Indicadores",
                # 1 - Balança Comercial - Exportação----
                tabPanel(
                  "Balança Comercial - Exportação",
                  panel(
                    ## Controle----
                    heading =
                      h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                         "Balança Comercial - Exportação"),
                    tags$div(
                      class = "seletor2",
                      pickerInput(
                        inputId = NS(id, "bc1muni"),
                        label = "Município",
                        choices = bc1 %>%
                          filter(localidade != "Pará") %>%
                          pull(localidade) %>% unique(),
                        width = "200px"
                      )
                    ),
                    tags$div(class = "seletor3", )
                  ),
                  fluidRow(
                    ## Gráfico - Balança Comercial - Exportação----
                    box(
                      title = textOutput(NS(id, "bc1txt1")),
                      collapsible = T,
                      collapsed = F,
                      headerBorder = T,
                      width = 12,
                      # Comparar muicípios
                      pickerInput(
                        inputId = NS(id, "bc1municomp"),
                        label = "Comparar Município",
                        choices = NULL,
                        width = "200px",
                        options =
                          list(`none-selected-text` = "Selecione um município")
                      ),
                      withSpinner(
                        echarts4rOutput(NS(id, "bc1graf")),
                        type = 8,
                        color = "#f2c94e",
                        size = 0.5
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "MDIC - Comexstat"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA")
                      ), downset_ui(NS(id, "bc1_1")))
                    )
                  ),
                  fluidRow(
                    ## Tabela - Balança Comercial - Exportação----
                    box(
                      title = textOutput(NS(id, "bc1txt2")),
                      collapsible = T,
                      collapsed = F,
                      headerBorder = T,
                      width = 12,
                      pickerInput(
                        inputId = NS(id, "bc1ano"),
                        label = "Ano",
                        choices = sort(unique(bc1[["ano"]]), decreasing = T),
                        width = "100px"
                      ),
                      withSpinner(
                        reactableOutput(NS(id, "bc1tab2"), height = "400px"),
                        type = 8,
                        color = "#f2c94e",
                        size = 0.5
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "MDIC - Comexstat"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA")
                      ), downset_ui(NS(id, "bc1_2")))
                    )
                  )
                ),
                
                # 2 - Balança Comercial - Importação----
                tabPanel(
                  "Balança Comercial - Importação",
                  panel(
                    ## Controle----
                        heading =
                          h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                             "Balança Comercial - Importação"),
                        tags$div(
                          class = "seletor2",
                          pickerInput(
                            inputId = NS(id, "bc2muni"),
                            label = "Município",
                            choices = bc2 %>%
                              filter(localidade != "Pará") %>%
                              pull(localidade) %>% unique(),
                            width = "200px"
                          )
                        )),
                  fluidRow(
                    ## Gráfico - Balança Comercial - Importação----
                    box(
                      title = textOutput(NS(id, "bc2txt1")),
                      collapsible = T,
                      collapsed = F,
                      headerBorder = T,
                      width = 12,
                      # Comparar muicípios
                      pickerInput(
                        inputId = NS(id, "bc2municomp"),
                        label = "Comparar Município",
                        choices = NULL,
                        width = "200px",
                        options = list(`none-selected-text` = "Selecione um município")
                      ),
                      withSpinner(
                        echarts4rOutput(NS(id, "bc2graf")),
                        type = 8,
                        color = "#f2c94e",
                        size = 0.5
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "MDIC - Alice Web"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA")
                      ), downset_ui(NS(id, "bc2_1")))
                    )
                  ),
                  fluidRow(
                    ## Tabela - Balança Comercial - Importação----
                    box(
                      title = textOutput(NS(id, "bc2txt2")),
                      collapsible = T,
                      collapsed = F,
                      headerBorder = T,
                      width = 12,
                      pickerInput(
                        inputId = NS(id, "bc2ano"),
                        label = "Ano",
                        choices = sort(unique(bc2[["ano"]]), decreasing = T),
                        width = "100px"
                      ),
                      withSpinner(
                        reactableOutput(NS(id, "bc2tab2"), height = "400px"),
                        type = 8,
                        color = "#f2c94e",
                        size = 0.5
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "MDIC - Alice Web"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA")
                      ), downset_ui(NS(id, "bc2_2")))
                    )
                  )
                ),
                
                # 3 - Saldo da Balança Comercial----
                tabPanel(
                  "Saldo da Balança Comercial",
                  panel(
                    ## Controle----
                    heading =
                      h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                         "Saldo da Balança Comercial"),
                        tags$div(
                          class = "seletor2",
                          pickerInput(
                            inputId = NS(id, "bc3muni"),
                            label = "Município",
                            choices = bc3 %>%
                              filter(localidade != "Pará") %>%
                              pull(localidade) %>% unique(),
                            width = "200px"
                          )
                        )),
                  fluidRow(
                    ## Gráfico - Saldo da Balança Comercial----
                    box(
                      title = textOutput(NS(id, "bc3txt1")),
                      collapsible = T,
                      collapsed = F,
                      headerBorder = T,
                      width = 12,
                      # Comparar municípios
                      pickerInput(
                        inputId = NS(id, "bc3municomp"),
                        label = "Comparar Município",
                        choices = NULL,
                        width = "200px",
                        options = list(`none-selected-text` = "Selecione um município")
                      ),
                      withSpinner(
                        echarts4rOutput(NS(id, "bc3graf")),
                        type = 8,
                        color = "#f2c94e",
                        size = 0.5
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "MDIC - Alice Web"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA")
                      ), downset_ui(NS(id, "bc3_1")))
                    )
                  ),
                  fluidRow(
                    ## Tabela - Saldo da Balança Comercial----
                    box(
                      title = textOutput(NS(id, "bc3txt2")),
                      collapsible = T,
                      collapsed = F,
                      headerBorder = T,
                      width = 12,
                      pickerInput(
                        inputId = NS(id, "bc3ano"),
                        label = "Ano",
                        choices = sort(unique(bc3[["ano"]]), decreasing = T),
                        width = "100px"
                      ),
                      withSpinner(
                        reactableOutput(NS(id, "bc3tab2"), height = "400px"),
                        type = 8,
                        color = "#f2c94e",
                        size = 0.5
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "MDIC - Alice Web"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA")
                      ), downset_ui(NS(id, "bc3_2")))
                    )
                  )
                ),
              )
            )))
}

# Função do modulo servidor
economia_bc_mp_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    #TÍTULO----
    # 1 - Balança Comercial - Exportação----
    ## Gráfico - Balança Comercial - Exportação----
    t11 <- reactive({
      req(input$bc1municomp)
      if (input$bc1municomp == "Selecione um município") {
        paste0(
          "Balança Comercial - Exportação, ",
          input$bc1muni,
          " - ",
          min(bc1$ano),
          " a ",
          max(bc1$ano)
        )
      } else {
        paste0(
          "Balança Comercial - Exportação, ",
          input$bc1muni,
          " x ",
          input$bc1municomp,
          " - ",
          min(bc1$ano),
          " a ",
          max(bc1$ano)
        )
      }
    })
    
    ## Tabela - Balança Comercial - Exportação da mesma Região de Integração----
    t12 <- reactive({
      ri <- bc1 %>%
        filter(ano == input$bc1ano, localidade == input$bc1muni) %>%
        pull(ri)
      paste0(
        "Balança Comercial - Exportação dos Municípios, Região de Integração ",
        ri,
        " - ",
        input$bc1ano
      )
    })
    
    # 2 - Balança Comercial - Importação----
    ## Gráfico - Balança Comercial - Importação----
    t21 <- reactive({
      req(input$bc2municomp)
      if (input$bc2municomp == "Selecione um município") {
        paste0(
          "Balança Comercial - Importação, ",
          input$bc2muni,
          " - ",
          min(bc2$ano),
          " a ",
          max(bc2$ano)
        )
      } else {
        paste0(
          "Balança Comercial - Importação, ",
          input$bc2muni,
          " x ",
          input$bc2municomp,
          " - ",
          min(bc2$ano),
          " a ",
          max(bc2$ano)
        )
      }
    })
    
    ## Tabela - Balança Comercial - Importação da mesma Região de Integração----
    t22 <- reactive({
      ri <- bc2 %>%
        filter(ano == input$bc2ano, localidade == input$bc2muni) %>%
        pull(ri)
      paste0(
        "Balança Comercial - Importação dos Municípios, Região de Integração ",
        ri,
        " - ",
        input$bc2ano
      )
    })
    
    # 3 - Saldo da Balança Comercial----
    ## Gráfico - Saldo da Balança Comercial----
    t31 <- reactive({
      req(input$bc3municomp)
      if (input$bc3municomp == "Selecione um município") {
        paste0(
          "Saldo da Balança Comercial, ",
          input$bc3muni,
          " - ",
          min(bc3$ano),
          " a ",
          max(bc3$ano)
        )
      } else {
        paste0(
          "Saldo da Balança Comercial, ",
          input$bc3muni,
          " x ",
          input$bc3municomp,
          " - ",
          min(bc3$ano),
          " a ",
          max(bc3$ano)
        )
      }
    })
    
    ## Tabela - Saldo da Balança Comercial da mesma Região de Integração----
    t32 <- reactive({
      ri <- bc3 %>%
        filter(ano == input$bc3ano, localidade == input$bc3muni) %>%
        pull(ri)
      paste0(
        "Saldo da Balança Comercial dos Municípios, Região de Integração ",
        ri,
        " - ",
        input$bc3ano
      )
    })
    
    #VISUALIZAÇÃO----
    # 1 - Balança Comercial - Exportação----
    ## Gráfico - Balança Comercial - Exportação----
    # Atualização da entrada
    bc1comp <- reactive({
      input$bc1muni
    })
    observeEvent(bc1comp(), {
      x <- bc1 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != bc1comp())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "bc1municomp",
        choices = c("Selecione um município", choices),
        session
      )
    })
    
    output$bc1txt1 <- renderText({
      t11()
    })
    
    output$bc1graf <- renderEcharts4r({
      req(input$bc1municomp)
      if (input$bc1municomp == "Selecione um município") {
        a <- bc1 %>% filter(localidade == input$bc1muni)
        a %>%
          e_charts(x = ano) %>%
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
            name = "Valores FOB US$",
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
        a <- bc1 %>% filter(localidade == input$bc1muni)
        b <- bc1 %>% filter(localidade == input$bc1municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$bc1muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$bc1municomp,
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
            name = "Valores FOB US$",
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
    ## Tabela - Balança Comercial - Exportação da mesma Região de Integração----
    output$bc1txt2 <- renderText({
      t12()
    })
    output$bc1tab2 <- renderReactable({
      ris <- bc1 %>%
        filter(ano == input$bc1ano, localidade == input$bc1muni) %>%
        pull(ri)
      x <-
        bc1 %>% filter(ano == input$bc1ano, localidade != "Pará")
      x <- x %>%
        filter(ri == ris) %>%
        arrange(desc(valor))
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
          posicao = colDef(name = "nº", width = 50),
          ri = colDef(name = "Região de Integração"),
          localidade = colDef(name = "Municípios"),
          Ano = colDef(align = "center"),
          valor = colDef(
            name = "Valores FOB US$",
            format = colFormat(
              digits = 4,
              separators = T,
              locales = "pt-BR"
            ),
            cell = data_bars(
              x,
              text_position = "above",
              box_shadow = TRUE,
              align_bars = "right",
              number_fmt = scales::number_format(big.mark = ".", decimal.mark = ",")
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
    
    # 2 - Balança Comercial - Importação----
    ## Gráfico - Balança Comercial - Importação----
    # Atualização da entrada
    bc2comp <- reactive({
      input$bc2muni
    })
    observeEvent(bc2comp(), {
      x <- bc2 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != bc2comp())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "bc2municomp",
        choices = c("Selecione um município", choices),
        session
      )
    })
    
    output$bc2txt1 <- renderText({
      t21()
    })
    
    output$bc2graf <- renderEcharts4r({
      req(input$bc2municomp)
      if (input$bc2municomp == "Selecione um município") {
        a <- bc2 %>% filter(localidade == input$bc2muni)
        a %>%
          e_charts(x = ano) %>%
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
            name = "Valores FOB US$",
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
        a <- bc2 %>% filter(localidade == input$bc2muni)
        b <- bc2 %>% filter(localidade == input$bc2municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$bc2muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$bc2municomp,
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
            name = "Valores FOB US$",
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
    ## Tabela - Balança Comercial - Importação da mesma Região de Integração----
    output$bc2txt2 <- renderText({
      t22()
    })
    output$bc2tab2 <- renderReactable({
      ris <- bc2 %>%
        filter(ano == input$bc2ano, localidade == input$bc2muni) %>%
        pull(ri)
      x <-
        bc2 %>% filter(ano == input$bc2ano, localidade != "Pará")
      x <- x %>%
        filter(ri == ris) %>%
        arrange(desc(valor))
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
          posicao = colDef(name = "nº", width = 50),
          ri = colDef(name = "Região de Integração"),
          localidade = colDef(name = "Municípios"),
          Ano = colDef(align = "center"),
          valor = colDef(
            name = "Valores FOB US$",
            format = colFormat(
              digits = 4,
              separators = T,
              locales = "pt-BR"
            ),
            cell = data_bars(
              x,
              text_position = "above",
              box_shadow = TRUE,
              align_bars = "right",
              number_fmt = scales::number_format(big.mark = ".", decimal.mark = ",")
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
    
    # 3 - Saldo da Balança Comercial----
    ## Gráfico - Saldo da Balança Comercial----
    # Atualização da entrada
    bc3comp <- reactive({
      input$bc3muni
    })
    observeEvent(bc3comp(), {
      x <- bc3 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != bc3comp())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "bc3municomp",
        choices = c("Selecione um município", choices),
        session
      )
    })
    
    output$bc3txt1 <- renderText({
      t31()
    })
    
    output$bc3graf <- renderEcharts4r({
      req(input$bc3municomp)
      if (input$bc3municomp == "Selecione um município") {
        a <- bc3 %>% filter(localidade == input$bc3muni)
        a %>%
          e_charts(x = ano) %>%
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
            name = "Valores FOB US$",
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
        a <- bc3 %>% filter(localidade == input$bc3muni)
        b <- bc3 %>% filter(localidade == input$bc3municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$bc3muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$bc3municomp,
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
            name = "Valores FOB US$",
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
    ## Tabela - Saldo da Balança Comercial da mesma Região de Integração----
    output$bc3txt2 <- renderText({
      t32()
    })
    output$bc3tab2 <- renderReactable({
      ris <- bc3 %>%
        filter(ano == input$bc3ano, localidade == input$bc3muni) %>%
        pull(ri)
      x <-
        bc3 %>% filter(ano == input$bc3ano, localidade != "Pará")
      x <- x %>%
        filter(ri == ris) %>%
        arrange(desc(valor))
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
          posicao = colDef(name = "nº", width = 50),
          ri = colDef(name = "Região de Integração"),
          localidade = colDef(name = "Municípios"),
          Ano = colDef(align = "center"),
          valor = colDef(
            name = "Valores FOB US$",
            format = colFormat(
              digits = 4,
              separators = T,
              locales = "pt-BR"
            ),
            cell = data_bars(
              x,
              text_position = "above",
              box_shadow = TRUE,
              align_bars = "right",
              number_fmt = scales::number_format(big.mark = ".", decimal.mark = ",")
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
    # DOWNLOADS----
    ## 1 - Balança Comercial - Exportação----
    ## Gráfico - Balança Comercial - Exportação----
    # Filtra os dados
    bc1_1 <- reactive({
      req(input$bc1municomp)
      if (input$bc1municomp == "Selecione um município") {
        a <- bc1 %>%
          filter(localidade == input$bc1muni)
      } else {
        a <-
          bc1 %>%
          filter(localidade == input$bc1muni)
        b <-
          bc1 %>%
          filter(localidade == input$bc1municomp)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(bc1_1(), {
      t11()
      downset_Server("bc1_1", bc1_1(), t11())
    })
    
    ## Tabela - Balança Comercial - Exportação----
    # Filtra os dados
    bc1_2 <- reactive({
      ris <-
        bc1 %>%
        filter(ano == input$bc1ano, localidade == input$bc1muni) %>%
        pull(ri)
      x <-
        bc1 %>%
        filter(ano == input$bc1ano, localidade != "Pará", ri == ris) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(bc1_2(), {
      t12()
      downset_Server("bc1_2", bc1_2(), t12())
    })
    
    
    ## 2 - Balança Comercial - Importação----
    ## Gráfico - Balança Comercial - Importação----
    # Filtra os dados
    bc2_1 <- reactive({
      req(input$bc2municomp)
      if (input$bc2municomp == "Selecione um município") {
        a <- bc2 %>%
          filter(localidade == input$bc2muni)
      } else {
        a <- bc2 %>% filter(localidade == input$bc2muni)
        b <- bc2 %>% filter(localidade == input$bc2municomp)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(bc2_1(), {
      t21()
      downset_Server("bc2_1", bc2_1(), t21())
    })
    
    
    ## Tabela - Balança Comercial - Importação----
    # Filtra os dados
    bc2_2 <- reactive({
      ris <- bc2 %>%
        filter(ano == input$bc2ano, localidade == input$bc2muni) %>%
        pull(ri)
      x <-
        bc2 %>% filter(ano == input$bc2ano, localidade != "Pará") %>%
        filter(ri == ris) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(bc2_2(), {
      t22()
      downset_Server("bc2_2", bc2_2(), t22())
    })
    
    ## 3 - Saldo da Balança Comercial----
    ## Gráfico - Saldo da Balança Comercial----
    # Filtra os dados
    bc3_1 <- reactive({
      req(input$bc3municomp)
      if (input$bc3municomp == "Selecione um município") {
        a <- bc3 %>%
          filter(localidade == input$bc3muni)
      } else {
        a <- bc3 %>% filter(localidade == input$bc3muni)
        b <- bc3 %>% filter(localidade == input$bc3municomp)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(bc3_1(), {
      t31()
      downset_Server("bc3_1", bc3_1(), t31())
    })
    
    ## Tabela - Saldo da Balança Comercial----
    # Filtra os dados
    bc3_2 <- reactive({
      ris <- bc3 %>%
        filter(ano == input$bc3ano, localidade == input$bc3muni) %>%
        pull(ri)
      x <-
        bc3 %>% filter(ano == input$bc3ano, localidade != "Pará") %>%
        filter(ri == ris) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(bc3_2(), {
      t32()
      downset_Server("bc3_2", bc3_2(), t32())
    })
    
  })
}


# # Play do Módulo
# ui <- dashboardPage(header = dashboardHeader(),
#                     sidebar = dashboardSidebar(),
#                     body = dashboardBody(fluidPage(economia_bc_mp_ui("economia_bc_mp"))))
# 
# server <- function(input, output) {
#   economia_bc_mp_Server("economia_bc_mp")
# }
# shinyApp(ui, server)
