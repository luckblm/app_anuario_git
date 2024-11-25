# Funções de módulo de Social - Inclusão Social - Municipal
# Função de UI
social_inclusao_social_mp_ui <- function(id) {
  fluidPage( # CAMINHO DO ARQUIVO CSS----
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "estilo.css")
    ),
    div(
      class = "navbar_social",
      navbarPage(
        tags$b("Inclusão Social - Municípios"),
        navbarMenu(
          tags$b("Escolha um Indicador"),
          # 1 - Famílias Atendidas e Valor Total Empregado no Programa Bolsa Família----
          tabPanel(
            "Famílias Atendidas e Valor Total Empregado no Programa Bolsa Família",
            panel( 
              ## Controle----
              heading =
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Famílias Atendidas e Valor Total Empregado no Programa Bolsa Família"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "inc1muni"),
                  label = "Município",
                  choices = inc1 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Famílias Atendidas e Valor Total Empregado no Programa Bolsa Família----
              box(
                title = textOutput(NS(id, "inc1txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "inc1municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "inc1graf"), height = "600px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "SAGI/MDS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "inc1_1"))
                )
              ),
              ## Tabela - Famílias Atendidas e Valor Total Empregado no Programa Bolsa Família----
              box(
                title = textOutput(NS(id, "inc1txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "inc1ano"),
                  label = "Ano",
                  choices = sort(unique(inc1[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "inc1tab1"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "SAGI/MDS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "inc1_2"))
                )
              )
            )
          ),
          # 2 - Famílias Atendidas e Valor Total Empregado no Programa Bolsa Família----
          tabPanel(
            "Famílias Atendidas e Valor Total Empregado no Programa Bolsa Família",
            panel( 
              ## Controle----
              heading =
                   h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Famílias Atendidas e Valor Total Empregado no Programa Bolsa Família"),
                   tags$div(
                     class = "seletor1",
                     pickerInput(
                       inputId = NS(id, "inc2muni"),
                       label = "Município",
                       choices = inc2 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                       width = "200px"
                     )
                   )
            ),
            fluidRow(
              ## Gráfico - Famílias Atendidas e Valor Total Empregado no Programa Bolsa Família----
              box(
                title = textOutput(NS(id, "inc2txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "inc2municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "inc2graf"), height = "600px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "SAGI/MDS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "inc2_1"))
                )
              ),
              ## Tabela - Famílias Atendidas e Valor Total Empregado no Programa Bolsa Família----
              box(
                title = textOutput(NS(id, "inc2txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "inc2ano"),
                  label = "Ano",
                  choices = sort(unique(inc2[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "inc2tab1"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "SAGI/MDS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "inc2_2"))
                )
              )
            )
          ),
          
          # 3 - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico)----
          tabPanel(
            "Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico)",
            panel( 
              ## Controle----
              heading =
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico)"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "inc3muni"),
                  label = "Município",
                  choices = inc3 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico)----
              box(
                title = textOutput(NS(id, "inc3txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "inc3municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "inc3graf")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "SAGI/MDS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "inc3_1"))
                )
              ),
              ## Tabela - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico)----
              box(
                title = textOutput(NS(id, "inc3txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "inc3ano"),
                  label = "Ano",
                  choices = sort(unique(inc3[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "inc3tab2"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "SAGI/MDS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "inc3_2"))
                )
              )
            )
          ),
          # 4 - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico) com rendimento familiar per capita de até 1/2 salário mínimo----
          tabPanel(
            "Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico) com rendimento familiar per capita de até 1/2 salário mínimo",
            panel( 
              ## Controle----
              heading =
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico) com rendimento familiar per capita de até 1/2 salário mínimo"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "inc4muni"),
                  label = "Município",
                  choices = inc4 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico) com rendimento familiar per capita de até 1/2 salário mínimo----
              box(
                title = textOutput(NS(id, "inc4txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "inc4municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "inc4graf")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "SAGI/MDS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "inc4_1"))
                )
              ),
              ## Tabela - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico) com rendimento familiar per capita de até 1/2 salário mínimo----
              box(
                title = textOutput(NS(id, "inc4txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "inc4ano"),
                  label = "Ano",
                  choices = sort(unique(inc4[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "inc4tab2"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "SAGI/MDS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "inc4_2"))
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
social_inclusao_social_mp_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # TÍTULOS----
    # 1 - Famílias Atendidas e Valor Total Empregado no Programa Bolsa Família----
    ## Gráfico - Famílias Atendidas e Valor Total Empregado no Programa Bolsa Família----
    t11 <- reactive({
      req(input$inc1municomp)
      if (input$inc1municomp == "Selecione um município") {
        paste0("Famílias Atendidas e Valor Total Empregado no Programa Bolsa Família, ",
         input$inc1muni, " - ", min(inc1$ano), " a ", max(inc1$ano))
      } else {
        paste0("Famílias Atendidas e Valor Total Empregado no Programa Bolsa Família,
         ", input$inc1muni, " x ", input$inc1municomp, " - ", min(inc1$ano), " a ", max(inc1$ano))
      }
    })
    ## Tabela - Famílias Atendidas e Valor Total Empregado no Programa Bolsa Família----
    t12 <- reactive({
      ri <- inc1 %>%
        filter(ano == input$inc1ano, localidade == input$inc1muni) %>%
        pull(ri)
      paste0("Famílias Atendidas e Valor Total Empregado no Programa Bolsa Família nos municípios da Região de integração do ",
       unique(ri), " - ", input$inc1ano)
    })
    # 2 - Famílias Atendidas e Valor Total Empregado no Programa Bolsa Família----
    ## Gráfico - Famílias Atendidas e Valor Total Empregado no Programa Bolsa Família----
    t21 <- reactive({
      req(input$inc2municomp)
      if (input$inc2municomp == "Selecione um município") {
        paste0("Famílias Atendidas e Valor Total Empregado no Programa Bolsa Família, ",
               input$inc2muni, " - ", min(inc2$ano), " a ", max(inc2$ano))
      } else {
        paste0("Famílias Atendidas e Valor Total Empregado no Programa Bolsa Família,
         ", input$inc2muni, " x ", input$inc2municomp, " - ", min(inc2$ano), " a ", max(inc2$ano))
      }
    })
    ## Tabela - Famílias Atendidas e Valor Total Empregado no Programa Bolsa Família----
    t22 <- reactive({
      ri <- inc2 %>%
        filter(ano == input$inc2ano, localidade == input$inc2muni) %>%
        pull(ri)
      paste0("Famílias Atendidas e Valor Total Empregado no Programa Bolsa Família nos municípios da Região de integração do ",
             unique(ri), " - ", input$inc2ano)
    })
    # 3 - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico)----
    ## Gráfico - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico)----
    t31 <- reactive({
      req(input$inc3municomp)
      if (input$inc3municomp == "Selecione um município") {
        paste0("Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico), ", input$inc3muni, " - ", min(inc3$ano), " a ", max(inc3$ano))
      } else {
        paste0("Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico), ", input$inc3muni, " x ", input$inc3municomp, " - ", min(inc3$ano), " a ", max(inc3$ano))
      }
    })
    ## Tabela - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico)----
    t32 <- reactive({
      ri <- inc3 %>%
        filter(ano == input$inc3ano, localidade == input$inc3muni) %>%
        pull(ri)
      paste0("Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico) nos municípios da Região de integração do ", unique(ri), " - ", input$inc3ano)
    })
    # 4 - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico) com rendimento familiar per capita de até 1/2 salário mínimo----
    ## Gráfico - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico) com rendimento familiar per capita de até 1/2 salário mínimo----
    t41 <- reactive({
      req(input$inc4municomp)
      if (input$inc4municomp == "Selecione um município") {
        paste0("Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico) com rendimento familiar per capita de até 1/2 salário mínimo, ", input$inc4muni, " - ", min(inc4$ano), " a ", max(inc4$ano))
      } else {
        paste0("Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico) com rendimento familiar per capita de até 1/2 salário mínimo, ", input$inc4muni, " x ", input$inc4municomp, " - ", min(inc4$ano), " a ", max(inc4$ano))
      }
    })
    ## Tabela - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico) com rendimento familiar per capita de até 1/2 salário mínimo----
    t42 <- reactive({
      ri <- inc4 %>%
        filter(ano == input$inc4ano, localidade == input$inc4muni) %>%
        pull(ri)
      paste0("Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico) com rendimento familiar per capita de até 1/2 salário mínimo nos municípios da Região de integração do ", unique(ri), " - ", input$inc4ano)
    })
    # VISUALIZAÇÃO----
    # 1 - Famílias Atendidas e Valor Total Empregado no Programa Bolsa Família----
    ## Gráfico - Famílias Atendidas e Valor Total Empregado no Programa Bolsa Família----
    # Atualização da entrada
    inc1comp <- reactive({
      input$inc1muni
    })
    observeEvent(inc1comp(), {
      x <- inc1 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != inc1comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "inc1municomp", choices = c("Selecione um município", choices), session)
    })

    ## Título
    output$inc1txt1 <- renderText({
      t11()
    })

    output$inc1graf <- renderEcharts4r({
      req(input$inc1municomp)
      if (input$inc1municomp == "Selecione um município") {
        x <- inc1 %>%
          filter(localidade == input$inc1muni) %>%
          pivot_wider(names_from = categoria, values_from = valor)
        x %>%
          e_charts(ano) %>%
          e_line(
            Famílias,
            x_index = 1,
            y_index = 1,
            name = "Famílias",
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_line(
            `Valor Total`,
            name = "Valor (R$)",
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
            index = 1,
            axisLabel = list(show = T, fontSize = 11),
            name = "Ano",
            splitLine = list(show = T),
            nameTextStyle = list(fontWeight = "bold", fontSize = 14)
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Ano",
            splitLine = list(show = T),
            nameTextStyle = list(fontWeight = "bold", fontSize = 14)
          ) %>%
          e_y_axis(
            index = 1,
            name = "Famílias",
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
          e_y_axis(
            name = "Valor (R$)",
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
          e_grid(height = "25%", bottom = "60%") %>%
          e_grid(height = "25%", top = "60%") %>%
          e_y_axis(gridIndex = 1) %>%
          e_x_axis(gridIndex = 1) %>%
          e_datazoom(x_index = c(0, 1), toolbox = F, fillerColor = "#E5F5F9")
      } else {
        x <- inc1 %>%
          filter(localidade == input$inc1muni) %>%
          pivot_wider(names_from = categoria, values_from = valor)
        y <- inc1 %>%
          filter(localidade == input$inc1municomp) %>%
          pivot_wider(names_from = categoria, values_from = valor)
        x %>%
          e_charts(ano) %>%
          e_line(
            `Valor Total`,
            name = input$inc1muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(y, ano) %>%
          e_line(
            `Valor Total`,
            name = input$inc1municomp,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(x, ano) %>%
          e_line(
            Famílias,
            x_index = 1,
            y_index = 1,
            name = input$inc1muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(y, ano) %>%
          e_line(
            Famílias,
            x_index = 1,
            y_index = 1,
            name = input$inc1municomp,
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
          e_x_axis(
            index = 1,
            axisLabel = list(show = T, fontSize = 11),
            name = "Ano",
            splitLine = list(show = T),
            nameTextStyle = list(fontWeight = "bold", fontSize = 14)
          ) %>%
          e_y_axis(
            name = "Valor (R$)",
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
          e_y_axis(
            index = 1,
            name = "Famílias",
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
          e_grid(height = "25%", bottom = "60%") %>%
          e_grid(height = "25%", top = "60%") %>%
          e_y_axis(gridIndex = 1) %>%
          e_x_axis(gridIndex = 1) %>%
          e_datazoom(x_index = c(0, 1), toolbox = F, fillerColor = "#E5F5F9")
      }
    })
    ## Tabela - Famílias Atendidas e Valor Total Empregado no Programa Bolsa Família----
    output$inc1txt2 <- renderText({
      t12()
    })
    output$inc1tab1 <- renderReactable({
      ris <- inc1 %>%
        filter(ano == input$inc1ano, localidade == input$inc1muni) %>%
        pull(ri)
      x <- inc1 %>% filter(ano == input$inc1ano, localidade != "Pará")
      x <- x %>%
        filter(ri == ris) %>%
        select(ri, localidade, categoria, valor) %>%
        pivot_wider(names_from = categoria, values_from = valor)
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
          localidade = colDef(name = "Municípios")
        ),
        defaultColDef =
          colDef(
            footerStyle = list(fontWeight = "bold"),
            format = colFormat(separators = T),
            headerStyle = list(background = "#f7f7f8"),
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
    # 2 - Famílias Atendidas e Valor Total Empregado no Programa Bolsa Família----
    ## Gráfico - Famílias Atendidas e Valor Total Empregado no Programa Bolsa Família----
    # Atualização da entrada
    inc2comp <- reactive({
      input$inc2muni
    })
    observeEvent(inc2comp(), {
      x <- inc2 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != inc2comp())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "inc2municomp", choices = c("Selecione um município", choices), session)
    })
    
    ## Título
    output$inc2txt1 <- renderText({
      t21()
    })
    
    output$inc2graf <- renderEcharts4r({
      req(input$inc2municomp)
      if (input$inc2municomp == "Selecione um município") {
        x <- inc2 %>%
          filter(localidade == input$inc2muni) %>%
          pivot_wider(names_from = categoria, values_from = valor)
        x %>%
          e_charts(ano) %>%
          e_line(
            Famílias,
            x_index = 1,
            y_index = 1,
            name = "Famílias",
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_line(
            `Valor Total`,
            name = "Valor (R$)",
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
            index = 1,
            axisLabel = list(show = T, fontSize = 11),
            name = "Ano",
            splitLine = list(show = T),
            nameTextStyle = list(fontWeight = "bold", fontSize = 14)
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Ano",
            splitLine = list(show = T),
            nameTextStyle = list(fontWeight = "bold", fontSize = 14)
          ) %>%
          e_y_axis(
            index = 1,
            name = "Famílias",
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
          e_y_axis(
            name = "Valor (R$)",
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
          e_grid(height = "25%", bottom = "60%") %>%
          e_grid(height = "25%", top = "60%") %>%
          e_y_axis(gridIndex = 1) %>%
          e_x_axis(gridIndex = 1) %>%
          e_datazoom(x_index = c(0, 1), toolbox = F, fillerColor = "#E5F5F9")
      } else {
        x <- inc2 %>%
          filter(localidade == input$inc2muni) %>%
          pivot_wider(names_from = categoria, values_from = valor)
        y <- inc2 %>%
          filter(localidade == input$inc2municomp) %>%
          pivot_wider(names_from = categoria, values_from = valor)
        x %>%
          e_charts(ano) %>%
          e_line(
            `Valor Total`,
            name = input$inc2muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(y, ano) %>%
          e_line(
            `Valor Total`,
            name = input$inc2municomp,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(x, ano) %>%
          e_line(
            Famílias,
            x_index = 1,
            y_index = 1,
            name = input$inc2muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(y, ano) %>%
          e_line(
            Famílias,
            x_index = 1,
            y_index = 1,
            name = input$inc2municomp,
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
          e_x_axis(
            index = 1,
            axisLabel = list(show = T, fontSize = 11),
            name = "Ano",
            splitLine = list(show = T),
            nameTextStyle = list(fontWeight = "bold", fontSize = 14)
          ) %>%
          e_y_axis(
            name = "Valor (R$)",
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
          e_y_axis(
            index = 1,
            name = "Famílias",
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
          e_grid(height = "25%", bottom = "60%") %>%
          e_grid(height = "25%", top = "60%") %>%
          e_y_axis(gridIndex = 1) %>%
          e_x_axis(gridIndex = 1) %>%
          e_datazoom(x_index = c(0, 1), toolbox = F, fillerColor = "#E5F5F9")
      }
    })
    ## Tabela - Famílias Atendidas e Valor Total Empregado no Programa Bolsa Família----
    output$inc2txt2 <- renderText({
      t22()
    })
    output$inc2tab1 <- renderReactable({
      ris <- inc2 %>%
        filter(ano == input$inc2ano, localidade == input$inc2muni) %>%
        pull(ri)
      x <- inc2 %>% filter(ano == input$inc2ano, localidade != "Pará")
      x <- x %>%
        filter(ri == ris) %>%
        select(ri, localidade, categoria, valor) %>%
        pivot_wider(names_from = categoria, values_from = valor)
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
          localidade = colDef(name = "Municípios")
        ),
        defaultColDef =
          colDef(
            footerStyle = list(fontWeight = "bold"),
            format = colFormat(separators = T),
            headerStyle = list(background = "#f7f7f8"),
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
    # 3 - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico)----
    ## Gráfico - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico)----
    # Atualização da entrada
    inc3comp <- reactive({
      input$inc3muni
    })
    observeEvent(inc3comp(), {
      x <- inc3 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != inc3comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "inc3municomp", choices = c("Selecione um município", choices), session)
    })

    output$inc3txt1 <- renderText({
      t31()
    })

    output$inc3graf <- renderEcharts4r({
      req(input$inc3municomp)
      if (input$inc3municomp == "Selecione um município") {
        a <- inc3 %>% filter(localidade == input$inc3muni)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f17701",
            name = input$inc3muni,
            legend = F,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 1, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Ano",
            splitLine = list(show = T), nameTextStyle = list(fontWeight = "bold", fontSize = 14)
          ) %>%
          e_y_axis(
            name = "Total de famílias",
            nameTextStyle =
              list(fontWeight = "bold", fontSize = 14),
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
        a <- inc3 %>% filter(localidade == input$inc3muni)
        b <- inc3 %>% filter(localidade == input$inc3municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$inc3muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$inc3municomp,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 1, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Ano",
            splitLine = list(show = T), nameTextStyle = list(fontWeight = "bold", fontSize = 14)
          ) %>%
          e_y_axis(
            name = "Total de famílias",
            nameTextStyle =
              list(fontWeight = "bold", fontSize = 14),
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
    ## Tabela - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico)----
    output$inc3txt2 <- renderText({
      t32()
    })
    output$inc3tab2 <- renderReactable({
      ris <- inc3 %>%
        filter(ano == input$inc3ano, localidade == input$inc3muni) %>%
        pull(ri)
      x <- inc3 %>% filter(ano == input$inc3ano, localidade != "Pará")
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
            name = "Total de famílias",
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
    # 4 - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico) com rendimento familiar per capita de até 1/2 salário mínimo----
    ## Gráfico - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico) com rendimento familiar per capita de até 1/2 salário mínimo----
    # Atualização da entrada
    inc4comp <- reactive({
      input$inc4muni
    })
    observeEvent(inc4comp(), {
      x <- inc4 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != inc4comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "inc4municomp", choices = c("Selecione um município", choices), session)
    })

    output$inc4txt1 <- renderText({
      t41()
    })

    output$inc4graf <- renderEcharts4r({
      req(input$inc4municomp)
      if (input$inc4municomp == "Selecione um município") {
        a <- inc4 %>% filter(localidade == input$inc4muni)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f17701",
            name = input$inc4muni,
            legend = F,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 1, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Ano",
            splitLine = list(show = T), nameTextStyle = list(fontWeight = "bold", fontSize = 14)
          ) %>%
          e_y_axis(
            name = "Total de famílias",
            nameTextStyle =
              list(fontWeight = "bold", fontSize = 14),
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
        a <- inc4 %>% filter(localidade == input$inc4muni)
        b <- inc4 %>% filter(localidade == input$inc4municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$inc4muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$inc4municomp,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 1, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Ano",
            splitLine = list(show = T), nameTextStyle = list(fontWeight = "bold", fontSize = 14)
          ) %>%
          e_y_axis(
            name = "Total de famílias",
            nameTextStyle =
              list(fontWeight = "bold", fontSize = 14),
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
    ## Tabela - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico) com rendimento familiar per capita de até 1/2 salário mínimo----
    output$inc4txt2 <- renderText({
      t42()
    })
    output$inc4tab2 <- renderReactable({
      ris <- inc4 %>%
        filter(ano == input$inc4ano, localidade == input$inc4muni) %>%
        pull(ri)
      x <- inc4 %>% filter(ano == input$inc4ano, localidade != "Pará")
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
            name = "Total de famílias",
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
    # DOWNLOADS----
    # 1 - Famílias Atendidas e Valor Total Empregado no Programa Bolsa Família----
    ## Gráfico - Famílias Atendidas e Valor Total Empregado no Programa Bolsa Família----
    # Filtra os dados
    inc1_1 <- reactive({
      req(input$inc1municomp)
      if (input$inc1municomp == "Selecione um município") {
        a <- inc1 %>%
          filter(localidade == input$inc1muni)
      } else {
        a <- inc1 %>%
          filter(localidade == input$inc1muni)
        b <- inc1 %>%
          filter(localidade == input$inc1municomp)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inc1_1(), {
      downset_Server("inc1_1", inc1_1(), t11())
    })
    ## Tabela - Famílias Atendidas e Valor Total Empregado no Programa Bolsa Família----
    # Filtra os dados
    inc1_2 <- reactive({
      ris <- inc1 %>%
        filter(ano == input$inc1ano, localidade == input$inc1muni) %>%
        pull(ri)
      x <- inc1 %>%
        filter(
          ano == input$inc1ano,
          localidade != "Pará",
          ri == ris
        )
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inc1_2(), {
      downset_Server("inc1_2", inc1_2(), t12())
    })
    # 2 - Famílias Atendidas e Valor Total Empregado no Programa Bolsa Família----
    ## Gráfico - Famílias Atendidas e Valor Total Empregado no Programa Bolsa Família----
    # Filtra os dados
    inc2_1 <- reactive({
      req(input$inc2municomp)
      if (input$inc2municomp == "Selecione um município") {
        a <- inc2 %>%
          filter(localidade == input$inc2muni)
      } else {
        a <- inc2 %>%
          filter(localidade == input$inc2muni)
        b <- inc2 %>%
          filter(localidade == input$inc2municomp)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inc2_1(), {
      downset_Server("inc2_1", inc2_1(), t21())
    })
    ## Tabela - Famílias Atendidas e Valor Total Empregado no Programa Bolsa Família----
    # Filtra os dados
    inc2_2 <- reactive({
      ris <- inc2 %>%
        filter(ano == input$inc2ano, localidade == input$inc2muni) %>%
        pull(ri)
      x <- inc2 %>%
        filter(
          ano == input$inc2ano,
          localidade != "Pará",
          ri == ris
        )
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inc2_2(), {
      downset_Server("inc2_2", inc2_2(), t22())
    })
    # 3 - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico)----
    ## Gráfico - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico)----
    # Filtra os dados
    inc3_1 <- reactive({
      req(input$inc3municomp)
      if (input$inc3municomp == "Selecione um município") {
        a <- inc3 %>% filter(localidade == input$inc3muni)
      } else {
        a <- inc3 %>% filter(localidade == input$inc3muni)
        b <- inc3 %>% filter(localidade == input$inc3municomp)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inc3_1(), {
      downset_Server("inc3_1", inc3_1(), t31())
    })
    ## Tabela - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico)----
    # Filtra os dados
    inc3_2 <- reactive({
      ris <- inc3 %>%
        filter(ano == input$inc3ano, localidade == input$inc3muni) %>%
        pull(ri)
      x <- inc3 %>%
        filter(
          ano == input$inc3ano,
          localidade != "Pará",
          ri == ris
        ) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inc3_2(), {
      downset_Server("inc3_2", inc3_2(), t32())
    })

    # 4 - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico) com rendimento familiar per capita de até 1/2 salário mínimo----
    ## Gráfico - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico) com rendimento familiar per capita de até 1/2 salário mínimo----
    # Filtra os dados
    inc4_1 <- reactive({
      req(input$inc4municomp)
      if (input$inc4municomp == "Selecione um município") {
        a <- inc4 %>% filter(localidade == input$inc4muni)
      } else {
        a <- inc4 %>% filter(localidade == input$inc4muni)
        b <- inc4 %>% filter(localidade == input$inc4municomp)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inc4_1(), {
      downset_Server("inc4_1", inc4_1(), t41())
    })
    ## Tabela - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico) com rendimento familiar per capita de até 1/2 salário mínimo----
    # Filtra os dados
    inc4_2 <- reactive({
      ris <- inc4 %>%
        filter(ano == input$inc4ano, localidade == input$inc4muni) %>%
        pull(ri)
      x <- inc4 %>%
        filter(
          ano == input$inc4ano,
          localidade != "Pará",
          ri == ris
        ) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inc4_2(), {
      downset_Server("inc4_2", inc4_2(), t42())
    })
  })
}


# #Play do Módulo
# ui <- dashboardPage(
#   header = dashboardHeader(),
#   sidebar = dashboardSidebar(),
#   body = dashboardBody(fluidPage(
#     social_inclusao_social_mp_ui("social_inclusao_social_mp")
#   ))
# )
# 
# 
# server <- function(input, output) {
#   social_inclusao_social_mp_Server("social_inclusao_social_mp")
# }
# 
# shinyApp(ui, server)
