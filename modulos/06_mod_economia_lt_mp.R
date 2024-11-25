# Funções de módulo de Economia - Lavoura Temporária - Municipal
# Função de UI
economia_lt_mp_ui <- function(id) {
  fluidPage(
    # CAMINHO DO ARQUIVO CSS----
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "estilo.css")
    ),
    # Lista de Navegação lateral----
    div(
      class = "navbar_economia",
      navbarPage(
        tags$b("Lavoura Temporária - Municípios"),
        navbarMenu(
          tags$b("Escolha um Indicador"),
          # 1 - Área Destinada à Colheita----
          tabPanel(
            "Área Destinada à Colheita",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Área Destinada à Colheita"),
              tags$div(
                class = "seletor2",
                # select municípios
                pickerInput(
                  inputId = NS(id, "lt1muni"),
                  label = "Municípios",
                  choices = lt1 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Área plantada em hectares----
              box(
                title = textOutput(NS(id, "lt1txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "lt1ano"),
                  label = "Ano",
                  choices = sort(unique(lt1[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "lt1graf"), height = "600px"),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "lt1_1"))
                )
              ),
              ## Gráfico - Área plantada em hectares - Total----
              box(
                title = textOutput(NS(id, "lt1txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "lt1municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "lt1graf1")),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "lt1_2"))
                )
              )
            ),
            fluidRow(
              ## Tabela - Área plantada em hectares----
              box(
                title = textOutput(NS(id, "lt1txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "lt1tab"), height = "400px"),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "lt1_3"))
                )
              ),
              ## Tabela - Área plantada em hectares por municipio----
              box(
                title = textOutput(NS(id, "lt1txt4")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "lt1ano2"),
                  label = "Ano",
                  choices = sort(unique(lt1[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "lt1tab1"),height = "400px"),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "lt1_4"))
                )
              )
            )
          ),
          # 2 - Área Colhida----
          tabPanel(
            "Área Colhida",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Área Colhida"),
              tags$div(
                class = "seletor2",
                # select municípios
                pickerInput(
                  inputId = NS(id, "lt2muni"),
                  label = "Municípios",
                  choices = lt2 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Área Colhida à Colheita----
              box(
                title = textOutput(NS(id, "lt2txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "lt2ano"),
                  label = "Ano",
                  choices = sort(unique(lt2[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "lt2graf"), height = "600px"),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "lt2_1"))
                )
              ),
              ## Gráfico - Área Colhida à Colheita - Total----
              box(
                title = textOutput(NS(id, "lt2txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "lt2municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "lt2graf1")),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "lt2_2"))
                )
              )
            ),
            fluidRow(
              ## Tabela - Área Colhida à Colheita----
              box(
                title = textOutput(NS(id, "lt2txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "lt2tab"), height = "400px"),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "lt2_3"))
                )
              ),
              ## Tabela - Área Colhida à Colheita por municipio----
              box(
                title = textOutput(NS(id, "lt2txt4")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "lt2ano2"),
                  label = "Ano",
                  choices = sort(unique(lt2[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "lt2tab1")),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "lt2_4"))
                )
              )
            )
          ),
          # 3 - Quantidade Produzida----
          tabPanel(
            "Quantidade Produzida",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Quantidade Produzida"),
              tags$div(
                class = "seletor3",
                # select categoria
                pickerInput(
                  inputId = NS(id, "lt3cat"),
                  label = "Produto",
                  choices = unique(lt3[["categoria"]]),
                  width = "250px"
                )
              ),
              tags$div(
                class = "seletor2",
                # select municípios
                pickerInput(
                  inputId = NS(id, "lt3muni"),
                  label = "Municípios",
                  choices = NULL,
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Quantidade Produzida----
              box(
                title = textOutput(NS(id, "lt3txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                # Comparar municípios
                pickerInput(
                  inputId = NS(id, "lt3municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "lt3graf")),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PEVS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "lt3_1"))
                )
              )
            ),
            fluidRow(
              ## Tabela - Quantidade Produzida, por Tipo de Produto----
              box(
                title = textOutput(NS(id, "lt3txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "lt3tab")),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PEVS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "lt3_2"))
                )
              )
            )
          ),
          # 4 - Valor da Produção----
          tabPanel(
            "Valor da Produção",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Valor da Produção"),
              tags$div(
                class = "seletor2",
                # select municípios
                pickerInput(
                  inputId = NS(id, "lt4muni"),
                  label = "Municípios",
                  choices = lt4 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Valor da Produção----
              box(
                title = textOutput(NS(id, "lt4txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "lt4ano"),
                  label = "Ano",
                  choices = sort(unique(lt4[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "lt4graf"), height = "600px"),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "lt4_1"))
                )
              ),
              ## Gráfico - Valor da Produção - Total----
              box(
                title = textOutput(NS(id, "lt4txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "lt4municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "lt4graf1")),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "lt4_2"))
                )
              )
            ),
            fluidRow(
              ## Tabela - Valor da Produção----
              box(
                title = textOutput(NS(id, "lt4txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "lt4tab"), height = "400px"),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "lt4_3"))
                )
              ),
              ## Tabela - Valor da Produção por municipio----
              box(
                title = textOutput(NS(id, "lt4txt4")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "lt4ano2"),
                  label = "Ano",
                  choices = sort(unique(lt4[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "lt4tab1"),height = "400px"),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "lt4_4"))
                )
              )
            )
          ),
          # 5 - Rendimento médio da produção----
          tabPanel(
            "Rendimento médio da produção",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Rendimento médio da produção"),
              tags$div(
                class = "seletor3",
                # select categoria
                pickerInput(
                  inputId = NS(id, "lt5cat"),
                  label = "Produto",
                  choices = unique(lt5[["categoria"]]),
                  width = "250px"
                )
              ),
              tags$div(
                class = "seletor2",
                # select municípios
                pickerInput(
                  inputId = NS(id, "lt5muni"),
                  label = "Municípios",
                  choices = NULL,
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Rendimento médio da produção----
              box(
                title = textOutput(NS(id, "lt5txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                # Comparar municípios
                pickerInput(
                  inputId = NS(id, "lt5municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "lt5graf")),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "lt5_1"))
                )
              )
            ),
            fluidRow(
              ## Tabela - Rendimento médio da produção, por Tipo de Produto----
              box(
                title = textOutput(NS(id, "lt5txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "lt5tab")),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "lt5_2"))
                )
              )
            )
          )
          # fim
        )
      )
    )
  )
}

# Função do modulo servidor
economia_lt_mp_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    #TÍTULO----
    # 1 - Área plantada em hectares----
    ## Gráfico de barras - Área plantada em hectares ----
    t11 <- reactive({
      paste0("Área Plantada (em hectares) por Tipo de Lavoura Temporária, ",
             input$lt1muni, " - ", input$lt1ano)
    })
    ## Gráfico de linha - Área plantada em hectares  - Total----
    t12 <- reactive({
      req(input$lt1municomp)
      if (input$lt1municomp == "Seleconar Município") {
        paste0("Área Total Plantada (em hectares) da Lavoura Temporária,
               ", input$lt1muni, " - ", min(lt1$ano), " a ", max(lt1$ano))
      } else {
        paste0("Área Total Plantada (em hectares) da Lavoura Temporária, ",
               input$lt1muni, " x ", input$lt1municomp, " - ", min(lt1$ano), " a ", max(lt1$ano))
      }
    })
    ## Tabela - Área plantada em hectares por produto----
    t13 <- reactive({
      paste0(" Área Plantada (em hectares) por Tipo de Lavoura Temporária, ",
             input$lt1muni, " - ", min(lt1$ano), " a ", max(lt1$ano))
    })
    ## Tabela - Área plantada em hectares por produto----
    t14 <- reactive({
      ri <- lt1 %>%
        filter(localidade == input$lt1muni) %>%
        select(ri) %>%
        pull() %>%
        unique()
      paste0("Área Total Plantada (em hectares) da Lavoura Temporária por Município, Região de Integração ",
             ri, " - ", input$lt1ano2)
    })
    # 2 - Área Colhida à Colheita----
    ## Gráfico de barras- Área Colhida à Colheita ----
    t21 <- reactive({
      paste0("Área Colhida (em hectares) por Tipo de Lavoura Temporária, ",
             input$lt2muni, " - ", input$lt2ano)
    })
    ## Gráfico de linha - Área Colhida à Colheita  - Total----
    t22 <- reactive({
      req(input$lt2municomp)
      if (input$lt2municomp == "Seleconar Município") {
        paste0("Área Colhida (em hectares) por Tipo de Lavoura Temporária, ", input$lt2muni, " - ", min(lt2$ano), " a ", max(lt2$ano))
      } else {
        paste0("Área Colhida (em hectares) por Tipo de Lavoura Temporária, ", input$lt2muni, " x ", input$lt2municomp, " - ", min(lt2$ano), " a ", max(lt2$ano))
      }
    })
    ## Tabela - Área Colhida à Colheita por produto----
    t23 <- reactive({
      paste0(" Área Colhida (em hectares) por Tipo de Lavoura Temporária, ",
             input$lt2muni, " - ", min(lt2$ano), " a ", max(lt2$ano))
    })
    ## Tabela - Área Colhida à Colheita por produto----
    t24 <- reactive({
      ri <- lt2 %>%
        filter(localidade == input$lt2muni) %>%
        select(ri) %>%
        pull() %>%
        unique()
      paste0("Área Total Colhida (em hectares) da Lavoura Temporária por Município, Região de Integração ",
             ri, " - ", input$lt2ano2)
    })
    # 3 - Quantidade Produzida----
    ## Gráfico - Quantidade Produzida na Extração Vegetal, por Tipo de Produto Extrativo (Hectares)----
    t31 <- reactive({
      req(input$lt3municomp)
      if (input$lt3municomp == "Seleconar Município") {
        paste0("Quantidade Produzida da Lavoura Temporária de ", 
               input$lt3cat, ", ", input$lt3muni, " - ", min(lt3$ano), " a ", max(lt3$ano))
      } else {
        paste0("Quantidade Produzida da Lavoura Temporária de ", 
               input$lt3cat, ", ", input$lt3muni, " x ", input$lt3municomp, " - ", min(lt3$ano), " a ", max(lt3$ano))
      }
    })
    ## Tabela - Quantidade Produzida, por Tipo de Produto----
    t32 <- reactive({
      paste0("Quantidade Produzida por Tipo de Lavoura Temporária, ",
             input$lt3muni, " - ", min(lt3$ano), " a ", max(lt3$ano))
    })
    # 4 - Valor da Produção----
    ## Gráfico - Valor da Produção ----
    t41 <- reactive({
      paste0("Valor (Mil Reais) da Produção por Tipo de Lavoura Temporária, ",
             input$lt4muni, " - ", input$lt4ano)
    })
    ## Gráfico - Valor da Produção  - Total----
    t42 <- reactive({
      req(input$lt4municomp)
      if (input$lt4municomp == "Seleconar Município") {
        paste0("Valor Total (Mil Reais) da Produção da Lavoura Temporária, ", input$lt4muni, " - ", min(lt4$ano), " a ", max(lt4$ano))
      } else {
        paste0("Valor Total (Mil Reais) da Produção da Lavoura Temporária, ", input$lt4muni, " x ", input$lt4municomp, " - ", min(lt4$ano), " a ", max(lt4$ano))
      }
    })
    ## Tabela - Valor da Produção por produto----
    t43 <- reactive({
      paste0(" Valor (Mil Reais) da Produção por Tipo de Lavoura Temporária, ",
             input$lt4muni, " - ", min(lt4$ano), " a ", max(lt4$ano))
    })
    ## Tabela - Valor da Produção por produto----
    t44 <- reactive({
      ri <- lt4 %>%
        filter(localidade == input$lt4muni) %>%
        select(ri) %>%
        pull() %>%
        unique()
      paste0("Valor Total (Mil Reais) da Produção da Lavoura Temporária por Município, Região de Integração ",
             ri, " - ", input$lt4ano2)
    })
    # 5 - Rendimento médio da produção----
    ## Gráfico - Rendimento médio da produção, por Tipo de Produto----
    t51 <- reactive({
      req(input$lt5municomp)
      if (input$lt5municomp == "Seleconar Município") {
        paste0("Rendimento Médio da Lavoura Temporária de ", input$lt5cat, ", ",
               input$lt5muni, " - ", min(lt5$ano), " a ", max(lt5$ano))
      } else {
        paste0("Rendimento Médio da Lavoura Temporária de ", input$lt5cat, ", ",
               input$lt5muni, " x ", input$lt5municomp, " - ", min(lt5$ano), " a ", max(lt5$ano))
      }
    })
    ## Tabela - Rendimento médio da produção, por Tipo de Produto----
    t52 <- reactive({
      paste0("Rendimento Médio da Produção por Tipo de Lavoura Temporária, ",
             input$lt5muni, " - ", min(lt5$ano), " a ", max(lt5$ano))
    })
    #VISUALIZÃO----
    # 1 - Área plantada em hectares----
    ## Gráfico de barras - Área plantada em hectares ----
    output$lt1txt1 <- renderText({ t11() })
    output$lt1graf <- renderEcharts4r({
      a <- lt1 %>%
        filter(localidade == input$lt1muni, ano == input$lt1ano) %>%
        arrange(valor)
      a %>%
        e_charts(categoria) %>%
        e_bar(
          serie = valor,
          color = "#f2c94e",
          name = "Área (ha)",
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
              "function(params) {return Intl.NumberFormat('pt-BR', 
              { style: 'decimal'}).format(params.value[{{0}}]);}",
              .open = "{{",
              .close = "}}"
            )
          )
        ) %>%
        e_y_axis(
          name = " Área (ha)",
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
            formatter = htmlwidgets::JS("
              function (value, index) {
              return value.toLocaleString('pt-BR', { minimumFractionDigits: 0, maximumFractionDigits: 0 });
              }
            ")
          )
        ) %>%
        e_locale("pt-Br") %>%
        e_grid(show = T, width = "75%", height = "80%", left = "20%") %>%
        e_flip_coords()
    })
    ## Gráfico de linha - Área plantada em hectares  - Total----
    # Comparador_________________________________________________
    lt1locomp <- reactive({
      req(input$lt1muni)
      x <- lt1 %>%
        filter(localidade != "Pará", valor > 0) %>%
        select(localidade)
      x <- x %>% filter(localidade != input$lt1muni)
    })
    observeEvent(lt1locomp(), {
      choices <- unique(pull(lt1locomp()))
      updatePickerInput(inputId = "lt1municomp", choices = c("Seleconar Município", choices), session)
    })
    # ____________________________________________________________
    output$lt1txt2 <- renderText({ t12() })
    output$lt1graf1 <- renderEcharts4r({
      req(input$lt1municomp)
      if (input$lt1municomp == "Seleconar Município") {
        a <- lt1 %>%
          filter(localidade == input$lt1muni) %>%
          group_by(ano) %>%
          summarise(valor = sum(valor))
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f2c94e",
            name = "Área (ha)",
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
            name = "Área (ha)",
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
        a <- lt1 %>%
          filter(localidade == input$lt1muni) %>%
          group_by(ano) %>%
          summarise(valor = sum(valor))
        b <- lt1 %>%
          filter(localidade == input$lt1municomp) %>%
          group_by(ano) %>%
          summarise(valor = sum(valor))
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$lt1muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$lt1municomp,
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
            name = "Área (ha)",
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
    ## Tabela - Área plantada em hectares por produto----
    output$lt1txt3 <- renderText({ t13() })
    output$lt1tab <- renderReactable({
      req(input$lt1muni)
      x <- lt1 %>%
        filter(localidade == input$lt1muni) %>%
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
          categoria = colDef(name = "Produtos", width = 200)
        ),
        defaultColDef = colDef(
          footerStyle = list(fontWeight = "bold"), na = "0",
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
    ## Tabela - Área plantada em hectares por produto----
    output$lt1txt4 <- renderText({t14()})
    output$lt1tab1 <- renderReactable({
      req(input$lt1muni)
      ris <- lt1 %>%
        filter(localidade == input$lt1muni) %>%
        select(ri) %>%
        pull()
      x <- lt1 %>%
        filter(ano == input$lt1ano2, localidade != "Pará", ri == ris) %>%
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
            name = "Área (ha)", format = colFormat(separators = T),
            cell = data_bars(x,
              text_position = "above",
              box_shadow = TRUE,
              align_bars = "right",
              number_fmt = scales::number_format(
                big.mark = ".",
                decimal.mark = ","
              )
            )
          ),
          posicao = colDef(name = "nº", width = 50)
        ),
        defaultColDef = colDef(
          footerStyle = list(fontWeight = "bold"), na = "0",
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
    # 2 - Área Colhida à Colheita----
    ## Gráfico de barras- Área Colhida à Colheita ----
    output$lt2txt1 <- renderText({ t21() })
    output$lt2graf <- renderEcharts4r({
      a <- lt2 %>%
        filter(localidade == input$lt2muni, ano == input$lt2ano) %>%
        arrange(valor)
      a %>%
        e_charts(categoria) %>%
        e_bar(
          serie = valor,
          color = "#f2c94e",
          name = "Área (ha)",
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
          name = " Área (ha)",
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
            formatter = htmlwidgets::JS("
              function (value, index) {
              return value.toLocaleString('pt-BR', { minimumFractionDigits: 0, maximumFractionDigits: 0 });
              }
            ")
          )
        ) %>%
        e_locale("pt-Br") %>%
        e_grid(show = T, width = "75%", height = "80%", left = "20%") %>%
        e_flip_coords()
    })
    ## Gráfico de linha - Área Colhida à Colheita  - Total----
    lt2locomp <- reactive({
      req(input$lt2muni)
      x <- lt2 %>%
        filter(localidade != "Pará", valor > 0) %>%
        select(localidade)
      x <- x %>% filter(localidade != input$lt2muni)
    })
    observeEvent(lt2locomp(), {
      choices <- unique(pull(lt2locomp()))
      updatePickerInput(inputId = "lt2municomp", choices = c("Seleconar Município", choices), session)
    })
    output$lt2txt2 <- renderText({ t22() })
    output$lt2graf1 <- renderEcharts4r({
      req(input$lt2municomp)
      if (input$lt2municomp == "Seleconar Município") {
        a <- lt2 %>%
          filter(localidade == input$lt2muni) %>%
          group_by(ano) %>%
          summarise(valor = sum(valor))
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f2c94e",
            name = "Área (ha)",
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
            name = "Área (ha)",
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
        a <- lt2 %>%
          filter(localidade == input$lt2muni) %>%
          group_by(ano) %>%
          summarise(valor = sum(valor))
        b <- lt2 %>%
          filter(localidade == input$lt2municomp) %>%
          group_by(ano) %>%
          summarise(valor = sum(valor))
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$lt2muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$lt2municomp,
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
            name = "Área (ha)",
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
    ## Tabela - Área Colhida à Colheita por produto----
    output$lt2txt3 <- renderText({ t23() })
    output$lt2tab <- renderReactable({
      req(input$lt2muni)
      x <- lt2 %>%
        filter(localidade == input$lt2muni) %>%
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
          categoria = colDef(name = "Produtos", width = 200)
        ),
        defaultColDef = colDef(
          footerStyle = list(fontWeight = "bold"), na = "0",
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
    ## Tabela - Área Colhida à Colheita por produto----
    output$lt2txt4 <- renderText({ t24() })
    output$lt2tab1 <- renderReactable({
      req(input$lt2muni)
      ris <- lt2 %>%
        filter(localidade == input$lt2muni) %>%
        select(ri) %>%
        pull()
      x <- lt2 %>%
        filter(ano == input$lt2ano2, localidade != "Pará", ri == ris) %>%
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
            name = "Área (ha)", format = colFormat(separators = T),
            cell = data_bars(x,
              text_position = "above",
              box_shadow = TRUE,
              align_bars = "right",
              number_fmt = scales::number_format(
                big.mark = ".",
                decimal.mark = ","
              )
            )
          ),
          posicao = colDef(name = "nº", width = 50)
        ),
        defaultColDef = colDef(
          footerStyle = list(fontWeight = "bold"), na = "0",
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
    # 3 - Quantidade Produzida----
    ## Controle
    lt3loc <- reactive({
      lt3 %>%
        filter(categoria == input$lt3cat, localidade != "Pará", valor > 0) %>%
        select(localidade)
    })
    observeEvent(lt3loc(), {
      choices <- unique(pull(lt3loc()))
      updatePickerInput(inputId = "lt3muni", choices = choices, session)
    })
    lt3locomp <- reactive({
      req(input$lt3muni)
      x <- lt3 %>%
        filter(categoria == input$lt3cat, localidade != "Pará", valor > 0) %>%
        select(localidade)
      x <- x %>% filter(localidade != input$lt3muni)
    })
    observeEvent(lt3locomp(), {
      choices <- unique(pull(lt3locomp()))
      updatePickerInput(inputId = "lt3municomp", choices = c("Seleconar Município", choices), session)
    })
    ## Gráfico - Quantidade Produzida na Extração Vegetal, por Tipo de Produto Extrativo (Hectares)----
    output$lt3txt1 <- renderText({ t31() })
    output$lt3graf <- renderEcharts4r({
      req(input$lt3municomp)
      if (input$lt3municomp == "Seleconar Município") {
        a <- lt3 %>% filter(localidade == input$lt3muni, categoria == input$lt3cat)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f2c94e",
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
        a <- lt3 %>% filter(localidade == input$lt3muni, categoria == input$lt3cat)
        b <- lt3 %>% filter(localidade == input$lt3municomp, categoria == input$lt3cat)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$lt3muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$lt3municomp,
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
    ## Tabela - Quantidade Produzida, por Tipo de Produto----
    output$lt3txt2 <- renderText({ t32() })
    output$lt3tab <- renderReactable({
      req(input$lt3muni)
      x <- lt3 %>%
        filter(localidade == input$lt3muni) %>%
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
          categoria = colDef(name = "Produtos", width = 250)
        ),
        defaultColDef = colDef(
          footerStyle = list(fontWeight = "bold"), na = "0",
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
    # 4 - Valor da Produção----
    ## Gráfico - Valor da Produção ----
    output$lt4txt1 <- renderText({ t41() })
    output$lt4graf <- renderEcharts4r({
      a <- lt4 %>%
        filter(localidade == input$lt4muni, ano == input$lt4ano) %>%
        arrange(valor)
      a %>%
        e_charts(categoria) %>%
        e_bar(
          serie = valor,
          color = "#f2c94e",
          name = "Valor (Mil Reais)",
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
          name = " Valor (Mil Reais)",
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
            formatter = htmlwidgets::JS("
              function (value, index) {
              return value.toLocaleString('pt-BR', { minimumFractionDigits: 0, maximumFractionDigits: 0 });
              }
            ")
          )
        ) %>%
        e_locale("pt-Br") %>%
        e_grid(show = T, width = "75%", height = "80%", left = "20%") %>%
        e_flip_coords()
    })
    ## Gráfico - Valor da Produção  - Total----
    # Comparador_________________________________________________
    lt4locomp <- reactive({
      req(input$lt4muni)
      x <- lt4 %>%
        filter(localidade != "Pará", valor > 0) %>%
        select(localidade)
      x <- x %>% filter(localidade != input$lt4muni)
    })
    observeEvent(lt4locomp(), {
      choices <- unique(pull(lt4locomp()))
      updatePickerInput(inputId = "lt4municomp", choices = c("Seleconar Município", choices), session)
    })
    # ____________________________________________________________
    output$lt4txt2 <- renderText({ t42() })

    output$lt4graf1 <- renderEcharts4r({
      req(input$lt4municomp)
      if (input$lt4municomp == "Seleconar Município") {
        a <- lt4 %>%
          filter(localidade == input$lt4muni) %>%
          group_by(ano) %>%
          summarise(valor = sum(valor))
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f2c94e",
            name = "Valor (Mil Reais)",
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
          e_locale("pt-Br") %>%
          e_datazoom(toolbox = F, fillerColor = "#E5F5F9") %>%
          e_grid(show = T)
      } else {
        a <- lt4 %>%
          filter(localidade == input$lt4muni) %>%
          group_by(ano) %>%
          summarise(valor = sum(valor))
        b <- lt4 %>%
          filter(localidade == input$lt4municomp) %>%
          group_by(ano) %>%
          summarise(valor = sum(valor))
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$lt4muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$lt4municomp,
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
          e_locale("pt-Br") %>%
          e_datazoom(toolbox = F, fillerColor = "#E5F5F9") %>%
          e_grid(show = T)
      }
    })
    ## Tabela - Valor da Produção por produto----
    output$lt4txt3 <- renderText({ t43() })
    output$lt4tab <- renderReactable({
      req(input$lt4muni)
      x <- lt4 %>%
        filter(localidade == input$lt4muni) %>%
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
          categoria = colDef(name = "Produtos", width = 200)
        ),
        defaultColDef = colDef(
          footerStyle = list(fontWeight = "bold"), na = "0",
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
    ## Tabela - Valor da Produção por produto----
    output$lt4txt4 <- renderText({ t44() })
    output$lt4tab1 <- renderReactable({
      req(input$lt4muni)
      ris <- lt4 %>%
        filter(localidade == input$lt4muni) %>%
        select(ri) %>%
        pull()
      x <- lt4 %>%
        filter(ano == input$lt4ano2, localidade != "Pará", ri == ris) %>%
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
            name = "Valor (Mil Reais)", format = colFormat(separators = T),
            cell = data_bars(x,
              text_position = "above",
              box_shadow = TRUE,
              align_bars = "right",
              number_fmt = scales::number_format(
                big.mark = ".",
                decimal.mark = ","
              )
            )
          ),
          posicao = colDef(name = "nº", width = 50)
        ),
        defaultColDef = colDef(
          footerStyle = list(fontWeight = "bold"), na = "0",
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
    # 5 - Rendimento médio da produção----
    ## Controle
    lt5loc <- reactive({
      lt5 %>%
        filter(categoria == input$lt5cat, localidade != "Pará", valor > 0) %>%
        select(localidade)
    })
    observeEvent(lt5loc(), {
      choices <- unique(pull(lt5loc()))
      updatePickerInput(inputId = "lt5muni", choices = choices, session)
    })
    lt5locomp <- reactive({
      req(input$lt5muni)
      x <- lt5 %>%
        filter(categoria == input$lt5cat, localidade != "Pará", valor > 0) %>%
        select(localidade)
      x <- x %>% filter(localidade != input$lt5muni)
    })
    observeEvent(lt5locomp(), {
      choices <- unique(pull(lt5locomp()))
      updatePickerInput(inputId = "lt5municomp", choices = c("Seleconar Município", choices), session)
    })
    ## Gráfico - Rendimento médio da produção, por Tipo de Produto----
    output$lt5txt1 <- renderText({ t51() })
    output$lt5graf <- renderEcharts4r({
      req(input$lt5municomp)
      if (input$lt5municomp == "Seleconar Município") {
        a <- lt5 %>% filter(localidade == input$lt5muni, categoria == input$lt5cat)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f2c94e",
            name = "Rendimento médio",
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
            name = "Redimento Médio",
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
        a <- lt5 %>% filter(localidade == input$lt5muni, categoria == input$lt5cat)
        b <- lt5 %>% filter(localidade == input$lt5municomp, categoria == input$lt5cat)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = "Rendimento médio",
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$lt5municomp,
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
            name = "Redimento Médio",
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

    ## Tabela - Rendimento médio da produção, por Tipo de Produto----
    output$lt5txt2 <- renderText({ t52() })
    output$lt5tab <- renderReactable({
      req(input$lt5muni)
      x <- lt5 %>%
        filter(localidade == input$lt5muni) %>%
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
          categoria = colDef(name = "Produtos", width = 250)
        ),
        defaultColDef = colDef(
          footerStyle = list(fontWeight = "bold"), na = "0",
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
    #DOWNLOADS----
    # 1 - Área Destinada à Colheita----
    ## - Gráfico - Área plantada em hectares----
    # Filtra os dados
    lt1_1 <- reactive({
     lt1 %>%  filter(localidade == input$lt1muni, 
             ano == input$lt1ano) %>%
        arrange(valor)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(lt1_1(), {
      t11()
      downset_Server("lt1_1", lt1_1(), t11())
    })
    ## - Gráfico - Área plantada em hectares no município----
    # Filtra os dados
    lt1_2 <- reactive({
      req(input$lt1municomp)
      if (input$lt1municomp == "Seleconar Município") {
        a <- lt1 %>%
          filter(localidade == input$lt1muni) %>%
          group_by(tematica,
                   subtema,
                   indicador,
                   ri,
                   localidade,
                   ano) %>%
          summarise(valor = sum(valor),.groups = "drop")
    } else {
        a <- lt1 %>%
          filter(localidade == input$lt1muni) %>%
          group_by(tematica,
                   subtema,
                   indicador,
                   ri,
                   localidade,
                   ano) %>%
          summarise(valor = sum(valor),.groups = "drop")
        b <- lt1 %>%
          filter(localidade == input$lt1municomp) %>%
          group_by(tematica,
                   subtema,
                   indicador,
                   ri,
                   localidade,
                   ano) %>%
          summarise(valor = sum(valor),.groups = "drop")}      
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(lt1_2(), {
      t12()
      downset_Server("lt1_2", lt1_2(), t12())
    })
    ## - Tabela - Área plantada em hectares, por Tipo de Produto no município----
    # Filtra os dados
    lt1_3 <- reactive({
      req(input$lt1muni)
      x <- lt1 %>%
        filter(localidade == input$lt1muni) %>%
        pivot_wider(names_from = ano, values_from = valor)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(lt1_3(), {
      t13()
      downset_Server("lt1_3", lt1_3(), t13())
    })
    ## - Tabela - Área plantada em hectares nos municípios----
    # Filtra os dados
    lt1_4 <- reactive({
      req(input$lt1muni)
      ri <- lt1 %>%
        filter(localidade == input$lt1muni) %>%
        pull()
      x <- lt1 %>%
        filter(ano == input$lt1ano2, localidade != "Pará",
               ri == ri) %>%
        group_by(tematica,
                 subtema,
                 indicador,
                 ri, 
                 localidade) %>%
        summarise(valor = sum(valor), .groups = "drop") %>% 
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(lt1_4(), {
      t14()
      downset_Server("lt1_4", lt1_4(), t14())
    })
    # 2 - Área Colhida à Colheita----
    ## - Gráfico - Área Colhida à Colheita, por Tipo de Produto no município----
    # Filtra os dados
    lt2_1 <- reactive({
      a <- lt2 %>%
        filter(localidade == input$lt2muni, ano == input$lt2ano) %>%
        arrange(valor)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(lt2_1(), {
      t21()
      downset_Server("lt2_1", lt2_1(), t21())
    })
    ## - Gráfico - Área Total Colhida à Colheita no município----
    # Filtra os dados
    lt2_2 <- reactive({
    req(input$lt2municomp)
      if (input$lt2municomp == "Seleconar Município") {
        a <- lt2 %>%
          filter(localidade == input$lt2muni) %>%
          group_by(tematica,
                   subtema,
                   indicador,
                   ri,
                   localidade,
                   ano) %>%
          summarise(valor = sum(valor), .groups = "drop")
          } else {
        a <- lt2 %>%
          filter(localidade == input$lt2muni) %>%
          group_by(tematica,
                   subtema,
                   indicador,
                   ri,
                   localidade,
                   ano) %>%
          
          summarise(valor = sum(valor), .groups = "drop")
        b <- lt2 %>%
          filter(localidade == input$lt2municomp) %>%
          group_by(tematica,
                   subtema,
                   indicador,
                   ri,
                   localidade,

                   ano) %>%
          summarise(valor = sum(valor), .groups = "drop")
        df <- rbind(a,b)
        }  
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(lt2_2(), {
      t22()
      downset_Server("lt2_2", lt2_2(), t)
    })
    ## - Tabela - Área Total Colhida à Colheita, por Tipo de Produto no município----
    # Filtra os dados
    lt2_3 <- reactive({
    req(input$lt2muni)
      x <- lt2 %>%
        filter(localidade == input$lt2muni) %>%
        select(categoria, ano, valor) %>%
        pivot_wider(names_from = ano, values_from = valor)  
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(lt2_3(), {
      t23()
      downset_Server("lt2_3", lt2_3(), t23())
    })
    ## - Tabela - Área Total Colhida à Colheita nos municípios----
    # Filtra os dados
    lt2_4 <- reactive({
    req(input$lt2muni)
      ri <- lt2 %>%
        filter(localidade == input$lt2muni) %>%
        select(ri) %>%
        pull()
      x <- lt2 %>%
        filter(ano == input$lt2ano2, localidade != "Pará", ri == ri) %>%
        group_by(ri, localidade) %>%
        summarise(valor = sum(valor), .groups = "drop")
      x <- x %>% arrange(desc(valor))
      x <- x %>%
        mutate(posicao = row.names(.)) %>%
        select(posicao, ri, localidade, valor)  
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(lt2_4(), {
      t24()
      downset_Server("lt2_4", lt2_4(), t24())
    })

    # 3 - Quantidade Produzida----
    ## - Gráfico - Quantidade Produzida----
    # Filtra os dados
    lt3_1 <- reactive({
      req(input$lt3municomp)
      if (input$lt3municomp == "Seleconar Município") {
        a <- lt3 %>% filter(localidade == input$lt3muni, categoria == input$lt3cat)
        } else {
        a <- lt3 %>% filter(localidade == input$lt3muni, categoria == input$lt3cat)
        b <- lt3 %>% filter(localidade == input$lt3municomp, categoria == input$lt3cat)
        df <- rbind(a,b)
        }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(lt3_1(), {
      t31()
      downset_Server("lt3_1", lt3_1(), t31())
    })
    ## - Tabela - Quantidade Produzida por Tipo de Produto----
    # Filtra os dados
    lt3_2 <- reactive({
      req(input$lt3muni)
      x <- lt3 %>%
        filter(localidade == input$lt3muni) %>%
        pivot_wider(names_from = ano, values_from = valor)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(lt3_2(), {
      t32()
      downset_Server("lt3_2", lt3_2(), t32())
    })

    # 4 - Valor da Produção----
    ## - Gráfico - Valor da Produção, por Tipo de Produto----
    # Filtra os dados
    lt4_1 <- reactive({
      a <- lt4 %>%
        filter(localidade == input$lt4muni, ano == input$lt4ano) %>%
        arrange(valor)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(lt4_1(), {
      t41()
      downset_Server("lt4_1", lt4_1(), t41())
    })
    ## - Gráfico - Valor Total da Produção no município----
    # Filtra os dados
    lt4_2 <- reactive({
      req(input$lt4municomp)
      if (input$lt4municomp == "Seleconar Município") {
        a <- lt4 %>%
          filter(localidade == input$lt4muni) %>%
          group_by(tematica,
                   subtema,
                   indicador,
                   ri,
                   localidade,
                   ano) %>%
          summarise(valor = sum(valor), .groups = "drop")
          } else {
        a <- lt4 %>%
          filter(localidade == input$lt4muni) %>%
          group_by(tematica,
                   subtema,
                   indicador,
                   ri,
                   localidade,
                   ano) %>%
          summarise(valor = sum(valor), .groups = "drop")
        b <- lt4 %>%
          filter(localidade == input$lt4municomp) %>%
          group_by(tematica,
                   subtema,
                   indicador,
                   ri,
                   localidade,
                   ano) %>%
          summarise(valor = sum(valor), .groups = "drop")}
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(lt4_2(), {
      t42()
      downset_Server("lt4_2", lt4_2(), t42())
    })
    ## - Tabela - Valor Total da Produção, por Tipo de Produto no município----
    # Filtra os dados
    lt4_3 <- reactive({
      req(input$lt4muni)
      x <- lt4 %>%
        filter(localidade == input$lt4muni) %>%
        pivot_wider(names_from = ano, values_from = valor)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(lt4_3(), {
      t43()
      downset_Server("lt4_3", lt4_3(), t43())
    })
    ## - Tabela - Valor Total da Produção nos municípios ----
    # Filtra os dados
    lt4_4 <- reactive({
      ri <- lt4 %>%
        filter(localidade == input$lt4muni) %>%
        select(ri) %>%
        pull()
      x <- lt4 %>%
        filter(ano == input$lt4ano2, localidade != "Pará", ri == ri) %>%
        group_by(tematica,
                 subtema,
                 indicador,
                 ri,
                 localidade,
                 ano) %>%
        summarise(valor = sum(valor), .groups = "drop") %>% 
        arrange(desc(valor))

    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(lt4_4(), {
      t44()
      downset_Server("lt4_4", lt4_4(), t44())
    })

    # 5 - Rendimento médio da produção----
    ## - Gráfico - Rendimento médio da produção----
    # Filtra os dados
    lt5_1 <- reactive({
      req(input$lt5municomp)
      if (input$lt5municomp == "Seleconar Município") {
        a <- lt5 %>% filter(localidade == input$lt5muni, categoria == input$lt5cat)
        } else {
        a <- lt5 %>% filter(localidade == input$lt5muni, categoria == input$lt5cat)
        b <- lt5 %>% filter(localidade == input$lt5municomp, categoria == input$lt5cat)
        df <- rbind(a,b)}
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(lt5_1(), {
      t51()
      downset_Server("lt5_1", lt5_1(), t51())
    })
    ## - Tabela - Rendimento médio da produção por Tipo de Produto no município----
    # Filtra os dados
    lt5_2 <- reactive({
      req(input$lt5muni)
      x <- lt5 %>%
        filter(localidade == input$lt5muni) %>%
        pivot_wider(names_from = ano, values_from = valor)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(lt5_2(), {
      t52()
      downset_Server("lt5_2", lt5_2(), t52())
    })
  })
}


# Play do Módulo
# ui <- dashboardPage(
#   header = dashboardHeader(),
#   sidebar = dashboardSidebar(),
#   body = dashboardBody(fluidPage(economia_lt_mp_ui("economia_lt_mp")))
# )
# 
# 
# server <- function(input, output) {
#   economia_lt_mp_Server("economia_lt_mp")
# }
# 
# shinyApp(ui, server)
