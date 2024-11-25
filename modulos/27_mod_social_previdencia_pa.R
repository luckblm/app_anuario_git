# Funções de módulo de Social - Previdência Social - Estadual
# Função de UI
social_previdencia_pa_ui <- function(id) {
  fluidPage( # CAMINHO DO ARQUIVO CSS----
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "estilo.css")
    ),
    div(
      class = "navbar_social",
      navbarPage(
        tags$b("Previdência Social - Pará"),
        navbarMenu(
          tags$b("Escolha um Indicador"),
          # 1 - Quantidade de Benefícios Emitidos em Dezembro----
          tabPanel(
            "Quantidade de Benefícios Emitidos em Dezembro",
            panel(
              ## Controle----
              heading =
              h4(
                style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Quantidade de Benefícios Emitidos em Dezembro"
              ),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "prev1ano"),
                  label = "Ano",
                  choices = sort(unique(prev1[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "prev1ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(prev1[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Quantidade de Benefícios Emitidos em Dezembro----
              box(
                title = textOutput(NS(id, "prev1txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "prev1map"), height = "600px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "SINTESE/DATAPREV"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                  tags$h6(
                    "benefícios: previdenciários, assistenciais e de legislação específica "
                  )
                ))
              ),
              ## Tabela - Quantidade de Benefícios Emitidos em Dezembro----
              box(
                title = textOutput(NS(id, "prev1txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "prev1tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "SINTESE/DATAPREV"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                    tags$h6(
                      "benefícios: previdenciários, assistenciais e de legislação específica "
                    )
                  ),
                  downset_ui(NS(id, "prev1_1"))
                )
              )
            ),
            fluidRow(
              ## Gráfico - Quantidade de Benefícios Emitidos em Dezembro----
              box(
                title = textOutput(NS(id, "prev1txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "prev1graf")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "SINTESE/DATAPREV"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                    tags$h6(
                      "benefícios: previdenciários, assistenciais e de legislação específica "
                    )
                  ),
                  downset_ui(NS(id, "prev1_2"))
                )
              )
            )
          ),
          # 2 - Valor dos Benefícios Emitidos em Dezembro----
          tabPanel(
            "Valor dos Benefícios Emitidos em Dezembro",
            panel(
              ## Controle----
              heading =
              h4(
                style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Valor dos Benefícios Emitidos em Dezembro"
              ),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "prev2ano"),
                  label = "Ano",
                  choices = sort(unique(prev2[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "prev2ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(prev2[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Valor dos Benefícios Emitidos em Dezembro----
              box(
                title = textOutput(NS(id, "prev2txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "prev2map"), height = "600px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  tags$h6(tags$b("Fonte:"), "SINTESE/DATAPREV"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                  tags$h6(
                    "benefícios: previdenciários, assistenciais e de legislação específica "
                  )
                )
              ),
              ## Tabela - Valor dos Benefícios Emitidos em Dezembro----
              box(
                title = textOutput(NS(id, "prev2txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "prev2tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "SINTESE/DATAPREV"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                    tags$h6(
                      "benefícios: previdenciários, assistenciais e de legislação específica "
                    )
                  ),
                  downset_ui(NS(id, "prev2_1"))
                )
              )
            ),
            fluidRow(
              ## Gráfico - Valor dos Benefícios Emitidos em Dezembro----
              box(
                title = textOutput(NS(id, "prev2txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "prev2graf")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "SINTESE/DATAPREV"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                    tags$h6(
                      "benefícios: previdenciários, assistenciais e de legislação específica "
                    )
                  ),
                  downset_ui(NS(id, "prev2_2"))
                )
              )
            )
          ),
          # 3 - Valor dos Benefícios Emitidos ao Ano ----
          tabPanel(
            "Valor dos Benefícios Emitidos",
            panel(
              ## Controle----
              heading =
              h4(
                style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Valor dos Benefícios Emitidos"
              ),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "prev3ano"),
                  label = "Ano",
                  choices = sort(unique(prev3[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "prev3ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(prev3[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Valor dos Benefícios Emitidos ao Ano ----
              box(
                title = textOutput(NS(id, "prev3txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "prev3map"), height = "600px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  tags$h6(tags$b("Fonte:"), "SINTESE/DATAPREV"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                  tags$h6(
                    "benefícios: previdenciários, assistenciais e de legislação específica "
                  )
                )
              ),
              ## Tabela - Valor dos Benefícios Emitidos ao Ano ----
              box(
                title = textOutput(NS(id, "prev3txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "prev3tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "SINTESE/DATAPREV"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                    tags$h6(
                      "benefícios: previdenciários, assistenciais e de legislação específica "
                    )
                  ),
                  downset_ui(NS(id, "prev3_1"))
                )
              )
            ),
            fluidRow(
              ## Gráfico - Valor dos Benefícios Emitidos ao Ano ----
              box(
                title = textOutput(NS(id, "prev3txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "prev3graf")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "SINTESE/DATAPREV"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                    tags$h6(
                      "benefícios: previdenciários, assistenciais e de legislação específica "
                    )
                  ),
                  downset_ui(NS(id, "prev3_2"))
                )
              )
            )
          ),
          # 4 - Benefícios Emitidos pela Previdência Social por Localização----
          "----",
          ## Arrecadação e Benefícios Emitidos pela Previdência Social----
          "Arrecadação e Benefícios Emitidos pela Previdência Social",
          ### 4.1-Quantidade de benefícios emitidos no mês de dezembro----
          tabPanel(
            "Quantidade de benefícios emitidos no mês de dezembro",
            panel(
              ### Controle----
              heading =
              h4(
                style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Quantidade de benefícios emitidos no mês de dezembro"
              ),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "prev4ano1"),
                  label = "Ano",
                  choices = prev4_1 %>%
                    select(ano) %>%
                    pull() %>% unique() %>%
                    sort(decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "prev4ri1"),
                  label = "Pará/Região de Integração",
                  choices = prev4_1 %>% select(ri) %>% pull() %>% unique(),
                  width = "200px"
                )
              ),
              tags$div(
                class = "seletor3",
                # select categoria
                pickerInput(
                  inputId = NS(id, "prev4cat1"),
                  label = "Localização",
                  choices = prev4_1 %>% select(categoria) %>% pull() %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ### Mapa - Quantidade de benefícios emitidos no mês de dezembro----
              box(
                title = textOutput(NS(id, "prev4_1txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "prev4map1"), height = "600px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  tags$h6(tags$b("Fonte:"), "SINTESE/DATAPREV"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                  tags$h6(
                    "benefícios: previdenciários, assistenciais e de legislação específica "
                  )
                )
              ),
              ### Tabela - Quantidade de benefícios emitidos no mês de dezembro----
              box(
                title = textOutput(NS(id, "prev4_1txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "prev4tab1"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "SINTESE/DATAPREV"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                    tags$h6(
                      "benefícios: previdenciários, assistenciais e de legislação específica "
                    )
                  ),
                  downset_ui(NS(id, "prev4_1_1"))
                )
              )
            ),
            fluidRow(
              ### Gráfico - Quantidade de benefícios emitidos no mês de dezembro----
              box(
                title = textOutput(NS(id, "prev4_1txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "prev4graf1")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "SINTESE/DATAPREV"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                    tags$h6(
                      "benefícios: previdenciários, assistenciais e de legislação específica "
                    )
                  ),
                  downset_ui(NS(id, "prev4_1_2"))
                )
              )
            )
          ),

          ### 4.2-Valor dos benefícios emitidos no mês de dezembro (em R$)----
          tabPanel(
            "Valor dos benefícios emitidos no mês de dezembro (em R$)",
            panel(
              ### Controle----
              heading =
              h4(
                style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Valor dos benefícios emitidos no mês de dezembro (em R$)"
              ),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "prev4ano2"),
                  label = "Ano",
                  choices = prev4_2 %>%
                    select(ano) %>%
                    pull() %>%
                    unique() %>%
                    sort(decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "prev4ri2"),
                  label = "Pará/Região de Integração",
                  choices = prev4_2 %>% select(ri) %>% pull() %>% unique(),
                  width = "200px"
                )
              ),
              tags$div(
                class = "seletor3",
                # select categoria
                pickerInput(
                  inputId = NS(id, "prev4cat2"),
                  label = "Localização",
                  choices = prev4_2 %>% select(categoria) %>% pull() %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ### Mapa - Valor dos benefícios emitidos no mês de dezembro (em R$)----
              box(
                title = textOutput(NS(id, "prev4_2txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "prev4map2"), height = "600px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  tags$h6(tags$b("Fonte:"), "SINTESE/DATAPREV"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                  tags$h6(
                    "benefícios: previdenciários, assistenciais e de legislação específica "
                  )
                )
              ),
              ### Tabela - Valor dos benefícios emitidos no mês de dezembro (em R$)----
              box(
                title = textOutput(NS(id, "prev4_2txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "prev4tab2"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "SINTESE/DATAPREV"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                    tags$h6(
                      "benefícios: previdenciários, assistenciais e de legislação específica "
                    )
                  ),
                  downset_ui(NS(id, "prev4_2_1"))
                )
              )
            ),
            fluidRow(
              ### Gráfico - Valor dos benefícios emitidos no mês de dezembro (em R$)----
              box(
                title = textOutput(NS(id, "prev4_2txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "prev4graf2")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "SINTESE/DATAPREV"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                    tags$h6(
                      "benefícios: previdenciários, assistenciais e de legislação específica "
                    )
                  ),
                  downset_ui(NS(id, "prev4_2_2"))
                )
              )
            )
          ),
          ### 4.3-Valor dos benefícios emitidos no ano (em R$)----
          tabPanel(
            "Valor dos benefícios emitidos no ano (em R$)",
            panel(
              ### Controle----
              heading =
              h4(
                style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Valor dos benefícios emitidos no ano (em R$)"
              ),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "prev4ano3"),
                  label = "Ano",
                  choices = prev4_3 %>%
                    select(ano) %>%
                    pull() %>%
                    unique() %>%
                    sort(decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "prev4ri3"),
                  label = "Pará/Região de Integração",
                  choices = prev4_3 %>% select(ri) %>% pull() %>% unique(),
                  width = "200px"
                )
              ),
              tags$div(
                class = "seletor3",
                # select categoria
                pickerInput(
                  inputId = NS(id, "prev4cat3"),
                  label = "Localização",
                  choices = prev4_3 %>% select(categoria) %>% pull() %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ### Mapa - Valor dos benefícios emitidos no ano (em R$)----
              box(
                title = textOutput(NS(id, "prev4_3txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "prev4map3"), height = "600px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  tags$h6(tags$b("Fonte:"), "SINTESE/DATAPREV"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                  tags$h6(
                    "benefícios: previdenciários, assistenciais e de legislação específica "
                  )
                )
              ),
              ### Tabela - Valor dos benefícios emitidos no ano (em R$)----
              box(
                title = textOutput(NS(id, "prev4_3txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "prev4tab3"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "SINTESE/DATAPREV"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                    tags$h6(
                      "benefícios: previdenciários, assistenciais e de legislação específica "
                    )
                  ),
                  downset_ui(NS(id, "prev4_3_1"))
                )
              )
            ),
            fluidRow(
              ### Gráfico - Valor dos benefícios emitidos no ano (em R$)----
              box(
                title = textOutput(NS(id, "prev4_3txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "prev4graf3")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "SINTESE/DATAPREV"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                    tags$h6(
                      "benefícios: previdenciários, assistenciais e de legislação específica "
                    )
                  ),
                  downset_ui(NS(id, "prev4_3_2"))
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
social_previdencia_pa_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # TÍTULOS----
    # 1 - Quantidade de Benefícios Emitidos em Dezembro----
    ## Mapa - Quantidade de Benefícios Emitidos em Dezembro----
    t11 <- reactive({
      if (input$prev1ri == "Pará") {
        paste0(
          "Quantidade de Benefícios Emitidos em Dezembro, Pará - ",
          input$prev1ano
        )
      } else {
        paste0(
          "Quantidade de Benefícios Emitidos em Dezembro, Região de Integração ",
          input$prev1ri,
          " - ",
          input$prev1ano
        )
      }
    })

    ## Tabela - Quantidade de Benefícios Emitidos em Dezembro----
    t12 <- reactive({
      if (input$prev1ri == "Pará") {
        paste0(
          "Quantidade de Benefícios Emitidos em Dezembro por Município, Pará - ",
          input$prev1ano
        )
      } else {
        paste0(
          "Quantidade de Benefícios Emitidos em Dezembro por Município, Região de Integração ",
          input$prev1ri,
          " - ",
          input$prev1ano
        )
      }
    })

    ## Gráfico - Quantidade de Benefícios Emitidos em Dezembro----
    t13 <- reactive({
      paste0(
        "Quantidade de Benefícios Emitidos em Dezembro, Pará - ",
        min(prev1$ano),
        " a ",
        max(prev1$ano)
      )
    })

    # 2 - Valor dos Benefícios Emitidos em Dezembro----
    ## Mapa - Valor dos Benefícios Emitidos em Dezembro----
    t21 <- reactive({
      if (input$prev2ri == "Pará") {
        paste0(
          "Valor dos Benefícios Emitidos em Dezembro, Pará - ",
          input$prev2ano
        )
      } else {
        paste0(
          "Valor dos Benefícios Emitidos em Dezembro, Região de Integração ",
          input$prev2ri,
          " - ",
          input$prev2ano
        )
      }
    })

    ## Tabela - Valor dos Benefícios Emitidos em Dezembro----
    t22 <- reactive({
      if (input$prev2ri == "Pará") {
        paste0(
          "Valor dos Benefícios Emitidos em Dezembro por Município, Pará - ",
          input$prev2ano
        )
      } else {
        paste0(
          "Valor dos Benefícios Emitidos em Dezembro por Município, Região de Integração ",
          input$prev2ri,
          " - ",
          input$prev2ano
        )
      }
    })

    ## Gráfico - Valor dos Benefícios Emitidos em Dezembro----
    t23 <- reactive({
      paste0(
        "Valor dos Benefícios Emitidos em Dezembro, Pará - ",
        min(prev2$ano),
        " a ",
        max(prev2$ano)
      )
    })

    # 3 - Valor dos Benefícios Emitidos ao Ano ----
    ## Mapa - Valor dos Benefícios Emitidos ao Ano ----
    t31 <- reactive({
      if (input$prev3ri == "Pará") {
        paste0(
          "Valor dos Benefícios Emitidos ao Ano, Pará - ",
          input$prev3ano
        )
      } else {
        paste0(
          "Valor dos Benefícios Emitidos ao Ano, Região de Integração ",
          input$prev3ri,
          " - ",
          input$prev3ano
        )
      }
    })

    ## Tabela - Valor dos Benefícios Emitidos ao Ano ----
    t32 <- reactive({
      if (input$prev3ri == "Pará") {
        paste0(
          "Valor dos Benefícios Emitidos ao Ano por Município, Pará - ",
          input$prev3ano
        )
      } else {
        paste0(
          "Valor dos Benefícios Emitidos ao Ano por Município, Região de Integração ",
          input$prev3ri,
          " - ",
          input$prev3ano
        )
      }
    })

    ## Gráfico - Valor dos Benefícios Emitidos ao Ano ----
    t33 <- reactive({
      paste0(
        "Valor dos Benefícios Emitidos ao Ano em Dezembro, Pará - ",
        min(prev3$ano),
        " a ",
        max(prev3$ano)
      )
    })
    # 4 - Benefícios Emitidos pela Previdência Social por Localização----
    ## Arrecadação e Benefícios Emitidos pela Previdência Social----
    ### 4.1-Quantidade de benefícios emitidos no mês de dezembro----
    ### Mapa - Quantidade de benefícios emitidos no mês de dezembro----
    t411 <- reactive({
      if (input$prev4ri1 == "Pará") {
        paste0(
          "Quantidade de benefícios emitidos no mês de dezembro ",
          input$prev4cat1,
          ", Pará - ",
          input$prev4ano1
        )
      } else {
        paste0(
          "Quantidade de benefícios emitidos no mês de dezembro ",
          input$prev4cat1,
          ", Região de Integração ",
          input$prev4ri1,
          " - ",
          input$prev4ano1
        )
      }
    })

    ### Tabela - Quantidade de benefícios emitidos no mês de dezembro----
    t412 <- reactive({
      if (input$prev4ri1 == "Pará") {
        paste0(
          "Quantidade de benefícios emitidos no mês de dezembro Total por Localização e Município, Pará - ",
          input$prev4ano1
        )
      } else {
        paste0(
          "Quantidade de benefícios emitidos no mês de dezembro Total por Localização e Município, Região de Integração ",
          input$prev4ri1,
          " - ",
          input$prev4ano1
        )
      }
    })

    ### Gráfico - Quantidade de benefícios emitidos no mês de dezembro----
    t413 <- reactive({
      paste0(
        "Quantidade de benefícios emitidos no mês de dezembro Total e por Localização, Pará - ",
        min(prev4_1$ano),
        " a ",
        max(prev4_1$ano)
      )
    })
    ### 4.2-Valor dos benefícios emitidos no mês de dezembro (em R$)----
    ### Mapa - Valor dos benefícios emitidos no mês de dezembro (em R$)----
    t421 <- reactive({
      if (input$prev4ri2 == "Pará") {
        paste0(
          "Valor dos benefícios emitidos no mês de dezembro (em R$) ",
          input$prev4cat2,
          ", Pará - ",
          input$prev4ano2
        )
      } else {
        paste0(
          "Valor dos benefícios emitidos no mês de dezembro (em R$) ",
          input$demo2cat,
          ", Região de Integração ",
          input$prev4cat2,
          " - ",
          input$prev4ano2
        )
      }
    })

    ### Tabela - Valor dos benefícios emitidos no mês de dezembro (em R$)----
    t422 <- reactive({
      if (input$prev4ri2 == "Pará") {
        paste0(
          "Valor dos benefícios emitidos no mês de dezembro (em R$) Total por Localização e Município, Pará - ",
          input$prev4ano2
        )
      } else {
        paste0(
          "Valor dos benefícios emitidos no mês de dezembro (em R$) Total por Localização e Município, Região de Integração  ",
          input$prev4ri2,
          " - ",
          input$prev4ano2
        )
      }
    })

    ### Gráfico - Valor dos benefícios emitidos no mês de dezembro (em R$)----
    t423 <- reactive({
      paste0(
        "Valor dos benefícios emitidos no mês de dezembro (em R$) Total e por Localização, Pará - ",
        min(prev4_2$ano),
        " a ",
        max(prev4_2$ano)
      )
    })
    ### 4.3 - Valor dos benefícios emitidos no ano (em R$)----
    ### Mapa - Valor dos benefícios emitidos no ano (em R$)----
    t431 <- reactive({
      if (input$prev4ri3 == "Pará") {
        paste0(
          "Valor dos benefícios emitidos no ano (em R$) ",
          input$prev4cat3,
          ", Pará - ",
          input$prev4ano3
        )
      } else {
        paste0(
          "Valor dos benefícios emitidos no ano (em R$) ",
          input$prev4cat3,
          ", Região de Integração ",
          input$prev4ri3,
          " - ",
          input$prev4ano3
        )
      }
    })

    ### Tabela - Valor dos benefícios emitidos no ano (em R$)----
    t432 <- reactive({
      if (input$prev4ri3 == "Pará") {
        paste0(
          "Valor dos benefícios emitidos no ano (em R$) Total por Localização e Município, Pará - ",
          input$prev4ano3
        )
      } else {
        paste0(
          "Valor dos benefícios emitidos no ano (em R$) Total por Localização e Município, Região de Integração  ",
          input$prev4ri3,
          " - ",
          input$prev4ano3
        )
      }
    })

    ### Gráfico - Valor dos benefícios emitidos no ano (em R$)----
    t433 <- reactive({
      paste0(
        "Valor dos benefícios emitidos no ano (em R$) Total e por Localização, Pará - ",
        min(prev4_3$ano),
        " a ",
        max(prev4_3$ano)
      )
    })
    # VISUALIZAÇÃO----
    # 1 - Quantidade de Benefícios Emitidos em Dezembro----
    ## Mapa - Quantidade de Benefícios Emitidos em Dezembro----
    output$prev1txt1 <- renderText({
      t11()
    })

    output$prev1map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$prev1ri == "Pará") {
        df <- prev1 %>%
          filter(localidade != "Pará", ano == input$prev1ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x$valor[x$valor == 0] <- NA
      } else {
        df <- prev1 %>%
          filter(localidade != "Pará", ano == input$prev1ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$prev1ri)
        x$valor[x$valor == 0] <- NA
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
          c("#FFEBCD", "#ffcd97", "#ffaf66", "#fa9236", "#f17701"),
          domain = x$valor,
          bins = bins
        )
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Quantidade:</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
            format(
              round(x$valor, digits = 0),
              big.mark = ".",
              decimal.mark = ","
            )
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
          title = "Quantidade",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(
            big.mark = ".",
            decimal.mark = ",",
            digits = 2
          )
        )
    })
    ## Tabela - Quantidade de Benefícios Emitidos em Dezembro----
    output$prev1txt2 <- renderText({
      t12()
    })

    output$prev1tab <- renderReactable({
      if (input$prev1ri == "Pará") {
        x <- prev1 %>%
          filter(localidade != "Pará", ano == input$prev1ano) %>%
          select(ri, localidade, valor) %>%
          mutate(Percentual = (valor / sum(valor)) * 100)
      } else {
        x <- prev1 %>%
          filter(localidade != "Pará", ano == input$prev1ano) %>%
          select(ri, localidade, valor)
        x <- x %>%
          filter(ri == input$prev1ri) %>%
          mutate(Percentual = (valor / sum(valor)) * 100)
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
          ri = colDef(name = "Região de Integração", width = 200),
          localidade = colDef(name = "Municípios"),
          valor = colDef(
            name = "Quantidade",
            format = colFormat(separators = T, locales = "pt-Br")
          ),
          Percentual = colDef(format = colFormat(
            digits = 2,
            suffix = "%",
            locales = "pt-Br"
          ))
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
    ## Gráfico - Quantidade de Benefícios Emitidos em Dezembro----
    output$prev1txt3 <- renderText({
      t13()
    })
    output$prev1graf <- renderEcharts4r({
      prev1 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#f17701",
          name = "Quantidade",
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
          name = "Quantidade",
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

    # 2 - Valor dos Benefícios Emitidos em Dezembro----
    ## Mapa - Valor dos Benefícios Emitidos em Dezembro----
    output$prev2txt1 <- renderText({
      t21()
    })

    output$prev2map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$prev2ri == "Pará") {
        df <- prev2 %>%
          filter(localidade != "Pará", ano == input$prev2ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x$valor[x$valor == 0] <- NA
      } else {
        df <- prev2 %>%
          filter(localidade != "Pará", ano == input$prev2ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$prev2ri)
        x$valor[x$valor == 0] <- NA
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
          c("#FFEBCD", "#ffcd97", "#ffaf66", "#fa9236", "#f17701"),
          domain = x$valor,
          bins = bins
        )
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Valor(R$):</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
            format(
              x$valor,
              big.mark = ".",
              decimal.mark = ",",
              digits = 6
            )
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
          title = "Valor(R$)",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(
            big.mark = ".",
            decimal.mark = ",",
            digits = 0
          )
        )
    })
    ## Tabela - Valor dos Benefícios Emitidos em Dezembro----
    output$prev2txt2 <- renderText({
      t22()
    })
    output$prev2tab <- renderReactable({
      if (input$prev2ri == "Pará") {
        x <- prev2 %>%
          filter(localidade != "Pará", ano == input$prev2ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- prev2 %>%
          filter(localidade != "Pará", ano == input$prev2ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$prev2ri)
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
          ri = colDef(name = "Região de Integração", width = 200),
          localidade = colDef(name = "Municípios"),
          valor = colDef(
            name = "Valor dos Benefícios",
            format = colFormat(
              separators = T,
              digits = 0,
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

    ## Gráfico - Valor dos Benefícios Emitidos em Dezembro----
    output$prev2txt3 <- renderText({
      t23()
    })
    output$prev2graf <- renderEcharts4r({
      prev2 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#f17701",
          name = "Valor(R$)",
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
          name = "Valor(R$)",
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

    # 3 - Valor dos Benefícios Emitidos ao Ano ----
    ## Mapa - Valor dos Benefícios Emitidos ao Ano ----
    output$prev3txt1 <- renderText({
      t31()
    })

    output$prev3map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$prev3ri == "Pará") {
        df <- prev3 %>%
          filter(localidade != "Pará", ano == input$prev3ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x$valor[x$valor == 0] <- NA
      } else {
        df <- prev3 %>%
          filter(localidade != "Pará", ano == input$prev3ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$prev3ri)
        x$valor[x$valor == 0] <- NA
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
          c("#FFEBCD", "#ffcd97", "#ffaf66", "#fa9236", "#f17701"),
          domain = x$valor,
          bins = bins
        )
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Valor(R$):</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
            format(
              round(x$valor, digits = 0),
              big.mark = ".",
              decimal.mark = ","
            )
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
          title = "Valor(R$)",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(
            big.mark = ".",
            decimal.mark = ",",
            digits = 2
          )
        )
    })
    ## Tabela - Valor dos Benefícios Emitidos ao Ano ----
    output$prev3txt2 <- renderText({
      t32()
    })
    output$prev3tab <- renderReactable({
      if (input$prev3ri == "Pará") {
        x <- prev3 %>%
          filter(localidade != "Pará", ano == input$prev3ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- prev3 %>%
          filter(localidade != "Pará", ano == input$prev3ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$prev3ri)
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
          ri = colDef(name = "Região de Integração", width = 200),
          localidade = colDef(name = "Municípios"),
          valor = colDef(
            name = "Valor dos Benefícios",
            format = colFormat(
              separators = T,
              digits = 0,
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

    ## Gráfico - Valor dos Benefícios Emitidos ao Ano ----
    output$prev3txt3 <- renderText({
      t33()
    })
    output$prev3graf <- renderEcharts4r({
      prev3 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#f17701",
          name = "Valor(R$)",
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
          name = "Valor(R$)",
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
    # 4 - Benefícios Emitidos pela Previdência Social por Localização----
    ## Arrecadação e Benefícios Emitidos pela Previdência Social----
    ### 4.1-Quantidade de benefícios emitidos no mês de dezembro----
    ### Mapa - Quantidade de benefícios emitidos no mês de dezembro----
    output$prev4_1txt1 <- renderText({
      t411()
    })

    output$prev4map1 <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$prev4ri1 == "Pará") {
        df <- prev4_1 %>%
          filter(
            localidade != "Pará",
            ano == input$prev4ano1,
            categoria == input$prev4cat1
          ) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x$valor[x$valor == 0] <- NA
      } else {
        df <- prev4_1 %>%
          filter(
            localidade != "Pará",
            ano == input$prev4ano1,
            categoria == input$prev4cat1
          ) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$prev4ri1)
        x$valor[x$valor == 0] <- NA
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
          c("#FFEBCD", "#ffcd97", "#ffaf66", "#fa9236", "#f17701"),
          domain = x$valor,
          bins = bins
        )
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Quantidade:</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
            format(
              round(x$valor, digits = 0),
              big.mark = ".",
              decimal.mark = ","
            )
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
          title = "Quantidade",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(
            big.mark = ".",
            decimal.mark = ",",
            digits = 2
          )
        )
    })
    ### Tabela - Quantidade de benefícios emitidos no mês de dezembro----
    output$prev4_1txt2 <- renderText({
      t412()
    })

    output$prev4tab1 <- renderReactable({
      if (input$prev4ri1 == "Pará") {
        x <- prev4_1 %>%
          filter(
            localidade != "Pará",
            ano == input$prev4ano1
          ) %>%
          select(
            ri,
            localidade,
            categoria,
            valor
          ) %>%
          pivot_wider(names_from = categoria, values_from = valor)
      } else {
        x <- prev4_1 %>%
          filter(
            localidade != "Pará",
            ano == input$prev4ano1
          ) %>%
          select(
            ri,
            localidade,
            categoria,
            valor
          )
        x <- x %>%
          filter(ri == input$prev4ri1) %>%
          pivot_wider(names_from = categoria, values_from = valor)
      }
      reactable(
        x,
        defaultPageSize = 10,
        striped = FALSE,
        highlight = TRUE,
        bordered = TRUE,
        outlined = TRUE,
        resizable = TRUE,
        showSortable = TRUE,
        pagination = F,
        columns = list(
          ri = colDef(name = "Região de Integração", width = 200),
          localidade = colDef(width = 200, name = "Municípios")
        ),
        defaultColDef = colDef(
          footerStyle = list(fontWeight = "bold"),
          format = colFormat(
            separators = T,
            digits = 0,
            locales = "pt-BR"
          ),
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
    ### Gráfico - Quantidade de benefícios emitidos no mês de dezembro----
    output$prev4_1txt3 <- renderText({
      t413()
    })
    output$prev4graf1 <- renderEcharts4r({
      prev4_1 %>%
        filter(
          localidade == "Pará"
        ) %>%
        pivot_wider(names_from = categoria, values_from = valor) %>%
        e_chart(x = ano) %>%
        e_line(
          serie = Total,
          name = "Total",
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
          barWidth = "25%",
          itemStyle = list(barBorderRadius = 5)
        ) %>%
        e_line(
          serie = Urbano,
          name = "Urbano",
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
          itemStyle = list(barBorderRadius = 5)
        ) %>%
        e_line(
          serie = Rural,
          name = "Rural",
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
          itemStyle = list(barBorderRadius = 5)
        ) %>%
        e_legend(show = T) %>%
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
          name = "Quantidade",
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
    ### 4.2-Valor dos benefícios emitidos no mês de dezembro (em R$)----
    ### Mapa - Valor dos benefícios emitidos no mês de dezembro (em R$)----
    output$prev4_2txt1 <- renderText({
      t421()
    })

    output$prev4map2 <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$prev4ri2 == "Pará") {
        df <- prev4_2 %>%
          filter(
            localidade != "Pará",
            ano == input$prev4ano2,
            categoria == input$prev4cat2
          ) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x$valor[x$valor == 0] <- NA
      } else {
        df <- prev4_2 %>%
          filter(
            localidade != "Pará",
            ano == input$prev4ano2,
            categoria == input$prev4cat2
          ) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$prev4ri2)
        x$valor[x$valor == 0] <- NA
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
          c("#FFEBCD", "#ffcd97", "#ffaf66", "#fa9236", "#f17701"),
          domain = x$valor,
          bins = bins
        )
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Valor(R$):</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
            format(
              round(x$valor, digits = 0),
              big.mark = ".",
              decimal.mark = ","
            )
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
          title = "Valor(R$)",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(
            big.mark = ".",
            decimal.mark = ",",
            digits = 2
          )
        )
    })
    ### Tabela - Valor dos benefícios emitidos no mês de dezembro (em R$)----
    output$prev4_2txt2 <- renderText({
      t422()
    })

    output$prev4tab2 <- renderReactable({
      if (input$prev4ri2 == "Pará") {
        x <- prev4_2 %>%
          filter(
            localidade != "Pará",
            ano == input$prev4ano2
          ) %>%
          select(
            ri,
            localidade,
            categoria,
            valor
          ) %>%
          pivot_wider(names_from = categoria, values_from = valor)
      } else {
        x <- prev4_2 %>%
          filter(
            localidade != "Pará",
            ano == input$prev4ano2
          ) %>%
          select(
            ri,
            localidade,
            categoria,
            valor
          )
        x <- x %>%
          filter(ri == input$prev4ri2) %>%
          pivot_wider(names_from = categoria, values_from = valor)
      }
      reactable(
        x,
        defaultPageSize = 10,
        striped = FALSE,
        highlight = TRUE,
        bordered = TRUE,
        outlined = TRUE,
        resizable = TRUE,
        showSortable = TRUE,
        pagination = F,
        columns = list(
          ri = colDef(name = "Região de Integração", width = 200),
          localidade = colDef(width = 200, name = "Municípios"),
          Total = colDef(format = colFormat(
            separators = T,
            digits = 0,
            locales = "pt-BR"
          )),
          Urbano = colDef(format = colFormat(
            separators = T,
            digits = 0,
            locales = "pt-BR"
          )),
          Rural = colDef(format = colFormat(
            separators = T,
            digits = 0,
            locales = "pt-BR"
          ))
        ),
        defaultColDef = colDef(
          footerStyle = list(fontWeight = "bold"),
          format = colFormat(
            separators = T,
            digits = 0,
            locales = "pt-BR"
          ),
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
    ### Gráfico - valor dos benefícios emitidos no mês de dezembro (em R$)----
    output$prev4_2txt3 <- renderText({
      t423()
    })
    output$prev4graf2 <- renderEcharts4r({
      prev4_2 %>%
        filter(
          localidade == "Pará"
        ) %>%
        pivot_wider(names_from = categoria, values_from = valor) %>%
        e_chart(x = ano) %>%
        e_line(
          serie = Total,
          name = "Total",
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
          barWidth = "25%",
          itemStyle = list(barBorderRadius = 5)
        ) %>%
        e_line(
          serie = Urbano,
          name = "Urbano",
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
          itemStyle = list(barBorderRadius = 5)
        ) %>%
        e_line(
          serie = Rural,
          name = "Rural",
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
          itemStyle = list(barBorderRadius = 5)
        ) %>%
        e_legend(show = T) %>%
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
          name = "Valor(R$)",
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
    ### 4.3 - Valor dos benefícios emitidos no ano (em R$)----
    ### Mapa - Valor dos benefícios emitidos no ano (em R$)----
    output$prev4_3txt1 <- renderText({
      t431()
    })

    output$prev4map3 <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$prev4ri3 == "Pará") {
        df <- prev4_3 %>%
          filter(
            localidade != "Pará",
            ano == input$prev4ano3,
            categoria == input$prev4cat3
          ) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x$valor[x$valor == 0] <- NA
      } else {
        df <- prev4_3 %>%
          filter(
            localidade != "Pará",
            ano == input$prev4ano3,
            categoria == input$prev4cat3
          ) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$prev4ri3)
        x$valor[x$valor == 0] <- NA
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
          c("#FFEBCD", "#ffcd97", "#ffaf66", "#fa9236", "#f17701"),
          domain = x$valor,
          bins = bins
        )
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Valor(R$):</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
            format(
              round(x$valor, digits = 0),
              big.mark = ".",
              decimal.mark = ","
            )
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
          title = "Valor(R$)",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(
            big.mark = ".",
            decimal.mark = ",",
            digits = 2
          )
        )
    })
    ### Tabela - Valor dos benefícios emitidos no ano (em R$)----
    output$prev4_3txt2 <- renderText({
      t432()
    })

    output$prev4tab3 <- renderReactable({
      if (input$prev4ri2 == "Pará") {
        x <- prev4_3 %>%
          filter(
            localidade != "Pará",
            ano == input$prev4ano3
          ) %>%
          select(
            ri,
            localidade,
            categoria,
            valor
          ) %>%
          pivot_wider(names_from = categoria, values_from = valor)
      } else {
        x <- prev4_3 %>%
          filter(
            localidade != "Pará",
            ano == input$prev4ano3
          ) %>%
          select(
            ri,
            localidade,
            categoria,
            valor
          )
        x <- x %>%
          filter(ri == input$prev4ri3) %>%
          pivot_wider(names_from = categoria, values_from = valor)
      }
      reactable(
        x,
        defaultPageSize = 10,
        striped = FALSE,
        highlight = TRUE,
        bordered = TRUE,
        outlined = TRUE,
        resizable = TRUE,
        showSortable = TRUE,
        pagination = F,
        columns = list(
          ri = colDef(width = 200),
          ri = colDef(name = "Região de Integração", width = 200),
          localidade = colDef(width = 200, name = "Municípios"),
          Total = colDef(format = colFormat(
            separators = T,
            digits = 0,
            locales = "pt-BR"
          )),
          Urbano = colDef(format = colFormat(
            separators = T,
            digits = 0,
            locales = "pt-BR"
          )),
          Rural = colDef(format = colFormat(
            separators = T,
            digits = 0,
            locales = "pt-BR"
          ))
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
    ### Gráfico - Valor dos benefícios emitidos no ano (em R$)----
    output$prev4_3txt3 <- renderText({
      t433()
    })
    output$prev4graf3 <- renderEcharts4r({
      prev4_3 %>%
        filter(
          localidade == "Pará"
        ) %>%
        pivot_wider(names_from = categoria, values_from = valor) %>%
        e_chart(x = ano) %>%
        e_line(
          serie = Total,
          name = "Total",
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
          barWidth = "25%",
          itemStyle = list(barBorderRadius = 5)
        ) %>%
        e_line(
          serie = Urbano,
          name = "Urbano",
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
          itemStyle = list(barBorderRadius = 5)
        ) %>%
        e_line(
          serie = Rural,
          name = "Rural",
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
          itemStyle = list(barBorderRadius = 5)
        ) %>%
        e_legend(show = T) %>%
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
          name = "Valor(R$)",
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
    # DOWNLOADS----
    # 1 - Quantidade de Benefícios Emitidos em Dezembro----
    ## Tabela - Quantidade de Benefícios Emitidos em Dezembro----
    # Filtra os dados
    prev1_1 <- reactive({
      if (input$prev1ri == "Pará") {
        x <- prev1 %>%
          filter(
            localidade != "Pará",
            ano == input$prev1ano
          ) %>%
          mutate(Percentual = (valor / sum(valor)) * 100)
      } else {
        x <- prev1 %>%
          filter(
            localidade != "Pará",
            ano == input$prev1ano,
            ri == input$prev1ri
          ) %>%
          mutate(Percentual = (valor / sum(valor)) * 100)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(prev1_1(), {
      downset_Server("prev1_1", prev1_1(), t12())
    })
    ## Gráfico - Quantidade de Benefícios Emitidos em Dezembro----
    # Filtra os dados
    prev1_2 <- reactive({
      prev1 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(prev1_2(), {
      downset_Server("prev1_2", prev1_2(), t13())
    })

    # 2 - Valor dos Benefícios Emitidos em Dezembro----
    ## Tabela - Valor dos Benefícios Emitidos em Dezembro----
    # Filtra os dados
    prev2_1 <- reactive({
      if (input$prev2ri == "Pará") {
        x <- prev2 %>%
          filter(
            localidade != "Pará",
            ano == input$prev2ano
          )
      } else {
        x <- prev2 %>%
          filter(
            localidade != "Pará",
            ano == input$prev2ano,
            ri == input$prev2ri
          )
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(prev2_1(), {
      downset_Server("prev2_1", prev2_1(), t22())
    })
    ## Gráfico - Valor dos Benefícios Emitidos em Dezembro----
    # Filtra os dados
    prev2_2 <- reactive({
      prev2 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(prev2_2(), {
      downset_Server("prev2_2", prev2_2(), t23())
    })

    # 3 - Valor dos Benefícios Emitidos ao Ano ----
    ## Tabela - Valor dos Benefícios Emitidos ao Ano ----
    # Filtra os dados
    prev3_1 <- reactive({
      if (input$prev3ri == "Pará") {
        x <- prev3 %>%
          filter(
            localidade != "Pará",
            ano == input$prev3ano
          )
      } else {
        x <- prev3 %>%
          filter(
            localidade != "Pará",
            ano == input$prev3ano,
            ri == input$prev3ri
          )
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(prev3_1(), {
      downset_Server("prev3_1", prev3_1(), t32())
    })
    ## Gráfico - Valor dos Benefícios Emitidos ao Ano ----
    # Filtra os dados
    prev3_2 <- reactive({
      prev3 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(prev3_2(), {
      downset_Server("prev3_2", prev3_2(), t33())
    })

    # 4 - Benefícios Emitidos pela Previdência Social por Localização----
    ### 4.1-Quantidade de benefícios emitidos no mês de dezembro----
    ### Tabela - Quantidade de benefícios emitidos no mês de dezembro----
    # Filtra os dados
    prev4_1_1 <- reactive({
      if (input$prev4ri1 == "Pará") {
        x <- prev4_1 %>%
          filter(
            localidade != "Pará",
            ano == input$prev4ano1
          )
      } else {
        x <- prev4_1 %>%
          filter(
            localidade != "Pará",
            ano == input$prev4ano1,
            ri == input$prev4ri1
          )
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(prev4_1_1(), {
      downset_Server("prev4_1_1", prev4_1_1(), t412())
    })
    ### Gráfico - Quantidade de benefícios emitidos no mês de dezembro----
    # Filtra os dados
    prev4_1_2 <- reactive({
      prev4_1 %>%
        filter(
          localidade == "Pará"
        )
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(prev4_1_2(), {
      downset_Server("prev4_1_2", prev4_1_2(), t413())
    })

    ### 4.2-Valor dos benefícios emitidos no mês de dezembro (em R$)----
    ### Tabela - Valor dos benefícios emitidos no mês de dezembro (em R$)----
    # Filtra os dados
    prev4_2_1 <- reactive({
      if (input$prev4ri2 == "Pará") {
        x <- prev4_2 %>%
          filter(
            localidade != "Pará",
            ano == input$prev4ano2
          )
      } else {
        x <- prev4_2 %>%
          filter(
            localidade != "Pará",
            ano == input$prev4ano2,
            ri == input$prev4ri2
          )
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(prev4_2_1(), {
      downset_Server("prev4_2_1", prev4_2_1(), t422())
    })
    ### Gráfico - Valor dos benefícios emitidos no mês de dezembro (em R$)----
    # Filtra os dados
    prev4_2_2 <- reactive({
      prev4_2 %>%
        filter(
          localidade == "Pará"
        )
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(prev4_2_2(), {
      downset_Server("prev4_2_2", prev4_2_2(), t423())
    })

    ### 4.3-Valor dos benefícios emitidos no ano (em R$)----
    ### Tabela - Valor dos benefícios emitidos no ano (em R$)----
    # Filtra os dados
    prev4_3_1 <- reactive({
      if (input$prev4ri2 == "Pará") {
        x <- prev4_3 %>%
          filter(
            localidade != "Pará",
            ano == input$prev4ano3
          )
      } else {
        x <- prev4_3 %>%
          filter(
            localidade != "Pará",
            ano == input$prev4ano3,
            ri == input$prev4ri3
          )
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(prev4_3_1(), {
      downset_Server("prev4_3_1", prev4_3_1(), t432())
    })
    ### Gráfico - Valor dos benefícios emitidos no ano (em R$)----
    # Filtra os dados
    prev4_3_2 <- reactive({
      prev4_3 %>%
        filter(
          localidade == "Pará"
        )
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(prev4_3_2(), {
      downset_Server("prev4_3_2", prev4_3_2(), t433())
    })
  })
}


# Play do Módulo
# ui <- dashboardPage(header = dashboardHeader(),
#                     sidebar = dashboardSidebar(),
#                     body = dashboardBody(fluidPage(
#                       social_previdencia_pa_ui("social_previdencia_pa")
#                     )))
# 
# 
# server <- function(input, output) {
#   social_previdencia_pa_Server("social_previdencia_pa")
# }
# 
# shinyApp(ui, server)
