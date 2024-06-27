# Funções de módulo de Social - Segurança - Estadual
# Função de UI
social_seguranca_pa_ui <- function(id) {
  fluidPage(
    # CAMINHO DO ARQUIVO CSS----
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "estilo.css")
    ),
    div(
      class = "navbar_social",
      navbarPage(
        tags$b("Segurança - Pará"),
        navbarMenu(
          "Indicadores",
          # 1-Taxa de Homicídios Total por 100.000 habitantes----
          tabPanel(
            "Taxa de Homicídios Total por 100.000 habitantes",
            panel(
              ## Controle----
              heading =
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Taxa de Homicídios Total por 100.000 habitantes"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "seg1ano"),
                  label = "Ano",
                  choices = sort(unique(seg1[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "seg1ri"),
                  label = "Região de Integração",
                  choices = unique(seg1[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Taxa de Homicídios Total por 100.000 habitantes----
              box(
                title = textOutput(NS(id, "seg1txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "seg1map"), height = "600px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  tags$h6(tags$b("Fonte:"), "DATASUS"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                )
              ),
              ## Tabela - Taxa de Homicídios Total por 100.000 habitantes----
              box(
                title = textOutput(NS(id, "seg1txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "seg1tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(11,
                  tags$h6(tags$b("Fonte:"), "DATASUS"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                  downset_ui(NS(id, "seg1_1"))
                )
              )
            ),
            fluidRow(
              ## Gráfico - Taxa de Homicídios Total por 100.000 habitantes----
              box(
                title = textOutput(NS(id, "seg1txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "seg1graf")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(11,
                  tags$h6(tags$b("Fonte:"), "DATASUS"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                  downset_ui(NS(id, "seg1_2"))
                )
              )
            )
          ),
          # 2-Taxa de Homicídios de Jovens por 100.000 habitantes----
          tabPanel(
            "Taxa de Homicídios de Jovens por 100.000 habitantes",
            panel(
              ## Controle----
              heading =
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Taxa de Homicídios de Jovens por 100.000 habitantes"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "seg2ano"),
                  label = "Ano",
                  choices = sort(unique(seg2[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "seg2ri"),
                  label = "Região de Integração",
                  choices = unique(seg2[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Taxa de Homicídios de Jovens por 100.000 habitantes----
              box(
                title = textOutput(NS(id, "seg2txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "seg2map"), height = "600px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  tags$h6(tags$b("Fonte:"), "DATASUS"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                )
              ),
              ## Tabela - Taxa de Homicídios de Jovens por 100.000 habitantes----
              box(
                title = textOutput(NS(id, "seg2txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "seg2tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(11,
                  tags$h6(tags$b("Fonte:"), "DATASUS"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                  downset_ui(NS(id, "seg2_1"))
                )
              )
            ),
            fluidRow(
              ## Gráfico - Taxa de Homicídios de Jovens por 100.000 habitantes----
              box(
                title = textOutput(NS(id, "seg2txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "seg2graf")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(11,
                  tags$h6(tags$b("Fonte:"), "DATASUS"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                  downset_ui(NS(id, "seg2_2"))
                )
              )
            )
          ),
          # 3-Taxa de Mortes no Trânsito por 100.000 habitantes----
          tabPanel(
            "Taxa de Mortes no Trânsito por 100.000 habitantes",
            panel(
              ## Controle----
              heading =
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Taxa de Mortes no Trânsito por 100.000 habitantes"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "seg3ano"),
                  label = "Ano",
                  choices = sort(unique(seg3[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "seg3ri"),
                  label = "Região de Integração",
                  choices = unique(seg3[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Taxa de Mortes no Trânsito por 100.000 habitantes----
              box(
                title = textOutput(NS(id, "seg3txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "seg3map"), height = "600px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  tags$h6(tags$b("Fonte:"), "DATASUS"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                )
              ),
              ## Tabela - Taxa de Mortes no Trânsito por 100.000 habitantes----
              box(
                title = textOutput(NS(id, "seg3txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "seg3tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(11,
                  tags$h6(tags$b("Fonte:"), "DATASUS"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                  downset_ui(NS(id, "seg3_1"))
                )
              )
            ),
            fluidRow(
              ## Gráfico - Taxa de Mortes no Trânsito por 100.000 habitantes----
              box(
                title = textOutput(NS(id, "seg3txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "seg3graf")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(11,
                  tags$h6(tags$b("Fonte:"), "DATASUS"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                  downset_ui(NS(id, "seg3_2"))
                )
              )
            )
          ),
          # 4-Taxa do Crime de Homicídio por 100.000 habitantes----
          tabPanel(
            "Taxa do Crime de Homicídio por 100.000 habitantes",
            panel(
              ## Controle----
              heading =
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Taxa do Crime de Homicídio por 100.000 habitantes"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "seg4ano"),
                  label = "Ano",
                  choices = sort(unique(seg4[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "seg4ri"),
                  label = "Região de Integração",
                  choices = unique(seg4[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Taxa do Crime de Homicídio por 100.000 habitantes----
              box(
                title = textOutput(NS(id, "seg4txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "seg4map"), height = "600px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  tags$h6(tags$b("Fonte:"), "SEGUP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                )
              ),
              ## Tabela - Taxa do Crime de Homicídio por 100.000 habitantes----
              box(
                title = textOutput(NS(id, "seg4txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "seg4tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(11,
                  tags$h6(tags$b("Fonte:"), "SEGUP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                  downset_ui(NS(id, "seg4_1"))
                )
              )
            ),
            fluidRow(
              ## Gráfico - Taxa do Crime de Homicídio por 100.000 habitantes----
              box(
                title = textOutput(NS(id, "seg4txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "seg4graf")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(11,
                  tags$h6(tags$b("Fonte:"), "SEGUP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                  downset_ui(NS(id, "seg4_2"))
                )
              )
            )
          ),
          # 5-Taxa do Crime de Homicídio no Trânsito por 100.000 habitantes----
          tabPanel(
            "Taxa do Crime de Homicídio no Trânsito por 100.000 habitantes",
            panel(
              ## Controle----
              heading =
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Taxa do Crime de Homicídio no Trânsito por 100.000 habitantes"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "seg5ano"),
                  label = "Ano",
                  choices = sort(unique(seg5[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "seg5ri"),
                  label = "Região de Integração",
                  choices = unique(seg5[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Taxa do Crime de Homicídio no Trânsito por 100.000 habitantes----
              box(
                title = textOutput(NS(id, "seg5txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "seg5map"), height = "600px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  tags$h6(tags$b("Fonte:"), "SEGUP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                )
              ),
              ## Tabela - Taxa do Crime de Homicídio no Trânsito por 100.000 habitantes----
              box(
                title = textOutput(NS(id, "seg5txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "seg5tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(11,
                  tags$h6(tags$b("Fonte:"), "SEGUP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                  downset_ui(NS(id, "seg5_1"))
                )
              )
            ),
            fluidRow(
              ## Gráfico - Taxa do Crime de Homicídio no Trânsito por 100.000 habitantes----
              box(
                title = textOutput(NS(id, "seg5txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "seg5graf")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(11,
                  tags$h6(tags$b("Fonte:"), "SEGUP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                  downset_ui(NS(id, "seg5_2"))
                )
              )
            )
          ),
          # 6-Taxa do Crime de Roubo por 100.000 habitantes----
          tabPanel(
            "Taxa do Crime de Roubo por 100.000 habitantes",
            panel(
              ## Controle----
              heading =
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Taxa do Crime de Roubo por 100.000 habitantes"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "seg6ano"),
                  label = "Ano",
                  choices = sort(unique(seg6[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "seg6ri"),
                  label = "Região de Integração",
                  choices = unique(seg6[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Taxa do Crime de Roubo por 100.000 habitantes----
              box(
                title = textOutput(NS(id, "seg6txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "seg6map"), height = "600px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  tags$h6(tags$b("Fonte:"), "SEGUP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                )
              ),
              ## Tabela - Taxa do Crime de Roubo por 100.000 habitantes----
              box(
                title = textOutput(NS(id, "seg6txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "seg6tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(11,
                  tags$h6(tags$b("Fonte:"), "SEGUP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                  downset_ui(NS(id, "seg6_1"))
                )
              )
            ),
            fluidRow(
              ## Gráfico - Taxa do Crime de Roubo por 100.000 habitantes----
              box(
                title = textOutput(NS(id, "seg6txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "seg6graf")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(11,
                  tags$h6(tags$b("Fonte:"), "SEGUP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                  downset_ui(NS(id, "seg6_2"))
                )
              )
            )
          ),
          # 7-Taxa do Crime de Latrocínio por 100.000 habitantes----
          tabPanel(
            "Taxa do Crime de Latrocínio por 100.000 habitantes",
            panel(
              ## Controle----
              heading =
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Taxa do Crime de Latrocínio por 100.000 habitantes"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "seg7ano"),
                  label = "Ano",
                  choices = sort(unique(seg7[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "seg7ri"),
                  label = "Região de Integração",
                  choices = unique(seg7[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Taxa do Crime de Latrocínio por 100.000 habitantes----
              box(
                title = textOutput(NS(id, "seg7txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "seg7map"), height = "600px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  tags$h6(tags$b("Fonte:"), "SEGUP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                )
              ),
              ## Tabela - Taxa do Crime de Latrocínio por 100.000 habitantes----
              box(
                title = textOutput(NS(id, "seg7txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "seg7tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(11,
                  tags$h6(tags$b("Fonte:"), "SEGUP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                  downset_ui(NS(id, "seg7_1"))
                )
              )
            ),
            fluidRow(
              ## Gráfico - Taxa do Crime de Latrocínio por 100.000 habitantes----
              box(
                title = textOutput(NS(id, "seg7txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "seg7graf")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(11,
                  tags$h6(tags$b("Fonte:"), "SEGUP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                  downset_ui(NS(id, "seg7_2"))
                )
              )
            )
          ),
          # 8-Taxa do Crime de Lesão Corporal Seguida de Morte por 100.000 habitantes----
          tabPanel(
            "Taxa do Crime de Lesão Corporal Seguida de Morte por 100.000 habitantes",
            panel(
              ## Controle----
              heading =
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Taxa do Crime de Lesão Corporal Seguida de Morte por 100.000 habitantes"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "seg8ano"),
                  label = "Ano",
                  choices = sort(unique(seg8[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "seg8ri"),
                  label = "Região de Integração",
                  choices = unique(seg8[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Taxa do Crime de Lesão Corporal Seguida de Morte por 100.000 habitantes----
              box(
                title = textOutput(NS(id, "seg8txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "seg8map"), height = "600px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  tags$h6(tags$b("Fonte:"), "SEGUP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                )
              ),
              ## Tabela - Taxa do Crime de Lesão Corporal Seguida de Morte por 100.000 habitantes----
              box(
                title = textOutput(NS(id, "seg8txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "seg8tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(11,
                  tags$h6(tags$b("Fonte:"), "SEGUP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                  downset_ui(NS(id, "seg8_1"))
                )
              )
            ),
            fluidRow(
              ## Gráfico - Taxa do Crime de Lesão Corporal Seguida de Morte por 100.000 habitantes----
              box(
                title = textOutput(NS(id, "seg8txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "seg8graf")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(11,
                  tags$h6(tags$b("Fonte:"), "SEGUP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                  downset_ui(NS(id, "seg8_2"))
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
social_seguranca_pa_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    #TÍTULOS----
    # 1-Taxa de Homicídios Total por 100.000 habitantes----
    ## Mapa - Taxa de Homicídios Total por 100.000 habitantes----
    t11 <- reactive({
      if (input$seg1ri == "Pará") {
        paste0(
          "Taxa de Homicídios Total por 100.000 habitantes, Pará - ",
          input$seg1ano
        )
      } else{
        paste0(
          "Taxa de Homicídios Total por 100.000 habitantes, Região de Integração ",
          input$seg1ri,
          " - ",
          input$seg1ano
        )
      }
    })
    ## Tabela - Taxa de Homicídios Total por 100.000 habitantes----
    t12 <- reactive({
      if (input$seg1ri == "Pará") {
        paste0(
          "Taxa de Homicídios Total por 100.000 habitantes por Município, Pará - ",
          input$seg1ano
        )
      } else{
        paste0(
          "Taxa de Homicídios Total por 100.000 habitantes por Município, Região de Integração ",
          input$seg1ri,
          " - ",
          input$seg1ano
        )
      }
    })
    ## Gráfico - Taxa de Homicídios Total por 100.000 habitantes----
    t13 <- reactive({
      paste0("Taxa de Homicídios Total por 100.000 habitantes, Pará - ", min(seg1$ano), " a ", max(seg1$ano))
    })
    # 2-Taxa de Homicídios de Jovens por 100.000 habitantes----
    ## Mapa - Taxa de Homicídios de Jovens por 100.000 habitantes----
    t21 <- reactive({
      if (input$seg2ri == "Pará") {
        paste0(
          "Taxa de Homicídios de Jovens por 100.000 habitantes, Pará - ",
          input$seg2ano
        )
      } else{
        paste0(
          "Taxa de Homicídios de Jovens por 100.000 habitantes, Região de Integração ",
          input$seg2ri,
          " - ",
          input$seg2ano
        )
      }
    })
    ## Tabela - Taxa de Homicídios de Jovens por 100.000 habitantes----
    t22 <- reactive({
      if (input$seg2ri == "Pará") {
        paste0(
          "Taxa de Homicídios de Jovens por 100.000 habitantes por Município, Pará - ",
          input$seg2ano
        )
      } else{
        paste0(
          "Taxa de Homicídios de Jovens por 100.000 habitantes por Município, Região de Integração ",
          input$seg2ri,
          " - ",
          input$seg2ano
        )
      }
    })
    ## Gráfico - Taxa de Homicídios de Jovens por 100.000 habitantes----
    t23 <- reactive({
      paste0("Taxa de Homicídios de Jovens por 100.000 habitantes, Pará - ", min(seg2$ano), " a ", max(seg2$ano))
    })
    # 3-Taxa de Mortes no Trânsito por 100.000 habitantes----
    ## Mapa - Taxa de Mortes no Trânsito por 100.000 habitantes----
    t31 <- reactive({
      if (input$seg3ri == "Pará") {
        paste0(
          "Taxa de Mortes no Trânsito por 100.000 habitantes, Pará - ",
          input$seg3ano
        )
      } else{
        paste0(
          "Taxa de Mortes no Trânsito por 100.000 habitantes, Região de Integração ",
          input$seg3ri,
          " - ",
          input$seg3ano
        )
      }
    })
    ## Tabela - Taxa de Mortes no Trânsito por 100.000 habitantes----
    t32 <- reactive({
      if (input$seg3ri == "Pará") {
        paste0(
          "Taxa de Mortes no Trânsito por 100.000 habitantes por Município, Pará - ",
          input$seg3ano
        )
      } else{
        paste0(
          "Taxa de Mortes no Trânsito por 100.000 habitantes por Município, Região de Integração ",
          input$seg3ri,
          " - ",
          input$seg3ano
        )
      }
    })
    ## Gráfico - Taxa de Mortes no Trânsito por 100.000 habitantes----
    t33 <- reactive({
      paste0("Taxa de Mortes no Trânsito por 100.000 habitantes, Pará - ", min(seg3$ano), " a ", max(seg3$ano))
    })

    # 4-Taxa do Crime de Homicídio por 100.000 habitantes----
    ## Mapa - Taxa do Crime de Homicídio por 100.000 habitantes----
    t41 <- reactive({
      if (input$seg4ri == "Pará") {
        paste0(
          "Taxa do Crime de Homicídio por 100.000 habitantes, Pará - ",
          input$seg4ano
        )
      } else{
        paste0(
          "Taxa do Crime de Homicídio por 100.000 habitantes, Região de Integração ",
          input$seg4ri,
          " - ",
          input$seg4ano
        )
      }
    })
    ## Tabela - Taxa do Crime de Homicídio por 100.000 habitantes----
    t42 <- reactive({
      if (input$seg4ri == "Pará") {
        paste0(
          "Taxa do Crime de Homicídio por 100.000 habitantes por Município, Pará - ",
          input$seg4ano
        )
      } else{
        paste0(
          "Taxa do Crime de Homicídio por 100.000 habitantes por Município, Região de Integração ",
          input$seg4ri,
          " - ",
          input$seg4ano
        )
      }
    })
    ## Gráfico - Taxa do Crime de Homicídio por 100.000 habitantes----
    t43 <- reactive({
      paste0("Taxa do Crime de Homicídio por 100.000 habitantes, Pará - ", min(seg4$ano), " a ", max(seg4$ano))
    })

    # 5-Taxa do Crime de Homicídio no Trânsito por 100.000 habitantes----
    ## Mapa - Taxa do Crime de Homicídio no Trânsito por 100.000 habitantes----
    t51 <- reactive({
      if (input$seg5ri == "Pará") {
        paste0(
          "Taxa do Crime de Homicídio no Trânsito por 100.000 habitantes, Pará - ",
          input$seg5ano
        )
      } else{
        paste0(
          "Taxa do Crime de Homicídio no Trânsito por 100.000 habitantes, Região de Integração ",
          input$seg5ri,
          " - ",
          input$seg5ano
        )
      }
    })
    ## Tabela - Taxa do Crime de Homicídio no Trânsito por 100.000 habitantes----
    t52 <- reactive({
      if (input$seg5ri == "Pará") {
        paste0(
          "Taxa do Crime de Homicídio no Trânsito por 100.000 habitantes por Município, Pará - ",
          input$seg5ano
        )
      } else{
        paste0(
          "Taxa do Crime de Homicídio no Trânsito por 100.000 habitantes por Município, Região de Integração ",
          input$seg5ri,
          " - ",
          input$seg5ano
        )
      }
    })
    ## Gráfico - Taxa do Crime de Homicídio no Trânsito por 100.000 habitantes----
    t53 <- reactive({
      paste0("Taxa do Crime de Homicídio no Trânsito por 100.000 habitantes, Pará - ", min(seg5$ano), " a ", max(seg5$ano))
    })
    # 6-Taxa do Crime de Roubo por 100.000 habitantes----
    ## Mapa - Taxa do Crime de Roubo por 100.000 habitantes----
    t61 <- reactive({
      if (input$seg6ri == "Pará") {
        paste0(
          "Taxa do Crime de Roubo por 100.000 habitantes, Pará - ",
          input$seg6ano
        )
      } else{
        paste0(
          "Taxa do Crime de Roubo por 100.000 habitantes, Região de Integração ",
          input$seg6ri,
          " - ",
          input$seg6ano
        )
      }
    })
    ## Tabela - Taxa do Crime de Roubo por 100.000 habitantes----
    t62 <- reactive({
      if (input$seg6ri == "Pará") {
        paste0(
          "Taxa do Crime de Roubo por 100.000 habitantes por Município, Pará - ",
          input$seg6ano
        )
      } else{
        paste0(
          "Taxa do Crime de Roubo por 100.000 habitantes por Município, Região de Integração ",
          input$seg6ri,
          " - ",
          input$seg6ano
        )
      }
    })
    ## Gráfico - Taxa do Crime de Roubo por 100.000 habitantes----
    t63 <- reactive({
      paste0("Taxa do Crime de Roubo por 100.000 habitantes, Pará - ", min(seg6$ano), " a ", max(seg6$ano))
    })
    # 7-Taxa do Crime de Latrocínio por 100.000 habitantes----
    ## Mapa - Taxa do Crime de Latrocínio por 100.000 habitantes----
    t71 <- reactive({
      if (input$seg7ri == "Pará") {
        paste0(
          "Taxa do Crime de Latrocínio por 100.000 habitantes, Pará - ",
          input$seg7ano
        )
      } else{
        paste0(
          "Taxa do Crime de Latrocínio por 100.000 habitantes, Região de Integração ",
          input$seg7ri,
          " - ",
          input$seg7ano
        )
      }
    })
    ## Tabela - Taxa do Crime de Latrocínio por 100.000 habitantes----
    t72 <- reactive({
      if (input$seg7ri == "Pará") {
        paste0(
          "Taxa do Crime de Latrocínio por 100.000 habitantes por Município, Pará - ",
          input$seg7ano
        )
      } else{
        paste0(
          "Taxa do Crime de Latrocínio por 100.000 habitantes por Município, Região de Integração ",
          input$seg7ri,
          " - ",
          input$seg7ano
        )
      }
    })
    ## Gráfico - Taxa do Crime de Latrocínio por 100.000 habitantes----
    t73 <- reactive({
      paste0("Taxa do Crime de Latrocínio por 100.000 habitantes, Pará - ", min(seg7$ano), " a ", max(seg7$ano))
    })
    # 8-Taxa do Crime de Lesão Corporal Seguida de Morte por 100.000 habitantes----
    ## Mapa - Taxa do Crime de Lesão Corporal Seguida de Morte por 100.000 habitantes----
    t81 <- reactive({
      if (input$seg8ri == "Pará") {
        paste0(
          "Taxa do Crime de Lesão Corporal Seguida de Morte por 100.000 habitantes, Pará - ",
          input$seg8ano
        )
      } else{
        paste0(
          "Taxa do Crime de Lesão Corporal Seguida de Morte por 100.000 habitantes, Região de Integração ",
          input$seg8ri,
          " - ",
          input$seg8ano
        )
      }
    })
    ## Tabela - Taxa do Crime de Lesão Corporal Seguida de Morte por 100.000 habitantes----
    t82 <- reactive({
      if (input$seg8ri == "Pará") {
        paste0(
          "Taxa do Crime de Lesão Corporal Seguida de Morte por 100.000 habitantes por Município, Pará - ",
          input$seg8ano
        )
      } else{
        paste0(
          "Taxa do Crime de Lesão Corporal Seguida de Morte por 100.000 habitantes por Município, Região de Integração ",
          input$seg8ri,
          " - ",
          input$seg8ano
        )
      }
    })
    ## Gráfico - Taxa do Crime de Lesão Corporal Seguida de Morte por 100.000 habitantes----
    t83 <- reactive({
      paste0("Taxa do Crime de Lesão Corporal Seguida de Morte por 100.000 habitantes, Pará - ", min(seg8$ano), " a ", max(seg8$ano))
    })
    #VISUALIZAÇÃO----
    # 1-Taxa de Homicídios Total por 100.000 habitantes----
    ## Mapa - Taxa de Homicídios Total por 100.000 habitantes----
    output$seg1txt1 <- renderText({
      t11()
    })

    output$seg1map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$seg1ri == "Pará") {
        df <- seg1 %>%
          filter(localidade != "Pará", ano == input$seg1ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- seg1 %>%
          filter(localidade != "Pará", ano == input$seg1ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$seg1ri)
      }

      z <- x$valor
      
      # z <- x$valor[x$valor > 0]
      
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      
      # x$valor[x$valor == 0] <- NA
      
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }

      pal <- colorBin(c("#FFEBCD", "#ffcd97", "#ffaf66", "#fa9236", "#f17701"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Taxa:</b> %s",
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
          title = "Taxa",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 2)
        )
    })

    ## Tabela - Taxa de Homicídios Total por 100.000 habitantes----
    output$seg1txt2 <- renderText({
      t12()
    })

    output$seg1tab <- renderReactable({
      if (input$seg1ri == "Pará") {
        x <- seg1 %>%
          filter(localidade != "Pará", ano == input$seg1ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- seg1 %>%
          filter(localidade != "Pará", ano == input$seg1ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$seg1ri)
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
            name = "Taxa",
            na = "-",
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

    ## Gráfico - Taxa de Homicídios Total por 100.000 habitantes----
    output$seg1txt3 <- renderText({
      t13()
    })
    output$seg1graf <- renderEcharts4r({
      seg1 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#f17701",
          name = "Taxa",
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
          name = "Taxa",
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
    })

    # 2-Taxa de Homicídios de Jovens por 100.000 habitantes----
    ## Mapa - Taxa de Homicídios de Jovens por 100.000 habitantes----
    output$seg2txt1 <- renderText({
      t21()
    })

    output$seg2map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$seg2ri == "Pará") {
        df <- seg2 %>%
          filter(localidade != "Pará", ano == input$seg2ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- seg2 %>%
          filter(localidade != "Pará", ano == input$seg2ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$seg2ri)
      }

      z <- x$valor
      
      # z <- x$valor[x$valor > 0]
      
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      
      # x$valor[x$valor == 0] <- NA
      
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }

      pal <- colorBin(c("#FFEBCD", "#ffcd97", "#ffaf66", "#fa9236", "#f17701"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Taxa:</b> %s",
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
          title = "Taxa",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 2)
        )
    })

    ## Tabela - Taxa de Homicídios de Jovens por 100.000 habitantes----
    output$seg2txt2 <- renderText({
      t22()
    })

    output$seg2tab <- renderReactable({
      if (input$seg2ri == "Pará") {
        x <- seg2 %>%
          filter(localidade != "Pará", ano == input$seg2ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- seg2 %>%
          filter(localidade != "Pará", ano == input$seg2ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$seg2ri)
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
            name = "Taxa",
            na = "-",
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

    ## Gráfico - Taxa de Homicídios de Jovens por 100.000 habitantes----
    output$seg2txt3 <- renderText({
      t23()
    })
    output$seg2graf <- renderEcharts4r({
      seg2 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#f17701",
          name = "Taxa",
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
          name = "Taxa",
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
    })

    # 3-Taxa de Mortes no Trânsito por 100.000 habitantes----
    ## Mapa - Taxa de Mortes no Trânsito por 100.000 habitantes----
    output$seg3txt1 <- renderText({
      t31()
    })

    output$seg3map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$seg3ri == "Pará") {
        df <- seg3 %>%
          filter(localidade != "Pará", ano == input$seg3ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- seg3 %>%
          filter(localidade != "Pará", ano == input$seg3ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$seg3ri)
      }

      z <- x$valor
      
      # z <- x$valor[x$valor > 0]
      
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      
      # x$valor[x$valor == 0] <- NA
      
      bins <- c(bk)
     
       if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }

      pal <- colorBin(c("#FFEBCD", "#ffcd97", "#ffaf66", "#fa9236", "#f17701"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Taxa:</b> %s",
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
          title = "Taxa",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 2)
        )
    })

    ## Tabela - Taxa de Mortes no Trânsito por 100.000 habitantes----
    output$seg3txt2 <- renderText({
      t32()
    })

    output$seg3tab <- renderReactable({
      if (input$seg3ri == "Pará") {
        x <- seg3 %>%
          filter(localidade != "Pará", ano == input$seg3ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- seg3 %>%
          filter(localidade != "Pará", ano == input$seg3ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$seg3ri)
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
            name = "Taxa",
            na = "-",
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

    ## Gráfico - Taxa de Mortes no Trânsito por 100.000 habitantes----
    output$seg3txt3 <- renderText({
      t33()
    })
    output$seg3graf <- renderEcharts4r({
      seg3 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#f17701",
          name = "Taxa",
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
          name = "Taxa",
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
    })

    # 4-Taxa do Crime de Homicídio por 100.000 habitantes----
    ## Mapa - Taxa do Crime de Homicídio por 100.000 habitantes----
    output$seg4txt1 <- renderText({
      t41()
    })

    output$seg4map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$seg4ri == "Pará") {
        df <- seg4 %>%
          filter(localidade != "Pará", ano == input$seg4ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- seg4 %>%
          filter(localidade != "Pará", ano == input$seg4ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$seg4ri)
      }

      z <- x$valor
      
      # z <- x$valor[x$valor > 0]
      
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      
      # x$valor[x$valor == 0] <- NA
      
      bins <- c(bk)
      
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }

      pal <- colorBin(c("#FFEBCD", "#ffcd97", "#ffaf66", "#fa9236", "#f17701"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Taxa:</b> %s",
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
          title = "Taxa",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 2)
        )
    })

    ## Tabela - Taxa do Crime de Homicídio por 100.000 habitantes----
    output$seg4txt2 <- renderText({
      t42()
    })

    output$seg4tab <- renderReactable({
      if (input$seg4ri == "Pará") {
        x <- seg4 %>%
          filter(localidade != "Pará", ano == input$seg4ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- seg4 %>%
          filter(localidade != "Pará", ano == input$seg4ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$seg4ri)
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
            name = "Taxa",
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

    ## Gráfico - Taxa do Crime de Homicídio por 100.000 habitantes----
    output$seg4txt3 <- renderText({
      t43()
    })
    output$seg4graf <- renderEcharts4r({
      seg4 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#f17701",
          name = "Taxa",
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
          name = "Taxa",
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
    })

    # 5-Taxa do Crime de Homicídio no Trânsito por 100.000 habitantes----
    output$seg5map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$seg5ri == "Pará") {
        df <- seg5 %>%
          filter(localidade != "Pará", ano == input$seg5ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x$valor[x$valor == 0] <- NA
      } else {
        df <- seg5 %>%
          filter(localidade != "Pará", ano == input$seg5ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$seg5ri)
        x$valor[x$valor == 0] <- NA
      }
      ## Variável para teste Lógico
      z <- x$valor[x$valor > 0]

      bk <- getJenksBreaks(x$valor, 6, subset = NULL)
      bk <- unique(bk) # Para evitar o erro que quartiles iguais
      # ____________________________________________
      if (length(z) <= 9) {
        a <- max(x$valor, na.rm = T) + 1
        b <- min(x$valor, na.rm = T)
        c <- round((a - b) / 5, digits = 4)
        bk <- seq(b, a, c)
      }
      # ____________________________________________
      bins <- c(bk)
      pal <- colorBin(c("#FFEBCD", "#ffcd97", "#ffaf66", "#fa9236", "#f17701"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Taxa:</b> %s",
          x$name_muni,
          format(x$valor, big.mark = ".", decimal.mark = ",", digits = 3)
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
          title = "Taxa",
          position = "bottomright",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 2)
        )
    })

    ## Mapa - Taxa do Crime de Homicídio no Trânsito por 100.000 habitantes----
    output$seg5txt1 <- renderText({
      t51()
    })

    output$seg5map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$seg5ri == "Pará") {
        df <- seg5 %>%
          filter(localidade != "Pará", ano == input$seg5ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- seg5 %>%
          filter(localidade != "Pará", ano == input$seg5ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$seg5ri)
      }

      z <- x$valor
      
      # z <- x$valor[x$valor > 0]
      
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      
      # x$valor[x$valor == 0] <- NA
      
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }

      pal <- colorBin(c("#FFEBCD", "#ffcd97", "#ffaf66", "#fa9236", "#f17701"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Taxa:</b> %s",
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
          title = "Taxa",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 2)
        )
    })

    ## Tabela - Taxa do Crime de Homicídio no Trânsito por 100.000 habitantes----
    output$seg5txt2 <- renderText({
      t52()
    })

    output$seg5tab <- renderReactable({
      if (input$seg5ri == "Pará") {
        x <- seg5 %>%
          filter(localidade != "Pará", ano == input$seg5ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- seg5 %>%
          filter(localidade != "Pará", ano == input$seg5ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$seg5ri)
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
            name = "Taxa",
            na = "-",
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

    ## Gráfico - Taxa do Crime de Homicídio no Trânsito por 100.000 habitantes----
    output$seg5txt3 <- renderText({
      t53()
    })
    output$seg5graf <- renderEcharts4r({
      seg5 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#f17701",
          name = "Taxa",
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
          name = "Taxa",
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
    })

    # 6-Taxa do Crime de Roubo por 100.000 habitantes----
    ## Mapa - Taxa do Crime de Roubo por 100.000 habitantes----
    output$seg6txt1 <- renderText({
      t61()
    })

    output$seg6map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$seg6ri == "Pará") {
        df <- seg6 %>%
          filter(localidade != "Pará", ano == input$seg6ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- seg6 %>%
          filter(localidade != "Pará", ano == input$seg6ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$seg6ri)
      }

      z <- x$valor
      
      # z <- x$valor[x$valor > 0]
      
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      
      # x$valor[x$valor == 0] <- NA
      
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }

      pal <- colorBin(c("#FFEBCD", "#ffcd97", "#ffaf66", "#fa9236", "#f17701"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Taxa:</b> %s",
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
          title = "Taxa",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 2)
        )
    })

    ## Tabela - Taxa do Crime de Roubo por 100.000 habitantes----
    output$seg6txt2 <- renderText({
      t62()
    })

    output$seg6tab <- renderReactable({
      if (input$seg6ri == "Pará") {
        x <- seg6 %>%
          filter(localidade != "Pará", ano == input$seg6ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- seg6 %>%
          filter(localidade != "Pará", ano == input$seg6ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$seg6ri)
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
            name = "Taxa",
            na = "-",
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

    ## Gráfico - Taxa do Crime de Roubo por 100.000 habitantes----
    output$seg6txt3 <- renderText({
      t63()
    })
    output$seg6graf <- renderEcharts4r({
      seg6 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#f17701",
          name = "Taxa",
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
          name = "Taxa",
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
    })

    # 7-Taxa do Crime de Latrocínio por 100.000 habitantes----
    ## Mapa - Taxa do Crime de Latrocínio por 100.000 habitantes----
    output$seg7txt1 <- renderText({
      t71()
    })

    output$seg7map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação

      if (input$seg7ri == "Pará") {
        df <- seg7 %>%
          filter(localidade != "Pará", ano == input$seg7ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- seg7 %>%
          filter(localidade != "Pará", ano == input$seg7ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$seg7ri)
      }

      z <- x$valor
      
      # z <- x$valor[x$valor > 0]
      
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      
      # x$valor[x$valor == 0] <- NA
      
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }

      pal <- colorBin(c("#FFEBCD", "#ffcd97", "#ffaf66", "#fa9236", "#f17701"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Taxa:</b> %s",
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
          title = "Taxa",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 2)
        )
    })

    ## Tabela - Taxa do Crime de Latrocínio por 100.000 habitantes----
    output$seg7txt2 <- renderText({
      t72()
    })

    output$seg7tab <- renderReactable({
      if (input$seg7ri == "Pará") {
        x <- seg7 %>%
          filter(localidade != "Pará", ano == input$seg7ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- seg7 %>%
          filter(localidade != "Pará", ano == input$seg7ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$seg7ri)
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
            name = "Taxa",
            na = "-",
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

    ## Gráfico - Taxa do Crime de Latrocínio por 100.000 habitantes----
    output$seg7txt3 <- renderText({
      t73()
    })
    output$seg7graf <- renderEcharts4r({
      seg7 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#f17701",
          name = "Taxa",
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
          name = "Taxa",
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
    })

    # 8-Taxa do Crime de Lesão Corporal Seguida de Morte por 100.000 habitantes----
    ## Mapa - Taxa do Crime de Lesão Corporal Seguida de Morte por 100.000 habitantes----
    output$seg8txt1 <- renderText({
      t81()
    })

    output$seg8map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$seg8ri == "Pará") {
        df <- seg8 %>%
          filter(localidade != "Pará", ano == input$seg8ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- seg8 %>%
          filter(localidade != "Pará", ano == input$seg8ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$seg8ri)
      }

      z <- x$valor
      
      # z <- x$valor[x$valor > 0]
      
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      
      # x$valor[x$valor == 0] <- NA
      
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }

      pal <- colorBin(c("#FFEBCD", "#ffcd97", "#ffaf66", "#fa9236", "#f17701"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Taxa:</b> %s",
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
          title = "Taxa",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 2)
        )
    })

    ## Tabela - Taxa do Crime de Lesão Corporal Seguida de Morte por 100.000 habitantes----
    output$seg8txt2 <- renderText({
      t82()
    })

    output$seg8tab <- renderReactable({
      if (input$seg8ri == "Pará") {
        x <- seg8 %>%
          filter(localidade != "Pará", ano == input$seg8ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- seg8 %>%
          filter(localidade != "Pará", ano == input$seg8ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$seg8ri)
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
            name = "Taxa",
            na = "-",
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

    ## Gráfico - Taxa do Crime de Lesão Corporal Seguida de Morte por 100.000 habitantes----
    output$seg8txt3 <- renderText({
      t83()
    })
    output$seg8graf <- renderEcharts4r({
      seg8 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#f17701",
          name = "Taxa",
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
          name = "Taxa",
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
    })
    # DOWNLOADS----
    # 1-Taxa de Homicídios Total por 100.000 habitantes----
    ## Tabela - Taxa de Homicídios Total por 100.000 habitantes----
    # Filtra os dados
    seg1_1 <- reactive({
       if (input$seg1ri == "Pará") {
        x <- seg1 %>%
          filter(localidade != "Pará", ano == input$seg1ano)
      } else {
        x <- seg1 %>%
          filter(localidade != "Pará", 
                 ano == input$seg1ano,
                 ri == input$seg1ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(seg1_1(), {
      downset_Server("seg1_1", seg1_1(), t12())
    })
    ## Gráfico - Taxa de Homicídios Total por 100.000 habitantes----
    # Filtra os dados
    seg1_2 <- reactive({
      seg1 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(seg1_2(), {
      downset_Server("seg1_2", seg1_2(), t13())
    })

    # 2-Taxa de Homicídios de Jovens por 100.000 habitantes----
    ## Tabela - Taxa de Homicídios de Jovens por 100.000 habitantes----
    # Filtra os dados
    seg2_1 <- reactive({
      if (input$seg2ri == "Pará") {
        x <- seg2 %>%
          filter(localidade != "Pará", ano == input$seg2ano)
      } else {
        x <- seg2 %>%
          filter(localidade != "Pará",
                 ano == input$seg2ano,
                 ri == input$seg2ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(seg2_1(), {
      downset_Server("seg2_1", seg2_1(), t22())
    })
    ## Gráfico - Taxa de Homicídios de Jovens por 100.000 habitantes----
    # Filtra os dados
    seg2_2 <- reactive({
      seg2 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(seg2_2(), {
      downset_Server("seg2_2", seg2_2(), t23())
    })

    # 3-Taxa de Mortes no Trânsito por 100.000 habitantes----
    ## Tabela - Taxa de Mortes no Trânsito por 100.000 habitantes----
    # Filtra os dados
    seg3_1 <- reactive({if (input$seg3ri == "Pará") {
        x <- seg3 %>%
          filter(localidade != "Pará", ano == input$seg3ano)
      } else {
        x <- seg3 %>%
          filter(localidade != "Pará",
                 ano == input$seg3ano,
                 ri == input$seg3ri)
      }})
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(seg3_1(), {
      downset_Server("seg3_1", seg3_1(), t32())
    })
    ## Gráfico - Taxa de Mortes no Trânsito por 100.000 habitantes----
    # Filtra os dados
    seg3_2 <- reactive({
      seg3 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(seg3_2(), {
      downset_Server("seg3_2", seg3_2(), t33())
    })

    # 4-Taxa do Crime de Homicídio por 100.000 habitantes----
    ## Tabela - Taxa do Crime de Homicídio por 100.000 habitantes----
    # Filtra os dados
    seg4_1 <- reactive({
      if (input$seg4ri == "Pará") {
        x <- seg4 %>%
          filter(localidade != "Pará", ano == input$seg4ano)
      } else {
        x <- seg4 %>%
          filter(localidade != "Pará",
                 ano == input$seg4ano,
                 ri == input$seg4ri) 
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(seg4_1(), {
      downset_Server("seg4_1", seg4_1(), t42())
    })
    ## Gráfico - Taxa do Crime de Homicídio por 100.000 habitantes----
    # Filtra os dados
    seg4_2 <- reactive({
      seg4 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(seg4_2(), {
      downset_Server("seg4_2", seg4_2(), t43())
    })

    # 5-Taxa do Crime de Homicídio no Trânsito por 100.000 habitantes----
    ## Tabela - Taxa do Crime de Homicídio no Trânsito por 100.000 habitantes----
    # Filtra os dados
    seg5_1 <- reactive({
      if (input$seg5ri == "Pará") {
        x <- seg5 %>%
          filter(localidade != "Pará", ano == input$seg5ano)
      } else {
        x <- seg5 %>%
          filter(localidade != "Pará",
                 ano == input$seg5ano,
                 ri == input$seg5ri) 
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(seg5_1(), {
      downset_Server("seg5_1", seg5_1(), t52())
    })
    ## Gráfico - Taxa do Crime de Homicídio no Trânsito por 100.000 habitantes----
    # Filtra os dados
    seg5_2 <- reactive({
      seg5 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(seg5_2(), {
      downset_Server("seg5_2", seg5_2(), t53())
    })

    # 6-Taxa do Crime de Roubo por 100.000 habitantes----
    ## Tabela - Taxa do Crime de Roubo por 100.000 habitantes----
    # Filtra os dados
    seg6_1 <- reactive({
      if (input$seg6ri == "Pará") {
        x <- seg6 %>%
          filter(localidade != "Pará", ano == input$seg6ano) 
      } else {
        x <- seg6 %>%
          filter(localidade != "Pará",
                 ano == input$seg6ano,
                 ri == input$seg6ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(seg6_1(), {
      downset_Server("seg6_1", seg6_1(), t62())
    })
    ## Gráfico - Taxa do Crime de Roubo por 100.000 habitantes----
    # Filtra os dados
    seg6_2 <- reactive({
      seg6 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(seg6_2(), {
      downset_Server("seg6_2", seg6_2(), t63())
    })

    # 7-Taxa do Crime de Latrocínio por 100.000 habitantes----
    ## Tabela - Taxa do Crime de Latrocínio por 100.000 habitantes----
    # Filtra os dados
    seg7_1 <- reactive({
      if (input$seg7ri == "Pará") {
        x <- seg7 %>%
          filter(localidade != "Pará", ano == input$seg7ano) 
      } else {
        x <- seg7 %>%
          filter(localidade != "Pará",
                 ano == input$seg7ano,
                 ri == input$seg7ri) 
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(seg7_1(), {
      downset_Server("seg7_1", seg7_1(), t72())
    })
    ## Gráfico - Taxa do Crime de Latrocínio por 100.000 habitantes----
    # Filtra os dados
    seg7_2 <- reactive({
      seg7 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(seg7_2(), {
      downset_Server("seg7_2", seg7_2(), t73())
    })

    # 8-Taxa do Crime de Lesão Corporal Seguida de Morte por 100.000 habitantes----
    ## Tabela - Taxa do Crime de Lesão Corporal Seguida de Morte por 100.000 habitantes----
    # Filtra os dados
    seg8_1 <- reactive({
      if (input$seg8ri == "Pará") {
        x <- seg8 %>%
          filter(localidade != "Pará", ano == input$seg8ano) 
      } else {
        x <- seg8 %>%
          filter(localidade != "Pará",
                 ano == input$seg8ano,
                 ri == input$seg8ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(seg8_1(), {
      downset_Server("seg8_1", seg8_1(), t82())
    })
    ## Gráfico - Taxa do Crime de Lesão Corporal Seguida de Morte por 100.000 habitantes----
    # Filtra os dados
    seg8_2 <- reactive({
      seg8 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(seg8_2(), {
      downset_Server("seg8_2", seg8_2(), t83())
    })
  })
}

# Play do Módulo
# ui <- dashboardPage(
#   header = dashboardHeader(),
#   sidebar = dashboardSidebar(),
#   body = dashboardBody(fluidPage(
#     social_seguranca_pa_ui("social_seguranca_pa")
#   ))
# )
# 
# 
# server <- function(input, output) {
#   social_seguranca_pa_Server("social_seguranca_pa")
# }
# 
# shinyApp(ui, server)
