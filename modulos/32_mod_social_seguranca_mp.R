# Funções de módulo de Social - Segurança - Municipal
# Função de UI
social_seguranca_mp_ui <- function(id) {
  fluidPage(
    # CAMINHO DO ARQUIVO CSS----
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "estilo.css")
    ),
    div(
      class = "navbar_social",
      navbarPage(
        tags$b("Segurança - Municípios"),
        navbarMenu(
          tags$b("Escolha um Indicador"),
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
                  inputId = NS(id, "seg1muni"),
                  label = "Município",
                  choices = seg1 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Taxa de Homicídios Total por 100.000 habitantes----
              box(
                title = textOutput(NS(id, "seg1txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "seg1municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
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
                  downset_ui(NS(id, "seg1_1"))
                )
              ),

              ## Tabela - Taxa de Homicídios Total por 100.000 habitantes----
              box(
                title = textOutput(NS(id, "seg1txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "seg1ano"),
                  label = "Ano",
                  choices = sort(unique(seg1[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "seg1tab2"), height = "400px"),
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
                  inputId = NS(id, "seg2muni"),
                  label = "Município",
                  choices = seg2 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Taxa de Homicídios de Jovens por 100.000 habitantes----
              box(
                title = textOutput(NS(id, "seg2txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "seg2municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "seg2graf")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(11,
                  tags$h6(tags$b("Fonte:"), "FFF"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                  downset_ui(NS(id, "seg2_1"))
                )
              ),
              ## Tabela - Taxa de Homicídios de Jovens por 100.000 habitantes----
              box(
                title = textOutput(NS(id, "seg2txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "seg2ano"),
                  label = "Ano",
                  choices = sort(unique(seg2[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "seg2tab2"), height = "400px"),
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
                  inputId = NS(id, "seg3muni"),
                  label = "Município",
                  choices = seg3 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Taxa de Mortes no Trânsito por 100.000 habitantes----
              box(
                title = textOutput(NS(id, "seg3txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "seg3municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
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
                  downset_ui(NS(id, "seg3_1"))
                )
              ),
              ## Tabela - Taxa de Mortes no Trânsito por 100.000 habitantes----
              box(
                title = textOutput(NS(id, "seg3txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "seg3ano"),
                  label = "Ano",
                  choices = sort(unique(seg3[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "seg3tab2"), height = "400px"),
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
                  inputId = NS(id, "seg4muni"),
                  label = "Município",
                  choices = seg4 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Taxa do Crime de Homicídio por 100.000 habitantes----
              box(
                title = textOutput(NS(id, "seg4txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "seg4municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "seg4graf")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(11,
                  tags$h6(tags$b("Fonte:"), "SEGUP-SIAC"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                  downset_ui(NS(id, "seg4_1"))
                )
              ),

              ## Tabela - Taxa do Crime de Homicídio por 100.000 habitantes----
              box(
                title = textOutput(NS(id, "seg4txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                pickerInput(
                  inputId = NS(id, "seg4ano"),
                  label = "Ano",
                  choices = sort(unique(seg4[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "seg4tab2"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(11,
                  tags$h6(tags$b("Fonte:"), "SEGUP-SIAC"),
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
                  inputId = NS(id, "seg5muni"),
                  label = "Município",
                  choices = seg5 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Taxa do Crime de Homicídio no Trânsito por 100.000 habitantes----
              box(
                title = textOutput(NS(id, "seg5txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "seg5municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "seg5graf")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(11,
                  tags$h6(tags$b("Fonte:"), "FFF"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                  downset_ui(NS(id, "seg5_1"))
                )
              ),
              ## Tabela - Taxa do Crime de Homicídio no Trânsito por 100.000 habitantes----
              box(
                title = textOutput(NS(id, "seg5txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "seg5ano"),
                  label = "Ano",
                  choices = sort(unique(seg5[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "seg5tab2"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(11,
                  tags$h6(tags$b("Fonte:"), "SEGUP-SIAC"),
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
                  inputId = NS(id, "seg6muni"),
                  label = "Município",
                  choices = seg6 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Taxa do Crime de Roubo por 100.000 habitantes----
              box(
                title = textOutput(NS(id, "seg6txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "seg6municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "seg6graf")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(11,
                  tags$h6(tags$b("Fonte:"), "SEGUP-SIAC"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                  downset_ui(NS(id, "seg6_1"))
                )
              ),
              ## Tabela - Taxa do Crime de Roubo por 100.000 habitantes----
              box(
                title = textOutput(NS(id, "seg6txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "seg6ano"),
                  label = "Ano",
                  choices = sort(unique(seg6[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "seg6tab2"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(11,
                  tags$h6(tags$b("Fonte:"), "SEGUP-SIAC"),
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
                  inputId = NS(id, "seg7muni"),
                  label = "Município",
                  choices = seg7 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Taxa do Crime de Latrocínio por 100.000 habitantes----
              box(
                title = textOutput(NS(id, "seg7txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "seg7municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "seg7graf")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(11,
                  tags$h6(tags$b("Fonte:"), "SEGUP-SIAC"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                  downset_ui(NS(id, "seg7_1"))
                )
              ),
              ## Tabela - Taxa do Crime de Latrocínio por 100.000 habitantes----
              box(
                title = textOutput(NS(id, "seg7txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "seg7ano"),
                  label = "Ano",
                  choices = sort(unique(seg7[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "seg7tab2"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(11,
                  tags$h6(tags$b("Fonte:"), "SEGUP-SIAC"),
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
                  inputId = NS(id, "seg8muni"),
                  label = "Município",
                  choices = seg8 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Taxa do Crime de Lesão Corporal Seguida de Morte por 100.000 habitantes----
              box(
                title = textOutput(NS(id, "seg8txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "seg8municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "seg8graf")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(11,
                  tags$h6(tags$b("Fonte:"), "SEGUP-SIAC"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                  downset_ui(NS(id, "seg8_1"))
                )
              ),
              ## Tabela - Taxa do Crime de Lesão Corporal Seguida de Morte por 100.000 habitantes----
              box(
                title = textOutput(NS(id, "seg8txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "seg8ano"),
                  label = "Ano",
                  choices = sort(unique(seg8[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "seg8tab2"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(11,
                  tags$h6(tags$b("Fonte:"), "SEGUP-SIAC"),
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
social_seguranca_mp_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    #TÍTULOS----
    # 1-Taxa de Homicídios Total por 100.000 habitantes----
    ## Gráfico - Taxa do Crime de Lesão Corporal Seguida de Morte por 100.000 habitantes----
    t11 <- reactive({
      req(input$seg1municomp)
      if (input$seg1municomp == "Selecione um município") {
        paste0("Taxa de Homicídios Total por 100.000 habitantes, ", input$seg1muni, " - ", min(seg1$ano), " a ", max(seg1$ano))
      } else {
        paste0("Taxa de Homicídios Total por 100.000 habitantes, ", input$seg1muni, " x ", input$seg1municomp, " - ", min(seg1$ano), " a ", max(seg1$ano))
      }
    })
    ## Tabela - Taxa do Crime de Lesão Corporal Seguida de Morte por 100.000 habitantes----
    t12 <- reactive({
      ri <- seg1 %>%
        filter(ano == input$seg1ano, localidade == input$seg1muni) %>%
        pull(ri)
      paste0("Taxa do Crime de Lesão Corporal Seguida de Morte por 100.000 habitantes por Municípios, Região de Integração ", unique(ri), " - ", input$seg1ano)
    })
    # 2-Taxa de Homicídios de Jovens por 100.000 habitantes----
    ## Gráfico - Taxa de Homicídios de Jovens por 100.000 habitantes----
    t21 <- reactive({
      req(input$seg2municomp)
      if (input$seg2municomp == "Selecione um município") {
        paste0("Taxa de Homicídios de Jovens por 100.000 habitantes, ", input$seg2muni, " - ", min(seg2$ano), " a ", max(seg2$ano))
      } else {
        paste0("Taxa de Homicídios de Jovens por 100.000 habitantes, ", input$seg2muni, " x ", input$seg2municomp, " - ", min(seg2$ano), " a ", max(seg2$ano))
      }
    })
    ## Tabela - Taxa de Homicídios de Jovens por 100.000 habitantes----
    t22 <- reactive({
      ri <- seg2 %>%
        filter(ano == input$seg2ano, localidade == input$seg2muni) %>%
        pull(ri)
      paste0("Taxa de Homicídios de Jovens por 100.000 habitantes por Municípios, Região de Integração ", unique(ri), " - ", input$seg2ano)
    })
    # 3-Taxa de Mortes no Trânsito por 100.000 habitantes----
    ## Gráfico - Taxa de Mortes no Trânsito por 100.000 habitantes----
    t31 <- reactive({
      req(input$seg3municomp)
      if (input$seg3municomp == "Selecione um município") {
        paste0("Taxa de Mortes no Trânsito por 100.000 habitantes, ", input$seg3muni, " - ", min(seg3$ano), " a ", max(seg3$ano))
      } else {
        paste0("Taxa de Mortes no Trânsito por 100.000 habitantes, ", input$seg3muni, " x ", input$seg3municomp, " - ", min(seg3$ano), " a ", max(seg3$ano))
      }
    })
    ## Tabela - Taxa de Mortes no Trânsito por 100.000 habitantes----
    t32 <- reactive({
      ri <- seg3 %>%
        filter(ano == input$seg3ano, localidade == input$seg3muni) %>%
        pull(ri)
      paste0("Taxa de Mortes no Trânsito por 100.000 habitantes por Municípios, Região de Integração ", unique(ri), " - ", input$seg3ano)
    })
    # 4-Taxa do Crime de Homicídio por 100.000 habitantes----
    ## Gráfico - Taxa do Crime de Homicídio por 100.000 habitantes----
    # Atualização da entrada
    t41 <- reactive({
      req(input$seg4municomp)
      if (input$seg4municomp == "Selecione um município") {
        paste0("Taxa do Crime de Homicídio por 100.000 habitantes, ", input$seg4muni, " - ", min(seg4$ano), " a ", max(seg4$ano))
      } else {
        paste0("Taxa do Crime de Homicídio por 100.000 habitantes, ", input$seg4muni, " x ", input$seg4municomp, " - ", min(seg4$ano), " a ", max(seg4$ano))
      }
    })
    ## Tabela - Taxa do Crime de Homicídio por 100.000 habitantes----
    t42 <- reactive({
      ri <- seg4 %>%
        filter(ano == input$seg4ano, localidade == input$seg4muni) %>%
        pull(ri)
      paste0("Taxa do Crime de Homicídio por 100.000 habitantes por Municípios, Região de Integração ", unique(ri), " - ", input$seg4ano)
    })
    # 5-Taxa do Crime de Homicídio no Trânsito por 100.000 habitantes----
    ## Gráfico - Taxa do Crime de Homicídio no Trânsito por 100.000 habitantes----
    t51 <- reactive({
      req(input$seg5municomp)
      if (input$seg5municomp == "Selecione um município") {
        paste0("Taxa do Crime de Homicídio no Trânsito por 100.000 habitantes, ", input$seg5muni, " - ", min(seg5$ano), " a ", max(seg5$ano))
      } else {
        paste0("Taxa do Crime de Homicídio no Trânsito por 100.000 habitantes, ", input$seg5muni, " x ", input$seg5municomp, " - ", min(seg5$ano), " a ", max(seg5$ano))
      }
    })
    ## Tabela - Taxa do Crime de Homicídio no Trânsito por 100.000 habitantes----
    t52 <- reactive({
      ri <- seg5 %>%
        filter(ano == input$seg5ano, localidade == input$seg5muni) %>%
        pull(ri)
      paste0("Taxa do Crime de Homicídio no Trânsito por 100.000 habitantes por Municípios, Região de Integração ", unique(ri), " - ", input$seg5ano)
    })
    # 6-Taxa do Crime de Roubo por 100.000 habitantes----
    ## Gráfico - Taxa do Crime de Roubo por 100.000 habitantes----
    t61 <- reactive({
      req(input$seg6municomp)
      if (input$seg6municomp == "Selecione um município") {
        paste0("Taxa do Crime de Roubo por 100.000 habitantes, ", input$seg6muni, " - ", min(seg6$ano), " a ", max(seg6$ano))
      } else {
        paste0("Taxa do Crime de Roubo por 100.000 habitantes, ", input$seg6muni, " x ", input$seg6municomp, " - ", min(seg6$ano), " a ", max(seg6$ano))
      }
    })
    ## Tabela - Taxa do Crime de Roubo por 100.000 habitantes----
    t62 <- reactive({
      ri <- seg6 %>%
        filter(ano == input$seg6ano, localidade == input$seg6muni) %>%
        pull(ri)
      paste0("Taxa do Crime de Roubo por 100.000 habitantes por Municípios, Região de Integração ", unique(ri), " - ", input$seg6ano)
    })
    # 7-Taxa do Crime de Latrocínio por 100.000 habitantes----
    ## Gráfico - Taxa do Crime de Latrocínio por 100.000 habitantes----
    t71 <- reactive({
      req(input$seg7municomp)
      if (input$seg7municomp == "Selecione um município") {
        paste0("Taxa do Crime de Latrocínio por 100.000 habitantes, ", input$seg7muni, " - ", min(seg7$ano), " a ", max(seg7$ano))
      } else {
        paste0("Taxa do Crime de Latrocínio por 100.000 habitantes, ", input$seg7muni, " x ", input$seg7municomp, " - ", min(seg7$ano), " a ", max(seg7$ano))
      }
    })
    ## Tabela - Taxa do Crime de Latrocínio por 100.000 habitantes----
    t72 <- reactive({
      ri <- seg7 %>%
        filter(ano == input$seg7ano, localidade == input$seg7muni) %>%
        pull(ri)
      paste0("Taxa do Crime de Latrocínio por 100.000 habitantes por Municípios, Região de Integração ", unique(ri), " - ", input$seg7ano)
    })
    # 8-Taxa do Crime de Lesão Corporal Seguida de Morte por 100.000 habitantes----
    ## Gráfico - Taxa do Crime de Lesão Corporal Seguida de Morte por 100.000 habitantes----
    t81 <- reactive({
      req(input$seg8municomp)
      if (input$seg8municomp == "Selecione um município") {
        paste0("Taxa do Crime de Lesão Corporal Seguida de Morte por 100.000 habitantes, ", input$seg8muni, " - ", min(seg8$ano), " a ", max(seg8$ano))
      } else {
        paste0("Taxa do Crime de Lesão Corporal Seguida de Morte por 100.000 habitantes, ", input$seg8muni, " x ", input$seg8municomp, " - ", min(seg8$ano), " a ", max(seg8$ano))
      }
    })
    ## Tabela - Taxa do Crime de Lesão Corporal Seguida de Morte por 100.000 habitantes----
    t82 <- reactive({
      ri <- seg8 %>%
        filter(ano == input$seg8ano, localidade == input$seg8muni) %>%
        pull(ri)
      paste0("Taxa do Crime de Lesão Corporal Seguida de Morte por 100.000 habitantes por Municípios, Região de Integração ", unique(ri), " - ", input$seg8ano)
    })
    #VISUALIZAÇÃO----
    # 1-Taxa de Homicídios Total por 100.000 habitantes----
    ## Gráfico - Taxa do Crime de Lesão Corporal Seguida de Morte por 100.000 habitantes----
    # Atualização da entrada
    seg1comp <- reactive({
      input$seg1muni
    })
    observeEvent(seg1comp(), {
      x <- seg1 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != seg1comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "seg1municomp", choices = c("Selecione um município", choices), session)
    })

    output$seg1txt1 <- renderText({
    t11()  
    })

    output$seg1graf <- renderEcharts4r({
      req(input$seg1municomp)
      if (input$seg1municomp == "Selecione um município") {
        a <- seg1 %>% filter(localidade == input$seg1muni)
        a %>%
          e_charts(x = ano) %>%
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
      } else {
        a <- seg1 %>% filter(localidade == input$seg1muni)
        b <- seg1 %>% filter(localidade == input$seg1municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$seg1muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$seg1municomp,
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
      }
    })
    ## Tabela - Taxa do Crime de Lesão Corporal Seguida de Morte por 100.000 habitantes----
    output$seg1txt2 <- renderText({
    t12()  
    })
    output$seg1tab2 <- renderReactable({
      ris <- seg1 %>%
        filter(ano == input$seg1ano, localidade == input$seg1muni) %>%
        pull(ri)
      x <- seg1 %>% filter(ano == input$seg1ano, localidade != "Pará")
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
            na = "-",
            name = "Taxa",
            format = colFormat(
              digits = 2,
              separators = T,
              locales = "pt-BR"
            ),
            # cell = data_bars(x,
            #   text_position = "above",
            #   box_shadow = TRUE,
            #   align_bars = "right",
            #   number_fmt = scales::number_format(
            #     accuracy = 0.01,
            #     decimal.mark = ",", big.mark = ".",
            #   )
            # )
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


    # 2-Taxa de Homicídios de Jovens por 100.000 habitantes----

    ## Gráfico - Taxa de Homicídios de Jovens por 100.000 habitantes----
    # Atualização da entrada
    seg2comp <- reactive({
      input$seg2muni
    })
    observeEvent(seg2comp(), {
      x <- seg2 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != seg2comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "seg2municomp", choices = c("Selecione um município", choices), session)
    })

    output$seg2txt1 <- renderText({
    t21()  
    })

    output$seg2graf <- renderEcharts4r({
      req(input$seg2municomp)
      if (input$seg2municomp == "Selecione um município") {
        a <- seg2 %>% filter(localidade == input$seg2muni)
        a %>%
          e_charts(x = ano) %>%
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
            formatter =
              e_tooltip_pointer_formatter("decimal",
                digits = 2,
                locale = "pt-Br"
              ),
            axisPointer =
              list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Ano",
            splitLine = list(show = T),
            nameTextStyle = list(fontWeight = "bold", fontSize = 14)
          ) %>%
          e_y_axis(
            name = "Taxa",
            nameTextStyle =
              list(
                fontWeight = "bold",
                fontSize = 14
              ),
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
        a <- seg2 %>% filter(localidade == input$seg2muni)
        b <- seg2 %>% filter(localidade == input$seg2municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$seg2muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$seg2municomp,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter =
              e_tooltip_pointer_formatter("decimal",
                digits = 2,
                locale = "pt-Br"
              ),
            axisPointer =
              list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Ano",
            splitLine = list(show = T),
            nameTextStyle =
              list(
                fontWeight = "bold",
                fontSize = 14
              )
          ) %>%
          e_y_axis(
            name = "Taxa",
            nameTextStyle =
              list(
                fontWeight = "bold",
                fontSize = 14
              ),
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
    ## Tabela - Taxa de Homicídios de Jovens por 100.000 habitantes----
    output$seg2txt2 <- renderText({
    t22()  
    })
    output$seg2tab2 <- renderReactable({
      ris <- seg2 %>%
        filter(ano == input$seg2ano, localidade == input$seg2muni) %>%
        pull(ri)
      x <- seg2 %>% filter(ano == input$seg2ano, localidade != "Pará")
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
            na = "-",
            name = "Taxa",
            format = colFormat(
              digits = 2,
              separators = T,
              locales = "pt-BR"
            ),
            # cell = data_bars(x,
            #   text_position = "above",
            #   box_shadow = TRUE,
            #   align_bars = "right",
            #   number_fmt = scales::number_format(
            #     accuracy = 0.01,
            #     decimal.mark = ",", big.mark = "."
            #   )
            # )
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


    # 3-Taxa de Mortes no Trânsito por 100.000 habitantes----

    ## Gráfico - Taxa de Mortes no Trânsito por 100.000 habitantes----
    # Atualização da entrada
    seg3comp <- reactive({
      input$seg3muni
    })
    observeEvent(seg3comp(), {
      x <- seg3 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != seg3comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "seg3municomp", choices = c("Selecione um município", choices), session)
    })

    output$seg3txt1 <- renderText({
    t31()  
    })

    output$seg3graf <- renderEcharts4r({
      req(input$seg3municomp)
      if (input$seg3municomp == "Selecione um município") {
        a <- seg3 %>% filter(localidade == input$seg3muni)
        a %>%
          e_charts(x = ano) %>%
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
            formatter =
              e_tooltip_pointer_formatter("decimal", digits = 2, locale = "pt-Br"),
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
            nameTextStyle =
              list(fontWeight = "bold", fontSize = 14),
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
        a <- seg3 %>% filter(localidade == input$seg3muni)
        b <- seg3 %>% filter(localidade == input$seg3municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$seg3muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$seg3municomp,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter =
              e_tooltip_pointer_formatter("decimal",
                digits = 2,
                locale = "pt-Br"
              ),
            axisPointer =
              list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Ano",
            splitLine = list(show = T),
            nameTextStyle = list(fontWeight = "bold", fontSize = 14)
          ) %>%
          e_y_axis(
            name = "Taxa",
            nameTextStyle =
              list(
                fontWeight = "bold",
                fontSize = 14
              ),
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
    ## Tabela - Taxa de Mortes no Trânsito por 100.000 habitantes----
    output$seg3txt2 <- renderText({
    t32()  
    })
    output$seg3tab2 <- renderReactable({
      ris <- seg3 %>%
        filter(ano == input$seg3ano, localidade == input$seg3muni) %>%
        pull(ri)
      x <- seg3 %>% filter(ano == input$seg3ano, localidade != "Pará")
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
            na = "-",
            name = "Taxa",
            format = colFormat(
              digits = 2,
              separators = T,
              locales = "pt-BR"
            ),
            # cell = data_bars(x,
            #   text_position = "above",
            #   box_shadow = TRUE,
            #   align_bars = "right",
            #   number_fmt = scales::number_format(
            #     accuracy = 0.01,
            #     decimal.mark = ",", big.mark = "."
            #   )
            # )
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


    # 4-Taxa do Crime de Homicídio por 100.000 habitantes----

    ## Gráfico - Taxa do Crime de Homicídio por 100.000 habitantes----
    # Atualização da entrada
    seg4comp <- reactive({
      input$seg4muni
    })
    observeEvent(seg4comp(), {
      x <- seg4 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != seg4comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "seg4municomp", choices = c("Selecione um município", choices), session)
    })

    output$seg4txt1 <- renderText({
    t41()  
    })

    output$seg4graf <- renderEcharts4r({
      req(input$seg4municomp)
      if (input$seg4municomp == "Selecione um município") {
        a <- seg4 %>% filter(localidade == input$seg4muni)
        a %>%
          e_charts(x = ano) %>%
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
            formatter =
              e_tooltip_pointer_formatter("decimal",
                digits = 2,
                locale = "pt-Br"
              ),
            axisPointer =
              list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Ano",
            splitLine = list(show = T),
            nameTextStyle =
              list(
                fontWeight = "bold",
                fontSize = 14
              )
          ) %>%
          e_y_axis(
            name = "Taxa",
            nameTextStyle =
              list(
                fontWeight = "bold",
                fontSize = 14
              ),
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
        a <- seg4 %>% filter(localidade == input$seg4muni)
        b <- seg4 %>% filter(localidade == input$seg4municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$seg4muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$seg4municomp,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter =
              e_tooltip_pointer_formatter("decimal", digits = 2, locale = "pt-Br"),
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
            nameTextStyle =
              list(fontWeight = "bold", fontSize = 14),
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
    ## Tabela - Taxa do Crime de Homicídio por 100.000 habitantes----
    output$seg4txt2 <- renderText({
    t42()  
    })
    output$seg4tab2 <- renderReactable({
      ris <- seg4 %>%
        filter(ano == input$seg4ano, localidade == input$seg4muni) %>%
        pull(ri)
      x <- seg4 %>% filter(ano == input$seg4ano, localidade != "Pará")
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
            name = "Taxa",
            format = colFormat(
              digits = 4,
              separators = T,
              locales = "pt-BR"
            ),
            # cell = data_bars(x,
            #   text_position = "above",
            #   box_shadow = TRUE,
            #   align_bars = "right",
            #   number_fmt = scales::number_format(
            #     accuracy = 0.01,
            #     decimal.mark = ",", big.mark = "."
            #   )
            # )
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


    # 5-Taxa do Crime de Homicídio no Trânsito por 100.000 habitantes----

    ## Gráfico - Taxa do Crime de Homicídio no Trânsito por 100.000 habitantes----
    # Atualização da entrada
    seg5comp <- reactive({
      input$seg5muni
    })
    observeEvent(seg5comp(), {
      x <- seg5 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != seg5comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "seg5municomp", choices = c("Selecione um município", choices), session)
    })

    output$seg5txt1 <- renderText({
    t51()  
    })

    output$seg5graf <- renderEcharts4r({
      req(input$seg5municomp)
      if (input$seg5municomp == "Selecione um município") {
        a <- seg5 %>% filter(localidade == input$seg5muni)
        a %>%
          e_charts(x = ano) %>%
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
            formatter =
              e_tooltip_pointer_formatter("decimal",
                digits = 2,
                locale = "pt-Br"
              ),
            axisPointer =
              list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Ano",
            splitLine = list(show = T),
            nameTextStyle =
              list(
                fontWeight = "bold",
                fontSize = 14
              )
          ) %>%
          e_y_axis(
            name = "Taxa",
            nameTextStyle =
              list(
                fontWeight = "bold",
                fontSize = 14
              ),
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
        a <- seg5 %>% filter(localidade == input$seg5muni)
        b <- seg5 %>% filter(localidade == input$seg5municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$seg5muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$seg5municomp,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter =
              e_tooltip_pointer_formatter("decimal", digits = 2, locale = "pt-Br"),
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
            nameTextStyle =
              list(fontWeight = "bold", fontSize = 14),
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
    ## Tabela - Taxa do Crime de Homicídio no Trânsito por 100.000 habitantes----
    output$seg5txt2 <- renderText({
    t52()  
    })
    output$seg5tab2 <- renderReactable({
      ris <- seg5 %>%
        filter(ano == input$seg5ano, localidade == input$seg5muni) %>%
        pull(ri)
      x <- seg5 %>% filter(ano == input$seg5ano, localidade != "Pará")
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
            na = "-",
            name = "Taxa",
            format = colFormat(
              digits = 2,
              separators = T,
              locales = "pt-BR"
            ),
            # cell = data_bars(x,
            #   text_position = "above",
            #   box_shadow = TRUE,
            #   align_bars = "right",
            #   number_fmt = scales::number_format(
            #     accuracy = 0.01,
            #     decimal.mark = ",", big.mark = "."
            #   )
            # )
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
    # 6-Taxa do Crime de Roubo por 100.000 habitantes----
    ## Gráfico - Taxa do Crime de Roubo por 100.000 habitantes----
    # Atualização da entrada
    seg6comp <- reactive({
      input$seg6muni
    })
    observeEvent(seg6comp(), {
      x <- seg6 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != seg6comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "seg6municomp", choices = c("Selecione um município", choices), session)
    })

    output$seg6txt1 <- renderText({
    t61()  
    })

    output$seg6graf <- renderEcharts4r({
      req(input$seg6municomp)
      if (input$seg6municomp == "Selecione um município") {
        a <- seg6 %>% filter(localidade == input$seg6muni)
        a %>%
          e_charts(x = ano) %>%
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
            formatter =
              e_tooltip_pointer_formatter("decimal", digits = 2, locale = "pt-Br"),
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
            nameTextStyle =
              list(fontWeight = "bold", fontSize = 14),
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
        a <- seg6 %>% filter(localidade == input$seg6muni)
        b <- seg6 %>% filter(localidade == input$seg6municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$seg6muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$seg6municomp,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter =
              e_tooltip_pointer_formatter("decimal",
                digits = 2,
                locale = "pt-Br"
              ),
            axisPointer =
              list(type = "shadow")
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
      }
    })
    ## Tabela - Taxa do Crime de Roubo por 100.000 habitantes----
    output$seg6txt2 <- renderText({
    t62()  
    })
    output$seg6tab2 <- renderReactable({
      ris <- seg6 %>%
        filter(ano == input$seg6ano, localidade == input$seg6muni) %>%
        pull(ri)
      x <- seg6 %>% filter(ano == input$seg6ano, localidade != "Pará")
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
            na = "-",
            name = "Taxa",
            format = colFormat(
              digits = 2,
              separators = T,
              locales = "pt-BR"
            ),
            # cell = data_bars(x,
            #   text_position = "above",
            #   box_shadow = TRUE,
            #   align_bars = "right",
            #   number_fmt = scales::number_format(
            #     accuracy = 0.01,
            #     decimal.mark = ",", big.mark = "."
            #   )
            # )
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

    # 7-Taxa do Crime de Latrocínio por 100.000 habitantes----

    ## Gráfico - Taxa do Crime de Latrocínio por 100.000 habitantes----
    # Atualização da entrada
    seg7comp <- reactive({
      input$seg7muni
    })
    observeEvent(seg7comp(), {
      x <- seg7 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != seg7comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "seg7municomp", choices = c("Selecione um município", choices), session)
    })

    output$seg7txt1 <- renderText({
    t71()  
    })

    output$seg7graf <- renderEcharts4r({
      req(input$seg7municomp)
      if (input$seg7municomp == "Selecione um município") {
        a <- seg7 %>% filter(localidade == input$seg7muni)
        a %>%
          e_charts(x = ano) %>%
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
            formatter =
              e_tooltip_pointer_formatter("decimal", digits = 2, locale = "pt-Br"),
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
      } else {
        a <- seg7 %>% filter(localidade == input$seg7muni)
        b <- seg7 %>% filter(localidade == input$seg7municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$seg7muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$seg7municomp,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter =
              e_tooltip_pointer_formatter("decimal",
                digits = 2,
                locale = "pt-Br"
              ),
            axisPointer =
              list(type = "shadow")
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
      }
    })
    ## Tabela - Taxa do Crime de Latrocínio por 100.000 habitantes----
    output$seg7txt2 <- renderText({
    t72()  
    })
    output$seg7tab2 <- renderReactable({
      ris <- seg7 %>%
        filter(ano == input$seg7ano, localidade == input$seg7muni) %>%
        pull(ri)
      x <- seg7 %>% filter(ano == input$seg7ano, localidade != "Pará")
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
            na = "-",
            name = "Taxa",
            format = colFormat(
              digits = 2,
              separators = T,
              locales = "pt-BR"
            ),
            # cell = data_bars(x,
            #   text_position = "above",
            #   box_shadow = TRUE,
            #   align_bars = "right",
            #   number_fmt = scales::number_format(
            #     accuracy = 0.01,
            #     decimal.mark = ",", big.mark = "."
            #   )
            # )
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
    # 8-Taxa do Crime de Lesão Corporal Seguida de Morte por 100.000 habitantes----
    ## Gráfico - Taxa do Crime de Lesão Corporal Seguida de Morte por 100.000 habitantes----
    # Atualização da entrada
    seg8comp <- reactive({
      input$seg8muni
    })
    observeEvent(seg8comp(), {
      x <- seg8 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != seg8comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "seg8municomp", choices = c("Selecione um município", choices), session)
    })

    output$seg8txt1 <- renderText({
    t81()  
    })

    output$seg8graf <- renderEcharts4r({
      req(input$seg8municomp)
      if (input$seg8municomp == "Selecione um município") {
        a <- seg8 %>% filter(localidade == input$seg8muni)
        a <- na.omit(a)
        a %>%
          e_charts(x = ano) %>%
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
      } else {
        a <- seg8 %>% filter(localidade == input$seg8muni)
        b <- seg8 %>% filter(localidade == input$seg8municomp)
        a <- na.omit(a)
        b <- na.omit(b)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$seg8muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$seg8municomp,
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
      }
    })
    ## Tabela - Taxa do Crime de Lesão Corporal Seguida de Morte por 100.000 habitantes----
    output$seg8txt2 <- renderText({
    t82()  
    })
    output$seg8tab2 <- renderReactable({
      ris <- seg8 %>%
        filter(ano == input$seg8ano, localidade == input$seg8muni) %>%
        pull(ri)
      x <- seg8 %>% filter(ano == input$seg8ano, localidade != "Pará")
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
            name = "Taxa",
            na = "-",
            format = colFormat(
              digits = 2,
              separators = T,
              locales = "pt-BR"
            ),
            # cell = data_bars(x,
            #   text_position = "above",
            #   box_shadow = TRUE,
            #   align_bars = "right",
            #   number_fmt = scales::number_format(
            #     accuracy = 0.01,
            #     decimal.mark = ",", big.mark = "."
            #   )
            # )
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
    #DOWNLOADS----
    # 1-Taxa de Homicídios Total por 100.000 habitantes----
    ## Gráfico - Taxa de Homicídios Total por 100.000 habitantes----
    # Filtra os dados
    seg1_1 <- reactive({
      req(input$seg1municomp)
      if (input$seg1municomp == "Selecione um município") {
        a <- seg1 %>% filter(localidade == input$seg1muni)
        } else {
        a <- seg1 %>% filter(localidade == input$seg1muni)
        b <- seg1 %>% filter(localidade == input$seg1municomp)
        df <- rbind(a,b)}
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(seg1_1(), {
      downset_Server("seg1_1", seg1_1(), t11())
    })
    ## Tabela - Taxa de Homicídios Total por 100.000 habitantes----
    # Filtra os dados
    seg1_2 <- reactive({
      ris <- seg1 %>%
        filter(ano == input$seg1ano, localidade == input$seg1muni) %>%
        pull(ri)
      x <- seg1 %>% 
        filter(ano == input$seg1ano,
               localidade != "Pará",
               ri == ri) %>% 
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(seg1_2(), {
      downset_Server("seg1_2", seg1_2(), t12())
    })
    
    # 2-Taxa de Homicídios de Jovens por 100.000 habitantes----
    ## Gráfico - Taxa de Homicídios de Jovens por 100.000 habitantes----
    # Filtra os dados
    seg2_1 <- reactive({
      req(input$seg2municomp)
      if (input$seg2municomp == "Selecione um município") {
        a <- seg2 %>% filter(localidade == input$seg2muni)
        } else {
        a <- seg2 %>% filter(localidade == input$seg2muni)
        b <- seg2 %>% filter(localidade == input$seg2municomp)
        df <- rbind(a,b)}
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(seg2_1(), {
      downset_Server("seg2_1", seg2_1(), t21())
    })
    ## Tabela - Taxa de Homicídios de Jovens por 100.000 habitantes----
    # Filtra os dados
    seg2_2 <- reactive({
      ris <- seg2 %>%
        filter(ano == input$seg2ano, localidade == input$seg2muni) %>%
        pull(ri)
      x <- seg2 %>% 
        filter(ano == input$seg2ano,
               localidade != "Pará",
               ri == ri) %>% 
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(seg2_2(), {
      downset_Server("seg2_2", seg2_2(), t22())
    })

    # 3-Taxa de Mortes no Trânsito por 100.000 habitantes----
    ## Gráfico - Taxa de Mortes no Trânsito por 100.000 habitantes----
    # Filtra os dados
    seg3_1 <- reactive({
      req(input$seg3municomp)
      if (input$seg3municomp == "Selecione um município") {
        a <- seg3 %>% filter(localidade == input$seg3muni)
        } else {
        a <- seg3 %>% filter(localidade == input$seg3muni)
        b <- seg3 %>% filter(localidade == input$seg3municomp)
        df <- rbind(a,b)}
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(seg3_1(), {
      downset_Server("seg3_1", seg3_1(), t31())
    })
    ## Tabela - Taxa de Mortes no Trânsito por 100.000 habitantes----
    # Filtra os dados
    seg3_2 <- reactive({
      ris <- seg3 %>%
        filter(ano == input$seg3ano, localidade == input$seg3muni) %>%
        pull(ri)
      x <- seg3 %>% 
        filter(ano == input$seg3ano,
               localidade != "Pará",
               ri == ri) %>% 
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(seg3_2(), {
      downset_Server("seg3_2", seg3_2(), t32())
    })

    # 4-Taxa do Crime de Homicídio por 100.000 habitantes----
    ## Gráfico - Taxa do Crime de Homicídio por 100.000 habitantes----
    # Filtra os dados
    seg4_1 <- reactive({
      req(input$seg4municomp)
      if (input$seg4municomp == "Selecione um município") {
        a <- seg4 %>% filter(localidade == input$seg4muni)
        } else {
        a <- seg4 %>% filter(localidade == input$seg4muni)
        b <- seg4 %>% filter(localidade == input$seg4municomp)
        df <- rbind(a,b)}
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(seg4_1(), {
      downset_Server("seg4_1", seg4_1(), t41())
    })
    ## Tabela - Taxa do Crime de Homicídio por 100.000 habitantes----
    # Filtra os dados
    seg4_2 <- reactive({
      ris <- seg4 %>%
        filter(ano == input$seg4ano, localidade == input$seg4muni) %>%
        pull(ri)
      x <- seg4 %>% 
        filter(ano == input$seg4ano, 
               localidade != "Pará",
               ri == ri) %>% 
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(seg4_2(), {
      downset_Server("seg4_2", seg4_2(), t42())
    })

    # 5-Taxa do Crime de Homicídio no Trânsito por 100.000 habitantes----
    ## Gráfico - Taxa do Crime de Homicídio no Trânsito por 100.000 habitantes----
    # Filtra os dados
    seg5_1 <- reactive({
      req(input$seg5municomp)
      if (input$seg5municomp == "Selecione um município") {
        a <- seg5 %>% filter(localidade == input$seg5muni)
        } else {
        a <- seg5 %>% filter(localidade == input$seg5muni)
        b <- seg5 %>% filter(localidade == input$seg5municomp)
        df <- rbind(a,b)}
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(seg5_1(), {
      downset_Server("seg5_1", seg5_1(), t51())
    })
    ## Tabela - Taxa do Crime de Homicídio no Trânsito por 100.000 habitantes----
    # Filtra os dados
    seg5_2 <- reactive({
      ris <- seg5 %>%
        filter(ano == input$seg5ano, localidade == input$seg5muni) %>%
        pull(ri)
      x <- seg5 %>% 
        filter(ano == input$seg5ano, 
               localidade != "Pará",
               ri == ri) %>% 
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(seg5_2(), {
      downset_Server("seg5_2", seg5_2(), t52())
    })

    # 6-Taxa do Crime de Roubo por 100.000 habitantes----
    ## Gráfico - Taxa do Crime de Roubo por 100.000 habitantes----
    # Filtra os dados
    seg6_1 <- reactive({
      req(input$seg6municomp)
      if (input$seg6municomp == "Selecione um município") {
        a <- seg6 %>% filter(localidade == input$seg6muni)
        } else {
        a <- seg6 %>% filter(localidade == input$seg6muni)
        b <- seg6 %>% filter(localidade == input$seg6municomp)
        df <- rbind(a,b)}
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(seg6_1(), {
      downset_Server("seg6_1", seg6_1(), t61())
    })
    ## Tabela - Taxa do Crime de Roubo por 100.000 habitantes----
    # Filtra os dados
    seg6_2 <- reactive({
      ris <- seg6 %>%
        filter(ano == input$seg6ano, localidade == input$seg6muni) %>%
        pull(ri)
      x <- seg6 %>% 
        filter(ano == input$seg6ano, 
               localidade != "Pará",
               ri == ri) %>% 
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(seg6_2(), {
      downset_Server("seg6_2", seg6_2(), t62())
    })

    # 7-Taxa do Crime de Latrocínio por 100.000 habitantes----
    ## Gráfico - Taxa do Crime de Latrocínio por 100.000 habitantes----
    # Filtra os dados
    seg7_1 <- reactive({
      req(input$seg7municomp)
      if (input$seg7municomp == "Selecione um município") {
        a <- seg7 %>% filter(localidade == input$seg7muni)
        } else {
        a <- seg7 %>% filter(localidade == input$seg7muni)
        b <- seg7 %>% filter(localidade == input$seg7municomp)
        df <- rbind(a,b)}
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(seg7_1(), {
      downset_Server("seg7_1", seg7_1(), t71())
    })
    ## Tabela - Taxa do Crime de Latrocínio por 100.000 habitantes----
    # Filtra os dados
    seg7_2 <- reactive({
      ris <- seg7 %>%
        filter(ano == input$seg7ano, localidade == input$seg7muni) %>%
        pull(ri)
      x <- seg7 %>% 
        filter(ano == input$seg7ano, 
               localidade != "Pará",
               ri == ri) %>% 
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(seg7_2(), {
      downset_Server("seg7_2", seg7_2(), t72())
    })

    # 8-Taxa do Crime de Lesão Corporal Seguida de Morte por 100.000 habitantes----
    ## Gráfico - Taxa do Crime de Lesão Corporal Seguida de Morte por 100.000 habitantes----
    # Filtra os dados
    seg8_1 <- reactive({
      req(input$seg8municomp)
      if (input$seg8municomp == "Selecione um município") {
        a <- seg8 %>% filter(localidade == input$seg8muni)
        } else {
        a <- seg8 %>% filter(localidade == input$seg8muni)
        b <- seg8 %>% filter(localidade == input$seg8municomp)
        df <- rbind(a,b)}
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(seg8_1(), {
      downset_Server("seg8_1", seg8_1(), t81())
    })
    ## Tabela - Taxa do Crime de Lesão Corporal Seguida de Morte por 100.000 habitantes----
    # Filtra os dados
    seg8_2 <- reactive({
      ris <- seg8 %>%
        filter(ano == input$seg8ano, localidade == input$seg8muni) %>%
        pull(ri)
      x <- seg8 %>% 
        filter(ano == input$seg8ano,
               localidade != "Pará",
               ri == ri) %>% 
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(seg8_2(), {
      downset_Server("seg8_2", seg8_2(), t82())
    })
  })
}

# Play do Módulo
# ui <- dashboardPage(
#   header = dashboardHeader(),
#   sidebar = dashboardSidebar(),
#   body = dashboardBody(fluidPage(
#     social_seguranca_mp_ui("social_seguranca_mp")
#   ))
# )
# server <- function(input, output) {
#   social_seguranca_mp_Server("social_seguranca_mp")
# }
# 
# shinyApp(ui, server)
