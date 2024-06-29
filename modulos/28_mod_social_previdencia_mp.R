# Funções de módulo de Social - Previdência Social - Municipal
# Função de UI
social_previdencia_mp_ui <- function(id) {
  fluidPage(# CAMINHO DO ARQUIVO CSS----
            tags$head(
              tags$link(rel = "stylesheet", type = "text/css", href = "estilo.css")
            ),
            div(class = "navbar_social", navbarPage(
              tags$b("Previdência Social - Municípios"),
              navbarMenu(
                "Indicadores",
                # 1 - Quantidade de Benefícios Emitidos em Dezembro----
                tabPanel(
                  "Quantidade de Benefícios Emitidos em Dezembro",
                  panel(
                    ## Controle----
                    heading =
                        h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Quantidade de Benefícios Emitidos em Dezembro"),
                        tags$div(
                          class = "seletor1",
                          pickerInput(
                            inputId = NS(id, "prev1muni"),
                            label = "Município",
                            choices = prev1 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                            width = "200px"
                          )
                        )),
                  fluidRow(
                    ## Gráfico - Quantidade de Benefícios Emitidos em Dezembro----
                    box(
                      title = textOutput(NS(id, "prev1txt1")),
                      collapsible = T,
                      collapsed = F,
                      headerBorder = T,
                      solidHeader = T,
                      width = 12,
                      pickerInput(
                        inputId = NS(id, "prev1municomp"),
                        label = "Comparar Município",
                        choices = NULL,
                        width = "200px",
                        options = list(`none-selected-text` = "Selecione um município")
                      ),
                      withSpinner(
                        echarts4rOutput(NS(id, "prev1graf")),
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
                      ), downset_ui(NS(id, "prev1_1")))
                    ),
                    ## Tabela - Quantidade de Benefícios Emitidos em Dezembro----
                    box(
                      title = textOutput(NS(id, "prev1txt2")),
                      collapsible = T,
                      collapsed = F,
                      headerBorder = T,
                      width = 12,
                      pickerInput(
                        inputId = NS(id, "prev1ano"),
                        label = "Ano",
                        choices = sort(unique(prev1[["ano"]]), decreasing = T),
                        width = "100px"
                      ),
                      withSpinner(
                        reactableOutput(NS(id, "prev1tab2"), height = "400px"),
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
                      ), downset_ui(NS(id, "prev1_2")))
                    )
                  )
                ),
                
                # 2 - Valor dos Benefícios Emitidos em Dezembro----
                tabPanel(
                  "Valor dos Benefícios Emitidos em Dezembro",
                  panel(
                    ## Controle----
                    heading =
                        h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Valor dos Benefícios Emitidos em Dezembro"),
                        tags$div(
                          class = "seletor1",
                          pickerInput(
                            inputId = NS(id, "prev2muni"),
                            label = "Município",
                            choices = prev2 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                            width = "200px"
                          )
                        )),
                  fluidRow(
                    ## Gráfico - Valor dos Benefícios Emitidos em Dezembro----
                    box(
                      title = textOutput(NS(id, "prev2txt1")),
                      collapsible = T,
                      collapsed = F,
                      headerBorder = T,
                      width = 12,
                      pickerInput(
                        inputId = NS(id, "prev2municomp"),
                        label = "Comparar Município",
                        choices = NULL,
                        width = "200px",
                        options = list(`none-selected-text` = "Selecione um município")
                      ),
                      withSpinner(
                        echarts4rOutput(NS(id, "prev2graf")),
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
                      ), downset_ui(NS(id, "prev2_1")))
                    ),
                    ## Tabela - Valor dos Benefícios Emitidos em Dezembro----
                    box(
                      title = textOutput(NS(id, "prev2txt2")),
                      collapsible = T,
                      collapsed = F,
                      headerBorder = T,
                      width = 12,
                      pickerInput(
                        inputId = NS(id, "prev2ano"),
                        label = "Ano",
                        choices = sort(unique(prev2[["ano"]]), decreasing = T),
                        width = "100px"
                      ),
                      withSpinner(
                        reactableOutput(NS(id, "prev2tab2"), height = "400px"),
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
                      ), downset_ui(NS(id, "prev2_2")))
                    )
                  )
                ),
                
                # 3 - Valor dos Benefícios Emitidos ao Ano (benefícios previdenciários, assistenciais e de legislação específica)----
                tabPanel(
                  "Valor dos Benefícios Emitidos ao Ano",
                  panel(
                    ## Controle----
                    heading =
                        h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Valor dos Benefícios Emitidos ao Ano"),
                        tags$div(
                          class = "seletor1",
                          pickerInput(
                            inputId = NS(id, "prev3muni"),
                            label = "Município",
                            choices = prev3 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                            width = "200px"
                          )
                        )),
                  fluidRow(
                    ## Gráfico - Valor dos Benefícios Emitidos ao Ano----
                    box(
                      title = textOutput(NS(id, "prev3txt1")),
                      collapsible = T,
                      collapsed = F,
                      headerBorder = T,
                      width = 12,
                      pickerInput(
                        inputId = NS(id, "prev3municomp"),
                        label = "Comparar Município",
                        choices = NULL,
                        width = "200px",
                        options = list(`none-selected-text` = "Selecione um município")
                      ),
                      withSpinner(
                        echarts4rOutput(NS(id, "prev3graf")),
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
                      ), downset_ui(NS(id, "prev3_1")))
                    ),
                    
                    ## Tabela - Valor dos Benefícios Emitidos ao Ano----
                    box(
                      title = textOutput(NS(id, "prev3txt2")),
                      collapsible = T,
                      collapsed = F,
                      headerBorder = T,
                      width = 12,
                      pickerInput(
                        inputId = NS(id, "prev3ano"),
                        label = "Ano",
                        choices = sort(unique(prev3[["ano"]]), decreasing = T),
                        width = "100px"
                      ),
                      withSpinner(
                        reactableOutput(NS(id, "prev3tab2"), height = "400px"),
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
                      ), downset_ui(NS(id, "prev3_2")))
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
                    ## Controle----
                    heading =
                        h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Quantidade de benefícios emitidos no mês de dezembro"),
                        tags$div(
                          class = "seletor1",
                          # select Município
                          pickerInput(
                            inputId = NS(id, "prev4_1muni"),
                            label = "Município",
                            choices = prev4_1 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                            width = "200px"
                          )
                        )),
                  fluidRow(
                    ## Gráfico - Quantidade de benefícios emitidos no mês de dezembro----
                    box(
                      title = textOutput(NS(id, "prev4_1txt1")),
                      collapsible = T,
                      collapsed = F,
                      headerBorder = T,
                      width = 12,
                      column(
                        2,
                        pickerInput(
                          inputId = NS(id, "prev4_1municomp"),
                          label = "Comparar Município",
                          choices = NULL,
                          width = "200px",
                          options = list(`none-selected-text` = "Selecione um município")
                        ),
                      ),
                      column(
                        6,
                        pickerInput(
                          inputId = NS(id, "prev4_1ano"),
                          label = "Ano",
                          choices = prev4_1 %>%
                            select(ano) %>%
                            pull() %>% unique() %>%
                            sort(decreasing = T),
                          width = "100px"
                        )
                      ),
                      column(
                        12,
                        withSpinner(
                          echarts4rOutput(NS(id, "prev4_1graf")),
                          type = 8,
                          color = "#f17701",
                          size = 0.5
                        )
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "SINTESE/DATAPREV"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                        tags$h6(
                          "benefícios: previdenciários, assistenciais e de legislação específica "
                        )
                      ), downset_ui(NS(id, "prev4_1_1")))
                    ),
                    ## Tabela - Quantidade de benefícios emitidos no mês de dezembro----
                    box(
                      title = textOutput(NS(id, "prev4_1txt2")),
                      collapsible = T,
                      collapsed = F,
                      headerBorder = T,
                      width = 12,
                      withSpinner(
                        reactableOutput(NS(id, "prev4_1tab")),
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
                      ), downset_ui(NS(id, "prev4_1_2")))
                    ),
                    ## Tabela - Quantidade de benefícios emitidos no mês de dezembro----
                    box(
                      title = textOutput(NS(id, "prev4_1txt3")),
                      collapsible = T,
                      collapsed = F,
                      headerBorder = T,
                      width = 12,
                      pickerInput(
                        inputId = NS(id, "prev4_1ano2"),
                        label = "Ano",
                        choices = prev4_1 %>%
                          select(ano) %>%
                          pull() %>% unique() %>%
                          sort(decreasing = T),
                        width = "100px"
                      ),
                      withSpinner(
                        reactableOutput(NS(id, "prev4_1tab1"), height = "400px"),
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
                      ), downset_ui(NS(id, "prev4_1_3")))
                    ),
                  )
                ),
                
                
                ### 4.2-Valor dos benefícios emitidos no mês de dezembro (em R$)----
                tabPanel(
                  "Valor dos benefícios emitidos no mês de dezembro (em R$)",
                  panel(
                    ## Controle----
                    heading =
                        h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Valor dos benefícios emitidos no mês de dezembro (em R$)"),
                        tags$div(
                          class = "seletor1",
                          # select Município
                          pickerInput(
                            inputId = NS(id, "prev4_2muni"),
                            label = "Município",
                            choices = prev4_2 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                            width = "200px"
                          )
                        )),
                  fluidRow(
                    ## Gráfico - Valor dos benefícios emitidos no mês de dezembro (em R$)----
                    box(
                      title = textOutput(NS(id, "prev4_2txt1")),
                      collapsible = T,
                      collapsed = F,
                      headerBorder = T,
                      width = 12,
                      column(
                        2,
                        pickerInput(
                          inputId = NS(id, "prev4_2municomp"),
                          label = "Comparar Município",
                          choices = NULL,
                          width = "200px",
                          options = list(`none-selected-text` = "Selecione um município")
                        )
                      ),
                      column(
                        6,
                        pickerInput(
                          inputId = NS(id, "prev4_2ano"),
                          label = "Ano",
                          choices = prev4_2 %>%
                            select(ano) %>%
                            pull() %>%
                            unique() %>%
                            sort(decreasing = T),
                          width = "100px"
                        )
                      ),
                      column(
                        12,
                        withSpinner(
                          echarts4rOutput(NS(id, "prev4_2graf")),
                          type = 8,
                          color = "#f17701",
                          size = 0.5
                        )
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "SINTESE/DATAPREV"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                        tags$h6(
                          "benefícios: previdenciários, assistenciais e de legislação específica "
                        )
                      ), downset_ui(NS(id, "prev4_2_1")))
                    ),
                    ## Tabela - Valor dos benefícios emitidos no mês de dezembro (em R$)----
                    box(
                      title = textOutput(NS(id, "prev4_2txt2")),
                      collapsible = T,
                      collapsed = F,
                      headerBorder = T,
                      width = 12,
                      withSpinner(
                        reactableOutput(NS(id, "prev4_2tab")),
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
                      ), downset_ui(NS(id, "prev4_2_2")))
                    ),
                    ## Tabela - Valor dos benefícios emitidos no mês de dezembro (em R$)----
                    box(
                      title = textOutput(NS(id, "prev4_2txt3")),
                      collapsible = T,
                      collapsed = F,
                      headerBorder = T,
                      width = 12,
                      pickerInput(
                        inputId = NS(id, "prev4_2ano2"),
                        label = "Ano",
                        choices = prev4_2 %>%
                          select(ano) %>%
                          pull() %>%
                          unique() %>%
                          sort(decreasing = T),
                        width = "100px"
                      ),
                      withSpinner(
                        reactableOutput(NS(id, "prev4_2tab1"), height = "400px"),
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
                      ), downset_ui(NS(id, "prev4_2_3")))
                    )
                  )
                ),
                ### 4.3-Valor dos benefícios emitidos no ano (em R$)----
                tabPanel(
                  "Valor dos benefícios emitidos no ano (em R$)",
                  panel(
                    ## Controle----
                    heading =
                        h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Valor dos benefícios emitidos no ano (em R$)"),
                        tags$div(
                          class = "seletor1",
                          # select Município
                          pickerInput(
                            inputId = NS(id, "prev4_3muni"),
                            label = "Município",
                            choices = prev4_3 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                            width = "200px"
                          )
                        )),
                  fluidRow(
                    ## Gráfico - Valor dos benefícios emitidos no ano (em R$)----
                    box(
                      title = textOutput(NS(id, "prev4_3txt1")),
                      collapsible = T,
                      collapsed = F,
                      headerBorder = T,
                      width = 12,
                      column(
                        2,
                        pickerInput(
                          inputId = NS(id, "prev4_3municomp"),
                          label = "Comparar Município",
                          choices = NULL,
                          width = "200px",
                          options = list(`none-selected-text` = "Selecione um município")
                        )
                      ),
                      column(
                        6,
                        pickerInput(
                          inputId = NS(id, "prev4_3ano"),
                          label = "Ano",
                          choices = prev4_3 %>%
                            select(ano) %>%
                            pull() %>%
                            unique() %>%
                            sort(decreasing = T),
                          width = "100px"
                        )
                      ),
                      column(
                        12,
                        withSpinner(
                          echarts4rOutput(NS(id, "prev4_3graf")),
                          type = 8,
                          color = "#f17701",
                          size = 0.5
                        )
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "SINTESE/DATAPREV"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                        tags$h6(
                          "benefícios: previdenciários, assistenciais e de legislação específica "
                        )
                      ), downset_ui(NS(id, "prev4_3_1")))
                    ),
                    ## Tabela - Valor dos benefícios emitidos no ano (em R$)----
                    box(
                      title = textOutput(NS(id, "prev4_3txt2")),
                      collapsible = T,
                      collapsed = F,
                      headerBorder = T,
                      width = 12,
                      withSpinner(
                        reactableOutput(NS(id, "prev4_3tab")),
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
                      ), downset_ui(NS(id, "prev4_3_2")))
                    ),
                    ## Tabela - Valor dos benefícios emitidos no ano (em R$)----
                    box(
                      title = textOutput(NS(id, "prev4_3txt3")),
                      collapsible = T,
                      collapsed = F,
                      headerBorder = T,
                      width = 12,
                      pickerInput(
                        inputId = NS(id, "prev4_3ano2"),
                        label = "Ano",
                        choices = prev4_3 %>%
                          select(ano) %>%
                          pull() %>%
                          unique() %>%
                          sort(decreasing = T),
                        width = "100px"
                      ),
                      withSpinner(
                        reactableOutput(NS(id, "prev4_3tab1"), height = "400px"),
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
                      ), downset_ui(NS(id, "prev4_3_3")))
                    )
                  )
                )
              )
            )))
}
# Função do modulo servidor
social_previdencia_mp_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    #TÍTULOS----
    # 1 - Quantidade de Benefícios Emitidos em Dezembro----
    ## Gráfico - Quantidade de Benefícios Emitidos em Dezembro----
    # Atualização da entrada
    t11 <- reactive({
      req(input$prev1municomp)
      if (input$prev1municomp == "Selecione um município") {
        paste0(
          "Quantidade de Benefícios Emitidos em Dezembro, ",
          input$prev1muni,
          " - ",
          min(prev1$ano),
          " a ",
          max(prev1$ano)
        )
      } else {
        paste0(
          "Quantidade de Benefícios Emitidos em Dezembro, ",
          input$prev1muni,
          " x ",
          input$prev1municomp,
          " - ",
          min(prev1$ano),
          " a ",
          max(prev1$ano)
        )
      }
    })
    ## Tabela - Quantidade de Benefícios Emitidos em Dezembro----
    t12 <- reactive({
      ri <- prev1 %>%
        filter(ano == input$prev1ano, localidade == input$prev1muni) %>%
        pull(ri)
      paste0(
        "Quantidade de Benefícios Emitidos em Dezembro por Município, Região de Integração ",
        unique(ri),
        " - ",
        input$prev1ano
      )
    })
    # 2 - Valor dos Benefícios Emitidosem Dezembro----
    ## Gráfico - Valor dos Benefícios Emitidos em Dezembro----
    t21 <- reactive({
      req(input$prev2municomp)
      if (input$prev2municomp == "Selecione um município") {
        paste0(
          "Valor dos Benefícios Emitidos em Dezembro, ",
          input$prev2muni,
          " - ",
          min(prev2$ano),
          " a ",
          max(prev2$ano)
        )
      } else {
        paste0(
          "Valor dos Benefícios Emitidos em Dezembro, ",
          input$prev2muni,
          " x ",
          input$prev2municomp,
          " - ",
          min(prev2$ano),
          " a ",
          max(prev2$ano)
        )
      }
    })
    ## Tabela - Valor dos Benefícios Emitidos em Dezembro----
    t22 <- reactive({
      ri <- prev2 %>%
        filter(ano == input$prev2ano, localidade == input$prev2muni) %>%
        pull(ri)
      paste0(
        "Valor dos Benefícios Emitidos em Dezembro por Município, Região de Integração ",
        unique(ri),
        " - ",
        input$prev2ano
      )
    })
    # 3 - Valor dos Benefícios Emitidos ao Ano----
    ## Gráfico - Valor dos Benefícios Emitidos ao Ano----
    t31 <- reactive({
      req(input$prev3municomp)
      if (input$prev3municomp == "Selecione um município") {
        paste0(
          "Valor dos Benefícios Emitidos ao Ano, ",
          input$prev3muni,
          " - ",
          min(prev3$ano),
          " a ",
          max(prev3$ano)
        )
      } else {
        paste0(
          "Valor dos Benefícios Emitidos ao Ano, ",
          input$prev3muni,
          " x ",
          input$prev3municomp,
          " - ",
          min(prev3$ano),
          " a ",
          max(prev3$ano)
        )
      }
    })
    ## Tabela - Valor dos Benefícios Emitidos ao Ano----
    t32 <- reactive({
      ri <- prev3 %>%
        filter(ano == input$prev3ano, localidade == input$prev3muni) %>%
        pull(ri)
      paste0(
        "Valor dos Benefícios Emitidos ao Ano por Município, Região de Integração ",
        unique(ri),
        " - ",
        input$prev3ano
      )
    })
    # 4 - Benefícios Emitidos pela Previdência Social por Localização----
    ## Arrecadação e Benefícios Emitidos pela Previdência Social----
    ### 4.1-Quantidade de benefícios emitidos no mês de dezembro----
    ## Gráfico - Quantidade de benefícios emitidos no mês de dezembro----
    t411 <- reactive({
      req(input$prev4_1municomp)
      if (input$prev4_1municomp == "Selecione um município") {
        paste0(
          "Quantidade de benefícios emitidos no mês de dezembro por Localização, ",
          input$prev4_1muni,
          " - ",
          input$prev4_1ano
        )
      } else {
        paste0(
          "Quantidade de benefícios emitidos no mês de dezembro por Localização, ",
          input$prev4_1muni,
          " x ",
          input$prev4_1municomp,
          " - ",
          input$prev4_1ano
        )
      }
    })
    ## Tabela - Quantidade de benefícios emitidos no mês de dezembro----
    t412 <- reactive({
      paste0(
        "Quantidade de benefícios emitidos no mês de dezembro por Localização, ",
        input$prev4_1muni,
        " - ",
        min(prev4_1$ano),
        " a ",
        max(prev4_1$ano)
      )
    })
    ## Tabela - Quantidade de benefícios emitidos no mês de dezembro----
    t413 <- reactive({
      ri <- prev4_1 %>%
        filter(
          ano == input$prev4_1ano2,
          localidade == input$prev4_1muni
        ) %>%
        pull(ri)
      paste0(
        "Quantidade de benefícios emitidos no mês de dezembro por Localização e Município, Região de Integração ",
        unique(ri),
        " - ",
        input$prev4_1ano2
      )
    })
    ### 4.2-Valor dos benefícios emitidos no mês de dezembro (em R$)----
    ## Gráfico - Valor dos benefícios emitidos no mês de dezembro (em R$)----
    t421 <- reactive({
      req(input$prev4_2municomp)
      if (input$prev4_2municomp == "Selecione um município") {
        paste0(
          "Valor dos benefícios emitidos no mês de dezembro (em R$) por Localização, ",
          input$prev4_2muni,
          " - ",
          input$prev4_2ano
        )
      } else {
        paste0(
          "Valor dos benefícios emitidos no mês de dezembro (em R$) por Localização, ",
          input$prev4_2muni,
          " x ",
          input$prev4_2municomp,
          " - ",
          input$prev4_2ano
        )
      }
    })
    ## Tabela - Valor dos benefícios emitidos no mês de dezembro (em R$)----
    t422 <- reactive({
      paste0(
        "Valor dos benefícios emitidos no mês de dezembro (em R$) por Localização, ",
        input$prev4_2muni,
        " - ",
        min(prev4_2$ano),
        " a ",
        max(prev4_2$ano)
      )
    })
    ## Tabela - Valor dos benefícios emitidos no mês de dezembro (em R$)----
    t423 <- reactive({
      ri <- prev4_2 %>%
        filter(
          ano == input$prev4_2ano2,
          localidade == input$prev4_2muni
        ) %>%
        pull(ri)
      paste0(
        "Valor dos benefícios emitidos no mês de dezembro (em R$) por Localização e Município, Região de Integração ",
        unique(ri),
        " - ",
        input$prev4_2ano2
      )
    })
    ### 4.3 - Valor dos benefícios emitidos no ano (em R$)----
    ## Gráfico - Valor dos benefícios emitidos no ano (em R$)----
    # Atualização da entrada
    ## Título
    t431 <- reactive({
      req(input$prev4_3municomp)
      if (input$prev4_3municomp == "Selecione um município") {
        paste0(
          "Valor dos benefícios emitidos no ano (em R$) por Localização, ",
          input$prev4_3muni,
          " - ",
          input$prev4_3ano
        )
      } else {
        paste0(
          "Valor dos benefícios emitidos no ano (em R$) por Localização, ",
          input$prev4_3muni,
          " x ",
          input$prev4_3municomp,
          " - ",
          input$prev4_3ano
        )
      }
    })
    ## Tabela - Valor dos benefícios emitidos no ano (em R$)----
    t432 <- reactive({
      paste0(
        "Valor dos benefícios emitidos no ano (em R$) por Localização, ",
        input$prev4_3muni,
        " - ",
        min(prev4_3$ano),
        " a ",
        max(prev4_3$ano)
      )
    })
    ## Tabela - Valor dos benefícios emitidos no ano (em R$)----
    t433 <- reactive({
      ri <- prev4_3 %>%
        filter(
          ano == input$prev4_3ano2,
          localidade == input$prev4_3muni
        ) %>%
        pull(ri)
      paste0(
        "Valor dos benefícios emitidos no ano (em R$) por Localização e Município, Região de Integração ",
        unique(ri),
        " - ",
        input$prev4_3ano2
      )
    })
    #VISUALIZAÇÃO----
    # 1 - Quantidade de Benefícios Emitidos em Dezembro----
    ## Gráfico - Quantidade de Benefícios Emitidos em Dezembro----
    # Atualização da entrada
    prev1comp <- reactive({
      input$prev1muni
    })
    observeEvent(prev1comp(), {
      x <- prev1 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != prev1comp())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "prev1municomp",
        choices = c("Selecione um município", choices),
        session
      )
    })
    output$prev1txt1 <- renderText({
      t11()
    })
    output$prev1graf <- renderEcharts4r({
      req(input$prev1municomp)
      if (input$prev1municomp == "Selecione um município") {
        a <- prev1 %>% filter(localidade == input$prev1muni)
        a %>%
          e_charts(x = ano) %>%
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
        a <- prev1 %>% filter(localidade == input$prev1muni)
        b <-
          prev1 %>% filter(localidade == input$prev1municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$prev1muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$prev1municomp,
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
      }
    })
    ## Tabela - Quantidade de Benefícios Emitidos em Dezembro----
    output$prev1txt2 <- renderText({
      t12()
    })
    output$prev1tab2 <- renderReactable({
      ris <- prev1 %>%
        filter(ano == input$prev1ano, localidade == input$prev1muni) %>%
        pull(ri)
      x <-
        prev1 %>% filter(ano == input$prev1ano, localidade != "Pará")
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
          valor = colDef(
            name = "Quantidade",
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
    
    # 2 - Valor dos Benefícios Emitidos em Dezembro----
    ## Gráfico - Valor dos Benefícios Emitidos em Dezembro----
    # Atualização da entrada
    prev2comp <- reactive({
      input$prev2muni
    })
    observeEvent(prev2comp(), {
      x <- prev2 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != prev2comp())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "prev2municomp",
        choices = c("Selecione um município", choices),
        session
      )
    })
    
    output$prev2txt1 <- renderText({
      t21()
    })
    
    output$prev2graf <- renderEcharts4r({
      req(input$prev2municomp)
      if (input$prev2municomp == "Selecione um município") {
        a <- prev2 %>% filter(localidade == input$prev2muni)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f17701",
            name = "Valor",
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
            name = "Valor",
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
        a <- prev2 %>% filter(localidade == input$prev2muni)
        b <-
          prev2 %>% filter(localidade == input$prev2municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$prev2muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$prev2municomp,
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
            name = "Valor",
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
    ## Tabela - Valor dos Benefícios Emitidos em Dezembro----
    output$prev2txt2 <- renderText({
      t22()
    })
    output$prev2tab2 <- renderReactable({
      ris <- prev2 %>%
        filter(ano == input$prev2ano, localidade == input$prev2muni) %>%
        pull(ri)
      x <-
        prev2 %>% filter(ano == input$prev2ano, localidade != "Pará")
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
          valor = colDef(
            name = "Valor",
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
    
    # 3 - Valor dos Benefícios Emitidos ao Ano (benefícios previdenciários, assistenciais e de legislação específica)----
    ## Gráfico - Valor dos Benefícios Emitidos ao Ano (benefícios previdenciários, assistenciais e de legislação específica)----
    # Atualização da entrada
    prev3comp <- reactive({
      input$prev3muni
    })
    observeEvent(prev3comp(), {
      x <- prev3 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != prev3comp())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "prev3municomp",
        choices = c("Selecione um município", choices),
        session
      )
    })
    
    output$prev3txt1 <- renderText({
      t31()
    })
    
    output$prev3graf <- renderEcharts4r({
      req(input$prev3municomp)
      if (input$prev3municomp == "Selecione um município") {
        a <- prev3 %>% filter(localidade == input$prev3muni)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f17701",
            name = "Valor",
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
            name = "Valor",
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
        a <- prev3 %>% filter(localidade == input$prev3muni)
        b <-
          prev3 %>% filter(localidade == input$prev3municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$prev3muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$prev3municomp,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            label = list(
              show = F,
              formatter = formatar_numero_br(1),
              position = "top",
              fontWeight = "bold",
              fontSize = 12
            ),
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
            name = "Valor",
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
    ## Tabela - Valor dos Benefícios Emitidos ao Ano (benefícios previdenciários, assistenciais e de legislação específica)----
    output$prev3txt2 <- renderText({
      t32()
    })
    output$prev3tab2 <- renderReactable({
      ris <- prev3 %>%
        filter(ano == input$prev3ano, localidade == input$prev3muni) %>%
        pull(ri)
      x <-
        prev3 %>% filter(ano == input$prev3ano, localidade != "Pará")
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
          valor = colDef(
            name = "Valor",
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
    
    
    # 4 - Benefícios Emitidos pela Previdência Social por Localização----
    ## Arrecadação e Benefícios Emitidos pela Previdência Social----
    ### 4.1-Quantidade de benefícios emitidos no mês de dezembro----
    ## Gráfico - Quantidade de benefícios emitidos no mês de dezembro----
    # Atualização da entrada
    prev4_1comp <- reactive({
      input$prev4_1muni
    })
    observeEvent(prev4_1comp(), {
      x <- prev4_1 %>% filter(
        localidade != "Pará"
      )
      x <- x %>% filter(localidade != prev4_1comp())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "prev4_1municomp",
        choices = c("Selecione um município", choices),
        session
      )
    })
    
    ## Título
    output$prev4_1txt1 <- renderText({
      t411()
    })
    
    output$prev4_1graf <- renderEcharts4r({
      req(input$prev4_1municomp)
      if (input$prev4_1municomp == "Selecione um município") {
        a <-
          prev4_1 %>% filter(
            localidade == input$prev4_1muni,
            ano == input$prev4_1ano,
            categoria != "Total"
          )
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            color = "#f17701",
            name = "Quantidade",
            legend = F,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            barWidth = "20%",
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 0, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Zona",
            nameLocation = "middle",
            nameTextStyle = list(
              fontWeight = "bold",
              padding = c(20, 0, 0, 0),
              fontSize = 14
            )
          ) %>%
          e_y_axis(
            name = "Quantidade",
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
          e_grid(show = T)
      } else {
        a <-
          prev4_1 %>% filter(
            localidade == input$prev4_1muni,
            ano == input$prev4_1ano,
            categoria != "Total"
          )
        b <-
          prev4_1 %>% filter(
            localidade == input$prev4_1municomp,
            ano == input$prev4_1ano,
            categoria != "Total"
          )
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            name = input$prev4_1muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            barWidth = "20%",
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, categoria) %>%
          e_bar(
            serie = valor,
            name = input$prev4_1municomp,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            barWidth = "20%",
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 0, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Zona",
            nameLocation = "middle",
            nameTextStyle = list(
              fontWeight = "bold",
              padding = c(20, 0, 0, 0),
              fontSize = 14
            )
          ) %>%
          e_y_axis(
            name = "Quantidade",
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
          e_grid(show = T)
      }
    })
    ## Tabela - Quantidade de benefícios emitidos no mês de dezembro----
    output$prev4_1txt2 <- renderText({
      t412()
    })
    output$prev4_1tab <- renderReactable({
      x <- prev4_1 %>%
        filter(
          localidade == input$prev4_1muni
        ) %>%
        pivot_wider(names_from = categoria, values_from = valor) %>%
        select(ano, Urbano, Rural, Total)
      x %>% reactable(
        defaultSorted = list(ano = "desc"),
        defaultPageSize = 10,
        striped = FALSE,
        highlight = TRUE,
        bordered = TRUE,
        outlined = TRUE,
        resizable = TRUE,
        showSortable = TRUE,
        pagination = F,
        columns = list(
          ano = colDef(name = "Ano")
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
    ## Tabela - Quantidade de benefícios emitidos no mês de dezembro----
    output$prev4_1txt3 <- renderText({
      t413()
    })
    output$prev4_1tab1 <- renderReactable({
      ris <- prev4_1 %>%
        filter(
          ano == input$prev4_1ano2,
          localidade == input$prev4_1muni
        ) %>%
        pull(ri)
      x <- prev4_1 %>% filter(
        ano == input$prev4_1ano2,
        localidade != "Pará"
      )
      x <- x %>%
        filter(ri == ris) %>%
        pivot_wider(names_from = categoria, values_from = valor) %>%
        select(ri, localidade, Urbano, Rural, Total)
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
          localidade = colDef(name = "Municípios")),
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
    
    ### 4.2-Valor dos benefícios emitidos no mês de dezembro (em R$)----
    ## Gráfico - Valor dos benefícios emitidos no mês de dezembro (em R$)----
    # Atualização da entrada
    prev4_2comp <- reactive({
      input$prev4_2muni
    })
    observeEvent(prev4_2comp(), {
      x <- prev4_2 %>% filter(
        localidade != "Pará"
      )
      x <- x %>% filter(localidade != prev4_2comp())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "prev4_2municomp",
        choices = c("Selecione um município", choices),
        session
      )
    })
    output$prev4_2txt1 <- renderText({
      t421()
    })
    output$prev4_2graf <- renderEcharts4r({
      req(input$prev4_2municomp)
      if (input$prev4_2municomp == "Selecione um município") {
        a <-
          prev4_2 %>% filter(
            localidade == input$prev4_2muni,
            ano == input$prev4_2ano,
            categoria != "Total"
          )
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            color = "#f17701",
            name = "Valor (R$)",
            legend = F,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            barWidth = "20%",
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 0, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Zona",
            nameLocation = "middle",
            nameTextStyle = list(
              fontWeight = "bold",
              padding = c(20, 0, 0, 0),
              fontSize = 14
            )
          ) %>%
          e_y_axis(
            name = "Valor",
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
          e_grid(show = T)
      } else {
        a <-
          prev4_2 %>% filter(
            localidade == input$prev4_2muni,
            ano == input$prev4_2ano,
            categoria != "Total"
          )
        b <-
          prev4_2 %>% filter(
            localidade == input$prev4_2municomp,
            ano == input$prev4_2ano,
            categoria != "Total"
          )
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            name = input$prev4_2muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            barWidth = "20%",
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, categoria) %>%
          e_bar(
            serie = valor,
            name = input$prev4_2municomp,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            barWidth = "20%",
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 0, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Zona",
            nameLocation = "middle",
            nameTextStyle = list(
              fontWeight = "bold",
              padding = c(20, 0, 0, 0),
              fontSize = 14
            )
          ) %>%
          e_y_axis(
            name = "Valor",
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
          e_grid(show = T)
      }
    })
    ## Tabela - Valor dos benefícios emitidos no mês de dezembro (em R$)----
    output$prev4_2txt2 <- renderText({
      t422()
    })
    output$prev4_2tab <- renderReactable({
      x <- prev4_2 %>%
        filter(
          localidade == input$prev4_2muni
        ) %>%
        pivot_wider(names_from = categoria, values_from = valor) %>%
        select(ano, Urbano, Rural, Total)
      x %>% reactable(
        defaultSorted = list(ano = "desc"),
        defaultPageSize = 10,
        striped = FALSE,
        highlight = TRUE,
        bordered = TRUE,
        outlined = TRUE,
        resizable = TRUE,
        showSortable = TRUE,
        pagination = F,
        columns = list(
          ano = colDef(name = "Ano")
        ),
        defaultColDef =
          colDef(
            footerStyle = list(fontWeight = "bold"),
            format = colFormat(separators = T, digits = 0),
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
    ## Tabela - Valor dos benefícios emitidos no mês de dezembro (em R$)----
    output$prev4_2txt3 <- renderText({
      t423()
    })
    output$prev4_2tab1 <- renderReactable({
      ris <- prev4_2 %>%
        filter(
          ano == input$prev4_2ano2,
          localidade == input$prev4_2muni
        ) %>%
        pull(ri)
      x <- prev4_2 %>% filter(
        ano == input$prev4_2ano2,
        localidade != "Pará"
      )
      x <- x %>%
        filter(ri == ris) %>%
        pivot_wider(names_from = categoria, values_from = valor) %>%
        select(ri, localidade, Urbano, Rural, Total)
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
          localidade = colDef(name = "Municípios")),
        defaultColDef =
          colDef(
            footerStyle = list(fontWeight = "bold"),
            format = colFormat(separators = T, digits = 0),
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
    
    ### 4.3 - Valor dos benefícios emitidos no ano (em R$)----
    
    ## Gráfico - Valor dos benefícios emitidos no ano (em R$)----
    # Atualização da entrada
    prev4_3comp <- reactive({
      input$prev4_3muni
    })
    observeEvent(prev4_3comp(), {
      x <- prev4_3 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != prev4_3comp())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "prev4_3municomp",
        choices = c("Selecione um município", choices),
        session
      )
    })
    
    ## Título
    output$prev4_3txt1 <- renderText({
      t431()
    })
    
    output$prev4_3graf <- renderEcharts4r({
      req(input$prev4_3municomp)
      if (input$prev4_3municomp == "Selecione um município") {
        a <- prev4_3 %>% filter(
          localidade == input$prev4_3muni,
          ano == input$prev4_3ano,
          categoria != "Total"
        )
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            color = "#f17701",
            name = "Valor (R$)",
            legend = F,
            symbol = "roundRect",
            symbolSize = 6,
            barWidth = "20%",
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
            name = "Zona",
            nameLocation = "middle",
            nameTextStyle = list(
              fontWeight = "bold",
              padding = c(20, 0, 0, 0),
              fontSize = 14
            )
          ) %>%
          e_y_axis(
            name = "Valor",
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
          e_grid(show = T)
      } else {
        a <- prev4_3 %>% filter(
          localidade == input$prev4_3muni,
          ano == input$prev4_3ano,
          categoria != "Total"
        )
        b <- prev4_3 %>% filter(
          localidade == input$prev4_3municomp,
          ano == input$prev4_3ano,
          categoria != "Total"
        )
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            name = input$prev4_3muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            barWidth = "20%",
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, categoria) %>%
          e_bar(
            serie = valor,
            name = input$prev4_3municomp,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            barWidth = "20%",
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
            name = "Zona",
            nameLocation = "middle",
            nameTextStyle = list(
              fontWeight = "bold",
              padding = c(20, 0, 0, 0),
              fontSize = 14
            )
          ) %>%
          e_y_axis(
            name = "Valor",
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
          e_grid(show = T)
      }
    })
    ## Tabela - Valor dos benefícios emitidos no ano (em R$)----
    output$prev4_3txt2 <- renderText({
      t432()
    })
    output$prev4_3tab <- renderReactable({
      x <- prev4_3 %>%
        filter(
          localidade == input$prev4_3muni
        ) %>%
        pivot_wider(names_from = categoria, values_from = valor) %>%
        select(ano, Urbano, Rural, Total)
      x %>% reactable(
        defaultSorted = list(ano = "desc"),
        defaultPageSize = 10,
        striped = FALSE,
        highlight = TRUE,
        bordered = TRUE,
        outlined = TRUE,
        resizable = TRUE,
        showSortable = TRUE,
        pagination = F,
        columns = list(
          ano = colDef(name = "Ano")
        ),
        defaultColDef =
          colDef(
            footerStyle = list(fontWeight = "bold"),
            format = colFormat(separators = T, digits = 0),
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
    ## Tabela - Valor dos benefícios emitidos no ano (em R$)----
    output$prev4_3txt3 <- renderText({
      t433()
    })
    output$prev4_3tab1 <- renderReactable({
      ris <- prev4_3 %>%
        filter(
          ano == input$prev4_3ano2,
          localidade == input$prev4_3muni
        ) %>%
        pull(ri)
      x <- prev4_3 %>% filter(
        ano == input$prev4_3ano2,
        localidade != "Pará"
      )
      x <- x %>%
        filter(ri == ris) %>%
        pivot_wider(names_from = categoria, values_from = valor) %>%
        select(ri, localidade, Urbano, Rural, Total)
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
          localidade = colDef(name = "Municípios")),
        defaultColDef =
          colDef(
            footerStyle = list(fontWeight = "bold"),
            format = colFormat(separators = T, digits = 0),
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
    #DOWNLOADS----
    # 1 - Quantidade de Benefícios Emitidos em Dezembro----
    ## Gráfico - Quantidade de Benefícios Emitidos em Dezembro----
    # Filtra os dados
    prev1_1 <- reactive({
      req(input$prev1municomp)
      if (input$prev1municomp == "Selecione um município") {
        a <- prev1 %>% filter(localidade == input$prev1muni)
      } else {
        a <- prev1 %>% filter(localidade == input$prev1muni)
        b <-
          prev1 %>% filter(localidade == input$prev1municomp)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(prev1_1(), {
      downset_Server("prev1_1", prev1_1(), t11())
    })
    ## Tabela - Quantidade de Benefícios Emitidos em Dezembro----
    # Filtra os dados
    prev1_2 <- reactive({
      ris <- prev1 %>%
        filter(ano == input$prev1ano, localidade == input$prev1muni) %>%
        pull(ri)
      x <- prev1 %>%
        filter(ano == input$prev1ano, localidade != "Pará", ri == ris) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(prev1_2(), {
      downset_Server("prev1_2", prev1_2(), t12())
    })
    
    # 2 - Valor dos Benefícios Emitidos em Dezembro----
    ## Gráfico - Valor dos Benefícios Emitidos em Dezembro----
    # Filtra os dados
    prev2_1 <- reactive({
      req(input$prev2municomp)
      if (input$prev2municomp == "Selecione um município") {
        a <- prev2 %>% filter(localidade == input$prev2muni)
      } else {
        a <- prev2 %>% filter(localidade == input$prev2muni)
        b <-
          prev2 %>% filter(localidade == input$prev2municomp)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(prev2_1(), {
      downset_Server("prev2_1", prev2_1(), t21())
    })
    ## Tabela - Valor dos Benefícios Emitidos em Dezembro----
    # Filtra os dados
    prev2_2 <- reactive({
      ris <- prev2 %>%
        filter(ano == input$prev2ano, localidade == input$prev2muni) %>%
        pull(ri)
      x <- prev2 %>%
        filter(ano == input$prev2ano, localidade != "Pará", ri == ris) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(prev2_2(), {
      downset_Server("prev2_2", prev2_2(), t22())
    })
    
    # 3 - Valor dos Benefícios Emitidos ao Ano (benefícios previdenciários, assistenciais e de legislação específica)----
    ## Gráfico - Valor dos Benefícios Emitidos ao Ano----
    # Filtra os dados
    prev3_1 <- reactive({
      req(input$prev3municomp)
      if (input$prev3municomp == "Selecione um município") {
        a <- prev3 %>% filter(localidade == input$prev3muni)
      } else {
        a <- prev3 %>% filter(localidade == input$prev3muni)
        b <-
          prev3 %>% filter(localidade == input$prev3municomp)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(prev3_1(), {
      downset_Server("prev3_1", prev3_1(), t31())
    })
    ## Tabela - Valor dos Benefícios Emitidos ao Ano----
    # Filtra os dados
    prev3_2 <- reactive({
      ris <- prev3 %>%
        filter(ano == input$prev3ano, localidade == input$prev3muni) %>%
        pull(ri)
      x <- prev3 %>%
        filter(ano == input$prev3ano, localidade != "Pará", ri == ris) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(prev3_2(), {
      downset_Server("prev3_2", prev3_2(), t32())
    })
    
    # 4 - Benefícios Emitidos pela Previdência Social por Localização----
    ### 4.1-Quantidade de benefícios emitidos no mês de dezembro----
    ## Gráfico - Quantidade de benefícios emitidos no mês de dezembro----
    # Filtra os dados
    prev4_1_1 <- reactive({
      req(input$prev4_1municomp)
      if (input$prev4_1municomp == "Selecione um município") {
        a <-
          prev4_1 %>% filter(
            localidade == input$prev4_1muni,
            ano == input$prev4_1ano,
            categoria != "Total"
          )
      } else {
        a <-
          prev4_1 %>% filter(
            localidade == input$prev4_1muni,
            ano == input$prev4_1ano,
            categoria != "Total"
          )
        b <-
          prev4_1 %>% filter(
            localidade == input$prev4_1municomp,
            ano == input$prev4_1ano,
            Variável == "Quantidade de benefícios emitidos no mês de dezembro",
            categoria != "Total"
          )
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(prev4_1_1(), {
      downset_Server("prev4_1_1", prev4_1_1(), t411())
    })
    ## Tabela - Quantidade de benefícios emitidos no mês de dezembro----
    # Filtra os dados
    prev4_1_2 <- reactive({
      x <- prev4_1 %>%
        filter(
          localidade == input$prev4_1muni
        )
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(prev4_1_2(), {
      downset_Server("prev4_1_2", prev4_1_2(), t412())
    })
    ## Tabela - Quantidade de benefícios emitidos no mês de dezembro----
    # Filtra os dados
    prev4_1_3 <- reactive({
      ris <- prev4_1 %>%
        filter(
          ano == input$prev4_1ano2,
          localidade == input$prev4_1muni
        ) %>%
        pull(ri)
      x <- prev4_1 %>% filter(
        ano == input$prev4_1ano2,
        localidade != "Pará",
        ri == ris
      )
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(prev4_1_3(), {
      downset_Server("prev4_1_3", prev4_1_3(), t413())
    })
    
    ### 4.2-Valor dos benefícios emitidos no mês de dezembro (em R$)----
    ## Gráfico - Valor dos benefícios emitidos no mês de dezembro (em R$)----
    # Filtra os dados
    prev4_2_1 <- reactive({
      req(input$prev4_2municomp)
      if (input$prev4_2municomp == "Selecione um município") {
        a <-
          prev4_2 %>% filter(
            localidade == input$prev4_2muni,
            ano == input$prev4_2ano,
            categoria != "Total"
          )
      } else {
        a <-
          prev4_2 %>% filter(
            localidade == input$prev4_2muni,
            ano == input$prev4_2ano,
            categoria != "Total"
          )
        b <-
          prev4_2 %>% filter(
            localidade == input$prev4_2municomp,
            ano == input$prev4_2ano,
            categoria != "Total"
          )
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(prev4_2_1(), {
      downset_Server("prev4_2_1", prev4_2_1(), t421())
    })
    ## Tabela - Valor dos benefícios emitidos no mês de dezembro (em R$)----
    # Filtra os dados
    prev4_2_2 <- reactive({
      x <- prev4_2 %>%
        filter(
          localidade == input$prev4_2muni
        )
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(prev4_2_2(), {
      downset_Server("prev4_2_2", prev4_2_2(), t422())
    })
    ## Tabela - Valor dos benefícios emitidos no mês de dezembro (em R$)----
    # Filtra os dados
    prev4_2_3 <- reactive({
      ris <- prev4_2 %>%
        filter(
          ano == input$prev4_2ano2,
          localidade == input$prev4_2muni
        ) %>%
        pull(ri)
      x <- prev4_2 %>% filter(
        ano == input$prev4_2ano2,
        localidade != "Pará",
        ri == ris
      )
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(prev4_2_3(), {
      downset_Server("prev4_2_3", prev4_2_3(), t423())
    })
    
    ### 4.3-Valor dos benefícios emitidos no ano (em R$)----
    ## Gráfico - Valor dos benefícios emitidos no ano (em R$)----
    # Filtra os dados
    prev4_3_1 <- reactive({
      req(input$prev4_3municomp)
      if (input$prev4_3municomp == "Selecione um município") {
        a <- prev4_3 %>% filter(
          localidade == input$prev4_3muni,
          ano == input$prev4_3ano,
          categoria != "Total"
        )
      } else {
        a <- prev4_3 %>% filter(
          localidade == input$prev4_3muni,
          ano == input$prev4_3ano,
          categoria != "Total"
        )
        b <- prev4_3 %>% filter(
          localidade == input$prev4_3municomp,
          ano == input$prev4_3ano,
          categoria != "Total"
        )
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(prev4_3_1(), {
      downset_Server("prev4_3_1", prev4_3_1(), t431())
    })
    ## Tabela - Valor dos benefícios emitidos no ano (em R$)----
    # Filtra os dados
    prev4_3_2 <- reactive({
      x <- prev4_3 %>%
        filter(
          localidade == input$prev4_3muni
        )
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(prev4_3_2(), {
      downset_Server("prev4_3_2", prev4_3_2(), t432())
    })
    ## Tabela - Valor dos benefícios emitidos no ano (em R$)----
    # Filtra os dados
    prev4_3_3 <- reactive({
      ris <- prev4_3 %>%
        filter(
          ano == input$prev4_3ano2,
          localidade == input$prev4_3muni
        ) %>%
        pull(ri)
      x <- prev4_3 %>% filter(
        ano == input$prev4_3ano2,
        localidade != "Pará",
        ri == ris
      )
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(prev4_3_3(), {
      downset_Server("prev4_3_3", prev4_3_3(), t433())
    })
  })
}

# Play do Módulo
# ui <- dashboardPage(header = dashboardHeader(),
#                     sidebar = dashboardSidebar(),
#                     body = dashboardBody(fluidPage(
#                       social_previdencia_mp_ui("social_previdencia_mp")
#                     )))
# 
# 
# server <- function(input, output) {
#   social_previdencia_mp_Server("social_previdencia_mp")
# }
# 
# shinyApp(ui, server)
