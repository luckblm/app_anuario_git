# Funções de módulo de Economia - Finanças Públicas - Municipal
# Função de UI
economia_fp_mp_ui <- function(id) {
  fluidPage(# CAMINHO DO ARQUIVO CSS----
            tags$head(
              tags$link(rel = "stylesheet", type = "text/css", href = "estilo.css")
            ),
            # Lista de Navegação lateral----
            div(class = "navbar_economia", navbarPage(
              tags$b("Finanças Públicas - Municípios"),
              navbarMenu(
                tags$b("Escolha um Indicador"),
                # 1 - Repasse de ICMS----
                tabPanel(
                  "Repasse de ICMS",
                  panel(
                    ## Controle----
                    heading =
                      h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;", "Repasse de ICMS"),
                    tags$div(
                      class = "seletor2",
                      pickerInput(
                        inputId = NS(id, "fp1muni"),
                        label = "Município",
                        choices = fp1 %>%
                          filter(localidade != "Pará") %>%
                          pull(localidade) %>%
                          unique(),
                        width = "200px"
                      )
                    )
                  ),
                  fluidRow(
                    ## Gráfico - Repasse de ICMS----
                    box(
                      title = textOutput(NS(id, "fp1txt1")),
                      collapsible = T,
                      collapsed = F,
                      headerBorder = T,
                      width = 12,
                      # Comparar municípios
                      pickerInput(
                        inputId = NS(id, "fp1municomp"),
                        label = "Comparar Município",
                        choices = NULL,
                        width = "200px",
                        options = list(`none-selected-text` = "Selecione um município")
                      ),
                      withSpinner(
                        echarts4rOutput(NS(id, "fp1graf")),
                        type = 8,
                        color = "#f2c94e",
                        size = 0.5
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "DITES-SEFA"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                        tags$h6(tags$b("Nota:"), "Valor - R$ 1,00 (valores correntes)")
                      ), downset_ui(NS(id, "fp1_1")))
                    )
                  ),
                  fluidRow(
                    ## Tabela - Repasse de ICMS----
                    box(
                      title = textOutput(NS(id, "fp1txt2")),
                      collapsible = T,
                      collapsed = F,
                      headerBorder = T,
                      width = 12,
                      pickerInput(
                        inputId = NS(id, "fp1ano"),
                        label = "Ano",
                        choices = sort(unique(fp1[["ano"]]), decreasing = T),
                        width = "100px"
                      ),
                      withSpinner(
                        reactableOutput(NS(id, "fp1tab"), height = "400px"),
                        type = 8,
                        color = "#f2c94e",
                        size = 0.5
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "DITES-SEFA"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                        tags$h6(tags$b("Nota:"), "Valor - R$ 1,00 (valores correntes)")
                      ), downset_ui(NS(id, "fp1_2")))
                    )
                  )
                ),
                
                # 2 - Demonstrativo dos Índices de Participação na Arrecadação do ICMS----
                tabPanel(
                  "Demonstrativo dos Índices de Participação na Arrecadação do ICMS",
                  panel(
                    ## Controle----
                    heading =
                      h4(
                        style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                        "Demonstrativo dos Índices de Participação na Arrecadação do ICMS"
                      ),
                    tags$div(
                      class = "seletor2",
                      pickerInput(
                        inputId = NS(id, "fp2muni"),
                        label = "Município",
                        choices = fp2 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                        width = "200px"
                      )
                    )
                  ),
                  fluidRow(
                    ## Gráfico - Demonstrativo dos Índices de Participação na Arrecadação do ICMS----
                    box(
                      title = textOutput(NS(id, "fp2txt1")),
                      collapsible = T,
                      collapsed = F,
                      headerBorder = T,
                      width = 12,
                      # Comparar municípios
                      pickerInput(
                        inputId = NS(id, "fp2municomp"),
                        label = "Comparar Município",
                        choices = NULL,
                        width = "200px",
                        options = list(`none-selected-text` = "Selecione um município")
                      ),
                      withSpinner(
                        echarts4rOutput(NS(id, "fp2graf")),
                        type = 8,
                        color = "#f2c94e",
                        size = 0.5
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "DITES-SEFA"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA")
                      ), downset_ui(NS(id, "fp2_1")))
                    )
                  ),
                  fluidRow(
                    ## Tabela - Demonstrativo dos Índices de Participação na Arrecadação do ICMS----
                    box(
                      title = textOutput(NS(id, "fp2txt2")),
                      collapsible = T,
                      collapsed = F,
                      headerBorder = T,
                      width = 12,
                      pickerInput(
                        inputId = NS(id, "fp2ano"),
                        label = "Ano",
                        choices = sort(unique(fp2[["ano"]]), decreasing = T),
                        width = "100px"
                      ),
                      withSpinner(
                        reactableOutput(NS(id, "fp2tab"), height = "400px"),
                        type = 8,
                        color = "#f2c94e",
                        size = 0.5
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "DITES-SEFA"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA")
                      ), downset_ui(NS(id, "fp2_2")))
                    )
                  )
                ),
                
                # 3 - Repasse de IPI dos Municípios----
                tabPanel(
                  "Repasse de IPI",
                  panel(
                    ## Controle----
                    heading =
                      h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;", "Repasse de IPI"),
                    tags$div(
                      class = "seletor2",
                      pickerInput(
                        inputId = NS(id, "fp3muni"),
                        label = "Município",
                        choices = fp3 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                        width = "200px"
                      )
                    )
                  ),
                  fluidRow(
                    ## Gráfico - Repasse de IPI dos Municípios----
                    box(
                      title = textOutput(NS(id, "fp3txt1")),
                      collapsible = T,
                      collapsed = F,
                      headerBorder = T,
                      width = 12,
                      # Comparar municípios
                      pickerInput(
                        inputId = NS(id, "fp3municomp"),
                        label = "Comparar Município",
                        choices = NULL,
                        width = "200px",
                        options = list(`none-selected-text` = "Selecione um município")
                      ),
                      withSpinner(
                        echarts4rOutput(NS(id, "fp3graf")),
                        type = 8,
                        color = "#f2c94e",
                        size = 0.5
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "DITES-SEFA"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                        tags$h6(tags$b("Nota:"), "Valor - R$ 1,00 (valores correntes)")
                      ), downset_ui(NS(id, "fp3_1")))
                    )
                  ),
                  fluidRow(
                    ## Tabela - Repasse de IPI dos Municípios----
                    box(
                      title = textOutput(NS(id, "fp3txt2")),
                      collapsible = T,
                      collapsed = F,
                      headerBorder = T,
                      width = 12,
                      pickerInput(
                        inputId = NS(id, "fp3ano"),
                        label = "Ano",
                        choices = sort(unique(fp3[["ano"]]), decreasing = T),
                        width = "100px"
                      ),
                      withSpinner(
                        reactableOutput(NS(id, "fp3tab"), height = "400px"),
                        type = 8,
                        color = "#f2c94e",
                        size = 0.5
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "DITES-SEFA"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                        tags$h6(tags$b("Nota:"), "Valor - R$ 1,00 (valores correntes)")
                      ), downset_ui(NS(id, "fp3_2")))
                    )
                  )
                ),
                
                # 4 - Repasse de IPVA  dos Municípios----
                tabPanel(
                  "Repasse de IPVA",
                  panel(
                    ## Controle----
                    heading =
                      h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;", "Repasse de IPVA"),
                    tags$div(
                      class = "seletor2",
                      pickerInput(
                        inputId = NS(id, "fp4muni"),
                        label = "Município",
                        choices = fp4 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                        width = "200px"
                      )
                    )
                  ),
                  fluidRow(
                    ## Gráfico - Repasse de IPVA  dos Municípios----
                    box(
                      title = textOutput(NS(id, "fp4txt1")),
                      collapsible = T,
                      collapsed = F,
                      headerBorder = T,
                      width = 12,
                      # Comparar municípios
                      pickerInput(
                        inputId = NS(id, "fp4municomp"),
                        label = "Comparar Município",
                        choices = NULL,
                        width = "200px",
                        options = list(`none-selected-text` = "Selecione um município")
                      ),
                      withSpinner(
                        echarts4rOutput(NS(id, "fp4graf")),
                        type = 8,
                        color = "#f2c94e",
                        size = 0.5
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "DITES-SEFA"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                        tags$h6(tags$b("Nota:"), "Valor - R$ 1,00 (valores correntes)")
                      ), downset_ui(NS(id, "fp4_1")))
                    )
                  ),
                  fluidRow(
                    ## Tabela - Repasse de IPVA  dos Municípios----
                    box(
                      title = textOutput(NS(id, "fp4txt2")),
                      collapsible = T,
                      collapsed = F,
                      headerBorder = T,
                      width = 12,
                      pickerInput(
                        inputId = NS(id, "fp4ano"),
                        label = "Ano",
                        choices = sort(unique(fp4[["ano"]]), decreasing = T),
                        width = "100px"
                      ),
                      withSpinner(
                        reactableOutput(NS(id, "fp4tab"), height = "400px"),
                        type = 8,
                        color = "#f2c94e",
                        size = 0.5
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "DITES-SEFA"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                        tags$h6(tags$b("Nota:"), "Valor - R$ 1,00 (valores correntes)")
                      ), downset_ui(NS(id, "fp4_2")))
                    )
                  )
                ),
                
                # 5 - Receita Orçamentária----
                tabPanel(
                  "Receita Orçamentária",
                  panel(
                    ## Controle----
                    heading =
                      h4(
                        style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                        "Receita Orçamentária"
                      ),
                    tags$div(
                      class = "seletor2",
                      pickerInput(
                        inputId = NS(id, "fp5muni"),
                        label = "Município",
                        choices = fp5 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                        width = "200px"
                      )
                    )
                  ),
                  fluidRow(
                    ## Gráfico - Receita Orçamentária----
                    box(
                      title = textOutput(NS(id, "fp5txt1")),
                      collapsible = T,
                      collapsed = F,
                      headerBorder = T,
                      width = 12,
                      # Comparar municípios
                      pickerInput(
                        inputId = NS(id, "fp5municomp"),
                        label = "Comparar Município",
                        choices = NULL,
                        width = "200px",
                        options = list(`none-selected-text` = "Selecione um município")
                      ),
                      withSpinner(
                        echarts4rOutput(NS(id, "fp5graf")),
                        type = 8,
                        color = "#f2c94e",
                        size = 0.5
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "STN-FINBRA"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                        tags$h6(tags$b("Nota:"), "Valor - R$ 1,00 (valores correntes)")
                      ), downset_ui(NS(id, "fp5_1")))
                    )
                  ),
                  fluidRow(
                    ## Tabela - Receita Orçamentária----
                    box(
                      title = textOutput(NS(id, "fp5txt2")),
                      collapsible = T,
                      collapsed = F,
                      headerBorder = T,
                      width = 12,
                      pickerInput(
                        inputId = NS(id, "fp5ano"),
                        label = "Ano",
                        choices = sort(unique(fp5[["ano"]]), decreasing = T),
                        width = "100px"
                      ),
                      withSpinner(
                        reactableOutput(NS(id, "fp5tab"), height = "400px"),
                        type = 8,
                        color = "#f2c94e",
                        size = 0.5
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "STN-FINBRA"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                        tags$h6(tags$b("Nota:"), "Valor - R$ 1,00 (valores correntes)")
                      ), downset_ui(NS(id, "fp5_2")))
                    )
                  )
                ),
                
                # 6 - Receitas Correntes----
                tabPanel(
                  "Receitas Correntes",
                  panel(
                    ## Controle----
                    heading =
                      h4(
                        style = "margin-bottom: 2px;margin-top: 2px;
                                 font-weight: bold;color: #6D6D6D;",
                        "Receitas Correntes"
                      ),
                    tags$div(
                      class = "seletor2",
                      pickerInput(
                        inputId = NS(id, "fp6muni"),
                        label = "Município",
                        choices = fp6 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                        width = "200px"
                      )
                    )
                  ),
                  fluidRow(
                    ## Gráfico - Receitas Correntes----
                    box(
                      title = textOutput(NS(id, "fp6txt1")),
                      collapsible = T,
                      collapsed = F,
                      headerBorder = T,
                      width = 12,
                      # Comparar municípios
                      pickerInput(
                        inputId = NS(id, "fp6municomp"),
                        label = "Comparar Município",
                        choices = NULL,
                        width = "200px",
                        options = list(`none-selected-text` = "Selecione um município")
                      ),
                      withSpinner(
                        echarts4rOutput(NS(id, "fp6graf")),
                        type = 8,
                        color = "#f2c94e",
                        size = 0.5
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "STN-FINBRA"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                        tags$h6(tags$b("Nota:"), "Valor - R$ 1,00 (valores correntes)")
                      ), downset_ui(NS(id, "fp6_1")))
                    )
                  ),
                  fluidRow(
                    ## Tabela - Receitas Correntes----
                    box(
                      title = textOutput(NS(id, "fp6txt2")),
                      collapsible = T,
                      collapsed = F,
                      headerBorder = T,
                      width = 12,
                      pickerInput(
                        inputId = NS(id, "fp6ano"),
                        label = "Ano",
                        choices = sort(unique(fp6[["ano"]]), decreasing = T),
                        width = "100px"
                      ),
                      withSpinner(
                        reactableOutput(NS(id, "fp6tab"), height = "400px"),
                        type = 8,
                        color = "#f2c94e",
                        size = 0.5
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "STN-FINBRA"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                        tags$h6(tags$b("Nota:"), "Valor - R$ 1,00 (valores correntes)")
                      ), downset_ui(NS(id, "fp6_2")))
                    )
                  )
                ),
                
                # 7 - Impostos----
                tabPanel(
                  "Impostos",
                  panel(
                    ## Controle----
                    heading =
                      h4(style = "margin-bottom: 2px;margin-top: 2px;
                                 font-weight: bold;color: #6D6D6D;", "Impostos"),
                    tags$div(
                      class = "seletor2",
                      pickerInput(
                        inputId = NS(id, "fp7muni"),
                        label = "Município",
                        choices = fp7 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                        width = "200px"
                      )
                    )
                  ),
                  fluidRow(
                    ## Gráfico - Impostos----
                    box(
                      title = textOutput(NS(id, "fp7txt1")),
                      collapsible = T,
                      collapsed = F,
                      headerBorder = T,
                      width = 12,
                      # Comparar municípios
                      pickerInput(
                        inputId = NS(id, "fp7municomp"),
                        label = "Comparar Município",
                        choices = NULL,
                        width = "200px",
                        options = list(`none-selected-text` = "Selecione um município")
                      ),
                      withSpinner(
                        echarts4rOutput(NS(id, "fp7graf")),
                        type = 8,
                        color = "#f2c94e",
                        size = 0.5
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "STN-FINBRA"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                        tags$h6(tags$b("Nota:"), "Valor - R$ 1,00 (valores correntes)")
                      ), downset_ui(NS(id, "fp7_1")))
                    )
                  ),
                  fluidRow(
                    ## Tabela - Impostos----
                    box(
                      title = textOutput(NS(id, "fp7txt2")),
                      collapsible = T,
                      collapsed = F,
                      headerBorder = T,
                      width = 12,
                      pickerInput(
                        inputId = NS(id, "fp7ano"),
                        label = "Ano",
                        choices = sort(unique(fp7[["ano"]]), decreasing = T),
                        width = "100px"
                      ),
                      withSpinner(
                        reactableOutput(NS(id, "fp7tab"), height = "400px"),
                        type = 8,
                        color = "#f2c94e",
                        size = 0.5
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "STN-FINBRA"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                        tags$h6(tags$b("Nota:"), "Valor - R$ 1,00 (valores correntes)")
                      ), downset_ui(NS(id, "fp7_2")))
                    )
                  )
                ),
                
                # 8 - Receita de Transferências Correntes----
                tabPanel(
                  "Receita de Transferências Correntes",
                  panel(
                    ## Controle----
                    heading =
                      h4(
                        style = "margin-bottom: 2px;margin-top: 2px;
                             font-weight: bold;color: #6D6D6D;",
                        "Receita de Transferências Correntes"
                      ),
                    tags$div(
                      class = "seletor2",
                      pickerInput(
                        inputId = NS(id, "fp8muni"),
                        label = "Município",
                        choices = fp8 %>% filter(localidade != "Pará") %>%
                          pull(localidade) %>% unique(),
                        width = "200px"
                      )
                    )
                  ),
                  fluidRow(
                    ## Gráfico - Receita de Transferências Correntes----
                    box(
                      title = textOutput(NS(id, "fp8txt1")),
                      collapsible = T,
                      collapsed = F,
                      headerBorder = T,
                      width = 12,
                      # Comparar municípios
                      pickerInput(
                        inputId = NS(id, "fp8municomp"),
                        label = "Comparar Município",
                        choices = NULL,
                        width = "200px",
                        options = list(`none-selected-text` = "Selecione um município")
                      ),
                      withSpinner(
                        echarts4rOutput(NS(id, "fp8graf")),
                        type = 8,
                        color = "#f2c94e",
                        size = 0.5
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "STN-FINBRA"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                        tags$h6(tags$b("Nota:"), "Valor - R$ 1,00 (valores correntes)")
                      ), downset_ui(NS(id, "fp8_1")))
                    )
                  ),
                  fluidRow(
                    ## Tabela - Receita de Transferências Correntes----
                    box(
                      title = textOutput(NS(id, "fp8txt2")),
                      collapsible = T,
                      collapsed = F,
                      headerBorder = T,
                      width = 12,
                      pickerInput(
                        inputId = NS(id, "fp8ano"),
                        label = "Ano",
                        choices = sort(unique(fp8[["ano"]]), decreasing = T),
                        width = "100px"
                      ),
                      withSpinner(
                        reactableOutput(NS(id, "fp8tab"), height = "400px"),
                        type = 8,
                        color = "#f2c94e",
                        size = 0.5
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "STN-FINBRA"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                        tags$h6(tags$b("Nota:"), "Valor - R$ 1,00 (valores correntes)")
                      ), downset_ui(NS(id, "fp8_2")))
                    )
                  )
                )
              )
            )))
}
# Função do modulo servidor
economia_fp_mp_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    #TÍTULOS----
    # 1 - Repasse de ICMS----
    ## Gráfico - Repasse de ICMS----
    t11 <- reactive({
      req(input$fp1municomp)
      if (input$fp1municomp == "Selecione um município") {
        paste0("Repasse de ICMS, ",
               input$fp1muni,
               " - ",
               min(fp1$ano),
               " a ",
               max(fp1$ano))
      } else {
        paste0(
          "Repasse de ICMS, ",
          input$fp1muni,
          " x ",
          input$fp1municomp,
          " - ",
          min(fp1$ano),
          " a ",
          max(fp1$ano)
        )
      }
    })
    ## Tabela - Repasse de ICMS da mesma Região de Integração----
    t12 <- reactive({
      ri <- fp1 %>%
        filter(ano == input$fp1ano, localidade == input$fp1muni) %>%
        pull(ri)
      paste0("Repasse de ICMS por Município, Região de Integração ",
             ri,
             " - ",
             input$fp1ano)
    })
    # 2 - Demonstrativo dos Índices de Participação na Arrecadação do ICMS----
    ## Gráfico - Demonstrativo dos Índices de Participação na Arrecadação do ICMS----
    
    t21 <- reactive({
      req(input$fp2municomp)
      if (input$fp2municomp == "Selecione um município") {
        paste0(
          "Índices de Participação na Arrecadação do ICMS, ",
          input$fp2muni,
          " - ",
          min(fp2$ano),
          " a ",
          max(fp2$ano)
        )
      } else {
        paste0(
          "Índices de Participação na Arrecadação do ICMS, ",
          input$fp2muni,
          " x ",
          input$fp2municomp,
          " - ",
          min(fp2$ano),
          " a ",
          max(fp2$ano)
        )
      }
    })
    
    ## Tabela - Demonstrativo dos Índices de Participação na Arrecadação do ICMS da mesma Região de Integração----
    t22 <- reactive({
      ri <- fp2 %>%
        filter(ano == input$fp2ano, localidade == input$fp2muni) %>%
        pull(ri)
      paste0(
        "Índices de Participação na Arrecadação do ICMS dos Municípios, Região de Integração ",
        ri,
        " - ",
        input$fp2ano
      )
    })
    # 3 - Repasse de IPI----
    ## Gráfico - Repasse de IPI----
    t31 <- reactive({
      req(input$fp3municomp)
      if (input$fp3municomp == "Selecione um município") {
        paste0("Repasse de IPI, ",
               input$fp3muni,
               " - ",
               min(fp3$ano),
               " a ",
               max(fp3$ano))
      } else {
        paste0(
          "Repasse de IPI, ",
          input$fp3muni,
          " x ",
          input$fp3municomp,
          " - ",
          min(fp3$ano),
          " a ",
          max(fp3$ano)
        )
      }
    })
    ## Tabela - Repasse de IPI da mesma Região de Integração----
    t32 <- reactive({
      ri <- fp3 %>%
        filter(ano == input$fp3ano, localidade == input$fp3muni) %>%
        pull(ri)
      paste0("Repasse de IPI por Município, Região de Integração ",
             ri,
             " - ",
             input$fp3ano)
    })
    # 4 - Repasse de IPVA----
    ## Gráfico - Repasse de IPVA----
    t41 <- reactive({
      req(input$fp4municomp)
      if (input$fp4municomp == "Selecione um município") {
        paste0("Repasse de IPVA, ",
               input$fp4muni,
               " - ",
               min(fp4$ano),
               " a ",
               max(fp4$ano))
      } else {
        paste0(
          "Repasse de IPVA, ",
          input$fp4muni,
          " x ",
          input$fp4municomp,
          " - ",
          min(fp4$ano),
          " a ",
          max(fp4$ano)
        )
      }
    })
    ## Tabela - Repasse de IPVA da mesma Região de Integração----
    t42 <- reactive({
      ri <- fp4 %>%
        filter(ano == input$fp4ano, localidade == input$fp4muni) %>%
        pull(ri)
      paste0("Repasse de IPVA por Município, Região de Integração ",
             ri,
             " - ",
             input$fp4ano)
    })
    # 5 - Receita Orçamentária----
    ## Gráfico - Receita Orçamentária----
    t51 <- reactive({
      req(input$fp5municomp)
      if (input$fp5municomp == "Selecione um município") {
        paste0("Receita Orçamentária, ",
               input$fp5muni,
               " - ",
               min(fp5$ano),
               " a ",
               max(fp5$ano))
      } else {
        paste0(
          "Receita Orçamentária, ",
          input$fp5muni,
          " x ",
          input$fp5municomp,
          " - ",
          min(fp5$ano),
          " a ",
          max(fp5$ano)
        )
      }
    })
    ## Tabela - Receita Orçamentária da mesma Região de Integração----
    t52 <- reactive({
      ri <- fp5 %>%
        filter(ano == input$fp5ano, localidade == input$fp5muni) %>%
        pull(ri)
      paste0(
        "Receita Orçamentária dos Municípios, Região de Integração ",
        ri,
        " - ",
        input$fp5ano
      )
    })
    # 6 - Receitas Correntes----
    ## Gráfico - Receitas Correntes----
    t61 <- reactive({
      req(input$fp6municomp)
      if (input$fp6municomp == "Selecione um município") {
        paste0("Receitas Correntes, ",
               input$fp6muni,
               " - ",
               min(fp6$ano),
               " a ",
               max(fp6$ano))
      } else {
        paste0(
          "Receitas Correntes, ",
          input$fp6muni,
          " x ",
          input$fp6municomp,
          " - ",
          min(fp6$ano),
          " a ",
          max(fp6$ano)
        )
      }
    })
    ## Tabela - Receitas Correntes da mesma Região de Integração----
    t62 <- reactive({
      ri <- fp6 %>%
        filter(ano == input$fp6ano, localidade == input$fp6muni) %>%
        pull(ri)
      paste0("Receitas Correntes dos Municipios, Região de Integração ",
             ri,
             " - ",
             input$fp6ano)
    })
    # 7 - Impostos----
    ## Gráfico - Impostos----
    t71 <- reactive({
      req(input$fp7municomp)
      if (input$fp7municomp == "Selecione um município") {
        paste0("Impostos, ",
               input$fp7muni,
               " - ",
               min(fp7$ano),
               " a ",
               max(fp7$ano))
      } else {
        paste0(
          "Impostos, ",
          input$fp7muni,
          " x ",
          input$fp7municomp,
          " - ",
          min(fp7$ano),
          " a ",
          max(fp7$ano)
        )
      }
    })
    ## Tabela - Impostos da mesma Região de Integração----
    t72 <- reactive({
      ri <- fp7 %>%
        filter(ano == input$fp7ano, localidade == input$fp7muni) %>%
        pull(ri)
      paste0("Impostos dos Municípios, Região de Integração ",
             ri,
             " - ",
             input$fp7ano)
    })
    # 8 - Receita de Transferências Correntes----
    ## Gráfico - Receita de Transferências Correntes----
    
    t81 <- reactive({
      req(input$fp8municomp)
      if (input$fp8municomp == "Selecione um município") {
        paste0(
          "Receita de Transferências Correntes, ",
          input$fp8muni,
          " - ",
          min(fp8$ano),
          " a ",
          max(fp8$ano)
        )
      } else {
        paste0(
          "Receita de Transferências Correntes, ",
          input$fp8muni,
          " x ",
          input$fp8municomp,
          " - ",
          min(fp8$ano),
          " a ",
          max(fp8$ano)
        )
      }
    })
    
    ## Tabela - Receita de Transferências Correntes da mesma Região de Integração----
    t82 <- reactive({
      ri <- fp8 %>%
        filter(ano == input$fp8ano, localidade == input$fp8muni) %>%
        pull(ri)
      paste0(
        "Receita de Transferências Correntes dos Municípios, Região de Integração ",
        ri,
        " - ",
        input$fp8ano
      )
    })
    #VISUALIZAÇÃO----
    # 1 - Repasse de ICMS----
    ## Gráfico - Repasse de ICMS----
    # Atualização da entrada
    fp1comp <- reactive({
      input$fp1muni
    })
    observeEvent(fp1comp(), {
      x <- fp1 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != fp1comp())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "fp1municomp",
        choices = c("Selecione um município", choices),
        session
      )
    })
    
    output$fp1txt1 <- renderText({
      t11()
    })
    
    output$fp1graf <- renderEcharts4r({
      req(input$fp1municomp)
      if (input$fp1municomp == "Selecione um município") {
        a <- fp1 %>% filter(localidade == input$fp1muni)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f2c94e",
            name = "Valor (R$)",
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
      } else {
        a <- fp1 %>% filter(localidade == input$fp1muni)
        b <- fp1 %>% filter(localidade == input$fp1municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$fp1muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$fp1municomp,
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
    ## Tabela - Repasse de ICMS da mesma Região de Integração----
    output$fp1txt2 <- renderText({
      t12()
    })
    output$fp1tab <- renderReactable({
      ris <- fp1 %>%
        filter(ano == input$fp1ano, localidade == input$fp1muni) %>%
        pull(ri)
      x <-
        fp1 %>% filter(ano == input$fp1ano, localidade != "Pará")
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
            name = "Valor(R$)",
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
    # 2 - Demonstrativo dos Índices de Participação na Arrecadação do ICMS----
    ## Gráfico - Demonstrativo dos Índices de Participação na Arrecadação do ICMS----
    # Atualização da entrada
    fp2comp <- reactive({
      input$fp2muni
    })
    observeEvent(fp2comp(), {
      x <- fp2 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != fp2comp())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "fp2municomp",
        choices = c("Selecione um município", choices),
        session
      )
    })
    
    output$fp2txt1 <- renderText({
      t21()
    })
    
    output$fp2graf <- renderEcharts4r({
      req(input$fp2municomp)
      if (input$fp2municomp == "Selecione um município") {
        a <- fp2 %>% filter(localidade == input$fp2muni)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f2c94e",
            name = "Índice",
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
            name = "Índice",
            nameTextStyle = list(fontWeight = "bold", fontSize = 14),
            scale = T,
            axisLabel = list(
              formatter = htmlwidgets::JS(
                "
              function (value, index) {
              return value.toLocaleString('pt-BR', { minimumFractionDigits: 2, maximumFractionDigits: 2 });
              }
            "
              )
            )
          ) %>%
          e_locale("pt-Br") %>%
          e_datazoom(toolbox = F, fillerColor = "#E5F5F9") %>%
          e_grid(show = T)
      } else {
        a <- fp2 %>% filter(localidade == input$fp2muni)
        b <- fp2 %>% filter(localidade == input$fp2municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$fp2muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$fp2municomp,
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
    ## Tabela - Demonstrativo dos Índices de Participação na Arrecadação do ICMS da mesma Região de Integração----
    output$fp2txt2 <- renderText({
      t22()
    })
    output$fp2tab <- renderReactable({
      ris <- fp2 %>%
        filter(ano == input$fp2ano, localidade == input$fp2muni) %>%
        pull(ri)
      x <-
        fp2 %>% filter(ano == input$fp2ano, localidade != "Pará")
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
            name = "Índice",
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
              number_fmt = scales::number_format(
                big.mark = ".",
                decimal.mark = ",",
                accuracy = 0.01
              )
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
    # 3 - Repasse de IPI----
    ## Gráfico - Repasse de IPI----
    # Atualização da entrada
    fp3comp <- reactive({
      input$fp3muni
    })
    observeEvent(fp3comp(), {
      x <- fp3 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != fp3comp())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "fp3municomp",
        choices = c("Selecione um município", choices),
        session
      )
    })
    
    output$fp3txt1 <- renderText({
      t31()
    })
    
    output$fp3graf <- renderEcharts4r({
      req(input$fp3municomp)
      if (input$fp3municomp == "Selecione um município") {
        a <- fp3 %>% filter(localidade == input$fp3muni)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f2c94e",
            name = "Valor(R$)",
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
        a <- fp3 %>% filter(localidade == input$fp3muni)
        b <- fp3 %>% filter(localidade == input$fp3municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$fp3muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$fp3municomp,
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
    ## Tabela - Repasse de IPI da mesma Região de Integração----
    output$fp3txt2 <- renderText({
      t32()
    })
    output$fp3tab <- renderReactable({
      ris <- fp3 %>%
        filter(ano == input$fp3ano, localidade == input$fp3muni) %>%
        pull(ri)
      x <-
        fp3 %>% filter(ano == input$fp3ano, localidade != "Pará")
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
            name = "Valor(R$)",
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
    # 4 - Repasse de IPVA----
    ## Gráfico - Repasse de IPVA----
    # Atualização da entrada
    fp4comp <- reactive({
      input$fp4muni
    })
    observeEvent(fp4comp(), {
      x <- fp4 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != fp4comp())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "fp4municomp",
        choices = c("Selecione um município", choices),
        session
      )
    })
    
    output$fp4txt1 <- renderText({
      t41()
    })
    
    output$fp4graf <- renderEcharts4r({
      req(input$fp4municomp)
      if (input$fp4municomp == "Selecione um município") {
        a <- fp4 %>% filter(localidade == input$fp4muni)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f2c94e",
            name = "Valor(R$)",
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
        a <- fp4 %>% filter(localidade == input$fp4muni)
        b <- fp4 %>% filter(localidade == input$fp4municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$fp4muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$fp4municomp,
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
    ## Tabela - Repasse de IPVA da mesma Região de Integração----
    output$fp4txt2 <- renderText({
      t42()
    })
    output$fp4tab <- renderReactable({
      ris <- fp4 %>%
        filter(ano == input$fp4ano, localidade == input$fp4muni) %>%
        pull(ri)
      x <-
        fp4 %>% filter(ano == input$fp4ano, localidade != "Pará")
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
            name = "Valor(R$)",
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
    # 5 - Receita Orçamentária----
    ## Gráfico - Receita Orçamentária----
    # Atualização da entrada
    fp5comp <- reactive({
      input$fp5muni
    })
    observeEvent(fp5comp(), {
      x <- fp5 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != fp5comp())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "fp5municomp",
        choices = c("Selecione um município", choices),
        session
      )
    })
    
    output$fp5txt1 <- renderText({
      req(input$fp5municomp)
      if (input$fp5municomp == "Selecione um município") {
        paste0(
          "Gráfico - Receita Orçamentária - ",
          input$fp5muni,
          " - ",
          min(fp5$ano),
          " a ",
          max(fp5$ano)
        )
      } else {
        paste0(
          "Gráfico - Receita Orçamentária - ",
          input$fp5muni,
          " x ",
          input$fp5municomp,
          " - ",
          min(fp5$ano),
          " a ",
          max(fp5$ano)
        )
      }
    })
    
    output$fp5graf <- renderEcharts4r({
      req(input$fp5municomp)
      if (input$fp5municomp == "Selecione um município") {
        a <- fp5 %>% filter(localidade == input$fp5muni)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f2c94e",
            name = "Valor(R$)",
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
        a <- fp5 %>% filter(localidade == input$fp5muni)
        b <- fp5 %>% filter(localidade == input$fp5municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$fp5muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$fp5municomp,
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
    ## Tabela - Receita Orçamentária da mesma Região de Integração----
    output$fp5txt2 <- renderText({
      t52()
    })
    output$fp5tab <- renderReactable({
      ris <- fp5 %>%
        filter(ano == input$fp5ano, localidade == input$fp5muni) %>%
        pull(ri)
      x <-
        fp5 %>% filter(ano == input$fp5ano, localidade != "Pará")
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
            name = "Valor(R$)",
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
    # 6 - Receitas Correntes----
    ## Gráfico - Receitas Correntes----
    # Atualização da entrada
    fp6comp <- reactive({
      input$fp6muni
    })
    observeEvent(fp6comp(), {
      x <- fp6 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != fp6comp())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "fp6municomp",
        choices = c("Selecione um município", choices),
        session
      )
    })
    
    output$fp6txt1 <- renderText({
      req(input$fp6municomp)
      if (input$fp6municomp == "Selecione um município") {
        paste0(
          "Gráfico - Receitas Correntes - ",
          input$fp6muni,
          " - ",
          min(fp6$ano),
          " a ",
          max(fp6$ano)
        )
      } else {
        paste0(
          "Gráfico - Receitas Correntes - ",
          input$fp6muni,
          " x ",
          input$fp6municomp,
          " - ",
          min(fp6$ano),
          " a ",
          max(fp6$ano)
        )
      }
    })
    
    output$fp6graf <- renderEcharts4r({
      req(input$fp6municomp)
      if (input$fp6municomp == "Selecione um município") {
        a <- fp6 %>% filter(localidade == input$fp6muni)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f2c94e",
            name = "Valor(R$)",
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
        a <- fp6 %>% filter(localidade == input$fp6muni)
        b <- fp6 %>% filter(localidade == input$fp6municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$fp6muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$fp6municomp,
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
    ## Tabela - Receitas Correntes da mesma Região de Integração----
    output$fp6txt2 <- renderText({
      t62()
    })
    output$fp6tab <- renderReactable({
      ris <- fp6 %>%
        filter(ano == input$fp6ano, localidade == input$fp6muni) %>%
        pull(ri)
      x <-
        fp6 %>% filter(ano == input$fp6ano, localidade != "Pará")
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
            name = "Valor (R$))",
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
    # 7 - Impostos----
    ## Gráfico - Impostos----
    # Atualização da entrada
    fp7comp <- reactive({
      input$fp7muni
    })
    observeEvent(fp7comp(), {
      x <- fp7 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != fp7comp())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "fp7municomp",
        choices = c("Selecione um município", choices),
        session
      )
    })
    
    output$fp7txt1 <- renderText({
      req(input$fp7municomp)
      if (input$fp7municomp == "Selecione um município") {
        paste0("Gráfico - Impostos - ",
               input$fp7muni,
               " - ",
               min(fp7$ano),
               " a ",
               max(fp7$ano))
      } else {
        paste0(
          "Impostos - ",
          input$fp7muni,
          " x ",
          input$fp7municomp,
          " - ",
          min(fp7$ano),
          " a ",
          max(fp7$ano)
        )
      }
    })
    
    output$fp7graf <- renderEcharts4r({
      req(input$fp7municomp)
      if (input$fp7municomp == "Selecione um município") {
        a <- fp7 %>% filter(localidade == input$fp7muni)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f2c94e",
            name = "Valor(R$)",
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
        a <- fp7 %>% filter(localidade == input$fp7muni)
        b <- fp7 %>% filter(localidade == input$fp7municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$fp7muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$fp7municomp,
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
    ## Tabela - Impostos da mesma Região de Integração----
    output$fp7txt2 <- renderText({
      t72()
    })
    output$fp7tab <- renderReactable({
      ris <- fp7 %>%
        filter(ano == input$fp7ano, localidade == input$fp7muni) %>%
        pull(ri)
      x <-
        fp7 %>% filter(ano == input$fp7ano, localidade != "Pará")
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
            name = "Valor (R$)",
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
    # 8 - Receita de Transferências Correntes----
    ## Gráfico - Receita de Transferências Correntes----
    # Atualização da entrada
    fp8comp <- reactive({
      input$fp8muni
    })
    observeEvent(fp8comp(), {
      x <- fp8 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != fp8comp())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "fp8municomp",
        choices = c("Selecione um município", choices),
        session
      )
    })
    
    output$fp8txt1 <- renderText({
      req(input$fp8municomp)
      if (input$fp8municomp == "Selecione um município") {
        paste0(
          "Receita de Transferências Correntes - ",
          input$fp8muni,
          " - ",
          min(fp8$ano),
          " a ",
          max(fp8$ano)
        )
      } else {
        paste0(
          "Receita de Transferências Correntes - ",
          input$fp8muni,
          " x ",
          input$fp8municomp,
          " - ",
          min(fp8$ano),
          " a ",
          max(fp8$ano)
        )
      }
    })
    
    output$fp8graf <- renderEcharts4r({
      req(input$fp8municomp)
      if (input$fp8municomp == "Selecione um município") {
        a <- fp8 %>% filter(localidade == input$fp8muni)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f2c94e",
            name = "Valor(R$)",
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
        a <- fp8 %>% filter(localidade == input$fp8muni)
        b <- fp8 %>% filter(localidade == input$fp8municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$fp8muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$fp8municomp,
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
    ## Tabela - Receita de Transferências Correntes da mesma Região de Integração----
    output$fp8txt2 <- renderText({
      t82()
    })
    output$fp8tab <- renderReactable({
      ris <- fp8 %>%
        filter(ano == input$fp8ano, localidade == input$fp8muni) %>%
        pull(ri)
      x <-
        fp8 %>% filter(ano == input$fp8ano, localidade != "Pará")
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
            name = "Valor(R$)",
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
    ## 1 - Repasse de ICMS----
    ## Gráfico de linha - Repasse de ICMS----
    # Filtra os dados
    fp1_1 <- reactive({
      req(input$fp1municomp)
      if (input$fp1municomp == "Selecione um município") {
        a <- fp1 %>% filter(localidade == input$fp1muni)
      } else {
        a <- fp1 %>% filter(localidade == input$fp1muni)
        b <- fp1 %>% filter(localidade == input$fp1municomp)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(fp1_1(), {
      t11()
      downset_Server("fp1_1", fp1_1(), t11())
    })
    ## Tabela - Repasse de ICMS----
    # Filtra os dados
    fp1_2 <- reactive({
      ris <- fp1 %>%
        filter(ano == input$fp1ano, localidade == input$fp1muni) %>%
        pull(ri)
      x <-
        fp1 %>%
        filter(ano == input$fp1ano, localidade != "Pará") %>%
        filter(ri == ris) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(fp1_2(), {
      t12()
      downset_Server("fp1_2", fp1_2(), t12())
    })
    
    ## 2 - Demonstrativo dos Índices de Participação na Arrecadação do ICMS----
    ## Gráfico - Demonstrativo dos Índices de Participação na Arrecadação do ICMS----
    # Filtra os dados
    fp2_1 <- reactive({
      req(input$fp2municomp)
      if (input$fp2municomp == "Selecione um município") {
        a <- fp2 %>% filter(localidade == input$fp2muni)
      }
      else {
        a <- fp2 %>% filter(localidade == input$fp2muni)
        b <- fp2 %>% filter(localidade == input$fp2municomp)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(fp2_1(), {
      t21()
      downset_Server("fp2_1", fp2_1(), t21())
    })
    ## Tabela - Demonstrativo dos Índices de Participação na Arrecadação do ICMS----
    # Filtra os dados
    fp2_2 <- reactive({
      ris <-
        fp2 %>%
        filter(ano == input$fp2ano, localidade == input$fp2muni) %>%
        pull(ri)
      x <-
        fp2 %>%
        filter(ano == input$fp2ano, localidade != "Pará") %>%
        filter(ri == ris) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(fp2_2(), {
      t22()
      downset_Server("fp2_2", fp2_2(), t22())
    })
    
    ## 3 - Repasse de IPI dos Municípios----
    ## Gráfico - Repasse de IPI dos Municípios----
    # Filtra os dados
    fp3_1 <- reactive({
      req(input$fp3municomp)
      if (input$fp3municomp == "Selecione um município") {
        a <- fp3 %>% filter(localidade == input$fp3muni)
      } else {
        a <- fp3 %>% filter(localidade == input$fp3muni)
        b <- fp3 %>% filter(localidade == input$fp3municomp)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(fp3_1(), {
      t31()
      downset_Server("fp3_1", fp3_1(), t31())
    })
    ## Tabela - Repasse de IPI dos Municípios----
    # Filtra os dados
    fp3_2 <- reactive({
      ris <- fp3 %>%
        filter(ano == input$fp3ano, localidade == input$fp3muni) %>%
        pull(ri)
      x <-
        fp3 %>%
        filter(ano == input$fp3ano, localidade != "Pará", ri == ris) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(fp3_2(), {
      t32()
      downset_Server("fp3_2", fp3_2(), t32())
    })
    
    ## 4 - Repasse de IPVA  dos Municípios----
    ## Gráfico - Repasse de IPVA  dos Municípios----
    # Filtra os dados
    fp4_1 <- reactive({
      req(input$fp4municomp)
      if (input$fp4municomp == "Selecione um município") {
        a <- fp4 %>%
          filter(localidade == input$fp4muni)
      } else {
        a <- fp4 %>% filter(localidade == input$fp4muni)
        b <- fp4 %>% filter(localidade == input$fp4municomp)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(fp4_1(), {
      t41()
      downset_Server("fp4_1", fp4_1(), t41())
    })
    ## Tabela - Repasse de IPVA  dos Municípios----
    # Filtra os dados
    fp4_2 <- reactive({
      ris <- fp4 %>%
        filter(ano == input$fp4ano, localidade == input$fp4muni) %>%
        pull(ri)
      x <-
        fp4 %>%
        filter(ano == input$fp4ano, localidade != "Pará", ri == ris) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(fp4_2(), {
      t42()
      downset_Server("fp4_2", fp4_2(), t42())
    })
    
    ## 5 - Receita Orçamentária----
    ## Gráfico - Receita Orçamentária----
    # Filtra os dados
    fp5_1 <- reactive({
      req(input$fp5municomp)
      if (input$fp5municomp == "Selecione um município") {
        a <- fp5 %>%
          filter(localidade == input$fp5muni)
        
      } else {
        a <- fp5 %>% filter(localidade == input$fp5muni)
        b <- fp5 %>% filter(localidade == input$fp5municomp)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(fp5_1(), {
      t51()
      downset_Server("fp5_1", fp5_1(), t51())
    })
    ## Tabela - Receita Orçamentária----
    # Filtra os dados
    fp5_2 <- reactive({
      ris <- fp5 %>%
        filter(ano == input$fp5ano, localidade == input$fp5muni) %>%
        pull(ri)
      x <-
        fp5 %>%
        filter(ano == input$fp5ano, localidade != "Pará", ri == ris) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(fp5_2(), {
      t52()
      downset_Server("fp5_2", fp5_2(), t52())
    })
    
    ## 6 - Receitas Correntes----
    ## Gráfico - Receitas Correntes----
    # Filtra os dados
    fp6_1 <- reactive({
      req(input$fp6municomp)
      if (input$fp6municomp == "Selecione um município") {
        a <- fp6 %>% filter(localidade == input$fp6muni)
      } else {
        a <- fp6 %>% filter(localidade == input$fp6muni)
        b <- fp6 %>% filter(localidade == input$fp6municomp)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(fp6_1(), {
      t61()
      downset_Server("fp6_1", fp6_1(), t61())
    })
    ## Tabela - Receitas Correntes----
    # Filtra os dados
    fp6_2 <- reactive({
      ris <- fp6 %>%
        filter(ano == input$fp6ano, localidade == input$fp6muni) %>%
        pull(ri)
      x <-
        fp6 %>%
        filter(ano == input$fp6ano, localidade != "Pará", ri == ris) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(fp6_2(), {
      t62()
      downset_Server("fp6_2", fp6_2(), t62())
    })
    ## 7 - Impostos----
    ## Gráfico - Impostos----
    # Filtra os dados
    fp7_1 <- reactive({
      req(input$fp7municomp)
      if (input$fp7municomp == "Selecione um município") {
        a <- fp7 %>% filter(localidade == input$fp7muni)
      } else {
        a <- fp7 %>% filter(localidade == input$fp7muni)
        b <- fp7 %>% filter(localidade == input$fp7municomp)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(fp7_1(), {
      t71()
      downset_Server("fp7_1", fp7_1(), t71())
    })
    ## Tabela - Impostos----
    # Filtra os dados
    fp7_2 <- reactive({
      ris <- fp7 %>%
        filter(ano == input$fp7ano, localidade == input$fp7muni) %>%
        pull(ri)
      x <-
        fp7 %>%
        filter(ano == input$fp7ano, localidade != "Pará", ri == ris) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(fp7_2(), {
      t72()
      downset_Server("fp7_2", fp7_2(), t72())
    })
    
    ## 8 - Receita de Transferências Correntes----
    ## Gráfico - Receita de Transferências Correntes----
    # Filtra os dados
    fp8_1 <- reactive({
      req(input$fp8municomp)
      if (input$fp8municomp == "Selecione um município") {
        a <-
          fp8 %>%
          filter(localidade == input$fp8muni)
      } else {
        a <- fp8 %>% filter(localidade == input$fp8muni)
        b <- fp8 %>% filter(localidade == input$fp8municomp)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(fp8_1(), {
      t81()
      downset_Server("fp8_1", fp8_1(), t81())
    })
    ## Tabela - Receita de Transferências Correntes----
    # Filtra os dados
    fp8_2 <- reactive({
      ris <- fp8 %>%
        filter(ano == input$fp8ano, localidade == input$fp8muni) %>%
        pull(ri)
      x <-
        fp8 %>%
        filter(ano == input$fp8ano, localidade != "Pará", ri == ris) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(fp8_2(), {
      t82()
      downset_Server("fp8_2", fp8_2(), t82())
    })
  })
}

# # Play do Módulo
# ui <- dashboardPage(header = dashboardHeader(),
#                     sidebar = dashboardSidebar(),
#                     body = dashboardBody(fluidPage(economia_fp_mp_ui("economia_fp_mp"))))
# 
# 
# server <- function(input, output) {
#   economia_fp_mp_Server("economia_fp_mp")
# }
# 
# shinyApp(ui, server)
