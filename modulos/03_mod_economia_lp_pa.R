# Funções de módulo de Economia - Lavoura Permanente - Estadual
# Função de UI
economia_lp_pa_ui <- function(id) {
  fluidPage(# CAMINHO DO ARQUIVO CSS----
            tags$head(
              tags$link(rel = "stylesheet", type = "text/css", href = "estilo.css")
            ),
            # Lista de Navegação lateral----
            div(class = "navbar_economia",
                navbarPage(
                  tags$b("Lavoura Permanente - Pará"),
                  navbarMenu(
                    "Indicadores",
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
                          class = "seletor1",
                          pickerInput(
                            inputId = NS(id, "lp1ano"),
                            label = "Ano",
                            choices = sort(unique(lp1[["ano"]]), decreasing = T),
                            width = "100px"
                          )
                        ),
                        tags$div(
                          class = "seletor2",
                          pickerInput(
                            inputId = NS(id, "lp1ri"),
                            label = "Pará/Região de Integração",
                            choices = unique(lp1[["ri"]]),
                            width = "200px"
                          )
                        ),
                        tags$div(
                          class = "seletor2",
                          pickerInput(
                            inputId = NS(id, "lp1cat"),
                            label = "Produto",
                            choices = unique(lp1[["categoria"]]),
                            width = "200px"
                          )
                        )
                      ),
                      fluidRow(
                        ## Mapa - Área Destinada à Colheita----
                        box(
                          title = textOutput(NS(id, "lp1txt1")),
                          collapsible = T,
                          collapsed = F,
                          headerBorder = T,
                          width = 12,
                          withSpinner(
                            leafletOutput(NS(id, "lp1map"), height = "600px"),
                            type = 8,
                            color = "#f2a92a",
                            size = 0.5
                          ),
                          footer = list(tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                                        tags$h6(tags$b("Elaboração:"), "FAPESPA"))
                        ),
                        ## Tabela - Área Destinada à Colheita----
                        box(
                          title = textOutput(NS(id, "lp1txt2")),
                          collapsible = T,
                          collapsed = F,
                          width = 12,
                          headerBorder = T,
                          solidHeader = F,
                          withSpinner(
                            reactableOutput(NS(id, "lp1tab"), height = "400px"),
                            type = 8,
                            color = "#f2a92a",
                            size = 0.5
                          ),
                          footer = list(column(
                            11,
                            tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA")
                          ),
                          downset_ui(NS(id, "lp1_1")))
                        )
                      ),
                      fluidRow(
                        ## Gráfico - Área Destinada à Colheita----
                        box(
                          title = textOutput(NS(id, "lp1txt3")),
                          collapsible = T,
                          collapsed = F,
                          width = 12,
                          headerBorder = T,
                          solidHeader = F,
                          withSpinner(
                            echarts4rOutput(NS(id, "lp1graf")),
                            type = 8,
                            color = "#f2a92a",
                            size = 0.5
                          ),
                          footer = list(column(
                            11,
                            tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA")
                          ),
                          downset_ui(NS(id, "lp1_2")))
                        ),
                        ## Tabela - Área Destinada à Colheita----
                        box(
                          title = textOutput(NS(id, "lp1txt4")),
                          collapsible = T,
                          collapsed = F,
                          width = 12,
                          headerBorder = T,
                          solidHeader = F,
                          withSpinner(
                            reactableOutput(NS(id, "lp1tabcat")),
                            type = 8,
                            color = "#f2a92a",
                            size = 0.5
                          ),
                          footer = list(column(
                            11,
                            tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA")
                          ),
                          downset_ui(NS(id, "lp1_3")))
                        )
                      )
                    ),
                    
                    # 2 - Área Colhida à Colheita----
                    tabPanel(
                      "Área Colhida à Colheita",
                      panel(
                        ## Controle----
                        heading =
                          h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                             "Área Colhida à Colheita"),
                        tags$div(
                          class = "seletor1",
                          pickerInput(
                            inputId = NS(id, "lp2ano"),
                            label = "Ano",
                            choices = sort(unique(lp2[["ano"]]), decreasing = T),
                            width = "100px"
                          )
                        ),
                        tags$div(
                          class = "seletor2",
                          pickerInput(
                            inputId = NS(id, "lp2ri"),
                            label = "Pará/Região de Integração",
                            choices = unique(lp2[["ri"]]),
                            width = "200px"
                          )
                        ),
                        tags$div(
                          class = "seletor2",
                          pickerInput(
                            inputId = NS(id, "lp2cat"),
                            label = "Produto",
                            choices = unique(lp2[["categoria"]]),
                            width = "200px"
                          )
                        )
                      ),
                      fluidRow(
                        ## Mapa - Área Colhida à Colheita----
                        box(
                          title = textOutput(NS(id, "lp2txt1")),
                          collapsible = T,
                          collapsed = F,
                          headerBorder = T,
                          width = 12,
                          withSpinner(
                            leafletOutput(NS(id, "lp2map"), height = "600px"),
                            type = 8,
                            color = "#f2a92a",
                            size = 0.5
                          ),
                          footer = list(tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                                        tags$h6(tags$b("Elaboração:"), "FAPESPA"))
                        ),
                        ## Tabela - Área Colhida à Colheita----
                        box(
                          title = textOutput(NS(id, "lp2txt2")),
                          collapsible = T,
                          collapsed = F,
                          width = 12,
                          headerBorder = T,
                          solidHeader = F,
                          withSpinner(
                            reactableOutput(NS(id, "lp2tab"), height = "400px"),
                            type = 8,
                            color = "#f2a92a",
                            size = 0.5
                          ),
                          footer = list(column(
                            11,
                            tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA")
                          ),
                          downset_ui(NS(id, "lp2_1")))
                        )
                      ),
                      fluidRow(
                        ## Gráfico - Área Colhida à Colheita----
                        box(
                          title = textOutput(NS(id, "lp2txt3")),
                          collapsible = T,
                          collapsed = F,
                          width = 12,
                          headerBorder = T,
                          solidHeader = F,
                          withSpinner(
                            echarts4rOutput(NS(id, "lp2graf")),
                            type = 8,
                            color = "#f2a92a",
                            size = 0.5
                          ),
                          footer = list(column(
                            11,
                            tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA")
                          ),
                          downset_ui(NS(id, "lp2_2")))
                        ),
                        ## Tabela - Área Colhida à Colheita----
                        box(
                          title = textOutput(NS(id, "lp2txt4")),
                          collapsible = T,
                          collapsed = F,
                          width = 12,
                          headerBorder = T,
                          solidHeader = F,
                          withSpinner(
                            reactableOutput(NS(id, "lp2tabcat"), height = "600px"),
                            type = 8,
                            color = "#f2a92a",
                            size = 0.5
                          ),
                          footer = list(column(
                            11,
                            tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA")
                          ),
                          downset_ui(NS(id, "lp2_3")))
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
                          class = "seletor1",
                          # Selecionar
                          pickerInput(
                            inputId = NS(id, "lp3ano"),
                            label = "Ano",
                            choices = sort(unique(lp3[["ano"]]), decreasing = T),
                            width = "100px"
                          )
                        ),
                        tags$div(
                          class = "seletor2",
                          pickerInput(
                            inputId = NS(id, "lp3ri"),
                            label = "Pará/Região de Integração",
                            choices = unique(lp3[["ri"]]),
                            width = "200px"
                          )
                        ),
                        tags$div(
                          class = "seletor2",
                          pickerInput(
                            inputId = NS(id, "lp3cat"),
                            label = "Produto",
                            choices = unique(lp3[["categoria"]]),
                            width = "200px"
                          )
                        )
                      ),
                      fluidRow(
                        ## Mapa - Quantidade Produzida----
                        box(
                          title = textOutput(NS(id, "lp3txt1")),
                          collapsible = T,
                          collapsed = F,
                          headerBorder = T,
                          width = 12,
                          withSpinner(
                            leafletOutput(NS(id, "lp3map"), height = "600px"),
                            type = 8,
                            color = "#f2a92a",
                            size = 0.5
                          ),
                          footer = list(tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                                        tags$h6(tags$b("Elaboração:"), "FAPESPA"))
                        ),
                        ## Tabela - Quantidade Produzida----
                        box(
                          title = textOutput(NS(id, "lp3txt2")),
                          collapsible = T,
                          collapsed = F,
                          width = 12,
                          headerBorder = T,
                          solidHeader = F,
                          withSpinner(
                            reactableOutput(NS(id, "lp3tab"), height = "400px"),
                            type = 8,
                            color = "#f2a92a",
                            size = 0.5
                          ),
                          footer = list(column(
                            11,
                            tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA")
                          ),
                          downset_ui(NS(id, "lp3_1")))
                        )
                      ),
                      fluidRow(
                        ## Gráfico - Quantidade Produzida----
                        box(
                          title = textOutput(NS(id, "lp3txt3")),
                          collapsible = T,
                          collapsed = F,
                          width = 12,
                          headerBorder = T,
                          solidHeader = F,
                          withSpinner(
                            echarts4rOutput(NS(id, "lp3graf")),
                            type = 8,
                            color = "#f2a92a",
                            size = 0.5
                          ),
                          footer = list(column(
                            11,
                            tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA")
                          ),
                          downset_ui(NS(id, "lp3_2")))
                        ),
                        ## Tabela - Quantidade Produzida----
                        box(
                          title = textOutput(NS(id, "lp3txt4")),
                          collapsible = T,
                          collapsed = F,
                          width = 12,
                          headerBorder = T,
                          solidHeader = F,
                          withSpinner(
                            reactableOutput(NS(id, "lp3tabcat"), height = "600px"),
                            type = 8,
                            color = "#f2a92a",
                            size = 0.5
                          ),
                          footer = list(column(
                            11,
                            tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA")
                          ),
                          downset_ui(NS(id, "lp3_3")))
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
                          class = "seletor1",
                          pickerInput(
                            inputId = NS(id, "lp4ano"),
                            label = "Ano",
                            choices = sort(unique(lp4[["ano"]]), decreasing = T),
                            width = "100px"
                          )
                        ),
                        tags$div(
                          class = "seletor2",
                          pickerInput(
                            inputId = NS(id, "lp4ri"),
                            label = "Pará/Região de Integração",
                            choices = unique(lp4[["ri"]]),
                            width = "200px"
                          )
                        ),
                        tags$div(
                          class = "seletor2",
                          pickerInput(
                            inputId = NS(id, "lp4cat"),
                            label = "Produto",
                            choices = unique(lp4[["categoria"]]),
                            width = "200px"
                          )
                        )
                      ),
                      fluidRow(
                        ## Mapa - Valor da Produção----
                        box(
                          title = textOutput(NS(id, "lp4txt1")),
                          collapsible = T,
                          collapsed = F,
                          headerBorder = T,
                          width = 12,
                          withSpinner(
                            leafletOutput(NS(id, "lp4map"), height = "600px"),
                            type = 8,
                            color = "#f2a92a",
                            size = 0.5
                          ),
                          footer = list(tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                                        tags$h6(tags$b("Elaboração:"), "FAPESPA"))
                        ),
                        ## Tabela - Valor da Produção----
                        box(
                          title = textOutput(NS(id, "lp4txt2")),
                          collapsible = T,
                          collapsed = F,
                          width = 12,
                          headerBorder = T,
                          solidHeader = F,
                          withSpinner(
                            reactableOutput(NS(id, "lp4tab"), height = "400px"),
                            type = 8,
                            color = "#f2a92a",
                            size = 0.5
                          ),
                          footer = list(column(
                            11,
                            tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA")
                          ),
                          downset_ui(NS(id, "lp4_1")))
                        )
                      ),
                      fluidRow(
                        ## Gráfico - Valor da Produção----
                        box(
                          title = textOutput(NS(id, "lp4txt3")),
                          collapsible = T,
                          collapsed = F,
                          width = 12,
                          headerBorder = T,
                          solidHeader = F,
                          withSpinner(
                            echarts4rOutput(NS(id, "lp4graf")),
                            type = 8,
                            color = "#f2a92a",
                            size = 0.5
                          ),
                          footer = list(column(
                            11,
                            tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA")
                          ),
                          downset_ui(NS(id, "lp4_2")))
                        ),
                        ## Tabela - Valor da Produção----
                        box(
                          title = textOutput(NS(id, "lp4txt4")),
                          collapsible = T,
                          collapsed = F,
                          width = 12,
                          headerBorder = T,
                          solidHeader = F,
                          withSpinner(
                            reactableOutput(NS(id, "lp4tabcat")),
                            type = 8,
                            color = "#f2a92a",
                            size = 0.5
                          ),
                          footer = list(column(
                            11,
                            tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA")
                          ),
                          downset_ui(NS(id, "lp4_3")))
                        )
                      )
                    ),
                    
                    # 5 - Rendimento Médio da produção----
                    tabPanel(
                      "Rendimento Médio da produção",
                      panel(
                        ## Controle----
                        heading =
                          h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                             "Rendimento Médio da produção"),
                        tags$div(
                          class = "seletor1",
                          pickerInput(
                            inputId = NS(id, "lp5ano"),
                            label = "Ano",
                            choices = sort(unique(lp5[["ano"]]), decreasing = T),
                            width = "100px"
                          )
                        ),
                        tags$div(
                          class = "seletor2",
                          pickerInput(
                            inputId = NS(id, "lp5ri"),
                            label = "Pará/Região de Integração",
                            choices = unique(lp5[["ri"]]),
                            width = "200px"
                          )
                        ),
                        tags$div(
                          class = "seletor2",
                          pickerInput(
                            inputId = NS(id, "lp5cat"),
                            label = "Produto",
                            choices = unique(lp5[["categoria"]]),
                            width = "200px"
                          )
                        )
                      ),
                      fluidRow(
                        ## Mapa - Rendimento Médio da produção----
                        box(
                          title = textOutput(NS(id, "lp5txt1")),
                          collapsible = T,
                          collapsed = F,
                          headerBorder = T,
                          width = 12,
                          withSpinner(
                            leafletOutput(NS(id, "lp5map"), height = "600px"),
                            type = 8,
                            color = "#f2a92a",
                            size = 0.5
                          ),
                          footer = list(tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                                        tags$h6(tags$b("Elaboração:"), "FAPESPA"))
                        ),
                        ## Tabela - Rendimento Médio da produção----
                        box(
                          title = textOutput(NS(id, "lp5txt2")),
                          collapsible = T,
                          collapsed = F,
                          width = 12,
                          headerBorder = T,
                          solidHeader = F,
                          withSpinner(
                            reactableOutput(NS(id, "lp5tab"), height = "600px"),
                            type = 8,
                            color = "#f2a92a",
                            size = 0.5
                          ),
                          footer = list(column(
                            11,
                            tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA")
                          ),
                          downset_ui(NS(id, "lp5_1")))
                        )
                      ),
                      fluidRow(
                        ## Gráfico - Rendimento Médio da produção----
                        box(
                          title = textOutput(NS(id, "lp5txt3")),
                          collapsible = T,
                          collapsed = F,
                          width = 12,
                          headerBorder = T,
                          solidHeader = F,
                          withSpinner(
                            echarts4rOutput(NS(id, "lp5graf")),
                            type = 8,
                            color = "#f2a92a",
                            size = 0.5
                          ),
                          footer = list(column(
                            11,
                            tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA")
                          ),
                          downset_ui(NS(id, "lp5_2")))
                        ),
                        ## Tabela - Rendimento Médio da produção----
                        box(
                          title = textOutput(NS(id, "lp5txt4")),
                          collapsible = T,
                          collapsed = F,
                          width = 12,
                          headerBorder = T,
                          solidHeader = F,
                          withSpinner(
                            reactableOutput(NS(id, "lp5tabcat")),
                            type = 8,
                            color = "#f2a92a",
                            size = 0.5
                          ),
                          footer = list(column(
                            11,
                            tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA")
                          ),
                          downset_ui(NS(id, "lp5_3")))
                        )
                      )
                    )
                  )
                )))
}

# Função do modulo servidor
economia_lp_pa_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    #TÍTULOS----
    # 1-Área Destinada à Colheita----
    ## Mapa - Área Destinada à Colheita (Hectares)----
    t11 <- reactive({
      if (input$lp1ri == "Pará") {
        paste0(
          "Área Destinada à Colheita (Hectares) da Lavoura Permanente de ",
          input$lp1cat,
          ", Pará - ",
          input$lp1ano
        )
      } else {
        paste0(
          "Área Destinada à Colheita (Hectares) da Lavoura Permanente de ",
          input$lp1cat,
          " dos Municípios, Região de Integração ",
          input$lp1ri,
          " - ",
          input$lp1ano
        )
      }
    })
    ## Tabela - Área Destinada à Colheita----
    t12 <- reactive({
      if (input$lp1ri == "Pará") {
        paste0(
          "Área Destinada à Colheita (Hectares) da Lavoura Permanente de ",
          input$lp1cat,
          " dos Municípios, Pará - ",
          input$lp1ano
        )
      } else {
        paste0(
          "Área Destinada à Colheita (Hectares) da Lavoura Permanente de ",
          input$lp1cat,
          " dos Municípios, Região de Integração ",
          input$lp1ri,
          " - ",
          input$lp1ano
        )
      }
    })
    ## Gráfico - Área Destinada à Colheita----
    t13 <- reactive({
      paste0(
        "Área Total Destinada à Colheita (Hectares) da Lavoura Permanente de ",
        input$lp1cat,
        ", Pará - ",
        min(lp1$ano),
        " a ",
        max(lp1$ano)
      )
    })
    ## Tabela - Área Destinada à Colheita----
    t14 <- reactive({
      paste0(
        "Área Total Destinada à Colheita (Hectares) por tipo de Lavoura Permanente, Pará - ",
        input$lp1ano
      )
    })
    # 2-Área Colhida à Colheita----
    ## Mapa - Área Colhida à Colheita (Hectares)----
    t21 <- reactive({
      if (input$lp2ri == "Pará") {
        paste0(
          "Área Colhida (Hectares) da Lavoura Permanente de ",
          input$lp2cat,
          ", Pará - ",
          input$lp2ano
        )
      } else {
        paste0(
          "Área Colhida (Hectares) da Lavoura Permanente de ",
          input$lp2cat,
          ", Região de Integração ",
          input$lp2ri,
          " - ",
          input$lp2ano
        )
      }
    })
    ## Tabela - Área Colhida à Colheita----
    t22 <- reactive({
      if (input$lp2ri == "Pará") {
        paste0(
          "Área Colhida (Hectares) da Lavoura Permanente de ",
          input$lp2cat,
          " dos Municípios, Pará - ",
          input$lp2ano
        )
      } else {
        paste0(
          "Área Colhida (Hectares) da Lavoura Permanente de ",
          input$lp2cat,
          " dos Municípios, Região de Integração ",
          input$lp2ri,
          " - ",
          input$lp2ano
        )
      }
    })
    ## Gráfico - Área Colhida à Colheita----
    t23 <- reactive({
      paste0("Área Total Colhida (Hectares) da Lavoura Permanente, Pará - ",
             input$lp2cat)
    })
    ## Tabela - Área Colhida à Colheita----
    t24 <- reactive({
      paste0("Área Total Colhida (Hectares) por tipo de Lavoura Permanente, Pará - ",
             input$lp2ano)
    })
    # 3 - Quantidade Produzida----
    ## Mapa - Quantidade Produzida----
    t31 <- reactive({
      if (input$lp3ri == "Pará") {
        paste0(
          "Quantidade Produzida da Lavoura Permanente de ",
          input$lp3cat,
          ", Pará - ",
          input$lp3ano
        )
      } else {
        paste0(
          "Quantidade Produzida da Lavoura Permanente de ",
          input$lp3cat,
          ", Região de Integração ",
          input$lp3ri,
          " - ",
          input$lp3ano
        )
      }
    })
    ## Tabela - Quantidade Produzida----
    t32 <- reactive({
      if (input$lp3ri == "Pará") {
        paste0(
          "Quantidade Produzida da Lavoura Permanente de ",
          input$lp3cat,
          " dos Municípios, Pará - ",
          input$lp3ano
        )
      } else {
        paste0(
          "Quantidade Produzida da Lavoura Permanente de ",
          input$lp3cat,
          " dos Municípios, Região de Integração ",
          input$lp3ri,
          " - ",
          input$lp3ano
        )
      }
    })
    ## Gráfico - Quantidade Produzida----
    t33 <- reactive({
      paste0(
        "Quantidade Total Produzida da Lavoura Permanente de ",
        input$lp3cat ,
        ", Pará - ",
        min(lp3$ano),
        " a ",
        max(lp3$ano)
        
      )
    })
    ## Tabela - Quantidade Produzida----
    t34 <- reactive({
      paste0("Quantidade Total Produzida por Tipo de Lavoura Permanente, Pará - ",
             input$lp3ano)
    })
    # 4 - Valor da Produção----
    ## Mapa - Valor da Produção----
    t41 <- reactive({
      if (input$lp3ri == "Pará") {
        paste0(
          "Valor da Produção (em mil reais) da Lavoura Permanente de ",
          input$lp4cat,
          ", Pará ",
          input$lp4ano
        )
      } else {
        paste0(
          "Valor da Produção (em mil reais) da Lavoura Permanente de ",
          input$lp3cat,
          " dos Municípios, Região de Integração ",
          input$lp3ri,
          " - ",
          input$lp3ano
        )
      }
      
    })
    ## Tabela - Valor da Produção----
    t42 <- reactive({
      if (input$lp4ri == "Pará") {
        paste0(
          "Valor da Produção (em mil reais) da Lavoura Permanente ",
          input$lp4cat,
          " dos Municípios, Pará - ",
          input$lp4ano
        )
      } else {
        paste0(
          "Valor da Produção (em mil reais) da Lavoura Permanente ",
          input$lp4cat,
          " dos Municípios, Região de Integração ",
          input$lp4ri,
          " - ",
          input$lp4ano
        )
      }
    })
    ## Gráfico - Valor da Produção----
    t43 <- reactive({
      paste0(
        "Valor Total da Produção (em mil reais) da Lavoura Permanente de ",
        input$lp4cat,
        ", Pará - ",
        min(lp4$ano),
        " a ",
        max(lp4$ano)
      )
    })
    ## Tabela - Valor da Produção----
    t44 <- reactive({
      paste0(
        "Valor Total da Produção (em mil reais) por Tipo de Lavoura Permanente, Pará - ",
        input$lp4ano
      )
    })
    # 5 - Rendimento Médio da produção----
    ## Mapa - Rendimento Médio da produção----
    t51 <- reactive({
      paste0(
        "Rendimento Médio da Produção da Lavoura Permanente de ",
        input$lp5cat,
        " - ",
        input$lp5ano
      )
    })
    ## Tabela - Rendimento Médio da produção----
    t52 <- reactive({
      if (input$lp5ri == "Pará") {
        paste0(
          "Rendimento Médio da Produção da Lavoura Permanente ",
          input$lp5cat,
          " dos Municípios, Pará - ",
          input$lp5ano
        )
      } else {
        paste0(
          "Rendimento Médio da Produção da Lavoura Permanente ",
          input$lp5cat,
          " dos Municípios, Região de Integração ",
          input$lp5ri,
          " - ",
          input$lp5ano
        )
      }
    })
    ## Gráfico - Rendimento Médio da produção----
    t53 <- reactive({
      paste0(
        "Rendimento Médio da Produção da Lavoura Permanente de ",
        input$lp5cat,
        ", Pará - ",
        min(lp5$ano),
        " a ",
        max(lp5$ano)
        
      )
    })
    ## Tabela - Rendimento Médio da produção----
    t54 <- reactive({
      paste0("Rendimento Médio da Produção por tipo de Lavoura Permanente, Pará - ",
             input$lp5ano)
    })
    #VISUALIZAÇÃO----
    # 1-Área Destinada à Colheita----
    ## Mapa - Área Destinada à Colheita (Hectares)----
    output$lp1txt1 <- renderText({
      t11()
    })
    output$lp1map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$lp1ri == "Pará") {
        df <- lp1 %>%
          filter(localidade != "Pará",
                 ano == input$lp1ano,
                 categoria == input$lp1cat) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- lp1 %>%
          filter(localidade != "Pará",
                 ano == input$lp1ano,
                 categoria == input$lp1cat) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$lp1ri)
      }
      
      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 5, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }
      
      pal <-
        colorBin(
          c("#ffff81", "#fad254", "#f2a92a", "#ae520b", "#6b0000"),
          domain = x$valor,
          bins = bins
        )
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Área(ha):</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Sem Produção",
            format(x$valor, big.mark = ".", decimal.mark = ",")
          )
        ) %>%
        lapply(htmltools::HTML)
      
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
          values = ~ valor,
          opacity = 0.7,
          title = "Área(ha)",
          position = "bottomright",
          na.label = "Sem Produção",
          labFormat = labelFormat_decimal(
            big.mark = ".",
            decimal.mark = ",",
            digits = 2
          )
        )
    })
    ## Tabela - Área Destinada à Colheita----
    output$lp1txt2 <- renderText({
      t12()
    })
    # Renderização da base lp1
    output$lp1tab <- renderReactable({
      if (input$lp1ri == "Pará") {
        x <- lp1 %>%
          filter(localidade != "Pará",
                 ano == input$lp1ano,
                 categoria == input$lp1cat) %>%
          select(ri, localidade, valor)
      } else {
        x <- lp1 %>%
          filter(localidade != "Pará",
                 ano == input$lp1ano,
                 categoria == input$lp1cat) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$lp1ri)
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
            name = "Área(ha)",
            format = colFormat(separators = T,
                               locales = "pt-BR")
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
    ## Gráfico - Área Destinada à Colheita----
    output$lp1txt3 <- renderText({
      t13()
    })
    output$lp1graf <- renderEcharts4r({
      lp1 %>%
        filter(categoria == input$lp1cat, localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#f2a92a",
          name = "Área (ha)",
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
          name = "Área(ha)",
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
    })
    ## Tabela - Área Destinada à Colheita----
    output$lp1txt4 <- renderText({
      t14()
    })
    output$lp1tabcat <- renderReactable({
      x <- lp1 %>%
        filter(localidade == "Pará", ano == input$lp1ano) %>%
        select(categoria, valor) %>%
        arrange(desc(valor)) %>%
        mutate(Percentual = (valor / sum(valor)) * 100)
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
          categoria = colDef(name = "Produtos"),
          valor = colDef(
            name = "Área(ha)",
            format = colFormat(separators = T, locales = "pt-BR")
          ),
          Percentual =
            colDef(
              name = "Percentual(%)",
              format = colFormat(
                digits = 2,
                separators = T,
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
    
    # 2-Área Colhida à Colheita----
    ## Mapa - Área Colhida à Colheita (Hectares)----
    output$lp2txt1 <- renderText({
      t21()
    })
    output$lp2map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$lp2ri == "Pará") {
        df <- lp2 %>%
          filter(localidade != "Pará",
                 ano == input$lp2ano,
                 categoria == input$lp2cat) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- lp2 %>%
          filter(localidade != "Pará",
                 ano == input$lp2ano,
                 categoria == input$lp2cat) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$lp2ri)
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
          c("#ffff81", "#fad254", "#f2a92a", "#ae520b", "#6b0000"),
          domain = x$valor,
          bins = bins
        )
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Área(ha):</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Sem Produção",
            format(x$valor, big.mark = ".", decimal.mark = ",")
          )
        ) %>% lapply(htmltools::HTML)
      # Mapas com leafleft
      leaflet(x,
              options =
                leafletOptions(minZoom = 0,
                               maxZoom = 15)) %>%
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
          values = ~ valor,
          opacity = 0.7,
          title = "Área(ha)",
          position = "bottomright",
          na.label = "Sem Produção",
          labFormat = labelFormat_decimal(
            big.mark = ".",
            decimal.mark = ",",
            digits = 2
          )
        )
    })
    ## Tabela - Área Colhida à Colheita----
    output$lp2txt2 <- renderText({
      t22()
    })
    # Renderização da base lp2
    output$lp2tab <- renderReactable({
      if (input$lp2ri == "Pará") {
        x <- lp2 %>%
          filter(localidade != "Pará",
                 ano == input$lp2ano,
                 categoria == input$lp2cat) %>%
          select(ri, localidade, valor)
      } else {
        x <- lp2 %>%
          filter(localidade != "Pará",
                 ano == input$lp2ano,
                 categoria == input$lp2cat) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$lp2ri)
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
            name = "Área(ha)",
            format = colFormat(separators = T,
                               locales = "pt-BR")
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
    ## Gráfico - Área Colhida à Colheita----
    output$lp2txt3 <- renderText({
      t23()
    })
    output$lp2graf <- renderEcharts4r({
      lp2 %>%
        filter(categoria == input$lp2cat, localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#f2a92a",
          name = "Área (ha)",
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
          name = "Área(ha)",
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
    })
    ## Tabela - Área Colhida à Colheita----
    output$lp2txt4 <- renderText({
      t24()
    })
    output$lp2tabcat <- renderReactable({
      x <- lp2 %>%
        filter(localidade == "Pará", ano == input$lp2ano) %>%
        select(categoria, valor) %>%
        arrange(desc(valor)) %>%
        mutate(Percentual = (valor / sum(valor)) * 100)
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
          categoria = colDef(name = "Produtos"),
          valor = colDef(
            name = "Área(ha)",
            format = colFormat(separators = T,
                               locales = "pt-BR")
          ),
          Percentual = colDef(
            name = "Percentual(%)",
            format = colFormat(
              digits = 2,
              separators = T,
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
    
    # 3 - Quantidade Produzida----
    ## Mapa - Quantidade Produzida----
    output$lp3txt1 <- renderText({
      t31()
    })
    output$lp3map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$lp3ri == "Pará") {
        df <- lp3 %>%
          filter(localidade != "Pará",
                 ano == input$lp3ano,
                 categoria == input$lp3cat) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- lp3 %>%
          filter(localidade != "Pará",
                 ano == input$lp3ano,
                 categoria == input$lp3cat) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$lp3ri)
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
          c("#ffff81", "#fad254", "#f2a92a", "#ae520b", "#6b0000"),
          domain = x$valor,
          bins = bins
        )
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Quantidade Produzida:</b> %s ",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Sem Produção",
            format(x$valor, big.mark = ".", decimal.mark = ",")
          )
        ) %>% lapply(htmltools::HTML)
      
      # Mapas com leafleft
      leaflet(x,
              options =
                leafletOptions(minZoom = 0,
                               maxZoom = 15)) %>%
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
          values = ~ valor,
          opacity = 0.7,
          title = "Quantidade",
          position = "bottomright",
          na.label = "Sem Produção",
          labFormat = labelFormat_decimal(
            big.mark = ".",
            decimal.mark = ",",
            digits = 2
          )
        )
    })
    ## Tabela - Quantidade Produzida----
    output$lp3txt2 <- renderText({
      t32()
    })
    # Renderização da base lp3
    output$lp3tab <- renderReactable({
      if (input$lp3ri == "Pará") {
        x <- lp3 %>%
          filter(localidade != "Pará",
                 ano == input$lp3ano,
                 categoria == input$lp3cat) %>%
          select(ri, localidade, valor)
      } else {
        x <- lp3 %>%
          filter(localidade != "Pará",
                 ano == input$lp3ano,
                 categoria == input$lp3cat) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$lp3ri)
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
            name = "Quantidade",
            format = colFormat(separators = T,
                               locales = "pt-BR")
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
    ## Gráfico - Quantidade Produzida----
    output$lp3txt3 <- renderText({
      t33()
    })
    output$lp3graf <- renderEcharts4r({
      lp3 %>%
        filter(categoria == input$lp3cat, localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#f2a92a",
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
    })
    ## Tabela - Quantidade Produzida----
    output$lp3txt4 <- renderText({
      t34()
    })
    output$lp3tabcat <- renderReactable({
      x <- lp3 %>%
        filter(localidade == "Pará", ano == input$lp3ano) %>%
        select(categoria, valor) %>%
        arrange(desc(valor)) %>%
        mutate(Percentual = (valor / sum(valor)) * 100)
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
          categoria = colDef(name = "Produtos"),
          valor = colDef(
            name = "Quantidade",
            format = colFormat(separators = T,
                               locales = "pt-BR")
          ),
          Percentual = colDef(
            name = "Percentual(%)",
            format = colFormat(
              digits = 2,
              separators = T,
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
    # 4 - Valor da Produção----
    ## Mapa - Valor da Produção----
    output$lp4txt1 <- renderText({
      t41()
    })
    output$lp4map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$lp4ri == "Pará") {
        df <- lp4 %>%
          filter(localidade != "Pará",
                 ano == input$lp4ano,
                 categoria == input$lp4cat) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- lp4 %>%
          filter(localidade != "Pará",
                 ano == input$lp4ano,
                 categoria == input$lp4cat) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$lp4ri)
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
          c("#ffff81", "#fad254", "#f2a92a", "#ae520b", "#6b0000"),
          domain = x$valor,
          bins = bins
        )
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Valor (em mil reais):</b> %s ",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Sem Produção",
            format(x$valor, big.mark = ".", decimal.mark = ",")
          )
        ) %>% lapply(htmltools::HTML)
      
      # Mapas com leafleft
      leaflet(x,
              options =
                leafletOptions(minZoom = 0,
                               maxZoom = 15)) %>%
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
          values = ~ valor,
          opacity = 0.7,
          title = "Valor (em mil reais)",
          position = "bottomright",
          na.label = "Sem Produção",
          labFormat = labelFormat_decimal(
            big.mark = ".",
            decimal.mark = ",",
            digits = 2
          )
        )
    })
    ## Tabela - Valor da Produção----
    output$lp4txt2 <- renderText({
      t42()
    })
    # Renderização da base lp4
    output$lp4tab <- renderReactable({
      if (input$lp4ri == "Pará") {
        x <- lp4 %>%
          filter(localidade != "Pará",
                 ano == input$lp4ano,
                 categoria == input$lp4cat) %>%
          select(ri, localidade, valor)
      } else {
        x <- lp4 %>%
          filter(localidade != "Pará",
                 ano == input$lp4ano,
                 categoria == input$lp4cat) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$lp4ri)
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
            name = "Valor (em mil reais)",
            format = colFormat(separators = T,
                               locales = "pt-BR")
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
    ## Gráfico - Valor da Produção----
    output$lp4txt3 <- renderText({
      t43()
    })
    output$lp4graf <- renderEcharts4r({
      lp4 %>%
        filter(categoria == input$lp4cat, localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#f2a92a",
          name = "Valor (em mil reais)",
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
          name = "Valor(em mil reais)",
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
    })
    ## Tabela - Valor da Produção----
    output$lp4txt4 <- renderText({
      t44()
    })
    output$lp4tabcat <- renderReactable({
      x <- lp4 %>%
        filter(localidade == "Pará", ano == input$lp4ano) %>%
        select(categoria, valor) %>%
        arrange(desc(valor)) %>%
        mutate(Percentual = (valor / sum(valor)) * 100)
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
          categoria = colDef(name = "Produtos"),
          valor = colDef(
            name = "Valor (em mil reais)",
            format = colFormat(
              digits = 0,
              separators = T,
              locales = "pt-BR"
            )
          ),
          Percentual = colDef(
            name = "Percentual(%)",
            format = colFormat(
              digits = 2,
              separators = T,
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
    # 5 - Rendimento Médio da produção----
    ## Mapa - Rendimento Médio da produção----
    output$lp5txt1 <- renderText({
      t51()
    })
    output$lp5map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$lp5ri == "Pará") {
        df <- lp5 %>%
          filter(localidade != "Pará",
                 ano == input$lp5ano,
                 categoria == input$lp5cat) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- lp5 %>%
          filter(localidade != "Pará",
                 ano == input$lp5ano,
                 categoria == input$lp5cat) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$lp5ri)
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
          c("#ffff81", "#fad254", "#f2a92a", "#ae520b", "#6b0000"),
          domain = x$valor,
          bins = bins
        )
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Rendimento Médio:</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Sem Produção",
            format(x$valor, big.mark = ".", decimal.mark = ",")
          )
        ) %>% lapply(htmltools::HTML)
      
      # Mapas com leafleft
      leaflet(x,
              options =
                leafletOptions(minZoom = 0,
                               maxZoom = 15)) %>%
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
          values = ~ valor,
          opacity = 0.7,
          title = "Rendimento Médio",
          position = "bottomright",
          na.label = "Sem Produção",
          labFormat = labelFormat_decimal(
            big.mark = ".",
            decimal.mark = ",",
            digits = 2
          )
        )
    })
    ## Tabela - Rendimento Médio da produção----
    output$lp5txt2 <- renderText({
      t52()
    })
    # Renderização da base lp5
    output$lp5tab <- renderReactable({
      if (input$lp5ri == "Pará") {
        x <- lp5 %>%
          filter(localidade != "Pará",
                 ano == input$lp5ano,
                 categoria == input$lp5cat) %>%
          select(ri, localidade, valor)
      } else {
        x <- lp5 %>%
          filter(localidade != "Pará",
                 ano == input$lp5ano,
                 categoria == input$lp5cat) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$lp5ri)
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
            name = "Rendimento Médio",
            format = colFormat(separators = T,
                               locales = "pt-BR")
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
    ## Gráfico - Rendimento Médio da produção----
    output$lp5txt3 <- renderText({
      t53()
    })
    output$lp5graf <- renderEcharts4r({
      lp5 %>%
        filter(categoria == input$lp5cat, localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#f2a92a",
          name = "Rendimento Médio",
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
          name = "Rendimento Médio",
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
    })
    ## Tabela - Rendimento Médio da produção----
    output$lp5txt4 <- renderText({
      t54()
    })
    output$lp5tabcat <- renderReactable({
      x <- lp5 %>%
        filter(localidade == "Pará", ano == input$lp5ano) %>%
        select(categoria, valor) %>%
        arrange(desc(valor)) %>%
        mutate(Percentual = (valor / sum(valor)) * 100)
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
          categoria = colDef(name = "Produtos"),
          calor = colDef(
            name = "Rendimento Médio",
            format = colFormat(separators = T,
                               locales = "pt-BR")
          ),
          Percentual = colDef(
            name = "Percentual(%)",
            format = colFormat(
              digits = 2,
              separators = T,
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
    #DOWNLOADS----
    # 1 - Área Destinada à Colheita----
    ## - Tabela - Área Destinada à Colheita----
    # Filtra os dados
    lp1_1 <- reactive({
      if (input$lp1ri == "Pará") {
        x <- lp1 %>%
          filter(localidade != "Pará",
                 ano == input$lp1ano,
                 categoria == input$lp1cat)
      } else {
        x <- lp1 %>%
          filter(
            localidade != "Pará",
            ano == input$lp1ano,
            categoria == input$lp1cat,
            ri == input$lp1ri
          )
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(lp1_1(), {
      t12()
      downset_Server("lp1_1", lp1_1(), t12())
    })
    
    ## - Gráfico - Área Destinada à Colheita----
    # Filtra os dados
    lp1_2 <- reactive({
      lp1 %>%
        filter(categoria == input$lp1cat,
               localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(lp1_2(), {
      t13()
      downset_Server("lp1_2", lp1_2(), t13())
    })
    
    ## - Tabela - Área Destinada à Colheita----
    # Filtra os dados
    lp1_3 <- reactive({
      x <- lp1 %>%
        filter(localidade == "Pará", ano == input$lp1ano) %>%
        arrange(desc(valor)) %>%
        mutate(Percentual = (valor / sum(valor)) * 100)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(lp1_3(), {
      t14()
      downset_Server("lp1_3", lp1_3(), t14())
    })
    
    
    # 2 - Área Colhida à Colheita----
    ## - Tabela - Área Colhida à Colheita----
    # Filtra os dados
    lp2_1 <- reactive({
      if (input$lp2ri == "Pará") {
        x <- lp2 %>%
          filter(localidade != "Pará",
                 ano == input$lp2ano,
                 categoria == input$lp2cat)
      } else {
        x <- lp2 %>%
          filter(
            localidade != "Pará",
            ano == input$lp2ano,
            categoria == input$lp2cat,
            ri == input$lp2ri
          )
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(lp2_1(), {
      t22()
      downset_Server("lp2_1", lp2_1(), t22())
    })
    
    ## - Gráfico - Área Colhida à Colheita----
    # Filtra os dados
    lp2_2 <- reactive({
      lp2 %>%
        filter(categoria == input$lp2cat,
               localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(lp2_2(), {
      t23()
      downset_Server("lp2_2", lp2_2(), t23())
    })
    
    ## - Tabela - Área Colhida à Colheita----
    # Filtra os dados
    lp2_3 <- reactive({
      x <- lp2 %>%
        filter(localidade == "Pará",
               ano == input$lp2ano) %>%
        arrange(desc(valor)) %>%
        mutate(Percentual = (valor / sum(valor)) * 100)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(lp2_3(), {
      t24()
      downset_Server("lp2_3", lp2_3(), t24())
    })
    # 3 - Quantidade Produzida----
    ## - Tabela - Quantidade Produzida----
    # Filtra os dados
    lp3_1 <- reactive({
      if (input$lp3ri == "Pará") {
        x <- lp3 %>%
          filter(localidade != "Pará",
                 ano == input$lp3ano,
                 categoria == input$lp3cat)
      } else {
        x <- lp3 %>%
          filter(
            localidade != "Pará",
            ano == input$lp3ano,
            categoria == input$lp3cat,
            ri == input$lp3ri
          )
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(lp3_1(), {
      t32()
      downset_Server("lp3_1", lp3_1(), t32())
    })
    
    ## - Gráfico - Quantidade Produzida----
    # Filtra os dados
    lp3_2 <- reactive({
      lp3 %>%
        filter(categoria == input$lp3cat,
               localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(lp3_2(), {
      t33()
      downset_Server("lp3_2", lp3_2(), t33())
    })
    
    ## - Tabela - Quantidade Produzida----
    # Filtra os dados
    lp3_3 <- reactive({
      x <- lp3 %>%
        filter(localidade == "Pará",
               ano == input$lp3ano) %>%
        arrange(desc(valor)) %>%
        mutate(Percentual = (valor / sum(valor)) * 100)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(lp3_3(), {
      t34()
      downset_Server("lp3_3", lp3_3(), t34())
    })
    
    # 4 - Valor da Produção----
    ## - Tabela - Valor da Produção----
    # Filtra os dados
    lp4_1 <- reactive({
      if (input$lp4ri == "Pará") {
        x <- lp4 %>%
          filter(localidade != "Pará",
                 ano == input$lp4ano,
                 categoria == input$lp4cat)
      } else {
        x <- lp4 %>%
          filter(
            localidade != "Pará",
            ano == input$lp4ano,
            categoria == input$lp4cat,
            ri == input$lp4ri
          )
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(lp4_1(), {
      t42()
      downset_Server("lp4_1", lp4_1(), t42())
    })
    
    ## - Gráfico - Valor da Produção----
    # Filtra os dados
    lp4_2 <- reactive({
      lp4 %>%
        filter(categoria == input$lp4cat,
               localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(lp4_2(), {
      t43()
      downset_Server("lp4_2", lp4_2(), t43())
    })
    
    ## - Tabela - Valor da Produção----
    # Filtra os dados
    lp4_3 <- reactive({
      x <- lp4 %>%
        filter(localidade == "Pará",
               ano == input$lp4ano) %>%
        arrange(desc(valor)) %>%
        mutate(Percentual = (valor / sum(valor)) * 100)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(lp4_3(), {
      t44()
      downset_Server("lp4_3", lp4_3(), t44())
    })
    
    
    # 5 - Rendimento Médio da produção----
    ## Tabela - Rendimento Médio da produção----
    # Filtra os dados
    lp5_1 <- reactive({
      if (input$lp5ri == "Pará") {
        x <- lp5 %>%
          filter(localidade != "Pará",
                 ano == input$lp5ano,
                 categoria == input$lp5cat)
      } else {
        x <- lp5 %>%
          filter(
            localidade != "Pará",
            ano == input$lp5ano,
            categoria == input$lp5cat,
            ri == input$lp5ri
          )
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(lp5_1(), {
      t52()
      downset_Server("lp5_1", lp5_1(), t52())
    })
    
    ## Gráfico - Rendimento Médio da produção----
    # Filtra os dados
    lp5_2 <- reactive({
      lp5 %>%
        filter(categoria == input$lp5cat,
               localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(lp5_2(), {
      t53()
      downset_Server("lp5_2", lp5_2(), t53())
    })
    
    ## Tabela - Rendimento Médio da produção----
    # Filtra os dados
    lp5_3 <- reactive({
      x <- lp5 %>%
        filter(localidade == "Pará",
               ano == input$lp5ano) %>%
        arrange(desc(valor)) %>%
        mutate(Percentual = (valor / sum(valor)) * 100)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(lp5_3(), {
      t54()
      downset_Server("lp5_3", lp5_3(), t54())
    })
  })
}

#Play do Módulo
# ui <- dashboardPage(header = dashboardHeader(),
#                     sidebar = dashboardSidebar(),
#                     body = dashboardBody(fluidPage(economia_lp_pa_ui("economia_lp_pa"))))
# 
# server <- function(input, output) {
#   economia_lp_pa_Server("economia_lp_pa")
# }
# 
# shinyApp(ui, server)
