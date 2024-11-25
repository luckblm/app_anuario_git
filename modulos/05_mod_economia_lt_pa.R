# Funções de módulo de Economia - Lavoura Temporária - Estadual
# Função de UI
economia_lt_pa_ui <- function(id) {
  fluidPage(# CAMINHO DO ARQUIVO CSS----
            tags$head(
              tags$link(rel = "stylesheet", type = "text/css", href = "estilo.css")
            ),
            # Lista de Navegação lateral----
            div(class = "navbar_economia",
                navbarPage(
                  tags$b("Lavoura Temporária - Pará"),
                  navbarMenu(
                    tags$b("Escolha um Indicador"),
                    # 1 - Área plantada----
                    tabPanel(
                      "Área plantada",
                      panel(
                        ## Controle----
                        heading =
                          h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                             "Área plantada"),
                        tags$div(
                          class = "seletor1",
                          pickerInput(
                            inputId = NS(id, "lt1ano"),
                            label = "Ano",
                            choices = sort(unique(lt1[["ano"]]), decreasing = T),
                            width = "100px"
                          )
                        ),
                        tags$div(
                          class = "seletor2",
                          pickerInput(
                            inputId = NS(id, "lt1ri"),
                            label = "Pará/Região de Integração",
                            choices = unique(lt1[["ri"]]),
                            width = "200px"
                          )
                        ),
                        tags$div(
                          class = "seletor3",
                          pickerInput(
                            inputId = NS(id, "lt1cat"),
                            label = "Produto",
                            choices = unique(lt1[["categoria"]]),
                            width = "200px"
                          )
                        )
                      ),
                      fluidRow(
                        box(
                          ## Mapa - Área plantada em hectares----
                          title = textOutput(NS(id, "lt1txt1")),
                          collapsible = T,
                          collapsed = F,
                          headerBorder = T,
                          width = 12,
                          withSpinner(
                            leafletOutput(NS(id, "lt1map"), height = "600px"),
                            type = 8,
                            color = "#f2a92a",
                            size = 0.5
                          ),
                          footer = list(tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                                        tags$h6(tags$b("Elaboração:"), "FAPESPA"))
                        ),
                        ## Tabela - Área plantada em hectares----
                        box(
                          title = textOutput(NS(id, "lt1txt2")),
                          collapsible = T,
                          collapsed = F,
                          width = 12,
                          headerBorder = T,
                          solidHeader = F,
                          withSpinner(
                            reactableOutput(NS(id, "lt1tab"), height = "400px"),
                            type = 8,
                            color = "#f2a92a",
                            size = 0.5
                          ),
                          footer = list(column(
                            11,
                            tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA")
                          ),
                          downset_ui(NS(id, "lt1_1")))
                        )
                      ),
                      fluidRow(
                        ## Gráfico - Área plantada em hectares----
                        box(
                          title = textOutput(NS(id, "lt1txt3")),
                          collapsible = T,
                          collapsed = F,
                          width = 12,
                          headerBorder = T,
                          solidHeader = F,
                          withSpinner(
                            echarts4rOutput(NS(id, "lt1graf")),
                            type = 8,
                            color = "#f2a92a",
                            size = 0.5
                          ),
                          footer = list(column(
                            11,
                            tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA")
                          ),
                          downset_ui(NS(id, "lt1_2")))
                        ),
                        ## Tabela - Área plantada em hectares----
                        box(
                          title = textOutput(NS(id, "lt1txt4")),
                          collapsible = T,
                          collapsed = F,
                          width = 12,
                          headerBorder = T,
                          solidHeader = F,
                          withSpinner(
                            reactableOutput(NS(id, "lt1tabcat")),
                            type = 8,
                            color = "#f2a92a",
                            size = 0.5
                          ),
                          footer = list(column(
                            11,
                            tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA")
                          ),
                          downset_ui(NS(id, "lt1_3")))
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
                            inputId = NS(id, "lt2ano"),
                            label = "Ano",
                            choices = sort(unique(lt2[["ano"]]), decreasing = T),
                            width = "100px"
                          )
                        ),
                        tags$div(
                          class = "seletor2",
                          pickerInput(
                            inputId = NS(id, "lt2ri"),
                            label = "Pará/Região de Integração",
                            choices = unique(lt2[["ri"]]),
                            width = "200px"
                          )
                        ),
                        tags$div(
                          class = "seletor3",
                          pickerInput(
                            inputId = NS(id, "lt2cat"),
                            label = "Produto",
                            choices = unique(lt2[["categoria"]]),
                            width = "200px"
                          )
                        )
                      ),
                      fluidRow(
                        ## Mapa - Área Colhida à Colheita----
                        box(
                          title = textOutput(NS(id, "lt2txt1")),
                          collapsible = T,
                          collapsed = F,
                          headerBorder = T,
                          width = 12,
                          withSpinner(
                            leafletOutput(NS(id, "lt2map"), height = "600px"),
                            type = 8,
                            color = "#f2a92a",
                            size = 0.5
                          ),
                          footer = list(tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                                        tags$h6(tags$b("Elaboração:"), "FAPESPA"))
                        ),
                        ## Tabela - Área Colhida à Colheita----
                        box(
                          title = textOutput(NS(id, "lt2txt2")),
                          collapsible = T,
                          collapsed = F,
                          width = 12,
                          headerBorder = T,
                          solidHeader = F,
                          withSpinner(
                            reactableOutput(NS(id, "lt2tab"), height = "600px"),
                            type = 8,
                            color = "#f2a92a",
                            size = 0.5
                          ),
                          footer = list(column(
                            11,
                            tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA")
                          ),
                          downset_ui(NS(id, "lt2_1")))
                        )
                      ),
                      fluidRow(
                        ## Gráfico - Área Colhida à Colheita----
                        box(
                          title = textOutput(NS(id, "lt2txt3")),
                          collapsible = T,
                          collapsed = F,
                          width = 12,
                          headerBorder = T,
                          solidHeader = F,
                          withSpinner(
                            echarts4rOutput(NS(id, "lt2graf")),
                            type = 8,
                            color = "#f2a92a",
                            size = 0.5
                          ),
                          footer = list(column(
                            11,
                            tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA")
                          ),
                          downset_ui(NS(id, "lt2_2")))
                        ),
                        ## Tabela - Área Colhida à Colheita----
                        box(
                          title = textOutput(NS(id, "lt2txt4")),
                          collapsible = T,
                          collapsed = F,
                          width = 12,
                          headerBorder = T,
                          solidHeader = F,
                          withSpinner(
                            reactableOutput(NS(id, "lt2tabcat")),
                            type = 8,
                            color = "#f2a92a",
                            size = 0.5
                          ),
                          footer = list(column(
                            11,
                            tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA")
                          ),
                          downset_ui(NS(id, "lt2_3")))
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
                          pickerInput(
                            inputId = NS(id, "lt3ano"),
                            label = "Ano",
                            choices = sort(unique(lt3[["ano"]]), decreasing = T),
                            width = "100px"
                          )
                        ),
                        tags$div(
                          class = "seletor2",
                          pickerInput(
                            inputId = NS(id, "lt3ri"),
                            label = "Pará/Região de Integração",
                            choices = unique(lt3[["ri"]]),
                            width = "200px"
                          )
                        ),
                        tags$div(
                          class = "seletor3",
                          pickerInput(
                            inputId = NS(id, "lt3cat"),
                            label = "Produto",
                            choices = unique(lt3[["categoria"]]),
                            width = "200px"
                          )
                        )
                      ),
                      fluidRow(
                        ## Mapa - Quantidade Produzida----
                        box(
                          title = textOutput(NS(id, "lt3txt1")),
                          collapsible = T,
                          collapsed = F,
                          headerBorder = T,
                          width = 12,
                          withSpinner(
                            leafletOutput(NS(id, "lt3map"), height = "600px"),
                            type = 8,
                            color = "#f2a92a",
                            size = 0.5
                          ),
                          footer = list(tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                                        tags$h6(tags$b("Elaboração:"), "FAPESPA"))
                        ),
                        ## Tabela - Quantidade Produzida----
                        box(
                          title = textOutput(NS(id, "lt3txt2")),
                          collapsible = T,
                          collapsed = F,
                          width = 12,
                          headerBorder = T,
                          solidHeader = F,
                          withSpinner(
                            reactableOutput(NS(id, "lt3tab"), height = "600px"),
                            type = 8,
                            color = "#f2a92a",
                            size = 0.5
                          ),
                          footer = list(column(
                            11,
                            tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA")
                          ),
                          downset_ui(NS(id, "lt3_1")))
                        )
                      ),
                      ## Gráfico - Quantidade Produzida----
                      fluidRow(
                        box(
                          title = textOutput(NS(id, "lt3txt3")),
                          collapsible = T,
                          collapsed = F,
                          width = 12,
                          headerBorder = T,
                          solidHeader = F,
                          withSpinner(
                            echarts4rOutput(NS(id, "lt3graf")),
                            type = 8,
                            color = "#f2a92a",
                            size = 0.5
                          ),
                          footer = list(column(
                            11,
                            tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA")
                          ),
                          downset_ui(NS(id, "lt3_2")))
                        ),
                        ## Tabela - Quantidade Produzida----
                        box(
                          title = textOutput(NS(id, "lt3txt4")),
                          collapsible = T,
                          collapsed = F,
                          width = 12,
                          headerBorder = T,
                          solidHeader = F,
                          withSpinner(
                            reactableOutput(NS(id, "lt3tabcat")),
                            type = 8,
                            color = "#f2a92a",
                            size = 0.5
                          ),
                          footer = list(column(
                            11,
                            tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA")
                          ),
                          downset_ui(NS(id, "lt3_3")))
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
                          # Selecionar
                          pickerInput(
                            inputId = NS(id, "lt4ano"),
                            label = "Ano",
                            choices = sort(unique(lt4[["ano"]]), decreasing = T),
                            width = "100px"
                          )
                        ),
                        tags$div(
                          class = "seletor2",
                          # select R.I
                          pickerInput(
                            inputId = NS(id, "lt4ri"),
                            label = "Pará/Região de Integração",
                            choices = unique(lt4[["ri"]]),
                            width = "200px"
                          )
                        ),
                        tags$div(
                          class = "seletor3",
                          # select R.I
                          pickerInput(
                            inputId = NS(id, "lt4cat"),
                            label = "Produto",
                            choices = unique(lt4[["categoria"]]),
                            width = "200px"
                          )
                        )
                      ),
                      fluidRow(
                        ## Mapa - Valor da Produção----
                        box(
                          title = textOutput(NS(id, "lt4txt1")),
                          collapsible = T,
                          collapsed = F,
                          headerBorder = T,
                          width = 12,
                          withSpinner(
                            leafletOutput(NS(id, "lt4map"), height = "600px"),
                            type = 8,
                            color = "#f2a92a",
                            size = 0.5
                          ),
                          footer = list(tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                                        tags$h6(tags$b("Elaboração:"), "FAPESPA"))
                        ),
                        ## Tabela - Valor da Produção----
                        box(
                          title = textOutput(NS(id, "lt4txt2")),
                          collapsible = T,
                          collapsed = F,
                          width = 12,
                          headerBorder = T,
                          solidHeader = F,
                          withSpinner(
                            reactableOutput(NS(id, "lt4tab"), height = "600px"),
                            type = 8,
                            color = "#f2a92a",
                            size = 0.5
                          ),
                          footer = list(column(
                            11,
                            tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA")
                          ),
                          downset_ui(NS(id, "lt4_1")))
                        )
                      ),
                      fluidRow(
                        ## Gráfico - Valor da Produção----
                        box(
                          title = textOutput(NS(id, "lt4txt3")),
                          collapsible = T,
                          collapsed = F,
                          width = 12,
                          headerBorder = T,
                          solidHeader = F,
                          withSpinner(
                            echarts4rOutput(NS(id, "lt4graf")),
                            type = 8,
                            color = "#f2a92a",
                            size = 0.5
                          ),
                          footer = list(column(
                            11,
                            tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA")
                          ),
                          downset_ui(NS(id, "lt4_2")))
                        ),
                        ## Tabela - Valor da Produção----
                        box(
                          title = textOutput(NS(id, "lt4txt4")),
                          collapsible = T,
                          collapsed = F,
                          width = 12,
                          headerBorder = T,
                          solidHeader = F,
                          withSpinner(
                            reactableOutput(NS(id, "lt4tabcat")),
                            type = 8,
                            color = "#f2a92a",
                            size = 0.5
                          ),
                          footer = list(column(
                            11,
                            tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA")
                          ),
                          downset_ui(NS(id, "lt4_3")))
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
                            inputId = NS(id, "lt5ano"),
                            label = "Ano",
                            choices = sort(unique(lt5[["ano"]]), decreasing = T),
                            width = "100px"
                          )
                        ),
                        tags$div(
                          class = "seletor2",
                          pickerInput(
                            inputId = NS(id, "lt5ri"),
                            label = "Pará/Região de Integração",
                            choices = unique(lt5[["ri"]]),
                            width = "200px"
                          )
                        ),
                        tags$div(
                          class = "seletor3",
                          pickerInput(
                            inputId = NS(id, "lt5cat"),
                            label = "Produto",
                            choices = unique(lt5[["categoria"]]),
                            width = "200px"
                          )
                        )
                      ),
                      fluidRow(
                        ## Mapa - Rendimento Médio da produção----
                        box(
                          title = textOutput(NS(id, "lt5txt1")),
                          collapsible = T,
                          collapsed = F,
                          headerBorder = T,
                          width = 12,
                          withSpinner(
                            leafletOutput(NS(id, "lt5map"), height = "600px"),
                            type = 8,
                            color = "#f2a92a",
                            size = 0.5
                          ),
                          footer = list(tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                                        tags$h6(tags$b("Elaboração:"), "FAPESPA"))
                        ),
                        ## Tabela - Rendimento Médio da produção----
                        box(
                          title = textOutput(NS(id, "lt5txt2")),
                          collapsible = T,
                          collapsed = F,
                          width = 12,
                          headerBorder = T,
                          solidHeader = F,
                          withSpinner(
                            reactableOutput(NS(id, "lt5tab"), height = "600px"),
                            type = 8,
                            color = "#f2a92a",
                            size = 0.5
                          ),
                          footer = list(column(
                            11,
                            tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA")
                          ),
                          downset_ui(NS(id, "lt5_1")))
                        )
                      ),
                      fluidRow(
                        ## Gráfico - Rendimento Médio da produção----
                        box(
                          title = textOutput(NS(id, "lt5txt3")),
                          collapsible = T,
                          collapsed = F,
                          width = 12,
                          headerBorder = T,
                          solidHeader = F,
                          withSpinner(
                            echarts4rOutput(NS(id, "lt5graf")),
                            type = 8,
                            color = "#f2a92a",
                            size = 0.5
                          ),
                          footer = list(column(
                            11,
                            tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA")
                          ),
                          downset_ui(NS(id, "lt5_2")))
                        ),
                        ## Tabela - Rendimento Médio da produção----
                        box(
                          title = textOutput(NS(id, "lt5txt4")),
                          collapsible = T,
                          collapsed = F,
                          width = 12,
                          headerBorder = T,
                          solidHeader = F,
                          withSpinner(
                            reactableOutput(NS(id, "lt5tabcat")),
                            type = 8,
                            color = "#f2a92a",
                            size = 0.5
                          ),
                          footer = list(column(
                            11,
                            tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA")
                          ),
                          downset_ui(NS(id, "lt5_3")))
                        )
                      )
                    )
                  )
                )))
}
# Função do modulo servidor
economia_lt_pa_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    #TÍTULOS----
    # 1-Área plantada em hectares----
    ## Mapa - Área plantada em hectares (Hectares)----
    t11 <- reactive({
      if (input$lt1ri == "Pará") {
        paste0(
          "Área Plantada (em hectares) da Lavoura Temporária de ",
          input$lt1cat,
          ", Pará - ",
          input$lt1ano
        )
      } else {
        paste0(
          "Área Plantada (em hectares) da Lavoura Temporária de ",
          input$lt1cat,
          ", Região de Integração ",
          input$lt1ri,
          " - ",
          input$lt1ano
        )
      }
    })
    ## Tabela - Área plantada em hectares----
    t12 <- reactive({
      if (input$lt1ri == "Pará") {
        paste0(
          "Área Plantada (em hectares) da Lavoura Temporária de ",
          input$lt1cat,
          " dos Municípios, Pará - ",
          input$lt1ano
        )
      } else {
        paste0(
          "Área Plantada (em hectares) da Lavoura Temporária de ",
          input$lt1cat,
          " dos Municípios, Região de Integração ",
          input$lt1ri,
          " - ",
          input$lt1ano
        )
      }
    })
    ## Gráfico - Área plantada em hectares----
    t13 <- reactive({
      paste0(
        "Área Total Plantada (em hectares) da Lavoura Temporária",
        input$lt1cat,
        ",Pará - ",
        min(lt1$ano),
        " a ",
        max(lt1$ano)
      )
    })
    ## Tabela - Área plantada em hectares----
    t14 <- reactive({
      paste0(
        "Tabela - Área Total Plantada (em hectares) por Tipo de Lavoura Temporária, Pará - ",
        input$lt1ano
      )
    })
    # 2-Área Colhida à Colheita----
    ## Mapa - Área Colhida à Colheita (Hectares)----
    t21 <- reactive({
      if (input$lt2ri == "Pará") {
        paste0(
          "Área Colhida (em hectares) da Lavoura Temporária de ",
          input$lt2cat,
          ", Pará - ",
          input$lt2ano
        )
      } else {
        paste0(
          "Área Colhida (em hectares) da Lavoura Temporária de ",
          input$lt2cat,
          ", Região de Integração ",
          input$lt2ri,
          " - ",
          input$lt2ano
        )
      }
    })
    ## Tabela - Área Colhida à Colheita----
    t22 <- reactive({
      if (input$lt2ri == "Pará") {
        paste0(
          "Área Colhida (em hectares) da Lavoura Temporária de ",
          input$lt2cat,
          " dos Municípios, Pará - ",
          input$lt2ano
        )
      } else {
        paste0(
          "Área Colhida (em hectares) da Lavoura Temporária de ",
          input$lt2cat,
          " dos Municípios, Região de Integração ",
          input$lt2ri,
          " - ",
          input$lt2ano
        )
      }
    })
    ## Gráfico - Área Colhida à Colheita----
    t23 <- reactive({
      paste0(
        "Área Total Colhida (em hectares) da Lavoura Temporária de ",
        input$lt2cat,
        ", Pará - ",
        min(lt2$ano),
        " a ",
        max(lt2$ano)
      )
    })
    ## Tabela - Área Colhida à Colheita----
    t24 <- reactive({
      paste0(
        "Área Total Colhida (em hectares) por Tipo de Lavoura Temporária, Pará - ",
        input$lt2ano
      )
    })
    # 3 - Quantidade Produzida----
    ## Mapa - Quantidade Produzida----
    t31 <- reactive({
      if (input$lt3ri == "Pará") {
        paste0(
          "Quantidade Produzida da Lavoura Temporária de ",
          input$lt3cat,
          ", Pará - ",
          input$lt3ano
        )
      } else {
        paste0(
          "Quantidade Produzida da Lavoura Temporária de ",
          input$lt3cat,
          ", Região de Integração ",
          input$lt3ri,
          " - ",
          input$lt3ano
        )
      }
    })
    ## Tabela - Quantidade Produzida----
    t32 <- reactive({
      if (input$lt3ri == "Pará") {
        paste0(
          "Quantidade Produzida da Lavoura Temporária de ",
          input$lt3cat,
          " dos Municípios, Pará - ",
          input$lt3ano
        )
      } else {
        paste0(
          "Quantidade Produzida da Lavoura Temporária de ",
          input$lt3cat,
          " dos Municípios, Região de Integração ",
          input$lt3ri,
          " - ",
          input$lt3ano
        )
      }
    })
    ## Gráfico - Quantidade Produzida----
    t33 <- reactive({
      paste0(
        "Quantidade Total Produzida da Lavoura Temporária de ",
        input$lt3cat,
        ", Pará - ",
        min(lp3$ano),
        " a ",
        max(lp3$ano)
        
      )
    })
    ## Tabela - Quantidade Produzida----
    t34 <- reactive({
      paste0("Quantidade Total Produzida por Tipo de Lavoura Temporária, Pará - ",
             input$lt3ano)
    })
    # 4 - Valor da Produção----
    ## Mapa - Valor da Produção----
    t41 <- reactive({
      if (input$lt4ri == "Pará") {
        paste0(
          "Área Plantada (em hectares) da Lavoura Temporária de ",
          input$lt4cat,
          ", Pará - ",
          input$lt4ano
        )
      } else {
        paste0(
          "Área Plantada (em hectares) da Lavoura Temporária de ",
          input$lt4cat,
          " dos Municípios, Região de Integração ",
          input$lt4ri,
          " - ",
          input$lt4ano
        )
      }
    })
    ## Tabela - Valor da Produção----
    t42 <- reactive({
      if (input$lt4ri == "Pará") {
        paste0(
          "Área Plantada (em hectares) da Lavoura Temporária de ",
          input$lt4cat,
          " dos Municípios, Pará - ",
          input$lt4ano
        )
      } else {
        paste0(
          "Área Plantada (em hectares) da Lavoura Temporária de ",
          input$lt4cat,
          " dos Municípios, Região de Integração ",
          input$lt4ri,
          " - ",
          input$lt4ano
        )
      }
    })
    ## Gráfico - Valor da Produção----
    t43 <- reactive({
      paste0(
        "Valor Total (Mil Reais) da Produção da Lavoura Temporária de " ,
        input$lt4cat,
        ",Pará - ",
        min(lp4$ano),
        " a ",
        max(lp4$ano)
      )
    })
    ## Tabela - Valor da Produção----
    t44 <- reactive({
      paste0(
        "Valor Total (Mil Reais) da Produção por Tipo de Lavoura Temporária Pará - ",
        input$lt4ano
      )
    })
    # 5 - Rendimento Médio da produção----
    ## Mapa - Rendimento Médio da produção----
    t51 <- reactive({
      if (input$lt5ri == "Pará") {
        paste0(
          "Rendimento Médio da Produção da Lavoura Temporária de ",
          input$lt5cat,
          ", Pará - ",
          input$lt5ano
        )
      } else {
        paste0(
          "Rendimento Médio da Produção da Lavoura Temporária de ",
          input$lt5cat,
          ", Região de Integração ",
          input$lt5ri,
          " - ",
          input$lt5ano
        )
      }
    })
    ## Tabela - Rendimento Médio da produção----
    t52 <- reactive({
      if (input$lt5ri == "Pará") {
        paste0(
          "Rendimento Médio da Produção da Lavoura Temporária de ",
          input$lt5cat,
          " dos Municípios, Pará - ",
          input$lt5ano
        )
      } else {
        paste0(
          "Rendimento Médio da Produção da Lavoura Temporária de ",
          input$lt5cat,
          " dos Municípios, Região de Integração ",
          input$lt5ri,
          " - ",
          input$lt5ano
        )
      }
    })
    ## Gráfico - Rendimento Médio da produção----
    t53 <- reactive({
      paste0(
        "Rendimento Médio da Produção da Lavoura Temporária de " ,
        input$lt5cat,
        ",Pará - ",
        min(lp5$ano),
        " a ",
        max(lp5$ano)
      )
    })
    ## Tabela - Rendimento Médio da produção----
    t54 <- reactive({
      paste0("Rendimento Médio da Produção por Tipo de Lavoura Temporária, Pará - ",
             input$lt5ano)
    })
    #VISUALIZAÇÃO----
    # 1-Área plantada em hectares----
    ## Mapa - Área plantada em hectares (Hectares)----
    output$lt1txt1 <- renderText({
      t11()
    })
    output$lt1map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$lt1ri == "Pará") {
        df <- lt1 %>%
          filter(localidade != "Pará",
                 ano == input$lt1ano,
                 categoria == input$lt1cat) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- lt1 %>%
          filter(localidade != "Pará",
                 ano == input$lt1ano,
                 categoria == input$lt1cat) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$lt1ri)
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
    ## Tabela - Área plantada em hectares----
    output$lt1txt2 <- renderText({
      t12()
    })
    # Renderização da base lt1
    output$lt1tab <- renderReactable({
      if (input$lt1ri == "Pará") {
        x <- lt1 %>%
          filter(localidade != "Pará",
                 ano == input$lt1ano,
                 categoria == input$lt1cat) %>%
          select(ri, localidade, valor)
      } else {
        x <- lt1 %>%
          filter(localidade != "Pará",
                 ano == input$lt1ano,
                 categoria == input$lt1cat) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$lt1ri)
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
            format = colFormat(separators = T, locales = "pt-BR")
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
    ## Gráfico - Área plantada em hectares----
    output$lt1txt3 <- renderText({
      t13()
    })
    output$lt1graf <- renderEcharts4r({
      lt1 %>%
        filter(categoria == input$lt1cat, localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#f2a92a",
          name = "Área(ha)",
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
    ## Tabela - Área plantada em hectares----
    output$lt1txt4 <- renderText({
      t14()
    })
    output$lt1tabcat <- renderReactable({
      x <- lt1 %>%
        filter(localidade == "Pará", ano == input$lt1ano) %>%
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
    # 2-Área Colhida à Colheita----
    ## Mapa - Área Colhida à Colheita (Hectares)----
    output$lt2txt1 <- renderText({
      t21()
    })
    output$lt2map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$lt2ri == "Pará") {
        df <- lt2 %>%
          filter(localidade != "Pará",
                 ano == input$lt2ano,
                 categoria == input$lt2cat) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- lt2 %>%
          filter(localidade != "Pará",
                 ano == input$lt2ano,
                 categoria == input$lt2cat) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$lt2ri)
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
    ## Tabela - Área Colhida à Colheita----
    output$lt2txt2 <- renderText({
      t22()
    })
    # Renderização da base lt2
    output$lt2tab <- renderReactable({
      if (input$lt2ri == "Pará") {
        x <- lt2 %>%
          filter(localidade != "Pará",
                 ano == input$lt2ano,
                 categoria == input$lt2cat) %>%
          select(ri, localidade, valor)
      } else {
        x <- lt2 %>%
          filter(localidade != "Pará",
                 ano == input$lt2ano,
                 categoria == input$lt2cat) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$lt2ri)
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
    output$lt2txt3 <- renderText({
      t23()
    })
    output$lt2graf <- renderEcharts4r({
      lt2 %>%
        filter(categoria == input$lt2cat, localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#f2a92a",
          name = "Área(ha)",
          legend = F,
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
    ## Tabela - Área Colhida à Colheita----
    output$lt2txt4 <- renderText({
      t24()
    })
    output$lt2tabcat <- renderReactable({
      x <- lt2 %>%
        filter(localidade == "Pará", ano == input$lt2ano) %>%
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
    output$lt3txt1 <- renderText({
      t31()
    })
    output$lt3map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$lt3ri == "Pará") {
        df <- lt3 %>%
          filter(localidade != "Pará",
                 ano == input$lt3ano,
                 categoria == input$lt3cat) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- lt3 %>%
          filter(localidade != "Pará",
                 ano == input$lt3ano,
                 categoria == input$lt3cat) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$lt3ri)
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
          "<strong>%s</strong><br/> <b>Quantidade:</b> %s ",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Sem Produção",
            format(x$valor, big.mark = ".", decimal.mark = ",")
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
    output$lt3txt2 <- renderText({
      t32()
    })
    # Renderização da base lt3
    output$lt3tab <- renderReactable({
      if (input$lt3ri == "Pará") {
        x <- lt3 %>%
          filter(localidade != "Pará",
                 ano == input$lt3ano,
                 categoria == input$lt3cat) %>%
          select(ri, localidade, valor)
      } else {
        x <- lt3 %>%
          filter(localidade != "Pará",
                 ano == input$lt3ano,
                 categoria == input$lt3cat) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$lt3ri)
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
    output$lt3txt3 <- renderText({
      t33()
    })
    output$lt3graf <- renderEcharts4r({
      lt3 %>%
        filter(categoria == input$lt3cat, localidade == "Pará") %>%
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
    ## Tabela - Quantidade Produzida----
    output$lt3txt4 <- renderText({
      t34()
    })
    output$lt3tabcat <- renderReactable({
      x <- lt3 %>%
        filter(localidade == "Pará", ano == input$lt3ano) %>%
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
    output$lt4txt1 <- renderText({
      t41()
    })
    output$lt4map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$lt4ri == "Pará") {
        df <- lt4 %>%
          filter(localidade != "Pará",
                 ano == input$lt4ano,
                 categoria == input$lt4cat) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- lt4 %>%
          filter(localidade != "Pará",
                 ano == input$lt4ano,
                 categoria == input$lt4cat) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$lt4ri)
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
    output$lt4txt2 <- renderText({
      t42()
    })
    # Renderização da base lt4
    output$lt4tab <- renderReactable({
      if (input$lt4ri == "Pará") {
        x <- lt4 %>%
          filter(localidade != "Pará",
                 ano == input$lt4ano,
                 categoria == input$lt4cat) %>%
          select(ri, localidade, valor)
      } else {
        x <- lt4 %>%
          filter(localidade != "Pará",
                 ano == input$lt4ano,
                 categoria == input$lt4cat) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$lt4ri)
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
    output$lt4txt3 <- renderText({
      t43()
    })
    output$lt4graf <- renderEcharts4r({
      lt4 %>%
        filter(categoria == input$lt4cat, localidade == "Pará") %>%
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
    ## Tabela - Valor da Produção----
    output$lt4txt4 <- renderText({
      t44()
    })
    output$lt4tabcat <- renderReactable({
      x <- lt4 %>%
        filter(localidade == "Pará", ano == input$lt4ano) %>%
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
    # 5 - Rendimento Médio da produção----
    ## Mapa - Rendimento Médio da produção----
    output$lt5txt1 <- renderText({
      t51()
    })
    output$lt5map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$lt5ri == "Pará") {
        df <- lt5 %>%
          filter(localidade != "Pará",
                 ano == input$lt5ano,
                 categoria == input$lt5cat) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- lt5 %>%
          filter(localidade != "Pará",
                 ano == input$lt5ano,
                 categoria == input$lt5cat) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$lt5ri)
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
    output$lt5txt2 <- renderText({
      t52()
    })
    # Renderização da base lt5
    output$lt5tab <- renderReactable({
      if (input$lt5ri == "Pará") {
        x <- lt5 %>%
          filter(localidade != "Pará",
                 ano == input$lt5ano,
                 categoria == input$lt5cat) %>%
          select(ri, localidade, valor)
      } else {
        x <- lt5 %>%
          filter(localidade != "Pará",
                 ano == input$lt5ano,
                 categoria == input$lt5cat) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$lt5ri)
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
                               locales = "pt-BR"),
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
    output$lt5txt3 <- renderText({
      t53()
    })
    output$lt5graf <- renderEcharts4r({
      lt5 %>%
        filter(categoria == input$lt5cat, localidade == "Pará") %>%
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
    ## Tabela - Rendimento Médio da produção----
    output$lt5txt4 <- renderText({
      t54()
    })
    output$lt5tabcat <- renderReactable({
      x <- lt5 %>%
        filter(localidade == "Pará", ano == input$lt5ano) %>%
        select(categoria, valor) %>%
        arrange(desc(valor))
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
            name = "Rendimento Médio",
            format = colFormat(separators = T,
                               locales = "pt-BR")
          ),
          Percentual = colDef(
            name = "Percentual",
            format = colFormat(
              suffix = "%",
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
    ## - Tabela - Área plantada em hectares----
    # Filtra os dados
    lt1_1 <- reactive({
      if (input$lt1ri == "Pará") {
        x <- lt1 %>%
          filter(localidade != "Pará",
                 ano == input$lt1ano,
                 categoria == input$lt1cat)
      } else {
        x <- lt1 %>%
          filter(
            localidade != "Pará",
            ano == input$lt1ano,
            categoria == input$lt1cat,
            ri == input$lt1ri
          )
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(lt1_1(), {
      t12()
      downset_Server("lt1_1", lt1_1(), t12())
    })
    ## - Gráfico - Área plantada em hectares----
    # Filtra os dados
    lt1_2 <- reactive({
      lt1 %>%
        filter(categoria == input$lt1cat, localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(lt1_2(), {
      t13()
      downset_Server("lt1_2", lt1_2(), t13())
    })
    ## - Tabela - Área plantada em hectares----
    # Filtra os dados
    lt1_3 <- reactive({
      x <- lt1 %>%
        filter(localidade == "Pará", ano == input$lt1ano) %>%
        arrange(desc(valor)) %>%
        mutate(Percentual = (valor / sum(valor)) * 100)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(lt1_3(), {
      t14()
      downset_Server("lt1_3", lt1_3(), t14())
    })
    
    # 2 - Área Colhida à Colheita----
    ## - Tabela - Área Colhida à Colheita----
    # Filtra os dados
    lt2_1 <- reactive({
      if (input$lt2ri == "Pará") {
        x <- lt2 %>%
          filter(localidade != "Pará",
                 ano == input$lt2ano,
                 categoria == input$lt2cat)
      } else {
        x <- lt2 %>%
          filter(
            localidade != "Pará",
            ano == input$lt2ano,
            categoria == input$lt2cat,
            ri == input$lt2ri
          )
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(lt2_1(), {
      t22()
      downset_Server("lt2_1", lt2_1(), t22())
    })
    ## - Gráfico - Área Colhida à Colheita----
    # Filtra os dados
    lt2_2 <- reactive({
      lt2 %>%
        filter(categoria == input$lt2cat, localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(lt2_2(), {
      t23()
      downset_Server("lt2_2", lt2_2(), t23())
    })
    ## - Tabela - Área Colhida à Colheita----
    # Filtra os dados
    lt2_3 <- reactive({
      x <- lt2 %>%
        filter(localidade == "Pará", ano == input$lt2ano) %>%
        arrange(desc(valor)) %>%
        mutate(Percentual = (valor / sum(valor)) * 100)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(lt2_3(), {
      t24()
      downset_Server("lt2_3", lt2_3(), t24())
    })
    
    # 3 - Quantidade Produzida----
    ## - Tabela - Quantidade Produzida----
    # Filtra os dados
    lt3_1 <- reactive({
      if (input$lt3ri == "Pará") {
        x <- lt3 %>%
          filter(localidade != "Pará",
                 ano == input$lt3ano,
                 categoria == input$lt3cat)
      } else {
        x <- lt3 %>%
          filter(
            localidade != "Pará",
            ano == input$lt3ano,
            categoria == input$lt3cat,
            ri == input$lt3ri
          )
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(lt3_1(), {
      t31()
      downset_Server("lt3_1", lt3_1(), t31())
    })
    ## - Gráfico - Quantidade Produzida----
    # Filtra os dados
    lt3_2 <- reactive({
      lt3 %>%
        filter(categoria == input$lt3cat, localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(lt3_2(), {
      t32()
      downset_Server("lt3_2", lt3_2(), t32())
    })
    ## - Tabela - Quantidade Produzida----
    # Filtra os dados
    lt3_3 <- reactive({
      x <- lt3 %>%
        filter(localidade == "Pará", ano == input$lt3ano) %>%
        arrange(desc(valor)) %>%
        mutate(Percentual = (valor / sum(valor)) * 100)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(lt3_3(), {
      t34()
      downset_Server("lt3_3", lt3_3(), t34())
    })
    
    # 4 - Valor da Produção----
    ## - Tabela - Valor da Produção----
    # Filtra os dados
    lt4_1 <- reactive({
      if (input$lt4ri == "Pará") {
        x <- lt4 %>%
          filter(localidade != "Pará",
                 ano == input$lt4ano,
                 categoria == input$lt4cat)
      } else {
        x <- lt4 %>%
          filter(
            localidade != "Pará",
            ano == input$lt4ano,
            categoria == input$lt4cat,
            ri == input$lt4ri
          )
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(lt4_1(), {
      t42()
      downset_Server("lt4_1", lt4_1(), t42())
    })
    ## - Gráfico - Valor da Produção----
    # Filtra os dados
    lt4_2 <- reactive({
      lt4 %>%
        filter(categoria == input$lt4cat, localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(lt4_2(), {
      t43()
      downset_Server("lt4_2", lt4_2(), t43())
    })
    ## - Tabela - Valor da Produção----
    # Filtra os dados
    lt4_3 <- reactive({
      x <- lt4 %>%
        filter(localidade == "Pará",
               ano == input$lt4ano) %>%
        arrange(desc(valor)) %>%
        mutate(Percentual = (valor / sum(valor)) * 100)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(lt4_3(), {
      t44()
      downset_Server("lt4_3", lt4_3(), t44())
    })
    
    # 5 - Rendimento médio da produção----
    ## - Tabela - Rendimento Médio da produção----
    # Filtra os dados
    lt5_1 <- reactive({
      if (input$lt5ri == "Pará") {
        x <- lt5 %>%
          filter(localidade != "Pará",
                 ano == input$lt5ano,
                 categoria == input$lt5cat)
      } else {
        x <- lt5 %>%
          filter(
            localidade != "Pará",
            ano == input$lt5ano,
            categoria == input$lt5cat,
            ri == input$lt5ri
          )
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(lt5_1(), {
      t52()
      downset_Server("lt5_1", lt5_1(), t52())
    })
    ## - Gráfico - Rendimento Médio da produção----
    # Filtra os dados
    lt5_2 <- reactive({
      lt5 %>%
        filter(categoria == input$lt5cat, localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(lt5_2(), {
      t53()
      downset_Server("lt5_2", lt5_2(), t53())
    })
    ## - Tabela - Rendimento Médio da produção----
    # Filtra os dados
    lt5_3 <- reactive({
      x <- lt5 %>%
        filter(localidade == "Pará", ano == input$lt5ano) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(lt5_3(), {
      t54()
      downset_Server("lt5_3", lt5_3(), t54())
    })
  })
}

# Play do Módulo
# ui <- dashboardPage(header = dashboardHeader(),
#                     sidebar = dashboardSidebar(),
#                     body = dashboardBody(fluidPage(economia_lt_pa_ui("economia_lt_pa"))))
# server <- function(input, output) {
#   economia_lt_pa_Server("economia_lt_pa")
# }
# 
# shinyApp(ui, server)
