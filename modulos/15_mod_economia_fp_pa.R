# Funções de módulo de Economia - Finanças Públicas
# Função de UI
economia_fp_pa_ui <- function(id) {
  fluidPage(# CAMINHO DO ARQUIVO CSS----
            tags$head(
              tags$link(rel = "stylesheet", type = "text/css", href = "estilo.css")
            ),
            # Lista de Navegação lateral----
            div(class = "navbar_economia",
                navbarPage(
                  tags$b("Finanças Públicas - Pará"),
                  navbarMenu(
                    tags$b("Escolha um Indicador"),
                    # 1 - Repasse de ICMS----
                    tabPanel(
                      "Repasse de ICMS",
                      panel(
                        ## Controle----
                        heading =
                          h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                             "Repasse de ICMS"),
                        tags$div(
                          class = "seletor1",
                          pickerInput(
                            inputId = NS(id, "fp1ano"),
                            label = "Ano",
                            choices = sort(unique(fp1[["ano"]]), decreasing = T),
                            width = "100px"
                          )
                        ),
                        tags$div(
                          class = "seletor2",
                          pickerInput(
                            inputId = NS(id, "fp1ri"),
                            label = "Pará/Região de Integração",
                            choices = unique(fp1[["ri"]]),
                            width = "200px"
                          )
                        )
                      ),
                      fluidRow(
                        ## Mapa - Repasse de ICMS----
                        box(
                          title = textOutput(NS(id, "fp1txt1")),
                          collapsible = T,
                          collapsed = F,
                          headerBorder = T,
                          width = 12,
                          withSpinner(
                            leafletOutput(NS(id, "fp1map"), height = "600px"),
                            type = 8,
                            color = "#f2c94e",
                            size = 0.5
                          ),
                          footer = list(
                            tags$h6(tags$b("Fonte:"), "DITES-SEFA"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                            tags$h6(tags$b("Nota:"), "Valor - R$ 1,00 (valores correntes)")
                          )
                        ),
                        ## Tabela - Repasse de ICMS----
                        box(
                          title = textOutput(NS(id, "fp1txt2")),
                          collapsible = T,
                          collapsed = F,
                          width = 12,
                          headerBorder = T,
                          solidHeader = F,
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
                          ),
                          downset_ui(NS(id, "fp1_1")))
                        )
                      ),
                      ## Gráfico - Repasse de ICMS----
                      fluidRow(
                        box(
                          title = textOutput(NS(id, "fp1txt3")),
                          collapsible = T,
                          collapsed = F,
                          width = 12,
                          headerBorder = T,
                          solidHeader = F,
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
                          ),
                          downset_ui(NS(id, "fp1_2")))
                        )
                      )
                    ),
                    # 2 - Demonstrativo dos Índices de Participação na Arrecadação do ICMS----
                    tabPanel(
                      "Demonstrativo dos Índices de Participação na Arrecadação do ICMS",
                      panel(
                        ## Controle----
                        heading =
                          h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                             "Demonstrativo dos Índices de Participação na Arrecadação do ICMS"),
                        tags$div(
                          class = "seletor1",
                          # Selecionar
                          pickerInput(
                            inputId = NS(id, "fp2ano"),
                            label = "Ano",
                            choices = sort(unique(fp2[["ano"]]), decreasing = T),
                            width = "100px"
                          )
                        ),
                        tags$div(
                          class = "seletor2",
                          # select R.I
                          pickerInput(
                            inputId = NS(id, "fp2ri"),
                            label = "Pará/Região de Integração",
                            choices = unique(fp2[["ri"]]),
                            width = "200px"
                          )
                        )
                      ),
                      fluidRow(
                        ## Mapa - Demonstrativo dos Índices de Participação na Arrecadação do ICMS----
                        box(
                          title = textOutput(NS(id, "fp2txt1")),
                          collapsible = T,
                          collapsed = F,
                          headerBorder = T,
                          width = 12,
                          withSpinner(
                            leafletOutput(NS(id, "fp2map"), height = "600px"),
                            type = 8,
                            color = "#f2c94e",
                            size = 0.5
                          ),
                          footer = list(tags$h6(tags$b("Fonte:"), "DITES-SEFA"),
                                        tags$h6(tags$b("Elaboração:"), "FAPESPA"))
                        ),
                        ## Tabela - Demonstrativo dos Índices de Participação na Arrecadação do ICMS----
                        box(
                          title = textOutput(NS(id, "fp2txt2")),
                          collapsible = T,
                          collapsed = F,
                          width = 12,
                          headerBorder = T,
                          solidHeader = F,
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
                          ),
                          downset_ui(NS(id, "fp2_1")))
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
                font-weight: bold;color: #6D6D6D;",
                             "Repasse de IPI"),
                        tags$div(
                          class = "seletor1",
                          # Selecionar
                          pickerInput(
                            inputId = NS(id, "fp3ano"),
                            label = "Ano",
                            choices = sort(unique(fp3[["ano"]]), decreasing = T),
                            width = "100px"
                          )
                        ),
                        tags$div(
                          class = "seletor2",
                          # select R.I
                          pickerInput(
                            inputId = NS(id, "fp3ri"),
                            label = "Pará/Região de Integração",
                            choices = unique(fp3[["ri"]]),
                            width = "200px"
                          )
                        )
                      ),
                      fluidRow(
                        ## Mapa - Repasse de IPI dos Municípios----
                        box(
                          title = textOutput(NS(id, "fp3txt1")),
                          collapsible = T,
                          collapsed = F,
                          headerBorder = T,
                          width = 12,
                          withSpinner(
                            leafletOutput(NS(id, "fp3map"), height = "600px"),
                            type = 8,
                            color = "#f2c94e",
                            size = 0.5
                          ),
                          footer = list(
                            tags$h6(tags$b("Fonte:"), "DITES-SEFA"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                            tags$h6(tags$b("Nota:"), "Valor - R$ 1,00 (valores correntes)")
                          )
                        ),
                        ## Tabela - Repasse de IPI dos Municípios----
                        box(
                          title = textOutput(NS(id, "fp3txt2")),
                          collapsible = T,
                          collapsed = F,
                          width = 12,
                          headerBorder = T,
                          solidHeader = F,
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
                          ),
                          downset_ui(NS(id, "fp3_1")))
                        )
                      ),
                      ## Gráfico - Repasse de IPI dos Municípios----
                      fluidRow(
                        box(
                          title = textOutput(NS(id, "fp3txt3")),
                          collapsible = T,
                          collapsed = F,
                          width = 12,
                          headerBorder = T,
                          solidHeader = F,
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
                          ),
                          downset_ui(NS(id, "fp3_2")))
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
                font-weight: bold;color: #6D6D6D;",
                             "Repasse de IPVA"),
                        tags$div(
                          class = "seletor1",
                          # Selecionar
                          pickerInput(
                            inputId = NS(id, "fp4ano"),
                            label = "Ano",
                            choices = sort(unique(fp4[["ano"]]), decreasing = T),
                            width = "100px"
                          )
                        ),
                        tags$div(
                          class = "seletor2",
                          # select R.I
                          pickerInput(
                            inputId = NS(id, "fp4ri"),
                            label = "Pará/Região de Integração",
                            choices = unique(fp4[["ri"]]),
                            width = "200px"
                          )
                        )
                      ),
                      fluidRow(
                        ## Mapa - Repasse de IPVA  dos Municípios----
                        box(
                          title = textOutput(NS(id, "fp4txt1")),
                          collapsible = T,
                          collapsed = F,
                          headerBorder = T,
                          width = 12,
                          withSpinner(
                            leafletOutput(NS(id, "fp4map"), height = "600px"),
                            type = 8,
                            color = "#f2c94e",
                            size = 0.5
                          ),
                          footer = list(
                            tags$h6(tags$b("Fonte:"), "DITES-SEFA"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                            tags$h6(tags$b("Nota:"), "Valor - R$ 1,00 (valores correntes)")
                          )
                        ),
                        ## Tabela - Repasse de IPVA  dos Municípios----
                        box(
                          title = textOutput(NS(id, "fp4txt2")),
                          collapsible = T,
                          collapsed = F,
                          width = 12,
                          headerBorder = T,
                          solidHeader = F,
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
                          ),
                          downset_ui(NS(id, "fp4_1")))
                        )
                      ),
                      ## Gráfico - Repasse de IPVA  dos Municípios----
                      fluidRow(
                        box(
                          title = textOutput(NS(id, "fp4txt3")),
                          collapsible = T,
                          collapsed = F,
                          width = 12,
                          headerBorder = T,
                          solidHeader = F,
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
                          ),
                          downset_ui(NS(id, "fp4_2")))
                        )
                      )
                    ),
                    # 5 - Receita Orçamentária----
                    tabPanel(
                      "Receita Orçamentária",
                      panel(
                        ## Controle----
                        heading =
                          h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                             "Receita Orçamentária"),
                        tags$div(
                          class = "seletor1",
                          # Selecionar
                          pickerInput(
                            inputId = NS(id, "fp5ano"),
                            label = "Ano",
                            choices = sort(unique(fp5[["ano"]]), decreasing = T),
                            width = "100px"
                          )
                        ),
                        tags$div(
                          class = "seletor2",
                          # select R.I
                          pickerInput(
                            inputId = NS(id, "fp5ri"),
                            label = "Pará/Região de Integração",
                            choices = unique(fp5[["ri"]]),
                            width = "200px"
                          )
                        )
                      ),
                      fluidRow(
                        ## Mapa - Receita Orçamentária----
                        box(
                          title = textOutput(NS(id, "fp5txt1")),
                          collapsible = T,
                          collapsed = F,
                          headerBorder = T,
                          width = 12,
                          withSpinner(
                            leafletOutput(NS(id, "fp5map"), height = "600px"),
                            type = 8,
                            color = "#f2c94e",
                            size = 0.5
                          ),
                          footer = list(
                            tags$h6(tags$b("Fonte:"), "DITES-SEFA"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                            tags$h6(tags$b("Nota:"), "Valor - R$ 1,00 (valores correntes)")
                          )
                        ),
                        ## Tabela - Receita Orçamentária----
                        box(
                          title = textOutput(NS(id, "fp5txt2")),
                          collapsible = T,
                          collapsed = F,
                          width = 12,
                          headerBorder = T,
                          solidHeader = F,
                          withSpinner(
                            reactableOutput(NS(id, "fp5tab"), height = "400px"),
                            type = 8,
                            color = "#f2c94e",
                            size = 0.5
                          ),
                          footer = list(column(
                            11,
                            tags$h6(tags$b("Fonte:"), "DITES-SEFA"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                            tags$h6(tags$b("Nota:"), "Valor - R$ 1,00 (valores correntes)")
                          ),
                          downset_ui(NS(id, "fp5_1")))
                        )
                      ),
                      ## Gráfico - Receita Orçamentária----
                      fluidRow(
                        box(
                          title = textOutput(NS(id, "fp5txt3")),
                          collapsible = T,
                          collapsed = F,
                          width = 12,
                          headerBorder = T,
                          solidHeader = F,
                          withSpinner(
                            echarts4rOutput(NS(id, "fp5graf")),
                            type = 8,
                            color = "#f2c94e",
                            size = 0.5
                          ),
                          footer = list(column(
                            11,
                            tags$h6(tags$b("Fonte:"), "DITES-SEFA"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                            tags$h6(tags$b("Nota:"), "Valor - R$ 1,00 (valores correntes)")
                          ),
                          downset_ui(NS(id, "fp5_2")))
                        )
                      )
                    ),
                    # 6 - Receitas Correntes----
                    tabPanel(
                      "Receitas Correntes",
                      panel(
                        ## Controle----
                        heading =
                          h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                             "Receitas Correntes"),
                        tags$div(
                          class = "seletor1",
                          # Selecionar
                          pickerInput(
                            inputId = NS(id, "fp6ano"),
                            label = "Ano",
                            choices = sort(unique(fp6[["ano"]]), decreasing = T),
                            width = "100px"
                          )
                        ),
                        tags$div(
                          class = "seletor2",
                          # select R.I
                          pickerInput(
                            inputId = NS(id, "fp6ri"),
                            label = "Pará/Região de Integração",
                            choices = unique(fp6[["ri"]]),
                            width = "200px"
                          )
                        )
                      ),
                      fluidRow(
                        ## Mapa - Receitas Correntes----
                        box(
                          title = textOutput(NS(id, "fp6txt1")),
                          collapsible = T,
                          collapsed = F,
                          headerBorder = T,
                          width = 12,
                          withSpinner(
                            leafletOutput(NS(id, "fp6map"), height = "600px"),
                            type = 8,
                            color = "#f2c94e",
                            size = 0.5
                          ),
                          footer = list(
                            tags$h6(tags$b("Fonte:"), "DITES-SEFA"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                            tags$h6(tags$b("Nota:"), "Valor - R$ 1,00 (valores correntes)")
                          )
                        ),
                        ## Tabela - Receitas Correntes----
                        box(
                          title = textOutput(NS(id, "fp6txt2")),
                          collapsible = T,
                          collapsed = F,
                          width = 12,
                          headerBorder = T,
                          solidHeader = F,
                          withSpinner(
                            reactableOutput(NS(id, "fp6tab"), height = "400px"),
                            type = 8,
                            color = "#f2c94e",
                            size = 0.5
                          ),
                          footer = list(column(
                            11,
                            tags$h6(tags$b("Fonte:"), "DITES-SEFA"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                            tags$h6(tags$b("Nota:"), "Valor - R$ 1,00 (valores correntes)")
                          ),
                          downset_ui(NS(id, "fp6_1")))
                        )
                      ),
                      ## Gráfico - Receitas Correntes----
                      fluidRow(
                        box(
                          title = textOutput(NS(id, "fp6txt3")),
                          collapsible = T,
                          collapsed = F,
                          width = 12,
                          headerBorder = T,
                          solidHeader = F,
                          withSpinner(
                            echarts4rOutput(NS(id, "fp6graf")),
                            type = 8,
                            color = "#f2c94e",
                            size = 0.5
                          ),
                          footer = list(column(
                            11,
                            tags$h6(tags$b("Fonte:"), "DITES-SEFA"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                            tags$h6(tags$b("Nota:"), "Valor - R$ 1,00 (valores correntes)")
                          ),
                          downset_ui(NS(id, "fp6_2")))
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
                font-weight: bold;color: #6D6D6D;",
                             "Impostos"),
                        tags$div(
                          class = "seletor1",
                          # Selecionar
                          pickerInput(
                            inputId = NS(id, "fp7ano"),
                            label = "Ano",
                            choices = sort(unique(fp7[["ano"]]), decreasing = T),
                            width = "100px"
                          )
                        ),
                        tags$div(
                          class = "seletor2",
                          # select R.I
                          pickerInput(
                            inputId = NS(id, "fp7ri"),
                            label = "Pará/Região de Integração",
                            choices = unique(fp7[["ri"]]),
                            width = "200px"
                          )
                        )
                      ),
                      fluidRow(
                        ## Mapa - Impostos----
                        box(
                          title = textOutput(NS(id, "fp7txt1")),
                          collapsible = T,
                          collapsed = F,
                          headerBorder = T,
                          width = 12,
                          withSpinner(
                            leafletOutput(NS(id, "fp7map"), height = "600px"),
                            type = 8,
                            color = "#f2c94e",
                            size = 0.5
                          ),
                          footer = list(
                            tags$h6(tags$b("Fonte:"), "DITES-SEFA"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                            tags$h6(tags$b("Nota:"), "Valor - R$ 1,00 (valores correntes)")
                          )
                        ),
                        ## Tabela - Impostos----
                        box(
                          title = textOutput(NS(id, "fp7txt2")),
                          collapsible = T,
                          collapsed = F,
                          width = 12,
                          headerBorder = T,
                          solidHeader = F,
                          withSpinner(
                            reactableOutput(NS(id, "fp7tab"), height = "400px"),
                            type = 8,
                            color = "#f2c94e",
                            size = 0.5
                          ),
                          footer = list(column(
                            11,
                            tags$h6(tags$b("Fonte:"), "DITES-SEFA"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                            tags$h6(tags$b("Nota:"), "Valor - R$ 1,00 (valores correntes)")
                          ),
                          downset_ui(NS(id, "fp7_1")))
                        )
                      ),
                      ## Gráfico - Impostos----
                      fluidRow(
                        box(
                          title = textOutput(NS(id, "fp7txt3")),
                          collapsible = T,
                          collapsed = F,
                          width = 12,
                          headerBorder = T,
                          solidHeader = F,
                          withSpinner(
                            echarts4rOutput(NS(id, "fp7graf")),
                            type = 8,
                            color = "#f2c94e",
                            size = 0.5
                          ),
                          footer = list(column(
                            11,
                            tags$h6(tags$b("Fonte:"), "DITES-SEFA"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                            tags$h6(tags$b("Nota:"), "Valor - R$ 1,00 (valores correntes)")
                          ),
                          downset_ui(NS(id, "fp7_2")))
                        )
                      )
                    ),
                    # 8 - Receita de Transferências Correntes----
                    tabPanel(
                      "Receita de Transferências Correntes",
                      panel(
                        ## Controle----
                        heading =
                          h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                             "Receita de Transferências Correntes"),
                        tags$div(
                          class = "seletor1",
                          # Selecionar
                          pickerInput(
                            inputId = NS(id, "fp8ano"),
                            label = "Ano",
                            choices = sort(unique(fp8[["ano"]]), decreasing = T),
                            width = "100px"
                          )
                        ),
                        tags$div(
                          class = "seletor2",
                          # select R.I
                          pickerInput(
                            inputId = NS(id, "fp8ri"),
                            label = "Pará/Região de Integração",
                            choices = unique(fp8[["ri"]]),
                            width = "200px"
                          )
                        )
                      ),
                      fluidRow(
                        ## Mapa - Receita de Transferências Correntes----
                        box(
                          title = textOutput(NS(id, "fp8txt1")),
                          collapsible = T,
                          collapsed = F,
                          headerBorder = T,
                          width = 12,
                          withSpinner(
                            leafletOutput(NS(id, "fp8map"), height = "600px"),
                            type = 8,
                            color = "#f2c94e",
                            size = 0.5
                          ),
                          footer = list(
                            tags$h6(tags$b("Fonte:"), "DITES-SEFA"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                            tags$h6(tags$b("Nota:"), "Valor - R$ 1,00 (valores correntes)")
                          )
                        ),
                        ## Tabela - Receita de Transferências Correntes----
                        box(
                          title = textOutput(NS(id, "fp8txt2")),
                          collapsible = T,
                          collapsed = F,
                          width = 12,
                          headerBorder = T,
                          solidHeader = F,
                          withSpinner(
                            reactableOutput(NS(id, "fp8tab"), height = "400px"),
                            type = 8,
                            color = "#f2c94e",
                            size = 0.5
                          ),
                          footer = list(column(
                            11,
                            tags$h6(tags$b("Fonte:"), "DITES-SEFA"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                            tags$h6(tags$b("Nota:"), "Valor - R$ 1,00 (valores correntes)")
                          ),
                          downset_ui(NS(id, "fp8_1")))
                        )
                      ),
                      ## Gráfico - Receita de Transferências Correntes----
                      fluidRow(
                        box(
                          title = textOutput(NS(id, "fp8txt3")),
                          collapsible = T,
                          collapsed = F,
                          width = 12,
                          headerBorder = T,
                          solidHeader = F,
                          withSpinner(
                            echarts4rOutput(NS(id, "fp8graf")),
                            type = 8,
                            color = "#f2c94e",
                            size = 0.5
                          ),
                          footer = list(column(
                            11,
                            tags$h6(tags$b("Fonte:"), "DITES-SEFA"),
                            tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                            tags$h6(tags$b("Nota:"), "Valor - R$ 1,00 (valores correntes)")
                          ),
                          downset_ui(NS(id, "fp8_2")))
                        )
                      )
                    )
                  )
                )))
}
# Função do modulo servidor
economia_fp_pa_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    #TÍTULOS----
    # 1 - Repasse de ICMS----
    ## Mapa - Repasse de ICMS----
    t11 <- reactive({
      if (input$fp1ri == "Pará") {
        paste0("Repasse de ICMS, Pará - ",
               input$fp1ano)
      } else {
        paste0(
          "Repasse de ICMS, Região de Integração ",
          input$fp1ri,
          " - ",
          input$fp1ano
        )
      }
    })
    ## Tabela - Repasse de ICMS----
    t12 <- reactive({
      if (input$fp1ri == "Pará") {
        paste0("Repasse de ICMS dos Municípios, Pará - ",
               input$fp1ano)
      } else {
        paste0(
          "Repasse de ICMS dos Municípios, Região de Integração ",
          input$fp1ri,
          " - ",
          input$fp1ano
        )
      }
    })
    ## Gráfico - Repasse de ICMS----
    t13 <- reactive({
      paste0(
        "Repasse de ICMS, Pará - ",
        min(fp1$ano),
        " a ",
        max(fp1$ano)
      )
    })
    # 2 - Demonstrativo dos Índices de Participação na Arrecadação do ICMS----
    ## Mapa - Demonstrativo dos Índices de Participação na Arrecadação do ICMS----
    t21 <- reactive({
      if (input$fp2ri == "Pará") {
        paste0(
          "Índices de Participação na Arrecadação do ICMS, Pará - ",
          input$fp2ano
        )
      } else {
        paste0(
          "Índices de Participação na Arrecadação do ICMS, Região de Integração ",
          input$fp2ri,
          " - ",
          input$fp2ano
        )
      }
    })
    ## Tabela - Demonstrativo dos Índices de Participação na Arrecadação do ICMS----
    t22 <- reactive({
      if (input$fp2ri == "Pará") {
        paste0(
          "Índices de Participação na Arrecadação do ICMS dos Municípios, Pará - ",
          input$fp2ano
        )
      } else {
        paste0(
          "Índices de Participação na Arrecadação do ICMS dos Municípios, Região de Integração ",
          input$fp2ri,
          " - ",
          input$fp2ano
        )
      }
    })
    # 3 - Repasse de IPI----
    ## Mapa - Repasse de IPI----
    t31 <- reactive({
      if (input$fp3ri == "Pará") {
        paste0("Repasse de IPI aos Municípios, Pará - ",
               input$fp3ano)
      } else {
        paste0(
          "Repasse de IPI aos Municípios, Região de integração ",
          input$fp3ri,
          " - ",
          input$fp3ano
        )
      }
    })
    ## Tabela - Repasse de IPI----
    t32 <- reactive({
      if (input$fp3ri == "Pará") {
        paste0("Repasse de IPI aos Municípios dos Municípios, Pará - ",
               input$fp3ano)
      } else {
        paste0(
          "Repasse de IPI aos Municípios dos Municípios, Região de integração ",
          input$fp3ri,
          " - ",
          input$fp3ano
        )
      }
    })
    ## Gráfico - Repasse de IPI----
    t33 <- reactive({
      paste0(
        "Repasse Total de IPI, Pará - ",
        min(fp3$ano),
        " a ",
        max(fp3$ano)
      )
    })
    # 4 - Repasse de IPVA----
    ## Mapa - Repasse de IPVA----
    t41 <- reactive({
      if (input$fp4ri == "Pará") {
        paste0("Repasse de IPVA, Pará - ",
               input$fp4ano)
      } else {
        paste0(
          "Repasse de IPVA, Região de integração ",
          input$fp4ri,
          " - ",
          input$fp4ano
        )
      }
    })
    ## Tabela - Repasse de IPVA----
    t42 <- reactive({
      if (input$fp4ri == "Pará") {
        paste0("Repasse de IPVA dos Municípios, Pará - ",
               input$fp4ano)
      } else {
        paste0(
          "Repasse de IPVA dos Municípios, Região de integração ",
          input$fp4ri,
          " - ",
          input$fp4ano
        )
      }
    })
    ## Gráfico - Repasse de IPVA----
    t43 <- reactive({
      paste0(
        "Repasse Total de IPVA, Pará - ",
        min(fp4$ano),
        " a ",
        max(fp4$ano)
      )
    })
    # 5 - Receita Orçamentária----
    ## Mapa - Receita Orçamentária----
    t51 <- reactive({
      if (input$fp5ri == "Pará") {
        paste0("Receita Orçamentária, Pará - ",
               input$fp5ano)
      } else {
        paste0(
          "Receita Orçamentária, Região de integração ",
          input$fp5ri,
          " - ",
          input$fp5ano
        )
      }
    })
    ## Tabela - Receita Orçamentária----
    t52 <- reactive({
      if (input$fp5ri == "Pará") {
        paste0("Receita Orçamentária dos Municípios, Pará - ",
               input$fp5ano)
      } else {
        paste0(
          "Receita Orçamentária dos Municípios, Região de integração ",
          input$fp5ri,
          " - ",
          input$fp5ano
        )
      }
    })
    ## Gráfico - Receita Orçamentária----
    t53 <- reactive({
      paste0(
        "Receita Orçamentária Total, Pará - ",
        min(fp5$ano),
        " a ",
        max(fp5$ano)
      )
    })
    # 6 - Receitas Correntes----
    ## Mapa - Receitas Correntes----
    t61 <- reactive({
      if (input$fp6ri == "Pará") {
        paste0(
          "Valores Totais de Receitas Correntes, Pará - ",
          input$fp6ano
        )
      } else {
        paste0(
          "Valores Totais de Receitas Correntes, Região de integração ",
          input$fp6ri,
          " - ",
          input$fp6ano
        )
      }
    })
    ## Tabela - Receitas Correntes----
    t62 <- reactive({
      if (input$fp6ri == "Pará") {
        paste0(
          "Valores Totais de Receitas Correntes dos Municípios, Pará - ",
          input$fp6ano
        )
      } else {
        paste0(
          "Valores Totais de Receitas Correntes dos Municípios, Região de integração ",
          input$fp6ri,
          " - ",
          input$fp6ano
        )
      }
    })
    ## Gráfico - Receitas Correntes----
    t63 <- reactive({
      paste0(
        "Valores Totais de Receitas Correntes, Pará - ",
        min(fp6$ano),
        " a ",
        max(fp6$ano)
      )
    })
    # 7 - Impostos----
    ## Mapa - Impostos----
    t71 <- reactive({
      if (input$fp7ri == "Pará") {
        paste0("Valores Totais de Impostos, Pará - ",
               input$fp7ano)
      } else {
        paste0(
          "Valores Totais de Impostos, Região de integração ",
          input$fp7ri,
          " - ",
          input$fp7ano
        )
      }
    })
    ## Tabela - Impostos----
    t72 <- reactive({
      if (input$fp7ri == "Pará") {
        paste0("Valores Totais de Impostos dos Municípios, Pará - ",
               input$fp7ano)
      } else {
        paste0(
          "Valores Totais de Impostos dos Municípios, Região de integração ",
          input$fp7ri,
          " - ",
          input$fp7ano
        )
      }
    })
    ## Gráfico - Impostos----
    t73 <- reactive({
      paste0(
        "Valores Totais de Impostos, Pará - ",
        min(fp7$ano),
        " a ",
        max(fp7$ano)
      )
    })

    # 8 - Receita de Transferências Correntes----
    ## Mapa - Receita de Transferências Correntes----
    t81 <- reactive({
      if (input$fp8ri == "Pará") {
        paste0(
          "Valores Totais das Receitas de Transferências Correntes, Pará - ",
          input$fp8ano
        )
      } else {
        paste0(
          "Valores Totais das Receitas de Transferências Correntes, Região de integração ",
          input$fp8ri,
          " - ",
          input$fp8ano
        )
      }
    })
    ## Tabela - Receita de Transferências Correntes----
    t82 <- reactive({
      if (input$fp8ri == "Pará") {
        paste0(
          "Valores Totais das Receitas de Transferências Correntes dos Municípios, Pará - ",
          input$fp8ano
        )
      } else {
        paste0(
          "Valores Totais das Receitas de Transferências Correntes dos Municípios, Região de integração ",
          input$fp8ri,
          " - ",
          input$fp8ano
        )
      }
    })
    ## Gráfico - Receita de Transferências Correntes----
    t83 <- reactive({
      paste0(
        "Valores Totais das Receitas de Transferências Correntes, Pará - ",
        min(fp8$ano),
        " a ",
        max(fp8$ano)
      )
    })
    #VISUALIZAÇÃO----
    # 1 - Repasse de ICMS----
    ## Mapa - Repasse de ICMS----
    output$fp1txt1 <- renderText({
      t11()  
    })
    output$fp1map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$fp1ri == "Pará") {
        df <- fp1 %>%
          filter(localidade != "Pará", ano == input$fp1ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- fp1 %>%
          filter(localidade != "Pará", ano == input$fp1ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$fp1ri)
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
          "<strong>%s</strong><br/> <b>Valor(R$):</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
            format(x$valor, big.mark = ".", decimal.mark = ",")
          )
        ) %>%
        lapply(htmltools::HTML)
      leaflet(x,
              options = leafletOptions(minZoom = 0, maxZoom = 15)) %>%
        addTiles() %>%
        addPolygons(
          weight = 2,
          opacity = 1,
          color = "black",
          fillOpacity = 1,
          fillColor = ~ pal(valor),
          dashArray = 1,
          smoothFactor = 1.5,
          highlightOptions = highlightOptions(
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
    ## Tabela - Repasse de ICMS----
    output$fp1txt2 <- renderText({
      t12()  
    })
    output$fp1tab <- renderReactable({
      if (input$fp1ri == "Pará") {
        x <- fp1 %>%
          filter(localidade != "Pará", ano == input$fp1ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- fp1 %>%
          filter(localidade != "Pará", ano == input$fp1ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$fp1ri)
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
            name = "Valor(R$)",
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
    ## Gráfico - Repasse de ICMS----
    output$fp1txt3 <- renderText({
      t13()  
    })
    output$fp1graf <- renderEcharts4r({
      fp1 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
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
          splitLine = list(show = T),
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
    
    # 2 - Demonstrativo dos Índices de Participação na Arrecadação do ICMS----
    ## Mapa - Demonstrativo dos Índices de Participação na Arrecadação do ICMS----
    output$fp2txt1 <- renderText({
      t21()  
    })
    output$fp2map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$fp2ri == "Pará") {
        df <- fp2 %>%
          filter(localidade != "Pará", ano == input$fp2ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- fp2 %>%
          filter(localidade != "Pará", ano == input$fp2ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$fp2ri)
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
          "<strong>%s</strong><br/> <b>índice:</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
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
          title = "Índice",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(
            big.mark = ".",
            decimal.mark = ",",
            digits = 2
          )
        )
    })
    ## Tabela - Demonstrativo dos Índices de Participação na Arrecadação do ICMS----
    output$fp2txt2 <- renderText({
      t22()  
    })
    output$fp2tab <- renderReactable({
      if (input$fp2ri == "Pará") {
        x <- fp2 %>%
          filter(localidade != "Pará", ano == input$fp2ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- fp2 %>%
          filter(localidade != "Pará", ano == input$fp2ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$fp2ri)
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
            name = "Índice",
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
    
    # 3 - Repasse de IPI----
    ## Mapa - Repasse de IPI----
    output$fp3txt1 <- renderText({
      t31()  
    })
    output$fp3map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$fp3ri == "Pará") {
        df <- fp3 %>%
          filter(localidade != "Pará", ano == input$fp3ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- fp3 %>%
          filter(localidade != "Pará", ano == input$fp3ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$fp3ri)
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
          "<strong>%s</strong><br/> <b>Valor(R$):</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
            format(
              x$valor,
              big.mark = ".",
              decimal.mark = ",",
              digits = 2
            )
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
    ## Tabela - Repasse de IPI----
    output$fp3txt2 <- renderText({
      t32()
    })
    output$fp3tab <- renderReactable({
      if (input$fp3ri == "Pará") {
        x <- fp3 %>%
          filter(localidade != "Pará", ano == input$fp3ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- fp3 %>%
          filter(localidade != "Pará", ano == input$fp3ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$fp3ri)
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
            name = "Valor(R$)",
            format = colFormat(
              digits = 0,
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
    ## Gráfico - Repasse de IPI----
    output$fp3txt3 <- renderText({
      t33()
    })
    output$fp3graf <- renderEcharts4r({
      fp3 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
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
          splitLine = list(show = T),
          name = "Ano",
          splitLine = list(show = T),
          nameTextStyle = list(fontWeight = "bold", fontSize = 14)
        ) %>%
        e_y_axis(
          name = "Valor(R$)",
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
        e_datazoom(toolbox = F, fillerColor = "#E5F5F9") %>%
        e_grid(show = T)
    })
    # 4 - Repasse de IPVA----
    ## Mapa - Repasse de IPVA----
    output$fp4txt1 <- renderText({
      t41()
    })
    output$fp4map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$fp4ri == "Pará") {
        df <- fp4 %>%
          filter(localidade != "Pará", ano == input$fp4ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- fp4 %>%
          filter(localidade != "Pará", ano == input$fp4ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$fp4ri)
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
          "<strong>%s</strong><br/> <b>Valor(R$):</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
            format(
              x$valor,
              big.mark = ".",
              decimal.mark = ",",
              digits = 4
            )
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
    ## Tabela - Repasse de IPVA----
    output$fp4txt2 <- renderText({
      t42()
    })
    output$fp4tab <- renderReactable({
      if (input$fp4ri == "Pará") {
        x <- fp4 %>%
          filter(localidade != "Pará", ano == input$fp4ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- fp4 %>%
          filter(localidade != "Pará", ano == input$fp4ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$fp4ri)
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
            name = "Valor(R$)",
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
    ## Gráfico - Repasse de IPVA----
    output$fp4txt3 <- renderText({
      t43()
    })
    output$fp4graf <- renderEcharts4r({
      fp4 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
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
          splitLine = list(show = T),
          name = "Ano",
          splitLine = list(show = T),
          nameTextStyle = list(fontWeight = "bold", fontSize = 14)
        ) %>%
        e_y_axis(
          name = "Valor(R$)",
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
        e_datazoom(toolbox = F, fillerColor = "#E5F5F9") %>%
        e_grid(show = T)
    })
    # 5 - Receita Orçamentária----
    ## Mapa - Receita Orçamentária----
    output$fp5txt1 <- renderText({
      t51()
    })
    output$fp5map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$fp5ri == "Pará") {
        df <- fp5 %>%
          filter(localidade != "Pará", ano == input$fp5ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- fp5 %>%
          filter(localidade != "Pará", ano == input$fp5ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$fp5ri)
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
      conteudo <- sprintf(
        "<strong>%s</strong><br/> <b>Valor(R$):</b>%s",
        x$name_muni,
        ifelse(
          is.na(x$valor),
          "Não disponível",
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
    ## Tabela - Receita Orçamentária----
    output$fp5txt2 <- renderText({
      t52()
    })
    output$fp5tab <- renderReactable({
      if (input$fp5ri == "Pará") {
        x <- fp5 %>%
          filter(localidade != "Pará", ano == input$fp5ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- fp5 %>%
          filter(localidade != "Pará", ano == input$fp5ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$fp5ri)
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
            name = "Valor(R$)",
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
    ## Gráfico - Receita Orçamentária----
    output$fp5txt3 <- renderText({
      t53()
    })
    output$fp5graf <- renderEcharts4r({
      fp5 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
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
          splitLine = list(show = T),
          name = "Ano",
          splitLine = list(show = T),
          nameTextStyle = list(fontWeight = "bold", fontSize = 14)
        ) %>%
        e_y_axis(
          name = "Valor(R$)",
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
        e_datazoom(toolbox = F, fillerColor = "#E5F5F9") %>%
        e_grid(show = T)
    })
    # 6 - Receitas Correntes----
    ## Mapa - Receitas Correntes----
    output$fp6txt1 <- renderText({
      t61()
    })
    output$fp6map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$fp6ri == "Pará") {
        df <- fp6 %>%
          filter(localidade != "Pará", ano == input$fp6ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- fp6 %>%
          filter(localidade != "Pará", ano == input$fp6ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$fp6ri)
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
          "<strong>%s</strong><br/> <b>Valor(R$):</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
            formatC(
              x$valor,
              format = "f",
              big.mark = ".",
              decimal.mark = ",",
              digits = 0
            )
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
    ## Tabela - Receitas Correntes----
    output$fp6txt2 <- renderText({
      t62()
    })
    output$fp6tab <- renderReactable({
      if (input$fp6ri == "Pará") {
        x <- fp6 %>%
          filter(localidade != "Pará", ano == input$fp6ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- fp6 %>%
          filter(localidade != "Pará", ano == input$fp6ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$fp6ri)
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
            name = "Valor(R$)",
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
    ## Gráfico - Receitas Correntes----
    output$fp6txt3 <- renderText({
      t63()
    })
    output$fp6graf <- renderEcharts4r({
      fp6 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
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
          splitLine = list(show = T),
          name = "Ano",
          splitLine = list(show = T),
          nameTextStyle = list(fontWeight = "bold", fontSize = 14)
        ) %>%
        e_y_axis(
          name = "Valor(R$)",
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
        e_datazoom(toolbox = F, fillerColor = "#E5F5F9") %>%
        e_grid(show = T)
    })
    # 7 - Impostos----
    ## Mapa - Impostos----
    output$fp7txt1 <- renderText({
      t71()
    })
    output$fp7map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$fp7ri == "Pará") {
        df <- fp7 %>%
          filter(localidade != "Pará", ano == input$fp7ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- fp7 %>%
          filter(localidade != "Pará", ano == input$fp7ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$fp7ri)
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
          "<strong>%s</strong><br/> <b>Valor(R$):</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
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
    ## Tabela - Impostos----
    output$fp7txt2 <- renderText({
      t72()
    })
    output$fp7tab <- renderReactable({
      if (input$fp7ri == "Pará") {
        x <- fp7 %>%
          filter(localidade != "Pará", ano == input$fp7ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- fp7 %>%
          filter(localidade != "Pará", ano == input$fp7ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$fp7ri)
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
            name = "Valor(R$)",
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
    ## Gráfico - Impostos----
    output$fp7txt3 <- renderText({
      t73()
    })
    output$fp7graf <- renderEcharts4r({
      fp7 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
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
          splitLine = list(show = T),
          name = "Ano",
          splitLine = list(show = T),
          nameTextStyle = list(fontWeight = "bold", fontSize = 14)
        ) %>%
        e_y_axis(
          name = "Valor(R$)",
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
        e_datazoom(toolbox = F, fillerColor = "#E5F5F9") %>%
        e_grid(show = T)
    })
    
    # 8 - Receita de Transferências Correntes----
    ## Mapa - Receita de Transferências Correntes----
    output$fp8txt1 <- renderText({
      t81()
    })
    output$fp8map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$fp8ri == "Pará") {
        df <- fp8 %>%
          filter(localidade != "Pará", ano == input$fp8ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- fp8 %>%
          filter(localidade != "Pará", ano == input$fp8ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$fp8ri)
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
          "<strong>%s</strong><br/> <b>Valor(R$):</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
            formatC(
              x$valor,
              format = "f",
              big.mark = ".",
              decimal.mark = ",",
              digits = 0
            )
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
    ## Tabela - Receita de Transferências Correntes----
    output$fp8txt2 <- renderText({
      t82()
    })
    output$fp8tab <- renderReactable({
      if (input$fp8ri == "Pará") {
        x <- fp8 %>%
          filter(localidade != "Pará", ano == input$fp8ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- fp8 %>%
          filter(localidade != "Pará", ano == input$fp8ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$fp8ri)
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
            name = "Valor(R$)",
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
    ## Gráfico - Receita de Transferências Correntes----
    output$fp8txt3 <- renderText({
      t83()
    })
    output$fp8graf <- renderEcharts4r({
      fp8 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
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
          splitLine = list(show = T),
          name = "Ano",
          splitLine = list(show = T),
          nameTextStyle = list(fontWeight = "bold", fontSize = 14)
        ) %>%
        e_y_axis(
          name = "Valor(R$)",
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
        e_datazoom(toolbox = F, fillerColor = "#E5F5F9") %>%
        e_grid(show = T)
    })
    #DOWNLOADS----
    ## 1 - Repasse de ICMS----
    ## Tabela - Repasse de ICMS----
    # Filtra os dados
    fp1_1 <- reactive({
      if (input$fp1ri == "Pará") {
        x <- fp1 %>%
          filter(localidade != "Pará", 
                 ano == input$fp1ano)
      } else {
        x <- fp1 %>%
          filter(localidade != "Pará", 
                 ano == input$fp1ano,
                 ri == input$fp1ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(fp1_1(), {
      t12()
      downset_Server("fp1_1", fp1_1(), t12())
    })
    ## Gráfico - Repasse de ICMS----
    # Filtra os dados
    fp1_2 <- reactive({
      fp1 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(fp1_2(), {
      t13()
      downset_Server("fp1_2", fp1_2(), t13())
    })
    
    ## 2 - Demonstrativo dos Índices de Participação na Arrecadação do ICMS----
    ## Tabela - Demonstrativo dos Índices de Participação na Arrecadação do ICMS----
    # Filtra os dados
    fp2_1 <- reactive({
      if (input$fp2ri == "Pará") {
        x <- fp2 %>%
          filter(localidade != "Pará", 
                 ano == input$fp2ano)
      } else {
        x <- fp2 %>%
          filter(localidade != "Pará", 
                 ano == input$fp2ano,
                 ri == input$fp2ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(fp2_1(), {
      t22()
      downset_Server("fp2_1", fp2_1(), t22())
    })
    
    ## 3 - Repasse de IPI dos Municípios----
    ## Tabela - Repasse de IPI dos Municípios----
    # Filtra os dados
    fp3_1 <- reactive({
      if (input$fp3ri == "Pará") {
        x <- fp3 %>%
          filter(localidade != "Pará", 
                 ano == input$fp3ano) 
      } else {
        x <- fp3 %>%
          filter(localidade != "Pará", 
                 ano == input$fp3ano,
                 ri == input$fp3ri) 
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(fp3_1(), {
      t32()
      downset_Server("fp3_1", fp3_1(), t32())
    })
    ## Gráfico - Repasse de IPI dos Municípios----
    # Filtra os dados
    fp3_2 <- reactive({
      fp3 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(fp3_2(), {
      t33()
      downset_Server("fp3_2", fp3_2(), t33())
    })
    
    ## 4 - Repasse de IPVA  dos Municípios----
    ## Tabela - Repasse de IPVA  dos Municípios----
    # Filtra os dados
    fp4_1 <- reactive({
      if (input$fp4ri == "Pará") {
        x <- fp4 %>%
          filter(localidade != "Pará", 
                 ano == input$fp4ano)
      } else {
        x <- fp4 %>%
          filter(localidade != "Pará", 
                 ano == input$fp4ano,
                 ri == input$fp4ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(fp4_1(), {
      t42()
      downset_Server("fp4_1", fp4_1(), t42())
    })
    ## Gráfico - Repasse de IPVA  dos Municípios----
    # Filtra os dados
    fp4_2 <- reactive({
      fp4 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(fp4_2(), {
      t43()
      downset_Server("fp4_2", fp4_2(), t43())
    })
    
    ## 5 - Receita Orçamentária----
    ## Tabela - Receita Orçamentária----
    # Filtra os dados
    fp5_1 <- reactive({
      if (input$fp5ri == "Pará") {
        x <- fp5 %>%
          filter(localidade != "Pará",
                 ano == input$fp5ano)
      } else {
        x <- fp5 %>%
          filter(localidade != "Pará", 
                 ano == input$fp5ano,
                 ri == input$fp5ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(fp5_1(), {
      t52()
      downset_Server("fp5_1", fp5_1(), t52())
    })
    ## Gráfico - Receita Orçamentária----
    # Filtra os dados
    fp5_2 <- reactive({
      fp5 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(fp5_2(), {
      t53()
      downset_Server("fp5_2", fp5_2(), t53())
    })
    
    ## 6 - Receitas Correntes----
    ## Tabela - Receitas Correntes----
    # Filtra os dados
    fp6_1 <- reactive({
      if (input$fp6ri == "Pará") {
        x <- fp6 %>%
          filter(localidade != "Pará", 
                 ano == input$fp6ano)
      } else {
        x <- fp6 %>%
          filter(localidade != "Pará", 
                 ano == input$fp6ano,
                 ri == input$fp6ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(fp6_1(), {
      t62()
      downset_Server("fp6_1", fp6_1(), t62())
    })
    ## Gráfico - Receitas Correntes----
    # Filtra os dados
    fp6_2 <- reactive({
      fp6 %>%
        filter(localidade == "Pará") %>% 
        select(ri,
               localidade,
               ano,
               categoria,
               valor)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(fp6_2(), {
      t63()
      downset_Server("fp6_2", fp6_2(), t63())
    })
    
    ## 7 - Impostos----
    ## Tabela - Impostos----
    # Filtra os dados
    fp7_1 <- reactive({
      if (input$fp7ri == "Pará") {
        x <- fp7 %>%
          filter(localidade != "Pará", ano == input$fp7ano)
      } else {
        x <- fp7 %>%
          filter(localidade != "Pará", ano == input$fp7ano,
                 ri == input$fp7ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(fp7_1(), {
      t72()
      downset_Server("fp7_1", fp7_1(), t72())
    })
    ## Gráfico - Impostos----
    # Filtra os dados
    fp7_2 <- reactive({
      fp7 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(fp7_2(), {
      t73()
      downset_Server("fp7_2", fp7_2(), t73())
    })
    
    ## 8 - Receita de Transferências Correntes----
    ## Tabela - Receita de Transferências Correntes----
    # Filtra os dados
    fp8_1 <- reactive({
      if (input$fp8ri == "Pará") {
        x <- fp8 %>%
          filter(localidade != "Pará", 
                 ano == input$fp8ano)
      } else {
        x <- fp8 %>%
          filter(localidade != "Pará", 
                 ano == input$fp8ano,
                 ri == input$fp8ri)       }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(fp8_1(), {
      t82()
      downset_Server("fp8_1", fp8_1(), t82())
    })
    ## Gráfico - Receita de Transferências Correntes----
    # Filtra os dados
    fp8_2 <- reactive({
      fp8 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(fp8_2(), {
      t83()
      downset_Server("fp8_2", fp8_2(), t83())
    })
  })
}

# # Play do Módulo
# ui <- dashboardPage(header = dashboardHeader(),
#                     sidebar = dashboardSidebar(),
#                     body = dashboardBody(fluidPage(economia_fp_pa_ui("economia_fp_pa"))))
# server <- function(input, output) {
#   economia_fp_pa_Server("economia_fp_pa")
# }
# shinyApp(ui, server)
