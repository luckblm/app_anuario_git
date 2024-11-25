# Funções de módulo de Economia - PIB - Estadual
# Função de UI
economia_pib_pa_ui <- function(id) {
  fluidPage(
    # CAMINHO DO ARQUIVO CSS----
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "estilo.css")
    ),
    # Lista de Navegação lateral----
    div(
      class = "navbar_economia",
      navbarPage(
        tags$b("PIB - Pará"),
        navbarMenu(
          tags$b("Escolha um Indicador"),
          # 1 - Produto Interno Bruto a Preços Correntes (em Mil Reais)----
          tabPanel(
            "Produto Interno Bruto a Preços Correntes (em Mil Reais)",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Produto Interno Bruto a Preços Correntes (em Mil Reais)"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "pib1ano"),
                  label = "Ano",
                  choices = sort(unique(pib1[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "pib1ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(pib1[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Produto Interno Bruto a Preços Correntes (em Mil Reais)----
              box(
                title = textOutput(NS(id, "pib1txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "pib1map"), height = "600px"),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  tags$h6(tags$b("Fonte:"), "IBGE/FAPESPA"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                )
              ),
              ## Tabela - Produto Interno Bruto a Preços Correntes (em Mil Reais)----
              box(
                title = textOutput(NS(id, "pib1txt2")),
                collapsible = T,
                collapsed = F,
                width = 12,
                headerBorder = T,
                solidHeader = F,
                withSpinner(
                  reactableOutput(NS(id, "pib1tab"), height = "400px"),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/FAPESPA"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pib1_1"))
                )
              )
            ),
            fluidRow(
              ## Gráfico de linha Produto Interno Bruto a Preços Correntes (em Mil Reais)----
              box(
                title = textOutput(NS(id, "pib1txt3")),
                collapsible = T,
                collapsed = F,
                width = 12,
                headerBorder = T,
                solidHeader = F,
                withSpinner(
                  echarts4rOutput(NS(id, "pib1graf")),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/FAPESPA"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pib1_2"))
                )
              )
            )
          ),
          # 2 - Valor Adicionado Bruto a Preços Correntes Total (em Mil Reais)----
          tabPanel(
            "Valor Adicionado Bruto a Preços Correntes Total",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Valor Adicionado Bruto a Preços Correntes Total"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "pib2ano"),
                  label = "Ano",
                  choices = sort(unique(pib2[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "pib2ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(pib2[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Valor Adicionado Bruto a Preços Correntes Total (em Mil Reais)----
              box(
                title = textOutput(NS(id, "pib2txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "pib2map"), height = "600px"),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  tags$h6(tags$b("Fonte:"), "IBGE/FAPESPA"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                )
              ),
              ## Tabela - Valor Adicionado Bruto a Preços Correntes Total (em Mil Reais)----
              box(
                title = textOutput(NS(id, "pib2txt2")),
                collapsible = T,
                collapsed = F,
                width = 12,
                headerBorder = T,
                solidHeader = F,
                withSpinner(
                  reactableOutput(NS(id, "pib2tab"), height = "400px"),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/FAPESPA"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pib2_1"))
                )
              )
            ),
            fluidRow(
              ## Gráfico de linha Valor Adicionado Bruto a Preços Correntes Total (em Mil Reais)----
              box(
                title = textOutput(NS(id, "pib2txt3")),
                collapsible = T,
                collapsed = F,
                width = 12,
                headerBorder = T,
                solidHeader = F,
                withSpinner(
                  echarts4rOutput(NS(id, "pib2graf")),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/FAPESPA"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pib2_2"))
                )
              )
            )
          ),
          # 3 - Valor Adicionado Bruto a Preços Correntes da Agropecuária (em Mil Reais)----
          tabPanel(
            "Valor Adicionado Bruto a Preços Correntes da Agropecuária",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Valor Adicionado Bruto a Preços Correntes da Agropecuária"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "pib3ano"),
                  label = "Ano",
                  choices = sort(unique(pib3[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "pib3ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(pib3[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Valor Adicionado Bruto a Preços Correntes da Agropecuária (em Mil Reais)----
              box(
                title = textOutput(NS(id, "pib3txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "pib3map"), height = "600px"),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  tags$h6(tags$b("Fonte:"), "IBGE/FAPESPA"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                )
              ),
              ## Tabela - Valor Adicionado Bruto a Preços Correntes da Agropecuária (em Mil Reais)----
              box(
                title = textOutput(NS(id, "pib3txt2")),
                collapsible = T,
                collapsed = F,
                width = 12,
                headerBorder = T,
                solidHeader = F,
                withSpinner(
                  reactableOutput(NS(id, "pib3tab"), height = "400px"),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/FAPESPA"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pib3_1"))
                )
              )
            ),
            fluidRow(
              ## Gráfico de linha Valor Adicionado Bruto a Preços Correntes da Agropecuária (em Mil Reais)----
              box(
                title = textOutput(NS(id, "pib3txt3")),
                collapsible = T,
                collapsed = F,
                width = 12,
                headerBorder = T,
                solidHeader = F,
                withSpinner(
                  echarts4rOutput(NS(id, "pib3graf")),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/FAPESPA"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pib3_2"))
                )
              )
            )
          ),
          # 4 - Valor adicionado bruto a preços correntes da indústria (em Mil Reais)----
          tabPanel(
            "Valor adicionado bruto a preços correntes da indústria",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Valor adicionado bruto a preços correntes da indústria"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "pib4ano"),
                  label = "Ano",
                  choices = sort(unique(pib4[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "pib4ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(pib4[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Valor adicionado bruto a preços correntes da indústria (em Mil Reais)----
              box(
                title = textOutput(NS(id, "pib4txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "pib4map"), height = "600px"),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  tags$h6(tags$b("Fonte:"), "IBGE/FAPESPA"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                )
              ),
              ## Tabela - Valor adicionado bruto a preços correntes da indústria (em Mil Reais)----
              box(
                title = textOutput(NS(id, "pib4txt2")),
                collapsible = T,
                collapsed = F,
                width = 12,
                headerBorder = T,
                solidHeader = F,
                withSpinner(
                  reactableOutput(NS(id, "pib4tab"), height = "400px"),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/FAPESPA"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pib4_1"))
                )
              )
            ),
            fluidRow(
              ## Gráfico de linha Valor adicionado bruto a preços correntes da indústria (em Mil Reais)----
              box(
                title = textOutput(NS(id, "pib4txt3")),
                collapsible = T,
                collapsed = F,
                width = 12,
                headerBorder = T,
                solidHeader = F,
                withSpinner(
                  echarts4rOutput(NS(id, "pib4graf")),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/FAPESPA"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pib4_2"))
                )
              )
            )
          ),
          # 5 - Valor Adicionado Bruto a Preços Correntes dos Serviços, Exclusive Administração, Saúde e Educação Públicas e Seguridade Social (em Mil Reais)----
          tabPanel(
            "Valor Adicionado Bruto a Preços Correntes dos Serviços, Exclusive Administração, Saúde e Educação Públicas e Seguridade Social",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Valor Adicionado Bruto a Preços Correntes dos Serviços, Exclusive Administração, Saúde e Educação Públicas e Seguridade Social"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "pib5ano"),
                  label = "Ano",
                  choices = sort(unique(pib5[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "pib5ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(pib5[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Valor Adicionado Bruto a Preços Correntes dos Serviços, Exclusive Administração, Saúde e Educação Públicas e Seguridade Social (em Mil Reais)----
              box(
                title = textOutput(NS(id, "pib5txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "pib5map"), height = "600px"),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  tags$h6(tags$b("Fonte:"), "IBGE/FAPESPA"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                )
              ),
              ## Tabela - Valor Adicionado Bruto a Preços Correntes dos Serviços, Exclusive Administração, Saúde e Educação Públicas e Seguridade Social (em Mil Reais)----
              box(
                title = textOutput(NS(id, "pib5txt2")),
                collapsible = T,
                collapsed = F,
                width = 12,
                headerBorder = T,
                solidHeader = F,
                withSpinner(
                  reactableOutput(NS(id, "pib5tab"), height = "400px"),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/FAPESPA"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pib5_1"))
                )
              )
            ),
            fluidRow(
              ## Gráfico de linha Valor Adicionado Bruto a Preços Correntes dos Serviços, Exclusive Administração, Saúde e Educação Públicas e Seguridade Social (em Mil Reais)----
              box(
                title = textOutput(NS(id, "pib5txt3")),
                collapsible = T,
                collapsed = F,
                width = 12,
                headerBorder = T,
                solidHeader = F,
                withSpinner(
                  echarts4rOutput(NS(id, "pib5graf")),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/FAPESPA"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pib5_2"))
                )
              )
            )
          ),
          # 6 - Valor Adicionado Bruto a Preços Correntes da Administração, Saúde e Educação Públicas e Seguridade Social (em Mil Reais)----
          tabPanel(
            "Valor Adicionado Bruto a Preços Correntes da Administração, Saúde e Educação Públicas e Seguridade Social",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Valor Adicionado Bruto a Preços Correntes da Administração, Saúde e Educação Públicas e Seguridade Social"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "pib6ano"),
                  label = "Ano",
                  choices = sort(unique(pib6[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "pib6ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(pib6[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Valor Adicionado Bruto a Preços Correntes da Administração, Saúde e Educação Públicas e Seguridade Social (em Mil Reais)----
              box(
                title = textOutput(NS(id, "pib6txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "pib6map"), height = "600px"),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  tags$h6(tags$b("Fonte:"), "IBGE/FAPESPA"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                )
              ),
              ## Tabela - Valor Adicionado Bruto a Preços Correntes da Administração, Saúde e Educação Públicas e Seguridade Social (em Mil Reais)----
              box(
                title = textOutput(NS(id, "pib6txt2")),
                collapsible = T,
                collapsed = F,
                width = 12,
                headerBorder = T,
                solidHeader = F,
                withSpinner(
                  reactableOutput(NS(id, "pib6tab"), height = "400px"),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/FAPESPA"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pib6_1"))
                )
              )
            ),
            fluidRow(
              ## Gráfico de linha Valor Adicionado Bruto a Preços Correntes da Administração, Saúde e Educação Públicas e Seguridade Social (em Mil Reais)----
              box(
                title = textOutput(NS(id, "pib6txt3")),
                collapsible = T,
                collapsed = F,
                width = 12,
                headerBorder = T,
                solidHeader = F,
                withSpinner(
                  echarts4rOutput(NS(id, "pib6graf")),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/FAPESPA"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pib6_2"))
                )
              )
            )
          ),
          # 7 - Impostos, Líquidos de Subsídios, Sobre Produtos a Preços Correntes (em Mil Reais)----
          tabPanel(
            "Impostos, Líquidos de Subsídios, Sobre Produtos a Preços Correntes",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Impostos, Líquidos de Subsídios, Sobre Produtos a Preços Correntes"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "pib7ano"),
                  label = "Ano",
                  choices = sort(unique(pib7[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "pib7ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(pib7[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Impostos, Líquidos de Subsídios, Sobre Produtos a Preços Correntes (em Mil Reais)----
              box(
                title = textOutput(NS(id, "pib7txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "pib7map"), height = "600px"),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  tags$h6(tags$b("Fonte:"), "IBGE/FAPESPA"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                )
              ),
              ## Tabela - Impostos, Líquidos de Subsídios, Sobre Produtos a Preços Correntes (em Mil Reais)----
              box(
                title = textOutput(NS(id, "pib7txt2")),
                collapsible = T,
                collapsed = F,
                width = 12,
                headerBorder = T,
                solidHeader = F,
                withSpinner(
                  reactableOutput(NS(id, "pib7tab"), height = "400px"),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/FAPESPA"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pib7_1"))
                )
              )
            ),
            fluidRow(
              ## Gráfico de linha Impostos, Líquidos de Subsídios, Sobre Produtos a Preços Correntes (em Mil Reais)----
              box(
                title = textOutput(NS(id, "pib7txt3")),
                collapsible = T,
                collapsed = F,
                width = 12,
                headerBorder = T,
                solidHeader = F,
                withSpinner(
                  echarts4rOutput(NS(id, "pib7graf")),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/FAPESPA"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pib7_2"))
                )
              )
            )
          ),
          # 8 - Evolução do Produto Interno Bruto per Capita----
          tabPanel(
            "Evolução do Produto Interno Bruto per Capita",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   ""),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "pib8ano"),
                  label = "Ano",
                  choices = sort(unique(pib8[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "pib8ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(pib8[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Evolução do Produto Interno Bruto per Capita----
              box(
                title = textOutput(NS(id, "pib8txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "pib8map"), height = "600px"),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  tags$h6(tags$b("Fonte:"), "IBGE/FAPESPA"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                )
              ),
              ## Tabela - Evolução do Produto Interno Bruto per Capita----
              box(
                title = textOutput(NS(id, "pib8txt2")),
                collapsible = T,
                collapsed = F,
                width = 12,
                headerBorder = T,
                solidHeader = F,
                withSpinner(
                  reactableOutput(NS(id, "pib8tab"), height = "400px"),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/FAPESPA"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pib8_1"))
                )
              )
            ),
            fluidRow(
              ## Gráfico de linha Evolução do Produto Interno Bruto per Capita----
              box(
                title = textOutput(NS(id, "pib8txt3")),
                collapsible = T,
                collapsed = F,
                width = 12,
                headerBorder = T,
                solidHeader = F,
                withSpinner(
                  echarts4rOutput(NS(id, "pib8graf")),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/FAPESPA"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pib8_2"))
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
economia_pib_pa_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    #TÍTULOS----
    # 1 - Produto Interno Bruto a Preços Correntes (em Mil Reais)----
    ## Mapa - Produto Interno Bruto a Preços Correntes (em Mil Reais)----
    t11 <- reactive({
      if (input$pib1ri == "Pará") {
        paste0(
          "Produto Interno Bruto a Preços Correntes (em Mil Reais), ",
          input$pib1ri,
          " - ",
          input$pib1ano
        )
      } else{
        paste0(
          "Produto Interno Bruto a Preços Correntes (em Mil Reais), Região de Integração ",
          input$pib1ri,
          " - ",
          input$pib1ano
        )
      }
    })
    
    ## Tabela - Produto Interno Bruto a Preços Correntes (em Mil Reais)----
    t12 <- reactive({
      if (input$pib1ri == "Pará") {
        paste0(
          "Produto Interno Bruto a Preços Correntes (em Mil Reais) dos Municipios, ",
          input$pib1ri,
          " - ",
          input$pib1ano
        )
      } else{
        paste0(
          "Produto Interno Bruto a Preços Correntes (em Mil Reais) dos Municipios, Região de Integração ",
          input$pib1ri,
          " - ",
          input$pib1ano
        )
      }
    })
    
    ## Gráfico de Barras Produto Interno Bruto a Preços Correntes (em Mil Reais)----
    t13 <- reactive({
      paste0(
        "Produto Interno Bruto a Preços Correntes (em Mil Reais), Pará - ",
        min(pib1$ano),
        " a ",
        max(pib1$ano)
      )
    })

    # 2 - Valor Adicionado Bruto a Preços Correntes Total (em Mil Reais)----
    ## Mapa - Valor Adicionado Bruto a Preços Correntes Total (em Mil Reais)----
    t21 <- reactive({
      if (input$pib2ri == "Pará") {
        paste0(
          "Valor Adicionado Bruto a Preços Correntes Total (em Mil Reais), ",
          input$pib2ri,
          " - ",
          input$pib2ano
        )
      } else{
        paste0(
          "Valor Adicionado Bruto a Preços Correntes Total (em Mil Reais), Região de Integração ",
          input$pib2ri,
          " - ",
          input$pib2ano
        )
      }
    })
    
    ## Tabela - Valor Adicionado Bruto a Preços Correntes Total (em Mil Reais)----
    t22 <- reactive({
      if (input$pib2ri == "Pará") {
        paste0(
          "Valor Adicionado Bruto a Preços Correntes Total (em Mil Reais) dos Municípios, ",
          input$pib2ri,
          " - ",
          input$pib2ano
        )
      } else{
        paste0(
          "Valor Adicionado Bruto a Preços Correntes Total (em Mil Reais) dos Municípios, Região de Integração ",
          input$pib2ri,
          " - ",
          input$pib2ano
        )
      }
    })
    

    ## Gráfico de Barras Valor Adicionado Bruto a Preços Correntes Total (em Mil Reais)----
    t23 <- reactive({
      paste0(
        "Valor Adicionado Bruto a Preços Correntes Total (em Mil Reais), Pará - ",
        min(pib2$ano),
        " a ",
        max(pib2$ano)
      )
    })
    # 3 - Valor Adicionado Bruto a Preços Correntes da Agropecuária (em Mil Reais)----
    ## Mapa - Valor Adicionado Bruto a Preços Correntes da Agropecuária (em Mil Reais)----
    t31 <- reactive({
      if (input$pib3ri == "Pará") {
        paste0(
          "Valor Adicionado Bruto a Preços Correntes da Agropecuária (em Mil Reais), ",
          input$pib3ri,
          " - ",
          input$pib3ano
        )
      } else{
        paste0(
          "Valor Adicionado Bruto a Preços Correntes da Agropecuária (em Mil Reais), Região de Integração ",
          input$pib3ri,
          " - ",
          input$pib3ano
        )
      }
    })
    
    ## Tabela - da Valor Adicionado Bruto a Preços Correntes da Agropecuária (em Mil Reais)----
    t32 <- reactive({
      if (input$pib3ri == "Pará") {
        paste0(
          "Valor Adicionado Bruto a Preços Correntes da Agropecuária (em Mil Reais) dos Municípios, ",
          input$pib3ri,
          " - ",
          input$pib3ano
        )
      } else{
        paste0(
          "Valor Adicionado Bruto a Preços Correntes da Agropecuária (em Mil Reais) dos Municípios, Região de Integração ",
          input$pib3ri,
          " - ",
          input$pib3ano
        )
      }
    })
    
    ## Gráfico de Barras Valor Adicionado Bruto a Preços Correntes da Agropecuária (em Mil Reais)----
    t33 <- reactive({
      paste0(
        "Valor Adicionado Bruto a Preços Correntes da Agropecuária (em Mil Reais), Pará - ",
        min(pib3$ano),
        " a ",
        max(pib3$ano)
      )
    })
    # 4 - Valor adicionado bruto a preços correntes da indústria (em Mil Reais)----
    ## Mapa - Valor adicionado bruto a preços correntes da indústria (em Mil Reais)----
    t41 <- reactive({
      if (input$pib4ri == "Pará") {
        paste0(
          "Valor adicionado bruto a preços correntes da indústria (em Mil Reais), ",
          input$pib4ri,
          " - ",
          input$pib4ano
        )
      } else{
        paste0(
          "Valor adicionado bruto a preços correntes da indústria (em Mil Reais), Região de Integração ",
          input$pib4ri,
          " - ",
          input$pib4ano
        )
      }
    })
    
    ## Tabela - Valor adicionado bruto a preços correntes da indústria (em Mil Reais)----
    t42 <- reactive({
      if (input$pib4ri == "Pará") {
        paste0(
          "Valor adicionado bruto a preços correntes da indústria (em Mil Reais) dos municípios, ",
          input$pib4ri,
          " - ",
          input$pib4ano
        )
      } else{
        paste0(
          "Valor adicionado bruto a preços correntes da indústria (em Mil Reais)dos municípios, Região de Integração ",
          input$pib4ri,
          " - ",
          input$pib4ano
        )
      }
    })
    
    ## Gráfico de Barras Valor adicionado bruto a preços correntes da indústria (em Mil Reais)----
    t43 <- reactive({
      paste0(
        "Valor adicionado bruto a preços correntes da indústria (em Mil Reais), Pará - ",
        min(pib4$ano),
        " a ",
        max(pib4$ano)
      )
    })
    # 5 - Valor Adicionado Bruto a Preços Correntes dos Serviços, Exclusive Administração, Saúde e Educação Públicas e Seguridade Social (em Mil Reais)----
    ## Mapa - Valor Adicionado Bruto a Preços Correntes dos Serviços, Exclusive Administração, Saúde e Educação Públicas e Seguridade Social (em Mil Reais)----
    t51 <- reactive({
      if (input$pib5ri == "Pará") {
        paste0(
          "Valor Adicionado Bruto a Preços Correntes dos Serviços, Exclusive Administração, Saúde e Educação Públicas e Seguridade Social (em Mil Reais), ",
          input$pib5ri,
          " - ",
          input$pib5ano
        )
      } else{
        paste0(
          "Valor Adicionado Bruto a Preços Correntes dos Serviços, Exclusive Administração, Saúde e Educação Públicas e Seguridade Social (em Mil Reais), Região de Integração ",
          input$pib5ri,
          " - ",
          input$pib5ano
        )
      }
    })
    
    ## Tabela - Valor Adicionado Bruto a Preços Correntes dos Serviços, Exclusive Administração, Saúde e Educação Públicas e Seguridade Social (em Mil Reais)----
    t52 <- reactive({
      if (input$pib5ri == "Pará") {
        paste0(
          "Valor Adicionado Bruto a Preços Correntes dos Serviços, Exclusive Administração, Saúde e Educação Públicas e Seguridade Social (em Mil Reais) dos Municípios, ",
          input$pib5ri,
          " - ",
          input$pib5ano
        )
      } else{
        paste0(
          "Valor Adicionado Bruto a Preços Correntes dos Serviços, Exclusive Administração, Saúde e Educação Públicas e Seguridade Social (em Mil Reais) dos Municípios, Região de Integração ",
          input$pib5ri,
          " - ",
          input$pib5ano
        )
      }
    })
    
    ## Gráfico de Barras Valor Adicionado Bruto a Preços Correntes dos Serviços, Exclusive Administração, Saúde e Educação Públicas e Seguridade Social (em Mil Reais)----
    t53 <- reactive({
      paste0(
        "Valor Adicionado Bruto a Preços Correntes dos Serviços, Exclusive Administração, Saúde e Educação Públicas e Seguridade Social (em Mil Reais), Pará - ",
        min(pib5$ano),
        " a ",
        max(pib5$ano)
      )
    })
    # 6 - Valor Adicionado Bruto a Preços Correntes da Administração, Saúde e Educação Públicas e Seguridade Social (em Mil Reais)----
    ## Mapa - Valor Adicionado Bruto a Preços Correntes da Administração, Saúde e Educação Públicas e Seguridade Social (em Mil Reais)----
    t61 <- reactive({
      if (input$pib6ri == "Pará") {
        paste0(
          "Valor Adicionado Bruto a Preços Correntes da Administração, Saúde e Educação Públicas e Seguridade Social (em Mil Reais), ",
          input$pib6ri,
          " - ",
          input$pib6ano
        )
      } else{
        paste0(
          "Valor Adicionado Bruto a Preços Correntes da Administração, Saúde e Educação Públicas e Seguridade Social (em Mil Reais), Região de Integração ",
          input$pib6ri,
          " - ",
          input$pib6ano
        )
      }
    })
    
    ## Tabela - Valor Adicionado Bruto a Preços Correntes da Administração, Saúde e Educação Públicas e Seguridade Social (em Mil Reais)----
    t62 <- reactive({
      if (input$pib6ri == "Pará") {
        paste0(
          "Valor Adicionado Bruto a Preços Correntes da Administração, Saúde e Educação Públicas e Seguridade Social (em Mil Reais) dos Municípios, ",
          input$pib6ri,
          " - ",
          input$pib6ano
        )
      } else{
        paste0(
          "Valor Adicionado Bruto a Preços Correntes da Administração, Saúde e Educação Públicas e Seguridade Social (em Mil Reais) dos Municípios, Região de Integração ",
          input$pib6ri,
          " - ",
          input$pib6ano
        )
      }
    })
    
    ## Gráfico de Barras Valor Adicionado Bruto a Preços Correntes da Administração, Saúde e Educação Públicas e Seguridade Social (em Mil Reais)----
    t63 <- reactive({
      paste0(
        "Valor Adicionado Bruto a Preços Correntes da Administração, Saúde e Educação Públicas e Seguridade Social (em Mil Reais), Pará - ",
        min(pib6$ano),
        " a ",
        max(pib6$ano)
      )
    })

    # 7 - Impostos, Líquidos de Subsídios, Sobre Produtos a Preços Correntes (em Mil Reais)----
    ## Mapa - Impostos, Líquidos de Subsídios, Sobre Produtos a Preços Correntes (em Mil Reais)----
    t71 <- reactive({
      if (input$pib7ri == "Pará") {
        paste0(
          "Impostos, Líquidos de Subsídios, Sobre Produtos a Preços Correntes (em Mil Reais), ",
          input$pib7ri,
          " - ",
          input$pib7ano
        )
      } else{
        paste0(
          "Impostos, Líquidos de Subsídios, Sobre Produtos a Preços Correntes (em Mil Reais), Região de Integração ",
          input$pib7ri,
          " - ",
          input$pib7ano
        )
      }
    })
    
    ## Tabela - Impostos, Líquidos de Subsídios, Sobre Produtos a Preços Correntes (em Mil Reais)----
    t72 <- reactive({
      if (input$pib7ri == "Pará") {
        paste0(
          "Impostos, Líquidos de Subsídios, Sobre Produtos a Preços Correntes (em Mil Reais) dos Municípios, ",
          input$pib7ri,
          " - ",
          input$pib7ano
        )
      } else{
        paste0(
          "Impostos, Líquidos de Subsídios, Sobre Produtos a Preços Correntes (em Mil Reais) dos Municípios, Região de Integração ",
          input$pib7ri,
          " - ",
          input$pib7ano
        )
      }
    })
    
    ## Gráfico de Barras Impostos, Líquidos de Subsídios, Sobre Produtos a Preços Correntes (em Mil Reais)----
    t73 <- reactive({
      paste0(
        "Impostos, Líquidos de Subsídios, Sobre Produtos a Preços Correntes (em Mil Reais), Pará - ",
        min(pib7$ano),
        " a ",
        max(pib7$ano)
      )
    })

    # 8 - Evolução do Produto Interno Bruto per Capita----
    ## Mapa - Evolução do Produto Interno Bruto per Capita----
    t81 <- reactive({
      if (input$pib8ri == "Pará") {
        paste0(
          "Produto Interno Bruto per Capita, ",
          input$pib8ri,
          " - ",
          input$pib8ano
        )
      } else{
        paste0(
          "Produto Interno Bruto per Capita, Região de Integração ",
          input$pib8ri,
          " - ",
          input$pib8ano
        )
      }
    })
    
    ## Tabela - Evolução do Produto Interno Bruto per Capita----
    t82 <- reactive({
      if (input$pib8ri == "Pará") {
        paste0(
          "Produto Interno Bruto per Capita dos Municípios, ",
          input$pib8ri,
          " - ",
          input$pib8ano
        )
      } else{
        paste0(
          "Produto Interno Bruto per Capita dos Municípios, Região de Integração ",
          input$pib8ri,
          " - ",
          input$pib8ano
        )
      }
    })
    
    ## Gráfico de Barras Evolução do Produto Interno Bruto per Capita----
    t83 <- reactive({
      paste0(
        "Evolução do Produto Interno Bruto per Capita, Pará - ",
        min(pib8$ano),
        " a ",
        max(pib8$ano)
      )
    })
    #VISUALIZAÇÃO----
    # 1 - Produto Interno Bruto a Preços Correntes (em Mil Reais)----
    ## Mapa - Produto Interno Bruto a Preços Correntes (em Mil Reais)----
    output$pib1txt1 <- renderText({
      t11()
    })

    output$pib1map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$pib1ri == "Pará") {
        df <- pib1 %>%
          filter(localidade != "Pará", ano == input$pib1ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- pib1 %>%
          filter(localidade != "Pará", ano == input$pib1ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$pib1ri)
      }

      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }

      pal <- colorBin(c("#ffff81", "#fad254", "#f2a92a", "#ae520b", "#6b0000"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Valor (em Mil Reais):</b> %s ",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
            format(x$valor, big.mark = ".", decimal.mark = ",", digits = 4)
          )
          
        ) %>% lapply(htmltools::HTML)

      # Mapas com leafleft
      leaflet(x,
        options =
          leafletOptions(
            minZoom = 0,
            maxZoom = 15
          )
      ) %>%
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
          title = "Valor (em Mil Reais)",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 0)
        )
    })
    ## Tabela - Produto Interno Bruto a Preços Correntes (em Mil Reais)----
    output$pib1txt2 <- renderText({
      t12()
    })

    output$pib1tab <- renderReactable({
      if (input$pib1ri == "Pará") {
        x <- pib1 %>%
          filter(localidade != "Pará", ano == input$pib1ano) %>%
          select(ri, localidade, valor) %>%
          mutate(Percentual = (valor / sum(valor)) * 100)
      } else {
        x <- pib1 %>%
          filter(localidade != "Pará", ano == input$pib1ano) %>%
          select(ri, localidade, valor)
        x <- x %>%
          filter(ri == input$pib1ri) %>%
          mutate(Percentual = (valor / sum(valor)) * 100)
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
            name = "Valor (em Mil Reais)",
            format = colFormat(
              separators = T,
              locales = "pt-BR",
              digits = 0
            )
          ),
          Percentual = colDef(
            name = "Percentual(%)",
            format = colFormat(
              separators = T,
              locales = "pt-BR",
              digits = 2
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
    ## Gráfico de Barras Produto Interno Bruto a Preços Correntes (em Mil Reais)----
    output$pib1txt3 <- renderText({
      t13()
    })
    output$pib1graf <- renderEcharts4r({
      pib1 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#f2c94e",
          name = "Valor(em Mil Reais)",
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
          name = "Valor(em Mil Reais)",
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
    })

    # 2 - Valor Adicionado Bruto a Preços Correntes Total (em Mil Reais)----
    ## Mapa - Valor Adicionado Bruto a Preços Correntes Total (em Mil Reais)----
    output$pib2txt1 <- renderText({
      t21()
    })

    output$pib2map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$pib2ri == "Pará") {
        df <- pib2 %>%
          filter(localidade != "Pará", ano == input$pib2ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- pib2 %>%
          filter(localidade != "Pará", ano == input$pib2ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$pib2ri)
      }

      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }

      pal <- colorBin(c("#ffff81", "#fad254", "#f2a92a", "#ae520b", "#6b0000"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Total (em Mil Reais):</b> %s ",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
          format(x$valor, big.mark = ".", decimal.mark = ",", digits = 4)
          )
        ) %>% lapply(htmltools::HTML)

      # Mapas com leafleft
      leaflet(x,
        options =
          leafletOptions(
            minZoom = 0,
            maxZoom = 15
          )
      ) %>%
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
          title = "Total (em Mil Reais)",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 0)
        )
    })

    ## Tabela - Valor Adicionado Bruto a Preços Correntes Total (em Mil Reais)----
    output$pib2txt2 <- renderText({
      t22()
    })

    output$pib2tab <- renderReactable({
      if (input$pib2ri == "Pará") {
        x <- pib2 %>%
          filter(localidade != "Pará", ano == input$pib2ano) %>%
          select(ri, localidade, valor) %>%
          mutate(Percentual = (valor / sum(valor)) * 100)
      } else {
        x <- pib2 %>%
          filter(localidade != "Pará", ano == input$pib2ano) %>%
          select(ri, localidade, valor)
        x <- x %>%
          filter(ri == input$pib2ri) %>%
          mutate(Percentual = (valor / sum(valor)) * 100)
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
            name = "Valor(em Mil Reais)",
            format = colFormat(
              separators = T,
              locales = "pt-BR",
              digits = 0
            )
          ),
          Percentual = colDef(
            name = "Percentual(%)",
            format = colFormat(
              separators = T,
              locales = "pt-BR",
              digits = 2
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

    ## Gráfico de Barras Valor Adicionado Bruto a Preços Correntes Total (em Mil Reais)----
    output$pib2txt3 <- renderText({
      t23()
      })
    output$pib2graf <- renderEcharts4r({
      pib2 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#f2c94e",
          name = "Valor(em Mil Reais)",
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
          name = "Valor(em Mil Reais)",
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
    })


    # 3 - Valor Adicionado Bruto a Preços Correntes da Agropecuária (em Mil Reais)----
    ## Mapa - Valor Adicionado Bruto a Preços Correntes da Agropecuária (em Mil Reais)----
    output$pib3txt1 <- renderText({
      t31()
    })

    output$pib3map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$pib3ri == "Pará") {
        df <- pib3 %>%
          filter(localidade != "Pará", ano == input$pib3ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- pib3 %>%
          filter(localidade != "Pará", ano == input$pib3ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$pib3ri)
      }

      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }

      pal <- colorBin(c("#ffff81", "#fad254", "#f2a92a", "#ae520b", "#6b0000"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Total (em Mil Reais):</b> %s ",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
          format(x$valor, big.mark = ".", decimal.mark = ",", digits = 4)
          )
        ) %>% lapply(htmltools::HTML)

      # Mapas com leafleft
      leaflet(x,
        options =
          leafletOptions(
            minZoom = 0,
            maxZoom = 15
          )
      ) %>%
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
          title = "Total (em Mil Reais)",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 0)
        )
    })

    ## Tabela - da Valor Adicionado Bruto a Preços Correntes da Agropecuária (em Mil Reais)----
    output$pib3txt2 <- renderText({
      t32()
    })

    output$pib3tab <- renderReactable({
      if (input$pib3ri == "Pará") {
        x <- pib3 %>%
          filter(localidade != "Pará", ano == input$pib3ano) %>%
          select(ri, localidade, valor) %>%
          mutate(Percentual = (valor / sum(valor)) * 100)
      } else {
        x <- pib3 %>%
          filter(localidade != "Pará", ano == input$pib3ano) %>%
          select(ri, localidade, valor)
        x <- x %>%
          filter(ri == input$pib3ri) %>%
          mutate(Percentual = (valor / sum(valor)) * 100)
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
            name = "Valor(em Mil Reais)",
            format = colFormat(
              separators = T,
              locales = "pt-BR",
              digits = 0
            )
          ),
          Percentual = colDef(
            name = "Percentual(%)",
            format = colFormat(
              separators = T,
              locales = "pt-BR",
              digits = 2
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
    ## Gráfico de Barras Valor Adicionado Bruto a Preços Correntes da Agropecuária (em Mil Reais)----
    output$pib3txt3 <- renderText({
      t33()
    })
    output$pib3graf <- renderEcharts4r({
      pib3 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#f2c94e",
          name = "Valor(em Mil Reais)",
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
          name = "Valor(em Mil Reais)",
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
    })
    # 4 - Valor adicionado bruto a preços correntes da indústria (em Mil Reais)----
    ## Mapa - Valor adicionado bruto a preços correntes da indústria (em Mil Reais)----
    output$pib4txt1 <- renderText({
      t41()
    })

    output$pib4map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$pib4ri == "Pará") {
        df <- pib4 %>%
          filter(localidade != "Pará", ano == input$pib4ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- pib4 %>%
          filter(localidade != "Pará", ano == input$pib4ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$pib4ri)
      }

      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }

      pal <- colorBin(c("#ffff81", "#fad254", "#f2a92a", "#ae520b", "#6b0000"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Total (em Mil Reais):</b> %s ",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
          format(x$valor, big.mark = ".", decimal.mark = ",", digits = 4)
          )
        ) %>% lapply(htmltools::HTML)

      # Mapas com leafleft
      leaflet(x,
        options =
          leafletOptions(
            minZoom = 0,
            maxZoom = 15
          )
      ) %>%
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
          title = "Total (em Mil Reais)",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 0)
        )
    })


    ## Tabela - Valor adicionado bruto a preços correntes da indústria (em Mil Reais)----
    output$pib4txt2 <- renderText({
      t42()
    })

    output$pib4tab <- renderReactable({
      if (input$pib4ri == "Pará") {
        x <- pib4 %>%
          filter(localidade != "Pará", ano == input$pib4ano) %>%
          select(ri, localidade, valor) %>%
          mutate(Percentual = (valor / sum(valor)) * 100)
      } else {
        x <- pib4 %>%
          filter(localidade != "Pará", ano == input$pib4ano) %>%
          select(ri, localidade, valor)
        x <- x %>%
          filter(ri == input$pib4ri) %>%
          mutate(Percentual = (valor / sum(valor)) * 100)
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
            name = "Valor(em Mil Reais)",
            format = colFormat(
              separators = T,
              locales = "pt-BR",
              digits = 0
            )
          ),
          Percentual = colDef(
            name = "Percentual(%)",
            format = colFormat(
              separators = T,
              locales = "pt-BR",
              digits = 2
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

    ## Gráfico de Barras Valor adicionado bruto a preços correntes da indústria (em Mil Reais)----
    output$pib4txt3 <- renderText({
      t43()
    })
    output$pib4graf <- renderEcharts4r({
      pib4 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#f2c94e",
          name = "Valor(em Mil Reais)",
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
          name = "Valor(em Mil Reais)",
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
    })

    # 5 - Valor Adicionado Bruto a Preços Correntes dos Serviços, Exclusive Administração, Saúde e Educação Públicas e Seguridade Social (em Mil Reais)----
    ## Mapa - Valor Adicionado Bruto a Preços Correntes dos Serviços, Exclusive Administração, Saúde e Educação Públicas e Seguridade Social (em Mil Reais)----
    output$pib5txt1 <- renderText({
      t51()
    })

    output$pib5map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$pib5ri == "Pará") {
        df <- pib5 %>%
          filter(localidade != "Pará", ano == input$pib5ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- pib5 %>%
          filter(localidade != "Pará", ano == input$pib5ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$pib5ri)
      }

      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }

      pal <- colorBin(c("#ffff81", "#fad254", "#f2a92a", "#ae520b", "#6b0000"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Total (em Mil Reais):</b> %s ",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
          format(x$valor, big.mark = ".", decimal.mark = ",", digits = 4)
          )
        ) %>% lapply(htmltools::HTML)

      # Mapas com leafleft
      leaflet(x,
        options =
          leafletOptions(
            minZoom = 0,
            maxZoom = 15
          )
      ) %>%
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
            style = list("font-weight" = "normal", 
            padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"
          )
        ) %>%
        addLegend(
          pal = pal,
          values = ~valor,
          opacity = 0.7,
          title = "Total (em Mil Reais)",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat =
            labelFormat_decimal(
              big.mark = ".",
              decimal.mark = ",",
              digits = 0
            )
        )
    })


    ## Tabela - Valor Adicionado Bruto a Preços Correntes dos Serviços, Exclusive Administração, Saúde e Educação Públicas e Seguridade Social (em Mil Reais)----
    output$pib5txt2 <- renderText({
      t52()
    })

    output$pib5tab <- renderReactable({
      if (input$pib5ri == "Pará") {
        x <- pib5 %>%
          filter(localidade != "Pará", ano == input$pib5ano) %>%
          select(ri, localidade, valor) %>%
          mutate(Percentual = (valor / sum(valor)) * 100)
      } else {
        x <- pib5 %>%
          filter(localidade != "Pará", ano == input$pib5ano) %>%
          select(ri, localidade, valor)
        x <- x %>%
          filter(ri == input$pib5ri) %>%
          mutate(Percentual = (valor / sum(valor)) * 100)
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
            name = "Valor(em Mil Reais)",
            format = colFormat(
              separators = T,
              locales = "pt-BR",
              digits = 0
            )
          ),
          Percentual = colDef(
            name = "Percentual(%)",
            format = colFormat(
              separators = T,
              locales = "pt-BR",
              digits = 2
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

    ## Gráfico de Barras Valor Adicionado Bruto a Preços Correntes dos Serviços, Exclusive Administração, Saúde e Educação Públicas e Seguridade Social (em Mil Reais)----
    output$pib5txt3 <- renderText({
      t53()
    })
    output$pib5graf <- renderEcharts4r({
      pib5 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#f2c94e",
          name = "Valor(em Mil Reais)",
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
          name = "Valor(em Mil Reais)",
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
    })

    # 6 - Valor Adicionado Bruto a Preços Correntes da Administração, Saúde e Educação Públicas e Seguridade Social (em Mil Reais)----
    ## Mapa - Valor Adicionado Bruto a Preços Correntes da Administração, Saúde e Educação Públicas e Seguridade Social (em Mil Reais)----
    output$pib6txt1 <- renderText({
      t61()
    })

    output$pib6map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$pib6ri == "Pará") {
        df <- pib6 %>%
          filter(localidade != "Pará", ano == input$pib6ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- pib6 %>%
          filter(localidade != "Pará", ano == input$pib6ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$pib6ri)
      }

      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }

      pal <- colorBin(c("#ffff81", "#fad254", "#f2a92a", "#ae520b", "#6b0000"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Total (em Mil Reais):</b> %s ",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
          format(x$valor, big.mark = ".", decimal.mark = ",", digits = 4)
          )
        ) %>% lapply(htmltools::HTML)

      # Mapas com leafleft
      leaflet(x,
        options =
          leafletOptions(
            minZoom = 0,
            maxZoom = 15
          )
      ) %>%
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
          title = "Total (em Mil Reais)",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 0)
        )
    })


    ## Tabela - Valor Adicionado Bruto a Preços Correntes da Administração, Saúde e Educação Públicas e Seguridade Social (em Mil Reais)----
    output$pib6txt2 <- renderText({
      t62()
    })

    output$pib6tab <- renderReactable({
      if (input$pib6ri == "Pará") {
        x <- pib6 %>%
          filter(localidade != "Pará", ano == input$pib6ano) %>%
          select(ri, localidade, valor) %>%
          mutate(Percentual = (valor / sum(valor)) * 100)
      } else {
        x <- pib6 %>%
          filter(localidade != "Pará", ano == input$pib6ano) %>%
          select(ri, localidade, valor)
        x <- x %>%
          filter(ri == input$pib6ri) %>%
          mutate(Percentual = (valor / sum(valor)) * 100)
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
            name = "Valor(em Mil Reais)",
            format = colFormat(
              separators = T,
              locales = "pt-BR",
              digits = 0
            )
          ),
          Percentual = colDef(
            name = "Percentual(%)",
            format = colFormat(
              separators = T,
              locales = "pt-BR",
              digits = 2
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

    ## Gráfico de Barras Valor Adicionado Bruto a Preços Correntes da Administração, Saúde e Educação Públicas e Seguridade Social (em Mil Reais)----
    output$pib6txt3 <- renderText({
      t63()
    })
    output$pib6graf <- renderEcharts4r({
      pib6 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#f2c94e",
          name = "Valor(em Mil Reais)",
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
          name = "Valor(em Mil Reais)",
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
    })

    # 7 - Impostos, Líquidos de Subsídios, Sobre Produtos a Preços Correntes (em Mil Reais)----
    ## Mapa - Impostos, Líquidos de Subsídios, Sobre Produtos a Preços Correntes (em Mil Reais)----
    output$pib7txt1 <- renderText({
      t71()
    })

    output$pib7map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$pib7ri == "Pará") {
        df <- pib7 %>%
          filter(localidade != "Pará", ano == input$pib7ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- pib7 %>%
          filter(localidade != "Pará", ano == input$pib7ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$pib7ri)
      }

      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }

      pal <- colorBin(c("#ffff81", "#fad254", "#f2a92a", "#ae520b", "#6b0000"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Total (em Mil Reais):</b> %s ",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
          format(x$valor, big.mark = ".", decimal.mark = ",", digits = 4)
          )
        ) %>% lapply(htmltools::HTML)

      # Mapas com leafleft
      leaflet(x,
        options =
          leafletOptions(
            minZoom = 0,
            maxZoom = 15
          )
      ) %>%
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
          title = "Total (em Mil Reais)",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 0)
        )
    })
    ## Tabela - Impostos, Líquidos de Subsídios, Sobre Produtos a Preços Correntes (em Mil Reais)----
    output$pib7txt2 <- renderText({
      t72()
    })

    output$pib7tab <- renderReactable({
      if (input$pib7ri == "Pará") {
        x <- pib7 %>%
          filter(localidade != "Pará", ano == input$pib7ano) %>%
          select(ri, localidade, valor) %>%
          mutate(Percentual = (valor / sum(valor)) * 100)
      } else {
        x <- pib7 %>%
          filter(localidade != "Pará", ano == input$pib7ano) %>%
          select(ri, localidade, valor)
        x <- x %>%
          filter(ri == input$pib7ri) %>%
          mutate(Percentual = (valor / sum(valor)) * 100)
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
            name = "Valor(em Mil Reais)",
            format = colFormat(
              separators = T,
              locales = "pt-BR",
              digits = 0
            )
          ),
          Percentual = colDef(
            name = "Percentual(%)",
            format = colFormat(
              separators = T,
              locales = "pt-BR",
              digits = 2
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

    ## Gráfico de Barras Impostos, Líquidos de Subsídios, Sobre Produtos a Preços Correntes (em Mil Reais)----
    output$pib7txt3 <- renderText({
      t73()
    })
    output$pib7graf <- renderEcharts4r({
      pib7 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#f2c94e",
          name = "Valor(em Mil Reais)",
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
          name = "Valor(em Mil Reais)",
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
    })

    # 8 - Evolução do Produto Interno Bruto per Capita----
    ## Mapa - Evolução do Produto Interno Bruto per Capita----
    output$pib8txt1 <- renderText({
      t81()
    })

    output$pib8map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$pib8ri == "Pará") {
        df <- pib8 %>%
          filter(localidade != "Pará", ano == input$pib8ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- pib8 %>%
          filter(localidade != "Pará", ano == input$pib8ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$pib8ri)
      }

      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }

      pal <- colorBin(c("#ffff81", "#fad254", "#f2a92a", "#ae520b", "#6b0000"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Total (em Mil Reais):</b> %s ",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
          format(x$valor, big.mark = ".", decimal.mark = ",", digits = 4)
          )
        ) %>% lapply(htmltools::HTML)

      # Mapas com leafleft
      leaflet(x,
        options =
          leafletOptions(
            minZoom = 0,
            maxZoom = 15
          )
      ) %>%
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
          title = "Total (em Mil Reais)",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 0)
        )
    })

    ## Tabela - Evolução do Produto Interno Bruto per Capita----
    output$pib8txt2 <- renderText({
      t82()
    })

    output$pib8tab <- renderReactable({
      if (input$pib8ri == "Pará") {
        x <- pib8 %>%
          filter(localidade != "Pará", ano == input$pib8ano) %>%
          select(ri, localidade, valor) %>%
          mutate(Percentual = (valor / sum(valor)) * 100)
      } else {
        x <- pib8 %>%
          filter(localidade != "Pará", ano == input$pib8ano) %>%
          select(ri, localidade, valor)
        x <- x %>%
          filter(ri == input$pib8ri) %>%
          mutate(Percentual = (valor / sum(valor)) * 100)
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
            name = "Valor(em Mil Reais)",
            format = colFormat(
              separators = T,
              locales = "pt-BR",
              digits = 0
            )
          ),
          Percentual = colDef(
            name = "Percentual(%)",
            format = colFormat(
              separators = T,
              locales = "pt-BR",
              digits = 2
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
    ## Gráfico de Barras Evolução do Produto Interno Bruto per Capita----
    output$pib8txt3 <- renderText({
      t83()
    })
    output$pib8graf <- renderEcharts4r({
      pib8 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#f2c94e",
          name = "Valor(em Mil Reais)",
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
          name = "Valor(em Mil Reais)",
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
    })
    #DOWNLOADS----
    # 1 - Produto Interno Bruto a Preços Correntes (Mil Reais)----
    ## - Tabela - Produto Interno Bruto a Preços Correntes (em Mil Reais)----
    # Filtra os dados
    pib1_1 <- reactive({
      if (input$pib1ri == "Pará") {
        x <- pib1 %>%
          filter(localidade != "Pará", ano == input$pib1ano) %>%
          mutate(Percentual = (valor / sum(valor)) * 100)
      } else {
        x <- pib1 %>%
          filter(localidade != "Pará", 
                 ano == input$pib1ano,
                 ri == input$pib1ri
                 ) %>%
          mutate(Percentual = (valor / sum(valor)) * 100)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pib1_1(), {
      t12()
      downset_Server("pib1_1", pib1_1(), t12())
    })
    ## - Gráfico - Produto Interno Bruto a Preços Correntes (em Mil Reais)----
    # Filtra os dados
    pib1_2 <- reactive({
      pib1 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pib1_2(), {
      t13()
      downset_Server("pib1_2", pib1_2(), t13())
    })

    # 2 - Valor Adicionado Bruto a Preços Correntes Total (Mil Reais)----
    ## - Tabela - Valor Adicionado Bruto a Preços Correntes Total (em Mil Reais)----
    # Filtra os dados
    pib2_1 <- reactive({
      if (input$pib2ri == "Pará") {
        x <- pib2 %>%
          filter(localidade != "Pará", ano == input$pib2ano) %>%
          mutate(Percentual = (valor / sum(valor)) * 100)
      } else {
        x <- pib2 %>%
          filter(localidade != "Pará", 
                 ano == input$pib2ano,
                 ri == input$pib2ri
                 ) %>%
          mutate(Percentual = (valor / sum(valor)) * 100)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pib2_1(), {
      t22()
      downset_Server("pib2_1", pib2_1(), t22())
    })
    ## - Gráfico - Valor Adicionado Bruto a Preços Correntes Total (em Mil Reais)----
    # Filtra os dados
    pib2_2 <- reactive({
      pib2 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pib2_2(), {
      t23()
      downset_Server("pib2_2", pib2_2(), t23())
    })
    # 3 - Valor Adicionado Bruto a Preços Correntes da Agropecuária (Mil Reais)----
    ## - Tabela - da Valor Adicionado Bruto a Preços Correntes da Agropecuária----
    # Filtra os dados
    pib3_1 <- reactive({
      if (input$pib3ri == "Pará") {
        x <- pib3 %>%
          filter(localidade != "Pará", ano == input$pib3ano) %>%
          mutate(Percentual = (valor / sum(valor)) * 100)
      } else {
        x <- pib3 %>%
          filter(localidade != "Pará",
                 ano == input$pib3ano,
                 ri == input$pib3ri
                 ) %>%
          mutate(Percentual = (valor / sum(valor)) * 100)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pib3_1(), {
      t32()
      downset_Server("pib3_1", pib3_1(), t32())
    })
    ## - Gráfico - Valor Adicionado Bruto a Preços Correntes da Agropecuária----
    # Filtra os dados
    pib3_2 <- reactive({
      pib3 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pib3_2(), {
      t33()
      downset_Server("pib3_2", pib3_2(), t33())
    })

    # 4 - Valor adicionado bruto a preços correntes da indústria (Mil Reais)----
    ## - Tabela - Valor adicionado bruto a preços correntes da indústria----
    # Filtra os dados
    pib4_1 <- reactive({
      if (input$pib4ri == "Pará") {
        x <- pib4 %>%
          filter(localidade != "Pará", ano == input$pib4ano) %>%
          mutate(Percentual = (valor / sum(valor)) * 100)
      } else {
        x <- pib4 %>%
          filter(localidade != "Pará", 
                 ano == input$pib4ano,
                 ri == input$pib4ri
                 ) %>%
          mutate(Percentual = (valor / sum(valor)) * 100)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pib4_1(), {
      t42()
      downset_Server("pib4_1", pib4_1(), t42())
    })
    ## - Gráfico - Valor adicionado bruto a preços correntes da indústria----
    # Filtra os dados
    pib4_2 <- reactive({
      pib4 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pib4_2(), {
      t43()
      downset_Server("pib4_2", pib4_2(), t43())
    })

    # 5 - Valor Adicionado Bruto a Preços Correntes dos Serviços, Exclusive Administração, Saúde e Educação Públicas e Seguridade Social (Mil Reais)----
    ## - Tabela - Valor Adicionado Bruto a Preços Correntes dos Serviços, Exclusive Administração, Saúde e Educação Públicas e Seguridade Social----
    # Filtra os dados
    pib5_1 <- reactive({
      if (input$pib5ri == "Pará") {
        x <- pib5 %>%
          filter(localidade != "Pará", ano == input$pib5ano) %>%
          mutate(Percentual = (valor / sum(valor)) * 100)
      } else {
        x <- pib5 %>%
          filter(localidade != "Pará",
                 ano == input$pib5ano,
                 ri == input$pib5ri
                 ) %>%
          mutate(Percentual = (valor / sum(valor)) * 100)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pib5_1(), {
      t52()
      downset_Server("pib5_1", pib5_1(), t52())
    })
    ## - Gráfico - Valor Adicionado Bruto a Preços Correntes dos Serviços, Exclusive Administração, Saúde e Educação Públicas e Seguridade Social----
    # Filtra os dados
    pib5_2 <- reactive({
      pib5 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pib5_2(), {
      t53()
      downset_Server("pib5_2", pib5_2(), t53())
    })

    # 6 - Valor Adicionado Bruto a Preços Correntes da Administração, Saúde e Educação Públicas e Seguridade Social (Mil Reais)----
    ## - Tabela - Valor Adicionado Bruto a Preços Correntes da Administração, Saúde e Educação Públicas e Seguridade Social----
    # Filtra os dados
    pib6_1 <- reactive({
      if (input$pib6ri == "Pará") {
        x <- pib6 %>%
          filter(localidade != "Pará", ano == input$pib6ano) %>%
          mutate(Percentual = (valor / sum(valor)) * 100)
      } else {
        x <- pib6 %>%
          filter(localidade != "Pará",
                 ano == input$pib6ano,
                 ri == input$pib6ri
                 ) %>%
          mutate(Percentual = (valor / sum(valor)) * 100)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pib6_1(), {
     t62()
      downset_Server("pib6_1", pib6_1(), t62())
    })
    ## - Gráfico - Valor Adicionado Bruto a Preços Correntes da Administração, Saúde e Educação Públicas e Seguridade Social----
    # Filtra os dados
    pib6_2 <- reactive({
      pib6 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pib6_2(), {
      t63()
      downset_Server("pib6_2", pib6_2(), t63())
    })

    # 7 - Impostos, Líquidos de Subsídios, Sobre Produtos a Preços Correntes (Mil Reais)----
    ## - Tabela - Impostos, Líquidos de Subsídios, Sobre Produtos a Preços Correntes (em Mil Reais)----
    # Filtra os dados
    pib7_1 <- reactive({
      if (input$pib7ri == "Pará") {
        x <- pib7 %>%
          filter(localidade != "Pará", ano == input$pib7ano) %>%
          mutate(Percentual = (valor / sum(valor)) * 100)
      } else {
        x <- pib7 %>%
          filter(localidade != "Pará",
                 ano == input$pib7ano,
                 ri == input$pib7ri
                 ) %>%
          mutate(Percentual = (valor / sum(valor)) * 100)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pib7_1(), {
      t72()
      downset_Server("pib7_1", pib7_1(), t72())
    })
    ## - Gráfico - Impostos, Líquidos de Subsídios, Sobre Produtos a Preços Correntes----
    # Filtra os dados
    pib7_2 <- reactive({
      pib7 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pib7_2(), {
      t73()
      downset_Server("pib7_2", pib7_2(), t73())
    })

    # 8 - Evolução do Produto Interno Bruto per Capita----
    ## - Tabela - Evolução do Produto Interno Bruto per Capita----
    # Filtra os dados
    pib8_1 <- reactive({
      if (input$pib8ri == "Pará") {
        x <- pib8 %>%
          filter(localidade != "Pará", ano == input$pib8ano) %>%
          mutate(Percentual = (valor / sum(valor)) * 100)
      } else {
        x <- pib8 %>%
          filter(localidade != "Pará",
                 ano == input$pib8ano,
                 ri == input$pib8ri
                 ) %>%
          mutate(Percentual = (valor / sum(valor)) * 100)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pib8_1(), {
      t82()
      downset_Server("pib8_1", pib8_1(), t82())
    })
    ## - Gráfico - Evolução do Produto Interno Bruto per Capita----
    # Filtra os dados
    pib8_2 <- reactive({
      pib8 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pib8_2(), {
      t83()
      downset_Server("pib8_2", pib8_2(), t83())
    })
  })
}

# Play do Módulo
# ui <- dashboardPage(
#   header = dashboardHeader(),
#   sidebar = dashboardSidebar(),
#   body = dashboardBody(fluidPage(economia_pib_pa_ui("economia_pib_pa")))
# )
# 
# 
# server <- function(input, output) {
#   economia_pib_pa_Server("economia_pib_pa")
# }
# 
# shinyApp(ui, server)
