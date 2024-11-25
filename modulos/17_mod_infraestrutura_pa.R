# Funções de módulo de Infraestrutura - Estadual
# Função de UI
infraestrutura_pa_ui <- function(id) {
  fluidPage(tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "estilo.css")
  ),
  div(class = "navbar_infraestrutura",
      navbarPage(
        tags$b("Infraestrutura - Pará"),
        navbarMenu(
          tags$b("Escolha um Indicador"),
          # 1 - Total da Frota de Veículos Subdivididos em Licenciados e Não Licenciados----
          tabPanel(
            "Total da Frota de Veículos Subdivididos em Licenciados e Não Licenciados",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Total da Frota de Veículos Subdivididos em Licenciados e Não Licenciados"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "inf1ano"),
                  label = "Ano",
                  choices = sort(unique(inf1[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "inf1ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(inf1[["ri"]]),
                  width = "200px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "inf1cat"),
                  label = "Tipo de Frota/Total",
                  choices = unique(inf1[["categoria"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Total da Frota de Veículos Subdivididos em Licenciados e Não Licenciados----
              box(
                title = textOutput(NS(id, "inf1txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "inf1map"), height = "600px"),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(
                  tags$h6(tags$b("Fonte:"), "DETRAN"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                  tags$h6(
                    tags$b("Nota:"),
                    "Frota = Total de Veículos Licenciados + Não licenciados "
                  )
                )
              ),
              ## Tabela - Total da Frota de Veículos Subdivididos em Licenciados e Não Licenciados----
              box(
                title = textOutput(NS(id, "inf1txt2")),
                collapsible = T,
                collapsed = F,
                width = 12,
                headerBorder = T,
                solidHeader = F,
                withSpinner(
                  reactableOutput(NS(id, "inf1tab"), height = "400px"),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "DETRAN"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                  tags$h6(
                    tags$b("Nota:"),
                    "Frota = Total de Veículos Licenciados + Não licenciados "
                  )
                ),
                downset_ui(NS(id, "inf1_1")))
              )
            ),
            fluidRow(
              ## Gráfico Total da Frota de Veículos Subdivididos em Licenciados e Não Licenciados----
              box(
                title = textOutput(NS(id, "inf1txt3")),
                collapsible = T,
                collapsed = F,
                width = 12,
                headerBorder = T,
                solidHeader = F,
                withSpinner(
                  echarts4rOutput(NS(id, "inf1graf")),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "DETRAN"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                  tags$h6(
                    tags$b("Nota:"),
                    "Frota = Total de Veículos Licenciados + Não licenciados"
                  )
                ),
                downset_ui(NS(id, "inf1_2")))
              )
            )
          ),
          # 2 - Consumidores de Energia Elétrica Total----
          tabPanel(
            "Consumidores de Energia Elétrica Total",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Consumidores de Energia Elétrica Total"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "inf2ano"),
                  label = "Ano",
                  choices = sort(unique(inf2[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "inf2ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(inf2[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Consumidores de Energia Elétrica Total----
              box(
                title = textOutput(NS(id, "inf2txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "inf2map"), height = "600px"),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(
                  tags$h6(tags$b("Fonte:"), " EQUATORIAL ENERGIA"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                )
              ),
              ## Tabela - Consumidores de Energia Elétrica Total----
              box(
                title = textOutput(NS(id, "inf2txt2")),
                collapsible = T,
                collapsed = F,
                width = 12,
                headerBorder = T,
                solidHeader = F,
                withSpinner(
                  reactableOutput(NS(id, "inf2tab"), height = "400px"),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), " EQUATORIAL ENERGIA"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "inf2_1")))
              )
            ),
            fluidRow(
              ## Gráfico Consumidores de Energia Elétrica Total----
              box(
                title = textOutput(NS(id, "inf2txt3")),
                collapsible = T,
                collapsed = F,
                width = 12,
                headerBorder = T,
                solidHeader = F,
                withSpinner(
                  echarts4rOutput(NS(id, "inf2graf")),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), " EQUATORIAL ENERGIA"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "inf2_2")))
              )
            )
          ),
          # 3 - Consumidores de Energia Elétrica por Tipo----
          tabPanel(
            "Consumidores de Energia Elétrica por Tipo",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Consumidores de Energia Elétrica por Tipo"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "inf3ano"),
                  label = "Ano",
                  choices = sort(unique(inf3[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "inf3ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(inf3[["ri"]]),
                  width = "200px"
                )
              ),
              tags$div(
                class = "seletor2",
                # Categoria
                pickerInput(
                  inputId = NS(id, "inf3cat"),
                  label = "Tipo de Consumidores",
                  choices = unique(inf3[["categoria"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Consumidores de Energia Elétrica por Tipo----
              box(
                title = textOutput(NS(id, "inf3txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "inf3map"), height = "600px"),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(
                  tags$h6(tags$b("Fonte:"), " EQUATORIAL ENERGIA"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                )
              ),
              ## Tabela - Consumidores de Energia Elétrica por Tipo----
              box(
                title = textOutput(NS(id, "inf3txt2")),
                collapsible = T,
                collapsed = F,
                width = 12,
                headerBorder = T,
                solidHeader = F,
                withSpinner(
                  reactableOutput(NS(id, "inf3tab"), height = "400px"),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), " EQUATORIAL ENERGIA"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "inf3_1")))
              )
            ),
            fluidRow(
              ## Gráfico Consumidores de Energia Elétrica por Tipo----
              box(
                title = textOutput(NS(id, "inf3txt3")),
                collapsible = T,
                collapsed = F,
                width = 12,
                headerBorder = T,
                solidHeader = F,
                withSpinner(
                  echarts4rOutput(NS(id, "inf3graf")),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), " EQUATORIAL ENERGIA"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "inf3_2")))
              )
            )
          ),
          # 4 - Consumo de Energia Elétrica Total (kWH)----
          tabPanel(
            "Consumo de Energia Elétrica Total (kWH)",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Consumo de Energia Elétrica Total (kWH)"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "inf4ano"),
                  label = "Ano",
                  choices = sort(unique(inf4[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "inf4ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(inf4[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Consumo de Energia Elétrica Total (kWH)----
              box(
                title = textOutput(NS(id, "inf4txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "inf4map"), height = "600px"),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(
                  tags$h6(tags$b("Fonte:"), " EQUATORIAL ENERGIA"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                )
              ),
              ## Tabela - Consumo de Energia Elétrica Total (kWH)----
              box(
                title = textOutput(NS(id, "inf4txt2")),
                collapsible = T,
                collapsed = F,
                width = 12,
                headerBorder = T,
                solidHeader = F,
                withSpinner(
                  reactableOutput(NS(id, "inf4tab"), height = "400px"),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), " EQUATORIAL ENERGIA"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "inf4_1")))
              )
            ),
            fluidRow(
              ## Gráfico Consumo de Energia Elétrica Total (kWH)----
              box(
                title = textOutput(NS(id, "inf4txt3")),
                collapsible = T,
                collapsed = F,
                width = 12,
                headerBorder = T,
                solidHeader = F,
                withSpinner(
                  echarts4rOutput(NS(id, "inf4graf")),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), " EQUATORIAL ENERGIA"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "inf4_2")))
              )
            )
          ),
          # 5 - Consumo de Energia Elétrica por Tipo----
          tabPanel(
            "Consumo de Energia Elétrica por Tipo",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Consumo de Energia Elétrica por Tipo"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "inf5ano"),
                  label = "Ano",
                  choices = sort(unique(inf5[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "inf5ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(inf5[["ri"]]),
                  width = "200px"
                )
              ),
              tags$div(
                class = "seletor2",
                # Categoria
                pickerInput(
                  inputId = NS(id, "inf5cat"),
                  label = "Tipo de Consumo",
                  choices = sort(unique(inf5[["categoria"]]), decreasing = T),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Consumo de Energia Elétrica por Tipo----
              box(
                title = textOutput(NS(id, "inf5txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "inf5map"), height = "600px"),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(
                  tags$h6(tags$b("Fonte:"), " EQUATORIAL ENERGIA"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                )
              ),
              ## Tabela - Consumo de Energia Elétrica por Tipo----
              box(
                title = textOutput(NS(id, "inf5txt2")),
                collapsible = T,
                collapsed = F,
                width = 12,
                headerBorder = T,
                solidHeader = F,
                withSpinner(
                  reactableOutput(NS(id, "inf5tab"), height = "400px"),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), " EQUATORIAL ENERGIA"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "inf5_1")))
              )
            ),
            fluidRow(
              ## Gráfico Consumo de Energia Elétrica por Tipo----
              box(
                title = textOutput(NS(id, "inf5txt3")),
                collapsible = T,
                collapsed = F,
                width = 12,
                headerBorder = T,
                solidHeader = F,
                withSpinner(
                  echarts4rOutput(NS(id, "inf5graf")),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), " EQUATORIAL ENERGIA"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "inf5_2")))
              )
            )
          ),
          # 6 - Total de  Carregamento nos Portos----
          tabPanel(
            "Total de  Carregamento nos Portos",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Total de  Carregamento nos Portos"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "inf6ano"),
                  label = "Ano",
                  choices = sort(unique(inf6[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "inf6ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(inf6[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Total de  Carregamento nos Portos----
              box(
                title = textOutput(NS(id, "inf6txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "inf6map"), height = "500px"),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(
                  tags$h6(tags$b("Fonte:"), "CDP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                  tags$h6(tags$b("Nota:"), "Carregamento (Toneladas) = Mercadorias")
                )
              ),
              ## Tabela - Total de  Carregamento nos Portos----
              box(
                title = textOutput(NS(id, "inf6txt2")),
                collapsible = T,
                collapsed = F,
                width = 12,
                headerBorder = T,
                solidHeader = F,
                withSpinner(
                  reactableOutput(NS(id, "inf6tab")),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "CDP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                  tags$h6(tags$b("Nota:"), "Carregamento (Toneladas) = Mercadorias")
                ),
                downset_ui(NS(id, "inf6_1")))
              )
            ),
            fluidRow(
              ## Gráfico Total de  Carregamento nos Portos----
              box(
                title = textOutput(NS(id, "inf6txt3")),
                collapsible = T,
                collapsed = F,
                width = 12,
                headerBorder = T,
                solidHeader = F,
                withSpinner(
                  echarts4rOutput(NS(id, "inf6graf")),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "CDP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                  tags$h6(tags$b("Nota:"), "Carregamento (Toneladas) = Mercadorias")
                ),
                downset_ui(NS(id, "inf6_2")))
              )
            )
          ),
          # 7 - Total de  Descarregamento nos Portos----
          tabPanel(
            "Total de  Descarregamento nos Portos",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Total de  Descarregamento nos Portos"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "inf7ano"),
                  label = "Ano",
                  choices = sort(unique(inf7[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "inf7ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(inf7[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Total de  Descarregamento nos Portos----
              box(
                title = textOutput(NS(id, "inf7txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "inf7map"), height = "500px"),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(
                  tags$h6(tags$b("Fonte:"), "CDP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                  tags$h6(tags$b("Nota:"), "Descarregamento (Toneladas) = Mercadorias")
                )
              ),
              ## Tabela - Total de  Descarregamento nos Portos----
              box(
                title = textOutput(NS(id, "inf7txt2")),
                collapsible = T,
                collapsed = F,
                width = 12,
                headerBorder = T,
                solidHeader = F,
                withSpinner(
                  reactableOutput(NS(id, "inf7tab")),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "CDP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                  tags$h6(tags$b("Nota:"),
                          "Descarregamento (Toneladas) = Mercadorias")
                ),
                downset_ui(NS(id, "inf7_1")))
              )
            ),
            fluidRow(
              ## Gráfico Total de  Descarregamento nos Portos----
              box(
                title = textOutput(NS(id, "inf7txt3")),
                collapsible = T,
                collapsed = F,
                width = 12,
                headerBorder = T,
                solidHeader = F,
                withSpinner(
                  echarts4rOutput(NS(id, "inf7graf")),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "CDP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                  tags$h6(tags$b("Nota:"), "Descarregamento (Toneladas) = Mercadorias")
                ),
                downset_ui(NS(id, "inf7_2")))
              )
            )
          ),
          # 8 - Total de  Movimentação nos Portos----
          tabPanel(
            "Total de  Movimentação nos Portos",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Total de  Movimentação nos Portos"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "inf8ano"),
                  label = "Ano",
                  choices = sort(unique(inf8[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "inf8ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(inf8[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Total de  Movimentação nos Portos----
              box(
                title = textOutput(NS(id, "inf8txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "inf8map"), height = "500px"),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(
                  tags$h6(tags$b("Fonte:"), "CDP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                  tags$h6(tags$b("Nota:"), "Movimentação de Mercadorias (Toneladas)")
                )
              ),
              ## Tabela - Total de  Movimentação nos Portos----
              box(
                title = textOutput(NS(id, "inf8txt2")),
                collapsible = T,
                collapsed = F,
                width = 12,
                headerBorder = T,
                solidHeader = F,
                withSpinner(
                  reactableOutput(NS(id, "inf8tab")),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "CDP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                  tags$h6(tags$b("Nota:"), "Movimentação de Mercadorias (Toneladas)")
                ),
                downset_ui(NS(id, "inf8_1")))
              )
            ),
            fluidRow(
              ## Gráfico Total de  Movimentação nos Portos----
              box(
                title = textOutput(NS(id, "inf8txt3")),
                collapsible = T,
                collapsed = F,
                width = 12,
                headerBorder = T,
                solidHeader = F,
                withSpinner(
                  echarts4rOutput(NS(id, "inf8graf")),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "CDP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                  tags$h6(tags$b("Nota:"), "Movimentação de Mercadorias (Toneladas)")
                ),
                downset_ui(NS(id, "inf8_2")))
              )
            )
          ),
          # 9 - Total de  Embarcações----
          tabPanel(
            "Total de  Embarcações",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Total de  Embarcações"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "inf9ano"),
                  label = "Ano",
                  choices = sort(unique(inf9[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "inf9ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(inf9[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Total de  Embarcações----
              box(
                title = textOutput(NS(id, "inf9txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "inf9map"), height = "500px"),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(tags$h6(tags$b("Fonte:"), "CDP"),
                              tags$h6(tags$b("Elaboração:"), "FAPESPA"))
              ),
              ## Tabela - Total de  Embarcações----
              box(
                title = textOutput(NS(id, "inf9txt2")),
                collapsible = T,
                collapsed = F,
                width = 12,
                headerBorder = T,
                solidHeader = F,
                withSpinner(
                  reactableOutput(NS(id, "inf9tab")),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "CDP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "inf9_1")))
              )
            ),
            fluidRow(
              ## Gráfico Total de  Embarcações----
              box(
                title = textOutput(NS(id, "inf9txt3")),
                collapsible = T,
                collapsed = F,
                width = 12,
                headerBorder = T,
                solidHeader = F,
                withSpinner(
                  echarts4rOutput(NS(id, "inf9graf")),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "CDP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "inf9_2")))
              )
            )
          ),
          # 10 - Total das  Receitas Operacionais----
          tabPanel(
            "Total das  Receitas Operacionais",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Total das  Receitas Operacionais"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "inf10ano"),
                  label = "Ano",
                  choices = sort(unique(inf10[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "inf10ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(inf10[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Total das  Receitas Operacionais----
              box(
                title = textOutput(NS(id, "inf10txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "inf10map"), height = "500px"),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(tags$h6(tags$b("Fonte:"), "CDP"),
                              tags$h6(tags$b("Elaboração:"), "FAPESPA"))
              ),
              ## Tabela - Total das  Receitas Operacionais----
              box(
                title = textOutput(NS(id, "inf10txt2")),
                collapsible = T,
                collapsed = F,
                width = 12,
                headerBorder = T,
                solidHeader = F,
                withSpinner(
                  reactableOutput(NS(id, "inf10tab")),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "CDP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "inf10_1")))
              )
            ),
            fluidRow(
              ## Gráfico Total das  Receitas Operacionais----
              box(
                title = textOutput(NS(id, "inf10txt3")),
                collapsible = T,
                collapsed = F,
                width = 12,
                headerBorder = T,
                solidHeader = F,
                withSpinner(
                  echarts4rOutput(NS(id, "inf10graf")),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "CDP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "inf10_2")))
              )
            )
          ),
          # 11 - Total de Pouso mais decolagem de Aeronaves----
          tabPanel(
            "Total de Pouso mais decolagem de Aeronaves",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Total de Pouso mais decolagem de Aeronaves"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "inf11ano"),
                  label = "Ano",
                  choices = sort(unique(inf11[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "inf11ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(inf11[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Total de Pouso mais decolagem de Aeronaves----
              box(
                title = textOutput(NS(id, "inf11txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "inf11map"), height = "500px"),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(tags$h6(tags$b("Fonte:"), "INFRAERO"),
                              tags$h6(tags$b("Elaboração:"), "FAPESPA"))
              ),
              ## Tabela - Total de Pouso mais decolagem de Aeronaves----
              box(
                title = textOutput(NS(id, "inf11txt2")),
                collapsible = T,
                collapsed = F,
                width = 12,
                headerBorder = T,
                solidHeader = F,
                withSpinner(
                  reactableOutput(NS(id, "inf11tab")),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INFRAERO"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "inf11_1")))
              )
            ),
            fluidRow(
              ## Gráfico Total de Pouso mais decolagem de Aeronaves----
              box(
                title = textOutput(NS(id, "inf11txt3")),
                collapsible = T,
                collapsed = F,
                width = 12,
                headerBorder = T,
                solidHeader = F,
                withSpinner(
                  echarts4rOutput(NS(id, "inf11graf")),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INFRAERO"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "inf11_2")))
              )
            )
          ),
          # 12 - Total de Embarque mais desembarque de passageiros----
          tabPanel(
            "Total de Embarque mais desembarque de passageiros",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Total de Embarque mais desembarque de passageiros"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "inf12ano"),
                  label = "Ano",
                  choices = sort(unique(inf12[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "inf12ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(inf12[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Total de Embarque mais desembarque de passageiros----
              box(
                title = textOutput(NS(id, "inf12txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "inf12map"), height = "500px"),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(tags$h6(tags$b("Fonte:"), "INFRAERO"),
                              tags$h6(tags$b("Elaboração:"), "FAPESPA"))
              ),
              ## Tabela - Total de Embarque mais desembarque de passageiros----
              box(
                title = textOutput(NS(id, "inf12txt2")),
                collapsible = T,
                collapsed = F,
                width = 12,
                headerBorder = T,
                solidHeader = F,
                withSpinner(
                  reactableOutput(NS(id, "inf12tab")),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INFRAERO"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "inf12_1")))
              )
            ),
            fluidRow(
              ## Gráfico - Total de Embarque mais desembarque de passageiros----
              box(
                title = textOutput(NS(id, "inf12txt3")),
                collapsible = T,
                collapsed = F,
                width = 12,
                headerBorder = T,
                solidHeader = F,
                withSpinner(
                  echarts4rOutput(NS(id, "inf12graf")),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INFRAERO"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "inf12_2")))
              )
            )
          ),
          # 13 - Abastecimento de Água Segundo Consumidores e Volume Consumido----
          tabPanel(
            "Abastecimento de Água Segundo Consumidores e Volume Consumido",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Abastecimento de Água Segundo Consumidores e Volume Consumido"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "inf13ano"),
                  label = "Ano",
                  choices = sort(unique(inf13[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "inf13ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(inf13[["ri"]]),
                  width = "200px"
                )
              )
            ),
            
            ## Mapas - Abastecimento de Água Segundo Consumidores e Volume Consumido----
            fluidRow(
              ## Mapas - Abastecimento de Água Segundo Consumidores----
              box(
                title = textOutput(NS(id, "inf13txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "inf13map1"), height = "600px"),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(tags$h6(tags$b("Fonte:"), "SNIS"),
                              tags$h6(tags$b("Elaboração:"), "FAPESPA"))
              ),
              ## Mapas - Volume Consumido----
              box(
                title = textOutput(NS(id, "inf13txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "inf13map2"), height = "600px"),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(tags$h6(tags$b("Fonte:"), "SNIS"),
                              tags$h6(tags$b("Elaboração:"), "FAPESPA"))
              ),
              ## Tabelas - Abastecimento de Água Segundo Consumidores e Volume Consumido----
              ## Tabela - População total atendida com abastecimento de água (Hab.)----
              box(
                title = textOutput(NS(id, "inf13txt3")),
                collapsible = T,
                collapsed = F,
                width = 12,
                headerBorder = T,
                solidHeader = F,
                withSpinner(
                  reactableOutput(NS(id, "inf13tab1"), height = "400px"),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "SNIS"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "inf13_1")))
              ),
              ## Tabela - Volume consumido de água (1.000 m³ /ano)----
              box(
                title = textOutput(NS(id, "inf13txt4")),
                collapsible = T,
                collapsed = F,
                width = 12,
                headerBorder = T,
                solidHeader = F,
                withSpinner(
                  reactableOutput(NS(id, "inf13tab2"), height = "400px"),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "SNIS"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "inf13_2")))
              )
            ),
            fluidRow(
              ## Gráfico - Abastecimento de Água Segundo Consumidores e Volume Consumido----
              box(
                title = textOutput(NS(id, "inf13txt5")),
                collapsible = T,
                collapsed = F,
                width = 12,
                headerBorder = T,
                solidHeader = F,
                withSpinner(
                  echarts4rOutput(NS(id, "inf13graf")),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "SNIS"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "inf13_3")))
              )
            )
          )
        )
      )))
}

# Função do modulo servidor
infraestrutura_pa_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    #TÍTULOS----
    # 1 - Total da Frota de Veículos Subdivididos em Licenciados e Não Licenciados----
    ## Mapa - Total da Frota de Veículos----
    t11 <- reactive({
      if (input$inf1ri == "Pará") {
        paste0("Frota de Veículos ",
               input$inf1cat,", ",
               input$inf1ri," - ",
               input$inf1ano) 
      }else{
        paste0("Frota de Veículos ",
               input$inf1cat,", Região de Integração ",
               input$inf1ri," - ",
               input$inf1ano) 
      }
    })
    
    ## Tabela - Total da Frota de Veículos Subdivididos em Licenciados e Não Licenciados----
    t12 <- reactive({
      if (input$inf1ri == "Pará") {
        paste0(
          "Total da Frota de Veículos Subdivididos em Licenciados e Não Licenciados por Município, ",
          input$inf1ri,
          " - ",
          input$inf1ano
        )
      } else{
        paste0(
          "Total da Frota de Veículos Subdivididos em Licenciados e Não Licenciados por Município, Região de Integração ",
          input$inf1ri,
          " - ",
          input$inf1ano
        )
      }
    })
    
    ## Gráfico - Total da Frota de Veículos Subdivididos em Licenciados e Não Licenciados----
    t13 <- reactive({
      paste0(
        "Total da Frota de Veículos Subdivididos em Licenciados e Não Licenciados, Pará - ",
        min(inf1$ano),
        " a ",
        max(inf1$ano)
      )
    })
    
    # 2 - Consumidores de Energia Elétrica Total----
    ## Mapa - Consumidores de Energia Elétrica Total----
    t21 <- reactive({
      if (input$inf2ri == "Pará") {
        paste0("Consumidores de Energia Elétrica Total ",
               input$inf2cat,", ",
               input$inf2ri," - ",
               input$inf2ano) 
      }else{
        paste0("Consumidores de Energia Elétrica Total ",
               input$inf2cat,", Região de Integração ",
               input$inf2ri," - ",
               input$inf2ano) 
      }
    })
    
    ## Tabela - Consumidores de Energia Elétrica Total----
    t22 <- reactive({
      if (input$inf2ri == "Pará") {
        paste0("Consumidores de Energia Elétrica Total por Município, ",
               input$inf2ri," - ",
               input$inf2ano) 
      }else{
        paste0("Consumidores de Energia Elétrica Total por Município, Região de Integração ",
               input$inf2ri," - ",
               input$inf2ano) 
      }
    })

    ## Gráfico - Consumidores de Energia Elétrica Total----
    t23 <- reactive({
      paste0(
        "Consumidores de Energia Elétrica Total, Pará - ",
        min(inf2$ano),
        " a ",
        max(inf2$ano)
      )
    })

    # 3 - Consumidores de Energia Elétrica por Tipo----
    ## Mapa - Consumidores de Energia Elétrica por Tipo----
    t31 <- reactive({
      if (input$inf3ri == "Pará") {
        paste0("Consumidores de Energia Elétrica por Tipo ",
               input$inf3cat,", ",
               input$inf3ri," - ",
               input$inf3ano) 
      }else{
        paste0("Consumidores de Energia Elétrica por Tipo ",
               input$inf3cat,", Região de Integração ",
               input$inf3ri," - ",
               input$inf3ano) 
      }
    })
    
    ## Tabela - Consumidores de Energia Elétrica por Tipo----
    t32 <- reactive({
      if (input$inf3ri == "Pará") {
        paste0("Consumidores de Energia Elétrica por Tipo ",
               input$inf3cat," por Município, ",
               input$inf3ri," - ",
               input$inf3ano) 
      }else{
        paste0("Consumidores de Energia Elétrica por Tipo ",
               input$inf3cat," por Município, Região de Integração ",
               input$inf3ri," - ",
               input$inf3ano) 
      }
    })
    
    ## Gráfico - Consumidores de Energia Elétrica por Tipo----
    t33 <- reactive({
      paste0(
        "Consumidores de Energia Elétrica por Tipo, Pará - ",
        min(inf3$ano),
        " a ",
        max(inf3$ano)
      )
    })

    # 4 - Consumo de Energia Elétrica Total (kWH)----
    ## Mapa - Consumo de Energia Elétrica Total (kWH)----
    t41 <- reactive({
      if (input$inf4ri == "Pará") {
        paste0("Consumo de Energia Elétrica Total (kWH) ",
               input$inf4cat,", ",
               input$inf4ri," - ",
               input$inf4ano) 
      }else{
        paste0("Consumo de Energia Elétrica Total (kWH) ",
               input$inf4cat,", Região de Integração ",
               input$inf4ri," - ",
               input$inf4ano) 
      }
    })
    
    ## Tabela - Consumo de Energia Elétrica Total (kWH)----
    t42 <- reactive({
      if (input$inf4ri == "Pará") {
        paste0("Consumo de Energia Elétrica Total (kWH) ",
               input$inf4cat," por Município, ",
               input$inf4ri," - ",
               input$inf4ano) 
      }else{
        paste0("Consumo de Energia Elétrica Total (kWH) ",
               input$inf4cat," por Município, Região de Integração ",
               input$inf4ri," - ",
               input$inf4ano) 
      }
    })
    
    ## Gráfico - Consumo de Energia Elétrica Total (kWH)----
    t43 <- reactive({
      paste0(
        "Consumo de Energia Elétrica Total (kWH), Pará - ",
        min(inf4$ano),
        " a ",
        max(inf4$ano)
      )
    })

    # 5 - Consumo de Energia Elétrica por Tipo----
    ## Mapa - Consumo de Energia Elétrica por Tipo----
    t51 <- reactive({
      if (input$inf5ri == "Pará") {
        paste0("Consumo (kWH) de Energia Elétrica do tipo ",
               input$inf5cat,", ",
               input$inf5ri," - ",
               input$inf5ano) 
      }else{
        paste0("Consumo (kWH) de Energia Elétrica do tipo ",
               input$inf5cat,", Região de Integração ",
               input$inf5ri," - ",
               input$inf5ano) 
      }
    })
    
    ## Tabela - Consumo de Energia Elétrica por Tipo----
    t52 <- reactive({
      if (input$inf5ri == "Pará") {
        paste0("Consumo (kWH) de Energia Elétrica por Tipo, ",
               input$inf5ri," - ",
               input$inf5ano) 
      }else{
        paste0("Consumo (kWH) de Energia Elétrica por Tipo, Região de Integração ",
               input$inf5ri," - ",
               input$inf5ano) 
      }
    })
    
    ## Gráfico - Consumo de Energia Elétrica por Tipo----
    t53 <- reactive({
      paste0(
        "Consumo (kWH) de Energia Elétrica Total por Tipo, Pará - ",
        min(inf5$ano),
        " a ",
        max(inf5$ano)
      )
    })

    # 6 - Total de  Carregamento nos Portos----
    ## Mapa - Total de  Carregamento nos Portos----
    t61 <- reactive({
      if (input$inf6ri == "Pará") {
        paste0("Total de Carregamento (Mercadorias) nos Portos, Pará - ",
               input$inf6ano) 
      }else{
        paste0("Total de Carregamento (Mercadorias) nos Portos, Região de Integração ",
               input$inf6ri," - ",
               input$inf6ano) 
      }
    })
    
    ## Tabela - Total de  Carregamento nos Portos----
    t62 <- reactive({
      paste0("Total de Carregamento (Mercadorias) por Porto, Pará - ",
             input$inf6ano)
    })

    ## Gráfico - Total de  Carregamento nos Portos----
    t63 <- reactive({
      paste0(
        "Total de Carregamento (Mercadorias) nos Portos, Pará - ",
        min(inf6$ano),
        " a ",
        max(inf6$ano)
      )
    })

    # 7 - Total de  Descarregamento nos Portos----
    ## Mapa - Total de  Descarregamento nos Portos----
    t71 <- reactive({
      if (input$inf7ri == "Pará") {
        paste0("Total de Descarregamento (Mercadorias) nos Portos, Pará - ",
               input$inf7ano) 
      }else{
        paste0("Total de Descarregamento (Mercadorias) nos Portos, Região de Integração ",
               input$inf7ri," - ",
               input$inf7ano) 
      }
    })
    
    ## Tabela - Total de Descarregamento nos Portos----
    t72 <- reactive({
      paste0("Total de Descarregamento (Mercadorias) por Porto, Pará - ",
             input$inf7ano)
    })
    
    ## Gráfico - Total de  Descarregamento nos Portos----
    t73 <- reactive({
      paste0(
        "Gráfico - Total de Descarregamento (Mercadorias) nos Portos, Pará - ",
        min(inf7$ano),
        " a ",
        max(inf7$ano)
      )
    })
    # 8 - Total de  Movimentação nos Portos----
    ## Mapa - Total de  Movimentação nos Portos----
    t81 <- reactive({
      if (input$inf8ri == "Pará") {
        paste0("Total de Movimentação (Mercadorias) nos Portos, Pará - ",
               input$inf8ano) 
      }else{
        paste0("Total de Movimentação (Mercadorias) nos Portos, Região de Integração ",
               input$inf8ri," - ",
               input$inf8ano) 
      }
    })
    
    ## Tabelas - Total de  Movimentação nos Portos----
    t82 <- reactive({
      paste0("Total de Movimentação (Mercadorias) por Porto, Pará - ",
             input$inf8ano)
    })
    
    ## Gráfico - Total de  Movimentação nos Portos----
    t83 <- reactive({
      paste0(
        "Total de Movimentação (Mercadorias) nos Portos, Pará - ",
        min(inf8$ano),
        " a ",
        max(inf8$ano)
      )
    })
    # 9 - Total de  Embarcações----
    ## Mapa - Total de  Embarcações----
    t91 <- reactive({
      if (input$inf9ri == "Pará") {
        paste0("Total de Embarcações, Pará - ",
               input$inf9ano) 
      }else{
        paste0("Total de Embarcações, Região de Integração ",
               input$inf9ri," - ",
               input$inf9ano) 
      }
    })
    
    ## Tabela - Total de  Embarcações----
    t92 <- reactive({
      paste0("Total de Embarcaçõesnos por porto, Pará - ",
             input$inf9ano)
    })
    
    ## Gráfico - Total de  Embarcações----
    t93 <- reactive({
      paste0(
        " Total de Embarcações nos portos, Pará - ",
        min(inf9$ano),
        " a ",
        max(inf9$ano)
      )
    })

    # 10 - Total das Receitas Operacionais----
    ## Mapa - Total das  Receitas Operacionais----
    t101 <- reactive({
      if (input$inf10ri == "Pará") {
        paste0("Total das Receitas Operacionais, Pará - ",
               input$inf10ano) 
      }else{
        paste0("Total das Receitas Operacionais, Região de Integração ",
               input$inf10ri," - ",
               input$inf10ano) 
      }
    })
    ## Tabela - Total das  Receitas Operacionais----
    t102 <- reactive({
      paste0("Total das Receitas Operacionais por Porto, Pará - ",
             input$inf10ano)
    })
    
    ## Gráfico - Total das  Receitas Operacionais----
    t103 <- reactive({
      paste0("Total das Receitas Operacionais nos Portos, Pará - ",
        min(inf10$ano),
        " a ",
        max(inf10$ano)
      )
    })

    # 11 - Total de Pouso mais decolagem de Aeronaves ----
    ## Mapa - Total de Pouso mais decolagem de Aeronaves ----
    t111 <- reactive({
      if (input$inf11ri == "Pará") {
        paste0("Total de Pouso mais decolagem de Aeronaves, Pará - ",
               input$inf11ano) 
      }else{
        paste0("Total de Pouso mais decolagem de Aeronaves, Região de Integração ",
               input$inf11ri," - ",
               input$inf11ano) 
      }
    })
    
    ## Tabela - Total de Pouso mais decolagem de Aeronaves ----
    t112 <- reactive({
      paste0(
        "Tabela - Total de Pouso mais decolagem de Aeronaves por Aeroporto, Pará - ",
        input$inf11ano
      )
    })
    
    ## Gráfico - Total de Pouso mais decolagem de Aeronaves ----
    t113 <- reactive({
      paste0("Total de Pouso mais decolagem de Aeronaves nos Aeroporto, Pará - ",
        min(inf11$ano),
        " a ",
        max(inf11$ano)
      )
    })

    # 12 - Total de Embarque mais desembarque de passageiros----
    ## Mapa - Total de Embarque mais desembarque de passageiros ----
    t121 <- reactive({
      if (input$inf12ri == "Pará") {
        paste0("Total de Embarque mais desembarque de passageiros, Pará - ",
               input$inf12ano) 
      }else{
        paste0("Total de Embarque mais desembarque de passageiros, Região de Integração ",
               input$inf12ri," - ",
               input$inf12ano) 
      }
    })
    
    ## Tabela - Total de Embarque mais desembarque de passageiros ----
    t122 <- reactive({
      paste0(
        "Total de Embarque mais desembarque de passageiros por Aeroporto, Pará - ",
        input$inf12ano
      )
    })
    

    ## Gráfico - Total de Embarque mais desembarque de passageiros ----
    t123 <- reactive({
      paste0("Total de Embarque mais desembarque de passageiros nos Aeroporto, Pará - ",
        min(inf12$ano),
        " a ",
        max(inf12$ano)
      )
    })
    # 13 - Abastecimento de Água Segundo Consumidores e Volume Consumido----
    ## Mapa - População total atendida com abastecimento de água (Hab.)----
    t131 <- reactive({
      if (input$inf13ri == "Pará") {
        paste0("População total atendida com abastecimento de água (Hab.), Pará - ",
               input$inf13ano) 
      }else{
        paste0("População total atendida com abastecimento de água (Hab.), Região de Integração ",
               input$inf13ri," - ",
               input$inf13ano) 
      }
    })
    
    ## Mapa - Volume consumido de água (1.000 m³ /ano)----
    t132 <- reactive({
      
      if (input$inf4ri == "Pará") {
        paste0("Volume consumido de água (1.000 m³ /ano), Pará - ",
               input$inf4ano) 
      }else{
        paste0("Volume consumido de água (1.000 m³ /ano) Região de Integração ",
               input$inf4ri," - ",
               input$inf4ano) 
      }
    })
    
    ## Tabela - População total atendida com abastecimento de água (Hab.)----
    t133 <- reactive({
      paste0(
        "População total atendida com abastecimento de água (Hab.) por Município, ",
        input$inf13ri,
        " - ",
        input$inf13ano
      )
    })
    
    ## Tabela - Volume consumido de água (1.000 m³ /ano)----
    t134 <- reactive({
      paste0(
        "Volume consumido de água (1.000 m³ /ano) por Município, ",
        input$inf13ri,
        " - ",
        input$inf13ano
      )
    })
    ## Gráfico - Abastecimento de Água Segundo Consumidores e Volume Consumido----
    t135 <- reactive({
      paste0(
        "Abastecimento de Água Segundo Consumidores e Volume Consumido - ",
        min(inf13$ano),
        " a ",
        max(inf13$ano)
      )
    })
    #VISUALIZAÇÃO----
    # 1 - Total da Frota de Veículos Subdivididos em Licenciados e Não Licenciados----
    ## Mapa - Total da Frota de Veículos----
    output$inf1txt1 <- renderText({
      t11()
    })
    
    output$inf1map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$inf1ri == "Pará") {
        df <- inf1 %>%
          filter(localidade != "Pará",
                 ano == input$inf1ano,
                 categoria == input$inf1cat) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- inf1 %>%
          filter(localidade != "Pará",
                 ano == input$inf1ano,
                 categoria == input$inf1cat) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$inf1ri)
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
          c("#FECCFE", "#f2a1f1", "#e676e4", "#d844d5", "#c800c8"),
          domain = x$valor,
          bins = bins
        )
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/><b>%s:</b> %s ",
          x$name_muni,
          input$inf1cat,
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
          title = input$inf1cat,
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(
            big.mark = ".",
            decimal.mark = ",",
            digits = 2
          )
        )
    })
    ## Tabela - Total da Frota de Veículos Subdivididos em Licenciados e Não Licenciados----
    output$inf1txt2 <- renderText({
      t12()
    })
    
    output$inf1tab <- renderReactable({
      if (input$inf1ri == "Pará") {
        x <- inf1 %>%
          filter(localidade != "Pará", ano == input$inf1ano) %>%
          select(ri,
                 localidade,
                 categoria,
                 valor) %>%
          pivot_wider(names_from = categoria, values_from = valor)
      } else {
        x <- inf1 %>%
          filter(localidade != "Pará", ano == input$inf1ano) %>%
          select(ri,
                 localidade,
                 categoria,
                 valor) %>%
          pivot_wider(names_from = categoria, values_from = valor)
        x <- x %>% filter(ri == input$inf1ri)
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
          localidade = colDef(name = "Municípios", sticky = "left"),
          Frota = colDef(name = "Frota", format = colFormat(separators = T)),
          Licenciados = colDef(name = "Licenciados", format = colFormat(separators = T)),
          `Não Licenciados` = colDef(name = "Não Licenciados", format = colFormat(separators = T))
        ),
        defaultColDef = colDef(
          footerStyle = list(fontWeight = "bold"),
          na = "-",
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
    ## Gráfico - Total da Frota de Veículos Subdivididos em Licenciados e Não Licenciados----
        output$inf1txt3 <- renderText({
          t13()
        })

    output$inf1graf <- renderEcharts4r({
      inf1 %>%
        filter(localidade == "Pará") %>%
        pivot_wider(names_from = categoria, values_from = valor) %>%
        e_chart(x = ano) %>%
        e_line(
          serie = Licenciados,
          name = "Licenciados",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = `Não Licenciados`,
          name = "Não Licenciados",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
          itemStyle = list(barBorderRadius = 5)
        ) %>%
        e_line(
          serie = Frota,
          name = "Frota",
          legend = T,
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
    
    
    # 2 - Consumidores de Energia Elétrica Total----
    ## Mapa - Consumidores de Energia Elétrica Total----
    output$inf2txt1 <- renderText({
      t21()
      })
    
    output$inf2map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$inf2ri == "Pará") {
        df <- inf2 %>%
          filter(localidade != "Pará", ano == input$inf2ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- inf2 %>%
          filter(localidade != "Pará", ano == input$inf2ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$inf2ri)
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
          c("#FECCFE", "#f2a1f1", "#e676e4", "#d844d5", "#c800c8"),
          domain = x$valor,
          bins = bins
        )
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Unidades Consumidoras:</b> %s",
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
          title = "Unidades Consumidoras",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(
            big.mark = ".",
            decimal.mark = ",",
            digits = 2
          )
        )
    })
    
    ## Tabela - Consumidores de Energia Elétrica Total----
    output$inf2txt2 <- renderText({
      t22()
    })
    output$inf2tab <- renderReactable({
      if (input$inf2ri == "Pará") {
        x <- inf2 %>%
          filter(localidade != "Pará", ano == input$inf2ano) %>%
          select(ri, localidade, valor) %>%
          mutate(percentual = (valor / sum(valor)) * 100)
      } else {
        x <- inf2 %>%
          filter(localidade != "Pará", ano == input$inf2ano) %>%
          select(ri, localidade, valor)
        x <- x %>%
          filter(ri == input$inf2ri) %>%
          mutate(percentual = (valor / sum(valor)) * 100)
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
          localidade = colDef(name = "Municípios", sticky = "left"),
          valor = colDef(
            name = "Unidades Consumidoras",
            format = colFormat(separators = T, locales = "pt-BR")
          ),
          percentual = colDef(
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
    ## Gráfico - Consumidores de Energia Elétrica Total----
    output$inf2txt3 <- renderText({
      t23()
    })
    output$inf2graf <- renderEcharts4r({
      inf2 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#c800c8",
          name = "Consumidores",
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
          name = "Unidades Consumidoras",
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
    # 3 - Consumidores de Energia Elétrica por Tipo----
    ## Mapa - Consumidores de Energia Elétrica por Tipo----
    output$inf3txt1 <- renderText({
      t31()
    })
    
    output$inf3map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$inf3ri == "Pará") {
        df <- inf3 %>%
          filter(localidade != "Pará",
                 ano == input$inf3ano,
                 categoria == input$inf3cat) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- inf3 %>%
          filter(localidade != "Pará",
                 ano == input$inf3ano,
                 categoria == input$inf3cat) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$inf3ri)
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
          c("#FECCFE", "#f2a1f1", "#e676e4", "#d844d5", "#c800c8"),
          domain = x$valor,
          bins = bins
        )
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/><b>%s:</b> %s ",
          x$name_muni,
          input$inf3cat,
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
          title = input$inf3cat,
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(
            big.mark = ".",
            decimal.mark = ",",
            digits = 2
          )
        )
    })
    ## Tabela - Consumidores de Energia Elétrica por Tipo----
    output$inf3txt2 <- renderText({
      t32()
    })
    
    output$inf3tab <- renderReactable({
      if (input$inf3ri == "Pará") {
        x <- inf3 %>%
          filter(localidade != "Pará", ano == input$inf3ano) %>%
          select(ri,
                 localidade,
                 categoria,
                 valor) %>%
          pivot_wider(names_from = categoria, values_from = valor)
      } else {
        x <- inf3 %>%
          filter(localidade != "Pará", ano == input$inf3ano) %>%
          select(ri,
                 localidade,
                 categoria,
                 valor) %>%
          pivot_wider(names_from = categoria, values_from = valor)
        x <- x %>% filter(ri == input$inf3ri)
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
          ri = colDef(name = "Região de Integração",width = 200),
          localidade = colDef(name = "Municípios", sticky = "left"),
          Residencial = colDef(name = "Residencial", format = colFormat(separators = T)),
          Industrial = colDef(name = "Industrial", format = colFormat(separators = T)),
          Comercial = colDef(name = "Comercial", format = colFormat(separators = T)),
          Outros = colDef(name = "Outros", format = colFormat(separators = T))
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
    ## Gráfico - Consumidores de Energia Elétrica por Tipo----
    output$inf3txt3 <- renderText({
      t33()
    })
    output$inf3graf <- renderEcharts4r({
      inf3 %>%
        filter(localidade == "Pará") %>%
        pivot_wider(names_from = categoria, values_from = valor) %>%
        e_chart(x = ano) %>%
        e_line(
          serie = Residencial,
          name = "Residencial",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Industrial,
          name = "Industrial",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Comercial,
          name = "Comercial",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Outros,
          name = "Outros",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
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
          name = "Unidades Consumidoras",
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
    # 4 - Consumo de Energia Elétrica Total (kWH)----
    ## Mapa - Consumo de Energia Elétrica Total (kWH)----
    output$inf4txt1 <- renderText({
      t41()
    })
    
    output$inf4map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$inf4ri == "Pará") {
        df <- inf4 %>%
          filter(localidade != "Pará", ano == input$inf4ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- inf4 %>%
          filter(localidade != "Pará", ano == input$inf4ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$inf4ri)
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
          c("#FECCFE", "#f2a1f1", "#e676e4", "#d844d5", "#c800c8"),
          domain = x$valor,
          bins = bins
        )
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Total (kWH):</b> %s",
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
          title = "Total (kWH)",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(
            big.mark = ".",
            decimal.mark = ",",
            digits = 2
          )
        )
    })
    ## Tabela - Consumo de Energia Elétrica Total (kWH)----
    output$inf4txt2 <- renderText({
      t42()
    })
    
    output$inf4tab <- renderReactable({
      if (input$inf4ri == "Pará") {
        x <- inf4 %>%
          filter(localidade != "Pará", ano == input$inf4ano) %>%
          select(ri, localidade, valor) %>%
          mutate(percentual = (valor / sum(valor)) * 100)
      } else {
        x <- inf4 %>%
          filter(localidade != "Pará", ano == input$inf4ano) %>%
          select(ri, localidade, valor)
        x <- x %>%
          filter(ri == input$inf4ri) %>%
          mutate(percentual = (valor / sum(valor)) * 100)
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
            name = "Consumo (KWH)",
            format = colFormat(
              separators = T,
              locales = "pt-BR",
              digits = 2
            )
          ),
          percentual = colDef(
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
    ## Gráfico - Consumo de Energia Elétrica Total (kWH)----
    output$inf4txt3 <- renderText({
      t43()
    })
    output$inf4graf <- renderEcharts4r({
      inf4 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#c800c8",
          name = "Consumo(KWH)",
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
          name = "Consumo(KWH)",
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
    
    # 5 - Consumo de Energia Elétrica por Tipo----
    ## Mapa - Consumo de Energia Elétrica por Tipo----
    output$inf5txt1 <- renderText({
      t51()
    })
    
    output$inf5map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$inf5ri == "Pará") {
        df <- inf5 %>%
          filter(localidade != "Pará",
                 ano == input$inf5ano,
                 categoria == input$inf5cat) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- inf5 %>%
          filter(localidade != "Pará",
                 ano == input$inf5ano,
                 categoria == input$inf5cat) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$inf5ri)
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
          c("#FECCFE", "#f2a1f1", "#e676e4", "#d844d5", "#c800c8"),
          domain = x$valor,
          bins = bins
        )
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>%s:</b> %s ",
          x$name_muni,
          input$inf5cat,
          ifelse(
            is.na(x$valor),
            "Não disponível",
            format(
              round(x$valor, 0),
              big.mark = ".",
              decimal.mark = ","
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
          title = "Consumo (kWH)",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(
            big.mark = ".",
            decimal.mark = ",",
            digits = 2
          )
        )
    })
    ## Tabela - Consumo de Energia Elétrica por Tipo----
    output$inf5txt2 <- renderText({
      t52()
    })
    
    output$inf5tab <- renderReactable({
      if (input$inf5ri == "Pará") {
        x <- inf5 %>%
          filter(localidade != "Pará", ano == input$inf5ano) %>%
          select(ri,
                 localidade,
                 categoria,
                 valor) %>%
          pivot_wider(names_from = categoria, values_from = valor)
      } else {
        x <- inf5 %>%
          filter(localidade != "Pará", ano == input$inf5ano) %>%
          select(ri,
                 localidade,
                 categoria,
                 valor) %>%
          pivot_wider(names_from = categoria, values_from = valor)
        x <- x %>% filter(ri == input$inf5ri)
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
          localidade = colDef(name = "Municípios", sticky = "left"),
          Residencial = colDef(
            name = "Residencial",
            format = colFormat(separators = T, digits = 0)
          ),
          Industrial = colDef(
            name = "Industrial",
            format = colFormat(separators = T, digits = 0)
          ),
          Comercial = colDef(
            name = "Comercial",
            format = colFormat(separators = T, digits = 0)
          ),
          Outros = colDef(
            name = "Outros",
            format = colFormat(separators = T, digits = 0)
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
    ## Gráfico - Consumo de Energia Elétrica por Tipo----
    output$inf5txt3 <- renderText({
      t53()
    })
    output$inf5graf <- renderEcharts4r({
      inf5 %>%
        filter(localidade == "Pará") %>%
        pivot_wider(names_from = categoria, values_from = valor) %>%
        e_chart(x = ano) %>%
        e_line(
          serie = Residencial,
          name = "Residencial",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Industrial,
          name = "Industrial",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Comercial,
          name = "Comercial",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Outros,
          name = "Outros",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
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
          name = "Consumo (kWH)",
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
    # 6 - Total de  Carregamento nos Portos----
    ## Mapa - Total de  Carregamento nos Portos----
    output$inf6txt1 <- renderText({
      t61()
    })
    
    output$inf6map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$inf6ri == "Pará") {
        df <- inf6 %>%
          filter(localidade != "Pará", ano == input$inf6ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(portosgeo, df)
      } else {
        df <- inf6 %>%
          filter(localidade != "Pará", ano == input$inf6ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(portosgeo, df)
        x <- x %>% filter(ri == input$inf6ri)
      }
      
      if (input$inf6ri == "Pará") {
        leaflet(geopa,
                options =
                  leafletOptions(minZoom = 0,
                                 maxZoom = 15)) %>%
          addTiles() %>%
          addMarkers(
            data = x,
            ~ lng,
            ~ lat,
            label = ~ sprintf(
              "<strong>%s</strong> <br/> Carregamento (Toneladas): %s",
              porto,
              format(
                x$valor,
                big.mark = ".",
                decimal.mark = ",",
                digits = 2
              )
            ) %>% lapply(htmltools::HTML),
            labelOptions = labelOptions(textsize = "15px")
          ) %>%
          addPolygons(
            opacity = 1,
            color = "black",
            weight = 0.4,
            fill = F
          )
      } else {
        leaflet(x,
                options =
                  leafletOptions(minZoom = 0,
                                 maxZoom = 30)) %>%
          addTiles() %>%
          addMarkers(
            ~ lng,
            ~ lat,
            label = ~ sprintf(
              "<strong>%s</strong> <br/> Carregamento (Toneladas) %s",
              porto,
              format(
                x$valor,
                big.mark = ".",
                decimal.mark = ",",
                digits = 2
              )
            ) %>% lapply(htmltools::HTML),
            labelOptions = labelOptions(textsize = "15px")
          )
      }
    })
    ## Tabela - Total de  Carregamento nos Portos----
    output$inf6txt2 <- renderText({
      t62()
    })
    output$inf6tab <- renderReactable({
      inf6 %>%
        filter(localidade != "Pará", ano == input$inf6ano) %>%
        select(ri,
               localidade,
               valor) %>%
        reactable(
          defaultSortOrder = "desc",
          defaultSorted = "valor",
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
            localidade = colDef(name = "Portos"),
            valor = 
              colDef(name = " Carregamento (Toneladas)",
                     format = 
                       colFormat(
                         separators = T,
                         locales = "pt-BR",
                         digits = 0
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
    ## Gráfico - Total de  Carregamento nos Portos----
    output$inf6txt3 <- renderText({
      t63()
    })
    output$inf6graf <- renderEcharts4r({
      inf6 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#c800c8",
          name = "Carregamento (Toneladas)",
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
                                        digits = 0, 
                                        locale = "pt-BR"),
          axisPointer = list(type = "shadow")
        ) %>%
        e_x_axis(
          axisLabel = list(show = T, fontSize = 11),
          name = "Ano",
          splitLine = list(show = T),
          nameTextStyle = list(fontWeight = "bold", fontSize = 14)
        ) %>%
        e_y_axis(
          name = "Carregamento (Toneladas)",
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
    
    # 7 - Total de  Descarregamento nos Portos----
    ## Mapa - Total de  Descarregamento nos Portos----
    output$inf7txt1 <- renderText({
      t71()
    })
    
    output$inf7map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$inf7ri == "Pará") {
        df <- inf7 %>%
          filter(localidade != "Pará", 
                 ano == input$inf7ano) %>%
          select(ri, 
                 localidade, ano, valor)
        x <- cbind(portosgeo, df)
      } else {
        df <- inf7 %>%
          filter(localidade != "Pará", 
                 ano == input$inf7ano) %>%
          select(ri, 
                 localidade, ano, valor)
        x <- cbind(portosgeo, df)
        x <- x %>% filter(ri == input$inf7ri)
      }
      
      if (input$inf7ri == "Pará") {
        leaflet(geopa,
                options =
                  leafletOptions(minZoom = 0,
                                 maxZoom = 15)) %>%
          addTiles() %>%
          addMarkers(
            data = x,
            ~ lng,
            ~ lat,
            label = ~ sprintf(
              "<strong>%s</strong> <br/> Descarregamento (Toneladas): %s",
              porto,
              format(
                x$valor,
                big.mark = ".",
                decimal.mark = ",",
                digits = 2
              )
            ) %>% lapply(htmltools::HTML),
            labelOptions = labelOptions(textsize = "15px")
          ) %>%
          addPolygons(
            opacity = 1,
            color = "black",
            weight = 0.4,
            fill = F
          )
      } else {
        leaflet(x,
                options =
                  leafletOptions(minZoom = 0,
                                 maxZoom = 30)) %>%
          addTiles() %>%
          addMarkers(
            ~ lng,
            ~ lat,
            label = ~ sprintf(
              "<strong>%s</strong> <br/> Descarregamento (Toneladas): %s",
              porto,
              format(
                x$valor,
                big.mark = ".",
                decimal.mark = ",",
                digits = 4
              )
            ) %>% lapply(htmltools::HTML),
            labelOptions = labelOptions(textsize = "15px")
          )
      }
    })
    ## Tabela - Total de Descarregamento nos Portos----
    output$inf7txt2 <- renderText({
      t72()
    })
    
    output$inf7tab <- renderReactable({
      inf7 %>%
        filter(localidade != "Pará",ano == input$inf7ano) %>%
        select(ri,
               localidade,
               valor) %>%
        reactable(
          defaultSortOrder = "desc",
          defaultSorted = "valor",
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
              name = "Descarregamento (Toneladas)",
              format = colFormat(
                separators = T,
                locales = "pt-BR",
                digits = 0
              )
            ),
            percentual = colDef(
              format = colFormat(
                suffix = " %",
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
    ## Gráfico - Total de  Descarregamento nos Portos----
    output$inf7txt3 <- renderText({
      t73()
    })
    output$inf7graf <- renderEcharts4r({
      inf7 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#c800c8",
          name = "Descarregamento (Toneladas)",
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
          name = "Descarregamento (Toneladas)",
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
    # 8 - Total de  Movimentação nos Portos----
    ## Mapa - Total de  Movimentação nos Portos----
    output$inf8txt1 <- renderText({
      t81()
    })
    
    output$inf8map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$inf8ri == "Pará") {
        df <- inf8 %>%
          filter(localidade != "Pará", ano == input$inf8ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(portosgeo, df)
      } else {
        df <- inf8 %>%
          filter(localidade != "Pará", ano == input$inf8ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(portosgeo, df)
        x <- x %>% filter(ri == input$inf8ri)
      }
      
      if (input$inf8ri == "Pará") {
        leaflet(geopa,
                options =
                  leafletOptions(minZoom = 0,
                                 maxZoom = 15)) %>%
          addTiles() %>%
          addMarkers(
            data = x,
            ~ lng,
            ~ lat,
            label = ~ sprintf(
              "<strong>%s</strong> <br/> Movimentação (Toneladas): %s",
              porto,
              prettyNum(
                x$valor,
                big.mark = ".",
                decimal.mark = ",",
                digits = 5
              )
            ) %>% lapply(htmltools::HTML),
            labelOptions = labelOptions(textsize = "15px")
          ) %>%
          addPolygons(
            opacity = 1,
            color = "black",
            weight = 0.4,
            fill = F
          )
      } else {
        leaflet(x,
                options =
                  leafletOptions(minZoom = 0,
                                 maxZoom = 30)) %>%
          addTiles() %>%
          addMarkers(
            ~ lng,
            ~ lat,
            label = ~ sprintf(
              "<strong>%s</strong> <br/> Movimentação (Toneladas): %s",
              porto,
              format(
                x$valor,
                big.mark = ".",
                decimal.mark = ",",
                digits = 5
              )
            ) %>% lapply(htmltools::HTML),
            labelOptions = labelOptions(textsize = "15px")
          )
      }
    })
    ## Tabelas - Total de  Movimentação nos Portos----
    output$inf8txt2 <- renderText({
      t82()
    })
    
    output$inf8tab <- renderReactable({
      inf8 %>%
        filter(localidade != "Pará",ano == input$inf8ano) %>%
        select(ri,localidade, valor) %>%
        reactable(
          defaultSortOrder = "desc",
          defaultSorted = "valor",
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
              name = "Movimentação (Toneladas)",
              format = colFormat(
                separators = T,
                locales = "pt-BR",
                digits = 0
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
    ## Gráfico - Total de  Movimentação nos Portos----
    output$inf8txt3 <- renderText({
      t83()
    })
    output$inf8graf <- renderEcharts4r({
      inf8 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#c800c8",
          name = "Movimentação (Toneladas)",
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
          name = "Movimentação (Toneladas)",
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
    # 9 - Total de  Embarcações----
    ## Mapa - Total de  Embarcações----
    output$inf9txt1 <- renderText({
      t91()
    })
    
    output$inf9map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$inf9ri == "Pará") {
        df <- inf9 %>%
          filter(localidade != "Pará", ano == input$inf9ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(portosgeo, df)
      } else {
        df <- inf9 %>%
          filter(localidade != "Pará", ano == input$inf9ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(portosgeo, df)
        x <- x %>% filter(ri == input$inf9ri)
      }
      
      if (input$inf9ri == "Pará") {
        leaflet(geopa,
                options =
                  leafletOptions(minZoom = 0,
                                 maxZoom = 15)) %>%
          addTiles() %>%
          addMarkers(
            data = x,
            ~ lng,
            ~ lat,
            label = ~ sprintf(
              "<strong>%s</strong> <br/> Embarcações (Unidades): %s",
              porto,
              prettyNum(
                x$valor,
                big.mark = ".",
                decimal.mark = ",",
                digits = 5
              )
            ) %>% lapply(htmltools::HTML),
            labelOptions = labelOptions(textsize = "15px")
          ) %>%
          addPolygons(
            opacity = 1,
            color = "black",
            weight = 0.4,
            fill = F
          )
      } else {
        leaflet(x,
                options =
                  leafletOptions(minZoom = 0,
                                 maxZoom = 30)) %>%
          addTiles() %>%
          addMarkers(
            ~ lng,
            ~ lat,
            label = ~ sprintf(
              "<strong>%s</strong> <br/> Embarcações (Unidades): %s",
              porto,
              format(
                x$valor,
                big.mark = ".",
                decimal.mark = ",",
                digits = 5
              )
            ) %>% lapply(htmltools::HTML),
            labelOptions = labelOptions(textsize = "15px")
          )
      }
    })
    ## Tabela - Total de  Embarcações----
    output$inf9txt2 <- renderText({
      t92()
    })
    
    output$inf9tab <- renderReactable({
      inf9 %>%
        filter(localidade != "Pará",ano == input$inf9ano) %>%
        select(ri,
               localidade, 
               valor) %>%
        reactable(
          defaultSortOrder = "desc",
          defaultSorted = "valor",
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
              name = "Embarcações (Unidades)",
              format = colFormat(
                separators = T,
                locales = "pt-BR",
                digits = 0
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
    ## Gráfico - Total de  Embarcações----
    output$inf9txt3 <- renderText({
      t93()
    })
    output$inf9graf <- renderEcharts4r({
      inf9 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#c800c8",
          name = "Embarcações (Unidades)",
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
          name = "Embarcações (Unidades)",
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
    # 10 - Total das Receitas Operacionais----
    ## Mapa - Total das  Receitas Operacionais----
    output$inf10txt1 <- renderText({
      t101()
    })
    
    output$inf10map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$inf10ri == "Pará") {
        df <- inf10 %>%
          filter(localidade != "Pará", ano == input$inf10ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(portosgeo, df)
      } else {
        df <- inf10 %>%
          filter(localidade != "Pará", ano == input$inf10ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(portosgeo, df)
        x <- x %>% filter(ri == input$inf10ri)
      }
      
      if (input$inf10ri == "Pará") {
        leaflet(geopa,
                options =
                  leafletOptions(minZoom = 0,
                                 maxZoom = 15)) %>%
          addTiles() %>%
          addMarkers(
            data = x,
            ~ lng,
            ~ lat,
            label = ~ sprintf(
              "<strong>%s</strong> <br/> Receitas Operacionais(R$): %s",
              porto,
              prettyNum(
                x$valor,
                big.mark = ".",
                decimal.mark = ",",
                digits = 5
              )
            ) %>% lapply(htmltools::HTML),
            labelOptions = labelOptions(textsize = "15px")
          ) %>%
          addPolygons(
            opacity = 1,
            color = "black",
            weight = 0.4,
            fill = F
          )
      } else {
        leaflet(x,
                options =
                  leafletOptions(minZoom = 0,
                                 maxZoom = 30)) %>%
          addTiles() %>%
          addMarkers(
            ~ lng,
            ~ lat,
            label = ~ sprintf(
              "<strong>%s</strong> <br/> Receitas Operacionais(R$): %s",
              porto,
              format(
                x$valor,
                big.mark = ".",
                decimal.mark = ",",
                digits = 5
              )
            ) %>% lapply(htmltools::HTML),
            labelOptions = labelOptions(textsize = "15px")
          )
      }
    })
    
    ## Tabela - Total das  Receitas Operacionais----
    output$inf10txt2 <- renderText({
      t102()
    })
    
    output$inf10tab <- renderReactable({
      inf10 %>%
        filter(localidade != "Pará", ano == input$inf10ano) %>%
        select(ri, localidade, valor) %>%
        reactable(
          defaultSortOrder = "desc",
          defaultSorted = "valor",
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
              name = "Receitas Operacionais(R$)",
              format = colFormat(
                separators = T,
                locales = "pt-BR",
                digits = 0
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
    ## Gráfico - Total das  Receitas Operacionais----
    output$inf10txt3 <- renderText({
      t103()
    })
    output$inf10graf <- renderEcharts4r({
      inf10 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#c800c8",
          name = "Receitas Operacionais(R$)",
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
          name = "Receitas Operacionais(R$)",
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
    # 11 - Total de Pouso mais decolagem de Aeronaves ----
    ## Mapa - Total de Pouso mais decolagem de Aeronaves ----
    output$inf11txt1 <- renderText({
      t111()
    })
    
    output$inf11map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$inf11ri == "Pará") {
        df <- inf11 %>%
          filter(localidade != "Pará", ano == input$inf11ano) %>%
          select(ri,
                 localidade,
                 categoria,
                 ano,
                 valor) %>%
          pivot_wider(names_from = categoria, values_from = valor)
        x <- cbind(aeroportosgeo, df)
      } else {
        df <- inf11 %>%
          filter(localidade != "Pará", ano == input$inf11ano) %>%
          select(ri,
                 localidade,
                 categoria,
                 ano,
                 valor) %>%
          pivot_wider(names_from = categoria, values_from = valor)
        x <- cbind(aeroportosgeo, df)
        x <- x %>% filter(ri == input$inf11ri)
      }
      
      if (input$inf11ri == "Pará") {
        leaflet(geopa,
                options =
                  leafletOptions(minZoom = 0,
                                 maxZoom = 15)) %>%
          addTiles() %>%
          addMarkers(
            data = x,
            ~ lng,
            ~ lat,
            label = sprintf(
              "<strong>%s</strong> <br/> Total: %s <br/> Doméstico: %s <br/> Internacional: %s",
              x,
              prettyNum(
                x$Total,
                big.mark = ".",
                decimal.mark = ",",
                digits = 5
              ),
              prettyNum(
                x$Doméstico,
                big.mark = ".",
                decimal.mark = ",",
                digits = 5
              ),
              prettyNum(
                x$Internacional,
                big.mark = ".",
                decimal.mark = ",",
                digits = 5
              )
            ) %>% lapply(htmltools::HTML),
            labelOptions = labelOptions(textsize = "15px")
          ) %>%
          addPolygons(
            opacity = 1,
            color = "black",
            weight = 0.4,
            fill = F
          )
      } else {
        leaflet(x,
                options =
                  leafletOptions(minZoom = 0,
                                 maxZoom = 30)) %>%
          addTiles() %>%
          addMarkers(
            ~ lng,
            ~ lat,
            label =  sprintf(
              "<strong>%s</strong> <br/> Total: %s <br/> Doméstico: %s <br/> Internacional: %s",
              x,
              prettyNum(
                x$Total,
                big.mark = ".",
                decimal.mark = ",",
                digits = 5
              ),
              prettyNum(
                x$Doméstico,
                big.mark = ".",
                decimal.mark = ",",
                digits = 5
              ),
              prettyNum(
                x$Internacional,
                big.mark = ".",
                decimal.mark = ",",
                digits = 5
              )
            ) %>% lapply(htmltools::HTML),
            labelOptions = labelOptions(textsize = "15px")
          )
      }
    })
    ## Tabela - Total de Pouso mais decolagem de Aeronaves ----
    output$inf11txt2 <- renderText({
      t112()
    })
    
    output$inf11tab <- renderReactable({
      inf11 %>%
        filter(localidade != "Pará", ano == input$inf11ano) %>%
        pivot_wider(names_from = categoria, values_from = valor) %>%
        select(
          -tematica,
          -subtema,
          -indicador,
          -ano
        ) %>%
        reactable(
          defaultPageSize = 10,
          striped = FALSE,
          highlight = TRUE,
          bordered = TRUE,
          outlined = TRUE,
          resizable = TRUE,
          showSortable = TRUE,
          pagination = F,
          columns = list(
            ri = colDef(name = "Região de Integração",width = 200),
            localidade = colDef(name = "Municípios"),
            Doméstico = colDef(name = "Doméstico", format = colFormat(separators = T)),
            Internacional = colDef(name = "Internacional", format = colFormat(separators = T)),
            Total = colDef(name = "Total", format = colFormat(separators = T))
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
    
    ## Gráfico - Total de Pouso mais decolagem de Aeronaves ----
    output$inf11txt3 <- renderText({
      t113()
    })
    output$inf11graf <- renderEcharts4r({
      inf11 %>%
        filter(ri == "Pará") %>%
        pivot_wider(names_from = categoria, values_from = valor) %>%
        e_chart(x = ano) %>%
        e_line(
          serie = Doméstico,
          name = "Doméstico",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Internacional,
          name = "Internacional",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Total,
          name = "Total",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
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
          name = "Nº de Pousos",
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
    
    # 12 - Total de Embarque mais desembarque de passageiros----
    ## Mapa - Total de Embarque mais desembarque de passageiros ----
    output$inf12txt1 <- renderText({
      t121()
    })
    
    output$inf12map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$inf12ri == "Pará") {
        df <- inf12 %>%
          filter(localidade != "Pará", ano == input$inf12ano) %>%
          select(ri,
                 localidade,
                 categoria,
                 ano,
                 valor) %>%
          pivot_wider(names_from = categoria, values_from = valor)
        x <- cbind(aeroportosgeo, df)
      } else {
        df <- inf12 %>%
          filter(localidade != "Pará", ano == input$inf12ano) %>%
          select(ri,
                 localidade,
                 categoria,
                 ano,
                 valor) %>%
          pivot_wider(names_from = categoria, values_from = valor)
        x <- cbind(aeroportosgeo, df)
        x <- x %>% filter(ri == input$inf12ri)
      }
      
      if (input$inf12ri == "Pará") {
        leaflet(geopa,
                options =
                  leafletOptions(minZoom = 0,
                                 maxZoom = 15)) %>%
          addTiles() %>%
          addMarkers(
            data = x,
            ~ lng,
            ~ lat,
            label = ~ sprintf(
              "<strong>%s</strong> <br/> Total: %s <br/> Doméstico: %s <br/> Internacional: %s",
              aeroporto,
              prettyNum(
                x$Total,
                big.mark = ".",
                decimal.mark = ",",
                digits = 5
              ),
              prettyNum(
                x$Doméstico,
                big.mark = ".",
                decimal.mark = ",",
                digits = 5
              ),
              prettyNum(
                x$Internacional,
                big.mark = ".",
                decimal.mark = ",",
                digits = 5
              )
            ) %>% lapply(htmltools::HTML),
            labelOptions = labelOptions(textsize = "15px")
          ) %>%
          addPolygons(
            opacity = 1,
            color = "black",
            weight = 0.4,
            fill = F
          )
      } else {
        leaflet(x,
                options =
                  leafletOptions(minZoom = 0,
                                 maxZoom = 30)) %>%
          addTiles() %>%
          addMarkers(
            ~ lng,
            ~ lat,
            label = ~ sprintf(
              "<strong>%s</strong> <br/> Total: %s <br/> Doméstico: %s <br/> Internacional: %s",
              aeroporto,
              prettyNum(
                x$Total,
                big.mark = ".",
                decimal.mark = ",",
                digits = 5
              ),
              prettyNum(
                x$Doméstico,
                big.mark = ".",
                decimal.mark = ",",
                digits = 5
              ),
              prettyNum(
                x$Internacional,
                big.mark = ".",
                decimal.mark = ",",
                digits = 5
              )
            ) %>% lapply(htmltools::HTML),
            labelOptions = labelOptions(textsize = "15px")
          )
      }
    })
    ## Tabela - Total de Embarque mais desembarque de passageiros ----
    output$inf12txt2 <- renderText({
      t122()
    })
    
    output$inf12tab <- renderReactable({
      inf12 %>%
        filter(localidade != "Pará", ano == input$inf12ano) %>%
        pivot_wider(names_from = categoria, values_from = valor) %>%
        select(
          -tematica,
          -subtema,
          -indicador,
          -ano
        ) %>%
        reactable(
          defaultPageSize = 10,
          striped = FALSE,
          highlight = TRUE,
          bordered = TRUE,
          outlined = TRUE,
          resizable = TRUE,
          showSortable = TRUE,
          pagination = F,
          columns = list(
            ri = colDef(name = "Região de Integração",width = 200),
            localidade = colDef(name = "Municípios"),
            Doméstico = colDef(name = "Doméstico", format = colFormat(separators = T)),
            Internacional = colDef(name = "Internacional", format = colFormat(separators = T)),
            Total = colDef(name = "Total", format = colFormat(separators = T))
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
    
    ## Gráfico - Total de Embarque mais desembarque de passageiros ----
    output$inf12txt3 <- renderText({
      t123()
    })
    output$inf12graf <- renderEcharts4r({
      inf12 %>%
        filter(localidade == "Pará") %>%
        pivot_wider(names_from = categoria, values_from = valor) %>%
        e_chart(x = ano) %>%
        e_line(
          serie = Doméstico,
          name = "Doméstico",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Internacional,
          name = "Internacional",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Total,
          name = "Total",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
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
          name = "Nº de Embarques",
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
    # 13 - Abastecimento de Água Segundo Consumidores e Volume Consumido----
    ## Mapa - População total atendida com abastecimento de água (Hab.)----
    output$inf13txt1 <- renderText({
      t131()
    })
    
    output$inf13map1 <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$inf13ri == "Pará") {
        df <- inf13 %>%
          filter(
            localidade != "Pará",
            ano == input$inf13ano,
            categoria == "População total atendida com abastecimento de água (Hab.)"
          ) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- inf13 %>%
          filter(
            localidade != "Pará",
            ano == input$inf13ano,
            categoria == "População total atendida com abastecimento de água (Hab.)"
          ) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$inf13ri)
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
          c("#FECCFE", "#f2a1f1", "#e676e4", "#d844d5", "#c800c8"),
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
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(
            big.mark = ".",
            decimal.mark = ",",
            digits = 2
          )
        )
    })
    ## Mapa - Volume consumido de água (1.000 m³ /ano)----
    output$inf13txt2 <- renderText({
      t132()
    })
    
    output$inf13map2 <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$inf13ri == "Pará") {
        df <- inf13 %>%
          filter(
            localidade != "Pará",
            ano == input$inf13ano,
            categoria == "Volume consumido de água (1.000 m³ /ano)"
          ) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- inf13 %>%
          filter(
            localidade != "Pará",
            ano == input$inf13ano,
            categoria == "Volume consumido de água (1.000 m³ /ano)"
          ) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$inf13ri)
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
          c("#FECCFE", "#f2a1f1", "#e676e4", "#d844d5", "#c800c8"),
          domain = x$valor,
          bins = bins
        )
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Consumo (1.000 m³ /ano):</b> %s",
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
          title = "Consumo (1.000 m³ /ano)",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(
            big.mark = ".",
            decimal.mark = ",",
            digits = 2
          )
        )
    })
    ## Tabela - População total atendida com abastecimento de água (Hab.)----
    output$inf13txt3 <- renderText({
      t133()
    })
    
    output$inf13tab1 <- renderReactable({
      if (input$inf13ri == "Pará") {
        x <- inf13 %>%
          filter(
            localidade != "Pará",
            ano == input$inf13ano,
            categoria == "População total atendida com abastecimento de água (Hab.)"
          ) %>%
          select(ri, localidade, valor) %>%
          mutate(percentual = (valor / sum(valor)) * 100)
      } else {
        x <- inf13 %>%
          filter(
            localidade != "Pará",
            ano == input$inf13ano,
            categoria == "População total atendida com abastecimento de água (Hab.)"
          ) %>%
          select(ri, localidade, valor) %>%
          mutate(percentual = (valor / sum(valor)) * 100)
        x <- x %>% filter(ri == input$inf13ri)
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
            name = "População",
            format = colFormat(
              separators = T,
              locales = "pt-BR",
              digits = 0
            )
          ),
          percentual = colDef(
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
    ## Tabela - Volume consumido de água (1.000 m³ /ano)----
    output$inf13txt4 <- renderText({
      t134()
    })
    output$inf13tab2 <- renderReactable({
      if (input$inf13ri == "Pará") {
        x <- inf13 %>%
          filter(
            localidade != "Pará",
            ano == input$inf13ano,
            categoria == "Volume consumido de água (1.000 m³ /ano)"
          ) %>%
          select(ri, localidade, valor) %>%
          mutate(percentual = (valor / sum(valor)) * 100)
      } else {
        x <- inf13 %>%
          filter(
            localidade != "Pará",
            ano == input$inf13ano,
            categoria == "Volume consumido de água (1.000 m³ /ano)"
          ) %>%
          select(ri, localidade, valor) %>%
          mutate(percentual = (valor / sum(valor)) * 100)
        x <- x %>% filter(ri == input$inf13ri)
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
            name = "Consumo",
            format = colFormat(
              separators = T,
              locales = "pt-BR",
              digits = 0
            )
          ),
          percentual = colDef(
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
    ## Gráfico - Abastecimento de Água Segundo Consumidores e Volume Consumido----
    output$inf13txt5 <- renderText({
      t135()
    })
    output$inf13graf <- renderEcharts4r({
      inf13 %>%
        filter(localidade == "Pará") %>%
        pivot_wider(names_from = categoria, values_from = valor) %>%
        e_chart(x = ano) %>%
        e_line(
          serie = `População total atendida com abastecimento de água (Hab.)`,
          name = "População Atendida",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          y_index = 1,
          serie = `Volume consumido de água (1.000 m³ /ano)`,
          name = "Volume consumido (1.000 m³ /ano)",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
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
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = 14,
            padding = c(0, 0, 0, 20),
            verticalAlign = "top",
            lineHeight = 70
          )
        ) %>%
        e_y_axis(
          name = "População",
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
        e_y_axis(
          index = 1,
          name = "(1.000 m³ /ano)",
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
    #Sessão Download----
    #1 - Total da Frota de Veículos Subdivididos em Licenciados e Não Licenciados----
    ## Tabela - Total da Frota de Veículos Subdivididos em Licenciados e Não Licenciados----
    # Filtra os dados
    inf1_1 <- reactive({
      if (input$inf1ri == "Pará") {
        x <- inf1 %>%
          filter(localidade != "Pará", ano == input$inf1ano)
      } else {
        x <- inf1 %>%
          filter(localidade != "Pará", 
                 ano == input$inf1ano,
                 ri == input$inf1ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf1_1(), {
      t12()
      downset_Server("inf1_1", inf1_1(), t12())
    })
    ## Gráfico Total da Frota de Veículos Subdivididos em Licenciados e Não Licenciados----
    # Filtra os dados
    inf1_2 <- reactive({
      inf1 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf1_2(), {
      t13()
      downset_Server("inf1_2", inf1_2(), t13())
    })
    
    #2 - Consumidores de Energia Elétrica Total----
    ## Tabela - Consumidores de Energia Elétrica Total----
    # Filtra os dados
    inf2_1 <- reactive({
      if (input$inf2ri == "Pará") {
        x <- inf2 %>%
          filter(localidade != "Pará", ano == input$inf2ano) %>%
          mutate(percentual = (valor / sum(valor)) * 100)
      } else {
        x <- inf2 %>%
          filter(localidade != "Pará", 
                 ano == input$inf2ano,
                 ri == input$inf2ri
                 ) %>%
          mutate(percentual = (valor / sum(valor)) * 100)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf2_1(), {
      t22()
      downset_Server("inf2_1", inf2_1(), t22())
    })
    ## Gráfico Consumidores de Energia Elétrica Total----
    # Filtra os dados
    inf2_2 <- reactive({
      inf2 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf2_2(), {
      t23()
      downset_Server("inf2_2", inf2_2(), t23())
    })
    
    #3 - Consumidores de Energia Elétrica por Tipo----
    ## Tabela - Consumidores de Energia Elétrica por Tipo----
    # Filtra os dados
    inf3_1 <- reactive({
      if (input$inf3ri == "Pará") {
        x <- inf3 %>%
          filter(localidade != "Pará", ano == input$inf3ano) %>%
          pivot_wider(names_from = categoria, values_from = valor)
      } else {
        x <- inf3 %>%
          filter(localidade != "Pará",
                 ano == input$inf3ano,
                 ri == input$inf3ri
                 )
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf3_1(), {
      t32()
      downset_Server("inf3_1", inf3_1(), t32())
    })
    ## Gráfico Consumidores de Energia Elétrica por Tipo----
    # Filtra os dados
    inf3_2 <- reactive({
      inf3 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf3_2(), {
      t <- "inf3_2"
      downset_Server("inf3_2", inf3_2(), t)
    })
    
    #4 - Consumo de Energia Elétrica Total (kWH)----
    ## Tabela - Consumo de Energia Elétrica Total (kWH)----
    # Filtra os dados
    inf4_1 <- reactive({
      if (input$inf4ri == "Pará") {
        x <- inf4 %>%
          filter(localidade != "Pará", 
                 ano == input$inf4ano) %>%
          mutate(percentual = (valor / sum(valor)) * 100)
      } else {
        x <- inf4 %>%
          filter(localidade != "Pará",
                 ano == input$inf4ano,
                 ri == input$inf4ri
                 ) %>%
          mutate(percentual = (valor / sum(valor)) * 100)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf4_1(), {
      t42()
      downset_Server("inf4_1", inf4_1(), t42())
    })
    ## Gráfico Consumo de Energia Elétrica Total (kWH)----
    # Filtra os dados
    inf4_2 <- reactive({
      inf4 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf4_2(), {
      t43()
      downset_Server("inf4_2", inf4_2(), t43())
    })
    
    #5 - Consumo de Energia Elétrica por Tipo----
    ## Tabela - Consumo de Energia Elétrica por Tipo----
    # Filtra os dados
    inf5_1 <- reactive({
      if (input$inf5ri == "Pará") {
        x <- inf5 %>%
          filter(localidade != "Pará", 
                 ano == input$inf5ano,) %>%
          pivot_wider(names_from = categoria, values_from = valor)
      } else {
        x <- inf5 %>%
          filter(localidade != "Pará",
                 ano == input$inf5ano,
                 ri == input$inf5ri
          ) 
        }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf5_1(), {
      t52()
      downset_Server("inf5_1", inf5_1(), t52())
    })
    ## Gráfico Consumo de Energia Elétrica por Tipo----
    # Filtra os dados
    inf5_2 <- reactive({
      inf5 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf5_2(), {
      t53()
      downset_Server("inf5_2", inf5_2(), t53())
    })
    
    #6 - Total de  Carregamento nos Portos----
    ## Tabela - Total de  Carregamento nos Portos----
    # Filtra os dados
    inf6_1 <- reactive({
      inf6 %>%
        filter(localidade != "Pará", 
               ano == input$inf6ano) 
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf6_1(), {
      t62()
      downset_Server("inf6_1", inf6_1(), t62())
    })
    ## Gráfico Total de  Carregamento nos Portos----
    # Filtra os dados
    inf6_2 <- reactive({
      inf6 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf6_2(), {
      t63()
      downset_Server("inf6_2", inf6_2(), t63())
    })
    
    #7 - Total de  Descarregamento nos Portos----
    ## Tabela - Total de  Descarregamento nos Portos----
    # Filtra os dados
    inf7_1 <- reactive({
      inf7 %>%
        filter(localidade != "Pará",
               ano == input$inf7ano) 
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf7_1(), {
      t72()
      downset_Server("inf7_1", inf7_1(), t72())
    })
    ## Gráfico Total de  Descarregamento nos Portos----
    # Filtra os dados
    inf7_2 <- reactive({
      inf7 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf7_2(), {
      t73()
      downset_Server("inf7_2", inf7_2(), t73())
    })
    
    #8 - Total de  Movimentação nos Portos----
    ## Tabela - Total de  Movimentação nos Portos----
    # Filtra os dados
    inf8_1 <- reactive({
      inf8 %>%
        filter(localidade != "Pará",
               ano == input$inf8ano) 
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf8_1(), {
      t82()
      downset_Server("inf8_1", inf8_1(), t82())
    })
    ## Gráfico Total de  Movimentação nos Portos----
    # Filtra os dados
    inf8_2 <- reactive({
      inf8 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf8_2(), {
      t83()
      downset_Server("inf8_2", inf8_2(), t83())
    })
    
    #9 - Total de  Embarcações----
    ## Tabela - Total de  Embarcações----
    # Filtra os dados
    inf9_1 <- reactive({
      inf9 %>%
        filter(localidade != "Pará",
               ano == input$inf9ano) 
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf9_1(), {
      t92()
      downset_Server("inf9_1", inf9_1(), t92())
    })
    ## Gráfico Total de  Embarcações----
    # Filtra os dados
    inf9_2 <- reactive({
      inf9 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf9_2(), {
      t93()
      downset_Server("inf9_2", inf9_2(), t93())
    })
    
    #10 - Total das Receitas Operacionais----
    ## Tabela - Total das  Receitas Operacionais----
    # Filtra os dados
    inf10_1 <- reactive({
      inf10 %>%
        filter(localidade != "Pará",
               ano == input$inf10ano) 
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf10_1(), {
      t102()
      downset_Server("inf10_1", inf10_1(), t102())
    })
    ## Gráfico Total das  Receitas Operacionais----
    # Filtra os dados
    inf10_2 <- reactive({
      inf10 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf10_2(), {
      t103()
      downset_Server("inf10_2", inf10_2(), t103())
    })
    
    #11 - Total de Pouso mais decolagem de Aeronaves----
    ## Tabela - Total de Pouso mais decolagem de Aeronaves----
    # Filtra os dados
    inf11_1 <- reactive({
      inf11 %>%
        filter(localidade != "Pará",
               ano == input$inf11ano) 
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf11_1(), {
      t112()
      downset_Server("inf11_1", inf11_1(), t112())
    })
    ## Gráfico Total de Pouso mais decolagem de Aeronaves----
    # Filtra os dados
    inf11_2 <- reactive({
      inf11 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf11_2(), {
      t113()
      downset_Server("inf11_2", inf11_2(), t113())
    })
    
    #12 - Total de Embarque mais desembarque de passageiros----
    ## Tabela - Total de Embarque mais desembarque de passageiros----
    # Filtra os dados
    inf12_1 <- reactive({
      inf12 %>%
        filter(localidade != "Pará",
               ano == input$inf12ano)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf12_1(), {
      t122()
      downset_Server("inf12_1", inf12_1(), t122())
    })
    ## Gráfico - Total de Embarque mais desembarque de passageiros----
    # Filtra os dados
    inf12_2 <- reactive({
      inf12 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf12_2(), {
      t123()
      downset_Server("inf12_2", inf12_2(), t123())
    })
    
    #13 - Abastecimento de Água Segundo Consumidores e Volume Consumido----
    ## Tabela - População total atendida com abastecimento de água (Hab.)----
    # Filtra os dados
    inf13_1 <- reactive({
      if (input$inf13ri == "Pará") {
        x <- inf13 %>%
          filter(
            localidade != "Pará",
            ano == input$inf13ano,
            categoria == "População total atendida com abastecimento de água (Hab.)"
          ) %>%
          mutate(percentual = (valor / sum(valor)) * 100)
      } else {
        x <- inf13 %>%
          filter(
            localidade != "Pará",
            ano == input$inf13ano,
            categoria == "População total atendida com abastecimento de água (Hab.)",
            ri == input$inf13ri
          ) %>%
          mutate(percentual = (valor / sum(valor)) * 100)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf13_1(), {
      t133()
      downset_Server("inf13_1", inf13_1(), t133())
    })
    ## Tabela - Volume consumido de água (1.000 m³ /ano)----
    # Filtra os dados
    inf13_2 <- reactive({
      if (input$inf13ri == "Pará") {
        x <- inf13 %>%
          filter(
            localidade != "Pará",
            ano == input$inf13ano,
            categoria == "Volume consumido de água (1.000 m³ /ano)"
          ) %>%
          mutate(percentual = (valor / sum(valor)) * 100)
      } else {
        x <- inf13 %>%
          filter(
            localidade != "Pará",
            ano == input$inf13ano,
            categoria == "Volume consumido de água (1.000 m³ /ano)",
            ri == input$inf13ri
          ) %>% 
          mutate(percentual = (valor / sum(valor)) * 100)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf13_2(), {
      t134()
      downset_Server("inf13_2", inf13_2(), t134())
    })
    ## Gráfico - Abastecimento de Água Segundo Consumidores e Volume Consumido----
    # Filtra os dados
    inf13_3 <- reactive({
      inf13 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf13_3(), {
      t135()
      downset_Server("inf13_3", inf13_3(), t135())
    })
  })
}

# Play do Módulo
# ui <- dashboardPage(header = dashboardHeader(),
#                     sidebar = dashboardSidebar(),
#                     body = dashboardBody(fluidPage(
#                       infraestrutura_pa_ui("infraestrutura_pa")
#                     )))
# 
# 
# server <- function(input, output) {
#   infraestrutura_pa_Server("infraestrutura_pa")
# }
# 
# shinyApp(ui, server)
