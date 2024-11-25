# Funções de módulo de Social - Saúde - Estadual
# Função de UI
social_saude_pa_ui <- function(id) {
  fluidPage( # CAMINHO DO ARQUIVO CSS----
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "estilo.css")
    ),
    div(
      class = "navbar_social",
      navbarPage(
        tags$b("Saúde - Pará"),
        navbarMenu(
          tags$b("Escolha um Indicador"),
          # 1-Taxa de Mortalidade Infantil----
          tabPanel(
            "Taxa de Mortalidade Infantil",
            panel(
              ## Controle----
              heading =
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Taxa de Mortalidade Infantil"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "sd1ano"),
                  label = "Ano",
                  choices = sort(unique(sd1[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "sd1ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(sd1[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Taxa de Mortalidade Infantil----
              box(
                title = textOutput(NS(id, "sd1txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "sd1map"), height = "600px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  tags$h6(tags$b("Fonte:"), "DATASUS"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                )
              ),
              ## Tabela - Taxa de Mortalidade Infantil----
              box(
                title = textOutput(NS(id, "sd1txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "sd1tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd1_1"))
                )
              )
            ),
            fluidRow(
              ## Gráfico - Taxa de Mortalidade Infantil----
              box(
                title = textOutput(NS(id, "sd1txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "sd1graf")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd1_2"))
                )
              )
            )
          ),
          # 2-Taxa de Mortalidade na Infância----
          tabPanel(
            "Taxa de Mortalidade na Infância",
            panel(
              ## Controle----
              heading =
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Taxa de Mortalidade na Infância"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "sd2ano"),
                  label = "Ano",
                  choices = sort(unique(sd2[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "sd2ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(sd2[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Taxa de Mortalidade na Infância----
              box(
                title = textOutput(NS(id, "sd2txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "sd2map"), height = "600px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  )
                )
              ),
              ## Tabela - Taxa de Mortalidade na Infância----
              box(
                title = textOutput(NS(id, "sd2txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "sd2tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd2_1"))
                )
              )
            ),
            fluidRow(
              ## Gráfico - Taxa de Mortalidade na Infância----
              box(
                title = textOutput(NS(id, "sd2txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "sd2graf")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd2_2"))
                )
              )
            )
          ),
          # 3-Taxa de Mortalidade Materna----
          tabPanel(
            "Taxa de Mortalidade Materna",
            panel(
              ## Controle----
              heading =
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Taxa de Mortalidade Materna"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "sd3ano"),
                  label = "Ano",
                  choices = sort(unique(sd3[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "sd3ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(sd3[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Taxa de Mortalidade Materna----
              box(
                title = textOutput(NS(id, "sd3txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "sd3map"), height = "600px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  )
                )
              ),
              ## Tabela - Taxa de Mortalidade Materna----
              box(
                title = textOutput(NS(id, "sd3txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "sd3tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd3_1"))
                )
              )
            ),
            fluidRow(
              ## Gráfico - Taxa de Mortalidade Materna----
              box(
                title = textOutput(NS(id, "sd3txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "sd3graf")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd3_2"))
                )
              )
            )
          ),
          # 4-Taxa de Natalidade----
          tabPanel(
            "Taxa de Natalidade",
            panel(
              ## Controle----
              heading =
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Taxa de Natalidade"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "sd4ano"),
                  label = "Ano",
                  choices = sort(unique(sd4[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "sd4ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(sd4[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Taxa de Natalidade----
              box(
                title = textOutput(NS(id, "sd4txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "sd4map"), height = "600px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  )
                )
              ),
              ## Tabela - Taxa de Natalidade----
              box(
                title = textOutput(NS(id, "sd4txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "sd4tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd4_1"))
                )
              )
            ),
            fluidRow(
              ## Gráfico - Taxa de Natalidade----
              box(
                title = textOutput(NS(id, "sd4txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "sd4graf")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd4_2"))
                )
              )
            )
          ),
          # 5-Taxa de Mortalidade Geral----
          tabPanel(
            "Taxa de Mortalidade Geral",
            panel(
              ## Controle----
              heading =
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Taxa de Mortalidade Geral"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "sd5ano"),
                  label = "Ano",
                  choices = sort(unique(sd5[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "sd5ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(sd5[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - axa de Mortalidade Geral----
              box(
                title = textOutput(NS(id, "sd5txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "sd5map"), height = "600px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  )
                )
              ),
              ## Tabela - axa de Mortalidade Geral----
              box(
                title = textOutput(NS(id, "sd5txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "sd5tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd5_1"))
                )
              )
            ),
            fluidRow(
              ## Gráfico - axa de Mortalidade Geral----
              box(
                title = textOutput(NS(id, "sd5txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "sd5graf")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd5_2"))
                )
              )
            )
          ),
          # 6-Taxa de Mortalidade por Sexo----
          tabPanel(
            "Taxa de Mortalidade por Sexo",
            panel(
              ## Controle----
              heading =
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Taxa de Mortalidade por Sexo"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "sd6ano"),
                  label = "Ano",
                  choices = sort(unique(sd6[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "sd6ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(sd6[["ri"]]),
                  width = "200px"
                )
              ),
              tags$div(
                class = "seletor3",
                # select Sexo
                pickerInput(
                  inputId = NS(id, "sd6cat"),
                  label = "Sexo",
                  choices = unique(sd6[["categoria"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Taxa de Mortalidade por Sexo----
              box(
                title = textOutput(NS(id, "sd6txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "sd6map"), height = "600px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  )
                )
              ),
              ## Tabela - Taxa de Mortalidade por Sexo----
              box(
                title = textOutput(NS(id, "sd6txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "sd6tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd6_1"))
                )
              )
            ),
            fluidRow(
              ## Gráfico - Taxa de Mortalidade por Sexo----
              box(
                title = textOutput(NS(id, "sd6txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "sd6graf")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd_6_2"))
                )
              )
            )
          ),
          # 7-Número de Hospitais----
          tabPanel(
            "",
            panel(
              ## Controle----
              heading =
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Número de Hospitais"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "sd7ano"),
                  label = "Ano",
                  choices = sort(unique(sd7[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "sd7ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(sd7[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Número de Hospitais----
              box(
                title = textOutput(NS(id, "sd7txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "sd7map"), height = "600px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  )
                )
              ),
              ## Tabela - Número de Hospitais----
              box(
                title = textOutput(NS(id, "sd7txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "sd7tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd7_1"))
                )
              )
            ),
            fluidRow(
              ## Gráfico - Número de Hospitais----
              box(
                title = textOutput(NS(id, "sd7txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "sd7graf")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd7_2"))
                )
              )
            )
          ),
          # 8-Número de Postos e Centros de Saúde por 10.000 Habitantes----
          tabPanel(
            "Número de Postos e Centros de Saúde por 10.000 Habitantes",
            panel(
              ## Controle----
              heading =
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Número de Postos e Centros de Saúde por 10.000 Habitantes"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "sd8ano"),
                  label = "Ano",
                  choices = sort(unique(sd8[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "sd8ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(sd8[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Número de Postos e Centros de Saúde por 10.000 Habitantes----
              box(
                title = textOutput(NS(id, "sd8txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "sd8map"), height = "600px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  )
                )
              ),
              ## Tabela - Número de Postos e Centros de Saúde por 10.000 Habitantes----
              box(
                title = textOutput(NS(id, "sd8txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "sd8tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd8_1"))
                )
              )
            ),
            fluidRow(
              ## Gráfico - Número de Postos e Centros de Saúde por 10.000 Habitantes----
              box(
                title = textOutput(NS(id, "sd8txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "sd8graf")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd8_2"))
                )
              )
            )
          ),
          # 9-Médicos por 10 Mil Habitantes----
          tabPanel(
            "Médicos por 10 Mil Habitantes",
            panel(
              ## Controle----
              heading =
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Médicos por 10 Mil Habitantes"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "sd9ano"),
                  label = "Ano",
                  choices = sort(unique(sd9[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "sd9ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(sd9[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Médicos por 10 Mil Habitantes----
              box(
                title = textOutput(NS(id, "sd9txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "sd9map"), height = "600px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  )
                )
              ),
              ## Tabela - Médicos por 10 Mil Habitantes----
              box(
                title = textOutput(NS(id, "sd9txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "sd9tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd9_1"))
                )
              )
            ),
            fluidRow(
              ## Gráfico - Médicos por 10 Mil Habitantes----
              box(
                title = textOutput(NS(id, "sd9txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "sd9graf")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd9_2"))
                )
              )
            )
          ),
          # 10-Leito Hospitalar por Mil Habitantes----
          tabPanel(
            "Leito Hospitalar por Mil Habitantes",
            panel(
              ## Controle----
              heading =
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Leito Hospitalar por Mil Habitantes"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "sd10ano"),
                  label = "Ano",
                  choices = sort(unique(sd10[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "sd10ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(sd10[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Leito Hospitalar por Mil Habitantes----
              box(
                title = textOutput(NS(id, "sd10txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "sd10map"), height = "600px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  )
                )
              ),
              ## Tabela - Leito Hospitalar por Mil Habitantes----
              box(
                title = textOutput(NS(id, "sd10txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "sd10tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd10_1"))
                )
              )
            ),
            fluidRow(
              ## Gráfico - Leito Hospitalar por Mil Habitantes----
              box(
                title = textOutput(NS(id, "sd10txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "sd10graf")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd10_2"))
                )
              )
            )
          ),
          # 11-Percentual de Nascidos Vivos com 7 ou Mais Consultas Pré-natal----
          tabPanel(
            "Percentual de Nascidos Vivos com 7 ou Mais Consultas Pré-natal",
            panel(
              ## Controle----
              heading =
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Percentual de Nascidos Vivos com 7 ou Mais Consultas Pré-natal"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "sd11ano"),
                  label = "Ano",
                  choices = sort(unique(sd11[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "sd11ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(sd11[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Percentual de Nascidos Vivos com 7 ou Mais Consultas Pré-natal----
              box(
                title = textOutput(NS(id, "sd11txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "sd11map"), height = "600px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  )
                )
              ),
              ## Tabela - Percentual de Nascidos Vivos com 7 ou Mais Consultas Pré-natal----
              box(
                title = textOutput(NS(id, "sd11txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "sd11tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd11_1"))
                )
              )
            ),
            fluidRow(
              ## Gráfico - Percentual de Nascidos Vivos com 7 ou Mais Consultas Pré-natal----
              box(
                title = textOutput(NS(id, "sd11txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "sd11graf")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd11_2"))
                )
              )
            )
          ),
          # 12-Percentual de Nascidos Vivos por Parto Normal----
          tabPanel(
            "Percentual de Nascidos Vivos por Parto Normal",
            panel(
              ## Controle----
              heading =
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Percentual de Nascidos Vivos por Parto Normal"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "sd12ano"),
                  label = "Ano",
                  choices = sort(unique(sd12[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "sd12ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(sd12[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Percentual de Nascidos Vivos por Parto Normal----
              box(
                title = textOutput(NS(id, "sd12txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "sd12map"), height = "600px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  )
                )
              ),
              ## Tabela - Percentual de Nascidos Vivos por Parto Normal----
              box(
                title = textOutput(NS(id, "sd12txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "sd12tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd12_1"))
                )
              )
            ),
            fluidRow(
              ## Gráfico - Percentual de Nascidos Vivos por Parto Normal----
              box(
                title = textOutput(NS(id, "sd12txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "sd12graf")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd12_2"))
                )
              )
            )
          ),
          # 13-Percentual de Nascidos Vivos por Parto Cesário----
          tabPanel(
            "Percentual de Nascidos Vivos por Parto Cesário",
            panel(
              ## Controle----
              heading =
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Percentual de Nascidos Vivos por Parto Cesário"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "sd13ano"),
                  label = "Ano",
                  choices = sort(unique(sd13[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "sd13ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(sd13[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Percentual de Nascidos Vivos por Parto Cesário----
              box(
                title = textOutput(NS(id, "sd13txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "sd13map"), height = "600px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  )
                )
              ),
              ## Tabela - Percentual de Nascidos Vivos por Parto Cesário----
              box(
                title = textOutput(NS(id, "sd13txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "sd13tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd13_1"))
                )
              )
            ),
            fluidRow(
              ## Gráfico - Percentual de Nascidos Vivos por Parto Cesário----
              box(
                title = textOutput(NS(id, "sd13txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "sd13graf")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd13_2"))
                )
              )
            )
          ),
          # 14-Percentual de Nascidos Vivos de Mães Adolescentes na Faixa Etária de 10 a 19 Anos----
          tabPanel(
            "Percentual de Nascidos Vivos de Mães Adolescentes na Faixa Etária de 10 a 19 Anos",
            panel(
              ## Controle----
              heading =
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Percentual de Nascidos Vivos de Mães Adolescentes na Faixa Etária de 10 a 19 Anos"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "sd14ano"),
                  label = "Ano",
                  choices = sort(unique(sd14[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "sd14ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(sd14[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Percentual de Nascidos Vivos de Mães Adolescentes na Faixa Etária de 10 a 19 Anos----
              box(
                title = textOutput(NS(id, "sd14txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "sd14map"), height = "600px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  )
                )
              ),
              ## Tabela - Percentual de Nascidos Vivos de Mães Adolescentes na Faixa Etária de 10 a 19 Anos----
              box(
                title = textOutput(NS(id, "sd14txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "sd14tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd14_1"))
                )
              )
            ),
            fluidRow(
              ## Gráfico - Percentual de Nascidos Vivos de Mães Adolescentes na Faixa Etária de 10 a 19 Anos----
              box(
                title = textOutput(NS(id, "sd14txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "sd14graf")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd14_2"))
                )
              )
            )
          ),
          # 15-Taxa de Incidência da Hanseníase----
          tabPanel(
            "Taxa de Incidência da Hanseníase",
            panel(
              ## Controle----
              heading =
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Taxa de Incidência da Hanseníase"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "sd15ano"),
                  label = "Ano",
                  choices = sort(unique(sd15[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "sd15ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(sd15[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Taxa de Incidência da Hanseníase----
              box(
                title = textOutput(NS(id, "sd15txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "sd15map"), height = "600px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  )
                )
              ),
              ## Tabela - Taxa de Incidência da Hanseníase----
              box(
                title = textOutput(NS(id, "sd15txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "sd15tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd15_1"))
                )
              )
            ),
            fluidRow(
              ## Gráfico - Taxa de Incidência da Hanseníase----
              box(
                title = textOutput(NS(id, "sd15txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "sd15graf")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd15_2"))
                )
              )
            )
          ),
          # 16-Taxa de Incidência da Tuberculose----
          tabPanel(
            "Taxa de Incidência da Tuberculose",
            panel(
              ## Controle----
              heading =
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Taxa de Incidência da Tuberculose"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "sd16ano"),
                  label = "Ano",
                  choices = sort(unique(sd16[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "sd16ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(sd16[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Taxa de Incidência da Tuberculose----
              box(
                title = textOutput(NS(id, "sd16txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "sd16map"), height = "600px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  )
                )
              ),
              ## Tabela - Taxa de Incidência da Tuberculose----
              box(
                title = textOutput(NS(id, "sd16txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "sd16tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd16_1"))
                )
              )
            ),
            fluidRow(
              ## Gráfico - Taxa de Incidência da Tuberculose----
              box(
                title = textOutput(NS(id, "sd16txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "sd16graf")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd16_2"))
                )
              )
            )
          ),
          # 17-Proporção de Mulheres de 25 a 64 anos que Realizaram Exames Citopatológicos do Colo do Útero----
          tabPanel(
            "Proporção de Mulheres de 25 a 64 anos que Realizaram Exames Citopatológicos do Colo do Útero",
            panel(
              ## Controle----
              heading =
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Proporção de Mulheres de 25 a 64 anos que Realizaram Exames Citopatológicos do Colo do Útero"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "sd17ano"),
                  label = "Ano",
                  choices = sort(unique(sd17[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "sd17ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(sd17[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Proporção de Mulheres de 25 a 64 anos que Realizaram Exames Citopatológicos do Colo do Útero----
              box(
                title = textOutput(NS(id, "sd17txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "sd17map"), height = "600px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  )
                )
              ),
              ## Tabela - Proporção de Mulheres de 25 a 64 anos que Realizaram Exames Citopatológicos do Colo do Útero----
              box(
                title = textOutput(NS(id, "sd17txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "sd17tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd17_1"))
                )
              )
            ),
            fluidRow(
              ## Gráfico - Proporção de Mulheres de 25 a 64 anos que Realizaram Exames Citopatológicos do Colo do Útero----
              box(
                title = textOutput(NS(id, "sd17txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "sd17graf")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd17_2"))
                )
              )
            )
          ),
          # 18-Óbitos por Residência Neoplasias (Tumores)----
          tabPanel(
            "Óbitos por Residência Neoplasias (Tumores)",
            panel(
              ## Controle----
              heading =
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Óbitos por Residência Neoplasias (Tumores)"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "sd18ano"),
                  label = "Ano",
                  choices = sort(unique(sd18[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "sd18ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(sd18[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Óbitos por Residência Neoplasias (Tumores)----
              box(
                title = textOutput(NS(id, "sd18txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "sd18map"), height = "600px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  )
                )
              ),
              ## Tabela - Óbitos por Residência Neoplasias (Tumores)----
              box(
                title = textOutput(NS(id, "sd18txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "sd18tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd18_1"))
                )
              )
            ),
            fluidRow(
              ## Gráfico - Óbitos por Residência Neoplasias (Tumores)----
              box(
                title = textOutput(NS(id, "sd18txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "sd18graf")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd18_2"))
                )
              )
            )
          ),
          # 19-Óbitos por Residência Algumas Doenças Infecciosas e Parasitárias----
          tabPanel(
            "Óbitos por Residência Algumas Doenças Infecciosas e Parasitárias",
            panel(
              ## Controle----
              heading =
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Óbitos por Residência Algumas Doenças Infecciosas e Parasitárias"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "sd19ano"),
                  label = "Ano",
                  choices = sort(unique(sd19[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "sd19ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(sd19[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Óbitos por Residência Algumas Doenças Infecciosas e Parasitárias----
              box(
                title = textOutput(NS(id, "sd19txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "sd19map"), height = "600px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  )
                )
              ),
              ## Tabela - Óbitos por Residência Algumas Doenças Infecciosas e Parasitárias----
              box(
                title = textOutput(NS(id, "sd19txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "sd19tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd19_1"))
                )
              )
            ),
            fluidRow(
              ## Gráfico - Óbitos por Residência Algumas Doenças Infecciosas e Parasitárias----
              box(
                title = textOutput(NS(id, "sd19txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "sd19graf")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd19_2"))
                )
              )
            )
          ),
          # 20-Óbitos por Residência Doenças do Aparelho Circulatório----
          tabPanel(
            "Óbitos por Residência Doenças do Aparelho Circulatório",
            panel(
              ## Controle----
              heading =
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Óbitos por Residência Doenças do Aparelho Circulatório"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "sd20ano"),
                  label = "Ano",
                  choices = sort(unique(sd20[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "sd20ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(sd20[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Óbitos por Residência Doenças do Aparelho Circulatório----
              box(
                title = textOutput(NS(id, "sd20txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "sd20map"), height = "600px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  )
                )
              ),
              ## Tabela - Óbitos por Residência Doenças do Aparelho Circulatório----
              box(
                title = textOutput(NS(id, "sd20txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "sd20tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd20_1"))
                )
              )
            ),
            fluidRow(
              ## Gráfico - Óbitos por Residência Doenças do Aparelho Circulatório----
              box(
                title = textOutput(NS(id, "sd20txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "sd20graf")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd20_2"))
                )
              )
            )
          ),
          # 21-Óbitos por Residência Doenças do Aparelho Digestivo----
          tabPanel(
            "Óbitos por Residência Doenças do Aparelho Digestivo",
            panel(
              ## Controle----
              heading =
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Óbitos por Residência Doenças do Aparelho Digestivo"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "sd21ano"),
                  label = "Ano",
                  choices = sort(unique(sd21[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "sd21ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(sd21[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Óbitos por Residência Doenças do Aparelho Digestivo----
              box(
                title = textOutput(NS(id, "sd21txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "sd21map"), height = "600px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  )
                )
              ),
              ## Tabela - Óbitos por Residência Doenças do Aparelho Digestivo----
              box(
                title = textOutput(NS(id, "sd21txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "sd21tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd21_1"))
                )
              )
            ),
            fluidRow(
              ## Gráfico - Óbitos por Residência Doenças do Aparelho Digestivo----
              box(
                title = textOutput(NS(id, "sd21txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "sd21graf")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd21_2"))
                )
              )
            )
          ),
          # 22-Óbitos por Residência Doenças do Aparelho Respiratório----
          tabPanel(
            "Óbitos por Residência Doenças do Aparelho Respiratório",
            panel(
              ## Controle----
              heading =
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Óbitos por Residência Doenças do Aparelho Respiratório"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "sd22ano"),
                  label = "Ano",
                  choices = sort(unique(sd22[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "sd22ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(sd22[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Óbitos por Residência Doenças do Aparelho Respiratório----
              box(
                title = textOutput(NS(id, "sd22txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "sd22map"), height = "600px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  )
                )
              ),
              ## Tabela - Óbitos por Residência Doenças do Aparelho Respiratório----
              box(
                title = textOutput(NS(id, "sd22txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "sd22tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd22_1"))
                )
              )
            ),
            fluidRow(
              ## Gráfico - Óbitos por Residência Doenças do Aparelho Respiratório----
              box(
                title = textOutput(NS(id, "sd22txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "sd22graf")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd22_2"))
                )
              )
            )
          ),
          # 23-Óbitos por Residência Algumas Afecções Originadas no Período Perinatal----
          tabPanel(
            "Óbitos por Residência Algumas Afecções Originadas no Período Perinatal",
            panel(
              ## Controle----
              heading =
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Óbitos por Residência Algumas Afecções Originadas no Período Perinatal"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "sd23ano"),
                  label = "Ano",
                  choices = sort(unique(sd23[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "sd23ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(sd23[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Óbitos por Residência Algumas Afecções Originadas no Período Perinatal----
              box(
                title = textOutput(NS(id, "sd23txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "sd23map"), height = "600px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  )
                )
              ),
              ## Tabela - Óbitos por Residência Algumas Afecções Originadas no Período Perinatal----
              box(
                title = textOutput(NS(id, "sd23txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "sd23tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd23_1"))
                )
              )
            ),
            fluidRow(
              ## Gráfico - Óbitos por Residência Algumas Afecções Originadas no Período Perinatal----
              box(
                title = textOutput(NS(id, "sd23txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "sd23graf")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd23_2"))
                )
              )
            )
          ),
          # 24-Percentual de Óbitos por Infarto Agudo do Miocárdio (IAM)----
          tabPanel(
            "Percentual de Óbitos por Infarto Agudo do Miocárdio (IAM)",
            panel(
              ## Controle----
              heading =
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Percentual de Óbitos por Infarto Agudo do Miocárdio (IAM)"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "sd24ano"),
                  label = "Ano",
                  choices = sort(unique(sd24[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "sd24ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(sd24[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Percentual de Óbitos por Infarto Agudo do Miocárdio (IAM)----
              box(
                title = textOutput(NS(id, "sd24txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "sd24map"), height = "600px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  )
                )
              ),
              ## Tabela - Percentual de Óbitos por Infarto Agudo do Miocárdio (IAM)----
              box(
                title = textOutput(NS(id, "sd24txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "sd24tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd24_1"))
                )
              )
            ),
            fluidRow(
              ## Gráfico - Percentual de Óbitos por Infarto Agudo do Miocárdio (IAM)----
              box(
                title = textOutput(NS(id, "sd24txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "sd24graf")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd24_2"))
                )
              )
            )
          ),
          # 25-Taxa de Cobertura da Atenção Primária à Saúde----
          tabPanel(
            "Taxa de Cobertura da Atenção Primária à Saúde",
            panel(
              ## Controle----
              heading =
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Taxa de Cobertura da Atenção Primária à Saúde"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "sd25ano"),
                  label = "Ano",
                  choices = sort(unique(sd25[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "sd25ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(sd25[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Taxa de Cobertura da Atenção Primária à Saúde----
              box(
                title = textOutput(NS(id, "sd25txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "sd25map"), height = "600px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  )
                )
              ),
              ## Tabela - Taxa de Cobertura da Atenção Primária à Saúde----
              box(
                title = textOutput(NS(id, "sd25txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "sd25tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd25_1"))
                )
              )
            ),
            fluidRow(
              ## Gráfico - Taxa de Cobertura da Atenção Primária à Saúde----
              box(
                title = textOutput(NS(id, "sd25txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "sd25graf")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd25_2"))
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
social_saude_pa_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    #TÍTULOS----
    # 1-Taxa de Mortalidade Infantil----
    ## Mapa - Taxa de Mortalidade Infantil----
    t11 <- reactive({
      if (input$sd1ri == "Pará") {
        paste0(
          "Taxa de Mortalidade Infantil, Pará - ",
          input$sd1ano
        )
      } else{
        paste0(
          "Taxa de Mortalidade Infantil, Região de Integração ",
          input$sd1ri,
          " - ",
          input$sd1ano
        )
      }
    })
    ## Tabela - Taxa de Mortalidade Infantil----
    t12 <- reactive({
      if (input$sd1ri == "Pará") {
        paste0(
          "Taxa de Mortalidade Infantil por Município, Pará - ",
          input$sd1ano
        )
      } else{
        paste0(
          "Taxa de Mortalidade Infantil por Município, Região de Integração ",
          input$sd1ri,
          " - ",
          input$sd1ano
        )
      }
    })
    ## Gráfico - Taxa de Mortalidade Infantil----
    t13 <- reactive({
      paste0("Taxa de Mortalidade Infantil, Pará - ", min(sd1$ano), " a ", max(sd1$ano))
    })
  
    # 2-Taxa de Mortalidade na Infância----
    ## Mapa - Taxa de Mortalidade na Infância----
    t21 <- reactive({
      if (input$sd2ri == "Pará") {
        paste0(
          "Taxa de Mortalidade na Infância, Pará - ",
          input$sd2ano
        )
      } else{
        paste0(
          "Taxa de Mortalidade na Infância, Região de Integração ",
          input$sd2ri,
          " - ",
          input$sd2ano
        )
      }
    })
    ## Tabela - Taxa de Mortalidade na Infância----
    t22 <- reactive({
      if (input$sd2ri == "Pará") {
        paste0(
          "Taxa de Mortalidade na Infância por Município, Pará - ",
          input$sd2ano
        )
      } else{
        paste0(
          "Taxa de Mortalidade na Infância por Município, Região de Integração ",
          input$sd2ri,
          " - ",
          input$sd2ano
        )
      }
    })
    ## Gráfico - Taxa de Mortalidade na Infância----
    t23 <- reactive({
      paste0("Taxa de Mortalidade na Infância, Pará - ", min(sd2$ano), " a ", max(sd2$ano))
    })

    # 3-Taxa de Mortalidade Materna----
    ## Mapa - Taxa de Mortalidade Materna----
    t31 <- reactive({
      if (input$sd3ri == "Pará") {
        paste0(
          "Taxa de Mortalidade Materna, Pará - ",
          input$sd3ano
        )
      } else{
        paste0(
          "Taxa de Mortalidade Materna, Região de Integração ",
          input$sd3ri,
          " - ",
          input$sd3ano
        )
      }
    })
    ## Tabela - Taxa de Mortalidade Materna----
    t32 <- reactive({
      if (input$sd3ri == "Pará") {
        paste0(
          "Taxa de Mortalidade Materna por Município, Pará - ",
          input$sd3ano
        )
      } else{
        paste0(
          "Taxa de Mortalidade Materna por Município, Região de Integração ",
          input$sd3ri,
          " - ",
          input$sd3ano
        )
      }
    })
    ## Gráfico - Taxa de Mortalidade Materna----
    t33 <- reactive({
      paste0("Taxa de Mortalidade Materna, Pará - ", min(sd3$ano), " a ", max(sd3$ano))
    })
    # 4-Taxa de Natalidade----
    ## Mapa - Taxa de Natalidade----
    t42 <- reactive({
      if (input$sd4ri == "Pará") {
        paste0(
          "Taxa de Natalidade por Município, Pará - ",
          input$sd4ano
        )
      } else{
        paste0(
          "Taxa de Natalidade por Município, Região de Integração ",
          input$sd4ri,
          " - ",
          input$sd4ano
        )
      }
    })
    ## Tabela - Taxa de Natalidade----
    t42 <- reactive({
      if (input$sd4ri == "Pará") {
        paste0(
          "Taxa de Natalidade por Município, Pará - ",
          input$sd4ano
        )
      } else{
        paste0(
          "Taxa de Natalidade por Município, Região de Integração ",
          input$sd4ri,
          " - ",
          input$sd4ano
        )
      }
    })
    ## Gráfico - Taxa de Natalidade----
    t43 <- reactive({
      paste0("Taxa de Natalidade, Pará - ", min(sd4$ano), " a ", max(sd4$ano))
    })
    
    # 5-Taxa de Mortalidade Geral----
    ## Mapa - Taxa de Mortalidade Geral----
    t51 <- reactive({
      if (input$sd5ri == "Pará") {
        paste0(
          "Taxa de Mortalidade Geral, Pará - ",
          input$sd5ano
        )
      } else{
        paste0(
          "Taxa de Mortalidade Geral, Região de Integração ",
          input$sd5ri,
          " - ",
          input$sd5ano
        )
      }
    })
    ## Tabela - Taxa de Mortalidade Geral----
    t52 <- reactive({
      if (input$sd5ri == "Pará") {
        paste0(
          "Taxa de Mortalidade Geral por Município, Pará - ",
          input$sd5ano
        )
      } else{
        paste0(
          "Taxa de Mortalidade Geral por Município, Região de Integração ",
          input$sd5ri,
          " - ",
          input$sd5ano
        )
      }
    })
    ## Gráfico - Taxa de Mortalidade Geral----
    t53 <- reactive({
      paste0("Taxa de Mortalidade Geral, Pará - ", min(sd5$ano), " a ", max(sd5$ano))
    })

    # 6-Taxa de Mortalidade por Sexo----
    ## Mapa - Taxa de Mortalidade por Sexo----
    t61 <- reactive({
      if (input$sd6ri == "Pará") {
        paste0(
          "Taxa de Mortalidade por Sexo ",
          input$sd6cat,
          ", Pará - ",
          input$sd6ano
        )
      } else{
        paste0(
          "Taxa de Mortalidade por Sexo ",
          input$sd6cat,
          ", Região de Integração ",
          input$sd6ri,
          " - ",
          input$sd6ano
        )
      }
    })
    ## Tabela - Taxa de Mortalidade por Sexo----
    t62 <- reactive({
      if (input$sd6ri == "Pará") {
        paste0(
          "Taxa de Mortalidade por Sexo e Município, Pará -  ",
          input$sd6ano
        )
      } else{
        paste0(
          "Taxa de Mortalidade por Sexo e Município, Região de Integração ",
          input$sd6ri,
          " - ",
          input$sd6ano
        )
      }
    })
    ## Gráfico - Taxa de Mortalidade por Sexo----
    t63 <- reactive({
      paste0("Taxa de Mortalidade por Sexo, Pará - ", min(sd6$ano), " a ", max(sd6$ano))
    })
    # 7-Número de Hospitais----
    ## Mapa - Número de Hospitais----
    t71 <- reactive({
      if (input$sd7ri == "Pará") {
        paste0(
          "Número de Hospitais, Pará - ",
          input$sd7ano
        )
      } else{
        paste0(
          "Número de Hospitais, Região de Integração ",
          input$sd7ri,
          " - ",
          input$sd7ano
        )
      }
    })
    ## Tabela - Número de Hospitais----
    t72 <- reactive({
        if (input$sd7ri == "Pará") {
          paste0(
            "Número de Hospitais por Município, Pará - ",
            input$sd7ano
          )
        } else{
          paste0(
            "Número de Hospitais por Município, Região de Integração ",
            input$sd7ri,
            " - ",
            input$sd7ano
          )
        }
    })
    ## Gráfico - Número de Hospitais----
    t73 <- reactive({
      paste0("Número de Hospitais, Pará - ", min(sd7$ano), " a ", max(sd7$ano))
    })

    # 8-Número de Postos e Centros de Saúde por 10.000 Habitantes----
    ## Mapa - Número de Postos e Centros de Saúde por 10.000 Habitantes----
    t81 <- reactive({
      if (input$sd8ri == "Pará") {
        paste0(
          "Número de Postos e Centros de Saúde por 10.000 Habitantes, Pará - ",
          input$sd8ano
        )
      } else{
        paste0(
          "Número de Postos e Centros de Saúde por 10.000 Habitantes, Região de Integração ",
          input$sd8ri,
          " - ",
          input$sd8ano
        )
      }
    })
    ## Tabela - Número de Postos e Centros de Saúde por 10.000 Habitantes----
    t82 <- reactive({
      if (input$sd8ri == "Pará") {
        paste0(
          "Número de Postos e Centros de Saúde por 10.000 Habitantes por Município, Pará - ",
          input$sd8ano
        )
      } else{
        paste0(
          "Número de Postos e Centros de Saúde por 10.000 Habitantes por Município, Região de Integração ",
          input$sd8ri,
          " - ",
          input$sd8ano
        )
      }    })
    ## Gráfico - Número de Postos e Centros de Saúde por 10.000 Habitantes----
    t83 <- reactive({
      paste0("Número de Postos e Centros de Saúde por 10.000 Habitantes, Pará - ", min(sd8$ano), " a ", max(sd8$ano))
    })
    
    # 9-Médicos por 10 Mil Habitantes----
    ## Mapa - Médicos por 10 Mil Habitantes----
    t91 <- reactive({
      if (input$sd9ri == "Pará") {
        paste0(
          "Médicos por 10 Mil Habitantes, Pará - ",
          input$sd9ano
        )
      } else{
        paste0(
          "Médicos por 10 Mil Habitantes, Região de Integração ",
          input$sd9ri,
          " - ",
          input$sd9ano
        )
      }
    })
    ## Tabela - Médicos por 10 Mil Habitantes----
    t92 <- reactive({
      if (input$sd9ri == "Pará") {
        paste0(
          "Médicos por 10 Mil Habitantes por Município, Pará - ",
          input$sd9ano
        )
      } else{
        paste0(
          "Médicos por 10 Mil Habitantes por Município, Região de Integração ",
          input$sd9ri,
          " - ",
          input$sd9ano
        )
      }
    })
    ## Gráfico - Médicos por 10 Mil Habitantes----
    t93 <- reactive({
      paste0("Médicos por 10 Mil Habitantes, Pará - ", min(sd9$ano), " a ", max(sd9$ano))
    })
    
    # 10-Leito Hospitalar por Mil Habitantes----
    ## Mapa - Leito Hospitalar por Mil Habitantes----
    t101 <- reactive({
      if (input$sd10ri == "Pará") {
        paste0(
          "Leito Hospitalar por Mil Habitantes, Pará - ",
          input$sd10ano
        )
      } else{
        paste0(
          "Leito Hospitalar por Mil Habitantes, Região de Integração ",
          input$sd10ri,
          " - ",
          input$sd10ano
        )
      }
    })
    ## Tabela - Leito Hospitalar por Mil Habitantes----
    t102 <- reactive({
      if (input$sd10ri == "Pará") {
        paste0(
          "Leito Hospitalar por Mil Habitantes por Município, Pará - ",
          input$sd10ano
        )
      } else{
        paste0(
          "Leito Hospitalar por Mil Habitantes por Município, Região de Integração ",
          input$sd10ri,
          " - ",
          input$sd10ano
        )
      }
    })
    ## Gráfico - Leito Hospitalar por Mil Habitantes----
    t103 <- reactive({
      paste0("Leito Hospitalar por Mil Habitantes, Pará - ", min(sd10$ano), " a ", max(sd10$ano))
    })
    
    # 11-Percentual de Nascidos Vivos com 7 ou Mais Consultas Pré-natal----
    
    ## Mapa - Percentual de Nascidos Vivos com 7 ou Mais Consultas Pré-natal----
    t111 <- reactive({
      if (input$sd11ri == "Pará") {
        paste0(
          "Percentual de Nascidos Vivos com 7 ou Mais Consultas Pré-natal, Pará - ",
          input$sd11ano
        )
      } else{
        paste0(
          "Percentual de Nascidos Vivos com 7 ou Mais Consultas Pré-natal, Região de Integração ",
          input$sd11ri,
          " - ",
          input$sd11ano
        )
      }
    })
    ## Tabela - Percentual de Nascidos Vivos com 7 ou Mais Consultas Pré-natal----
    t112 <- reactive({
      if (input$sd11ri == "Pará") {
        paste0(
          "Percentual de Nascidos Vivos com 7 ou Mais Consultas Pré-natal por Município, Pará - ",
          input$sd11ano
        )
      } else{
        paste0(
          "Percentual de Nascidos Vivos com 7 ou Mais Consultas Pré-natal por Município, Região de Integração ",
          input$sd11ri,
          " - ",
          input$sd11ano
        )
      }
    })
    ## Gráfico - Percentual de Nascidos Vivos com 7 ou Mais Consultas Pré-natal----
    t113 <- reactive({
      paste0("Percentual de Nascidos Vivos com 7 ou Mais Consultas Pré-natal, Pará - ", min(sd11$ano), " a ", max(sd11$ano))
    })

    # 12-Percentual de Nascidos Vivos por Parto Normal----
    ## Mapa - Percentual de Nascidos Vivos por Parto Normal----
    t121 <- reactive({
      if (input$sd12ri == "Pará") {
        paste0(
          "Percentual de Nascidos Vivos por Parto Normal, Pará - ",
          input$sd12ano
        )
      } else{
        paste0(
          "Percentual de Nascidos Vivos por Parto Normal, Região de Integração ",
          input$sd12ri,
          " - ",
          input$sd12ano
        )
      }
    })
    ## Tabela - Percentual de Nascidos Vivos por Parto Normal----
    t122 <- reactive({
      if (input$sd12ri == "Pará") {
        paste0(
          "Percentual de Nascidos Vivos por Parto Normal por Município, Pará - ",
          input$sd12ano
        )
      } else{
        paste0(
          "Percentual de Nascidos Vivos por Parto Normal por Município, Região de Integração ",
          input$sd12ri,
          " - ",
          input$sd12ano
        )
      }
    })
    ## Gráfico - Percentual de Nascidos Vivos por Parto Normal----
    t123 <- reactive({
      paste0("Percentual de Nascidos Vivos por Parto Normal, Pará - ", min(sd12$ano), " a ", max(sd12$ano))
    })
  
    # 13-Percentual de Nascidos Vivos por Parto Cesário----
    ## Mapa - Percentual de Nascidos Vivos por Parto Cesário----
    t131 <- reactive({
      if (input$sd13ri == "Pará") {
        paste0(
          "Percentual de Nascidos Vivos por Parto Cesário, Pará - ",
          input$sd13ano
        )
      } else{
        paste0(
          "Percentual de Nascidos Vivos por Parto Cesário, Região de Integração ",
          input$sd13ri,
          " - ",
          input$sd13ano
        )
      }
    })
    ## Tabela - Percentual de Nascidos Vivos por Parto Cesário----
    t132 <- reactive({
      if (input$sd13ri == "Pará") {
        paste0(
          "Percentual de Nascidos Vivos por Parto Cesário por Município, Pará - ",
          input$sd13ano
        )
      } else{
        paste0(
          "Percentual de Nascidos Vivos por Parto Cesário por Município, Região de Integração ",
          input$sd13ri,
          " - ",
          input$sd13ano
        )
      }
    })
    ## Gráfico - Percentual de Nascidos Vivos por Parto Cesário----
    t133 <- reactive({
      paste0("Percentual de Nascidos Vivos por Parto Cesário, Pará - ", min(sd13$ano), " a ", max(sd13$ano))
    })

    # 14-Percentual de Nascidos Vivos de Mães Adolescentes na Faixa Etária de 10 a 19 Anos----
    ## Mapa - Percentual de Nascidos Vivos de Mães Adolescentes na Faixa Etária de 10 a 19 Anos----
    t141 <- reactive({
      if (input$sd14ri == "Pará") {
        paste0(
          "Percentual de Nascidos Vivos de Mães Adolescentes na Faixa Etária de 10 a 19 Anos, Pará - ",
          input$sd14ano
        )
      } else{
        paste0(
          "Percentual de Nascidos Vivos de Mães Adolescentes na Faixa Etária de 10 a 19 Anos, Região de Integração ",
          input$sd14ri,
          " - ",
          input$sd14ano
        )
      }
    })
    ## Tabela - Percentual de Nascidos Vivos de Mães Adolescentes na Faixa Etária de 10 a 19 Anos----
    t142 <- reactive({
      if (input$sd14ri == "Pará") {
        paste0(
          "Percentual de Nascidos Vivos de Mães Adolescentes na Faixa Etária de 10 a 19 Anos por Município, Pará - ",
          input$sd14ano
        )
      } else{
        paste0(
          "Percentual de Nascidos Vivos de Mães Adolescentes na Faixa Etária de 10 a 19 Anos por Município, Região de Integração ",
          input$sd14ri,
          " - ",
          input$sd14ano
        )
      }
    })
    ## Gráfico - Percentual de Nascidos Vivos de Mães Adolescentes na Faixa Etária de 10 a 19 Anos----
    t143 <- reactive({
      paste0("Percentual de Nascidos Vivos de Mães Adolescentes na Faixa Etária de 10 a 19 Anos, Pará - ", min(sd14$ano), " a ", max(sd14$ano))
    })

    # 15-Taxa de Incidência da Hanseníase----
    ## Mapa - Taxa de Incidência da Hanseníase----
    t151 <- reactive({
      if (input$sd15ri == "Pará") {
        paste0(
          "Taxa de Incidência da Hanseníase, Pará - ",
          input$sd15ano
        )
      } else{
        paste0(
          "Taxa de Incidência da Hanseníase, Região de Integração ",
          input$sd15ri,
          " - ",
          input$sd15ano
        )
      }
    })
    ## Tabela - Taxa de Incidência da Hanseníase----
    t152 <- reactive({
      if (input$sd15ri == "Pará") {
        paste0(
          "Taxa de Incidência da Hanseníase por Município, Pará - ",
          input$sd15ano
        )
      } else{
        paste0(
          "Taxa de Incidência da Hanseníase por Município, Região de Integração ",
          input$sd15ri,
          " - ",
          input$sd15ano
        )
      }
    })
    ## Gráfico - Taxa de Incidência da Hanseníase----
    t153 <- reactive({
      paste0("Taxa de Incidência da Hanseníase, Pará - ", min(sd15$ano), " a ", max(sd15$ano))
    })

    # 16-Taxa de Incidência da Tuberculose----
    ## Mapa - Taxa de Incidência da Tuberculose----
    t161 <- reactive({
      if (input$sd16ri == "Pará") {
        paste0(
          "Taxa de Incidência da Tuberculose, Pará - ",
          input$sd16ano
        )
      } else{
        paste0(
          "Taxa de Incidência da Tuberculose, Região de Integração ",
          input$sd16ri,
          " - ",
          input$sd16ano
        )
      }
    })
    ## Tabela - Taxa de Incidência da Tuberculose----
    t162 <- reactive({
      if (input$sd16ri == "Pará") {
        paste0(
          "Taxa de Incidência da Tuberculose por Município, Pará - ",
          input$sd16ano
        )
      } else{
        paste0(
          "Taxa de Incidência da Tuberculose por Município, Região de Integração ",
          input$sd16ri,
          " - ",
          input$sd16ano
        )
      }
    })
    ## Gráfico - Taxa de Incidência da Tuberculose----
    t163 <- reactive({
      paste0("Taxa de Incidência da Tuberculose, Pará - ", min(sd16$ano), " a ", max(sd16$ano))
    })

    # 17-Proporção de Mulheres de 25 a 64 anos que Realizaram Exames Citopatológicos do Colo do Útero----
    ## Mapa - Proporção de Mulheres de 25 a 64 anos que Realizaram Exames Citopatológicos do Colo do Útero----
    t171 <- reactive({
      if (input$sd17ri == "Pará") {
        paste0(
          "Proporção de Mulheres de 25 a 64 anos que Realizaram Exames Citopatológicos do Colo do Útero, Pará - ",
          input$sd17ano
        )
      } else{
        paste0(
          "Proporção de Mulheres de 25 a 64 anos que Realizaram Exames Citopatológicos do Colo do Útero, Região de Integração ",
          input$sd17ri,
          " - ",
          input$sd17ano
        )
      }
    })
    ## Tabela - Proporção de Mulheres de 25 a 64 anos que Realizaram Exames Citopatológicos do Colo do Útero----
    t172 <- reactive({
      if (input$sd17ri == "Pará") {
        paste0(
          "Proporção de Mulheres de 25 a 64 anos que Realizaram Exames Citopatológicos do Colo do Útero por Município, Pará - ",
          input$sd17ano
        )
      } else{
        paste0(
          "Proporção de Mulheres de 25 a 64 anos que Realizaram Exames Citopatológicos do Colo do Útero por Município, Região de Integração ",
          input$sd17ri,
          " - ",
          input$sd17ano
        )
      }
    })
    ## Gráfico - Proporção de Mulheres de 25 a 64 anos que Realizaram Exames Citopatológicos do Colo do Útero----
    t173 <- reactive({
      paste0("Proporção de Mulheres de 25 a 64 anos que Realizaram Exames Citopatológicos do Colo do Útero, Pará - ", min(sd17$ano), " a ", max(sd17$ano))
    })

    # 18-Óbitos por Residência Neoplasias (Tumores)----
    ## Mapa - Óbitos por Residência Neoplasias (Tumores)----
    t181 <- reactive({
      if (input$sd18ri == "Pará") {
        paste0(
          "Óbitos por Residência Neoplasias (Tumores), Pará - ",
          input$sd18ano
        )
      } else{
        paste0(
          "Óbitos por Residência Neoplasias (Tumores), Região de Integração ",
          input$sd18ri,
          " - ",
          input$sd18ano
        )
      }
    })
    ## Tabela - Óbitos por Residência Neoplasias (Tumores)----
    t182 <- reactive({
      if (input$sd18ri == "Pará") {
        paste0(
          "Óbitos por Residência Neoplasias (Tumores) por Município, Pará - ",
          input$sd18ano
        )
      } else{
        paste0(
          "Óbitos por Residência Neoplasias (Tumores) por Município, Região de Integração ",
          input$sd18ri,
          " - ",
          input$sd18ano
        )
      }
    })
    ## Gráfico - Óbitos por Residência Neoplasias (Tumores)----
    t183 <- reactive({
      paste0("Óbitos por Residência Neoplasias (Tumores), Pará - ", min(sd18$ano), " a ", max(sd18$ano))
    })
    
    # 19-Óbitos por Residência Algumas Doenças Infecciosas e Parasitárias----
    ## Mapa - Óbitos por Residência Algumas Doenças Infecciosas e Parasitárias----
    t191 <- reactive({
      if (input$sd19ri == "Pará") {
        paste0(
          "Óbitos por Residência Algumas Doenças Infecciosas e Parasitárias, Pará - ",
          input$sd19ano
        )
      } else{
        paste0(
          "Óbitos por Residência Algumas Doenças Infecciosas e Parasitárias, Região de Integração ",
          input$sd19ri,
          " - ",
          input$sd19ano
        )
      }
    })
    ## Tabela - Óbitos por Residência Algumas Doenças Infecciosas e Parasitárias----
    t192 <- reactive({
      if (input$sd19ri == "Pará") {
        paste0(
          "Óbitos por Residência Algumas Doenças Infecciosas e Parasitárias por Município, Pará - ",
          input$sd19ano
        )
      } else{
        paste0(
          "Óbitos por Residência Algumas Doenças Infecciosas e Parasitárias por Município, Região de Integração ",
          input$sd19ri,
          " - ",
          input$sd19ano
        )
      }
    })
    ## Gráfico - Óbitos por Residência Algumas Doenças Infecciosas e Parasitárias----
    t193 <- reactive({
      paste0("Óbitos por Residência Algumas Doenças Infecciosas e Parasitárias, Pará - ", min(sd19$ano), " a ", max(sd19$ano))
    })
    
    # 20-Óbitos por Residência Doenças do Aparelho Circulatório----
    ## Mapa - Óbitos por Residência Doenças do Aparelho Circulatório----
    t201 <- reactive({
      if (input$sd20ri == "Pará") {
        paste0(
          "Óbitos por Residência Doenças do Aparelho Circulatório, Pará - ",
          input$sd20ano
        )
      } else{
        paste0(
          "Óbitos por Residência Doenças do Aparelho Circulatório, Região de Integração ",
          input$sd20ri,
          " - ",
          input$sd20ano
        )
      }
    })
    ## Tabela - Óbitos por Residência Doenças do Aparelho Circulatório----
    t202 <- reactive({
      if (input$sd20ri == "Pará") {
        paste0(
          "Óbitos por Residência Doenças do Aparelho Circulatório por Município, Pará - ",
          input$sd20ano
        )
      } else{
        paste0(
          "Óbitos por Residência Doenças do Aparelho Circulatório por Município, Região de Integração ",
          input$sd20ri,
          " - ",
          input$sd20ano
        )
      }
    })
    ## Gráfico - Óbitos por Residência Doenças do Aparelho Circulatório----
    t203 <- reactive({
      paste0("Óbitos por Residência Doenças do Aparelho Circulatório, Pará - ", min(sd20$ano), " a ", max(sd20$ano))
    })

    # 21-Óbitos por Residência Doenças do Aparelho Digestivo----
    ## Mapa - Óbitos por Residência Doenças do Aparelho Digestivo----
    t211 <- reactive({
      if (input$sd21ri == "Pará") {
        paste0(
          "Óbitos por Residência Doenças do Aparelho Digestivo, Pará - ",
          input$sd21ano
        )
      } else{
        paste0(
          "Óbitos por Residência Doenças do Aparelho Digestivo, Região de Integração ",
          input$sd21ri,
          " - ",
          input$sd21ano
        )
      }
    })
    ## Tabela - Óbitos por Residência Doenças do Aparelho Digestivo----
    t212 <- reactive({
      if (input$sd21ri == "Pará") {
        paste0(
          "Óbitos por Residência Doenças do Aparelho Digestivo por Município, Pará - ",
          input$sd21ano
        )
      } else{
        paste0(
          "Óbitos por Residência Doenças do Aparelho Digestivo por Município, Região de Integração ",
          input$sd21ri,
          " - ",
          input$sd21ano
        )
      }
    })
    ## Gráfico - Óbitos por Residência Doenças do Aparelho Digestivo----
    t213 <- reactive({
      paste0("Óbitos por Residência Doenças do Aparelho Digestivo, Pará - ", min(sd21$ano), " a ", max(sd21$ano))
    })

    # 22-Óbitos por Residência Doenças do Aparelho Respiratório----
    ## Mapa - Óbitos por Residência Doenças do Aparelho Respiratório----
    t221 <- reactive({
      if (input$sd22ri == "Pará") {
        paste0(
          "Óbitos por Residência Doenças do Aparelho Respiratório, Pará - ",
          input$sd22ano
        )
      } else{
        paste0(
          "Óbitos por Residência Doenças do Aparelho Respiratório, Região de Integração ",
          input$sd22ri,
          " - ",
          input$sd22ano
        )
      }
    })
    ## Tabela - Óbitos por Residência Doenças do Aparelho Respiratório----
    t222 <- reactive({
      if (input$sd22ri == "Pará") {
        paste0(
          "Óbitos por Residência Doenças do Aparelho Respiratório por Município, Pará - ",
          input$sd22ano
        )
      } else{
        paste0(
          "Óbitos por Residência Doenças do Aparelho Respiratório por Município, Região de Integração ",
          input$sd22ri,
          " - ",
          input$sd22ano
        )
      }
    })
    ## Gráfico - Óbitos por Residência Doenças do Aparelho Respiratório----
    t223 <- reactive({
      paste0("Óbitos por Residência Doenças do Aparelho Respiratório, Pará - ", min(sd22$ano), " a ", max(sd22$ano))
    })

    # 23-Óbitos por Residência Algumas Afecções Originadas no Período Perinatal----
    ## Mapa - Óbitos por Residência Algumas Afecções Originadas no Período Perinatal----
    t231 <- reactive({
      if (input$sd23ri == "Pará") {
        paste0(
          "Óbitos por Residência Algumas Afecções Originadas no Período Perinatal, Pará - ",
          input$sd23ano
        )
      } else{
        paste0(
          "Óbitos por Residência Algumas Afecções Originadas no Período Perinatal, Região de Integração ",
          input$sd23ri,
          " - ",
          input$sd23ano
        )
      }
    })
    ## Tabela - Óbitos por Residência Algumas Afecções Originadas no Período Perinatal----
    t232 <- reactive({
      if (input$sd23ri == "Pará") {
        paste0(
          "Óbitos por Residência Algumas Afecções Originadas no Período Perinatal por Município, Pará - ",
          input$sd23ano
        )
      } else{
        paste0(
          "Óbitos por Residência Algumas Afecções Originadas no Período Perinatal por Município, Região de Integração ",
          input$sd23ri,
          " - ",
          input$sd23ano
        )
      }
    })
    ## Gráfico - Óbitos por Residência Algumas Afecções Originadas no Período Perinatal----
    t233 <- reactive({
      paste0("Óbitos por Residência Algumas Afecções Originadas no Período Perinatal, Pará - ", min(sd23$ano), " a ", max(sd23$ano))
    })

    # 24-Percentual de Óbitos por Infarto Agudo do Miocárdio (IAM)----
    ## Mapa - Percentual de Óbitos por Infarto Agudo do Miocárdio (IAM)----
    t241 <- reactive({
      if (input$sd24ri == "Pará") {
        paste0(
          "Percentual de Óbitos por Infarto Agudo do Miocárdio (IAM), Pará - ",
          input$sd24ano
        )
      } else{
        paste0(
          "Percentual de Óbitos por Infarto Agudo do Miocárdio (IAM), Região de Integração ",
          input$sd24ri,
          " - ",
          input$sd24ano
        )
      }
    })
    ## Tabela - Percentual de Óbitos por Infarto Agudo do Miocárdio (IAM)----
    t242 <- reactive({
      if (input$sd24ri == "Pará") {
        paste0(
          "Percentual de Óbitos por Infarto Agudo do Miocárdio (IAM) por Município, Pará - ",
          input$sd24ano
        )
      } else{
        paste0(
          "Percentual de Óbitos por Infarto Agudo do Miocárdio (IAM) por Município, Região de Integração ",
          input$sd24ri,
          " - ",
          input$sd24ano
        )}
    })
    ## Gráfico - Percentual de Óbitos por Infarto Agudo do Miocárdio (IAM)----
    t243 <- reactive({
      paste0("Percentual de Óbitos por Infarto Agudo do Miocárdio (IAM), Pará - ", min(sd24$ano), " a ", max(sd24$ano))
    })

    # 25-Taxa de Cobertura da Atenção Primária à Saúde----
    ## Mapa - Taxa de Cobertura da Atenção Primária à Saúde----
    t251 <- reactive({
      if (input$sd25ri == "Pará") {
        paste0(
          "Taxa de Cobertura da Atenção Primária à Saúde, Pará - ",
          input$sd25ano
        )
      } else{
        paste0(
          "Taxa de Cobertura da Atenção Primária à Saúde, Região de Integração ",
          input$sd25ri,
          " - ",
          input$sd25ano
        )
      }
    })
    ## Tabela - Taxa de Cobertura da Atenção Primária à Saúde----
    t252 <- reactive({
       if (input$sd25ri == "Pará") {
        paste0(
          "Taxa de Cobertura da Atenção Primária à Saúde por Município, Pará - ",
          input$sd25ano
        )
      } else{
        paste0(
          "Taxa de Cobertura da Atenção Primária à Saúde por Município, Região de Integração ",
          input$sd25ri,
          " - ",
          input$sd25ano
        )
      }
    })
    ## Gráfico - Taxa de Cobertura da Atenção Primária à Saúde----
    t253 <- reactive({
      paste0("Taxa de Cobertura da Atenção Primária à Saúde, Pará - ", min(sd25$ano), " a ", max(sd25$ano))
    })
    
    #VISUALIZAÇÃO----
    # 1-Taxa de Mortalidade Infantil----
    ## Mapa - Taxa de Mortalidade Infantil----
    output$sd1txt1 <- renderText({
    t11()  
    })

    output$sd1map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$sd1ri == "Pará") {
        df <- sd1 %>%
          filter(localidade != "Pará", ano == input$sd1ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x$valor[x$valor == 0] <- NA
      } else {
        df <- sd1 %>%
          filter(localidade != "Pará", ano == input$sd1ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$sd1ri)
        x$valor[x$valor == 0] <- NA
      }

      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
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

    ## Tabela - Taxa de Mortalidade Infantil----
    output$sd1txt2 <- renderText({
    t12() 
    })

    output$sd1tab <- renderReactable({
      if (input$sd1ri == "Pará") {
        x <- sd1 %>%
          filter(localidade != "Pará", ano == input$sd1ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- sd1 %>%
          filter(localidade != "Pará", ano == input$sd1ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$sd1ri)
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
          valor = colDef(name = "Taxa", format = colFormat(separators = T, digits = 2, locales = "pt-BR"))
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

    ## Gráfico - Taxa de Mortalidade Infantil----
    output$sd1txt3 <- renderText({
    t13()  
    })
    
    output$sd1graf <- renderEcharts4r({
      sd1 %>%
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

    # 2-Taxa de Mortalidade na Infância----
    ## Mapa - Taxa de Mortalidade na Infância----
    output$sd2txt1 <- renderText({
    t21()  
    })

    output$sd2map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$sd2ri == "Pará") {
        df <- sd2 %>%
          filter(localidade != "Pará", ano == input$sd2ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x$valor[x$valor == 0] <- NA
      } else {
        df <- sd2 %>%
          filter(localidade != "Pará", ano == input$sd2ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$sd2ri)
        x$valor[x$valor == 0] <- NA
      }

      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
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
          format(round(x$valor, digits = 0), big.mark = ".", decimal.mark = ",")
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

    ## Tabela - Taxa de Mortalidade na Infância----
    output$sd2txt2 <- renderText({
    t22() 
    })

    output$sd2tab <- renderReactable({
      if (input$sd2ri == "Pará") {
        x <- sd2 %>%
          filter(localidade != "Pará", ano == input$sd2ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- sd2 %>%
          filter(localidade != "Pará", ano == input$sd2ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$sd2ri)
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
              digit = 2,
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

    ## Gráfico - Taxa de Mortalidade na Infância----
    output$sd2txt3 <- renderText({
    t23() 
    })
    output$sd2graf <- renderEcharts4r({
      sd2 %>%
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

    # 3-Taxa de Mortalidade Materna----
    ## Mapa - Taxa de Mortalidade Materna----
    output$sd3txt1 <- renderText({
    t31()  
    })

    output$sd3map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$sd3ri == "Pará") {
        df <- sd3 %>%
          filter(localidade != "Pará", ano == input$sd3ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x$valor[x$valor == 0] <- NA
      } else {
        df <- sd3 %>%
          filter(localidade != "Pará", ano == input$sd3ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$sd3ri)
        x$valor[x$valor == 0] <- NA
      }

      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
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

    ## Tabela - Taxa de Mortalidade Materna----
    output$sd3txt2 <- renderText({
    t32()  
    })

    output$sd3tab <- renderReactable({
      if (input$sd3ri == "Pará") {
        x <- sd3 %>%
          filter(localidade != "Pará", ano == input$sd3ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- sd3 %>%
          filter(localidade != "Pará", ano == input$sd3ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$sd3ri)
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
              digit = 2,
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

    ## Gráfico - Taxa de Mortalidade Materna----
    output$sd3txt3 <- renderText({
    t33()  
    })
    output$sd3graf <- renderEcharts4r({
      sd3 %>%
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

    # 4-Taxa de Natalidade----
    ## Mapa - Taxa de Natalidade----
    output$sd4txt2 <- renderText({
    t41()  
    })

    output$sd4map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$sd4ri == "Pará") {
        df <- sd4 %>%
          filter(localidade != "Pará", ano == input$sd4ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x$valor[x$valor == 0] <- NA
      } else {
        df <- sd4 %>%
          filter(localidade != "Pará", ano == input$sd4ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$sd4ri)
        x$valor[x$valor == 0] <- NA
      }

      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
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

    ## Tabela - Taxa de Natalidade----
    output$sd4txt2 <- renderText({
    t42()  
    })

    output$sd4tab <- renderReactable({
      if (input$sd4ri == "Pará") {
        x <- sd4 %>%
          filter(localidade != "Pará", ano == input$sd4ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- sd4 %>%
          filter(localidade != "Pará", ano == input$sd4ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$sd4ri)
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
              digit = 2,
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

    ## Gráfico - Taxa de Natalidade----
    output$sd4txt3 <- renderText({
    t43()  
    })
    output$sd4graf <- renderEcharts4r({
      sd4 %>%
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

    # 5-Taxa de Mortalidade Geral----
    ## Mapa - Taxa de Mortalidade Geral----
    output$sd5txt1 <- renderText({
    t51()  
    })

    output$sd5map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$sd5ri == "Pará") {
        df <- sd5 %>%
          filter(localidade != "Pará", ano == input$sd5ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x$valor[x$valor == 0] <- NA
      } else {
        df <- sd5 %>%
          filter(localidade != "Pará", ano == input$sd5ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$sd5ri)
        x$valor[x$valor == 0] <- NA
      }

      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
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

    ## Tabela - Taxa de Mortalidade Geral----
    output$sd5txt2 <- renderText({
    t52() 
    })

    output$sd5tab <- renderReactable({
      if (input$sd5ri == "Pará") {
        x <- sd5 %>%
          filter(localidade != "Pará", ano == input$sd5ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- sd5 %>%
          filter(localidade != "Pará", ano == input$sd5ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$sd5ri)
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
              digit = 2,
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

    ## Gráfico - Taxa de Mortalidade Geral----
    output$sd5txt3 <- renderText({
    t53()  
    })
    output$sd5graf <- renderEcharts4r({
      sd5 %>%
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

    # 6-Taxa de Mortalidade por Sexo----
    ## Mapa - Taxa de Mortalidade por Sexo----
    output$sd6txt1 <- renderText({
    t61()  
    })

    output$sd6map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$sd6ri == "Pará") {
        df <- sd6 %>%
          filter(localidade != "Pará", ano == input$sd6ano, categoria == input$sd6cat) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x$valor[x$valor == 0] <- NA
      } else {
        df <- sd6 %>%
          filter(localidade != "Pará", ano == input$sd6ano, categoria == input$sd6cat) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$sd6ri)
        x$valor[x$valor == 0] <- NA
      }

      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
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

    ## Tabela - Taxa de Mortalidade por Sexo----
    output$sd6txt2 <- renderText({
    t62() 
    })

    output$sd6tab <- renderReactable({
      if (input$sd6ri == "Pará") {
        x <- sd6 %>%
          filter(localidade != "Pará", ano == input$sd6ano) %>%
          select(ri, localidade, Categoria, valor) %>%
          pivot_wider(names_from = Categoria, values_from = Valor)
      } else {
        x <- sd6 %>%
          filter(localidade != "Pará", ano == input$sd6ano) %>%
          select(ri, localidade, Categoria, valor) %>%
          pivot_wider(names_from = Categoria, values_from = Valor)
        x <- x %>% filter(ri == input$sd6ri)
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
          Masculino = colDef(format = colFormat(separators = T, digits = 2, locales = "pt-BR")),
          Feminino = colDef(format = colFormat(separators = T, digits = 2, locales = "pt-BR"))
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

    ## Gráfico - Taxa de Mortalidade por Sexo----
    output$sd6txt3 <- renderText({
    t63()  
    })
    output$sd6graf <- renderEcharts4r({
      sd6 %>%
        filter(localidade == "Pará") %>%
        pivot_wider(names_from = Categoria, values_from = Valor) %>%
        e_chart(x = ano) %>%
        e_line(
          serie = Masculino,
          name = "Masculino",
          legend = F,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
          itemStyle = list(barBorderRadius = 5)
        ) %>%
        e_line(
          serie = Feminino,
          name = "Feminino",
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

    # 7-Número de Hospitais----
    ## Mapa - Número de Hospitais----
    output$sd7txt1 <- renderText({
    t71() 
    })

    output$sd7map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$sd7ri == "Pará") {
        df <- sd7 %>%
          filter(localidade != "Pará", ano == input$sd7ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x$valor[x$valor == 0] <- NA
      } else {
        df <- sd7 %>%
          filter(localidade != "Pará", ano == input$sd7ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$sd7ri)
        x$valor[x$valor == 0] <- NA
      }

      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }

      pal <- colorBin(c("#FFEBCD", "#ffcd97", "#ffaf66", "#fa9236", "#f17701"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Número:</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
          format(round(x$valor, digits = 0), big.mark = ".", decimal.mark = ",")
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
          title = "Número",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 2)
        )
    })

    ## Tabela - Número de Hospitais----
    output$sd7txt2 <- renderText({
    t72() 
    })

    output$sd7tab <- renderReactable({
      if (input$sd7ri == "Pará") {
        x <- sd7 %>%
          filter(localidade != "Pará", ano == input$sd7ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- sd7 %>%
          filter(localidade != "Pará", ano == input$sd7ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$sd7ri)
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
            na = "-",
            name = "Número",
            format = colFormat(
              
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

    ## Gráfico - Número de Hospitais----
    output$sd7txt3 <- renderText({
    t73() 
    })
    output$sd7graf <- renderEcharts4r({
      sd7 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#f17701",
          name = "Número",
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
          name = "Número",
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

    # 8-Número de Postos e Centros de Saúde por 10.000 Habitantes----
    ## Mapa - Número de Postos e Centros de Saúde por 10.000 Habitantes----
    output$sd8txt1 <- renderText({
    t81() 
    })

    output$sd8map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$sd8ri == "Pará") {
        df <- sd8 %>%
          filter(localidade != "Pará", ano == input$sd8ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x$valor[x$valor == 0] <- NA
      } else {
        df <- sd8 %>%
          filter(localidade != "Pará", ano == input$sd8ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$sd8ri)
        x$valor[x$valor == 0] <- NA
      }

      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }

      pal <- colorBin(c("#FFEBCD", "#ffcd97", "#ffaf66", "#fa9236", "#f17701"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Número:</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
          format(round(x$valor, digits = 0), big.mark = ".", decimal.mark = ",")
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
          title = "Número",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 2)
        )
    })

    ## Tabela - Número de Postos e Centros de Saúde por 10.000 Habitantes----
    output$sd8txt2 <- renderText({
    t82() 
    })

    output$sd8tab <- renderReactable({
      if (input$sd8ri == "Pará") {
        x <- sd8 %>%
          filter(localidade != "Pará", ano == input$sd8ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- sd8 %>%
          filter(localidade != "Pará", ano == input$sd8ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$sd8ri)
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
            name = "Número",
            format = colFormat(
              
              separators = T,
              digit = 2,
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

    ## Gráfico - Número de Postos e Centros de Saúde por 10.000 Habitantes----
    output$sd8txt3 <- renderText({
    t83() 
    })
    
    output$sd8graf <- renderEcharts4r({
      sd8 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#f17701",
          name = "Número",
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
          name = "Número",
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

    # 9-Médicos por 10 Mil Habitantes----
    ## Mapa - Médicos por 10 Mil Habitantes----
    output$sd9txt1 <- renderText({
    t91() 
    })

    output$sd9map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$sd9ri == "Pará") {
        df <- sd9 %>%
          filter(localidade != "Pará", ano == input$sd9ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x$valor[x$valor == 0] <- NA
      } else {
        df <- sd9 %>%
          filter(localidade != "Pará", ano == input$sd9ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$sd9ri)
        x$valor[x$valor == 0] <- NA
      }

      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }

      pal <- colorBin(c("#FFEBCD", "#ffcd97", "#ffaf66", "#fa9236", "#f17701"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Nº Médicos:</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
          format(round(x$valor, digits = 0), big.mark = ".", decimal.mark = ",")
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
          title = "Nº Médicos",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 2)
        )
    })

    ## Tabela - Médicos por 10 Mil Habitantes----
    output$sd9txt2 <- renderText({
    t92() 
    })

    output$sd9tab <- renderReactable({
      if (input$sd9ri == "Pará") {
        x <- sd9 %>%
          filter(localidade != "Pará", ano == input$sd9ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- sd9 %>%
          filter(localidade != "Pará", ano == input$sd9ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$sd9ri)
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
            name = "Nº Médicos",
            format = colFormat(
              
              separators = T,
              digit = 2,
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

    ## Gráfico - Médicos por 10 Mil Habitantes----
    output$sd9txt3 <- renderText({
    t93() 
    })
    
    output$sd9graf <- renderEcharts4r({
      sd9 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#f17701",
          name = "Número",
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
          name = "Número",
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

    # 10-Leito Hospitalar por Mil Habitantes----
    ## Mapa - Leito Hospitalar por Mil Habitantes----
    output$sd10txt1 <- renderText({
    t101()  
    })

    output$sd10map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$sd10ri == "Pará") {
        df <- sd10 %>%
          filter(localidade != "Pará", ano == input$sd10ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x$valor[x$valor == 0] <- NA
      } else {
        df <- sd10 %>%
          filter(localidade != "Pará", ano == input$sd10ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$sd10ri)
        x$valor[x$valor == 0] <- NA
      }

      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }

      pal <- colorBin(c("#FFEBCD", "#ffcd97", "#ffaf66", "#fa9236", "#f17701"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Nº Leitos:</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
          format(round(x$valor, digits = 0), big.mark = ".", decimal.mark = ",")
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
          title = "Nº Leitos",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 2)
        )
    })

    ## Tabela - Leito Hospitalar por Mil Habitantes----
    output$sd10txt2 <- renderText({
    t102()  
    })

    output$sd10tab <- renderReactable({
      if (input$sd10ri == "Pará") {
        x <- sd10 %>%
          filter(localidade != "Pará", ano == input$sd10ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- sd10 %>%
          filter(localidade != "Pará", ano == input$sd10ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$sd10ri)
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
            name = "Nº Leitos",
            format = colFormat(
              
              separators = T,
              digit = 2,
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

    ## Gráfico - Leito Hospitalar por Mil Habitantes----
    output$sd10txt3 <- renderText({
    t103() 
    })
    
    output$sd10graf <- renderEcharts4r({
      sd10 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#f17701",
          name = "Número",
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
          name = "Número",
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

    # 11-Percentual de Nascidos Vivos com 7 ou Mais Consultas Pré-natal----
    ## Mapa - Percentual de Nascidos Vivos com 7 ou Mais Consultas Pré-natal----
    output$sd11txt1 <- renderText({
    t111() 
    })

    output$sd11map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$sd11ri == "Pará") {
        df <- sd11 %>%
          filter(localidade != "Pará", ano == input$sd11ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x$valor[x$valor == 0] <- NA
      } else {
        df <- sd11 %>%
          filter(localidade != "Pará", ano == input$sd11ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$sd11ri)
        x$valor[x$valor == 0] <- NA
      }

      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }

      pal <- colorBin(c("#FFEBCD", "#ffcd97", "#ffaf66", "#fa9236", "#f17701"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Percentual(%%):</b> %s",
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
          title = "Percentual(%)",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 2)
        )
    })

    ## Tabela - Percentual de Nascidos Vivos com 7 ou Mais Consultas Pré-natal----
    output$sd11txt2 <- renderText({
    t112() 
    })

    output$sd11tab <- renderReactable({
      if (input$sd11ri == "Pará") {
        x <- sd11 %>%
          filter(localidade != "Pará", ano == input$sd11ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- sd11 %>%
          filter(localidade != "Pará", ano == input$sd11ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$sd11ri)
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
            name = "Percentual(%)",
            format = colFormat(
              separators = T,
              digit = 2,
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

    ## Gráfico - Percentual de Nascidos Vivos com 7 ou Mais Consultas Pré-natal----
    output$sd11txt3 <- renderText({
    t113() 
    })
    output$sd11graf <- renderEcharts4r({
      sd11 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#f17701",
          name = "Percentual(%)",
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
          name = "Percentual(%)",
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

    # 12-Percentual de Nascidos Vivos por Parto Normal----
    ## Mapa - Percentual de Nascidos Vivos por Parto Normal----
    output$sd12txt1 <- renderText({
    t121() 
    })

    output$sd12map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$sd12ri == "Pará") {
        df <- sd12 %>%
          filter(localidade != "Pará", ano == input$sd12ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x$valor[x$valor == 0] <- NA
      } else {
        df <- sd12 %>%
          filter(localidade != "Pará", ano == input$sd12ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$sd12ri)
        x$valor[x$valor == 0] <- NA
      }

      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }

      pal <- colorBin(c("#FFEBCD", "#ffcd97", "#ffaf66", "#fa9236", "#f17701"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Percentual(%%):</b> %s",
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
          title = "Percentual(%)",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 2)
        )
    })

    ## Tabela - Percentual de Nascidos Vivos por Parto Normal----
    output$sd12txt2 <- renderText({
    t122() 
    })

    output$sd12tab <- renderReactable({
      if (input$sd12ri == "Pará") {
        x <- sd12 %>%
          filter(localidade != "Pará", ano == input$sd12ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- sd12 %>%
          filter(localidade != "Pará", ano == input$sd12ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$sd12ri)
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
            name = "Percentual(%)",
            format = colFormat(
              separators = T,
              digit = 2,
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

    ## Gráfico - Percentual de Nascidos Vivos por Parto Normal----
    output$sd12txt3 <- renderText({
    t123() 
    })
    
    output$sd12graf <- renderEcharts4r({
      sd12 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#f17701",
          name = "Percentual(%)",
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
          name = "Valor(R$)",
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

    # 13-Percentual de Nascidos Vivos por Parto Cesário----
    ## Mapa - Percentual de Nascidos Vivos por Parto Cesário----
    output$sd13txt1 <- renderText({
    t131() 
    })

    output$sd13map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$sd13ri == "Pará") {
        df <- sd13 %>%
          filter(localidade != "Pará", ano == input$sd13ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x$valor[x$valor == 0] <- NA
      } else {
        df <- sd13 %>%
          filter(localidade != "Pará", ano == input$sd13ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$sd13ri)
        x$valor[x$valor == 0] <- NA
      }

      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }

      pal <- colorBin(c("#FFEBCD", "#ffcd97", "#ffaf66", "#fa9236", "#f17701"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Percentual(%%):</b> %s",
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
          title = "Percentual(%)",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 2)
        )
    })

    ## Tabela - Percentual de Nascidos Vivos por Parto Cesário----
    output$sd13txt2 <- renderText({
    t132() 
    })

    output$sd13tab <- renderReactable({
      if (input$sd13ri == "Pará") {
        x <- sd13 %>%
          filter(localidade != "Pará", ano == input$sd13ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- sd13 %>%
          filter(localidade != "Pará", ano == input$sd13ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$sd13ri)
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
            name = "Percentual(%)",
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

    ## Gráfico - Percentual de Nascidos Vivos por Parto Cesário----
    output$sd13txt3 <- renderText({
    t133() 
    })
    output$sd13graf <- renderEcharts4r({
      sd13 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#f17701",
          name = "Percentual(%)",
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
          name = "Percentual(%)",
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

    # 14-Percentual de Nascidos Vivos de Mães Adolescentes na Faixa Etária de 10 a 19 Anos----
    ## Mapa - Percentual de Nascidos Vivos de Mães Adolescentes na Faixa Etária de 10 a 19 Anos----
    output$sd14txt1 <- renderText({
    t141() 
    })

    output$sd14map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$sd14ri == "Pará") {
        df <- sd14 %>%
          filter(localidade != "Pará", ano == input$sd14ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x$valor[x$valor == 0] <- NA
      } else {
        df <- sd14 %>%
          filter(localidade != "Pará", ano == input$sd14ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$sd14ri)
        x$valor[x$valor == 0] <- NA
      }

      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }

      pal <- colorBin(c("#FFEBCD", "#ffcd97", "#ffaf66", "#fa9236", "#f17701"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Percentual(%%):</b> %s",
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
          title = "Percentual(%)",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 2)
        )
    })

    ## Tabela - Percentual de Nascidos Vivos de Mães Adolescentes na Faixa Etária de 10 a 19 Anos----
    output$sd14txt2 <- renderText({
    t142() 
    })

    output$sd14tab <- renderReactable({
      if (input$sd14ri == "Pará") {
        x <- sd14 %>%
          filter(localidade != "Pará", ano == input$sd14ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- sd14 %>%
          filter(localidade != "Pará", ano == input$sd14ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$sd14ri)
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
            name = "Percentual(%)",
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

    ## Gráfico - Percentual de Nascidos Vivos de Mães Adolescentes na Faixa Etária de 10 a 19 Anos----
    output$sd14txt3 <- renderText({
    t143() 
    })
    output$sd14graf <- renderEcharts4r({
      sd14 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#f17701",
          name = "Percentual(%)",
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
          name = "Percentual(%)",
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

    # 15-Taxa de Incidência da Hanseníase----
    ## Mapa - Taxa de Incidência da Hanseníase----
    output$sd15txt1 <- renderText({
    t151() 
    })

    output$sd15map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$sd15ri == "Pará") {
        df <- sd15 %>%
          filter(localidade != "Pará", ano == input$sd15ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x$valor[x$valor == 0] <- NA
      } else {
        df <- sd15 %>%
          filter(localidade != "Pará", ano == input$sd15ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$sd15ri)
        x$valor[x$valor == 0] <- NA
      }

      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
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

    ## Tabela - Taxa de Incidência da Hanseníase----
    output$sd15txt2 <- renderText({
    t152() 
    })

    output$sd15tab <- renderReactable({
      if (input$sd15ri == "Pará") {
        x <- sd15 %>%
          filter(localidade != "Pará", ano == input$sd15ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- sd15 %>%
          filter(localidade != "Pará", ano == input$sd15ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$sd15ri)
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
            na = "-",
            name = "Taxa",
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

    ## Gráfico - Taxa de Incidência da Hanseníase----
    output$sd15txt3 <- renderText({
    t153()  
    })
    output$sd15graf <- renderEcharts4r({
      sd15 %>%
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

    # 16-Taxa de Incidência da Tuberculose----
    ## Mapa - Taxa de Incidência da Tuberculose----
    output$sd16txt1 <- renderText({
    t161() 
    })

    output$sd16map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$sd16ri == "Pará") {
        df <- sd16 %>%
          filter(localidade != "Pará", ano == input$sd16ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x$valor[x$valor == 0] <- NA
      } else {
        df <- sd16 %>%
          filter(localidade != "Pará", ano == input$sd16ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$sd16ri)
        x$valor[x$valor == 0] <- NA
      }

      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
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

    ## Tabela - Taxa de Incidência da Tuberculose----
    output$sd16txt2 <- renderText({
    t162() 
    })

    output$sd16tab <- renderReactable({
      if (input$sd16ri == "Pará") {
        x <- sd16 %>%
          filter(localidade != "Pará", ano == input$sd16ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- sd16 %>%
          filter(localidade != "Pará", ano == input$sd16ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$sd16ri)
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
            na = "-",
            name = "Taxa",
            format = colFormat(
              separators = T,
              digit = 2,
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

    ## Gráfico - Taxa de Incidência da Tuberculose----
    output$sd16txt3 <- renderText({
    t163() 
    })
    output$sd16graf <- renderEcharts4r({
      sd16 %>%
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

    # 17-Proporção de Mulheres de 25 a 64 anos que Realizaram Exames Citopatológicos do Colo do Útero----
    ## Mapa - Proporção de Mulheres de 25 a 64 anos que Realizaram Exames Citopatológicos do Colo do Útero----
    output$sd17txt1 <- renderText({
    t171() 
    })

    output$sd17map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$sd17ri == "Pará") {
        df <- sd17 %>%
          filter(localidade != "Pará", ano == input$sd17ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x$valor[x$valor == 0] <- NA
      } else {
        df <- sd17 %>%
          filter(localidade != "Pará", ano == input$sd17ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$sd17ri)
        x$valor[x$valor == 0] <- NA
      }

      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }

      pal <- colorBin(c("#FFEBCD", "#ffcd97", "#ffaf66", "#fa9236", "#f17701"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Proporção:</b> %s",
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
          title = "Proporção",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 2)
        )
    })

    ## Tabela - Proporção de Mulheres de 25 a 64 anos que Realizaram Exames Citopatológicos do Colo do Útero----
    output$sd17txt2 <- renderText({
    t172() 
    })

    output$sd17tab <- renderReactable({
      if (input$sd17ri == "Pará") {
        x <- sd17 %>%
          filter(localidade != "Pará", ano == input$sd17ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- sd17 %>%
          filter(localidade != "Pará", ano == input$sd17ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$sd17ri)
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
            name = "Proporção",
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

    ## Gráfico - Proporção de Mulheres de 25 a 64 anos que Realizaram Exames Citopatológicos do Colo do Útero----
    output$sd17txt3 <- renderText({
    t173() 
    })
    output$sd17graf <- renderEcharts4r({
      sd17 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#f17701",
          name = "Proporção",
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
          name = "Proporção",
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

    # 18-Óbitos por Residência Neoplasias (Tumores)----
    ## Mapa - Óbitos por Residência Neoplasias (Tumores)----
    output$sd18txt1 <- renderText({
    t181() 
    })

    output$sd18map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$sd18ri == "Pará") {
        df <- sd18 %>%
          filter(localidade != "Pará", ano == input$sd18ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x$valor[x$valor == 0] <- NA
      } else {
        df <- sd18 %>%
          filter(localidade != "Pará", ano == input$sd18ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$sd18ri)
        x$valor[x$valor == 0] <- NA
      }

      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }

      pal <- colorBin(c("#FFEBCD", "#ffcd97", "#ffaf66", "#fa9236", "#f17701"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Nº Óbitos:</b> %s",
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
          title = "Nº Óbitos",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 2)
        )
    })

    ## Tabela - Óbitos por Residência Neoplasias (Tumores)----
    output$sd18txt2 <- renderText({
    t182() 
    })

    output$sd18tab <- renderReactable({
      if (input$sd18ri == "Pará") {
        x <- sd18 %>%
          filter(localidade != "Pará", ano == input$sd18ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- sd18 %>%
          filter(localidade != "Pará", ano == input$sd18ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$sd18ri)
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
            na = "-",
            name = "Nº Óbitos",
            format = colFormat(
              
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

    ## Gráfico - Óbitos por Residência Neoplasias (Tumores)----
    output$sd18txt3 <- renderText({
    t183() 
    })
    
    output$sd18graf <- renderEcharts4r({
      sd18 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#f17701",
          name = "Número",
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
          name = "Número",
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

    # 19-Óbitos por Residência Algumas Doenças Infecciosas e Parasitárias----
    ## Mapa - Óbitos por Residência Algumas Doenças Infecciosas e Parasitárias----
    output$sd19txt1 <- renderText({
    t191() 
    })

    output$sd19map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$sd19ri == "Pará") {
        df <- sd19 %>%
          filter(localidade != "Pará", ano == input$sd19ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x$valor[x$valor == 0] <- NA
      } else {
        df <- sd19 %>%
          filter(localidade != "Pará", ano == input$sd19ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$sd19ri)
        x$valor[x$valor == 0] <- NA
      }

      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }

      pal <- colorBin(c("#FFEBCD", "#ffcd97", "#ffaf66", "#fa9236", "#f17701"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Nº Óbitos:</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
          format(round(x$valor, digits = 0), big.mark = ".", decimal.mark = ",")
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
          title = "Nº Óbitos",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 0)
        )
    })

    ## Tabela - Óbitos por Residência Algumas Doenças Infecciosas e Parasitárias----
    output$sd19txt2 <- renderText({
    t192() 
    })

    output$sd19tab <- renderReactable({
      if (input$sd19ri == "Pará") {
        x <- sd19 %>%
          filter(localidade != "Pará", ano == input$sd19ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- sd19 %>%
          filter(localidade != "Pará", ano == input$sd19ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$sd19ri)
      }
      x %>% reactable(
        defaultPageSize = 10, striped = FALSE, highlight = TRUE, bordered = TRUE, outlined = TRUE, resizable = TRUE, showSortable = TRUE, pagination = F, columns = list(
          ri = colDef(name = "Região de Integração"),
          localidade = colDef(name = "Municípios"),
          valor = colDef(
            na = "-",
            name = "Nº Óbitos",
            format = colFormat(separators = T, locales = "pt-BR")
          )
        ), defaultColDef = colDef(
          footerStyle = list(fontWeight = "bold"),
          headerStyle = list(background = "#f7f7f8")
        ), language = reactableLang(
          noData = "Sem informação",
          pageInfo = "{rowStart} a {rowEnd} de {rows} linhas",
          pagePrevious = "Anterior",
          pageNext = "Próximo",
          pagePreviousLabel = "Anterior",
          pageNextLabel = "Proximo"
        )
      )
    })

    ## Gráfico - Óbitos por Residência Algumas Doenças Infecciosas e Parasitárias----
    output$sd19txt3 <- renderText({
    t193() 
    })
    
    output$sd19graf <- renderEcharts4r({
      sd19 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#f17701",
          name = "Número",
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
          name = "Número",
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

    # 20-Óbitos por Residência Doenças do Aparelho Circulatório----
    ## Mapa - Óbitos por Residência Doenças do Aparelho Circulatório----
    output$sd20txt1 <- renderText({
    t201() 
    })

    output$sd20map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$sd20ri == "Pará") {
        df <- sd20 %>%
          filter(localidade != "Pará", ano == input$sd20ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x$valor[x$valor == 0] <- NA
      } else {
        df <- sd20 %>%
          filter(localidade != "Pará", ano == input$sd20ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$sd20ri)
        x$valor[x$valor == 0] <- NA
      }

      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }

      pal <- colorBin(c("#FFEBCD", "#ffcd97", "#ffaf66", "#fa9236", "#f17701"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Nº Óbitos:</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
          format(round(x$valor, digits = 0), big.mark = ".", decimal.mark = ",")
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
          title = "Nº Óbitos",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 0)
        )
    })

    ## Tabela - Óbitos por Residência Doenças do Aparelho Circulatório----
    output$sd20txt2 <- renderText({
    t202() 
    })

    output$sd20tab <- renderReactable({
      if (input$sd20ri == "Pará") {
        x <- sd20 %>%
          filter(localidade != "Pará", ano == input$sd20ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- sd20 %>%
          filter(localidade != "Pará", ano == input$sd20ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$sd20ri)
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
            na = "-",
            name = "Nº Óbitos",
            format = colFormat(
              
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

    ## Gráfico - Óbitos por Residência Doenças do Aparelho Circulatório----
    output$sd20txt3 <- renderText({
    t203() 
    })
    output$sd20graf <- renderEcharts4r({
      sd20 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#f17701",
          name = "Número",
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
          name = "Número",
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

    # 21-Óbitos por Residência Doenças do Aparelho Digestivo----
    ## Mapa - Óbitos por Residência Doenças do Aparelho Digestivo----
    output$sd21txt1 <- renderText({
    t211() 
    })

    output$sd21map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$sd21ri == "Pará") {
        df <- sd21 %>%
          filter(localidade != "Pará", ano == input$sd21ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x$valor[x$valor == 0] <- NA
      } else {
        df <- sd21 %>%
          filter(localidade != "Pará", ano == input$sd21ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$sd21ri)
        x$valor[x$valor == 0] <- NA
      }

      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }

      pal <- colorBin(c("#FFEBCD", "#ffcd97", "#ffaf66", "#fa9236", "#f17701"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Nº Óbitos:</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
          format(round(x$valor, digits = 0), big.mark = ".", decimal.mark = ",")
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
          title = "Nº Óbitos",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 0)
        )
    })

    ## Tabela - Óbitos por Residência Doenças do Aparelho Digestivo----
    output$sd21txt2 <- renderText({
    t212() 
    })

    output$sd21tab <- renderReactable({
      if (input$sd21ri == "Pará") {
        x <- sd21 %>%
          filter(localidade != "Pará", ano == input$sd21ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- sd21 %>%
          filter(localidade != "Pará", ano == input$sd21ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$sd21ri)
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
            na = "-",
            name = "Nº Óbitos",
            format = colFormat(
              
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

    ## Gráfico - Óbitos por Residência Doenças do Aparelho Digestivo----
    output$sd21txt3 <- renderText({
    t213() 
    })
    output$sd21graf <- renderEcharts4r({
      sd21 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#f17701",
          name = "Número",
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
          name = "Número",
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

    # 22-Óbitos por Residência Doenças do Aparelho Respiratório----
    ## Mapa - Óbitos por Residência Doenças do Aparelho Respiratório----
    output$sd22txt1 <- renderText({
    t221() 
    })

    output$sd22map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$sd22ri == "Pará") {
        df <- sd22 %>%
          filter(localidade != "Pará", ano == input$sd22ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x$valor[x$valor == 0] <- NA
      } else {
        df <- sd22 %>%
          filter(localidade != "Pará", ano == input$sd22ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$sd22ri)
        x$valor[x$valor == 0] <- NA
      }

      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }

      pal <- colorBin(c("#FFEBCD", "#ffcd97", "#ffaf66", "#fa9236", "#f17701"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Nº Óbitos:</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
          format(round(x$valor, digits = 0), big.mark = ".", decimal.mark = ",")
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
          title = "Nº Óbitos",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 0)
        )
    })

    ## Tabela - Óbitos por Residência Doenças do Aparelho Respiratório----
    output$sd22txt2 <- renderText({
    t222() 
    })

    output$sd22tab <- renderReactable({
      if (input$sd22ri == "Pará") {
        x <- sd22 %>%
          filter(localidade != "Pará", ano == input$sd22ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- sd22 %>%
          filter(localidade != "Pará", ano == input$sd22ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$sd22ri)
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
            na = "-",
            name = "Nº Óbitos",
            format = colFormat(
              
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

    ## Gráfico - Óbitos por Residência Doenças do Aparelho Respiratório----
    output$sd22txt3 <- renderText({
    t223() 
    })
    output$sd22graf <- renderEcharts4r({
      sd22 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#f17701",
          name = "Número",
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
          name = "Número",
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

    # 23-Óbitos por Residência Algumas Afecções Originadas no Período Perinatal----
    ## Mapa - Óbitos por Residência Algumas Afecções Originadas no Período Perinatal----
    output$sd23txt1 <- renderText({
    t231() 
    })

    output$sd23map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$sd23ri == "Pará") {
        df <- sd23 %>%
          filter(localidade != "Pará", ano == input$sd23ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x$valor[x$valor == 0] <- NA
      } else {
        df <- sd23 %>%
          filter(localidade != "Pará", ano == input$sd23ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$sd23ri)
        x$valor[x$valor == 0] <- NA
      }

      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }

      pal <- colorBin(c("#FFEBCD", "#ffcd97", "#ffaf66", "#fa9236", "#f17701"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Nº Óbitos:</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
          format(round(x$valor, digits = 0), big.mark = ".", decimal.mark = ",")
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
          title = "Nº Óbitos",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 2)
        )
    })

    ## Tabela - Óbitos por Residência Algumas Afecções Originadas no Período Perinatal----
    output$sd23txt2 <- renderText({
    t232() 
    })

    output$sd23tab <- renderReactable({
      if (input$sd23ri == "Pará") {
        x <- sd23 %>%
          filter(localidade != "Pará", ano == input$sd23ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- sd23 %>%
          filter(localidade != "Pará", ano == input$sd23ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$sd23ri)
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
            name = "Nº Óbitos",
            na = "-",
            format = colFormat(
              
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

    ## Gráfico - Óbitos por Residência Algumas Afecções Originadas no Período Perinatal----
    output$sd23txt3 <- renderText({
    t233() 
    })
    output$sd23graf <- renderEcharts4r({
      sd23 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#f17701",
          name = "Número",
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
          name = "Número",
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

    # 24-Percentual de Óbitos por Infarto Agudo do Miocárdio (IAM)----
    ## Mapa - Percentual de Óbitos por Infarto Agudo do Miocárdio (IAM)----
    output$sd24txt1 <- renderText({
    t241() 
    })

    output$sd24map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$sd24ri == "Pará") {
        df <- sd24 %>%
          filter(localidade != "Pará", ano == input$sd24ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x$valor[x$valor == 0] <- NA
      } else {
        df <- sd24 %>%
          filter(localidade != "Pará", ano == input$sd24ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$sd24ri)
        x$valor[x$valor == 0] <- NA
      }

      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }

      pal <- colorBin(c("#FFEBCD", "#ffcd97", "#ffaf66", "#fa9236", "#f17701"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Percentual(%%):</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
          format(round(x$valor, digits = 0), big.mark = ".", decimal.mark = ",")
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
          title = "Percentual(%)",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 2)
        )
    })

    ## Tabela - Percentual de Óbitos por Infarto Agudo do Miocárdio (IAM)----
    output$sd24txt2 <- renderText({
    t242() 
    })

    output$sd24tab <- renderReactable({
      if (input$sd24ri == "Pará") {
        x <- sd24 %>%
          filter(localidade != "Pará", ano == input$sd24ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- sd24 %>%
          filter(localidade != "Pará", ano == input$sd24ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$sd24ri)
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
            na = "-",
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

    ## Gráfico - Percentual de Óbitos por Infarto Agudo do Miocárdio (IAM)----
    output$sd24txt3 <- renderText({
    t243() 
    })
    output$sd24graf <- renderEcharts4r({
      sd24 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#f17701",
          name = "Percentual(%)",
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
          name = "Percentual(%)",
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

    # 25-Taxa de Cobertura da Atenção Primária à Saúde----
    ## Mapa - Taxa de Cobertura da Atenção Primária à Saúde----
    output$sd25txt1 <- renderText({
    t251() 
    })

    output$sd25map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$sd25ri == "Pará") {
        df <- sd25 %>%
          filter(localidade != "Pará", ano == input$sd25ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x$valor[x$valor == 0] <- NA
      } else {
        df <- sd25 %>%
          filter(localidade != "Pará", ano == input$sd25ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$sd25ri)
        x$valor[x$valor == 0] <- NA
      }

      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
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

    ## Tabela - Taxa de Cobertura da Atenção Primária à Saúde----
    output$sd25txt2 <- renderText({
    t252() 
    })

    output$sd25tab <- renderReactable({
      if (input$sd25ri == "Pará") {
        x <- sd25 %>%
          filter(localidade != "Pará", ano == input$sd25ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- sd25 %>%
          filter(localidade != "Pará", ano == input$sd25ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$sd25ri)
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

    ## Gráfico - Taxa de Cobertura da Atenção Primária à Saúde----
    output$sd25txt3 <- renderText({
    t253() 
    })
    
    output$sd25graf <- renderEcharts4r({
      sd25 %>%
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
    # 1-Taxa de Mortalidade Infantil----
    ## Tabela - Taxa de Mortalidade Infantil----
    # Filtra os dados
    sd1_1 <- reactive({
      if (input$sd1ri == "Pará") {
        x <- sd1 %>%
          filter(localidade != "Pará", 
                 ano == input$sd1ano)
      } else {
        x <- sd1 %>%
          filter(localidade != "Pará", 
                 ano == input$sd1ano,
                 ri == input$sd1ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd1_1(), {
      downset_Server("sd1_1", sd1_1(), t12())
    })
    ## Gráfico - Taxa de Mortalidade Infantil----
    # Filtra os dados
    sd1_2 <- reactive({
      sd1 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd1_2(), {
      downset_Server("sd1_2", sd1_2(), t13())
    })

    # 2-Taxa de Mortalidade na Infância----
    ## Tabela - Taxa de Mortalidade na Infância----
    # Filtra os dados
    sd2_1 <- reactive({
      if (input$sd2ri == "Pará") {
        x <- sd2 %>%
          filter(localidade != "Pará",
                 ano == input$sd2ano)
      } else {
        x <- sd2 %>%
          filter(localidade != "Pará", 
                 ano == input$sd2ano,
                 ri == input$sd2ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd2_1(), {
      downset_Server("sd2_1", sd2_1(), t22())
    })
    ## Gráfico - Taxa de Mortalidade na Infância----
    # Filtra os dados
    sd2_2 <- reactive({
      sd2 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd2_2(), {
      downset_Server("sd2_2", sd2_2(), t23())
    })

    # 3-Taxa de Mortalidade Materna----
    ## Tabela - Taxa de Mortalidade Materna----
    # Filtra os dados
    sd3_1 <- reactive({
      if (input$sd3ri == "Pará") {
        x <- sd3 %>%
          filter(localidade != "Pará",
                 ano == input$sd3ano)
      } else {
        x <- sd3 %>%
          filter(localidade != "Pará",
                 ano == input$sd3ano,
                 ri == input$sd3ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd3_1(), {
      downset_Server("sd3_1", sd3_1(), t32())
    })
    ## Gráfico - Taxa de Mortalidade Materna----
    # Filtra os dados
    sd3_2 <- reactive({
      sd3 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd3_2(), {
      downset_Server("sd3_2", sd3_2(), t33())
    })

    # 4-Taxa de Natalidade----
    ## Tabela - Taxa de Natalidade----
    # Filtra os dados
    sd4_1 <- reactive({
      if (input$sd4ri == "Pará") {
        x <- sd4 %>%
          filter(localidade != "Pará",
                 ano == input$sd4ano) 
      } else {
        x <- sd4 %>%
          filter(localidade != "Pará",
                 ano == input$sd4ano,
                 ri == input$sd4ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd4_1(), {
      downset_Server("sd4_1", sd4_1(), t42())
    })
    ## Gráfico - Taxa de Natalidade----
    # Filtra os dados
    sd4_2 <- reactive({
      sd4 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd4_2(), {
      downset_Server("sd4_2", sd4_2(), t43())
    })

    # 5-Taxa de Mortalidade Geral----
    ## Tabela - axa de Mortalidade Geral----
    # Filtra os dados
    sd5_1 <- reactive({
      if (input$sd5ri == "Pará") {
        x <- sd5 %>%
          filter(localidade != "Pará",
                 ano == input$sd5ano) 
      } else {
        x <- sd5 %>%
          filter(localidade != "Pará",
                 ano == input$sd5ano,
                 ri == input$sd5ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd5_1(), {
      downset_Server("sd5_1", sd5_1(), t52())
    })
    ## Gráfico - axa de Mortalidade Geral----
    # Filtra os dados
    sd5_2 <- reactive({
      sd5 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd5_2(), {
      downset_Server("sd5_2", sd5_2(), t53())
    })

    # 6-Taxa de Mortalidade por Sexo----
    ## Tabela - Taxa de Mortalidade por Sexo----
    # Filtra os dados
    sd6_1 <- reactive({
      if (input$sd6ri == "Pará") {
        x <- sd6 %>%
          filter(localidade != "Pará",
                 ano == input$sd6ano)
      } else {
        x <- sd6 %>%
          filter(localidade != "Pará",
                 ano == input$sd6ano,
                 ri == input$sd6ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd6_1(), {
      downset_Server("sd6_1", sd6_1(), t62())
    })
    ## Gráfico - Taxa de Mortalidade por Sexo----
    # Filtra os dados
    sd6_2 <- reactive({
      sd6 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd6_2(), {
      downset_Server("sd6_2", sd6_2(), t63())
    })

    # 7-Número de Hospitais----
    ## Tabela - Número de Hospitais----
    # Filtra os dados
    sd7_1 <- reactive({
      if (input$sd7ri == "Pará") {
        x <- sd7 %>%
          filter(localidade != "Pará",
                 ano == input$sd7ano)
      } else {
        x <- sd7 %>%
          filter(localidade != "Pará",
                 ano == input$sd7ano,
                 ri == input$sd7ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd7_1(), {
      downset_Server("sd7_1", sd7_1(), t72())
    })
    ## Gráfico - Número de Hospitais----
    # Filtra os dados
    sd7_2 <- reactive({
      sd7 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd7_2(), {
      downset_Server("sd7_2", sd7_2(), t73())
    })

    # 8-Número de Postos e Centros de Saúde por 10.000 Habitantes----
    ## Tabela - Número de Postos e Centros de Saúde por 10.000 Habitantes----
    # Filtra os dados
    sd8_1 <- reactive({
      if (input$sd8ri == "Pará") {
        x <- sd8 %>%
          filter(localidade != "Pará",
                 ano == input$sd8ano)
      } else {
        x <- sd8 %>%
          filter(localidade != "Pará",
                 ano == input$sd8ano,
                 ri == input$sd8ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd8_1(), {
      downset_Server("sd8_1", sd8_1(), t82())
    })
    ## Gráfico - Número de Postos e Centros de Saúde por 10.000 Habitantes----
    # Filtra os dados
    sd8_2 <- reactive({
      sd8 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd8_2(), {
      downset_Server("sd8_2", sd8_2(), t83())
    })

    # 9-Médicos por 10 Mil Habitantes----
    ## Tabela - Médicos por 10 Mil Habitantes----
    # Filtra os dados
    sd9_1 <- reactive({
      if (input$sd9ri == "Pará") {
        x <- sd9 %>%
          filter(localidade != "Pará",
                 ano == input$sd9ano)
      } else {
        x <- sd9 %>%
          filter(localidade != "Pará",
                 ano == input$sd9ano,
                 ri == input$sd9ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd9_1(), {
      downset_Server("sd9_1", sd9_1(), t92())
    })
    ## Gráfico - Médicos por 10 Mil Habitantes----
    # Filtra os dados
    sd9_2 <- reactive({
      sd9 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd9_2(), {
      downset_Server("sd9_2", sd9_2(), t93())
    })

    # 10-Leito Hospitalar por Mil Habitantes----
    ## Tabela - Leito Hospitalar por Mil Habitantes----
    # Filtra os dados
    sd10_1 <- reactive({
      if (input$sd10ri == "Pará") {
        x <- sd10 %>%
          filter(localidade != "Pará", 
                 ano == input$sd10ano)
      } else {
        x <- sd10 %>%
          filter(localidade != "Pará",
                 ano == input$sd10ano,
                 ri == input$sd10ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd10_1(), {
      downset_Server("sd10_1", sd10_1(), t102())
    })
    ## Gráfico - Leito Hospitalar por Mil Habitantes----
    # Filtra os dados
    sd10_2 <- reactive({
      sd10 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd10_2(), {
      downset_Server("sd10_2", sd10_2(), t103())
    })

    # 11-Percentual de Nascidos Vivos com 7 ou Mais Consultas Pré-natal----
    ## Tabela - Percentual de Nascidos Vivos com 7 ou Mais Consultas Pré-natal----
    # Filtra os dados
    sd11_1 <- reactive({
      if (input$sd11ri == "Pará") {
        x <- sd11 %>%
          filter(localidade != "Pará",
                 ano == input$sd11ano) 
      } else {
        x <- sd11 %>%
          filter(localidade != "Pará",
                 ano == input$sd11ano,
                 ri == input$sd11ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd11_1(), {
      downset_Server("sd11_1", sd11_1(), t112())
    })
    ## Gráfico - Percentual de Nascidos Vivos com 7 ou Mais Consultas Pré-natal----
    # Filtra os dados
    sd11_2 <- reactive({
      sd11 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd11_2(), {
      downset_Server("sd11_2", sd11_2(), t113())
    })

    # 12-Percentual de Nascidos Vivos por Parto Normal----
    ## Tabela - Percentual de Nascidos Vivos por Parto Normal----
    # Filtra os dados
    sd12_1 <- reactive({
      if (input$sd12ri == "Pará") {
        x <- sd12 %>%
          filter(localidade != "Pará",
                 ano == input$sd12ano)
      } else {
        x <- sd12 %>%
          filter(localidade != "Pará",
                 ano == input$sd12ano,
                 ri == input$sd12ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd12_1(), {
      downset_Server("sd12_1", sd12_1(), t122())
    })
    ## Gráfico - Percentual de Nascidos Vivos por Parto Normal----
    # Filtra os dados
    sd12_2 <- reactive({
      sd12 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd12_2(), {
      downset_Server("sd12_2", sd12_2(), t123())
    })

    # 13-Percentual de Nascidos Vivos por Parto Cesário----
    ## Tabela - Percentual de Nascidos Vivos por Parto Cesário----
    # Filtra os dados
    sd13_1 <- reactive({
      if (input$sd13ri == "Pará") {
        x <- sd13 %>%
          filter(localidade != "Pará",
                 ano == input$sd13ano) 
      } else {
        x <- sd13 %>%
          filter(localidade != "Pará",
                 ano == input$sd13ano,
                 ri == input$sd13ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd13_1(), {
      downset_Server("sd13_1", sd13_1(), t132())
    })
    ## Gráfico - Percentual de Nascidos Vivos por Parto Cesário----
    # Filtra os dados
    sd13_2 <- reactive({
      sd13 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd13_2(), {
      downset_Server("sd13_2", sd13_2(), t133())
    })

    # 14-Percentual de Nascidos Vivos de Mães Adolescentes na Faixa Etária de 10 a 19 Anos----
    ## Tabela - Percentual de Nascidos Vivos de Mães Adolescentes na Faixa Etária de 10 a 19 Anos----
    # Filtra os dados
    sd14_1 <- reactive({
      if (input$sd14ri == "Pará") {
        x <- sd14 %>%
          filter(localidade != "Pará",
                 ano == input$sd14ano)
      } else {
        x <- sd14 %>%
          filter(localidade != "Pará",
                 ano == input$sd14ano,
                 ri == input$sd14ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd14_1(), {
      downset_Server("sd14_1", sd14_1(), t142())
    })
    ## Gráfico - Percentual de Nascidos Vivos de Mães Adolescentes na Faixa Etária de 10 a 19 Anos----
    # Filtra os dados
    sd14_2 <- reactive({
      sd14 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd14_2(), {
      downset_Server("sd14_2", sd14_2(), t143())
    })

    # 15-Taxa de Incidência da Hanseníase----
    ## Tabela - Taxa de Incidência da Hanseníase----
    # Filtra os dados
    sd15_1 <- reactive({
      if (input$sd15ri == "Pará") {
        x <- sd15 %>%
          filter(localidade != "Pará",
                 ano == input$sd15ano) 
      } else {
        x <- sd15 %>%
          filter(localidade != "Pará",
                 ano == input$sd15ano,
                 ri == input$sd15ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd15_1(), {
      downset_Server("sd15_1", sd15_1(), t152())
    })
    ## Gráfico - Taxa de Incidência da Hanseníase----
    # Filtra os dados
    sd15_2 <- reactive({
      sd15 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd15_2(), {
      downset_Server("sd15_2", sd15_2(), t153())
    })

    # 16-Taxa de Incidência da Tuberculose----
    ## Tabela - Taxa de Incidência da Tuberculose----
    # Filtra os dados
    sd16_1 <- reactive({
      if (input$sd16ri == "Pará") {
        x <- sd16 %>%
          filter(localidade != "Pará",
                 ano == input$sd16ano) 
      } else {
        x <- sd16 %>%
          filter(localidade != "Pará",
                 ano == input$sd16ano,
                 ri == input$sd16ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd16_1(), {
      downset_Server("sd16_1", sd16_1(), t162())
    })
    ## Gráfico - Taxa de Incidência da Tuberculose----
    # Filtra os dados
    sd16_2 <- reactive({
      sd16 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd16_2(), {
      downset_Server("sd16_2", sd16_2(), t163())
    })

    # 17-Proporção de Mulheres de 25 a 64 anos que Realizaram Exames Citopatológicos do Colo do Útero----
    ## Tabela - Proporção de Mulheres de 25 a 64 anos que Realizaram Exames Citopatológicos do Colo do Útero----
    # Filtra os dados
    sd17_1 <- reactive({
      if (input$sd17ri == "Pará") {
        x <- sd17 %>%
          filter(localidade != "Pará",
                 ano == input$sd17ano) 
      } else {
        x <- sd17 %>%
          filter(localidade != "Pará",
                 ano == input$sd17ano,
                 ri == input$sd17ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd17_1(), {
      downset_Server("sd17_1", sd17_1(), t172())
    })
    ## Gráfico - Proporção de Mulheres de 25 a 64 anos que Realizaram Exames Citopatológicos do Colo do Útero----
    # Filtra os dados
    sd17_2 <- reactive({
      sd17 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd17_2(), {
      downset_Server("sd17_2", sd17_2(), t173())
    })

    # 18-Óbitos por Residência Neoplasias (Tumores)----
    ## Tabela - Óbitos por Residência Neoplasias (Tumores)----
    # Filtra os dados
    sd18_1 <- reactive({
      if (input$sd18ri == "Pará") {
        x <- sd18 %>%
          filter(localidade != "Pará",
                 ano == input$sd18ano)
      } else {
        x <- sd18 %>%
          filter(localidade != "Pará",
                 ano == input$sd18ano,
                 ri == input$sd18ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd18_1(), {
      downset_Server("sd18_1", sd18_1(), t182())
    })
    ## Gráfico - Óbitos por Residência Neoplasias (Tumores)----
    # Filtra os dados
    sd18_2 <- reactive({
      sd18 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd18_2(), {
      downset_Server("sd18_2", sd18_2(), t183())
    })

    # 19-Óbitos por Residência Algumas Doenças Infecciosas e Parasitárias----
    ## Tabela - Óbitos por Residência Algumas Doenças Infecciosas e Parasitárias----
    # Filtra os dados
    sd19_1 <- reactive({
      if (input$sd19ri == "Pará") {
        x <- sd19 %>%
          filter(localidade != "Pará",
                 ano == input$sd19ano) 
      } else {
        x <- sd19 %>%
          filter(localidade != "Pará",
                 ano == input$sd19ano,
                 ri == input$sd19ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd19_1(), {
      downset_Server("sd19_1", sd19_1(), t192())
    })
    ## Gráfico - Óbitos por Residência Algumas Doenças Infecciosas e Parasitárias----
    # Filtra os dados
    sd19_2 <- reactive({
      sd19 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd19_2(), {
      downset_Server("sd19_2", sd19_2(), t193())
    })

    # 20-Óbitos por Residência Doenças do Aparelho Circulatório----
    ## Tabela - Óbitos por Residência Doenças do Aparelho Circulatório----
    # Filtra os dados
    sd20_1 <- reactive({
      if (input$sd20ri == "Pará") {
        x <- sd20 %>%
          filter(localidade != "Pará",
                 ano == input$sd20ano) 
      } else {
        x <- sd20 %>%
          filter(localidade != "Pará",
                 ano == input$sd20ano,
                 ri == input$sd20ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd20_1(), {
      downset_Server("sd20_1", sd20_1(), t202())
    })
    ## Gráfico - Óbitos por Residência Doenças do Aparelho Circulatório----
    # Filtra os dados
    sd20_2 <- reactive({
      sd20 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd20_2(), {
      downset_Server("sd20_2", sd20_2(), t203())
    })

    # 21-Óbitos por Residência Doenças do Aparelho Digestivo----
    ## Tabela - Óbitos por Residência Doenças do Aparelho Digestivo----
    # Filtra os dados
    sd21_1 <- reactive({
      if (input$sd21ri == "Pará") {
        x <- sd21 %>%
          filter(localidade != "Pará",
                 ano == input$sd21ano)
      } else {
        x <- sd21 %>%
          filter(localidade != "Pará",
                 ano == input$sd21ano,
                 ri == input$sd21ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd20_1(), {
      downset_Server("sd21_1", sd21_1(), t212())
    })
    ## Gráfico - Óbitos por Residência Doenças do Aparelho Digestivo----
    # Filtra os dados
    sd21_2 <- reactive({
      sd21 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd20_2(), {
      downset_Server("sd21_2", sd21_2(), t213())
    })

    # 22-Óbitos por Residência Doenças do Aparelho Respiratório----
    ## Tabela - Óbitos por Residência Doenças do Aparelho Respiratório----
    # Filtra os dados
    sd22_1 <- reactive({
      if (input$sd22ri == "Pará") {
        x <- sd22 %>%
          filter(localidade != "Pará",
                 ano == input$sd22ano)
      } else {
        x <- sd22 %>%
          filter(localidade != "Pará",
                 ano == input$sd22ano,
                 ri == input$sd22ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd22_1(), {
      downset_Server("sd22_1", sd22_1(), t222())
    })
    ## Gráfico - Óbitos por Residência Doenças do Aparelho Respiratório----
    # Filtra os dados
    sd22_2 <- reactive({
      sd22 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd22_2(), {
      downset_Server("sd22_2", sd22_2(), t223())
    })

    # 23-Óbitos por Residência Algumas Afecções Originadas no Período Perinatal----
    ## Tabela - Óbitos por Residência Algumas Afecções Originadas no Período Perinatal----
    # Filtra os dados
    sd23_1 <- reactive({
      if (input$sd23ri == "Pará") {
        x <- sd23 %>%
          filter(localidade != "Pará",
                 ano == input$sd23ano)
      } else {
        x <- sd23 %>%
          filter(localidade != "Pará",
                 ano == input$sd23ano,
                 ri == input$sd23ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd23_1(), {
      downset_Server("sd23_1", sd23_1(), t232())
    })
    ## Gráfico - Óbitos por Residência Algumas Afecções Originadas no Período Perinatal----
    # Filtra os dados
    sd23_2 <- reactive({
      sd23 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd23_2(), {
      downset_Server("sd23_2", sd23_2(), t233())
    })

    # 24-Percentual de Óbitos por Infarto Agudo do Miocárdio (IAM)----
    ## Tabela - Percentual de Óbitos por Infarto Agudo do Miocárdio (IAM)----
    # Filtra os dados
    sd24_1 <- reactive({
      if (input$sd24ri == "Pará") {
        x <- sd24 %>%
          filter(localidade != "Pará",
                 ano == input$sd24ano)
      } else {
        x <- sd24 %>%
          filter(localidade != "Pará",
                 ano == input$sd24ano,
                 ri == input$sd24ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd24_1(), {
      downset_Server("sd24_1", sd24_1(), t242())
    })
    ## Gráfico - Percentual de Óbitos por Infarto Agudo do Miocárdio (IAM)----
    # Filtra os dados
    sd24_2 <- reactive({
      sd24 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd24_2(), {
      downset_Server("sd24_2", sd24_2(), t243())
    })

    # 25-Taxa de Cobertura da Atenção Primária à Saúde----
    ## Tabela - Taxa de Cobertura da Atenção Primária à Saúde----
    # Filtra os dados
    sd25_1 <- reactive({
      if (input$sd25ri == "Pará") {
        x <- sd25 %>%
          filter(localidade != "Pará",
                 ano == input$sd25ano) 
      } else {
        x <- sd25 %>%
          filter(localidade != "Pará",
                 ano == input$sd25ano,
                 ri == input$sd25ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd25_1(), {
      downset_Server("sd25_1", sd25_1(), t252())
    })
    ## Gráfico - Taxa de Cobertura da Atenção Primária à Saúde----
    # Filtra os dados
    sd25_2 <- reactive({
      sd25 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd25_2(), {
      downset_Server("sd25_2", sd25_2(), t253())
    })
  })
}

# # Play do Módulo
# ui <- dashboardPage(
#   header = dashboardHeader(),
#   sidebar = dashboardSidebar(),
#   body = dashboardBody(fluidPage(social_saude_pa_ui("social_saude_pa")))
# )
# 
# 
# server <- function(input, output) {
#   social_saude_pa_Server("social_saude_pa")
# }
# 
# shinyApp(ui, server)
