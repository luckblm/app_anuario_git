# Funções de módulo de Social - Saúde - Municipal
# Função de UI
social_saude_mp_ui <- function(id) {
  fluidPage( # CAMINHO DO ARQUIVO CSS----
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "estilo.css")
    ),
    div(
      class = "navbar_social",
      navbarPage(
        tags$b("Saúde - Municípios"),
        navbarMenu(
          tags$b("Escolha um Indicador"),
          # 1-Taxa de Mortalidade Infantil----
          tabPanel(
            "Taxa de Mortalidade Infantil",
            panel(
              ## Controle----
              heading =
              h4(
                style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Taxa de Mortalidade Infantil"
              ),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "sd1muni"),
                  label = "Município",
                  choices = sd1 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Taxa de Mortalidade Infantil----
              box(
                title = textOutput(NS(id, "sd1txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "sd1municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
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
                  downset_ui(NS(id, "sd1_1"))
                )
              ),
              ## Tabela - Taxa de Mortalidade Infantil----
              box(
                title = textOutput(NS(id, "sd1txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "sd1ano"),
                  label = "Ano",
                  choices = sort(unique(sd1[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "sd1tab2")),
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
              h4(
                style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Taxa de Mortalidade na Infância"
              ),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "sd2muni"),
                  label = "Município",
                  choices = sd2 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Taxa de Mortalidade na Infância----
              box(
                title = textOutput(NS(id, "sd2txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "sd2municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
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
                  downset_ui(NS(id, "sd2_1"))
                )
              ),

              ## Tabela - Taxa de Mortalidade na Infância----
              box(
                title = textOutput(NS(id, "sd2txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "sd2ano"),
                  label = "Ano",
                  choices = sort(unique(sd2[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "sd2tab2")),
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
              h4(
                style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Taxa de Mortalidade Materna"
              ),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "sd3muni"),
                  label = "Município",
                  choices = sd3 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Taxa de Mortalidade Materna----
              box(
                title = textOutput(NS(id, "sd3txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "sd3municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
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
                  downset_ui(NS(id, "sd3_1"))
                )
              ),

              ## Tabela - Taxa de Mortalidade Materna----
              box(
                title = textOutput(NS(id, "sd3txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "sd3ano"),
                  label = "Ano",
                  choices = sort(unique(sd3[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "sd3tab2")),
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
              h4(
                style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Taxa de Natalidade"
              ),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "sd4muni"),
                  label = "Município",
                  choices = sd4 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Taxa de Natalidade----
              box(
                title = textOutput(NS(id, "sd4txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "sd4municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
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
                  downset_ui(NS(id, "sd4_1"))
                )
              ),

              ## Tabela - Taxa de Natalidade----
              box(
                title = textOutput(NS(id, "sd4txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "sd4ano"),
                  label = "Ano",
                  choices = sort(unique(sd4[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "sd4tab2")),
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
              h4(
                style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Taxa de Mortalidade Geral"
              ),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "sd5muni"),
                  label = "Município",
                  choices = sd5 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Taxa de Mortalidade Geral----
              box(
                title = textOutput(NS(id, "sd5txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "sd5municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
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
                  downset_ui(NS(id, "sd5_1"))
                )
              ),

              ## Tabela - Taxa de Mortalidade Geral----
              box(
                title = textOutput(NS(id, "sd5txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "sd5ano"),
                  label = "Ano",
                  choices = sort(unique(sd5[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "sd5tab2")),
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
              h4(
                style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Taxa de Mortalidade por Sexo"
              ),
              tags$div(
                class = "seletor1",
                # select Município
                pickerInput(
                  inputId = NS(id, "sd6muni"),
                  label = "Município",
                  choices = sd6 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Taxa de Mortalidade por Sexo----
              box(
                title = textOutput(NS(id, "sd6txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                column(
                  2,
                  pickerInput(
                    inputId = NS(id, "sd6municomp"),
                    label = "Comparar Município",
                    choices = NULL,
                    width = "200px",
                    options = list(`none-selected-text` = "Selecione um município")
                  )
                ),
                column(
                  6,
                  pickerInput(
                    inputId = NS(id, "sd6ano"),
                    label = "Ano",
                    choices = sort(unique(sd6[["ano"]]), decreasing = T),
                    width = "100px"
                  )
                ),
                column(
                  12,
                  withSpinner(
                    echarts4rOutput(NS(id, "sd6graf")),
                    type = 8,
                    color = "#f17701",
                    size = 0.5
                  )
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DATASUS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd6_1"))
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
                  reactableOutput(NS(id, "sd6tab")),
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
                  downset_ui(NS(id, "sd6_2"))
                )
              ),
              ## Tabela - Taxa de Mortalidade por Sexo----
              box(
                title = textOutput(NS(id, "sd6txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "sd6ano2"),
                  label = "Ano",
                  choices = sort(unique(sd6[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "sd6tab1")),
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
                  downset_ui(NS(id, "sd6_3"))
                )
              )
            )
          ),

          # 7-Número de Hospitais----
          tabPanel(
            "Número de Hospitais",
            panel(
              ## Controle----
              heading =
              h4(
                style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Número de Hospitais"
              ),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "sd7muni"),
                  label = "Município",
                  choices = sd7 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Número de Hospitais----
              box(
                title = textOutput(NS(id, "sd7txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "sd7municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
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
                  downset_ui(NS(id, "sd7_1"))
                )
              ),

              ## Tabela - Número de Hospitais----
              box(
                title = textOutput(NS(id, "sd7txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "sd7ano"),
                  label = "Ano",
                  choices = sort(unique(sd7[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "sd7tab2")),
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
              h4(
                style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Número de Postos e Centros de Saúde por 10.000 Habitantes"
              ),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "sd8muni"),
                  label = "Município",
                  choices = sd8 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Número de Postos e Centros de Saúde por 10.000 Habitantes----
              box(
                title = textOutput(NS(id, "sd8txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "sd8municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
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
                  downset_ui(NS(id, "sd8_1"))
                )
              ),

              ## Tabela - Número de Postos e Centros de Saúde por 10.000 Habitantes----
              box(
                title = textOutput(NS(id, "sd8txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "sd8ano"),
                  label = "Ano",
                  choices = sort(unique(sd8[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "sd8tab2")),
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
              h4(
                style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Médicos por 10 Mil Habitantes"
              ),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "sd9muni"),
                  label = "Município",
                  choices = sd9 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Médicos por 10 Mil Habitantes----
              box(
                title = textOutput(NS(id, "sd9txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "sd9municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
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
                  downset_ui(NS(id, "sd9_1"))
                )
              ),

              ## Tabela - Médicos por 10 Mil Habitantes----
              box(
                title = textOutput(NS(id, "sd9txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "sd9ano"),
                  label = "Ano",
                  choices = sort(unique(sd9[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "sd9tab2")),
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
              h4(
                style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Leito Hospitalar por Mil Habitantes"
              ),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "sd10muni"),
                  label = "Município",
                  choices = sd10 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Leito Hospitalar por Mil Habitantes----
              box(
                title = textOutput(NS(id, "sd10txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "sd10municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
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
                  downset_ui(NS(id, "sd10_1"))
                )
              ),

              ## Tabela - Leito Hospitalar por Mil Habitantes----
              box(
                title = textOutput(NS(id, "sd10txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "sd10ano"),
                  label = "Ano",
                  choices = sort(unique(sd10[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "sd10tab2")),
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
              h4(
                style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Percentual de Nascidos Vivos com 7 ou Mais Consultas Pré-natal"
              ),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "sd11muni"),
                  label = "Município",
                  choices = sd11 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Percentual de Nascidos Vivos com 7 ou Mais Consultas Pré-natal----
              box(
                title = textOutput(NS(id, "sd11txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "sd11municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
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
                  downset_ui(NS(id, "sd11_1"))
                )
              ),

              ## Tabela - Percentual de Nascidos Vivos com 7 ou Mais Consultas Pré-natal----
              box(
                title = textOutput(NS(id, "sd11txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "sd11ano"),
                  label = "Ano",
                  choices = sort(unique(sd11[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "sd11tab2")),
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
              h4(
                style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Percentual de Nascidos Vivos por Parto Normal"
              ),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "sd12muni"),
                  label = "Município",
                  choices = sd12 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Percentual de Nascidos Vivos por Parto Normal----
              box(
                title = textOutput(NS(id, "sd12txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "sd12municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
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
                  downset_ui(NS(id, "sd12_1"))
                )
              ),

              ## Tabela - Percentual de Nascidos Vivos por Parto Normal----
              box(
                title = textOutput(NS(id, "sd12txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "sd12ano"),
                  label = "Ano",
                  choices = sort(unique(sd12[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "sd12tab2")),
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
              h4(
                style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Percentual de Nascidos Vivos por Parto Cesário"
              ),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "sd13muni"),
                  label = "Município",
                  choices = sd13 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Percentual de Nascidos Vivos por Parto Cesário----
              box(
                title = textOutput(NS(id, "sd13txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "sd13municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
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
                  downset_ui(NS(id, "sd13_1"))
                )
              ),

              ## Tabela - Percentual de Nascidos Vivos por Parto Cesário----
              box(
                title = textOutput(NS(id, "sd13txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "sd13ano"),
                  label = "Ano",
                  choices = sort(unique(sd13[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "sd13tab2")),
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
              h4(
                style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Percentual de Nascidos Vivos de Mães Adolescentes na Faixa Etária de 10 a 19 Anos"
              ),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "sd14muni"),
                  label = "Município",
                  choices = sd14 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Percentual de Nascidos Vivos de Mães Adolescentes na Faixa Etária de 10 a 19 Anos----
              box(
                title = textOutput(NS(id, "sd14txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "sd14municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
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
                  downset_ui(NS(id, "sd14_1"))
                )
              ),
              ## Tabela - Percentual de Nascidos Vivos de Mães Adolescentes na Faixa Etária de 10 a 19 Anos----
              box(
                title = textOutput(NS(id, "sd14txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "sd14ano"),
                  label = "Ano",
                  choices = sort(unique(sd14[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "sd14tab2")),
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
              h4(
                style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Taxa de Incidência da Hanseníase"
              ),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "sd15muni"),
                  label = "Município",
                  choices = sd15 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Taxa de Incidência da Hanseníase----
              box(
                title = textOutput(NS(id, "sd15txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "sd15municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
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
                  downset_ui(NS(id, "sd15_1"))
                )
              ),

              ## Tabela - Taxa de Incidência da Hanseníase----
              box(
                title = textOutput(NS(id, "sd15txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "sd15ano"),
                  label = "Ano",
                  choices = sort(unique(sd15[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "sd15tab2")),
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
              h4(
                style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Taxa de Incidência da Tuberculose"
              ),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "sd16muni"),
                  label = "Município",
                  choices = sd16 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Taxa de Incidência da Tuberculose----
              box(
                title = textOutput(NS(id, "sd16txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "sd16municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
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
                  downset_ui(NS(id, "sd16_1"))
                )
              ),

              ## Tabela - Taxa de Incidência da Tuberculose----
              box(
                title = textOutput(NS(id, "sd16txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "sd16ano"),
                  label = "Ano",
                  choices = sort(unique(sd16[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "sd16tab2")),
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
              h4(
                style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Proporção de Mulheres de 25 a 64 anos que Realizaram Exames Citopatológicos do Colo do Útero"
              ),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "sd17muni"),
                  label = "Município",
                  choices = sd17 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Proporção de Mulheres de 25 a 64 anos que Realizaram Exames Citopatológicos do Colo do Útero----
              box(
                title = textOutput(NS(id, "sd17txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "sd17municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
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
                  downset_ui(NS(id, "sd17_1"))
                )
              ),

              ## Tabela - Proporção de Mulheres de 25 a 64 anos que Realizaram Exames Citopatológicos do Colo do Útero----
              box(
                title = textOutput(NS(id, "sd17txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "sd17ano"),
                  label = "Ano",
                  choices = sort(unique(sd17[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "sd17tab2")),
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
              h4(
                style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Óbitos por Residência Neoplasias (Tumores)"
              ),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "sd18muni"),
                  label = "Município",
                  choices = sd18 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Óbitos por Residência Neoplasias (Tumores)----
              box(
                title = textOutput(NS(id, "sd18txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "sd18municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
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
                  downset_ui(NS(id, "sd18_1"))
                )
              ),

              ## Tabela - Óbitos por Residência Neoplasias (Tumores)----
              box(
                title = textOutput(NS(id, "sd18txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "sd18ano"),
                  label = "Ano",
                  choices = sort(unique(sd18[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "sd18tab2")),
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
              h4(
                style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Óbitos por Residência Algumas Doenças Infecciosas e Parasitárias"
              ),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "sd19muni"),
                  label = "Município",
                  choices = sd19 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Óbitos por Residência Algumas Doenças Infecciosas e Parasitárias----
              box(
                title = textOutput(NS(id, "sd19txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "sd19municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
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
                  downset_ui(NS(id, "sd19_1"))
                )
              ),

              ## Tabela - Óbitos por Residência Algumas Doenças Infecciosas e Parasitárias----
              box(
                title = textOutput(NS(id, "sd19txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "sd19ano"),
                  label = "Ano",
                  choices = sort(unique(sd19[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "sd19tab2")),
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
              h4(
                style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Óbitos por Residência Doenças do Aparelho Circulatório"
              ),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "sd20muni"),
                  label = "Município",
                  choices = sd20 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Óbitos por Residência Doenças do Aparelho Circulatório----
              box(
                title = textOutput(NS(id, "sd20txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "sd20municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
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
                  downset_ui(NS(id, "sd20_1"))
                )
              ),

              ## Tabela - Óbitos por Residência Doenças do Aparelho Circulatório----
              box(
                title = textOutput(NS(id, "sd20txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "sd20ano"),
                  label = "Ano",
                  choices = sort(unique(sd20[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "sd20tab2")),
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
              h4(
                style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Óbitos por Residência Doenças do Aparelho Digestivo"
              ),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "sd21muni"),
                  label = "Município",
                  choices = sd21 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Óbitos por Residência Doenças do Aparelho Digestivo----
              box(
                title = textOutput(NS(id, "sd21txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "sd21municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
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
                  downset_ui(NS(id, "sd21_1"))
                )
              ),

              ## Tabela - Óbitos por Residência Doenças do Aparelho Digestivo----
              box(
                title = textOutput(NS(id, "sd21txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "sd21ano"),
                  label = "Ano",
                  choices = sort(unique(sd21[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "sd21tab2")),
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
              h4(
                style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Óbitos por Residência Doenças do Aparelho Respiratório"
              ),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "sd22muni"),
                  label = "Município",
                  choices = sd22 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Óbitos por Residência Doenças do Aparelho Respiratório----
              box(
                title = textOutput(NS(id, "sd22txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "sd22municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
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
                  downset_ui(NS(id, "sd22_1"))
                )
              ),

              ## Tabela - Óbitos por Residência Doenças do Aparelho Respiratório----
              box(
                title = textOutput(NS(id, "sd22txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "sd22ano"),
                  label = "Ano",
                  choices = sort(unique(sd22[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "sd22tab2")),
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
              h4(
                style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Óbitos por Residência Algumas Afecções Originadas no Período Perinatal"
              ),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "sd23muni"),
                  label = "Município",
                  choices = sd23 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Óbitos por Residência Algumas Afecções Originadas no Período Perinatal----
              box(
                title = textOutput(NS(id, "sd23txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "sd23municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
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
                  downset_ui(NS(id, "sd23_1"))
                )
              ),

              ## Tabela - Óbitos por Residência Algumas Afecções Originadas no Período Perinatal----
              box(
                title = textOutput(NS(id, "sd23txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "sd23ano"),
                  label = "Ano",
                  choices = sort(unique(sd23[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "sd23tab2")),
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
              h4(
                style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Percentual de Óbitos por Infarto Agudo do Miocárdio (IAM)"
              ),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "sd24muni"),
                  label = "Município",
                  choices = sd24 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Percentual de Óbitos por Infarto Agudo do Miocárdio (IAM)----
              box(
                title = textOutput(NS(id, "sd24txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "sd24municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
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
                  downset_ui(NS(id, "sd24_1"))
                )
              ),

              ## Tabela - Percentual de Óbitos por Infarto Agudo do Miocárdio (IAM)----
              box(
                title = textOutput(NS(id, "sd24txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "sd24ano"),
                  label = "Ano",
                  choices = sort(unique(sd24[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "sd24tab2")),
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
              h4(
                style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Taxa de Cobertura da Atenção Primária à Saúde"
              ),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "sd25muni"),
                  label = "Município",
                  choices = sd25 %>% 
                  filter(localidade != "Pará") %>% 
                  pull(localidade) %>% 
                  unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Taxa de Cobertura da Atenção Primária à Saúde----
              box(
                title = textOutput(NS(id, "sd25txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "sd25municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "sd25graf")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DAB - Departamento de Saúde da Família"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "sd25_1"))
                )
              ),

              ## Tabela - Taxa de Cobertura da Atenção Primária à Saúde----
              box(
                title = textOutput(NS(id, "sd25txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "sd25ano"),
                  label = "Ano",
                  choices = sort(unique(sd25[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "sd25tab2")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DAB - Departamento de Saúde da Família"),
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
social_saude_mp_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # TÍTULOS----
    # 1-Taxa de Mortalidade Infantil----
    ## Gráfico - Taxa de Mortalidade Infantil----
    t11 <- reactive({
      req(input$sd1municomp)
      if (input$sd1municomp == "Selecione um município") {
        paste0(
          "Taxa de Mortalidade Infantil, ",
          input$sd1muni,
          " - ",
          min(sd1$ano),
          " a ",
          max(sd1$ano)
        )
      } else {
        paste0(
          "Taxa de Mortalidade Infantil, ",
          input$sd1muni,
          " x ",
          input$sd1municomp,
          " - ",
          min(sd1$ano),
          " a ",
          max(sd1$ano)
        )
      }
    })

    ## Tabela - Taxa de Mortalidade Infantil----
    t12 <- reactive({
      ri <- sd1 %>%
        filter(ano == input$sd1ano, localidade == input$sd1muni) %>%
        pull(ri)
      paste0(
        "Taxa de Mortalidade Infantil por Município, Região de Integração ",
        unique(ri),
        " - ",
        input$sd1ano
      )
    })

    # 2-Taxa de Mortalidade na Infância----
    ## Gráfico - Taxa de Mortalidade na Infância----
    t21 <- reactive({
      req(input$sd2municomp)
      if (input$sd2municomp == "Selecione um município") {
        paste0(
          "Taxa de Mortalidade na Infância, ",
          input$sd2muni,
          " - ",
          min(sd2$ano),
          " a ",
          max(sd2$ano)
        )
      } else {
        paste0(
          "Taxa de Mortalidade na Infância, ",
          input$sd2muni,
          " x ",
          input$sd2municomp,
          " - ",
          min(sd2$ano),
          " a ",
          max(sd2$ano)
        )
      }
    })
    ## Tabela - Taxa de Mortalidade na Infância----
    t22 <- reactive({
      ri <- sd2 %>%
        filter(ano == input$sd2ano, localidade == input$sd2muni) %>%
        pull(ri)
      paste0(
        "Taxa de Mortalidade na Infância por Município, Região de Integração ",
        unique(ri),
        " - ",
        input$sd2ano
      )
    })
    # 3-Taxa de Mortalidade Materna----
    ## Gráfico - Taxa de Mortalidade Materna----
    t31 <- reactive({
      req(input$sd3municomp)
      if (input$sd3municomp == "Selecione um município") {
        paste0(
          "Taxa de Mortalidade Materna, ",
          input$sd3muni,
          " - ",
          min(sd3$ano),
          " a ",
          max(sd3$ano)
        )
      } else {
        paste0(
          "Taxa de Mortalidade Materna, ",
          input$sd3muni,
          " x ",
          input$sd3municomp,
          " - ",
          min(sd3$ano),
          " a ",
          max(sd3$ano)
        )
      }
    })
    ## Tabela - Taxa de Mortalidade Materna----
    t32 <- reactive({
      ri <- sd3 %>%
        filter(ano == input$sd3ano, localidade == input$sd3muni) %>%
        pull(ri)
      paste0(
        "Taxa de Mortalidade Materna por Município, Região de Integração ",
        unique(ri),
        " - ",
        input$sd3ano
      )
    })

    # 4-Taxa de Natalidade----
    ## Gráfico - Taxa de Natalidade----
    # Atualização da entrada
    t41 <- reactive({
      req(input$sd4municomp)
      if (input$sd4municomp == "Selecione um município") {
        paste0(
          "Taxa de Natalidade, ",
          input$sd4muni,
          " - ",
          min(sd4$ano),
          " a ",
          max(sd4$ano)
        )
      } else {
        paste0(
          "Taxa de Natalidade, ",
          input$sd4muni,
          " x ",
          input$sd4municomp,
          " - ",
          min(sd4$ano),
          " a ",
          max(sd4$ano)
        )
      }
    })
    ## Tabela - Taxa de Natalidade----
    t42 <- reactive({
      ri <- sd4 %>%
        filter(ano == input$sd4ano, localidade == input$sd4muni) %>%
        pull(ri)
      paste0(
        "Taxa de Natalidade por Município, Região de Integração ",
        unique(ri),
        " - ",
        input$sd4ano
      )
    })
    # 5-Taxa de Mortalidade Geral----
    ## Gráfico - Taxa de Mortalidade Geral----
    # Atualização da entrada
    t51 <- reactive({
      req(input$sd5municomp)
      if (input$sd5municomp == "Selecione um município") {
        paste0(
          "Taxa de Mortalidade Geral, ",
          input$sd5muni,
          " - ",
          min(sd5$ano),
          " a ",
          max(sd5$ano)
        )
      } else {
        paste0(
          "Taxa de Mortalidade Geral, ",
          input$sd5muni,
          " x ",
          input$sd5municomp,
          " - ",
          min(sd5$ano),
          " a ",
          max(sd5$ano)
        )
      }
    })
    ## Tabela - Taxa de Mortalidade Geral----
    t52 <- reactive({
      ri <- sd5 %>%
        filter(ano == input$sd5ano, localidade == input$sd5muni) %>%
        pull(ri)
      paste0(
        "Taxa de Mortalidade Geral por Município, Região de Integração ",
        unique(ri),
        " - ",
        input$sd5ano
      )
    })
    # 6-Taxa de Mortalidade por Sexo----
    ## Gráfico - Taxa de Mortalidade por Sexo----
    t61 <- reactive({
      req(input$sd6municomp)
      if (input$sd6municomp == "Selecione um município") {
        paste0(
          "Taxa de Mortalidade por Sexo, ",
          input$sd6muni,
          " - ",
          input$sd6ano
        )
      } else {
        paste0(
          "Taxa de Mortalidade por Sexo, ",
          input$sd6muni,
          " x ",
          input$sd6municomp,
          " - ",
          input$sd6ano
        )
      }
    })
    ## Tabela - Taxa de Mortalidade por Sexo----
    t62 <- reactive({
      paste0(
        "Taxa de Mortalidade por Sexo, ",
        input$sd6muni,
        " - ",
        min(sd6$ano),
        " a ",
        max(sd6$ano)
      )
    })
    ## Tabela - Taxa de Mortalidade por Sexo----
    t63 <- reactive({
      ri <- sd6 %>%
        filter(ano == input$sd6ano2, localidade == input$sd6muni) %>%
        pull(ri)
      paste0(
        "Taxa de Mortalidade por Sexo por Município, Região de Integração ",
        unique(ri),
        " - ",
        input$sd6ano2
      )
    })
    # 7-Número de Hospitais----
    ## Gráfico - Número de Hospitais----
    t71 <- reactive({
      req(input$sd7municomp)
      if (input$sd7municomp == "Selecione um município") {
        paste0(
          "Número de Hospitais, ",
          input$sd7muni,
          " - ",
          min(sd7$ano),
          " a ",
          max(sd7$ano)
        )
      } else {
        paste0(
          "Número de Hospitais, ",
          input$sd7muni,
          " x ",
          input$sd7municomp,
          " - ",
          min(sd7$ano),
          " a ",
          max(sd7$ano)
        )
      }
    })

    ## Tabela - Número de Hospitais----
    t72 <- reactive({
      ri <- sd7 %>%
        filter(ano == input$sd7ano, localidade == input$sd7muni) %>%
        pull(ri)
      paste0(
        "Número de Hospitais por Município, Região de Integração ",
        unique(ri),
        " - ",
        input$sd7ano
      )
    })

    # 8-Número de Postos e Centros de Saúde por 10.000 Habitantes----
    ## Gráfico - Número de Postos e Centros de Saúde por 10.000 Habitantes----
    t81 <- reactive({
      req(input$sd8municomp)
      if (input$sd8municomp == "Selecione um município") {
        paste0(
          "Número de Postos e Centros de Saúde por 10.000 Habitantes, ",
          input$sd8muni,
          " - ",
          min(sd8$ano),
          " a ",
          max(sd8$ano)
        )
      } else {
        paste0(
          "Número de Postos e Centros de Saúde por 10.000 Habitantes, ",
          input$sd8muni,
          " x ",
          input$sd8municomp,
          " - ",
          min(sd8$ano),
          " a ",
          max(sd8$ano)
        )
      }
    })
    ## Tabela - Número de Postos e Centros de Saúde por 10.000 Habitantes----
    t82 <- reactive({
      ri <- sd8 %>%
        filter(ano == input$sd8ano, localidade == input$sd8muni) %>%
        pull(ri)
      paste0(
        "Número de Postos e Centros de Saúde por 10.000 Habitantes por Município, Região de Integração ",
        unique(ri),
        " - ",
        input$sd8ano
      )
    })

    # 9-Médicos por 10 Mil Habitantes----
    ## Gráfico - Médicos por 10 Mil Habitantes----
    t91 <- reactive({
      req(input$sd9municomp)
      if (input$sd9municomp == "Selecione um município") {
        paste0(
          "Médicos por 10 Mil Habitantes, ",
          input$sd9muni,
          " - ",
          min(sd9$ano),
          " a ",
          max(sd9$ano)
        )
      } else {
        paste0(
          "Médicos por 10 Mil Habitantes, ",
          input$sd9muni,
          " x ",
          input$sd9municomp,
          " - ",
          min(sd9$ano),
          " a ",
          max(sd9$ano)
        )
      }
    })
    ## Tabela - Médicos por 10 Mil Habitantes----
    t92 <- reactive({
      ri <- sd9 %>%
        filter(ano == input$sd9ano, localidade == input$sd9muni) %>%
        pull(ri)
      paste0(
        "Médicos por 10 Mil Habitantes por Município, Região de Integração ",
        unique(ri),
        " - ",
        input$sd9ano
      )
    })

    # 10-Leito Hospitalar por Mil Habitantes----
    ## Gráfico - Leito Hospitalar por Mil Habitantes----
    t101 <- reactive({
      req(input$sd10municomp)
      if (input$sd10municomp == "Selecione um município") {
        paste0(
          "Leito Hospitalar por Mil Habitantes, ",
          input$sd10muni,
          " - ",
          min(sd10$ano),
          " a ",
          max(sd10$ano)
        )
      } else {
        paste0(
          "Leito Hospitalar por Mil Habitantes, ",
          input$sd10muni,
          " x ",
          input$sd10municomp,
          " - ",
          min(sd10$ano),
          " a ",
          max(sd10$ano)
        )
      }
    })
    ## Tabela - Leito Hospitalar por Mil Habitantes----
    t102 <- reactive({
      ri <- sd10 %>%
        filter(ano == input$sd10ano, localidade == input$sd10muni) %>%
        pull(ri)
      paste0(
        "Leito Hospitalar por Mil Habitantes por Município, Região de Integração ",
        unique(ri),
        " - ",
        input$sd10ano
      )
    })

    # 11-Percentual de Nascidos Vivos com 7 ou Mais Consultas Pré-natal----
    ## Gráfico - Percentual de Nascidos Vivos com 7 ou Mais Consultas Pré-natal----
    t111 <- reactive({
      req(input$sd11municomp)
      if (input$sd11municomp == "Selecione um município") {
        paste0(
          "Percentual de Nascidos Vivos com 7 ou Mais Consultas Pré-natal, ",
          input$sd11muni,
          " - ",
          min(sd11$ano),
          " a ",
          max(sd11$ano)
        )
      } else {
        paste0(
          "Percentual de Nascidos Vivos com 7 ou Mais Consultas Pré-natal, ",
          input$sd11muni,
          " x ",
          input$sd11municomp,
          " - ",
          min(sd11$ano),
          " a ",
          max(sd11$ano)
        )
      }
    })
    ## Tabela - Percentual de Nascidos Vivos com 7 ou Mais Consultas Pré-natal----
    t112 <- reactive({
      ri <- sd11 %>%
        filter(ano == input$sd11ano, localidade == input$sd11muni) %>%
        pull(ri)
      paste0(
        "Percentual de Nascidos Vivos com 7 ou Mais Consultas Pré-natal por Município, Região de Integração ",
        unique(ri),
        " - ",
        input$sd11ano
      )
    })

    # 12-Percentual de Nascidos Vivos por Parto Normal----
    ## Gráfico - Percentual de Nascidos Vivos por Parto Normal----
    t121 <- reactive({
      req(input$sd12municomp)
      if (input$sd12municomp == "Selecione um município") {
        paste0(
          "Percentual de Nascidos Vivos por Parto Normal, ",
          input$sd12muni,
          " - ",
          min(sd12$ano),
          " a ",
          max(sd12$ano)
        )
      } else {
        paste0(
          "Percentual de Nascidos Vivos por Parto Normal, ",
          input$sd12muni,
          " x ",
          input$sd12municomp,
          " - ",
          min(sd12$ano),
          " a ",
          max(sd12$ano)
        )
      }
    })
    ## Tabela - Percentual de Nascidos Vivos por Parto Normal----
    t122 <- reactive({
      ri <- sd12 %>%
        filter(ano == input$sd12ano, localidade == input$sd12muni) %>%
        pull(ri)
      paste0(
        "Percentual de Nascidos Vivos por Parto Normal por Município, Região de Integração ",
        unique(ri),
        " - ",
        input$sd12ano
      )
    })

    # 13-Percentual de Nascidos Vivos por Parto Cesário----
    ## Gráfico - Percentual de Nascidos Vivos por Parto Cesário----
    t131 <- reactive({
      req(input$sd13municomp)
      if (input$sd13municomp == "Selecione um município") {
        paste0(
          "Percentual de Nascidos Vivos por Parto Cesário, ",
          input$sd13muni,
          " - ",
          min(sd13$ano),
          " a ",
          max(sd13$ano)
        )
      } else {
        paste0(
          "Percentual de Nascidos Vivos por Parto Cesário, ",
          input$sd13muni,
          " x ",
          input$sd13municomp,
          " - ",
          min(sd13$ano),
          " a ",
          max(sd13$ano)
        )
      }
    })
    ## Tabela - Percentual de Nascidos Vivos por Parto Cesário----
    t132 <- reactive({
      ri <- sd13 %>%
        filter(ano == input$sd13ano, localidade == input$sd13muni) %>%
        pull(ri)
      paste0(
        "Percentual de Nascidos Vivos por Parto Cesário por Município, Região de Integração ",
        unique(ri),
        " - ",
        input$sd13ano
      )
    })

    # 14-Percentual de Nascidos Vivos de Mães Adolescentes na Faixa Etária de 10 a 19 Anos----
    ## Gráfico - Percentual de Nascidos Vivos de Mães Adolescentes na Faixa Etária de 10 a 19 Anos----
    t141 <- reactive({
      req(input$sd14municomp)
      if (input$sd14municomp == "Selecione um município") {
        paste0(
          "Percentual de Nascidos Vivos de Mães Adolescentes na Faixa Etária de 10 a 19 Anos, ",
          input$sd14muni,
          " - ",
          min(sd14$ano),
          " a ",
          max(sd14$ano)
        )
      } else {
        paste0(
          "Percentual de Nascidos Vivos de Mães Adolescentes na Faixa Etária de 10 a 19 Anos, ",
          input$sd14muni,
          " x ",
          input$sd14municomp,
          " - ",
          min(sd14$ano),
          " a ",
          max(sd14$ano)
        )
      }
    })
    ## Tabela - Percentual de Nascidos Vivos de Mães Adolescentes na Faixa Etária de 10 a 19 Anos----
    t142 <- reactive({
      ri <- sd14 %>%
        filter(ano == input$sd14ano, localidade == input$sd14muni) %>%
        pull(ri)
      paste0(
        "Percentual de Nascidos Vivos de Mães Adolescentes na Faixa Etária de 10 a 19 Anos por Município, Região de Integração ",
        unique(ri),
        " - ",
        input$sd14ano
      )
    })

    # 15-Taxa de Incidência da Hanseníase----
    ## Gráfico - Taxa de Incidência da Hanseníase----
    t151 <- reactive({
      req(input$sd15municomp)
      if (input$sd15municomp == "Selecione um município") {
        paste0(
          "Taxa de Incidência da Hanseníase, ",
          input$sd15muni,
          " - ",
          min(sd15$ano),
          " a ",
          max(sd15$ano)
        )
      } else {
        paste0(
          "Taxa de Incidência da Hanseníase, ",
          input$sd15muni,
          " x ",
          input$sd15municomp,
          " - ",
          min(sd15$ano),
          " a ",
          max(sd15$ano)
        )
      }
    })
    ## Tabela - Taxa de Incidência da Hanseníase----
    t152 <- reactive({
      ri <- sd15 %>%
        filter(ano == input$sd15ano, localidade == input$sd15muni) %>%
        pull(ri)
      paste0(
        "Taxa de Incidência da Hanseníase - ",
        unique(ri),
        " por Município, Região de Integração ",
        input$sd15ano
      )
    })

    # 16-Taxa de Incidência da Tuberculose----
    ## Gráfico - Taxa de Incidência da Tuberculose----
    t161 <- reactive({
      req(input$sd16municomp)
      if (input$sd16municomp == "Selecione um município") {
        paste0(
          "Taxa de Incidência da Tuberculose, ",
          input$sd16muni,
          " - ",
          min(sd16$ano),
          " a ",
          max(sd16$ano)
        )
      } else {
        paste0(
          "Taxa de Incidência da Tuberculose, ",
          input$sd16muni,
          " x ",
          input$sd16municomp,
          " - ",
          min(sd16$ano),
          " a ",
          max(sd16$ano)
        )
      }
    })
    ## Tabela - Taxa de Incidência da Tuberculose----
    t162 <- reactive({
      ri <- sd16 %>%
        filter(ano == input$sd16ano, localidade == input$sd16muni) %>%
        pull(ri)
      paste0(
        "Taxa de Incidência da Tuberculose por Município, Região de Integração ",
        unique(ri),
        " - ",
        input$sd16ano
      )
    })

    # 17-Proporção de Mulheres de 25 a 64 anos que Realizaram Exames Citopatológicos do Colo do Útero----
    ## Gráfico - Proporção de Mulheres de 25 a 64 anos que Realizaram Exames Citopatológicos do Colo do Útero----
    t171 <- reactive({
      req(input$sd17municomp)
      if (input$sd17municomp == "Selecione um município") {
        paste0(
          "Proporção de Mulheres de 25 a 64 anos que Realizaram Exames Citopatológicos do Colo do Útero, ",
          input$sd17muni,
          " - ",
          min(sd17$ano),
          " a ",
          max(sd17$ano)
        )
      } else {
        paste0(
          "Proporção de Mulheres de 25 a 64 anos que Realizaram Exames Citopatológicos do Colo do Útero, ",
          input$sd17muni,
          " x ",
          input$sd17municomp,
          " - ",
          min(sd17$ano),
          " a ",
          max(sd17$ano)
        )
      }
    })
    ## Tabela - Proporção de Mulheres de 25 a 64 anos que Realizaram Exames Citopatológicos do Colo do Útero----
    t172 <- reactive({
      ri <- sd17 %>%
        filter(ano == input$sd17ano, localidade == input$sd17muni) %>%
        pull(ri)
      paste0(
        "Proporção de Mulheres de 25 a 64 anos que Realizaram Exames Citopatológicos do Colo do Útero por Município, Região de Integração ",
        unique(ri),
        " - ",
        input$sd17ano
      )
    })

    # 18-Óbitos por Residência Neoplasias (Tumores)----
    ## Gráfico - Óbitos por Residência Neoplasias (Tumores)----
    t181 <- reactive({
      req(input$sd18municomp)
      if (input$sd18municomp == "Selecione um município") {
        paste0(
          "Óbitos por Residência Neoplasias (Tumores), ",
          input$sd18muni,
          " - ",
          min(sd18$ano),
          " a ",
          max(sd18$ano)
        )
      } else {
        paste0(
          "Óbitos por Residência Neoplasias (Tumores), ",
          input$sd18muni,
          " x ",
          input$sd18municomp,
          " - ",
          min(sd18$ano),
          " a ",
          max(sd18$ano)
        )
      }
    })
    ## Tabela - Óbitos por Residência Neoplasias (Tumores)----
    t182 <- reactive({
      ri <- sd18 %>%
        filter(ano == input$sd18ano, localidade == input$sd18muni) %>%
        pull(ri)
      paste0(
        "Óbitos por Residência Neoplasias (Tumores) por Município, Região de Integração ",
        unique(ri),
        " - ",
        input$sd18ano
      )
    })

    # 19-Óbitos por Residência Algumas Doenças Infecciosas e Parasitárias----
    ## Gráfico - Óbitos por Residência Algumas Doenças Infecciosas e Parasitárias----
    t191 <- reactive({
      req(input$sd19municomp)
      if (input$sd19municomp == "Selecione um município") {
        paste0(
          "Óbitos por Residência Algumas Doenças Infecciosas e Parasitárias, ",
          input$sd19muni,
          " - ",
          min(sd19$ano),
          " a ",
          max(sd19$ano)
        )
      } else {
        paste0(
          "Óbitos por Residência Algumas Doenças Infecciosas e Parasitárias, ",
          input$sd19muni,
          " x ",
          input$sd19municomp,
          " - ",
          min(sd19$ano),
          " a ",
          max(sd19$ano)
        )
      }
    })
    ## Tabela - Óbitos por Residência Algumas Doenças Infecciosas e Parasitárias----
    t192 <- reactive({
      ri <- sd19 %>%
        filter(ano == input$sd19ano, localidade == input$sd19muni) %>%
        pull(ri)
      paste0(
        "Óbitos por Residência Algumas Doenças Infecciosas e Parasitárias por Município, Região de Integração ",
        unique(ri),
        " - ",
        input$sd19ano
      )
    })

    # 20-Óbitos por Residência Doenças do Aparelho Circulatório----
    ## Gráfico - Óbitos por Residência Doenças do Aparelho Circulatório----
    t201 <- reactive({
      req(input$sd20municomp)
      if (input$sd20municomp == "Selecione um município") {
        paste0(
          "Óbitos por Residência Doenças do Aparelho Circulatório, ",
          input$sd20muni,
          " - ",
          min(sd20$ano),
          " a ",
          max(sd20$ano)
        )
      } else {
        paste0(
          "Óbitos por Residência Doenças do Aparelho Circulatório, ",
          input$sd20muni,
          " x ",
          input$sd20municomp,
          " - ",
          min(sd20$ano),
          " a ",
          max(sd20$ano)
        )
      }
    })
    ## Tabela - Óbitos por Residência Doenças do Aparelho Circulatório----
    t202 <- reactive({
      ri <- sd20 %>%
        filter(ano == input$sd20ano, localidade == input$sd20muni) %>%
        pull(ri)
      paste0(
        "Óbitos por Residência Doenças do Aparelho Circulatório por Município, Região de Integração ",
        unique(ri),
        " - ",
        input$sd20ano
      )
    })

    # 21-Óbitos por Residência Doenças do Aparelho Digestivo----
    ## Gráfico - Óbitos por Residência Doenças do Aparelho Digestivo----
    t211 <- reactive({
      req(input$sd21municomp)
      if (input$sd21municomp == "Selecione um município") {
        paste0(
          "Óbitos por Residência Doenças do Aparelho Digestivo, ",
          input$sd21muni,
          " - ",
          min(sd21$ano),
          " a ",
          max(sd21$ano)
        )
      } else {
        paste0(
          "Óbitos por Residência Doenças do Aparelho Digestivo, ",
          input$sd21muni,
          " x ",
          input$sd21municomp,
          " - ",
          min(sd21$ano),
          " a ",
          max(sd21$ano)
        )
      }
    })
    ## Tabela - Óbitos por Residência Doenças do Aparelho Digestivo----
    t212 <- reactive({
      ri <- sd21 %>%
        filter(ano == input$sd21ano, localidade == input$sd21muni) %>%
        pull(ri)
      paste0(
        "Óbitos por Residência Doenças do Aparelho Digestivo por Município, Região de Integração ",
        unique(ri),
        " - ",
        input$sd21ano
      )
    })

    # 22-Óbitos por Residência Doenças do Aparelho Respiratório----
    ## Gráfico - Óbitos por Residência Doenças do Aparelho Respiratório----
    t221 <- reactive({
      req(input$sd22municomp)
      if (input$sd22municomp == "Selecione um município") {
        paste0(
          "Óbitos por Residência Doenças do Aparelho Respiratório, ",
          input$sd22muni,
          " - ",
          min(sd22$ano),
          " a ",
          max(sd22$ano)
        )
      } else {
        paste0(
          "Óbitos por Residência Doenças do Aparelho Respiratório, ",
          input$sd22muni,
          " x ",
          input$sd22municomp,
          " - ",
          min(sd22$ano),
          " a ",
          max(sd22$ano)
        )
      }
    })
    ## Tabela - Óbitos por Residência Doenças do Aparelho Respiratório----
    t222 <- reactive({
      ri <- sd22 %>%
        filter(ano == input$sd22ano, localidade == input$sd22muni) %>%
        pull(ri)
      paste0(
        "Óbitos por Residência Doenças do Aparelho Respiratório por Município, Região de Integração ",
        unique(ri),
        " - ",
        input$sd22ano
      )
    })

    # 23-Óbitos por Residência Algumas Afecções Originadas no Período Perinatal----
    ## Gráfico - Óbitos por Residência Algumas Afecções Originadas no Período Perinatal----
    t231 <- reactive({
      req(input$sd23municomp)
      if (input$sd23municomp == "Selecione um município") {
        paste0(
          "Óbitos por Residência Algumas Afecções Originadas no Período Perinatal, ",
          input$sd23muni,
          " - ",
          min(sd23$ano),
          " a ",
          max(sd23$ano)
        )
      } else {
        paste0(
          "Óbitos por Residência Algumas Afecções Originadas no Período Perinatal, ",
          input$sd23muni,
          " x ",
          input$sd23municomp,
          " - ",
          min(sd23$ano),
          " a ",
          max(sd23$ano)
        )
      }
    })
    ## Tabela - Óbitos por Residência Algumas Afecções Originadas no Período Perinatal----
    t232 <- reactive({
      ri <- sd23 %>%
        filter(ano == input$sd23ano, localidade == input$sd23muni) %>%
        pull(ri)
      paste0(
        "Óbitos por Residência Algumas Afecções Originadas no Período Perinatal por Município, Região de Integração ",
        unique(ri),
        " - ",
        input$sd23ano
      )
    })

    # 24-Percentual de Óbitos por Infarto Agudo do Miocárdio (IAM)----
    ## Gráfico - Percentual de Óbitos por Infarto Agudo do Miocárdio (IAM)----
    t241 <- reactive({
      req(input$sd24municomp)
      if (input$sd24municomp == "Selecione um município") {
        paste0(
          "Percentual de Óbitos por Infarto Agudo do Miocárdio (IAM), ",
          input$sd24muni,
          " - ",
          min(sd24$ano),
          " a ",
          max(sd24$ano)
        )
      } else {
        paste0(
          "Percentual de Óbitos por Infarto Agudo do Miocárdio (IAM), ",
          input$sd24muni,
          " x ",
          input$sd24municomp,
          " - ",
          min(sd24$ano),
          " a ",
          max(sd24$ano)
        )
      }
    })
    ## Tabela - Percentual de Óbitos por Infarto Agudo do Miocárdio (IAM)----
    t242 <- reactive({
      ri <- sd24 %>%
        filter(ano == input$sd24ano, localidade == input$sd24muni) %>%
        pull(ri)
      paste0(
        "Percentual de Óbitos por Infarto Agudo do Miocárdio (IAM) por Município, Região de Integração ",
        unique(ri),
        " - ",
        input$sd24ano
      )
    })
    # 25-Taxa de Cobertura da Atenção Primária à Saúde----
    ## Gráfico - Taxa de Cobertura da Atenção Primária à Saúde----
    t251 <- reactive({
      req(input$sd25municomp)
      if (input$sd25municomp == "Selecione um município") {
        paste0(
          "Taxa de Cobertura da Atenção Primária à Saúde, ",
          input$sd25muni,
          " - ",
          min(sd25$ano),
          " a ",
          max(sd25$ano)
        )
      } else {
        paste0(
          "Taxa de Cobertura da Atenção Primária à Saúde, ",
          input$sd25muni,
          " x ",
          input$sd25municomp,
          " - ",
          min(sd25$ano),
          " a ",
          max(sd25$ano)
        )
      }
    })
    ## Tabela - Taxa de Cobertura da Atenção Primária à Saúde----
    t252 <- reactive({
      ri <- sd25 %>%
        filter(ano == input$sd25ano, localidade == input$sd25muni) %>%
        pull(ri)
      paste0(
        "Taxa de Cobertura da Atenção Primária à Saúde por Município, Região de Integração ",
        unique(ri),
        " - ",
        input$sd25ano
      )
    })
    # VISUALIZAÇÃO----
    # 1-Taxa de Mortalidade Infantil----
    ## Gráfico - Taxa de Mortalidade Infantil----
    # Atualização da entrada
    sd1comp <- reactive({
      input$sd1muni
    })
    observeEvent(sd1comp(), {
      x <- sd1 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != sd1comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "sd1municomp",
        choices = c("Selecione um município", choices),
        session
      )
    })

    output$sd1txt1 <- renderText({
      t11()
    })

    output$sd1graf <- renderEcharts4r({
      req(input$sd1municomp)
      if (input$sd1municomp == "Selecione um município") {
        a <- sd1 %>% filter(localidade == input$sd1muni)
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
        a <- sd1 %>% filter(localidade == input$sd1muni)
        b <- sd1 %>% filter(localidade == input$sd1municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$sd1muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$sd1municomp,
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
      }
    })
    ## Tabela - Taxa de Mortalidade Infantil----
    output$sd1txt2 <- renderText({
      t12()
    })
    output$sd1tab2 <- renderReactable({
      ris <- sd1 %>%
        filter(ano == input$sd1ano, localidade == input$sd1muni) %>%
        pull(ri)
      x <-
        sd1 %>% filter(ano == input$sd1ano, localidade != "Pará")
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
            cell = data_bars(
              x,
              text_position = "above",
              box_shadow = TRUE,
              align_bars = "right",
              number_fmt = scales::number_format(
                accuracy = 0.01,
                decimal.mark = ","
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

    # 2-Taxa de Mortalidade na Infância----

    ## Gráfico - Taxa de Mortalidade na Infância----
    # Atualização da entrada
    sd2comp <- reactive({
      input$sd2muni
    })
    observeEvent(sd2comp(), {
      x <- sd2 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != sd2comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "sd2municomp",
        choices = c("Selecione um município", choices),
        session
      )
    })

    output$sd2txt1 <- renderText({
      t21()
    })

    output$sd2graf <- renderEcharts4r({
      req(input$sd2municomp)
      if (input$sd2municomp == "Selecione um município") {
        a <- sd2 %>% filter(localidade == input$sd2muni)
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
        a <- sd2 %>% filter(localidade == input$sd2muni)
        b <- sd2 %>% filter(localidade == input$sd2municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$sd2muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$sd2municomp,
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
      }
    })
    ## Tabela - Taxa de Mortalidade na Infância----
    output$sd2txt2 <- renderText({
      t22()
    })
    output$sd2tab2 <- renderReactable({
      ris <- sd2 %>%
        filter(ano == input$sd2ano, localidade == input$sd2muni) %>%
        pull(ri)
      x <-
        sd2 %>% filter(ano == input$sd2ano, localidade != "Pará")
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
            cell = data_bars(
              x,
              text_position = "above",
              box_shadow = TRUE,
              align_bars = "right",
              number_fmt = scales::number_format(
                accuracy = 0.01,
                decimal.mark = ","
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

    # 3-Taxa de Mortalidade Materna----
    ## Gráfico - Taxa de Mortalidade Materna----
    # Atualização da entrada
    sd3comp <- reactive({
      input$sd3muni
    })
    observeEvent(sd3comp(), {
      x <- sd3 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != sd3comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "sd3municomp",
        choices = c("Selecione um município", choices),
        session
      )
    })

    output$sd3txt1 <- renderText({
      t31()
    })

    output$sd3graf <- renderEcharts4r({
      req(input$sd3municomp)
      if (input$sd3municomp == "Selecione um município") {
        a <- sd3 %>% filter(localidade == input$sd3muni)
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
        a <- sd3 %>% filter(localidade == input$sd3muni)
        b <- sd3 %>% filter(localidade == input$sd3municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$sd3muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$sd3municomp,
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
      }
    })
    ## Tabela - Taxa de Mortalidade Materna----
    output$sd3txt2 <- renderText({
      t32()
    })
    output$sd3tab2 <- renderReactable({
      ris <- sd3 %>%
        filter(ano == input$sd3ano, localidade == input$sd3muni) %>%
        pull(ri)
      x <-
        sd3 %>% filter(ano == input$sd3ano, localidade != "Pará")
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
            cell = data_bars(
              x,
              text_position = "above",
              box_shadow = TRUE,
              align_bars = "right",
              number_fmt = scales::number_format(
                accuracy = 0.01,
                decimal.mark = ","
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

    # 4-Taxa de Natalidade----
    ## Gráfico - Taxa de Natalidade----
    # Atualização da entrada
    sd4comp <- reactive({
      input$sd4muni
    })
    observeEvent(sd4comp(), {
      x <- sd4 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != sd4comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "sd4municomp",
        choices = c("Selecione um município", choices),
        session
      )
    })

    output$sd4txt1 <- renderText({
      t41()
    })
    output$sd4graf <- renderEcharts4r({
      req(input$sd4municomp)
      if (input$sd4municomp == "Selecione um município") {
        a <- sd4 %>% filter(localidade == input$sd4muni)
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
        a <- sd4 %>% filter(localidade == input$sd4muni)
        b <- sd4 %>% filter(localidade == input$sd4municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$sd4muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$sd4municomp,
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
      }
    })
    ## Tabela - Taxa de Natalidade----
    output$sd4txt2 <- renderText({
      t42()
    })
    output$sd4tab2 <- renderReactable({
      ris <- sd4 %>%
        filter(ano == input$sd4ano, localidade == input$sd4muni) %>%
        pull(ri)
      x <-
        sd4 %>% filter(ano == input$sd4ano, localidade != "Pará")
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
            cell = data_bars(
              x,
              text_position = "above",
              box_shadow = TRUE,
              align_bars = "right",
              number_fmt = scales::number_format(
                accuracy = 0.1,
                decimal.mark = ","
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

    # 5-Taxa de Mortalidade Geral----
    ## Gráfico - Taxa de Mortalidade Geral----
    # Atualização da entrada
    sd5comp <- reactive({
      input$sd5muni
    })
    observeEvent(sd5comp(), {
      x <- sd5 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != sd5comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "sd5municomp",
        choices = c("Selecione um município", choices),
        session
      )
    })

    output$sd5txt1 <- renderText({
      t51()
    })
    output$sd5graf <- renderEcharts4r({
      req(input$sd5municomp)
      if (input$sd5municomp == "Selecione um município") {
        a <- sd5 %>% filter(localidade == input$sd5muni)
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
        a <- sd5 %>% filter(localidade == input$sd5muni)
        b <- sd5 %>% filter(localidade == input$sd5municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$sd5muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$sd5municomp,
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
      }
    })
    ## Tabela - Taxa de Mortalidade Geral----
    output$sd5txt2 <- renderText({
      t52()
    })
    output$sd5tab2 <- renderReactable({
      ris <- sd5 %>%
        filter(ano == input$sd5ano, localidade == input$sd5muni) %>%
        pull(ri)
      x <-
        sd5 %>% filter(ano == input$sd5ano, localidade != "Pará")
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
            cell = data_bars(
              x,
              text_position = "above",
              box_shadow = TRUE,
              align_bars = "right",
              number_fmt = scales::number_format(
                accuracy = 0.01,
                decimal.mark = ","
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
    # 6-Taxa de Mortalidade por Sexo----
    ## Gráfico - Taxa de Mortalidade por Sexo----
    # Atualização da entrada
    sd6comp <- reactive({
      input$sd6muni
    })
    observeEvent(sd6comp(), {
      x <- sd6 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != sd6comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "sd6municomp",
        choices = c("Selecione um município", choices),
        session
      )
    })

    ## Título
    output$sd6txt1 <- renderText({
      t61()
    })

    output$sd6graf <- renderEcharts4r({
      req(input$sd6municomp)
      if (input$sd6municomp == "Selecione um município") {
        a <-
          sd6 %>% filter(localidade == input$sd6muni, ano == input$sd6ano)
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            color = "#f17701",
            name = "Taxa",
            legend = F,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            barWidth = "20%",
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 2, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Sexo",
            nameLocation = "middle",
            nameTextStyle =
              list(
                fontWeight = "bold",
                padding = c(20, 0, 0, 0),
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
            formatter = htmlwidgets::JS(
              "
              function (value, index) {
              return value.toLocaleString('pt-BR', { minimumFractionDigits: 2, maximumFractionDigits: 2 });
              }
            "
            )
          ) %>%
          e_locale("pt-Br") %>%
          e_grid(show = T)
      } else {
        a <-
          sd6 %>% filter(localidade == input$sd6muni, ano == input$sd6ano)
        b <-
          sd6 %>% filter(
            localidade == input$sd6municomp,
            ano == input$sd6ano
          )
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            name = input$sd6muni,
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
            name = input$sd6municomp,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            barWidth = "20%",
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 2, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Sexo",
            nameLocation = "middle",
            nameTextStyle =
              list(
                fontWeight = "bold",
                padding = c(20, 0, 0, 0),
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
            formatter = htmlwidgets::JS(
              "
              function (value, index) {
              return value.toLocaleString('pt-BR', { minimumFractionDigits: 2, maximumFractionDigits: 2 });
              }
            "
            )
          ) %>%
          e_locale("pt-Br") %>%
          e_grid(show = T)
      }
    })
    ## Tabela - Taxa de Mortalidade por Sexo----
    output$sd6txt2 <- renderText({
      t62()
    })
    output$sd6tab <- renderReactable({
      x <- sd6 %>%
        filter(localidade == input$sd6muni) %>%
        pivot_wider(names_from = categoria, values_from = valor) %>%
        select(-c(tematica:localidade))
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
          ano = colDef(name = "Ano"),
          Masculino = colDef(format = colFormat(
            digits = 2, locales = "pt-BR"
          )),
          Feminino = colDef(format = colFormat(
            digits = 2, locales = "pt-BR"
          ))
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
    ## Tabela - Taxa de Mortalidade por Sexo----
    output$sd6txt3 <- renderText({
      t63()
    })
    output$sd6tab1 <- renderReactable({
      ris <- sd6 %>%
        filter(ano == input$sd6ano2, localidade == input$sd6muni) %>%
        pull(ri)
      x <-
        sd6 %>% filter(ano == input$sd6ano2, localidade != "Pará")
      x <- x %>%
        filter(ri == ris) %>%
        select(
          ri,
          localidade,
          categoria,
          valor
        ) %>%
        pivot_wider(names_from = categoria, values_from = valor)
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
          Masculino = colDef(format = colFormat(
            digits = 2, locales = "pt-BR"
          )),
          Feminino = colDef(format = colFormat(
            digits = 2, locales = "pt-BR"
          ))
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
    # 7-Número de Hospitais----
    ## Gráfico - Número de Hospitais----
    # Atualização da entrada
    sd7comp <- reactive({
      input$sd7muni
    })
    observeEvent(sd7comp(), {
      x <- sd7 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != sd7comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "sd7municomp",
        choices = c("Selecione um município", choices),
        session
      )
    })

    output$sd7txt1 <- renderText({
      t71()
    })

    output$sd7graf <- renderEcharts4r({
      req(input$sd7municomp)
      maximo <- max(sd7$valor)
      if (input$sd7municomp == "Selecione um município") {
        a <- sd7 %>% filter(localidade == input$sd7muni)
        a %>%
          e_charts(x = ano) %>%
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
            name = "Número",
            nameTextStyle = list(fontWeight = "bold", fontSize = 14),
            scale = T,
            min = 0,
            max = maximo,
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
        a <- sd7 %>% filter(localidade == input$sd7muni)
        b <- sd7 %>% filter(localidade == input$sd7municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$sd7muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$sd7municomp,
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
            name = "Número",
            nameTextStyle = list(fontWeight = "bold", fontSize = 14),
            scale = T,
            min = 0,
            max = max(sd7$valor),
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
    ## Tabela - Número de Hospitais----
    output$sd7txt2 <- renderText({
      t72()
    })
    output$sd7tab2 <- renderReactable({
      ris <- sd7 %>%
        filter(ano == input$sd7ano, localidade == input$sd7muni) %>%
        pull(ri)
      x <-
        sd7 %>% filter(ano == input$sd7ano, localidade != "Pará")
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
            name = "Número",
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

    # 8-Número de Postos e Centros de Saúde por 10.000 Habitantes----
    ## Gráfico - Número de Postos e Centros de Saúde por 10.000 Habitantes----
    # Atualização da entrada
    sd8comp <- reactive({
      input$sd8muni
    })
    observeEvent(sd8comp(), {
      x <- sd8 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != sd8comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "sd8municomp",
        choices = c("Selecione um município", choices),
        session
      )
    })

    output$sd8txt1 <- renderText({
      t81()
    })

    output$sd8graf <- renderEcharts4r({
      req(input$sd8municomp)
      if (input$sd8municomp == "Selecione um município") {
        a <- sd8 %>% filter(localidade == input$sd8muni)
        a %>%
          e_charts(x = ano) %>%
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
            name = "Número",
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
        a <- sd8 %>% filter(localidade == input$sd8muni)
        b <- sd8 %>% filter(localidade == input$sd8municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$sd8muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$sd8municomp,
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
            name = "Número",
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
    ## Tabela - Número de Postos e Centros de Saúde por 10.000 Habitantes----
    output$sd8txt2 <- renderText({
      t82()
    })
    output$sd8tab2 <- renderReactable({
      ris <- sd8 %>%
        filter(ano == input$sd8ano, localidade == input$sd8muni) %>%
        pull(ri)
      x <-
        sd8 %>% filter(ano == input$sd8ano, localidade != "Pará")
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
            name = "Número",
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

    # 9-Médicos por 10 Mil Habitantes----
    ## Gráfico - Médicos por 10 Mil Habitantes----
    # Atualização da entrada
    sd9comp <- reactive({
      input$sd9muni
    })
    observeEvent(sd9comp(), {
      x <- sd9 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != sd9comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "sd9municomp",
        choices = c("Selecione um município", choices),
        session
      )
    })

    output$sd9txt1 <- renderText({
      t91()
    })
    output$sd9graf <- renderEcharts4r({
      req(input$sd9municomp)
      if (input$sd9municomp == "Selecione um município") {
        a <- sd9 %>% filter(localidade == input$sd9muni)
        a %>%
          e_charts(x = ano) %>%
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
            name = "Número",
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
        a <- sd9 %>% filter(localidade == input$sd9muni)
        b <- sd9 %>% filter(localidade == input$sd9municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$sd9muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$sd9municomp,
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
            name = "Número",
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
      }
    })
    ## Tabela - Médicos por 10 Mil Habitantes----
    output$sd9txt2 <- renderText({
      t92()
    })
    output$sd9tab2 <- renderReactable({
      ris <- sd9 %>%
        filter(ano == input$sd9ano, localidade == input$sd9muni) %>%
        pull(ri)
      x <-
        sd9 %>% filter(ano == input$sd9ano, localidade != "Pará")
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
            name = "Número",
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

    # 10-Leito Hospitalar por Mil Habitantes----
    ## Gráfico - Leito Hospitalar por Mil Habitantes----
    # Atualização da entrada
    sd10comp <- reactive({
      input$sd10muni
    })
    observeEvent(sd10comp(), {
      x <- sd10 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != sd10comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "sd10municomp",
        choices = c("Selecione um município", choices),
        session
      )
    })

    output$sd10txt1 <- renderText({
      t101()
    })

    output$sd10graf <- renderEcharts4r({
      req(input$sd10municomp)
      if (input$sd10municomp == "Selecione um município") {
        a <- sd10 %>% filter(localidade == input$sd10muni)
        a %>%
          e_charts(x = ano) %>%
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
            name = "Número",
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
        a <- sd10 %>% filter(localidade == input$sd10muni)
        b <- sd10 %>% filter(localidade == input$sd10municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$sd10muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$sd10municomp,
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
            name = "Número",
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
      }
    })
    ## Tabela - Leito Hospitalar por Mil Habitantes----
    output$sd10txt2 <- renderText({
      t102()
    })
    output$sd10tab2 <- renderReactable({
      ris <- sd10 %>%
        filter(ano == input$sd10ano, localidade == input$sd10muni) %>%
        pull(ri)
      x <-
        sd10 %>% filter(ano == input$sd10ano, localidade != "Pará")
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
            name = "Número",
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
              number_fmt = scales::number_format(accuracy = 0.01, decimal.mark = ",")
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

    # 11-Percentual de Nascidos Vivos com 7 ou Mais Consultas Pré-natal----
    ## Gráfico - Percentual de Nascidos Vivos com 7 ou Mais Consultas Pré-natal----
    # Atualização da entrada
    sd11comp <- reactive({
      input$sd11muni
    })
    observeEvent(sd11comp(), {
      x <- sd11 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != sd11comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "sd11municomp",
        choices = c("Selecione um município", choices),
        session
      )
    })

    output$sd11txt1 <- renderText({
      t111()
    })

    output$sd11graf <- renderEcharts4r({
      req(input$sd11municomp)
      if (input$sd11municomp == "Selecione um município") {
        a <- sd11 %>% filter(localidade == input$sd11muni)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f17701",
            name = "Percentual",
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
            name = "Percentual",
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
        a <- sd11 %>% filter(localidade == input$sd11muni)
        b <- sd11 %>% filter(localidade == input$sd11municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$sd11muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$sd11municomp,
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
            name = "Percentual",
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
      }
    })
    ## Tabela - Percentual de Nascidos Vivos com 7 ou Mais Consultas Pré-natal----
    output$sd11txt2 <- renderText({
      t112()
    })
    output$sd11tab2 <- renderReactable({
      ris <- sd11 %>%
        filter(ano == input$sd11ano, localidade == input$sd11muni) %>%
        pull(ri)
      x <-
        sd11 %>% filter(ano == input$sd11ano, localidade != "Pará")
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
            name = "Percentual",
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
              number_fmt = scales::number_format(accuracy = 0.01, decimal.mark = ",")
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

    # 12-Percentual de Nascidos Vivos por Parto Normal----
    ## Gráfico - Percentual de Nascidos Vivos por Parto Normal----
    # Atualização da entrada
    sd12comp <- reactive({
      input$sd12muni
    })
    observeEvent(sd12comp(), {
      x <- sd12 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != sd12comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "sd12municomp",
        choices = c("Selecione um município", choices),
        session
      )
    })

    output$sd12txt1 <- renderText({
      t121()
    })

    output$sd12graf <- renderEcharts4r({
      req(input$sd12municomp)
      if (input$sd12municomp == "Selecione um município") {
        a <- sd12 %>% filter(localidade == input$sd12muni)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f17701",
            name = "Percentual",
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
            name = "Percentual",
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
        a <- sd12 %>% filter(localidade == input$sd12muni)
        b <- sd12 %>% filter(localidade == input$sd12municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$sd12muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$sd12municomp,
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
            name = "Percentual",
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
      }
    })
    ## Tabela - Percentual de Nascidos Vivos por Parto Normal----
    output$sd12txt2 <- renderText({
      t122()
    })
    output$sd12tab2 <- renderReactable({
      ris <- sd12 %>%
        filter(ano == input$sd12ano, localidade == input$sd12muni) %>%
        pull(ri)
      x <-
        sd12 %>% filter(ano == input$sd12ano, localidade != "Pará")
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
            name = "Percentual",
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
              number_fmt = scales::number_format(accuracy = 0.01, decimal.mark = ",")
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

    # 13-Percentual de Nascidos Vivos por Parto Cesário----
    ## Gráfico - Percentual de Nascidos Vivos por Parto Cesário----
    # Atualização da entrada
    sd13comp <- reactive({
      input$sd13muni
    })
    observeEvent(sd13comp(), {
      x <- sd13 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != sd13comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "sd13municomp",
        choices = c("Selecione um município", choices),
        session
      )
    })

    output$sd13txt1 <- renderText({
      t131()
    })

    output$sd13graf <- renderEcharts4r({
      req(input$sd13municomp)
      if (input$sd13municomp == "Selecione um município") {
        a <- sd13 %>% filter(localidade == input$sd13muni)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f17701",
            name = "Percentual",
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
            name = "Percentual",
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
        a <- sd13 %>% filter(localidade == input$sd13muni)
        b <- sd13 %>% filter(localidade == input$sd13municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$sd13muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$sd13municomp,
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
            name = "Percentual",
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
      }
    })
    ## Tabela - Percentual de Nascidos Vivos por Parto Cesário----
    output$sd13txt2 <- renderText({
      t132()
    })
    output$sd13tab2 <- renderReactable({
      ris <- sd13 %>%
        filter(ano == input$sd13ano, localidade == input$sd13muni) %>%
        pull(ri)
      x <-
        sd13 %>% filter(ano == input$sd13ano, localidade != "Pará")
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
            name = "Percentual",
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
              number_fmt = scales::number_format(accuracy = 0.01, decimal.mark = ",")
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

    # 14-Percentual de Nascidos Vivos de Mães Adolescentes na Faixa Etária de 10 a 19 Anos----
    ## Gráfico - Percentual de Nascidos Vivos de Mães Adolescentes na Faixa Etária de 10 a 19 Anos----
    # Atualização da entrada
    sd14comp <- reactive({
      input$sd14muni
    })
    observeEvent(sd14comp(), {
      x <- sd14 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != sd14comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "sd14municomp",
        choices = c("Selecione um município", choices),
        session
      )
    })

    output$sd14txt1 <- renderText({
      t141()
    })

    output$sd14graf <- renderEcharts4r({
      req(input$sd14municomp)
      if (input$sd14municomp == "Selecione um município") {
        a <- sd14 %>% filter(localidade == input$sd14muni)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f17701",
            name = "Percentual",
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
            name = "Percentual",
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
        a <- sd14 %>% filter(localidade == input$sd14muni)
        b <- sd14 %>% filter(localidade == input$sd14municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$sd14muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$sd14municomp,
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
            name = "Percentual",
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
      }
    })
    ## Tabela - Percentual de Nascidos Vivos de Mães Adolescentes na Faixa Etária de 10 a 19 Anos----
    output$sd14txt2 <- renderText({
      t142()
    })
    output$sd14tab2 <- renderReactable({
      ris <- sd14 %>%
        filter(ano == input$sd14ano, localidade == input$sd14muni) %>%
        pull(ri)
      x <-
        sd14 %>% filter(ano == input$sd14ano, localidade != "Pará")
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
            name = "Percentual",
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
              number_fmt = scales::number_format(accuracy = 0.01, decimal.mark = ",")
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

    # 15-Taxa de Incidência da Hanseníase----
    ## Gráfico - Taxa de Incidência da Hanseníase----
    # Atualização da entrada
    sd15comp <- reactive({
      input$sd15muni
    })
    observeEvent(sd15comp(), {
      x <- sd15 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != sd15comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "sd15municomp",
        choices = c("Selecione um município", choices),
        session
      )
    })

    output$sd15txt1 <- renderText({
      t151()
    })

    output$sd15graf <- renderEcharts4r({
      req(input$sd15municomp)
      if (input$sd15municomp == "Selecione um município") {
        a <- sd15 %>% filter(localidade == input$sd15muni)
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
        a <- sd15 %>% filter(localidade == input$sd15muni)
        b <- sd15 %>% filter(localidade == input$sd15municomp)
        a %>%
          e_charts(x = ano) %>%
          e_bar(
            serie = valor,
            name = input$sd15muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_bar(
            serie = valor,
            name = input$sd15municomp,
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
      }
    })
    ## Tabela - Taxa de Incidência da Hanseníase----
    output$sd15txt2 <- renderText({
      t152()
    })
    output$sd15tab2 <- renderReactable({
      ris <- sd15 %>%
        filter(ano == input$sd15ano, localidade == input$sd15muni) %>%
        pull(ri)
      x <-
        sd15 %>% filter(ano == input$sd15ano, localidade != "Pará")
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
            cell = data_bars(
              x,
              text_position = "above",
              box_shadow = TRUE,
              align_bars = "right",
              number_fmt = scales::number_format(
                accuracy = 0.1,
                decimal.mark = ","
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

    # 16-Taxa de Incidência da Tuberculose----
    ## Gráfico - Taxa de Incidência da Tuberculose----
    # Atualização da entrada
    sd16comp <- reactive({
      input$sd16muni
    })
    observeEvent(sd16comp(), {
      x <- sd16 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != sd16comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "sd16municomp",
        choices = c("Selecione um município", choices),
        session
      )
    })

    output$sd16txt1 <- renderText({
      t161()
    })

    output$sd16graf <- renderEcharts4r({
      req(input$sd16municomp)
      if (input$sd16municomp == "Selecione um município") {
        a <- sd16 %>% filter(localidade == input$sd16muni)
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
        a <- sd16 %>% filter(localidade == input$sd16muni)
        b <- sd16 %>% filter(localidade == input$sd16municomp)
        a %>%
          e_charts(x = ano) %>%
          e_bar(
            serie = valor,
            name = input$sd16muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_bar(
            serie = valor,
            name = input$sd16municomp,
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
      }
    })
    ## Tabela - Taxa de Incidência da Tuberculose----
    output$sd16txt2 <- renderText({
      t162()
    })
    output$sd16tab2 <- renderReactable({
      ris <- sd16 %>%
        filter(ano == input$sd16ano, localidade == input$sd16muni) %>%
        pull(ri)
      x <-
        sd16 %>% filter(ano == input$sd16ano, localidade != "Pará")
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
            cell = data_bars(
              x,
              text_position = "above",
              box_shadow = TRUE,
              align_bars = "right",
              number_fmt = scales::number_format(accuracy = 0.01, decimal.mark = ",")
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

    # 17-Proporção de Mulheres de 25 a 64 anos que Realizaram Exames Citopatológicos do Colo do Útero----
    ## Gráfico - Proporção de Mulheres de 25 a 64 anos que Realizaram Exames Citopatológicos do Colo do Útero----
    # Atualização da entrada
    sd17comp <- reactive({
      input$sd17muni
    })
    observeEvent(sd17comp(), {
      x <- sd17 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != sd17comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "sd17municomp",
        choices = c("Selecione um município", choices),
        session
      )
    })

    output$sd17txt1 <- renderText({
      t171()
    })

    output$sd17graf <- renderEcharts4r({
      req(input$sd17municomp)
      if (input$sd17municomp == "Selecione um município") {
        a <- sd17 %>% filter(localidade == input$sd17muni)
        a %>%
          e_charts(x = ano) %>%
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
            name = "Proporção",
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
        a <- sd17 %>% filter(localidade == input$sd17muni)
        b <- sd17 %>% filter(localidade == input$sd17municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$sd17muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$sd17municomp,
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
            name = "Proporção",
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
      }
    })
    ## Tabela - Proporção de Mulheres de 25 a 64 anos que Realizaram Exames Citopatológicos do Colo do Útero----
    output$sd17txt2 <- renderText({
      t172()
    })
    output$sd17tab2 <- renderReactable({
      ris <- sd17 %>%
        filter(ano == input$sd17ano, localidade == input$sd17muni) %>%
        pull(ri)
      x <-
        sd17 %>% filter(ano == input$sd17ano, localidade != "Pará")
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
            name = "Proporção",
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
              number_fmt = scales::number_format(accuracy = 0.01, decimal.mark = ",")
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

    # 18-Óbitos por Residência Neoplasias (Tumores)----
    ## Gráfico - Óbitos por Residência Neoplasias (Tumores)----
    # Atualização da entrada
    sd18comp <- reactive({
      input$sd18muni
    })
    observeEvent(sd18comp(), {
      x <- sd18 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != sd18comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "sd18municomp",
        choices = c("Selecione um município", choices),
        session
      )
    })

    output$sd18txt1 <- renderText({
      t181()
    })

    output$sd18graf <- renderEcharts4r({
      req(input$sd18municomp)
      if (input$sd18municomp == "Selecione um município") {
        a <- sd18 %>% filter(localidade == input$sd18muni)
        a %>%
          e_charts(x = ano) %>%
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
            name = "Número",
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
        a <- sd18 %>% filter(localidade == input$sd18muni)
        b <- sd18 %>% filter(localidade == input$sd18municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$sd18muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$sd18municomp,
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
            name = "Número",
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
    ## Tabela - Óbitos por Residência Neoplasias (Tumores)----
    output$sd18txt2 <- renderText({
      t182()
    })
    output$sd18tab2 <- renderReactable({
      ris <- sd18 %>%
        filter(ano == input$sd18ano, localidade == input$sd18muni) %>%
        pull(ri)
      x <-
        sd18 %>% filter(ano == input$sd18ano, localidade != "Pará")
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
            name = "Número",
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

    # 19-Óbitos por Residência Algumas Doenças Infecciosas e Parasitárias----
    ## Gráfico - Óbitos por Residência Algumas Doenças Infecciosas e Parasitárias----
    # Atualização da entrada
    sd19comp <- reactive({
      input$sd19muni
    })
    observeEvent(sd19comp(), {
      x <- sd19 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != sd19comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "sd19municomp",
        choices = c("Selecione um município", choices),
        session
      )
    })

    output$sd19txt1 <- renderText({
      t191()
    })

    output$sd19graf <- renderEcharts4r({
      req(input$sd19municomp)
      if (input$sd19municomp == "Selecione um município") {
        a <- sd19 %>% filter(localidade == input$sd19muni)
        a %>%
          e_charts(x = ano) %>%
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
            name = "Número",
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
        a <- sd19 %>% filter(localidade == input$sd19muni)
        b <- sd19 %>% filter(localidade == input$sd19municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$sd19muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$sd19municomp,
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
            name = "Número",
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
    ## Tabela - Óbitos por Residência Algumas Doenças Infecciosas e Parasitárias----
    output$sd19txt2 <- renderText({
      t192()
    })
    output$sd19tab2 <- renderReactable({
      ris <- sd19 %>%
        filter(ano == input$sd19ano, localidade == input$sd19muni) %>%
        pull(ri)
      x <-
        sd19 %>% filter(ano == input$sd19ano, localidade != "Pará")
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
            name = "Número",
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

    # 20-Óbitos por Residência Doenças do Aparelho Circulatório----
    ## Gráfico - Óbitos por Residência Doenças do Aparelho Circulatório----
    # Atualização da entrada
    sd20comp <- reactive({
      input$sd20muni
    })
    observeEvent(sd20comp(), {
      x <- sd20 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != sd20comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "sd20municomp",
        choices = c("Selecione um município", choices),
        session
      )
    })

    output$sd20txt1 <- renderText({
      t201()
    })

    output$sd20graf <- renderEcharts4r({
      req(input$sd20municomp)
      if (input$sd20municomp == "Selecione um município") {
        a <- sd20 %>% filter(localidade == input$sd20muni)
        a %>%
          e_charts(x = ano) %>%
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
            name = "Número",
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
        a <- sd20 %>% filter(localidade == input$sd20muni)
        b <- sd20 %>% filter(localidade == input$sd20municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$sd20muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$sd20municomp,
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
            name = "Número",
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
    ## Tabela - Óbitos por Residência Doenças do Aparelho Circulatório----
    output$sd20txt2 <- renderText({
      t202()
    })
    output$sd20tab2 <- renderReactable({
      ris <- sd20 %>%
        filter(ano == input$sd20ano, localidade == input$sd20muni) %>%
        pull(ri)
      x <-
        sd20 %>% filter(ano == input$sd20ano, localidade != "Pará")
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
            name = "Número",
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

    # 21-Óbitos por Residência Doenças do Aparelho Digestivo----
    ## Gráfico - Óbitos por Residência Doenças do Aparelho Digestivo----
    # Atualização da entrada
    sd21comp <- reactive({
      input$sd21muni
    })
    observeEvent(sd21comp(), {
      x <- sd21 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != sd21comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "sd21municomp",
        choices = c("Selecione um município", choices),
        session
      )
    })

    output$sd21txt1 <- renderText({
      t211()
    })

    output$sd21graf <- renderEcharts4r({
      req(input$sd21municomp)
      if (input$sd21municomp == "Selecione um município") {
        a <- sd21 %>% filter(localidade == input$sd21muni)
        a %>%
          e_charts(x = ano) %>%
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
            name = "Número",
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
        a <- sd21 %>% filter(localidade == input$sd21muni)
        b <- sd21 %>% filter(localidade == input$sd21municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$sd21muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$sd21municomp,
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
            name = "Número",
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
    ## Tabela - Óbitos por Residência Doenças do Aparelho Digestivo----
    output$sd21txt2 <- renderText({
      t212()
    })
    output$sd21tab2 <- renderReactable({
      ris <- sd21 %>%
        filter(ano == input$sd21ano, localidade == input$sd21muni) %>%
        pull(ri)
      x <-
        sd21 %>% filter(ano == input$sd21ano, localidade != "Pará")
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
            name = "Número",
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

    # 22-Óbitos por Residência Doenças do Aparelho Respiratório----
    ## Gráfico - Óbitos por Residência Doenças do Aparelho Respiratório----
    # Atualização da entrada
    sd22comp <- reactive({
      input$sd22muni
    })
    observeEvent(sd22comp(), {
      x <- sd22 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != sd22comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "sd22municomp",
        choices = c("Selecione um município", choices),
        session
      )
    })

    output$sd22txt1 <- renderText({
      t221()
    })

    output$sd22graf <- renderEcharts4r({
      req(input$sd22municomp)
      if (input$sd22municomp == "Selecione um município") {
        a <- sd22 %>% filter(localidade == input$sd22muni)
        a %>%
          e_charts(x = ano) %>%
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
            name = "Número",
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
        a <- sd22 %>% filter(localidade == input$sd22muni)
        b <- sd22 %>% filter(localidade == input$sd22municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$sd22muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$sd22municomp,
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
            name = "Número",
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
    ## Tabela - Óbitos por Residência Doenças do Aparelho Respiratório----
    output$sd22txt2 <- renderText({
      t222()
    })
    output$sd22tab2 <- renderReactable({
      ris <- sd22 %>%
        filter(ano == input$sd22ano, localidade == input$sd22muni) %>%
        pull(ri)
      x <-
        sd22 %>% filter(ano == input$sd22ano, localidade != "Pará")
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
            name = "Número",
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

    # 23-Óbitos por Residência Algumas Afecções Originadas no Período Perinatal----
    ## Gráfico - Óbitos por Residência Algumas Afecções Originadas no Período Perinatal----
    # Atualização da entrada
    sd23comp <- reactive({
      input$sd23muni
    })
    observeEvent(sd23comp(), {
      x <- sd23 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != sd23comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "sd23municomp",
        choices = c("Selecione um município", choices),
        session
      )
    })

    output$sd23txt1 <- renderText({
      t231()
    })

    output$sd23graf <- renderEcharts4r({
      req(input$sd23municomp)
      if (input$sd23municomp == "Selecione um município") {
        a <- sd23 %>% filter(localidade == input$sd23muni)
        a %>%
          e_charts(x = ano) %>%
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
            name = "Número",
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
        a <- sd23 %>% filter(localidade == input$sd23muni)
        b <- sd23 %>% filter(localidade == input$sd23municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$sd23muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$sd23municomp,
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
            name = "Número",
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
    ## Tabela - Óbitos por Residência Algumas Afecções Originadas no Período Perinatal----
    output$sd23txt2 <- renderText({
      t232()
    })
    output$sd23tab2 <- renderReactable({
      ris <- sd23 %>%
        filter(ano == input$sd23ano, localidade == input$sd23muni) %>%
        pull(ri)
      x <-
        sd23 %>% filter(ano == input$sd23ano, localidade != "Pará")
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
            name = "Número",
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

    # 24-Percentual de Óbitos por Infarto Agudo do Miocárdio (IAM)----
    ## Gráfico - Percentual de Óbitos por Infarto Agudo do Miocárdio (IAM)----
    # Atualização da entrada
    sd24comp <- reactive({
      input$sd24muni
    })
    observeEvent(sd24comp(), {
      x <- sd24 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != sd24comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "sd24municomp",
        choices = c("Selecione um município", choices),
        session
      )
    })

    output$sd24txt1 <- renderText({
      t241()
    })

    output$sd24graf <- renderEcharts4r({
      req(input$sd24municomp)
      if (input$sd24municomp == "Selecione um município") {
        a <- sd24 %>% filter(localidade == input$sd24muni)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f17701",
            name = "Percentual",
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
            name = "Percentual",
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
        a <- sd24 %>% filter(localidade == input$sd24muni)
        b <- sd24 %>% filter(localidade == input$sd24municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$sd24muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$sd24municomp,
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
            name = "Percentual",
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
      }
    })
    ## Tabela - Percentual de Óbitos por Infarto Agudo do Miocárdio (IAM)----
    output$sd24txt2 <- renderText({
      t242()
    })
    output$sd24tab2 <- renderReactable({
      ris <- sd24 %>%
        filter(ano == input$sd24ano, localidade == input$sd24muni) %>%
        pull(ri)
      x <-
        sd24 %>% filter(ano == input$sd24ano, localidade != "Pará")
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
            name = "Percentual",
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
              number_fmt = scales::number_format(accuracy = 0.01, decimal.mark = ",")
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

    # 25-Taxa de Cobertura da Atenção Primária à Saúde----
    ## Gráfico - Taxa de Cobertura da Atenção Primária à Saúde----
    # Atualização da entrada
    sd25comp <- reactive({
      input$sd25muni
    })
    observeEvent(sd25comp(), {
      x <- sd25 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != sd25comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "sd25municomp",
        choices = c("Selecione um município", choices),
        session
      )
    })

    output$sd25txt1 <- renderText({
      t251()
    })

    output$sd25graf <- renderEcharts4r({
      req(input$sd25municomp)
      if (input$sd25municomp == "Selecione um município") {
        a <- sd25 %>% filter(localidade == input$sd25muni)
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
        a <- sd25 %>% filter(localidade == input$sd25muni)
        b <- sd25 %>% filter(localidade == input$sd25municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$sd25muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$sd25municomp,
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
      }
    })
    ## Tabela - Taxa de Cobertura da Atenção Primária à Saúde----
    output$sd25txt2 <- renderText({
      t252()
    })
    output$sd25tab2 <- renderReactable({
      ris <- sd25 %>%
        filter(ano == input$sd25ano, localidade == input$sd25muni) %>%
        pull(ri)
      x <-
        sd25 %>% filter(ano == input$sd25ano, localidade != "Pará")
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
            cell = data_bars(
              x,
              text_position = "above",
              box_shadow = TRUE,
              align_bars = "right",
              number_fmt = scales::number_format(accuracy = 0.01, decimal.mark = ",")
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
    # 1-Taxa de Mortalidade Infantil----
    ## Gráfico - Taxa de Mortalidade Infantil----
    # Filtra os dados
    sd1_1 <- reactive({
      req(input$sd1municomp)
      if (input$sd1municomp == "Selecione um município") {
        a <- sd1 %>% filter(localidade == input$sd1muni)
      } else {
        a <- sd1 %>% filter(localidade == input$sd1muni)
        b <- sd1 %>% filter(localidade == input$sd1municomp)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd1_1(), {
      downset_Server("sd1_1", sd1_1(), t11())
    })
    ## Tabela - Taxa de Mortalidade Infantil----
    # Filtra os dados
    sd1_2 <- reactive({
      ris <- sd1 %>%
        filter(ano == input$sd1ano, localidade == input$sd1muni) %>%
        pull(ri)
      x <- sd1 %>%
        filter(
          ano == input$sd1ano,
          localidade != "Pará",
          ri == ri
        ) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd1_2(), {
      downset_Server("sd1_2", sd1_2(), t12())
    })

    # 2-Taxa de Mortalidade na Infância----
    ## Gráfico - Taxa de Mortalidade na Infância----
    # Filtra os dados
    sd2_1 <- reactive({
      req(input$sd2municomp)
      if (input$sd2municomp == "Selecione um município") {
        a <- sd2 %>% filter(localidade == input$sd2muni)
      } else {
        a <- sd2 %>% filter(localidade == input$sd2muni)
        b <- sd2 %>% filter(localidade == input$sd2municomp)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd2_1(), {
      downset_Server("sd2_1", sd2_1(), t21())
    })
    ## Tabela - Taxa de Mortalidade na Infância----
    # Filtra os dados
    sd2_2 <- reactive({
      ris <- sd2 %>%
        filter(ano == input$sd2ano, localidade == input$sd2muni) %>%
        pull(ri)
      x <- sd2 %>%
        filter(
          ano == input$sd2ano,
          localidade != "Pará",
          ri == ri
        ) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd2_2(), {
      downset_Server("sd2_2", sd2_2(), t22())
    })

    # 3-Taxa de Mortalidade Materna----
    ## Gráfico - Taxa de Mortalidade Materna----
    # Filtra os dados
    sd3_1 <- reactive({
      req(input$sd3municomp)
      if (input$sd3municomp == "Selecione um município") {
        a <- sd3 %>% filter(localidade == input$sd3muni)
      } else {
        a <- sd3 %>% filter(localidade == input$sd3muni)
        b <- sd3 %>% filter(localidade == input$sd3municomp)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd3_1(), {
      downset_Server("sd3_1", sd3_1(), t31())
    })
    ## Tabela - Taxa de Mortalidade Materna----
    # Filtra os dados
    sd3_2 <- reactive({
      ris <- sd3 %>%
        filter(ano == input$sd3ano, localidade == input$sd3muni) %>%
        pull(ri)
      x <- sd3 %>%
        filter(
          ano == input$sd3ano,
          localidade != "Pará",
          ri == ri
        ) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd3_2(), {
      downset_Server("sd3_2", sd3_2(), t32())
    })

    # 4-Taxa de Natalidade----
    ## Gráfico - Taxa de Natalidade----
    # Filtra os dados
    sd4_1 <- reactive({
      req(input$sd4municomp)
      if (input$sd4municomp == "Selecione um município") {
        a <- sd4 %>% filter(localidade == input$sd4muni)
      } else {
        a <- sd4 %>% filter(localidade == input$sd4muni)
        b <- sd4 %>% filter(localidade == input$sd4municomp)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd4_1(), {
      downset_Server("sd4_1", sd4_1(), t41())
    })
    ## Tabela - Taxa de Natalidade----
    # Filtra os dados
    sd4_2 <- reactive({
      ris <- sd4 %>%
        filter(ano == input$sd4ano, localidade == input$sd4muni) %>%
        pull(ri)
      x <- sd4 %>%
        filter(
          ano == input$sd4ano,
          localidade != "Pará",
          ri == ri
        ) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd4_2(), {
      downset_Server("sd4_2", sd4_2(), t42())
    })

    # 5-Taxa de Mortalidade Geral----
    ## Gráfico - Taxa de Mortalidade Geral----
    # Filtra os dados
    sd5_1 <- reactive({
      req(input$sd5municomp)
      if (input$sd5municomp == "Selecione um município") {
        a <- sd5 %>% filter(localidade == input$sd5muni)
      } else {
        a <- sd5 %>% filter(localidade == input$sd5muni)
        b <- sd5 %>% filter(localidade == input$sd5municomp)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd5_1(), {
      downset_Server("sd5_1", sd5_1(), t51())
    })
    ## Tabela - Taxa de Mortalidade Geral----
    # Filtra os dados
    sd5_2 <- reactive({
      ris <- sd5 %>%
        filter(ano == input$sd5ano, localidade == input$sd5muni) %>%
        pull(ri)
      x <- sd5 %>%
        filter(
          ano == input$sd5ano,
          localidade != "Pará",
          ri == ri
        ) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd5_2(), {
      downset_Server("sd5_2", sd5_2(), t52())
    })

    # 6-Taxa de Mortalidade por Sexo----
    ## Gráfico - Taxa de Mortalidade por Sexo----
    # Filtra os dados
    sd6_1 <- reactive({
      req(input$sd6municomp)
      if (input$sd6municomp == "Selecione um município") {
        a <-
          sd6 %>% filter(localidade == input$sd6muni, ano == input$sd6ano)
      } else {
        a <-
          sd6 %>% filter(localidade == input$sd6muni, ano == input$sd6ano)
        b <-
          sd6 %>% filter(
            localidade == input$sd6municomp,
            ano == input$sd6ano
          )
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd6_1(), {
      downset_Server("sd6_1", sd6_1(), t61())
    })
    ## Tabela - Taxa de Mortalidade por Sexo----
    # Filtra os dados
    sd6_2 <- reactive({
      x <- sd6 %>%
        filter(localidade == input$sd6muni)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd6_2(), {
      downset_Server("sd6_2", sd6_2(), t62())
    })
    ## Tabela - Taxa de Mortalidade por Sexo----
    # Filtra os dados
    sd6_3 <- reactive({
      ris <- sd6 %>%
        filter(ano == input$sd6ano2, localidade == input$sd6muni) %>%
        pull(ri)
      x <- sd6 %>%
        filter(
          ano == input$sd6ano2,
          localidade != "Pará",
          ri == ri
        )
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd6_3(), {
      downset_Server("sd6_3", sd6_3(), t63())
    })

    # 7-Número de Hospitais----
    ## Gráfico - Número de Hospitais----
    # Filtra os dados
    sd7_1 <- reactive({
      req(input$sd7municomp)
      maximo <- max(sd7$valor)
      if (input$sd7municomp == "Selecione um município") {
        a <- sd7 %>% filter(localidade == input$sd7muni)
      } else {
        a <- sd7 %>% filter(localidade == input$sd7muni)
        b <- sd7 %>% filter(localidade == input$sd7municomp)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd7_1(), {
      downset_Server("sd7_1", sd7_1(), t71())
    })
    ## Tabela - Número de Hospitais----
    # Filtra os dados
    sd7_2 <- reactive({
      ris <- sd7 %>%
        filter(ano == input$sd7ano, localidade == input$sd7muni) %>%
        pull(ri)
      x <- sd7 %>%
        filter(
          ano == input$sd7ano,
          localidade != "Pará",
          ri == ri
        ) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd7_2(), {
      downset_Server("sd7_2", sd7_2(), t72())
    })

    # 8-Número de Postos e Centros de Saúde por 10.000 Habitantes----
    ## Gráfico - Número de Postos e Centros de Saúde por 10.000 Habitantes----
    # Filtra os dados
    sd8_1 <- reactive({
      req(input$sd8municomp)
      if (input$sd8municomp == "Selecione um município") {
        a <- sd8 %>% filter(localidade == input$sd8muni)
      } else {
        a <- sd8 %>% filter(localidade == input$sd8muni)
        b <- sd8 %>% filter(localidade == input$sd8municomp)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd8_1(), {
      downset_Server("sd8_1", sd8_1(), t81())
    })
    ## Tabela - Número de Postos e Centros de Saúde por 10.000 Habitantes----
    # Filtra os dados
    sd8_2 <- reactive({
      ris <- sd8 %>%
        filter(ano == input$sd8ano, localidade == input$sd8muni) %>%
        pull(ri)
      x <- sd8 %>%
        filter(
          ano == input$sd8ano,
          localidade != "Pará",
          ri == ri
        ) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd8_2(), {
      downset_Server("sd8_2", sd8_2(), t82())
    })

    # 9-Médicos por 10 Mil Habitantes----
    ## Gráfico - Médicos por 10 Mil Habitantes----
    # Filtra os dados
    sd9_1 <- reactive({
      req(input$sd9municomp)
      if (input$sd9municomp == "Selecione um município") {
        a <- sd9 %>% filter(localidade == input$sd9muni)
      } else {
        a <- sd9 %>% filter(localidade == input$sd9muni)
        b <- sd9 %>% filter(localidade == input$sd9municomp)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd9_1(), {
      downset_Server("sd9_1", sd9_1(), t91())
    })
    ## Tabela - Médicos por 10 Mil Habitantes----
    # Filtra os dados
    sd9_2 <- reactive({
      ris <- sd9 %>%
        filter(ano == input$sd9ano, localidade == input$sd9muni) %>%
        pull(ri)
      x <- sd9 %>%
        filter(
          ano == input$sd9ano,
          localidade != "Pará",
          ri == ri
        ) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd9_2(), {
      downset_Server("sd9_2", sd9_2(), t92())
    })

    # 10-Leito Hospitalar por Mil Habitantes----
    ## Gráfico - Leito Hospitalar por Mil Habitantes----
    # Filtra os dados
    sd10_1 <- reactive({
      req(input$sd10municomp)
      if (input$sd10municomp == "Selecione um município") {
        a <- sd10 %>% filter(localidade == input$sd10muni)
      } else {
        a <- sd10 %>% filter(localidade == input$sd10muni)
        b <- sd10 %>% filter(localidade == input$sd10municomp)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd10_1(), {
      downset_Server("sd10_1", sd10_1(), t101())
    })
    ## Tabela - Leito Hospitalar por Mil Habitantes----
    # Filtra os dados
    sd10_2 <- reactive({
      ris <- sd10 %>%
        filter(ano == input$sd10ano, localidade == input$sd10muni) %>%
        pull(ri)
      x <- sd10 %>%
        filter(
          ano == input$sd10ano,
          localidade != "Pará",
          ri == ri
        ) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd10_2(), {
      downset_Server("sd10_2", sd10_2(), t102())
    })

    # 11-Percentual de Nascidos Vivos com 7 ou Mais Consultas Pré-natal----
    ## Gráfico - Percentual de Nascidos Vivos com 7 ou Mais Consultas Pré-natal----
    # Filtra os dados
    sd11_1 <- reactive({
      req(input$sd11municomp)
      if (input$sd11municomp == "Selecione um município") {
        a <- sd11 %>% filter(localidade == input$sd11muni)
      } else {
        a <- sd11 %>% filter(localidade == input$sd11muni)
        b <- sd11 %>% filter(localidade == input$sd11municomp)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd11_1(), {
      downset_Server("sd11_1", sd11_1(), t111())
    })
    ## Tabela - Percentual de Nascidos Vivos com 7 ou Mais Consultas Pré-natal----
    # Filtra os dados
    sd11_2 <- reactive({
      ris <- sd11 %>%
        filter(ano == input$sd11ano, localidade == input$sd11muni) %>%
        pull(ri)
      x <- sd11 %>%
        filter(
          ano == input$sd11ano,
          localidade != "Pará",
          ri == ri
        ) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd11_2(), {
      downset_Server("sd11_2", sd11_2(), t112())
    })

    # 12-Percentual de Nascidos Vivos por Parto Normal----
    ## Gráfico - Percentual de Nascidos Vivos por Parto Normal----
    # Filtra os dados
    sd12_1 <- reactive({
      req(input$sd12municomp)
      if (input$sd12municomp == "Selecione um município") {
        a <- sd12 %>% filter(localidade == input$sd12muni)
      } else {
        a <- sd12 %>% filter(localidade == input$sd12muni)
        b <- sd12 %>% filter(localidade == input$sd12municomp)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd12_1(), {
      downset_Server("sd12_1", sd12_1(), t121())
    })
    ## Tabela - Percentual de Nascidos Vivos por Parto Normal----
    # Filtra os dados
    sd12_2 <- reactive({
      ris <- sd12 %>%
        filter(ano == input$sd12ano, localidade == input$sd12muni) %>%
        pull(ri)
      x <- sd12 %>%
        filter(
          ano == input$sd12ano,
          localidade != "Pará",
          ri == ri
        ) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd12_2(), {
      downset_Server("sd12_2", sd12_2(), t122())
    })

    # 13-Percentual de Nascidos Vivos por Parto Cesário----
    ## Gráfico - Percentual de Nascidos Vivos por Parto Cesário----
    # Filtra os dados
    sd13_1 <- reactive({
      req(input$sd13municomp)
      if (input$sd13municomp == "Selecione um município") {
        a <- sd13 %>% filter(localidade == input$sd13muni)
      } else {
        a <- sd13 %>% filter(localidade == input$sd13muni)
        b <- sd13 %>% filter(localidade == input$sd13municomp)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd13_1(), {
      downset_Server("sd13_1", sd13_1(), t131())
    })
    ## Tabela - Percentual de Nascidos Vivos por Parto Cesário----
    # Filtra os dados
    sd13_2 <- reactive({
      ris <- sd13 %>%
        filter(ano == input$sd13ano, localidade == input$sd13muni) %>%
        pull(ri)
      x <- sd13 %>%
        filter(
          ano == input$sd13ano,
          localidade != "Pará",
          ri == ri
        ) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd13_2(), {
      downset_Server("sd13_2", sd13_2(), t132())
    })

    # 14-Percentual de Nascidos Vivos de Mães Adolescentes na Faixa Etária de 10 a 19 Anos----
    ## Gráfico - Percentual de Nascidos Vivos de Mães Adolescentes na Faixa Etária de 10 a 19 Anos----
    # Filtra os dados
    sd14_1 <- reactive({
      req(input$sd14municomp)
      if (input$sd14municomp == "Selecione um município") {
        a <- sd14 %>% filter(localidade == input$sd14muni)
      } else {
        a <- sd14 %>% filter(localidade == input$sd14muni)
        b <- sd14 %>% filter(localidade == input$sd14municomp)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd14_1(), {
      downset_Server("sd14_1", sd14_1(), t141())
    })
    ## Tabela - Percentual de Nascidos Vivos de Mães Adolescentes na Faixa Etária de 10 a 19 Anos----
    # Filtra os dados
    sd14_2 <- reactive({
      ris <- sd14 %>%
        filter(ano == input$sd14ano, localidade == input$sd14muni) %>%
        pull(ri)
      x <- sd14 %>%
        filter(
          ano == input$sd14ano,
          localidade != "Pará",
          ri == ri
        ) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd14_2(), {
      downset_Server("sd14_2", sd14_2(), t142())
    })

    # 15-Taxa de Incidência da Hanseníase----
    ## Gráfico - Taxa de Incidência da Hanseníase----
    # Filtra os dados
    sd15_1 <- reactive({
      req(input$sd15municomp)
      if (input$sd15municomp == "Selecione um município") {
        a <- sd15 %>% filter(localidade == input$sd15muni)
      } else {
        a <- sd15 %>% filter(localidade == input$sd15muni)
        b <- sd15 %>% filter(localidade == input$sd15municomp)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd15_1(), {
      downset_Server("sd15_1", sd15_1(), t151())
    })
    ## Tabela - Taxa de Incidência da Hanseníase----
    # Filtra os dados
    sd15_2 <- reactive({
      ris <- sd15 %>%
        filter(ano == input$sd15ano, localidade == input$sd15muni) %>%
        pull(ri)
      x <- sd15 %>%
        filter(
          ano == input$sd15ano,
          localidade != "Pará",
          ri == ri
        ) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd15_2(), {
      downset_Server("sd15_2", sd15_2(), t152())
    })

    # 16-Taxa de Incidência da Tuberculose----
    ## Gráfico - Taxa de Incidência da Tuberculose----
    # Filtra os dados
    sd16_1 <- reactive({
      req(input$sd16municomp)
      if (input$sd16municomp == "Selecione um município") {
        a <- sd16 %>% filter(localidade == input$sd16muni)
      } else {
        a <- sd16 %>% filter(localidade == input$sd16muni)
        b <- sd16 %>% filter(localidade == input$sd16municomp)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd16_1(), {
      downset_Server("sd16_1", sd16_1(), t161())
    })
    ## Tabela - Taxa de Incidência da Tuberculose----
    # Filtra os dados
    sd16_2 <- reactive({
      ris <- sd16 %>%
        filter(ano == input$sd16ano, localidade == input$sd16muni) %>%
        pull(ri)
      x <- sd16 %>%
        filter(
          ano == input$sd16ano,
          localidade != "Pará",
          ri == ri
        ) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd16_2(), {
      downset_Server("sd16_2", sd16_2(), t162())
    })

    # 17-Proporção de Mulheres de 25 a 64 anos que Realizaram Exames Citopatológicos do Colo do Útero----
    ## Gráfico - Proporção de Mulheres de 25 a 64 anos que Realizaram Exames Citopatológicos do Colo do Útero----
    # Filtra os dados
    sd17_1 <- reactive({
      req(input$sd17municomp)
      if (input$sd17municomp == "Selecione um município") {
        a <- sd17 %>% filter(localidade == input$sd17muni)
      } else {
        a <- sd17 %>% filter(localidade == input$sd17muni)
        b <- sd17 %>% filter(localidade == input$sd17municomp)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd17_1(), {
      downset_Server("sd17_1", sd17_1(), t171())
    })
    ## Tabela - Proporção de Mulheres de 25 a 64 anos que Realizaram Exames Citopatológicos do Colo do Útero----
    # Filtra os dados
    sd17_2 <- reactive({
      ris <- sd17 %>%
        filter(ano == input$sd17ano, localidade == input$sd17muni) %>%
        pull(ri)
      x <- sd17 %>%
        filter(
          ano == input$sd17ano,
          localidade != "Pará",
          ri == ri
        ) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd17_2(), {
      downset_Server("sd17_2", sd17_2(), t172())
    })

    # 18-Óbitos por Residência Neoplasias (Tumores)----
    ## Gráfico - Óbitos por Residência Neoplasias (Tumores)----
    # Filtra os dados
    sd18_1 <- reactive({
      req(input$sd18municomp)
      if (input$sd18municomp == "Selecione um município") {
        a <- sd18 %>% filter(localidade == input$sd18muni)
      } else {
        a <- sd18 %>% filter(localidade == input$sd18muni)
        b <- sd18 %>% filter(localidade == input$sd18municomp)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd18_1(), {
      downset_Server("sd18_1", sd18_1(), t181())
    })
    ## Tabela - Óbitos por Residência Neoplasias (Tumores)----
    # Filtra os dados
    sd18_2 <- reactive({
      ris <- sd18 %>%
        filter(ano == input$sd18ano, localidade == input$sd18muni) %>%
        pull(ri)
      x <- sd18 %>%
        filter(
          ano == input$sd18ano,
          localidade != "Pará",
          ri == ri
        ) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd18_2(), {
      downset_Server("sd18_2", sd18_2(), t182())
    })

    # 19-Óbitos por Residência Algumas Doenças Infecciosas e Parasitárias----
    ## Gráfico - Óbitos por Residência Algumas Doenças Infecciosas e Parasitárias----
    # Filtra os dados
    sd19_1 <- reactive({
      req(input$sd19municomp)
      if (input$sd19municomp == "Selecione um município") {
        a <- sd19 %>% filter(localidade == input$sd19muni)
      } else {
        a <- sd19 %>% filter(localidade == input$sd19muni)
        b <- sd19 %>% filter(localidade == input$sd19municomp)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd19_1(), {
      downset_Server("sd19_1", sd19_1(), t191())
    })
    ## Tabela - Óbitos por Residência Algumas Doenças Infecciosas e Parasitárias----
    # Filtra os dados
    sd19_2 <- reactive({
      ris <- sd19 %>%
        filter(ano == input$sd19ano, localidade == input$sd19muni) %>%
        pull(ri)
      x <- sd19 %>%
        filter(
          ano == input$sd19ano,
          localidade != "Pará",
          ri == ri
        ) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd19_2(), {
      downset_Server("sd19_2", sd19_2(), t192())
    })

    # 20-Óbitos por Residência Doenças do Aparelho Circulatório----
    ## Gráfico - Óbitos por Residência Doenças do Aparelho Circulatório----
    # Filtra os dados
    sd20_1 <- reactive({
      req(input$sd20municomp)
      if (input$sd20municomp == "Selecione um município") {
        a <- sd20 %>% filter(localidade == input$sd20muni)
      } else {
        a <- sd20 %>% filter(localidade == input$sd20muni)
        b <- sd20 %>% filter(localidade == input$sd20municomp)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd20_1(), {
      downset_Server("sd20_1", sd20_1(), t201())
    })
    ## Tabela - Óbitos por Residência Doenças do Aparelho Circulatório----
    # Filtra os dados
    sd20_2 <- reactive({
      ris <- sd20 %>%
        filter(ano == input$sd20ano, localidade == input$sd20muni) %>%
        pull(ri)
      x <- sd20 %>%
        filter(
          ano == input$sd20ano,
          localidade != "Pará",
          ri == ri
        ) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd20_2(), {
      downset_Server("sd20_2", sd20_2(), t202())
    })

    # 21-Óbitos por Residência Doenças do Aparelho Digestivo----
    ## Gráfico - Óbitos por Residência Doenças do Aparelho Digestivo----
    # Filtra os dados
    sd21_1 <- reactive({
      req(input$sd20municomp)
      if (input$sd20municomp == "Selecione um município") {
        a <- sd20 %>% filter(localidade == input$sd20muni)
      } else {
        a <- sd21 %>% filter(localidade == input$sd21muni)
        b <- sd21 %>% filter(localidade == input$sd21municomp)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd21_1(), {
      downset_Server("sd21_1", sd21_1(), t211())
    })
    ## Tabela - Óbitos por Residência Doenças do Aparelho Digestivo----
    # Filtra os dados
    sd21_2 <- reactive({
      ris <- sd21 %>%
        filter(ano == input$sd21ano, localidade == input$sd21muni) %>%
        pull(ri)
      x <- sd21 %>%
        filter(
          ano == input$sd21ano,
          localidade != "Pará",
          ri == ri
        ) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd21_2(), {
      downset_Server("sd21_2", sd21_2(), t212())
    })

    # 22-Óbitos por Residência Doenças do Aparelho Respiratório----
    ## Gráfico - Óbitos por Residência Doenças do Aparelho Respiratório----
    # Filtra os dados
    sd22_1 <- reactive({
      req(input$sd22municomp)
      if (input$sd22municomp == "Selecione um município") {
        a <- sd22 %>% filter(localidade == input$sd22muni)
      } else {
        a <- sd22 %>% filter(localidade == input$sd22muni)
        b <- sd22 %>% filter(localidade == input$sd22municomp)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd22_1(), {
      downset_Server("sd22_1", sd22_1(), t221())
    })
    ## Tabela - Óbitos por Residência Doenças do Aparelho Respiratório----
    # Filtra os dados
    sd22_2 <- reactive({
      ris <- sd22 %>%
        filter(ano == input$sd22ano, localidade == input$sd22muni) %>%
        pull(ri)
      x <- sd22 %>%
        filter(
          ano == input$sd22ano,
          localidade != "Pará",
          ri == ri
        ) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd22_2(), {
      downset_Server("sd22_2", sd22_2(), t222())
    })

    # 23-Óbitos por Residência Algumas Afecções Originadas no Período Perinatal----
    ## Gráfico - Óbitos por Residência Algumas Afecções Originadas no Período Perinatal----
    # Filtra os dados
    sd23_1 <- reactive({
      req(input$sd23municomp)
      if (input$sd23municomp == "Selecione um município") {
        a <- sd23 %>% filter(localidade == input$sd23muni)
      } else {
        a <- sd23 %>% filter(localidade == input$sd23muni)
        b <- sd23 %>% filter(localidade == input$sd23municomp)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd23_1(), {
      downset_Server("sd23_1", sd23_1(), t231())
    })
    ## Tabela - Óbitos por Residência Algumas Afecções Originadas no Período Perinatal----
    # Filtra os dados
    sd23_2 <- reactive({
      ris <- sd23 %>%
        filter(ano == input$sd23ano, localidade == input$sd23muni) %>%
        pull(ri)
      x <- sd23 %>%
        filter(
          ano == input$sd23ano,
          localidade != "Pará",
          ri == ri
        ) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd23_2(), {
      downset_Server("sd23_2", sd23_2(), t232())
    })

    # 24-Percentual de Óbitos por Infarto Agudo do Miocárdio (IAM)----
    ## Gráfico - Percentual de Óbitos por Infarto Agudo do Miocárdio (IAM)----
    # Filtra os dados
    sd24_1 <- reactive({
      req(input$sd24municomp)
      if (input$sd24municomp == "Selecione um município") {
        a <- sd24 %>% filter(localidade == input$sd24muni)
      } else {
        a <- sd24 %>% filter(localidade == input$sd24muni)
        b <- sd24 %>% filter(localidade == input$sd24municomp)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd24_1(), {
      downset_Server("sd24_1", sd24_1(), t241())
    })
    ## Tabela - Percentual de Óbitos por Infarto Agudo do Miocárdio (IAM)----
    # Filtra os dados
    sd24_2 <- reactive({
      ris <- sd24 %>%
        filter(ano == input$sd24ano, localidade == input$sd24muni) %>%
        pull(ri)
      x <- sd24 %>%
        filter(
          ano == input$sd24ano,
          localidade != "Pará",
          ri == ri
        ) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd24_2(), {
      downset_Server("sd24_2", sd24_2(), t242())
    })

    # 25-Taxa de Cobertura da Atenção Primária à Saúde----
    ## Gráfico - Taxa de Cobertura da Atenção Primária à Saúde----
    # Filtra os dados
    sd25_1 <- reactive({
      req(input$sd25municomp)
      if (input$sd25municomp == "Selecione um município") {
        a <- sd25 %>% filter(localidade == input$sd25muni)
      } else {
        a <- sd25 %>% filter(localidade == input$sd25muni)
        b <- sd25 %>% filter(localidade == input$sd25municomp)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd25_1(), {
      downset_Server("sd25_1", sd25_1(), t251())
    })
    ## Tabela - Taxa de Cobertura da Atenção Primária à Saúde----
    # Filtra os dados
    sd25_2 <- reactive({
      ris <- sd25 %>%
        filter(ano == input$sd25ano, localidade == input$sd25muni) %>%
        pull(ri)
      x <- sd25 %>%
        filter(
          ano == input$sd25ano,
          localidade != "Pará",
          ri == ri
        ) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(sd25_2(), {
      downset_Server("sd25_2", sd25_2(), t252())
    })
  })
}

# Play do Módulo
# ui <- dashboardPage(header = dashboardHeader(),
#                     sidebar = dashboardSidebar(),
#                     body = dashboardBody(fluidPage(social_saude_mp_ui("social_saude_mp"))))
#
#
# server <- function(input, output) {
#   social_saude_mp_Server("social_saude_mp")
# }
#
# shinyApp(ui, server)
