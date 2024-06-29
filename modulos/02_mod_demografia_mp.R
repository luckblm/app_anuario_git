# Funções de módulo de Demografia Estadual
# Função de UI

demografia_mp_ui <- function(id) {
  fluidPage(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "estilo.css")
    ),
    div(
      class = "navbar_demografia",
      navbarPage(
        tags$b("Demografia - Municípios"),
        navbarMenu(
          "Indicadores",
          # 1 - População Total e Estimativas poplulacionais----
          tabPanel(
            "Estimativas Populacionais",
            panel( 
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Estimativas Populacionais"), 
              ## Controle----
              tags$div(
                class = "seletor2",
                # select Município
                pickerInput(
                  inputId = NS(id, "demo1muni"),
                  label = "Município",
                  choices = demo1 %>%
                    filter(localidade != "Pará") %>%
                    pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico de Barras População Total e Estimativas poplulacionais----
              box(
                title = textOutput(NS(id, "demo1txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                column(
                  3,
                  pickerInput(
                    inputId = NS(id, "demo1municomp"),
                    label = "Comparar Município",
                    choices = NULL,
                    width = "200px",
                    options = list(`none-selected-text` = "Selecione um município")
                  )
                ),
                column(
                  12,
                  withSpinner(
                    echarts4rOutput(NS(id, "demo1graf")),
                    type = 8,
                    color = "#3C8DBD",
                    size = 0.5
                  )
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(
                      style = "font-family: sans-serif;",
                      tags$b("Fonte:"), "IBGE"
                    ),
                    tags$h6(
                      style = "font-family: sans-serif;",
                      tags$b("Elaboração:"), "FAPESPA"
                    )
                  ),
                  downset_ui(NS(id, "d1_1"))
                )
              ),
              ## Tabela - População Total e Estimativas poplulacionais----
              box(
                title = textOutput(NS(id, "demo1txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "demo1ano"),
                  label = "Ano",
                  choices = sort(unique(demo1[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "demo1tab"), height = "400px"),
                  type = 8,
                  color = "#3C8DBD",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(
                      style = "font-family: sans-serif;",
                      tags$b("Fonte:"), "IBGE"
                    ),
                    tags$h6(
                      style = "font-family: sans-serif;",
                      tags$b("Elaboração:"), "FAPESPA"
                    )
                  ),
                  downset_ui(NS(id, "d1_2"))
                )
              )
            )
          ),
          # 2 - População por Faixa Etária----
          tabPanel(
            "População por Faixa Etária",
            panel( 
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "População por Faixa Etária"), 
              ## Controle----
              tags$div(
                class = "seletor2",
                # select Município
                pickerInput(
                  inputId = NS(id, "demo2muni"),
                  label = "Município",
                  choices = demo2 %>%
                    filter(localidade != "Pará") %>%
                    pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - População por Faixa Etária----
              box(
                title = textOutput(NS(id, "demo2txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                column(
                  2,
                  pickerInput(
                    inputId = NS(id, "demo2municomp"),
                    label = "Comparar Município",
                    choices = NULL,
                    width = "200px",
                    options =
                      list(`none-selected-text` = "Selecione um município")
                  )
                ),
                column(
                  6,
                  pickerInput(
                    inputId = NS(id, "demo2ano"),
                    label = "Ano",
                    choices = sort(unique(demo2[["ano"]]), decreasing = T),
                    width = "100px"
                  )
                ),
                column(
                  12,
                  withSpinner(
                    echarts4rOutput(NS(id, "demo2graf")),
                    type = 8,
                    color = "#3C8DBD",
                    size = 0.5
                  )
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(
                      style = "font-family: sans-serif;",
                      tags$b("Fonte:"), "IBGE"
                    ),
                    tags$h6(
                      style = "font-family: sans-serif;",
                      tags$b("Elaboração:"), "FAPESPA"
                    )
                  ),
                  downset_ui(NS(id, "d2_1"))
                )
              )
            ),
            fluidRow(
              ## Tabela - População por Faixa Etária----
              box(
                title = textOutput(NS(id, "demo2txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "demo2tab")),
                  type = 8,
                  color = "#3C8DBD",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(
                      style = "font-family: sans-serif;",
                      tags$b("Fonte:"), "IBGE"
                    ),
                    tags$h6(
                      style = "font-family: sans-serif;",
                      tags$b("Elaboração:"), "FAPESPA"
                    )
                  ),
                  downset_ui(NS(id, "d2_2"))
                )
              )
            )
          ),
          # 3 - População por sexo----
          tabPanel(
            "População por sexo",
            panel(
              ## Controle----
              heading =
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                 "População por sexo"), 
              tags$div(
                class = "seletor2",
                # select Município
                pickerInput(
                  inputId = NS(id, "demo3muni"),
                  label = "Município",
                  choices = demo3 %>%
                    filter(localidade != "Pará") %>%
                    pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - População por sexo----
              box(
                title = textOutput(NS(id, "demo3txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                # Comparar muicípios
                column(
                  2,
                  pickerInput(
                    inputId = NS(id, "demo3municomp"),
                    label = "Comparar Município",
                    choices = NULL,
                    width = "200px",
                    options =
                      list(`none-selected-text` = "Selecione um município")
                  )
                ),
                column(
                  6,
                  pickerInput(
                    inputId = NS(id, "demo3ano"),
                    label = "Ano",
                    choices = sort(unique(demo3[["ano"]]), decreasing = T),
                    width = "100px"
                  )
                ),
                column(
                  12,
                  withSpinner(
                    echarts4rOutput(NS(id, "demo3graf"), height = "350px"),
                    type = 8,
                    color = "#3C8DBD",
                    size = 0.5
                  )
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(
                      style = "font-family: sans-serif;",
                      tags$b("Fonte:"), "IBGE"
                    ),
                    tags$h6(
                      style = "font-family: sans-serif;",
                      tags$b("Elaboração:"), "FAPESPA"
                    )
                  ),
                  downset_ui(NS(id, "d3_1"))
                )
              ),
              ## Tabela - População por sexo----
              box(
                title = textOutput(NS(id, "demo3txt2")),
                collapsible = T,
                collapsed = F,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "demo3tab"), height = "350px"),
                  type = 8,
                  color = "#3C8DBD",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(
                      style = "font-family: sans-serif;",
                      tags$b("Fonte:"), "IBGE"
                    ),
                    tags$h6(
                      style = "font-family: sans-serif;",
                      tags$b("Elaboração:"), "FAPESPA"
                    )
                  ),
                  downset_ui(NS(id, "d3_2"))
                )
              )
            )
          ),
          # 4 - Razão de sexo----
          tabPanel(
            "Razão de sexo",
            panel( ## Controle----
                   heading =
                     h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                        "Razão de sexo"), 
                   tags$div(
                class = "seletor2",
                # select Município
                pickerInput(
                  inputId = NS(id, "demo4muni"),
                  label = "Município",
                  choices = demo4 %>%
                    filter(localidade != "Pará") %>%
                    pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Razão de sexo----
              box(
                title = textOutput(NS(id, "demo4txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "demo4municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "demo4graf")),
                  type = 8,
                  color = "#3C8DBD",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(
                      style = "font-family: sans-serif;",
                      tags$b("Fonte:"), "IBGE"
                    ),
                    tags$h6(
                      style = "font-family: sans-serif;",
                      tags$b("Elaboração:"), "FAPESPA"
                    )
                  ),
                  downset_ui(NS(id, "d4_1"))
                )
              )
            ),
            fluidRow(
              ## Tabela - Razão de sexo----
              box(
                title = textOutput(NS(id, "demo4txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "demo4tab"), height = "448px"),
                  type = 8,
                  color = "#3C8DBD",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(
                      style = "font-family: sans-serif;",
                      tags$b("Fonte:"), "IBGE"
                    ),
                    tags$h6(
                      style = "font-family: sans-serif;",
                      tags$b("Elaboração:"), "FAPESPA"
                    )
                  ),
                  downset_ui(NS(id, "d4_2"))
                )
              ),
              ## Gráfico - Razão de sexo----
              box(
                title = textOutput(NS(id, "demo4txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  billboarderOutput(NS(id, "demo4grafpie"), height = "400px"),
                  type = 8,
                  color = "#3C8DBD",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(
                    style = "font-family: sans-serif;",
                    tags$b("Fonte:"), "IBGE"
                  ),
                  tags$h6(
                    style = "font-family: sans-serif;",
                    tags$b("Elaboração:"), "FAPESPA"
                  ),
                  tags$h6(
                    style = "font-family: sans-serif;",
                    tags$b("Nota:"),
                    "O percentual de homens e mulheres
                    é calculado com base na contagem da razão de sexo por ano.
                    A razão de sexo é obtida dividindo o número
                    de homens pelo número de mulheres.
                    Os percentuais são calculados dividindo o número de homens ou
                    mulheres pelo total (homens + mulheres) e multiplicando por 100.
                    Isso permite acompanhar as flutuações na
                    distribuição de gênero ao longo do tempo."
                  )
                ))
              )
            )
          ),
          # 5 - Proporção de idosos----
          tabPanel(
            "Proporção de idosos",
            panel( ## Controle----
                   heading =
                     h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                        "Proporção de idosos"), 
                   tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "demo5muni"),
                  label = "Município",
                  choices = demo5 %>%
                    filter(localidade != "Pará") %>%
                    pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Proporção de idosos----
              box(
                title = textOutput(NS(id, "demo5txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                # Comparar muicípios
                pickerInput(
                  inputId = NS(id, "demo5municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "demo5graf")),
                  type = 8,
                  color = "#3C8DBD",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(
                      style = "font-family: sans-serif;",
                      tags$b("Fonte:"), "IBGE"
                    ),
                    tags$h6(
                      style = "font-family: sans-serif;",
                      tags$b("Elaboração:"), "FAPESPA"
                    )
                  ),
                  downset_ui(NS(id, "d5_1"))
                )
              )
            ),
            fluidRow(
              ## Tabela - Proporção de idosos----
              box(
                title = textOutput(NS(id, "demo5txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "demo5ano"),
                  label = "Ano",
                  choices = sort(unique(demo5[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "demo5tab2"), height = "400px"),
                  type = 8,
                  color = "#3C8DBD",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(
                      style = "font-family: sans-serif;",
                      tags$b("Fonte:"), "IBGE"
                    ),
                    tags$h6(
                      style = "font-family: sans-serif;",
                      tags$b("Elaboração:"), "FAPESPA"
                    )
                  ),
                  downset_ui(NS(id, "d5_2"))
                )
              )
            )
          ),
          # 6 - Índice de envelhecimento----
          tabPanel(
            "Índice de envelhecimento",
            panel( ## Controle----
                   heading =
                     h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                        "Índice de envelhecimento"),
                   tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "demo6muni"),
                  label = "Município",
                  choices = demo6 %>%
                    filter(localidade != "Pará") %>%
                    pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Índice de envelhecimento----
              box(
                title = textOutput(NS(id, "demo6txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                # Comparar muicípios
                pickerInput(
                  inputId = NS(id, "demo6municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "demo6graf")),
                  type = 8,
                  color = "#3C8DBD",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(
                      style = "font-family: sans-serif;",
                      tags$b("Fonte:"), "IBGE"
                    ),
                    tags$h6(
                      style = "font-family: sans-serif;",
                      tags$b("Elaboração:"), "FAPESPA"
                    )
                  ),
                  downset_ui(NS(id, "d6_1"))
                )
              )
            ),
            fluidRow(
              ## Tabela - Índice de envelhecimento----
              box(
                title = textOutput(NS(id, "demo6txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "demo6ano"),
                  label = "Ano",
                  choices = sort(unique(demo6[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "demo6tab2"), height = "400px"),
                  type = 8,
                  color = "#3C8DBD",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(
                      style = "font-family: sans-serif;",
                      tags$b("Fonte:"), "IBGE"
                    ),
                    tags$h6(
                      style = "font-family: sans-serif;",
                      tags$b("Elaboração:"), "FAPESPA"
                    )
                  ),
                  downset_ui(NS(id, "d6_2"))
                )
              )
            )
          ),
          # 7 - Razão de dependência----
          tabPanel(
            "Razão de dependência",
            panel( ## Controle----
                   heading =
                     h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                        "Razão de dependência"),
                   tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "demo7muni"),
                  label = "Município",
                  choices = demo7 %>%
                    filter(localidade != "Pará") %>%
                    pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Razão de dependência----
              box(
                title = textOutput(NS(id, "demo7txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                # Comparar muicípios
                pickerInput(
                  inputId = NS(id, "demo7municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "demo7graf")),
                  type = 8,
                  color = "#3C8DBD",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(
                      style = "font-family: sans-serif;",
                      tags$b("Fonte:"), "IBGE"
                    ),
                    tags$h6(
                      style = "font-family: sans-serif;",
                      tags$b("Elaboração:"), "FAPESPA"
                    )
                  ),
                  downset_ui(NS(id, "d7_1"))
                )
              )
            ),
            fluidRow(
              ## Tabela - Razão de dependência----
              box(
                title = textOutput(NS(id, "demo7txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "demo7ano"),
                  label = "Ano",
                  choices = sort(unique(demo7[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "demo7tab2"), height = "400px"),
                  type = 8,
                  color = "#3C8DBD",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(
                      style = "font-family: sans-serif;",
                      tags$b("Fonte:"), "IBGE"
                    ),
                    tags$h6(
                      style = "font-family: sans-serif;",
                      tags$b("Elaboração:"), "FAPESPA"
                    )
                  ),
                  downset_ui(NS(id, "d7_2"))
                )
              )
            )
          ),
          # 8 - Taxa de fecundidade Total----
          tabPanel(
            "Taxa de fecundidade Total",
            panel( ## Controle----
                   heading =
                     h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                        "Taxa de fecundidade Total"),
                   tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "demo8muni"),
                  label = "Município",
                  choices = demo8 %>%
                    filter(localidade != "Pará") %>%
                    pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Taxa de fecundidade Total----
              box(
                title = textOutput(NS(id, "demo8txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                # Comparar muicípios
                pickerInput(
                  inputId = NS(id, "demo8municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "demo8graf")),
                  type = 8,
                  color = "#3C8DBD",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(
                      style = "font-family: sans-serif;",
                      tags$b("Fonte:"), "IBGE"
                    ),
                    tags$h6(
                      style = "font-family: sans-serif;",
                      tags$b("Elaboração:"), "FAPESPA"
                    )
                  ),
                  downset_ui(NS(id, "d8_1"))
                )
              )
            ),
            fluidRow(
              ## Tabela - Taxa de fecundidade Total----
              box(
                title = textOutput(NS(id, "demo8txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "demo8ano"),
                  label = "Ano",
                  choices = sort(unique(demo8[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "demo8tab2"), height = "400px"),
                  type = 8,
                  color = "#3C8DBD",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(
                      style = "font-family: sans-serif;",
                      tags$b("Fonte:"), "IBGE"
                    ),
                    tags$h6(
                      style = "font-family: sans-serif;",
                      tags$b("Elaboração:"), "FAPESPA"
                    )
                  ),
                  downset_ui(NS(id, "d8_2"))
                )
              )
            )
          ),
          # 9 - Taxa específica de fecundidade por faixa etária----
          tabPanel(
            "Taxa específica de fecundidade por faixa etária",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",    
                   "Taxa específica de fecundidade por faixa etária"),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "demo9muni"),
                  label = "Município",
                  choices = demo9 %>%
                    filter(localidade != "Pará") %>%
                    pull(localidade) %>% unique(),
                  width = "200px"
                )
              ),
              tags$div(class = "seletor3", )
            ),
            fluidRow(
              ## Gráfico - População por Faixa Etária----
              box(
                title = textOutput(NS(id, "demo9txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                column(
                  2,
                  pickerInput(
                    inputId = NS(id, "demo9municomp"),
                    label = "Comparar Município",
                    choices = NULL,
                    width = "200px",
                    options = list(`none-selected-text` = "Selecione um município")
                  )
                ),
                column(
                  6,
                  pickerInput(
                    inputId = NS(id, "demo9ano"),
                    label = "Ano",
                    choices = sort(unique(demo9[["ano"]]), decreasing = T),
                    width = "100px"
                  )
                ),
                column(
                  12,
                  withSpinner(
                    echarts4rOutput(NS(id, "demo9graf")),
                    type = 8,
                    color = "#3C8DBD",
                    size = 0.5
                  )
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(
                      style = "font-family: sans-serif;",
                      tags$b("Fonte:"), "IBGE"
                    ),
                    tags$h6(
                      style = "font-family: sans-serif;",
                      tags$b("Elaboração:"), "FAPESPA"
                    )
                  ),
                  downset_ui(NS(id, "d9_1"))
                )
              )
            ),
            fluidRow(
              ## Tabela - População por Faixa Etária----
              box(
                title = textOutput(NS(id, "demo9txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "demo9tab")),
                  type = 8,
                  color = "#3C8DBD",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(
                      style = "font-family: sans-serif;",
                      tags$b("Fonte:"), "IBGE"
                    ),
                    tags$h6(
                      style = "font-family: sans-serif;",
                      tags$b("Elaboração:"), "FAPESPA"
                    )
                  ),
                  downset_ui(NS(id, "d9_2"))
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
demografia_mp_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    #TÍTULOS----
    # 1- População Total e Estimativas Populacionais ----
    ## Gráfico  de linha - População Total e Estimativas poplulacionais----
    t11 <- reactive({
    req(input$demo1municomp)
      if (input$demo1municomp == "Selecione um município") {
        paste0(
          "População Total e Estimativas Populacionais, ",
          input$demo1muni,
          " - ",
          min(demo1$ano),
          " a ",
          max(demo1$ano)
        )
      } else {
        paste0(
          "População Total e Estimativas Populacionais, ",
          input$demo1muni,
          " x ",
          input$demo1municomp,
          " - ",
          min(demo1$ano),
          " a ",
          max(demo1$ano)
        )
      }
    })
    ## Tabela - População Total e Estimativas Populacionais ----
    t12 <- reactive({
    ri <- demo1 %>%
        filter(
          ano == input$demo1ano,
          localidade == input$demo1muni
        ) %>%
        pull(ri)
      paste0(
        "População Total e Estimativas Populacionais por Município, Região de Integração ",
        ri,
        " - ",
        input$demo1ano
      )  
    })

    # 2-População por Faixa Etária----
    ## Gráfico - População por Faixa Etária----
    t21 <- reactive({
    req(input$demo2municomp)
      if (input$demo2municomp == "Selecione um município") {
        paste0(
          "População por Faixa Etária, ",
          input$demo2muni,
          " - ",
          input$demo2ano
        )
      } else {
        paste0(
          "População por Faixa Etária, ",
          input$demo2muni,
          " x ",
          input$demo2municomp,
          " - ",
          input$demo2ano
        )
      }  
    })
    ## Tabela - População por Faixa Etária----
    t22 <- reactive({
    paste0(
        "População por Faixa Etária, ",
        input$demo2muni,
        " - ",
        min(demo2$ano),
        " a ",
        max(demo2$ano)
      )  
    })

    # 3-População por sexo----
    ## Gráfico - População por sexo----
    t31 <- reactive({
     req(input$demo3municomp)
      if (input$demo3municomp == "Selecione um município") {
        paste0(
          "População por sexo, ",
          input$demo3muni,
          " - ",
          input$demo3ano
        )
      } else {
        paste0(
          "População por sexo, ",
          input$demo3muni,
          " x ",
          input$demo3municomp,
          " - ",
          input$demo3ano
        )
      } 
    })
    ## Tabela - População por sexo----
    t32 <- reactive({
    paste0(
        "População por sexo, ",
        input$demo3muni,
        " - ",
        min(demo3$ano),
        " a ",
        max(demo3$ano)
      )  
    })
    # 4-Razão de sexo  ----
    ## Gráfico - Razão de sexo----
    t41 <- reactive({
    req(input$demo4municomp)
      if (input$demo4municomp == "Selecione um município") {
        paste0(
          "Razão de sexo, ",
          input$demo4muni,
          " - ",
          min(demo4$ano),
          " a ",
          max(demo4$ano)
        )
      } else {
        paste0(
          "Razão de sexo, ",
          input$demo4muni,
          " x ",
          input$demo4municomp,
          " - ",
          min(demo4$ano),
          " a ",
          max(demo4$ano)
        )
      }  
    })
    ## Tabela - Razão de sexo----
    t42 <- reactive({
    paste0(
        "Razão de Sexo e Predominância de Gênero, ",
        input$demo4muni,
        " - ",
        min(demo4$ano),
        " a ",
        max(demo4$ano)
      )  
    })
    ## Gráfico - Razão de sexo----
    t43 <- reactive({
    paste0(
        "Predominância de Gênero, ",
        input$demo4muni,
        " - ",
        min(demo4$ano),
        " a ",
        max(demo4$ano)
      )  
    })
    # 5-Proporção de idosos  ----
    ## Proporção de idosos----
    t51 <- reactive({
    req(input$demo5municomp)
      if (input$demo5municomp == "Selecione um município") {
        paste0(
          "Proporção de idosos, ",
          input$demo5muni,
          " - ",
          max(demo5$ano),
          " a ",
          min(demo5$ano)
        )
      } else {
        paste0(
          "Proporção de idosos, ",
          input$demo5muni,
          " x ",
          input$demo5municomp,
          " - ",
          max(demo5$ano),
          " a ",
          min(demo5$ano)
        )
      }  
    })
    ## Tabela - Proporção de idosos da mesma Região de Integração----
    t52 <- reactive({
    ri <- demo5 %>%
        filter(
          ano == input$demo5ano,
          localidade == input$demo5muni
        ) %>%
        pull(ri)
      paste0(
        "Proporção de idosos por Município, Região de Integração ",
        ri,
        " - ",
        input$demo5ano
      )  
    })
    # 6-Índice de envelhecimento ----
    ## Gráfico - Índice de envelhecimento ----
    t61 <- reactive({
    req(input$demo6municomp)
      if (input$demo6municomp == "Selecione um município") {
        paste0(
          "Índice de envelhecimento, ",
          input$demo6muni,
          " - ",
          min(demo6$ano),
          " a ",
          max(demo6$ano)
        )
      } else {
        paste0(
          "Índice de envelhecimento, ",
          input$demo6muni,
          " x ",
          input$demo6municomp,
          " - ",
          max(demo6$ano),
          " a ",
          min(demo6$ano)
        )
      }  
    })
    ## Tabela - Índice de envelhecimento da mesma Região de Integração ----
    t62 <- reactive({
    ri <- demo6 %>%
        filter(
          ano == input$demo6ano,
          localidade == input$demo6muni
        ) %>%
        pull(ri)
      paste0(
        "Índice de envelhecimento por Município, Região de Integração ",
        ri,
        " - ",
        input$demo6ano
      )  
    })
    # 7-Razão de dependência ----
    ## Gráfico - Razão de dependência ----
    t71 <- reactive({
    req(input$demo7municomp)
      if (input$demo7municomp == "Selecione um município") {
        paste0(
          "Razão de dependência, ",
          input$demo7muni,
          " - ",
          max(demo7$ano),
          " a ",
          min(demo7$ano)
        )
      } else {
        paste0(
          "Razão de dependência, ",
          input$demo7muni,
          " x ",
          input$demo7municomp,
          " - ",
          max(demo7$ano),
          " a ",
          min(demo7$ano)
        )
      }  
    })
    ## Tabela - Razão de dependência da mesma Região de Integração----
    t72 <- reactive({
    ri <- demo7 %>%
        filter(
          ano == input$demo7ano,
          localidade == input$demo7muni
        ) %>%
        pull(ri)
      paste0(
        "Razão de dependência por Município, Região de Integração ",
        ri,
        " - ",
        input$demo7ano
      )  
    })
    # 8-Taxa de fecundidade Total ----
    ## Gráfico - Taxa de fecundidade Total ----
    t81 <- reactive({
    req(input$demo8municomp)
      if (input$demo8municomp == "Selecione um município") {
        paste0(
          "Taxa de fecundidade Total, ",
          input$demo8muni, " - ", max(demo8$ano), " a ", min(demo8$ano)
        )
      } else {
        paste0(
          "Taxa de fecundidade Total, ",
          input$demo8muni, " x ", input$demo8municomp, " - ", max(demo8$ano),
          " a ", min(demo8$ano)
        )
      }  
    })
    ## Tabela - Taxa de fecundidade Total da mesma Região de Integração ----
    t82 <- reactive({
    ri <- demo8 %>%
        filter(
          ano == input$demo8ano,
          localidade == input$demo8muni
        ) %>%
        pull(ri)
      paste0(
        "Taxa de fecundidade Total por Município, Região de Integração ",
        ri, " - ", input$demo8ano
      )  
    })
    # 9-Taxa específica de fecundidade por faixa etária----
    ## Gráfico - Taxa específica de fecundidade por faixa etária----
    t91 <- reactive({
    req(input$demo9municomp)
      if (input$demo9municomp == "Selecione um município") {
        paste0(
          "Taxa específica de fecundidade por faixa etária, ",
          input$demo9muni, " - ", input$demo9ano
        )
      } else {
        paste0(
          "Taxa específica de fecundidade por faixa etária, ",
          input$demo9muni, " x ", input$demo9municomp, " - ", input$demo9ano
        )
      }  
    })
    ## Tabela - Taxa específica de fecundidade por faixa etária----
    t92 <- reactive({
    paste0(
        "Taxa específica de fecundidade por faixa etária, ",
        input$demo9muni,
        " - ",
        min(demo9$ano),
        " a ",
        max(demo9$ano)
      )  
    })

    #VISUALIZAÇÃO----
    # 1- População Total e Estimativas Populacionais ----
    ## Gráfico  de linha - População Total e Estimativas poplulacionais----
    # Atualização da entrada
    demo1comp <- reactive({
      input$demo1muni
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(demo1comp(), {
      x <- demo1 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != demo1comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "demo1municomp",
        choices = c("Selecione um município", choices),
        session
      )
    })
    ## Título
    output$demo1txt1 <- renderText({
    t11()  
    })

    output$demo1graf <- renderEcharts4r({
      req(input$demo1municomp)
      if (input$demo1municomp == "Selecione um município") {
        a <- demo1 %>% filter(localidade == input$demo1muni)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#3C8DBD",
            name = "População",
            legend = F,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter =
              e_tooltip_pointer_formatter("decimal", digits = 0, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Ano",
            splitLine = list(show = T),
            nameTextStyle = list(fontWeight = "bold", fontSize = 14)
          ) %>%
          e_y_axis(
            name = "População",
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
        a <- demo1 %>% filter(localidade == input$demo1muni)
        b <- demo1 %>% filter(localidade == input$demo1municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$demo1muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$demo1municomp,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter =
              e_tooltip_pointer_formatter("decimal", digits = 0, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Ano",
            splitLine = list(show = T),
            nameTextStyle = list(fontWeight = "bold", fontSize = 14)
          ) %>%
          e_y_axis(
            name = "População",
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
    ## Tabela - População Total e Estimativas Populacionais ----
    output$demo1txt2 <- renderText({
    t12()  
    })
    output$demo1tab <- renderReactable({
      ris <- demo1 %>%
        filter(
          ano == input$demo1ano,
          localidade == input$demo1muni
        ) %>%
        pull(ri)
      x <- demo1 %>%
        filter(ano == input$demo1ano, localidade != "Pará") %>%
        select(ri, localidade, valor)
      x <- x %>% filter(ri == ris)
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
            format = colFormat(separators = T, locales = "pt-BR")
          )
        ),
        defaultColDef = colDef(
          footerStyle = list(fontWeight = "bold"),
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

    # 2-População por Faixa Etária----
    # Atualização da entrada
    demo2comp <- reactive({
      input$demo2muni
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(demo2comp(), {
      x <- demo2 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != demo2comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "demo2municomp",
        choices = c("Selecione um município", choices),
        session
      )
    })

    demo2comp1 <- reactive({
      input$demo2muni
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(demo2comp1(), {
      x <- demo2 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != demo2comp1())

      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "demo2municomp1",
        choices = c("Selecione um município", choices),
        session
      )
    })

    ## Gráfico - População por Faixa Etária----
    ## Título
    output$demo2txt1 <- renderText({
      t21()
    })

    output$demo2graf <- renderEcharts4r({
      req(input$demo2municomp)
      if (input$demo2municomp == "Selecione um município") {
        a <-
          demo2 %>% filter(
            localidade == input$demo2muni,
            ano == input$demo2ano
          )
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            color = "#3C8DBD",
            name = "População",
            legend = F,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter =
              e_tooltip_pointer_formatter("decimal", digits = 0, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Faixa Etária",
            nameLocation = "middle",
            nameTextStyle =
              list(
                fontWeight = "bold",
                padding = c(20, 0, 0, 0),
                fontSize = 14
              )
          ) %>%
          e_y_axis(
            name = "População",
            nameTextStyle =
              list(
                fontWeight = "bold",
                fontSize = 14
              ),
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
          e_grid(show = T)
      } else {
        a <-
          demo2 %>% filter(
            localidade == input$demo2muni,
            ano == input$demo2ano
          )
        b <-
          demo2 %>% filter(
            localidade == input$demo2municomp,
            ano == input$demo2ano
          )
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            name = input$demo2muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, categoria) %>%
          e_bar(
            serie = valor,
            name = input$demo2municomp,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter =
              e_tooltip_pointer_formatter("decimal", digits = 0, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Faixa Etária",
            nameLocation = "middle",
            nameTextStyle =
              list(
                fontWeight = "bold",
                padding = c(20, 0, 0, 0),
                fontSize = 14
              )
          ) %>%
          e_y_axis(
            name = "População",
            nameTextStyle =
              list(
                fontWeight = "bold",
                fontSize = 14
              ),
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
          e_grid(show = T)
      }
    })
    ## Tabela - População por Faixa Etária----
    output$demo2txt2 <- renderText({
      t22()
    })
    output$demo2tab <- renderReactable({
      x <- demo2 %>%
        filter(localidade == input$demo2muni) %>%
        pivot_wider(names_from = ano, values_from = valor) %>%
        select(c(-1:-6))
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
          categoria = colDef(
            name = "Faixa Etária",
            align = "left",
            style = list(backgroundColor = "#f7f7f8")
          )
        ),
        defaultColDef =
          colDef(
            footerStyle = list(fontWeight = "bold"),
            format = colFormat(separators = T,),
            headerStyle = list(background = "#f7f7f8"),
            cell = data_bars(
              x,
              fill_color = viridis(5),
              box_shadow = TRUE,
              align_bars = "left",
              text_position = "above",
              number_fmt =
                scales::number_format(
                  big.mark = ".", 
                  decimal.mark = ",")
            )
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
    # 3-População por sexo----
    ## Gráfico - População por sexo----
    demo3comp <- reactive({
      input$demo3muni
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(demo3comp(), {
      x <- demo3 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != demo3comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "demo3municomp",
        choices = c("Selecione um município", choices),
        session
      )
    })

    ## Título
    output$demo3txt1 <- renderText({
      t31()
    })

    output$demo3graf <- renderEcharts4r({
      req(input$demo3municomp)
      if (input$demo3municomp == "Selecione um município") {
        a <-
          demo3 %>% filter(
            localidade == input$demo3muni,
            ano == input$demo3ano
          )
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            color = "#3C8DBD",
            name = "População",
            legend = F,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            barWidth = "20%",
            itemStyle = list(barBorderRadius = 5, barWidth = "50%")
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter =
              e_tooltip_pointer_formatter("decimal", digits = 0, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Sexo",
            nameLocation = "middle",
            nameTextStyle = list(
              fontWeight = "bold",
              padding = c(20, 0, 0, 0),
              fontSize = 14
            )
          ) %>%
          e_y_axis(
            name = "População",
            nameTextStyle =
              list(fontWeight = "bold", fontSize = 14),
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
          e_grid(show = T)
      } else {
        a <-
          demo3 %>% filter(
            localidade == input$demo3muni,
            ano == input$demo3ano
          )
        b <-
          demo3 %>% filter(
            localidade == input$demo3municomp,
            ano == input$demo3ano
          )
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            name = input$demo3muni,
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
            name = input$demo3municomp,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            barWidth = "20%",
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter =
              e_tooltip_pointer_formatter("decimal", digits = 0, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Sexo",
            nameLocation = "middle",
            nameTextStyle = list(
              fontWeight = "bold",
              padding = c(20, 0, 0, 0),
              fontSize = 14
            )
          ) %>%
          e_y_axis(
            name = "População",
            nameTextStyle =
              list(fontWeight = "bold", fontSize = 14),
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
          e_grid(show = T)
      }
    })
    ## Tabela - População por sexo----
    output$demo3txt2 <- renderText({
      t32()
    })
    output$demo3tab <- renderReactable({
      x <- demo3 %>%
        filter(localidade == input$demo3muni) %>%
        pivot_wider(names_from = categoria, values_from = valor) %>%
        mutate(Masc_percentual = (Masculino / (Masculino + Feminino)) * 100) %>%
        mutate(Fem_percentual = (Feminino / (Masculino + Feminino)) * 100) %>%
        select(-1:-6)
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
          Masculino =
            colDef(name = "Masculino", format = colFormat(separators = T)),
          Feminino =
            colDef(name = "Feminino", format = colFormat(separators = T)),
          Masc_percentual =
            colDef(name = "Masculino(%)", format = colFormat(digits = 1)),
          Fem_percentual =
            colDef(name = "Feminino(%)", format = colFormat(digits = 1))
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
    # 4-Razão de sexo  ----
    ## Gráfico - Razão de sexo----
    # Atualização da entrada
    demo4comp <- reactive({
      input$demo4muni
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(demo4comp(), {
      x <- demo4 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != demo4comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "demo4municomp",
        choices = c("Selecione um município", choices),
        session
      )
    })

    output$demo4txt1 <- renderText({
      t41()
    })
    # Gráfico de linha e barras
    output$demo4graf <- renderEcharts4r({
      req(input$demo4municomp)
      if (input$demo4municomp == "Selecione um município") {
        a <- demo4 %>% filter(localidade == input$demo4muni)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#3C8DBD",
            name = "Razão",
            legend = F,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter =
              e_tooltip_pointer_formatter("decimal", digits = 1, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Ano",
            splitLine = list(show = T),
            nameTextStyle = list(fontWeight = "bold", fontSize = 14)
          ) %>%
          e_y_axis(
            name = "Razão",
            nameTextStyle = list(fontWeight = "bold", fontSize = 14),
            scale = T,
            axisLabel = list(
              formatter = htmlwidgets::JS(
                "
              function (value, index) {
              return value.toLocaleString('pt-BR',
              { minimumFractionDigits: 1, maximumFractionDigits: 1 });
              }
            "
              )
            )
          ) %>%
          e_locale("pt-Br") %>%
          e_datazoom(toolbox = F, fillerColor = "#E5F5F9") %>%
          e_grid(show = T)
      } else {
        a <- demo4 %>% filter(localidade == input$demo4muni)
        b <-
          demo4 %>% filter(localidade == input$demo4municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$demo4muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$demo4municomp,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter =
              e_tooltip_pointer_formatter("decimal", digits = 1, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Ano",
            splitLine = list(show = T),
            nameTextStyle = list(fontWeight = "bold", fontSize = 14)
          ) %>%
          e_y_axis(
            name = "Razão",
            nameTextStyle = list(fontWeight = "bold", fontSize = 14),
            scale = T,
            axisLabel = list(
              formatter = htmlwidgets::JS(
                "
              function (value, index) {
              return value.toLocaleString('pt-BR',
              { minimumFractionDigits: 1, maximumFractionDigits: 1 });
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

    ## Tabela - Razão de sexo----
    ## Título
    output$demo4txt2 <- renderText({
      t42()
    })
    output$demo4tab <- renderReactable({
      demo4$valor <- round(demo4$valor, digits = 1)
      demo4$pop <- demo4$valor
      demo4$pop[demo4$pop < 100] <- 1
      demo4$pop[demo4$pop > 100] <- 2
      demo4$pop[demo4$pop == 100] <- 3
      demo4$pop <- as.character(demo4$pop)
      demo4$pop[demo4$pop == "1"] <- "Mulheres"
      demo4$pop[demo4$pop == "2"] <- "Homens"
      demo4$pop[demo4$pop == "3"] <- "Equivalente"
      demo4 %>%
        filter(localidade == input$demo4muni) %>%
        select(ano, valor, pop) %>%
        reactable(
          defaultSorted = c("ano", "valor"),
          defaultPageSize = 10,
          striped = FALSE,
          highlight = TRUE,
          bordered = TRUE,
          outlined = TRUE,
          resizable = TRUE,
          showSortable = TRUE,
          pagination = F,
          columns = list(
            ano = colDef(name = "Ano",align = "center"),
            valor = colDef(
              name = "Razão",
              align = "center",
              format = colFormat(
                digits = 1,
                separators = T,
                locales = "pt-BR"
              )
            ),
            pop = colDef(name = "Predominância", align = "center")
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
    ## Gráfico - Razão de sexo----
    ## Título
    output$demo4txt3 <- renderText({
    t43()  
    })
    output$demo4grafpie <- renderBillboarder({
      demo4$valor <- round(demo4$valor, digits = 1)
      demo4$pop <- demo4$valor
      demo4$pop[demo4$pop < 100] <- 1
      demo4$pop[demo4$pop > 100] <- 2
      demo4$pop[demo4$pop == 100] <- 3
      demo4$pop <- as.character(demo4$pop)
      demo4$pop[demo4$pop == "1"] <- "Mulhres"
      demo4$pop[demo4$pop == "2"] <- "Homens"
      demo4$pop[demo4$pop == "3"] <- "Equivalente"

      x <- demo4 %>%
        filter(localidade == input$demo4muni) %>%
        select(pop) %>%
        table() %>%
        prop.table() %>%
        as_tibble() %>%
        rename(Sexo = 1, Percentual = 2) %>%
        mutate(Percentual = round((Percentual * 100), digits = 2))
      billboarder() %>%
        bb_piechart(x,
          label = list(
            format = htmlwidgets::JS(
              "function (value, index)
                        {return value.toLocaleString('pt-BR',
                        { minimumFractionDigits: 2, maximumFractionDigits: 2 });
                       }"
            )
          )
        ) %>%
        bb_tooltip(label = list(
          format = htmlwidgets::JS(
            " function (value, index)
            {return value.toLocaleString('pt-BR',
            { minimumFractionDigits: 2, maximumFractionDigits: 2 });
                            }"
          )
        ))
    })

    # 5-Proporção de idosos  ----
    ## Gráfico - Proporção de idosos----
    # Atualização da entrada
    demo5comp <- reactive({
      input$demo5muni
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(demo5comp(), {
      x <- demo5 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != demo5comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "demo5municomp",
        choices = c("Selecione um município", choices),
        session
      )
    })

    output$demo5txt1 <- renderText({
      t51()
    })

    output$demo5graf <- renderEcharts4r({
      req(input$demo5municomp)
      if (input$demo5municomp == "Selecione um município") {
        a <- demo5 %>% filter(localidade == input$demo5muni)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#3C8DBD",
            name = "Proporção",
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
            name = "Proporção",
            nameTextStyle = list(fontWeight = "bold", fontSize = 14),
            scale = T,
            axisLabel = list(
              formatter = htmlwidgets::JS(
                "
              function (value, index) {
              return value.toLocaleString('pt-BR',
              { minimumFractionDigits: 2, maximumFractionDigits: 2 });
              }
            "
              )
            )
          ) %>%
          e_locale("pt-Br") %>%
          e_datazoom(toolbox = F, fillerColor = "#E5F5F9") %>%
          e_grid(show = T)
      } else {
        a <- demo5 %>% filter(localidade == input$demo5muni)
        b <-
          demo5 %>% filter(localidade == input$demo5municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$demo5muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$demo5municomp,
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
            name = "Proporção",
            nameTextStyle = list(fontWeight = "bold", fontSize = 14),
            scale = T,
            axisLabel = list(
              formatter = htmlwidgets::JS(
                "
              function (value, index) {
              return value.toLocaleString('pt-BR',
              { minimumFractionDigits: 2, maximumFractionDigits: 2 });
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
    ## Tabela - Proporção de idosos da mesma Região de Integração----
    output$demo5txt2 <- renderText({
      t52()
    })
    output$demo5tab2 <- renderReactable({
      ris <- demo5 %>%
        filter(
          ano == input$demo5ano,
          localidade == input$demo5muni
        ) %>%
        pull(ri)
      x <-
        demo5 %>% filter(ano == input$demo5ano, localidade != "Pará")
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
          ri = colDef(name = "Região de Integração"),
          posicao = colDef(name = "nº", width = 50),
          localidade = colDef(name = "Municípios"),
          ano = colDef(align = "center"),
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
    # 6-Índice de envelhecimento ----
    ## Gráfico - Índice de envelhecimento ----
    # Atualização da entrada
    demo6comp <- reactive({
      input$demo6muni
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(demo6comp(), {
      x <- demo6 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != demo6comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "demo6municomp",
        choices = c("Selecione um município", choices),
        session
      )
    })

    output$demo6txt1 <- renderText({
      t61()
    })

    output$demo6graf <- renderEcharts4r({
      req(input$demo6municomp)
      if (input$demo6municomp == "Selecione um município") {
        a <- demo6 %>% filter(localidade == input$demo6muni)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#3C8DBD",
            name = "índice",
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
              return value.toLocaleString('pt-BR',
              { minimumFractionDigits: 2, maximumFractionDigits: 2 });
              }
            "
              )
            )
          ) %>%
          e_locale("pt-Br") %>%
          e_datazoom(toolbox = F, fillerColor = "#E5F5F9") %>%
          e_grid(show = T)
      } else {
        a <- demo6 %>% filter(localidade == input$demo6muni)
        b <-
          demo6 %>% filter(localidade == input$demo6municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$demo6muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$demo6municomp,
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
            name = "Índice",
            nameTextStyle = list(fontWeight = "bold", fontSize = 14),
            scale = T,
            axisLabel = list(
              formatter = htmlwidgets::JS(
                "
              function (value, index) {
              return value.toLocaleString('pt-BR',
              { minimumFractionDigits: 2, maximumFractionDigits: 2 });
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
    ## Tabela - Índice de envelhecimento da mesma Região de Integração ----
    output$demo6txt2 <- renderText({
      t62()
    })
    output$demo6tab2 <- renderReactable({
      ris <- demo6 %>%
        filter(
          ano == input$demo6ano,
          localidade == input$demo6muni
        ) %>%
        pull(ri)
      x <-
        demo6 %>% filter(ano == input$demo6ano, localidade != "Pará")
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
          ri = colDef(name = "Região de Integração"),
          posicao = colDef(name = "nº", width = 50),
          localidade = colDef(name = "Municípios"),
          ano = colDef(align = "center"),
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
              number_fmt =
                scales::number_format(accuracy = 0.01, decimal.mark = ",")
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
    # 7-Razão de dependência ----
    ## Gráfico - Razão de dependência ----
    # Atualização da entrada
    demo7comp <- reactive({
      input$demo7muni
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(demo7comp(), {
      x <- demo7 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != demo7comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "demo7municomp",
        choices = c("Selecione um município", choices),
        session
      )
    })

    output$demo7txt1 <- renderText({
      t71()
    })

    output$demo7graf <- renderEcharts4r({
      req(input$demo7municomp)
      if (input$demo7municomp == "Selecione um município") {
        a <- demo7 %>% filter(localidade == input$demo7muni)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#3C8DBD",
            name = "Razão",
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
            name = "Razão",
            nameTextStyle = list(fontWeight = "bold", fontSize = 14),
            scale = T,
            axisLabel = list(
              formatter = htmlwidgets::JS(
                "
              function (value, index) {
              return value.toLocaleString('pt-BR',
              { minimumFractionDigits: 2, maximumFractionDigits: 2 });
              }
            "
              )
            )
          ) %>%
          e_locale("pt-Br") %>%
          e_datazoom(toolbox = F, fillerColor = "#E5F5F9") %>%
          e_grid(show = T)
      } else {
        a <- demo7 %>% filter(localidade == input$demo7muni)
        b <-
          demo7 %>% filter(localidade == input$demo7municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$demo7muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$demo7municomp,
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
            name = "Razão",
            nameTextStyle = list(fontWeight = "bold", fontSize = 14),
            scale = T,
            axisLabel = list(
              formatter = htmlwidgets::JS(
                "
              function (value, index) {
              return value.toLocaleString('pt-BR',
              { minimumFractionDigits: 2, maximumFractionDigits: 2 });
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
    ## Tabela - Razão de dependência da mesma Região de Integração----
    output$demo7txt2 <- renderText({
      t72()
    })

    output$demo7tab2 <- renderReactable({
      ris <- demo7 %>%
        filter(
          ano == input$demo7ano,
          localidade == input$demo7muni
        ) %>%
        pull(ri)
      x <-
        demo7 %>% filter(ano == input$demo7ano, localidade != "Pará")
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
          ri = colDef(name = "Região de Integração"),
          posicao = colDef(name = "nº", width = 50),
          localidade = colDef(name = "Municípios"),
          ano = colDef(align = "center"),
          valor = colDef(
            name = "Razão",
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
    # 8-Taxa de fecundidade Total ----
    ## Gráfico - Taxa de fecundidade Total ----
    # Atualização da entrada
    demo8comp <- reactive({
      input$demo8muni
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(demo8comp(), {
      x <- demo8 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != demo8comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "demo8municomp",
        choices = c("Selecione um município", choices),
        session
      )
    })

    output$demo8txt1 <- renderText({
      t81()
    })

    output$demo8graf <- renderEcharts4r({
      req(input$demo8municomp)
      if (input$demo8municomp == "Selecione um município") {
        a <- demo8 %>% filter(localidade == input$demo8muni)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#3C8DBD",
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
              formatter = htmlwidgets::JS(
                "
              function (value, index) {
              return value.toLocaleString('pt-BR',
              { minimumFractionDigits: 2, maximumFractionDigits: 2 });
              }
            "
              )
            )
          ) %>%
          e_locale("pt-Br") %>%
          e_datazoom(toolbox = F, fillerColor = "#E5F5F9") %>%
          e_grid(show = T)
      } else {
        a <- demo8 %>% filter(localidade == input$demo8muni)
        b <-
          demo8 %>% filter(localidade == input$demo8municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$demo8muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$demo8municomp,
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
            nameTextStyle = list(fontWeight = "bold", fontSize = 14),
            scale = T,
            axisLabel = list(
              formatter = htmlwidgets::JS(
                "
              function (value, index) {
              return value.toLocaleString('pt-BR',
              { minimumFractionDigits: 2, maximumFractionDigits: 2 });
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
    ## Tabela-Taxa de fecundidade Total da mesma Região de Integração ----
    output$demo8txt2 <- renderText({
      t82()
    })

    output$demo8tab2 <- renderReactable({
      ris <- demo8 %>%
        filter(
          ano == input$demo8ano,
          localidade == input$demo8muni
        ) %>%
        pull(ri)
      x <-
        demo8 %>% filter(ano == input$demo8ano, localidade != "Pará")
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
          ri = colDef(name = "Região de Integração"),
          posicao = colDef(name = "nº", width = 50),
          localidade = colDef(name = "Municípios"),
          ano = colDef(align = "center"),
          valor = colDef(
            name = "Taxa",
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
    # 9-Taxa específica de fecundidade por faixa etária----
    ## Gráfico - Taxa específica de fecundidade por faixa etária----
    # Atualização da entrada
    demo9comp <- reactive({
      input$demo9muni
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(demo9comp(), {
      x <- demo9 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != demo9comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "demo9municomp",
        choices = c("Selecione um município", choices),
        session
      )
    })

    ## Título
    output$demo9txt1 <- renderText({
      t91()
    })
    output$demo9graf <- renderEcharts4r({
      req(input$demo9municomp)
      if (input$demo9municomp == "Selecione um município") {
        a <-
          demo9 %>% filter(
            localidade == input$demo9muni,
            ano == input$demo9ano
          )
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            color = "#3C8DBD",
            name = "Taxa",
            legend = F,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            barWidth = "40%",
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
            name = "Faixa Etária",
            nameLocation = "middle",
            splitLine = list(show = T),
            nameTextStyle = list(
              fontWeight = "bold",
              padding = c(20, 0, 0, 0),
              fontSize = 14
            )
          ) %>%
          e_y_axis(
            name = "Razão",
            nameTextStyle = list(fontWeight = "bold", fontSize = 14),
            scale = T,
            axisLabel = list(
              formatter = htmlwidgets::JS(
                "
              function (value, index) {
              return value.toLocaleString('pt-BR',
              { minimumFractionDigits: 2, maximumFractionDigits: 2 });
              }
            "
              )
            )
          ) %>%
          e_locale("pt-Br") %>%
          e_grid(show = T)
      } else {
        a <-
          demo9 %>% filter(
            localidade == input$demo9muni,
            ano == input$demo9ano
          )
        b <-
          demo9 %>% filter(
            localidade == input$demo9municomp,
            ano == input$demo9ano
          )
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            name = input$demo9muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, categoria) %>%
          e_bar(
            serie = valor,
            name = input$demo9municomp,
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
            name = "Faixa Etária",
            nameLocation = "middle",
            splitLine = list(show = T),
            nameTextStyle = list(
              fontWeight = "bold",
              padding = c(20, 0, 0, 0),
              fontSize = 14
            )
          ) %>%
          e_y_axis(
            name = "Razão",
            nameTextStyle = list(fontWeight = "bold", fontSize = 14),
            scale = T,
            axisLabel = list(
              formatter = htmlwidgets::JS(
                "
              function (value, index) {
              return value.toLocaleString('pt-BR',
              { minimumFractionDigits: 2, maximumFractionDigits: 2 });
              }
            "
              )
            )
          ) %>%
          e_locale("pt-Br") %>%
          e_grid(show = T)
      }
    })
    ## Tabela-Taxa específica de fecundidade por faixa etária----
    output$demo9txt2 <- renderText({
      t92()
    })
    output$demo9tab <- renderReactable({
      x <- demo9 %>%
        filter(localidade == input$demo9muni) %>%
        pivot_wider(names_from = ano, values_from = valor) %>%
        select(c(-1:-6))
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
          categoria = colDef(
            name = "Faixa Etária",
            align = "left",
            style = list(backgroundColor = "#f7f7f8")
          )
        ),
        defaultColDef =
          colDef(
            footerStyle = list(fontWeight = "bold"),
            format = colFormat(separators = T, digits = 2),
            headerStyle = list(background = "#f7f7f8"),
            cell = data_bars(
              x,
              fill_color = viridis(5),
              box_shadow = TRUE,
              align_bars = "right",
              text_position = "above",
              number_fmt =
                scales::number_format(
                  accuracy =0.01,
                  big.mark = ".", 
                  decimal.mark = ",")
            )
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
    #DOWNLOAD----
    # 1 - População Total e Estimativas poplulacionais----
    ## Gráfico de linha População Total e Estimativas poplulacionais----
    # Filtra os dados
    d1_1 <- reactive({
      req(input$demo1municomp)
      if (input$demo1municomp == "Selecione um município") {
        demo1 %>% filter(localidade == input$demo1muni)
      } else {
        a <- demo1 %>% filter(localidade == input$demo1muni)
        b <- demo1 %>% filter(localidade == input$demo1municomp)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(d1_1(),{
    t11()
    downset_Server("d1_1", d1_1(), t11())  
    })
    
    
    ## Tabela - População Total e Estimativas poplulacionais----
    # Filtra os dados
    d1_2 <- reactive({
      ri <- demo1 %>%
        filter(
          ano == input$demo1ano,
          localidade == input$demo1muni
        ) %>%
        pull(ri)
      x <- demo1 %>%
        filter(
          ano == input$demo1ano,
          localidade != "Pará"
        ) %>%
        filter(ri == ri) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(d1_2(),{
    t12()
    downset_Server("d1_2", d1_2(), t12())  
    })
    
    # 2 - População por Faixa Etária----
    ## Gráfico de barras - População por Faixa Etária----
    # Filtra os dados
    d2_1 <- reactive({
      req(input$demo2municomp)
      if (input$demo2municomp == "Selecione um município") {
        a <- demo2 %>%
          filter(localidade == input$demo2muni, ano == input$demo2ano)
      } else {
        a <-
          demo2 %>%
          filter(localidade == input$demo2muni, ano == input$demo2ano)
        b <-
          demo2 %>%
          filter(localidade == input$demo2municomp, ano == input$demo2ano)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(d2_1(),{
    t21()
    downset_Server("d2_1", d2_1(), t21())  
    })
    
    ## Tabela - População por Faixa Etária----
    # Filtra os dados
    d2_2 <- reactive({
      x <- demo2 %>%
        filter(localidade == input$demo2muni) %>%
        pivot_wider(names_from = ano, values_from = valor)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(d2_2(),{
    t22()
    downset_Server("d2_2", d2_2(), t22())  
    })
    
    
    # 3 - População por sexo----
    ## Gráfico - População por sexo----
    # Filtra os dados
    d3_1 <- reactive({
      req(input$demo3municomp)
      if (input$demo3municomp == "Selecione um município") {
        a <- demo3 %>%
          filter(localidade == input$demo3muni)
      } else {
        a <-
          demo3 %>% filter(localidade == input$demo3muni)
        b <-
          demo3 %>% filter(localidade == input$demo3municomp)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(d3_1(),{
    t31()  
    downset_Server("d3_1", d3_1(), t31())
      })
    

    ## Tabela - População por sexo----
    # Filtra os dados
    d3_2 <- reactive({
      x <- demo3 %>%
        filter(localidade == input$demo3muni) %>%
        pivot_wider(names_from = categoria, values_from = valor) %>%
        mutate(Masc_percentual = (Masculino / (Masculino + Feminino)) * 100) %>%
        mutate(Fem_percentual = (Feminino / (Masculino + Feminino)) * 100)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(d3_2(),{
    t32()  
    downset_Server("d3_2", d3_2(), t32())
      })
    

    # 4 - Razão de sexo----
    ## Gráfico - Razão de sexo----
    # Filtra os dados
    d4_1 <- reactive({
      req(input$demo4municomp)
      if (input$demo4municomp == "Selecione um município") {
        a <- demo4 %>%
          filter(localidade == input$demo4muni)
      } else {
        a <-
          demo4 %>% filter(localidade == input$demo4muni)
        b <-
          demo4 %>% filter(localidade == input$demo4municomp)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(d4_1(),{
    t41()  
    downset_Server("d4_1", d4_1(), t41())
      })
    
    
    ## Tabela - Razão de sexo----
    # Filtra os dados
    d4_2 <- reactive({
      demo4$valor <- round(demo4$valor, digits = 1)
      demo4$pop <- demo4$valor
      demo4$pop[demo4$pop < 100] <- 1
      demo4$pop[demo4$pop > 100] <- 2
      demo4$pop[demo4$pop == 100] <- 3
      demo4$pop <- as.character(demo4$pop)
      demo4$pop[demo4$pop == "1"] <- "Mulheres"
      demo4$pop[demo4$pop == "2"] <- "Homens"
      demo4$pop[demo4$pop == "3"] <- "Equivalente"
      demo4 %>%
        filter(localidade == input$demo4muni)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(d4_2(),{
    t42()  
    downset_Server("d4_2", d4_2(), t42())
      })
    
    
    # 5 - Proporção de idosos----
    ## Gráfico - Proporção de idosos----
    # Filtra os dados
    d5_1 <- reactive({
      req(input$demo5municomp)
      if (input$demo5municomp == "Selecione um município") {
        a <- demo5 %>%
          filter(localidade == input$demo5muni)
      } else {
        a <-
          demo5 %>%
          filter(localidade == input$demo5muni)
        b <-
          demo5 %>%
          filter(localidade == input$demo5municomp)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(d5_1(),{
    t51()  
    downset_Server("d5_1", d5_1(), t51())
      })
    
    
    ## Tabela - Proporção de idosos----
    # Filtra os dados
    d5_2 <- reactive({
      ri <-
        demo5 %>%
        filter(
          ano == input$demo5ano,
          localidade == input$demo5muni
        ) %>%
        pull(ri)
      x <-
        demo5 %>%
        filter(
          ano == input$demo5ano,
          localidade != "Pará",
          ri == ri
        ) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(d5_2(),{
    t52()  
    downset_Server("d5_2", d5_2(), t52())
      })
    
    
    # 6 - Índice de envelhecimento----
    ## Gráfico - Índice de envelhecimento----
    # Filtra os dados
    d6_1 <- reactive({
      req(input$demo6municomp)
      if (input$demo6municomp == "Selecione um município") {
        a <- demo6 %>%
          filter(localidade == input$demo6muni)
      } else {
        a <-
          demo6 %>%
          filter(localidade == input$demo6muni)
        b <-
          demo6 %>%
          filter(localidade == input$demo6municomp)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(d6_1(),{
    t61()  
    downset_Server("d6_1", d6_1(), t61())
      })
    

    ## Tabela - Índice de envelhecimento----
    # Filtra os dados
    d6_2 <- reactive({
      ri <- demo6 %>%
        filter(
          ano == input$demo6ano,
          localidade == input$demo6muni
        ) %>%
        pull(ri)
      x <-
        demo6 %>%
        filter(
          ano == input$demo6ano,
          localidade != "Pará",
          ri == ri
        ) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(d6_2(),{
    t62()  
    downset_Server("d6_2", d6_2(), t62())
      })
    

    # 7 - Razão de dependência----
    ## Gráfico - Razão de dependência----
    # Filtra os dados
    d7_1 <- reactive({
      req(input$demo7municomp)
      if (input$demo7municomp == "Selecione um município") {
        a <- demo7 %>%
          filter(localidade == input$demo7muni)
      } else {
        a <- demo7 %>% filter(localidade == input$demo7muni)
        b <-
          demo7 %>% filter(localidade == input$demo7municomp)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(d7_1(),{
    d7_1()  
    downset_Server("d7_1", d7_1(), t71())
      })
    
    
    ## Tabela - Razão de dependência----
    # Filtra os dados
    d7_2 <- reactive({
      ri <- demo7 %>%
        filter(
          ano == input$demo7ano,
          localidade == input$demo7muni
        ) %>%
        pull(ri)
      x <-
        demo7 %>%
        filter(
          ano == input$demo7ano,
          localidade != "Pará",
          ri == ri
        ) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(d7_2(),{
    d7_2()  
    downset_Server("d7_2", d7_2(), t72())
      })
    

    # 8 - Taxa de fecundidade Total----
    ## Gráfico - Taxa de fecundidade Total----
    # Filtra os dados
    d8_1 <- reactive({
      req(input$demo8municomp)
      if (input$demo8municomp == "Selecione um município") {
        a <- demo8 %>%
          filter(localidade == input$demo8muni)
      } else {
        a <-
          demo8 %>% filter(localidade == input$demo8muni)
        b <-
          demo8 %>% filter(localidade == input$demo8municomp)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(d8_1(),{
    d8_1()  
    downset_Server("d8_1", d8_1(), t81())
      })
    
    
    ## Tabela - Taxa de fecundidade Total----
    # Filtra os dados
    d8_2 <- reactive({
      ri <- demo8 %>%
        filter(
          ano == input$demo8ano,
          localidade == input$demo8muni
        ) %>%
        pull(ri)
      x <-
        demo8 %>%
        filter(
          ano == input$demo8ano,
          localidade != "Pará",
          ri == ri
        ) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(d8_2(),{
    d8_2()  
    downset_Server("d8_2", d8_2(), t82())
      })
    
    
    # 9 - Taxa específica de fecundidade por faixa etária----
    ## Gráfico - População por Faixa Etária----
    # Filtra os dados
    d9_1 <- reactive({
      req(input$demo9municomp)
      if (input$demo9municomp == "Selecione um município") {
        a <- demo9 %>%
          filter(
            localidade == input$demo9muni,
            ano == input$demo9ano
          )
      } else {
        a <-
          demo9 %>%
          filter(localidade == input$demo9muni, ano == input$demo9ano)
        b <-
          demo9 %>% filter(
            localidade == input$demo9municomp,
            ano == input$demo9ano
          )
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(d9_1(),{
    d9_1()  
    downset_Server("d9_1", d9_1(), t91())
      })
    
   
    ## Tabela-Taxa específica de fecundidade por faixa etária----
    # Filtra os dados
    d9_2 <- reactive({
      x <- demo9 %>%
        filter(localidade == input$demo9muni) %>%
        pivot_wider(names_from = ano, values_from = valor)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(d9_2(),{
    d9_2()  
    downset_Server("d9_2", d9_2(), t92())
      }) 
   
    
  })
}

# # Play do Módulo
# ui <- dashboardPage(
#   header = dashboardHeader(),
#   sidebar = dashboardSidebar(),
#   body = dashboardBody(
#     fluidPage(demografia_mp_ui("demografia_mp"))
#   )
# )
# 
# server <- function(input, output) {
#   demografia_mp_Server("demografia_mp")
# }
# 
# shinyApp(ui, server)
