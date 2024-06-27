# Funções de módulo de Infraestrutura - Municipal
# Função de UI
infraestrutura_mp_ui <- function(id) {
  fluidPage(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "estilo.css")
    ),
    div(
      class = "navbar_infraestrutura",
      navbarPage(
        tags$b("Infraestrutura - Municípios"),
        navbarMenu(
          "Indicadores",
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
                  inputId = NS(id, "inf1muni"),
                  label = "Município",
                  choices = inf1 %>% 
                    filter(localidade != "Pará") %>% 
                    pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Total da Frota de Veículos Subdivididos em Licenciados e Não Licenciados----
              box(
                title = textOutput(NS(id, "inf1txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                column(
                  2,
                  pickerInput(
                    inputId = NS(id, "inf1municomp1"),
                    label = "Comparar Município",
                    choices = NULL,
                    width = "200px",
                    options = list(`none-selected-text` = "Selecione um município")
                  )
                ),
                column(
                  2,
                  pickerInput(
                    inputId = NS(id, "inf1ano1"),
                    label = "Ano",
                    choices = sort(unique(inf1[["ano"]]), decreasing = T),
                    width = "100px"
                  )
                ),
                column(
                  12,
                  withSpinner(
                    echarts4rOutput(NS(id, "inf1graf"), height = "326px"),
                    type = 8,
                    color = "#c800c8",
                    size = 0.5
                  )
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DETRAN"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                    tags$h6(tags$b("Nota:"), "Frota = Total de Veículos Licenciados + Não licenciados")
                  ),
                  downset_ui(NS(id, "inf1_1"))
                )
              ),
              ## Tabela - Total da Frota de Veículos Subdivididos em Licenciados e Não Licenciados----
              box(
                title = textOutput(NS(id, "inf1txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "inf1tab"),height = "400px"),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DETRAN"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                    tags$h6(tags$b("Nota:"), "Frota = Total de Veículos Licenciados + Não licenciados")
                  ),
                  downset_ui(NS(id, "inf1_2"))
                )
              )
            ),
            fluidRow(
              ## Gráfico - Total da Frota de Veículos Subdivididos em Licenciados e Não Licenciados----
              box(
                title = textOutput(NS(id, "inf1txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "inf1municomp2"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "inf1graf2")),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "DETRAN"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "inf1_3"))
                )
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
                  inputId = NS(id, "inf2muni"),
                  label = "Município",
                  choices = inf2 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Consumidores de Energia Elétrica Total----
              box(
                title = textOutput(NS(id, "inf2txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                # Comparar municípios
                pickerInput(
                  inputId = NS(id, "inf2municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "inf2graf")),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "EQUATORIAL ENERGIA"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "inf2_1"))
                )
              )
            ),
            fluidRow(
              ## Tabela - Consumidores de Energia Elétrica Total----
              box(
                title = textOutput(NS(id, "inf2txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "inf2ano"),
                  label = "Ano",
                  choices = sort(unique(inf2[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "inf2tab2"),height = "400px"),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "EQUATORIAL ENERGIA"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "inf2_2"))
                )
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
                  inputId = NS(id, "inf3muni"),
                  label = "Município",
                  choices = inf3 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Consumidores de Energia Elétrica por Tipo----
              box(
                title = textOutput(NS(id, "inf3txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                column(
                  1,
                  # Selecionar Ano
                  pickerInput(
                    inputId = NS(id, "inf3ano1"),
                    label = "Ano",
                    choices = sort(unique(inf3[["ano"]]), decreasing = T),
                    width = "100px"
                  )
                ),
                column(
                  6,
                  # Comparar municipio
                  pickerInput(
                    inputId = NS(id, "inf3municomp1"),
                    label = "Comparar Município",
                    choices = NULL,
                    width = "200px",
                    options = list(`none-selected-text` = "Selecione um município")
                  )
                ),
                column(
                  12,
                  withSpinner(
                    echarts4rOutput(NS(id, "inf3graf"), height = "326px"),
                    type = 8,
                    color = "#c800c8",
                    size = 0.5
                  )
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "EQUATORIAL ENERGIA"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                    tags$h6(tags$b("Outros:"), "Corresponde a soma dos consumidores dos tipos Rural,
                        Poder Público, Iluminação Pública, Serviço Público e Consumo Próprio")
                  ),
                  downset_ui(NS(id, "inf3_1"))
                )
              ),
              ## Tabela - Consumidores de Energia Elétrica por Tipo----
              box(
                title = textOutput(NS(id, "inf3txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "inf3tab"),height = "400px"),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "EQUATORIAL ENERGIA"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                    tags$h6(tags$b("Outros:"), "Corresponde a soma dos consumidores dos tipos Rural,
                        Poder Público, Iluminação Pública, Serviço Público e Consumo Próprio")
                  ),
                  downset_ui(NS(id, "inf3_2"))
                )
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
                  inputId = NS(id, "inf4muni"),
                  label = "Município",
                  choices = inf4 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Consumo de Energia Elétrica Total (kWH)----
              box(
                title = textOutput(NS(id, "inf4txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                # Comparar municípios
                pickerInput(
                  inputId = NS(id, "inf4municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "inf4graf")),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "EQUATORIAL ENERGIA"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "inf4_1"))
                )
              )
            ),
            fluidRow(
              ## Tabela - Consumo de Energia Elétrica Total (kWH)----
              box(
                title = textOutput(NS(id, "inf4txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "inf4ano"),
                  label = "Ano",
                  choices = sort(unique(inf4[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "inf4tab2"),height = "400px"),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "EQUATORIAL ENERGIA"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "inf4_2"))
                )
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
                  inputId = NS(id, "inf5muni"),
                  label = "Município",
                  choices = inf5 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Consumo de Energia Elétrica por Tipo----
              box(
                title = textOutput(NS(id, "inf5txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                column(
                  2,
                  # Comparar município
                  pickerInput(
                    inputId = NS(id, "inf5municomp1"),
                    label = "Comparar Município",
                    choices = NULL,
                    width = "200px",
                    options = list(`none-selected-text` = "Selecione um município")
                  )
                ),
                column(
                  1,
                  # Selecionar ano
                  pickerInput(
                    inputId = NS(id, "inf5ano1"),
                    label = "Ano",
                    choices = sort(unique(inf5[["ano"]]), decreasing = T),
                    width = "100px"
                  )
                ),
                column(
                  12,
                  withSpinner(
                    echarts4rOutput(NS(id, "inf5graf")),
                    type = 8,
                    color = "#c800c8",
                    size = 0.5
                  )
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "EQUATORIAL ENERGIA"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                    tags$h6(tags$b("Outros:"), "Corresponde a soma dos consumidores dos tipos Rural,
                         Poder Público, Iluminação Pública, Serviço Público e Consumo Próprio")
                  ),
                  downset_ui(NS(id, "inf5_1"))
                )
              ),
              ## Tabela - Consumo de Energia Elétrica por Tipo----
              box(
                title = textOutput(NS(id, "inf5txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "inf5tab"),height = "400px"),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "EQUATORIAL ENERGIA"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                    tags$h6(tags$b("Outros:"), "Corresponde a soma dos consumidores dos tipos Rural,
                         Poder Público, Iluminação Pública, Serviço Público e Consumo Próprio")
                  ),
                  downset_ui(NS(id, "inf5_2"))
                )
              )
            )
          ),

          # 6 - Total de  Carregamento nos Portos----
          tabPanel(
            "Total de Carregamento nos Portos",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Total de  Carregamento nos Portos"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "inf6muni"),
                  label = "Portos",
                  choices = inf6 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Total de  Carregamento nos Portos----
              box(
                title = textOutput(NS(id, "inf6txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                # Comparar municípios
                pickerInput(
                  inputId = NS(id, "inf6municomp"),
                  label = "Comparar Portos",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um porto")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "inf6graf")),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "CDP"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "inf6_1"))
                )
              )
            ),
            fluidRow(
              ## Tabela - Total de  Carregamento nos Portos----
              box(
                title = textOutput(NS(id, "inf6txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "inf6ano"),
                  label = "Ano",
                  choices = sort(unique(inf6[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "inf6tab2"),height = "400px"),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "CDP"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "inf6_2"))
                )
              )
            )
          ),
          # 7 - Total de  Descarregamento nos Portos----
          tabPanel(
            "Total de Descarregamento nos Portos",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Total de Descarregamento nos Portos"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "inf7muni"),
                  label = "Portos",
                  choices = inf7 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Total de  Descarregamento nos Portos----
              box(
                title = textOutput(NS(id, "inf7txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                # Comparar municípios
                pickerInput(
                  inputId = NS(id, "inf7municomp"),
                  label = "Comparar Portos",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um porto")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "inf7graf")),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "CDP"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "inf7_1"))
                )
              )
            ),
            fluidRow(
              ## Tabela - Total de  Descarregamento nos Portos----
              box(
                title = textOutput(NS(id, "inf7txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "inf7ano"),
                  label = "Ano",
                  choices = sort(unique(inf7[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "inf7tab2"),height = "400px"),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "CDP"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "inf7_2"))
                )
              )
            )
          ),
          # 8 - Total de  Movimentação nos Portos----
          tabPanel(
            "Total de Movimentação nos Portos",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Total de Movimentação nos Portos"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "inf8muni"),
                  label = "Portos",
                  choices = inf8 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Total de  Movimentação nos Portos----
              box(
                title = textOutput(NS(id, "inf8txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                # Comparar municípios
                pickerInput(
                  inputId = NS(id, "inf8municomp"),
                  label = "Comparar Portos",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um porto")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "inf8graf")),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "CDP"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "inf8_1"))
                )
              )
            ),
            fluidRow(
              ## Tabela - Total de  Movimentação nos Portos----
              box(
                title = textOutput(NS(id, "inf8txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "inf8ano"),
                  label = "Ano",
                  choices = sort(unique(inf8[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "inf8tab2"),height = "400px"),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "CDP"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "inf8_2"))
                )
              )
            )
          ),

          # 9 - Total de  Embarcações----
          tabPanel(
            "Total de Embarcações",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Total de Embarcações"),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "inf9muni"),
                  label = "Portos",
                  choices = inf9 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Total de  Embarcações----
              box(
                title = textOutput(NS(id, "inf9txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                # Comparar municípios
                pickerInput(
                  inputId = NS(id, "inf9municomp"),
                  label = "Comparar Portos",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um porto")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "inf9graf")),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "CDP"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "inf9_1"))
                )
              )
            ),
            fluidRow(
              ## Tabela - Total de  Embarcações----
              box(
                title = textOutput(NS(id, "inf9txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "inf9ano"),
                  label = "Ano",
                  choices = sort(unique(inf9[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "inf9tab2"),height = "400px"),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "CDP"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "inf9_2"))
                )
              )
            )
          ),
          # 10 - Total das Receitas Operacionais----
          tabPanel(
            "Total das Receitas Operacionais",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Total das Receitas Operacionais"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "inf10muni"),
                  label = "Portos",
                  choices = inf10 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Total das Receitas Operacionais----
              box(
                title = textOutput(NS(id, "inf10txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                # Comparar municípios
                pickerInput(
                  inputId = NS(id, "inf10municomp"),
                  label = "Comparar Portos",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um porto")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "inf10graf")),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "CDP"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "inf10_1"))
                )
              )
            ),
            fluidRow(
              ## Tabela - Total das Receitas Operacionais----
              box(
                title = textOutput(NS(id, "inf10txt2")),
                collapsible = T,
                collapsed = F,
                pickerInput(
                  inputId = NS(id, "inf10ano"),
                  label = "Ano",
                  choices = sort(unique(inf10[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "inf10tab2"),height = "400px"),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "CDP"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "inf10_2"))
                )
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
                  inputId = NS(id, "inf11muni"),
                  label = "Aeroportos",
                  choices = inf11 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Total de Pouso mais decolagem de Aeronaves----
              box(
                title = textOutput(NS(id, "inf11txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                column(
                  2,
                  # Seletor Ano
                  pickerInput(
                    inputId = NS(id, "inf11ano1"),
                    label = "Ano",
                    choices = sort(unique(inf11[["ano"]]), decreasing = T),
                    width = "100px"
                  )
                ),
                column(
                  6,
                  # Comparar município
                  pickerInput(
                    inputId = NS(id, "inf11municomp1"),
                    label = "Comparar Aeroportos",
                    choices = NULL,
                    width = "200px",
                    options = list(`none-selected-text` = "Selecione um município")
                  )
                ),
                column(
                  12,
                  withSpinner(
                    echarts4rOutput(NS(id, "inf11graf"), height = "326px"),
                    type = 8,
                    color = "#c800c8",
                    size = 0.5
                  )
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "INFRAERO"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "inf11_1"))
                )
              ),
              ## Tabela - Total de Pouso mais decolagem de Aeronaves----
              box(
                title = textOutput(NS(id, "inf11txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "inf11tab"),height = "400px"),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "INFRAERO"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "inf11_2"))
                )
              )
            ),
            fluidRow(
              ## Gráfico - Total de Pouso mais decolagem de Aeronaves----
              box(
                title = textOutput(NS(id, "inf11txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                # Comparar município
                pickerInput(
                  inputId = NS(id, "inf11municomp2"),
                  label = "Comparar Aeroportos",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "inf11graf2")),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "INFRAERO"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "inf11_3"))
                )
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
                  inputId = NS(id, "inf12muni"),
                  label = "Aeroportos",
                  choices = inf12 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Total de Embarque mais desembarque de passageiros----
              box(
                title = textOutput(NS(id, "inf12txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                column(
                  2,
                  # Selecionar ano
                  pickerInput(
                    inputId = NS(id, "inf12ano1"),
                    label = "Ano",
                    choices = sort(unique(inf12[["ano"]]), decreasing = T),
                    width = "100px"
                  )
                ),
                column(
                  6,
                  # Comparar município
                  pickerInput(
                    inputId = NS(id, "inf12municomp1"),
                    label = "Comparar Aeroportos",
                    choices = NULL,
                    width = "200px",
                    options = list(`none-selected-text` = "Selecione um município")
                  )
                ),
                column(
                  12,
                  withSpinner(
                    echarts4rOutput(NS(id, "inf12graf"), height = "326px"),
                    type = 8,
                    color = "#c800c8",
                    size = 0.5
                  )
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "INFRAERO"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "inf12_1"))
                )
              ),
              ## Tabela - Total de Embarque mais desembarque de passageiros----
              box(
                title = textOutput(NS(id, "inf12txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "inf12tab"),height = "400px"),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "INFRAERO"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "inf12_2"))
                )
              )
            ),
            fluidRow(
              ## Gráfico - Total de Embarque mais desembarque de passageiros----
              box(
                title = textOutput(NS(id, "inf12txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                # Comparar município
                pickerInput(
                  inputId = NS(id, "inf12municomp2"),
                  label = "Comparar Aeroportos",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "inf12graf2")),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "INFRAERO"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "inf12_3"))
                )
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
                  inputId = NS(id, "inf13muni"),
                  label = "Município",
                  choices = inf13 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Abastecimento de Água Segundo Consumidores e Volume Consumido----
              box(
                title = textOutput(NS(id, "inf13txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                # Comparar municípios
                pickerInput(
                  inputId = NS(id, "inf13municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                hr(),
                withSpinner(
                  echarts4rOutput(NS(id, "inf13graf"), height = "600px"),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "SNIS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "inf13_1"))
                )
              )
            ),
            fluidRow(
              ## Tabela - Abastecimento de Água Segundo Consumidores e Volume Consumido----
              box(
                title = textOutput(NS(id, "inf13txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "inf13ano"),
                  label = "Ano",
                  choices = sort(unique(inf13[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "inf13tab2"),height = "400px"),
                  type = 8,
                  color = "#c800c8",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "SNIS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA"),
                    tags$h6(tags$b("Consumo ="), "Volume consumido de água (1.000 m³ /ano)")
                  ),
                  downset_ui(NS(id, "inf13_2"))
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
infraestrutura_mp_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    #TÍTULOS----
    # 1 - Total da Frota de Veículos Subdivididos em Licenciados e Não Licenciados----
    ## Gráfico de barras - Total da Frota de Veículos Subdivididos em Licenciados e Não Licenciados----
    t11 <- reactive({
      req(input$inf1municomp1)
      if (input$inf1municomp1 == "Selecione um município") {
        paste0("Total da Frota de Veículos Subdivididos em Licenciados e Não Licenciados, ", input$inf1muni, " - ", input$inf1ano1)
      } else {
        paste0("Total da Frota de Veículos Subdivididos em Licenciados e Não Licenciados, ", input$inf1muni, " x ", input$inf1municomp1, " - ", input$inf1ano1)
      }
    })
    ## Tabela - Total da Frota de Veículos Subdivididos em Licenciados e Não Licenciados----
    t12 <- reactive({
      paste0("Frota de Veículos no município de ",
             input$inf1muni, " - ", min(inf1$ano), " a ", max(inf1$ano))
    })
    ## Gráfico de linha - Total da Frota de Veículos Subdivididos em Licenciados e Não Licenciados----
    t13 <- reactive({
      req(input$inf1municomp2)
      if (input$inf1municomp2 == "Selecione um município") {
        paste0("Total da Frota de Veículos, ", input$inf1muni, " - ", min(inf1$ano), " a ", max(inf1$ano))
      } else {
        paste0("Total da Frota de Veículos, ", input$inf1muni, " x ", input$inf1municomp2, " - ", min(inf1$ano), " a ", max(inf1$ano))
      }
    })
    # 2 - Consumidores de Energia Elétrica Total----
    ## Gráfico de linha - Consumidores de Energia Elétrica Total----
    t21 <- reactive({
      req(input$inf2municomp)
      if (input$inf2municomp == "Selecione um município") {
        paste0("Consumidores de Energia Elétrica Total, ", input$inf2muni, " - ", min(inf2$ano), " a ", max(inf2$ano))
      } else {
        paste0("Consumidores de Energia Elétrica Total, ", input$inf2muni, " x ", input$inf2municomp, " - ", min(inf2$ano), " a ", max(inf2$ano))
      }
    })
    ## Tabela - Consumidores de Energia Elétrica Total da mesma Região de Integração----
    t22 <- reactive({
      ri <- inf2 %>%
        filter(ano == input$inf2ano, localidade == input$inf2muni) %>%
        pull(ri)
      paste0("Consumidores de Energia Elétrica Total dos municípios, Região de Integração ",
             ri, " - ", input$inf2ano)
    })
    # 3 - Consumidores de Energia Elétrica por Tipo----
    ## Gráfico de barras - Consumidores de Energia Elétrica por Tipo----
    t31 <- reactive({
      req(input$inf3municomp1)
      if (input$inf3municomp1 == "Selecione um município") {
        paste0("Consumidores de Energia Elétrica por Tipo, ", input$inf3muni, " - ", input$inf3ano1)
      } else {
        paste0("Consumidores de Energia Elétrica por Tipo, ", input$inf3muni, " x ", input$inf3municomp1, " - ", input$inf3ano)
      }
    })
    ## Tabela - Consumidores de Energia Elétrica por Tipo----
    t32 <- reactive({
      paste0("Consumidores de Energia Elétrica por Tipo, ",
             input$inf3muni, " - ", min(inf3$ano), " a ", max(inf3$ano))
    })
    # 4 - Consumo de Energia Elétrica Total (kWH)----
    ## Gráfico de linha - Consumo de Energia Elétrica Total (kWH)----
    t41 <- reactive({
      req(input$inf4municomp)
      if (input$inf4municomp == "Selecione um município") {
        paste0("Consumo de Energia Elétrica Total (kWH), ", input$inf4muni, " - ", min(inf4$ano), " a ", max(inf4$ano))
      } else {
        paste0("Consumo de Energia Elétrica Total (kWH), ", input$inf4muni, " x ", input$inf4municomp, " - ", min(inf4$ano), " a ", max(inf4$ano))
      }
    })
    ## Tabela - Consumo de Energia Elétrica Total (kWH) da mesma Região de Integração----
    t42 <- reactive({
      ri <- inf4 %>%
        filter(ano == input$inf4ano, localidade == input$inf4muni) %>%
        pull(ri)
      paste0("Consumo de Energia Elétrica Total (kWH) dos municípios, Região de Integração ",
             ri, " - ", input$inf4ano)
    })
    # 5 - Consumo de Energia Elétrica por Tipo----
    ## Gráfico de barras - Consumo de Energia Elétrica por Tipo----
    t51 <- reactive({
      req(input$inf5municomp1)
      if (input$inf5municomp1 == "Selecione um município") {
        paste0("Consumo de Energia Elétrica por Tipo, ", input$inf5muni, " - ", input$inf5ano1)
      } else {
        paste0("Consumo de Energia Elétrica por Tipo, ", input$inf5muni, " x ", input$inf5municomp1, " - ", input$inf5ano1)
      }
    })
    ## Tabela - Consumo de Energia Elétrica por Tipo----
    t52 <- reactive({
      paste0("Consumo (KWH) de Energia Elétrica por Tipo, ",
             input$inf5muni, " - ", min(inf5$ano), " a ", max(inf5$ano))
    })
    # 6 - Total de  Carregamento nos Portos----
    ## Gráfico de linha - Total de  Carregamento nos Portos----
    t61 <- reactive({
      req(input$inf6municomp)
      if (input$inf6municomp == "Selecione um porto") {
        paste0("Total de  Carregamento (Mercadorias) no Porto de ", input$inf6muni, " - ", min(inf6$ano), " a ", max(inf6$ano))
      } else {
        paste0("Total de  Carregamento (Mercadorias) nos Portos de ", input$inf6muni, " x ", input$inf6municomp, " - ", min(inf6$ano), " a ", max(inf6$ano))
      }
    })
    ## Tabela - Total de  Carregamento nos Portos----
    t62 <- reactive({
      paste0("Total de  Carregamento (Mercadorias) nos Portos", " - ", input$inf6ano)
    })
    # 7 - Total de  Descarregamento nos Portos----
    ## Gráfico de linha - Total de  Descarregamento nos Portos----
    t71 <- reactive({
      req(input$inf7municomp)
      if (input$inf7municomp == "Selecione um porto") {
        paste0("Total de  Descarregamento (Mercadorias) no Porto de ", input$inf7muni, " - ", min(inf7$ano), " a ", max(inf7$ano))
      } else {
        paste0("Total de  Descarregamento (Mercadorias) nos Portos de ", input$inf7muni, " x ", input$inf7municomp, " - ", min(inf7$ano), " a ", max(inf7$ano))
      }
    })
    ## Tabela - Total de  Descarregamento nos Portos----
    t72 <- reactive({
      paste0("Total de Descarregamento (Mercadorias) nos Portos - ", input$inf7ano)
    })
    # 8 - Total de  Movimentação nos Portos----
    ## Gráfico de linha - Total de  Movimentação nos Portos----
    t81 <- reactive({
      req(input$inf8municomp)
      if (input$inf8municomp == "Selecione um porto") {
        paste0("Total de  Movimentação (Mercadorias) no Porto de ", input$inf8muni, " - ", min(inf8$ano), " a ", max(inf8$ano))
      } else {
        paste0("Total de  Movimentação (Mercadorias) nos Portos de ", input$inf8muni, " x ", input$inf8municomp, " - ", min(inf8$ano), " a ", max(inf8$ano))
      }
    })
    ## Tabela - Total de  Movimentação nos Portos----
    t82 <- reactive({
      paste0("Total de   Movimentação (Mercadorias) nos Portos - ", input$inf8ano)
    })
    # 9 - Total de  Embarcações----
    ## Gráfico de linha- Total de  Embarcações----
    t91 <- reactive({
      req(input$inf9municomp)
      if (input$inf9municomp == "Selecione um porto") {
        paste0("Total de  Embarcações (Unidades) no Porto de ", input$inf9muni, " - ", min(inf9$ano), " a ", max(inf9$ano))
      } else {
        paste0("Total de  Embarcações (Unidades) nos Portos de ", input$inf9muni, " x ", input$inf9municomp, " - ", min(inf9$ano), " a ", max(inf9$ano))
      }
    })
    ## Tabela - Total de  Embarcações----
    t92 <- reactive({
      paste0("Total de  Embarcações (Unidades) nos Portos - ", input$inf9ano)
    })
    # 10 - Total das Receitas Operacionais----
    ## Gráfico de linha - Total das Receitas Operacionais----
    t101 <- reactive({
      req(input$inf10municomp)
      if (input$inf10municomp == "Selecione um porto") {
        paste0("Total das Receitas Operacionais (R$) do Porto de ", input$inf10muni, " - ", min(inf10$ano), " a ", max(inf10$ano))
      } else {
        paste0("Total das Receitas Operacionais (R$) dos Portos de ", input$inf10muni, " x ", input$inf10municomp, " - ", min(inf10$ano), " a ", max(inf10$ano))
      }
    })
    ## Tabela - Total das Receitas Operacionais----
    t102 <- reactive({
      paste0("Total das Receitas Operacionais dos Portos - ", input$inf10ano)
    })
    # 11 - Total de Pouso mais decolagem de Aeronaves ----
    ## Gráfico de barras - Total de Pouso mais decolagem de Aeronaves----
    t111 <- reactive({
      req(input$inf11municomp1)
      if (input$inf11municomp1 == "Selecione um aeroporto") {
        paste0("Pouso mais decolagem de Aeronaves no aeroporto de ", input$inf11muni, " - ", input$inf11ano1)
      } else {
        paste0("Pouso mais decolagem de Aeronaves nos aeroportos de ", input$inf11muni, " x ", input$inf11municomp1, " - ", input$inf11ano1)
      }
    })
    ## Tabela - Total de Pouso mais decolagem de Aeronaves----
    t112 <- reactive({
      paste0("Pouso mais decolagem de Aeronaves do aeroporto de ",
             input$inf11muni, " - ", min(inf11$ano), " a ", max(inf11$ano))
    })
    ## Gráfico - Total de Pouso mais decolagem de Aeronaves----
    t113 <- reactive({
      req(input$inf11municomp2)
      if (input$inf11municomp2 == "Selecione um aeroporto") {
        paste0("Total de Pouso mais decolagem de Aeronaves do aeroporto de ", input$inf11muni, " - ", min(inf11$ano), " a ", max(inf11$ano))
      } else {
        paste0("Total de Pouso mais decolagem de Aeronaves  dos aeroportos de ", input$inf11muni, " x ", input$inf11municomp2, " - ", min(inf11$ano), " a ", max(inf11$ano))
      }
    })
    # 12 - Total de Embarque mais desembarque de passageiros----
    ## Gráfico de barras - Total de Embarque mais desembarque de passageiros----
    t121 <- reactive({
      req(input$inf12municomp1)
      if (input$inf12municomp1 == "Selecione um aeroporto") {
        paste0("Embarque mais desembarque de passageiros do aeroporto de ", input$inf12muni, " - ", input$inf12ano1)
      } else {
        paste0("Embarque mais desembarque de passageiros dos aeroportos de ", input$inf12muni, " x ", input$inf12municomp1, " - ", input$inf12ano1)
      }
    })
    ## Tabela - Total de Embarque mais desembarque de passageiros----
    t122 <- reactive({
      paste0("Total de Embarque mais desembarque de passageiros do aeroporto de ",
             input$inf12muni, " - ", min(inf12$ano), " a ", max(inf12$ano))
    })
    ## Gráfico de linha - Total de Embarque mais desembarque de passageiros----
    t123 <- reactive({
      req(input$inf12municomp2)
      if (input$inf12municomp2 == "Selecione um aeroporto") {
        paste0("Total de Embarque mais desembarque de passageiros no aeroporto de ", input$inf12muni, " - ", min(inf12$ano), " a ", max(inf12$ano))
      } else {
        paste0("Total de Embarque mais desembarque de passageiros nos aeroportos de ", input$inf12muni, " x ", input$inf12municomp2, " - ", min(inf12$ano), " a ", max(inf12$ano))
      }
    })
    # 13 - Abastecimento de Água Segundo Consumidores e Volume Consumido----
    ## Gráfico de linha - Abastecimento de Água Segundo Consumidores e Volume Consumido----
    t131 <- reactive({
      req(input$inf13municomp)
      if (input$inf13municomp == "Selecione um município") {
        paste0("Abastecimento de Água Segundo Consumidores e Volume Consumido, ", input$inf13muni, " - ", min(inf13$ano), " a ", max(inf13$ano))
      } else {
        paste0("Abastecimento de Água Segundo Consumidores e Volume Consumido, ", input$inf13muni, " x ", input$inf13municomp, " - ", min(inf13$ano), " a ", max(inf13$ano))
      }
    })
    ## Tabela - Abastecimento de Água Segundo Consumidores e Volume Consumido da mesma Região de Integração----
    t132 <- reactive({
      ri <- inf13 %>%
        filter(ano == input$inf13ano, localidade == input$inf13muni) %>%
        pull(ri) %>%
        unique()
      paste0("Abastecimento de Água Segundo Consumidores e Volume Consumido dos Municípios, Região de Integração ",
             ri, " - ", input$inf13ano)
    })
    #VISUALIZAÇÃO----
    # 1 - Total da Frota de Veículos Subdivididos em Licenciados e Não Licenciados----
    ## Gráfico de barras - Total da Frota de Veículos Subdivididos em Licenciados e Não Licenciados----
    # Atualização da entrada
    inf1comp1 <- reactive({
      input$inf1muni
    })
    observeEvent(inf1comp1(), {
      x <- inf1 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != inf1comp1())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "inf1municomp1", choices = c("Selecione um município", choices), session)
    })

    ## Título
    output$inf1txt1 <- renderText({
      t11()
    })

    output$inf1graf <- renderEcharts4r({
      req(input$inf1municomp1)
      if (input$inf1municomp1 == "Selecione um município") {
        a <- inf1 %>% filter(localidade == input$inf1muni, 
        ano == input$inf1ano1, categoria != "Frota")
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            color = "#c800c8",
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
            name = "Tipo de Frota",
            nameLocation = "middle",
            nameTextStyle = list(fontWeight = "bold", padding = c(10, 0, 0, 0), fontSize = 14)
          ) %>%
          e_y_axis(
            name = "Quantidade",
            nameTextStyle =
              list(
                fontWeight = "bold",
                fontSize = 14
              ),
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
          e_grid(show = T, height = "70%")
      } else {
        a <- inf1 %>% filter(localidade == input$inf1muni, 
        ano == input$inf1ano1, categoria != "Frota")
        b <- inf1 %>% filter(localidade == input$inf1municomp1, 
        ano == input$inf1ano1, categoria != "Frota")
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            name = input$inf1muni,
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
            name = input$inf1municomp1,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            barWidth = "25%",
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 0, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Tipo de Frota",
            nameLocation = "middle",
            nameTextStyle = list(fontWeight = "bold", padding = c(10, 0, 0, 0), fontSize = 14)
          ) %>%
          e_y_axis(
            name = "Quantidade",
            nameTextStyle =
              list(
                fontWeight = "bold",
                fontSize = 14
              ),
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
          e_grid(show = T, height = "70%")
      }
    })
    ## Tabela - Total da Frota de Veículos Subdivididos em Licenciados e Não Licenciados----
    output$inf1txt2 <- renderText({
      t12()
    })
    output$inf1tab <- renderReactable({
      x <- inf1 %>%
        filter(localidade == input$inf1muni) %>%
        pivot_wider(names_from = categoria, values_from = valor) %>% 
        select(-c(tematica:localidade))
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
          ano = colDef(name = "Ano",defaultSortOrder = "desc"),
          Frota = colDef(name = "Total")
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
    ## Gráfico de linha - Total da Frota de Veículos Subdivididos em Licenciados e Não Licenciados----
    # Atualização da entrada
    inf1comp2 <- reactive({
      input$inf1muni
    })
    observeEvent(inf1comp2(), {
      x <- inf1 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != inf1comp2())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "inf1municomp2", choices = c("Selecione um município", choices), session)
    })
    ## Título
    output$inf1txt3 <- renderText({
      t13()
    })
    output$inf1graf2 <- renderEcharts4r({
      req(input$inf1municomp2)
      if (input$inf1municomp2 == "Selecione um município") {
        a <- inf1 %>% filter(localidade == input$inf1muni, 
        categoria == "Frota")
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#c800c8",
            name = "Frota",
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
            name = "Quantidade",
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
      } else {
        a <- inf1 %>% filter(localidade == input$inf1muni, categoria == "Frota")
        b <- inf1 %>% filter(localidade == input$inf1municomp2, categoria == "Frota")
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$inf1muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$inf1municomp2,
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
            name = "Quantidade",
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
      }
    })
    # 2 - Consumidores de Energia Elétrica Total----
    ## Gráfico de linha - Consumidores de Energia Elétrica Total----
    # Atualização da entrada
    inf2comp <- reactive({
      input$inf2muni
    })
    observeEvent(inf2comp(), {
      x <- inf2 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != inf2comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "inf2municomp", choices = c("Selecione um município", choices), session)
    })

    output$inf2txt1 <- renderText({
      t21()
    })
    output$inf2graf <- renderEcharts4r({
      req(input$inf2municomp)
      if (input$inf2municomp == "Selecione um município") {
        a <- inf2 %>% filter(localidade == input$inf2muni)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#c800c8",
            name = "Unidades Consumidoras",
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
            name = "Unidades Consumidoras",
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
      } else {
        a <- inf2 %>% filter(localidade == input$inf2muni)
        b <- inf2 %>% filter(localidade == input$inf2municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$inf2muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$inf2municomp,
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
            name = "Unidades Consumidoras",
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
      }
    })
    ## Tabela - Consumidores de Energia Elétrica Total da mesma Região de Integração----
    output$inf2txt2 <- renderText({
      t22()
    })
    output$inf2tab2 <- renderReactable({
      ris <- inf2 %>%
        filter(ano == input$inf2ano, localidade == input$inf2muni) %>%
        pull(ri)
      x <- inf2 %>% filter(ano == input$inf2ano, localidade != "Pará")
      x <- x %>%
        filter(ri == ris) %>%
        arrange(desc(valor))
      x <- x %>%
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
            name = "Unidades Consumidoras",
            format = colFormat(
              separators = T,
              locales = "pt-BR"
            ),
            cell = data_bars(x,
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
    # 3 - Consumidores de Energia Elétrica por Tipo----
    ## Gráfico de barras - Consumidores de Energia Elétrica por Tipo----
    # Atualização da entrada
    inf3comp1 <- reactive({
      input$inf3muni
    })
    observeEvent(inf3comp1(), {
      x <- inf3 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != inf3comp1())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "inf3municomp1", choices = c("Selecione um município", choices), session)
    })

    ## Título
    output$inf3txt1 <- renderText({
      t31()
    })
    output$inf3graf <- renderEcharts4r({
      req(input$inf3municomp1)
      if (input$inf3municomp1 == "Selecione um município") {
        a <- inf3 %>% filter(localidade == input$inf3muni, 
        ano == input$inf3ano1)
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            color = "#c800c8",
            name = "Unidades",
            legend = F,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            barWidth = "25%",
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 0, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Tipo de Consumidores",
            nameLocation = "middle",
            nameTextStyle = list(fontWeight = "bold", lineHeight = 25, fontSize = 14)
          ) %>%
          e_y_axis(
            name = "Unidades",
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
          e_grid(show = T, height = "70%")
      } else {
        a <- inf3 %>% filter(localidade == input$inf3muni, 
        ano == input$inf3ano1)
        b <- inf3 %>% filter(localidade == input$inf3municomp1, 
        ano == input$inf3ano1)
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            name = input$inf3muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            barWidth = "25%",
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, categoria) %>%
          e_bar(
            serie = valor,
            name = input$inf3municomp,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            barWidth = "25%",
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
            name = "Tipo de Consumidores",
            nameLocation = "middle",
            nameTextStyle = list(fontWeight = "bold", lineHeight = 25, fontSize = 14)
          ) %>%
          e_y_axis(
            name = "Unidades",
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
          e_grid(show = T, height = "70%")
      }
    })
    ## Tabela - Consumidores de Energia Elétrica por Tipo----
    output$inf3txt2 <- renderText({
      t32()
    })
    output$inf3tab <- renderReactable({
      x <- inf3 %>%
        filter(localidade == input$inf3muni) %>%
        pivot_wider(names_from = categoria, values_from = valor) %>%
        select(-c(tematica:localidade))
      x %>% reactable(
        defaultSorted = list(Ano = "desc"),
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
          Frota = colDef(name = "Total")
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

    # 4 - Consumo de Energia Elétrica Total (kWH)----
    ## Gráfico de linha - Consumo de Energia Elétrica Total (kWH)----
    # Atualização da entrada
    inf4comp <- reactive({
      input$inf4muni
    })
    observeEvent(inf4comp(), {
      x <- inf4 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != inf4comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "inf4municomp", choices = c("Selecione um município", choices), session)
    })

    output$inf4txt1 <- renderText({
      t41()
    })

    output$inf4graf <- renderEcharts4r({
      req(input$inf4municomp)
      if (input$inf4municomp == "Selecione um município") {
        a <- inf4 %>% filter(localidade == input$inf4muni)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#c800c8",
            name = "Consumo (KWH)",
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
            name = "Consumo (KWH)",
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
      } else {
        a <- inf4 %>% filter(localidade == input$inf4muni)
        b <- inf4 %>% filter(localidade == input$inf4municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$inf4muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$inf4municomp,
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
            name = "Consumo (KWH)",
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
      }
    })
    ## Tabela - Consumo de Energia Elétrica Total (kWH) da mesma Região de Integração----
    output$inf4txt2 <- renderText({
      t42()
    })
    output$inf4tab2 <- renderReactable({
      ris <- inf4 %>%
        filter(ano == input$inf4ano, 
        localidade == input$inf4muni) %>%
        pull(ri)
      x <- inf4 %>% filter(ano == input$inf4ano, 
      localidade != "Pará")
      x <- x %>%
        filter(ri == ris) %>%
        arrange(desc(valor))
      x <- x %>%
        mutate(posicao = row.names(.)) %>%
        select(posicao, 
          ri, 
          localidade, 
          valor)
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
            name = "Consumo (KWH)",
            format = colFormat(
              digits = 4,
              separators = T,
              locales = "pt-BR"
            ),
            cell = data_bars(x,
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

    # 5 - Consumo de Energia Elétrica por Tipo----
    ## Gráfico de barras - Consumo de Energia Elétrica por Tipo----
    # Atualização da entrada
    inf5comp1 <- reactive({
      input$inf5muni
    })
    observeEvent(inf5comp1(), {
      x <- inf5 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != inf5comp1())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "inf5municomp1", choices = c("Selecione um município", choices), session)
    })

    ## Título
    output$inf5txt1 <- renderText({
      t51()
    })

    output$inf5graf <- renderEcharts4r({
      req(input$inf5municomp1)
      if (input$inf5municomp1 == "Selecione um município") {
        a <- inf5 %>% filter(localidade == input$inf5muni, 
        ano == input$inf5ano1)
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            color = "#c800c8",
            name = "Consumo (KWH)",
            legend = F,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            barWidth = "25%",
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 0, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Tipo de Consumidores",
            nameLocation = "middle",
            nameTextStyle = list(fontWeight = "bold", lineHeight = 25, fontSize = 14)
          ) %>%
          e_y_axis(
            name = "Consumo (KWH)",
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
          e_grid(show = T, height = "70%")
      } else {
        a <- inf5 %>% filter(localidade == input$inf5muni, ano == input$inf5ano1)
        b <- inf5 %>% filter(localidade == input$inf5municomp1, ano == input$inf5ano1)
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            name = input$inf5muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            barWidth = "25%",
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, categoria) %>%
          e_bar(
            serie = valor,
            name = input$inf5municomp,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            barWidth = "25%",
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 0, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Tipo de Consumidores",
            nameLocation = "middle",
            nameTextStyle = list(fontWeight = "bold", lineHeight = 25, fontSize = 14)
          ) %>%
          e_y_axis(
            name = "Consumo (KWH)",
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
          e_grid(show = T, height = "70%")
      }
    })
    ## Tabela - Consumo de Energia Elétrica por Tipo----
    output$inf5txt2 <- renderText({
      t52()
    })
    output$inf5tab <- renderReactable({
      x <- inf5 %>%
        filter(localidade == input$inf5muni) %>%
        pivot_wider(names_from = categoria, values_from = valor) %>%
        select(-c(tematica:localidade))
      x %>% reactable(
        defaultSorted = list(Ano = "desc"),
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
          Frota = colDef(name = "Total")
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
    # 6 - Total de  Carregamento nos Portos----
    ## Gráfico de linha - Total de  Carregamento nos Portos----
    # Atualização da entrada
    inf6comp <- reactive({
      input$inf6muni
    })
    observeEvent(inf6comp(), {
      x <- inf6 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != inf6comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "inf6municomp", choices = c("Selecione um porto", choices), session)
    })

    output$inf6txt1 <- renderText({
      t61()
    })
    output$inf6graf <- renderEcharts4r({
      req(input$inf6municomp)
      if (input$inf6municomp == "Selecione um porto") {
        a <- inf6 %>% filter(localidade == input$inf6muni)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#c800c8",
            name = "Toneladas",
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
            name = "Toneladas",
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
      } else {
        a <- inf6 %>% filter(localidade == input$inf6muni)
        b <- inf6 %>% filter(localidade == input$inf6municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$inf6muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$inf6municomp,
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
            name = "Toneladas",
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
      }
    })
    ## Tabela - Total de  Carregamento nos Portos----
    output$inf6txt2 <- renderText({
      t62()
    })
    output$inf6tab2 <- renderReactable({
      x <- inf6 %>%
        filter(ano == input$inf6ano, localidade != "Pará") %>%
        arrange(desc(valor))
      x <- x %>%
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
          localidade = colDef(name = "Portos"),
          valor = colDef(
            name = "Carregamento (Toneladas)",
            format = colFormat(
              digits = 4,
              separators = T,
              locales = "pt-BR"
            ),
            cell = data_bars(x,
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
    # 7 - Total de  Descarregamento nos Portos----
    ## Gráfico de linha - Total de  Descarregamento nos Portos----
    # Atualização da entrada
    inf7comp <- reactive({
      input$inf7muni
    })
    observeEvent(inf7comp(), {
      x <- inf7 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != inf7comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "inf7municomp", choices = c("Selecione um porto", choices), session)
    })

    output$inf7txt1 <- renderText({
      t71()
    })

    output$inf7graf <- renderEcharts4r({
      req(input$inf7municomp)
      if (input$inf7municomp == "Selecione um porto") {
        a <- inf7 %>% filter(localidade == input$inf7muni)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#c800c8",
            name = "Toneladas",
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
            name = "Toneladas",
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
      } else {
        a <- inf7 %>% filter(localidade == input$inf7muni)
        b <- inf7 %>% filter(localidade == input$inf7municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$inf7muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$inf7municomp,
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
            name = "Toneladas",
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
      }
    })
    ## Tabela - Total de  Descarregamento nos Portos----
    output$inf7txt2 <- renderText({
      t72()
      })
    output$inf7tab2 <- renderReactable({
      x <- inf7 %>%
        filter(ano == input$inf7ano, localidade != "Pará") %>%
        arrange(desc(valor))
      x <- x %>%
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
          localidade = colDef(name = "Portos"),
          valor = colDef(
            name = "Descarregamento (Toneladas)",
            format = colFormat(
              digits = 4,
              separators = T,
              locales = "pt-BR"
            ),
            cell = data_bars(x,
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
    # 8 - Total de  Movimentação nos Portos----
    ## Gráfico de linha - Total de  Movimentação nos Portos----
    # Atualização da entrada
    inf8comp <- reactive({
      input$inf8muni
    })
    observeEvent(inf8comp(), {
      x <- inf8 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != inf8comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "inf8municomp", choices = c("Selecione um porto", choices), session)
    })

    output$inf8txt1 <- renderText({
      t81()
    })
    output$inf8graf <- renderEcharts4r({
      req(input$inf8municomp)
      if (input$inf8municomp == "Selecione um porto") {
        a <- inf8 %>% filter(localidade == input$inf8muni)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#c800c8",
            name = "Toneladas",
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
            name = "Toneladas",
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
      } else {
        a <- inf8 %>% filter(localidade == input$inf8muni)
        b <- inf8 %>% filter(localidade == input$inf8municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$inf8muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$inf8municomp,
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
            name = "Toneladas",
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
      }
    })
    ## Tabela - Total de  Movimentação nos Portos----
    output$inf8txt2 <- renderText({
      t82()
    })
    output$inf8tab2 <- renderReactable({
      x <- inf8 %>%
        filter(ano == input$inf8ano, localidade != "Pará") %>%
        arrange(desc(valor))
      x <- x %>%
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
          localidade = colDef(name = "Portos"),
          valor = colDef(
            name = "Movimentação (Toneladas)",
            format = colFormat(
              digits = 4,
              separators = T,
              locales = "pt-BR"
            ),
            cell = data_bars(x,
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
    # 9 - Total de  Embarcações----
    ## Gráfico de linha- Total de  Embarcações----
    # Atualização da entrada
    inf9comp <- reactive({
      input$inf9muni
    })
    observeEvent(inf9comp(), {
      x <- inf9 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != inf9comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "inf9municomp", choices = c("Selecione um porto", choices), session)
    })

    output$inf9txt1 <- renderText({
      t91()
    })

    output$inf9graf <- renderEcharts4r({
      req(input$inf9municomp)
      if (input$inf9municomp == "Selecione um porto") {
        a <- inf9 %>% filter(localidade == input$inf9muni)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#c800c8",
            name = "Unidades",
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
            name = "Unidades",
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
      } else {
        a <- inf9 %>% filter(localidade == input$inf9muni)
        b <- inf9 %>% filter(localidade == input$inf9municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$inf9muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$inf9municomp,
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
            name = "Unidades",
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
      }
    })
    ## Tabela - Total de  Embarcações----
    output$inf9txt2 <- renderText({
      t92()
    })
    output$inf9tab2 <- renderReactable({
      x <- inf9 %>%
        filter(ano == input$inf9ano, localidade != "Pará") %>%
        arrange(desc(valor))
      x <- x %>%
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
          localidade = colDef(name = "Portos"),
          valor = colDef(
            name = "Embarcações (Unidades)",
            format = colFormat(
              digits = 4,
              separators = T,
              locales = "pt-BR"
            ),
            cell = data_bars(x,
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
    # 10 - Total das Receitas Operacionais----
    ## Gráfico de linha - Total das Receitas Operacionais----
    # Atualização da entrada
    inf10comp <- reactive({
      input$inf10muni
    })
    observeEvent(inf10comp(), {
      x <- inf10 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != inf10comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "inf10municomp", choices = c("Selecione um porto", choices), session)
    })

    output$inf10txt1 <- renderText({
      t101()
    })

    output$inf10graf <- renderEcharts4r({
      req(input$inf10municomp)
      if (input$inf10municomp == "Selecione um porto") {
        a <- inf10 %>% filter(localidade == input$inf10muni)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#c800c8",
            name = "Receita Operacional (R$)",
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
            name = "Receita Operacional (R$)",
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
      } else {
        a <- inf10 %>% filter(localidade == input$inf10muni)
        b <- inf10 %>% filter(localidade == input$inf10municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$inf10muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$inf10municomp,
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
            name = "Receita Operacional (R$)",
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
      }
    })
    ## Tabela - Total das Receitas Operacionais----
    output$inf10txt2 <- renderText({
      t102()
    })
    output$inf10tab2 <- renderReactable({
      x <- inf10 %>%
        filter(ano == input$inf10ano, localidade != "Pará") %>%
        arrange(desc(valor))
      x <- x %>%
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
          localidade = colDef(name = "Portos"),
          valor = colDef(
            name = "Receita Operacional (R$)",
            format = colFormat(
              digits = 4,
              separators = T,
              locales = "pt-BR"
            ),
            cell = data_bars(x,
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
    # 11 - Total de Pouso mais decolagem de Aeronaves ----
    ## Gráfico de barras - Total de Pouso mais decolagem de Aeronaves----
    # Atualização da entrada
    inf11comp1 <- reactive({
      input$inf11muni
    })
    observeEvent(inf11comp1(), {
      x <- inf11 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != inf11comp1())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "inf11municomp1", choices = c("Selecione um aeroporto", choices), session)
    })

    ## Título
    output$inf11txt1 <- renderText({
      t111()
    })

    output$inf11graf <- renderEcharts4r({
      req(input$inf11municomp1)
      if (input$inf11municomp1 == "Selecione um aeroporto") {
        a <- inf11 %>% filter(localidade == input$inf11muni, 
        ano == input$inf11ano1, categoria != "Total")
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            color = "#c800c8",
            name = "Quantidadde",
            legend = F,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            barWidth = "25%",
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 0, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Tipo de voo",
            nameLocation = "middle",
            nameTextStyle = list(fontWeight = "bold", padding = c(10, 0, 0, 0), fontSize = 14)
          ) %>%
          e_y_axis(
            name = "Quantidade",
            nameTextStyle =
              list(
                fontWeight = "bold",
                fontSize = 14
              ),
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
          e_grid(show = T, height = "70%")
      } else {
        a <- inf11 %>% filter(localidade == input$inf11muni, 
        ano == input$inf11ano1, categoria != "Total")
        b <- inf11 %>% filter(localidade == input$inf11municomp1, 
        ano == input$inf11ano1, categoria != "Total")
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            name = input$inf11muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            barWidth = "25%",
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, categoria) %>%
          e_bar(
            serie = valor,
            name = input$inf11municomp1,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            barWidth = "25%",
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 0, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Tipo de voo",
            nameLocation = "middle",
            nameTextStyle = list(fontWeight = "bold", padding = c(10, 0, 0, 0), fontSize = 14)
          ) %>%
          e_y_axis(
            name = "Quantidade",
            nameTextStyle =
              list(
                fontWeight = "bold",
                fontSize = 14
              ),
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
          e_grid(show = T, height = "70%")
      }
    })
    ## Tabela - Total de Pouso mais decolagem de Aeronaves----
    output$inf11txt2 <- renderText({
      t112()
    })
    output$inf11tab <- renderReactable({
      x <- inf11 %>%
        filter(localidade == input$inf11muni) %>%
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
          Frota = colDef(name = "Total")
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
    ## Gráfico - Total de Pouso mais decolagem de Aeronaves----
    # Atualização da entrada
    inf11comp2 <- reactive({
      input$inf11muni
    })
    observeEvent(inf11comp2(), {
      x <- inf11 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != inf11comp2())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "inf11municomp2", choices = c("Selecione um aeroporto", choices), session)
    })
    ## Título
    output$inf11txt3 <- renderText({
      t113()
    })
    output$inf11graf2 <- renderEcharts4r({
      req(input$inf11municomp2)
      if (input$inf11municomp2 == "Selecione um aeroporto") {
        a <- inf11 %>% filter(localidade == input$inf11muni, categoria == "Total")
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#c800c8",
            name = input$inf11cat,
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
            name = "Quantidade",
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
      } else {
        a <- inf11 %>% filter(localidade == input$inf11muni, categoria == "Total")
        b <- inf11 %>% filter(localidade == input$inf11municomp2, categoria == "Total")
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$inf11muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$inf11municomp2,
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
            name = "Toneladas",
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
      }
    })
    # 12 - Total de Embarque mais desembarque de passageiros----

    ## Gráfico de barras - Total de Embarque mais desembarque de passageiros----
    # Atualização da entrada
    inf12comp1 <- reactive({
      input$inf12muni
    })
    observeEvent(inf12comp1(), {
      x <- inf12 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != inf12comp1())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "inf12municomp1", choices = c("Selecione um aeroporto", choices), session)
    })

    ## Título
    output$inf12txt1 <- renderText({
      t121()
    })
    output$inf12graf <- renderEcharts4r({
      req(input$inf12municomp1)
      if (input$inf12municomp1 == "Selecione um aeroporto") {
        a <- inf12 %>% filter(localidade == input$inf12muni, 
        ano == input$inf12ano1, categoria != "Total")
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            color = "#c800c8",
            name = "Quantidade",
            legend = F,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            barWidth = "25%",
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 0, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Tipo de voo",
            nameLocation = "middle",
            nameTextStyle = list(fontWeight = "bold", padding = c(10, 0, 0, 0), fontSize = 14)
          ) %>%
          e_y_axis(
            name = "Quantidade",
            nameTextStyle =
              list(
                fontWeight = "bold",
                fontSize = 14
              ),
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
          e_grid(show = T, height = "70%")
      } else {
        a <- inf12 %>% filter(localidade == input$inf12muni, 
        ano == input$inf12ano1, categoria != "Total")
        b <- inf12 %>% filter(localidade == input$inf12municomp1, 
        ano == input$inf12ano1, categoria != "Total")
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            name = input$inf12muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            barWidth = "25%",
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, categoria) %>%
          e_bar(
            serie = valor,
            name = input$inf12municomp,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            barWidth = "25%",
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 0, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Tipo de voo",
            nameLocation = "middle",
            nameTextStyle = list(fontWeight = "bold", padding = c(10, 0, 0, 0), fontSize = 14)
          ) %>%
          e_y_axis(
            name = "Quantidade",
            nameTextStyle =
              list(
                fontWeight = "bold",
                fontSize = 14
              ),
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
          e_grid(show = T, height = "70%")
      }
    })
    ## Tabela - Total de Embarque mais desembarque de passageiros----
    output$inf12txt2 <- renderText({
      t122()
    })
    output$inf12tab <- renderReactable({
      x <- inf12 %>%
        filter(localidade == input$inf12muni) %>%
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
          ri = colDef(name = "Região de Integração"),
          Frota = colDef(name = "Total")
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
    ## Gráfico de linha - Total de Embarque mais desembarque de passageiros----
    # Atualização da entrada
    inf12comp2 <- reactive({
      input$inf12muni
    })
    observeEvent(inf12comp2(), {
      x <- inf12 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != inf12comp2())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "inf12municomp2", choices = c("Selecione um aeroporto", choices), session)
    })
    ## Título
    output$inf12txt3 <- renderText({
      t123()
    })
    output$inf12graf2 <- renderEcharts4r({
      req(input$inf12municomp2)
      if (input$inf12municomp2 == "Selecione um aeroporto") {
        a <- inf12 %>% filter(localidade == input$inf12muni, categoria == "Total")
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#c800c8",
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
      } else {
        a <- inf12 %>% filter(localidade == input$inf12muni, categoria == "Total")
        b <- inf12 %>% filter(localidade == input$inf12municomp2, categoria == "Total")
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$inf12muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$inf12municomp2,
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
            name = "Quantidade",
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
      }
    })
    # 13 - Abastecimento de Água Segundo Consumidores e Volume Consumido----
    ## Gráfico de linha - Abastecimento de Água Segundo Consumidores e Volume Consumido----
    # Atualização da entrada
    inf13comp <- reactive({
      input$inf13muni
    })
    observeEvent(inf13comp(), {
      x <- inf13 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != inf13comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "inf13municomp", choices = c("Selecione um município", choices), session)
    })

    output$inf13txt1 <- renderText({
      t131()
    })

    output$inf13graf <- renderEcharts4r({
      req(input$inf13municomp)
      if (input$inf13municomp == "Selecione um município") {
        x <- inf13 %>%
          filter(localidade == input$inf13muni) %>%
          pivot_wider(names_from = categoria, values_from = valor)
        x %>%
          e_charts(ano) %>%
          e_line(
            x_index = 1,
            y_index = 1,
            `População total atendida com abastecimento de água (Hab.)`,
            name = "População Atendida",
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_line(
            `Volume consumido de água (1.000 m³ /ano)`,
            name = "Volume consumido (1.000 m³ /ano)",
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
          e_x_axis(
            index = 1,
            axisLabel = list(show = T, fontSize = 11),
            name = "Ano",
            splitLine = list(show = T),
            nameTextStyle = list(fontWeight = "bold", fontSize = 14)
          ) %>%
          e_y_axis(
            name = "(1.000 m³ /ano)",
            nameTextStyle =
              list(fontWeight = "bold", fontSize = 14),
            scale = T,
            axisLabel = list(
              formatter = htmlwidgets::JS("
              function (value, index) {
              return value.toLocaleString('pt-BR', { minimumFractionDigits: 0, maximumFractionDigits: 0 });
              }
            ")
            )
          ) %>%
          e_y_axis(
            index = 1,
            name = "População",
            nameTextStyle =
              list(fontWeight = "bold", fontSize = 14),
            scale = T,
            axisLabel = list(
              formatter = htmlwidgets::JS("
              function (value, index) {
              return value.toLocaleString('pt-BR', { minimumFractionDigits: 0, maximumFractionDigits: 0 });
              }
            ")
            )
          ) %>%
          e_grid(height = "25%", bottom = "60%") %>%
          e_grid(height = "25%", top = "60%") %>%
          e_y_axis(gridIndex = 1) %>%
          e_x_axis(gridIndex = 1) %>%
          e_datazoom(x_index = c(0, 1), toolbox = F, fillerColor = "#E5F5F9")
      } else {
        x <- inf13 %>%
          filter(localidade == input$inf13muni) %>%
          pivot_wider(names_from = categoria, values_from = valor)
        y <- inf13 %>%
          filter(localidade == input$inf13municomp) %>%
          pivot_wider(names_from = categoria, values_from = valor)
        # _______________________________________________________________________________
        x %>%
          e_charts(ano) %>%
          e_line(
            x_index = 1,
            y_index = 1,
            `População total atendida com abastecimento de água (Hab.)`,
            name = input$inf13muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_line(
            `Volume consumido de água (1.000 m³ /ano)`,
            name = input$inf13muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          # _______________________________________________________________________________
          e_data(y, ano) %>%
          e_line(
            x_index = 1,
            y_index = 1,
            `População total atendida com abastecimento de água (Hab.)`,
            name = input$inf13municomp,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_line(
            `Volume consumido de água (1.000 m³ /ano)`,
            name = input$inf13municomp,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          # _______________________________________________________________________________
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
          e_x_axis(
            index = 1,
            axisLabel = list(show = T, fontSize = 11),
            name = "Ano",
            splitLine = list(show = T),
            nameTextStyle = list(fontWeight = "bold", fontSize = 14)
          ) %>%
          e_y_axis(
            name = "(1.000 m³ /ano)",
            nameTextStyle =
              list(fontWeight = "bold", fontSize = 14),
            scale = T,
            axisLabel = list(
              formatter = htmlwidgets::JS("
              function (value, index) {
              return value.toLocaleString('pt-BR', { minimumFractionDigits: 0, maximumFractionDigits: 0 });
              }
            ")
            )
          ) %>%
          e_y_axis(
            index = 1,
            name = "População",
            nameTextStyle =
              list(fontWeight = "bold", fontSize = 14),
            scale = T,
            axisLabel = list(
              formatter = htmlwidgets::JS("
              function (value, index) {
              return value.toLocaleString('pt-BR', { minimumFractionDigits: 0, maximumFractionDigits: 0 });
              }
            ")
            )
          ) %>%
          e_grid(height = "25%", bottom = "60%") %>%
          e_grid(height = "25%", top = "60%") %>%
          e_y_axis(gridIndex = 1) %>%
          e_x_axis(gridIndex = 1) %>%
          e_datazoom(x_index = c(0, 1), toolbox = F, fillerColor = "#E5F5F9")
      }
    })
    ## Tabela - Abastecimento de Água Segundo Consumidores e Volume Consumido da mesma Região de Integração----
    output$inf13txt2 <- renderText({
      t132()
    })
    output$inf13tab2 <- renderReactable({
      ris <- inf13 %>%
        filter(ano == input$inf13ano, localidade == input$inf13muni) %>%
        pull(ri)
      x <- inf13 %>% filter(ano == input$inf13ano, localidade != "Pará")
      x <- x %>%
        filter(ri == ris) %>%
        pivot_wider(names_from = categoria, values_from = valor) %>%
        select(
          ri,
          localidade,
          `População total atendida com abastecimento de água (Hab.)`,
          `Volume consumido de água (1.000 m³ /ano)`
        )
      x %>% reactable(
        defaultSorted = list(`População total atendida com abastecimento de água (Hab.)` = "desc"),
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
          `População total atendida com abastecimento de água (Hab.)` = colDef(
            name = "População (Hab.)",
            format = colFormat(separators = T, locales = "pt-Br"),
            cell = data_bars(
              x,
              text_position = "above",
              box_shadow = TRUE,
              align_bars = "right",
              number_fmt = scales::number_format(decimal.mark = ",", big.mark = ".")
            )
          ),
          `Volume consumido de água (1.000 m³ /ano)` = colDef(
            name = "Consumo (1.000 m³ /ano)",
            format = colFormat(separators = T, locales = "pt-Br"),
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
    #DOWNLOADS----
    # 1 - Total da Frota de Veículos Subdivididos em Licenciados e Não Licenciados----
    ## - Gráfico - Frota de Veículos no município----
    # Filtra os dados
    inf1_1 <- reactive({
      req(input$inf1municomp1)
      if (input$inf1municomp1 == "Selecione um município") {
        a <- inf1 %>% filter(localidade == input$inf1muni, 
        ano == input$inf1ano1, categoria != "Frota")
        } else {
        a <- inf1 %>% filter(localidade == input$inf1muni, 
        ano == input$inf1ano1, categoria != "Frota")
        b <- inf1 %>% filter(localidade == input$inf1municomp1, 
        ano == input$inf1ano1, categoria != "Frota")
        df <- rbind(a,b)
        }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf1_1(), {
      t11()
      downset_Server("inf1_1", inf1_1(), t11())
    })
    ## - Tabela - Frota de Veículos no município----
    # Filtra os dados
    inf1_2 <- reactive({
      x <- inf1 %>%
        filter(localidade == input$inf1muni) %>%
        pivot_wider(names_from = categoria, values_from = valor) 
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf1_2(), {
      t12()
      downset_Server("inf1_2", inf1_2(), t12())
    })
    ## - Gráfico - Frota de Veículos no município----
    # Filtra os dados
    inf1_3 <- reactive({
      req(input$inf1municomp2)
      if (input$inf1municomp2 == "Selecione um município") {
        a <- inf1 %>% filter(localidade == input$inf1muni, 
        categoria == "Frota")
        } else {
        a <- inf1 %>% filter(localidade == input$inf1muni, 
        categoria == "Frota")
        b <- inf1 %>% filter(localidade == input$inf1municomp2, 
        categoria == "Frota")}
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf1_3(), {
      t13()
      downset_Server("inf1_3", inf1_3(), t13())
    })

    # 2 - Consumidores de Energia Elétrica Total----
    ## - Gráfico - Consumidores de Energia Elétrica Total no município----
    # Filtra os dados
    inf2_1 <- reactive({
      req(input$inf2municomp)
      if (input$inf2municomp == "Selecione um município") {
        a <- inf2 %>% filter(localidade == input$inf2muni)
        } else {
        a <- inf2 %>% filter(localidade == input$inf2muni)
        b <- inf2 %>% filter(localidade == input$inf2municomp)}
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf2_1(), {
      t21()
      downset_Server("inf2_1", inf2_1(), t21())
    })
    ## - Tabela - Consumidores de Energia Elétrica Total nos municípios----
    # Filtra os dados
    inf2_2 <- reactive({
      ris <- inf2 %>%
        filter(ano == input$inf2ano, 
               localidade == input$inf2muni) %>%
        pull(ri)
      x <- inf2 %>% 
        filter(ano == input$inf2ano, 
               localidade != "Pará",
               ri == ris) %>% 
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf2_2(), {
      t22()
      downset_Server("inf2_2", inf2_2(), t22())
    })

    # 3 - Consumidores de Energia Elétrica por Tipo----
    ## - Gráfico - Consumidores de Energia Elétrica por Tipo no município----
    # Filtra os dados
    inf3_1 <- reactive({
      req(input$inf3municomp1)
      if (input$inf3municomp1 == "Selecione um município") {
        a <- inf3 %>% filter(localidade == input$inf3muni, 
        ano == input$inf3ano1)
        } else {
        a <- inf3 %>% filter(localidade == input$inf3muni, 
        ano == input$inf3ano1)
        b <- inf3 %>% filter(localidade == input$inf3municomp1, 
        ano == input$inf3ano1)
        df <- rbind(a,b)}
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf3_1(), {
      t31()
      downset_Server("inf3_1", inf3_1(), t31())
    })
    ## - Tabela - Consumidores de Energia Elétrica por Tipo no município----
    # Filtra os dados
    inf3_2 <- reactive({
      x <- inf3 %>%
        filter(localidade == input$inf3muni) %>%
        pivot_wider(names_from = categoria, values_from = valor) 
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf3_2(), {
     t32()
      downset_Server("inf3_2", inf3_2(), t32())
    })

    # 4 - Consumo de Energia Elétrica Total (kWH)----
    ## - Gráfico - Consumo de Energia Elétrica Total (kWH) no município----
    # Filtra os dados
    inf4_1 <- reactive({
      req(input$inf4municomp)
      if (input$inf4municomp == "Selecione um município") {
        a <- inf4 %>% filter(localidade == input$inf4muni)
        } else {
        a <- inf4 %>% filter(localidade == input$inf4muni)
        b <- inf4 %>% filter(localidade == input$inf4municomp)
        df <- rbind(a,b)}
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf4_1(), {
      t41()
      downset_Server("inf4_1", inf4_1(), t41())
    })
    ## - Tabela - Consumo de Energia Elétrica Total (kWH) nos municípios----
    # Filtra os dados
    inf4_2 <- reactive({
      ris <- inf4 %>%
        filter(ano == input$inf4ano, 
        localidade == input$inf4muni) %>%
        pull(ri)
      x <- inf4 %>% 
        filter(ano == input$inf4ano, 
               localidade != "Pará",
               ri == ris) %>% 
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf4_2(), {
      t42()
      downset_Server("inf4_2", inf4_2(), t42())
    })

    # 5 - Consumo de Energia Elétrica por Tipo----
    ## - Gráfico - Consumo de Energia Elétrica por Tipo no município----
    # Filtra os dados
    inf5_1 <- reactive({
      req(input$inf5municomp1)
      if (input$inf5municomp1 == "Selecione um município") {
        a <- inf5 %>% filter(localidade == input$inf5muni, 
        ano == input$inf5ano1)
        } else {
        a <- inf5 %>% 
          filter(localidade == input$inf5muni, 
                 ano == input$inf5ano1)
        b <- inf5 %>% 
          filter(localidade == input$inf5municomp1, 
                 ano == input$inf5ano1)
        df <- rbind(a,b)}
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf5_1(), {
      t51()
      downset_Server("inf5_1", inf5_1(), t51())
    })
    ## - Tabela - Consumo (KWH) de Energia Elétrica por Tipo no município----
    # Filtra os dados
    inf5_2 <- reactive({
      x <- inf5 %>%
        filter(localidade == input$inf5muni) %>%
        pivot_wider(names_from = categoria, values_from = valor)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf5_2(), {
      t52()
      downset_Server("inf5_2", inf5_2(), t52())
    })

    # 6 - Total de  Carregamento nos Portos----
    ## - Gráfico - Total de Carregamento (Mercadorias)----
    # Filtra os dados
    inf6_1 <- reactive({
      req(input$inf6municomp)
      if (input$inf6municomp == "Selecione um porto") {
        a <- inf6 %>% filter(localidade == input$inf6muni)
        } else {
        a <- inf6 %>% filter(localidade == input$inf6muni)
        b <- inf6 %>% filter(localidade == input$inf6municomp)
        df <- rbind(a,b)}
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf6_1(), {
      t61()
      downset_Server("inf6_1", inf6_1(), t61())
    })
    ## - Tabela - Total de Carregamento (Mercadorias) nos Portos----
    # Filtra os dados
    inf6_2 <- reactive({
      x <- inf6 %>%
        filter(ano == input$inf6ano, localidade != "Pará") %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf6_2(), {
      t62() 
      downset_Server("inf6_2", inf6_2(), t62())
    })

    # 7 - Total de  Descarregamento nos Portos----
    ## -  Gráfico - Total de Descarregamento no Porto----
    # Filtra os dados
    inf7_1 <- reactive({
      req(input$inf7municomp)
      if (input$inf7municomp == "Selecione um porto") {
        a <- inf7 %>% filter(localidade == input$inf7muni)
        } else {
        a <- inf7 %>% filter(localidade == input$inf7muni)
        b <- inf7 %>% filter(localidade == input$inf7municomp)
        df <- rbind(a,b)}
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf7_1(), {
      t71()
      downset_Server("inf7_1", inf7_1(), t71())
    })
    ## - Tabela - Total de Descarregamento (Mercadorias) nos Portos----
    # Filtra os dados
    inf7_2 <- reactive({
      x <- inf7 %>%
        filter(ano == input$inf7ano, localidade != "Pará") %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf7_2(), {
      t72()
      downset_Server("inf7_2", inf7_2(), t72())
    })

    # 8 - Total de  Movimentação nos Portos----
    ## - Gráfico - Total de Movimentação (Mercadorias) no Porto----
    # Filtra os dados
    inf8_1 <- reactive({
      req(input$inf8municomp)
      if (input$inf8municomp == "Selecione um porto") {
        a <- inf8 %>% filter(localidade == input$inf8muni)
        } else {
        a <- inf8 %>% filter(localidade == input$inf8muni)
        b <- inf8 %>% filter(localidade == input$inf8municomp)
        df <- rbind(a,b)}
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf8_1(), {
      t81()
      downset_Server("inf8_1", inf8_1(), t81())
    })
    ## - Tabela - Total de Movimentação (Mercadorias) nos Portos----
    # Filtra os dados
    inf8_2 <- reactive({
      x <- inf8 %>%
        filter(ano == input$inf8ano, localidade != "Pará") %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf8_2(), {
     t82()
      downset_Server("inf8_2", inf8_2(), t82())
    })

    # 9 - Total de  Embarcações----
    ## - Gráfico - Total de Embarcações (Unidades) no Porto----
    # Filtra os dados
    inf9_1 <- reactive({
      req(input$inf9municomp)
      if (input$inf9municomp == "Selecione um porto") {
        a <- inf9 %>% filter(localidade == input$inf9muni)
        } else {
        a <- inf9 %>% filter(localidade == input$inf9muni)
        b <- inf9 %>% filter(localidade == input$inf9municomp)
        df <- rbind(a,b)}
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf9_1(), {
      t91()
      downset_Server("inf9_1", inf9_1(), t91())
    })
    ## - Tabela - Total de Embarcações (Unidades) nos Portos----
    # Filtra os dados
    inf9_2 <- reactive({
      x <- inf9 %>%
        filter(ano == input$inf9ano, localidade != "Pará") %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf9_2(), {
      t92()
      downset_Server("inf9_2", inf9_2(), t92())
    })

    # 10 - Total das Receitas Operacionais----
    ## - Gráfico - Total das Receitas Operacionais (R$) do Porto----
    # Filtra os dados
    inf10_1 <- reactive({
      req(input$inf10municomp)
      if (input$inf10municomp == "Selecione um porto") {
        a <- inf10 %>% filter(localidade == input$inf10muni)
        } else {
        a <- inf10 %>% filter(localidade == input$inf10muni)
        b <- inf10 %>% filter(localidade == input$inf10municomp)
        df <- rbind(a,b)}
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf10_1(), {
      t101()
      downset_Server("inf10_1", inf10_1(), t101())
    })
    ## - Tabela - Total das Receitas Operacionais dos Portos----
    # Filtra os dados
    inf10_2 <- reactive({
      x <- inf10 %>%
        filter(ano == input$inf10ano, localidade != "Pará") %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf10_2(), {
      t102()
      downset_Server("inf10_2", inf10_2(), t102())
    })

    # 11 - Total de Pouso mais decolagem de Aeronaves----
    ## - Gráfico - Pouso mais decolagem de Aeronaves no aeroporto----
    # Filtra os dados
    inf11_1 <- reactive({
      req(input$inf11municomp1)
      if (input$inf11municomp1 == "Selecione um aeroporto") {
        a <- inf11 %>% filter(localidade == input$inf11muni, 
        ano == input$inf11ano1, categoria != "Total")
        } else {
        a <- inf11 %>% filter(localidade == input$inf11muni, 
        ano == input$inf11ano1, categoria != "Total")
        b <- inf11 %>% filter(localidade == input$inf11municomp1, 
        ano == input$inf11ano1, categoria != "Total")
        df <- rbind(a,b)}
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf11_1(), {
      t111()
      downset_Server("inf11_1", inf11_1(), t111())
    })
    ## - Tabela - Pouso mais decolagem de Aeronaves do aeroporto----
    # Filtra os dados
    inf11_2 <- reactive({
      x <- inf11 %>%
        filter(localidade == input$inf11muni) %>%
        pivot_wider(names_from = categoria, values_from = valor)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf11_2(), {
     t112()
      downset_Server("inf11_2", inf11_2(), t112())
    })

    ## - Gráfico - Total de Pouso mais decolagem de Aeronaves do aeroporto----
    # Filtra os dados
    inf11_3 <- reactive({
      req(input$inf11municomp2)
      if (input$inf11municomp2 == "Selecione um aeroporto") {
        a <- inf11 %>% filter(localidade == input$inf11muni, categoria == "Total")
        } else {
        a <- inf11 %>% filter(localidade == input$inf11muni, categoria == "Total")
        b <- inf11 %>% filter(localidade == input$inf11municomp2, categoria == "Total")}
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf11_3(), {
      t113()
      downset_Server("inf11_3", inf11_3(), t113())
    })

    # 12 - Total de Embarque mais desembarque de passageiros----
    ## - Gráfico - Embarque mais desembarque de passageiros do aeroporto----
    # Filtra os dados
    inf12_1 <- reactive({
      req(input$inf12municomp1)
      if (input$inf12municomp1 == "Selecione um aeroporto") {
        a <- inf12 %>% filter(localidade == input$inf12muni, 
        ano == input$inf12ano1, categoria != "Total")
        } else {
        a <- inf12 %>% filter(localidade == input$inf12muni, 
        ano == input$inf12ano1, categoria != "Total")
        b <- inf12 %>% filter(localidade == input$inf12municomp1, 
        ano == input$inf12ano1, categoria != "Total")
        df <- rbind(a,b)}
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf12_1(), {
     t121()
      downset_Server("inf12_1", inf12_1(), t121())
    })
    ## - Tabela - Total de Embarque mais desembarque de passageiros do aeroporto----
    # Filtra os dados
    inf12_2 <- reactive({
      x <- inf12 %>%
        filter(localidade == input$inf12muni) %>%
        pivot_wider(names_from = categoria, values_from = valor)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf12_2(), {
      t122()
      downset_Server("inf12_2", inf12_2(), t122())
    })
    ## - Gráfico - Total de Embarque mais desembarque de passageiros no aeroporto----
    # Filtra os dados
    inf12_3 <- reactive({
      req(input$inf12municomp2)
      if (input$inf12municomp2 == "Selecione um aeroporto") {
        a <- inf12 %>% filter(localidade == input$inf12muni, categoria == "Total")
        } else {
        a <- inf12 %>% filter(localidade == input$inf12muni, categoria == "Total")
        b <- inf12 %>% filter(localidade == input$inf12municomp2, categoria == "Total")}
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf12_3(), {
      t123()
      downset_Server("inf12_3", inf12_3(), t123())
    })

    # 13 - Abastecimento de Água Segundo Consumidores e Volume Consumido----
    ## - Gráfico - Abastecimento de Água Segundo Consumidores e Volume Consumido no município----
    # Filtra os dados
    inf13_1 <- reactive({
      req(input$inf13municomp)
      if (input$inf13municomp == "Selecione um município") {
        a <- inf13 %>%
          filter(localidade == input$inf13muni) %>%
          pivot_wider(names_from = categoria, values_from = valor)
          } else {
        a <- inf13 %>%
          filter(localidade == input$inf13muni) %>%
          pivot_wider(names_from = categoria, values_from = valor)
        b <- inf13 %>%
          filter(localidade == input$inf13municomp) %>%
          pivot_wider(names_from = categoria, values_from = valor)
        df <- rbind(a,b)}
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf13_1(), {
      t131()
      downset_Server("inf13_1", inf13_1(), t131())
    })
    ## - Tabela - Abastecimento de Água Segundo Consumidores e Volume Consumido nos municípios----
    # Filtra os dados
    inf13_2 <- reactive({
      ris <- inf13 %>%
        filter(ano == input$inf13ano, localidade == input$inf13muni) %>%
        pull(ri)
      x <- inf13 %>% 
        filter(ano == input$inf13ano, localidade != "Pará",
               ri == ris)
        
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf13_2(), {
      t132()
      downset_Server("inf13_2", inf13_2(), t132())
    })
  })
}

# Play do Módulo
# ui <- dashboardPage(
#   header = dashboardHeader(),
#   sidebar = dashboardSidebar(),
#   body = dashboardBody(fluidPage(infraestrutura_mp_ui("infraestrutura_mp")))
# )
# 
# 
# server <- function(input, output) {
#   infraestrutura_mp_Server("infraestrutura_mp")
# }
# 
# shinyApp(ui, server)
