# Funções de módulo de Social - Educação - Municipal
# Função de UI
social_educacao_mp_ui <- function(id) {
  fluidPage(tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "estilo.css")
  ),
  # Lista de Navegação lateral----
  div(class = "navbar_social",
      navbarPage(
        tags$b("Educação - Municípios"),
        navbarMenu(
          tags$b("Escolha um Indicador"),
          # 1 - Taxa de Aprovação no Ensino Fundamental por Esfera Administrativa----
          tabPanel(
            "Taxa de Aprovação no Ensino Fundamental",
            panel(
              ## Controle----
              heading =
                  h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Taxa de Aprovação no Ensino Fundamental"),
                  tags$div(
                    class = "seletor1",
                    pickerInput(
                      inputId = NS(id, "edu1muni"),
                      label = "Município",
                      choices = edu1 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                      width = "200px"
                    )
                  )),
            fluidRow(
              ## Gráfico de Barras - Taxa de Aprovação no Ensino Fundamental----
              box(
                title = textOutput(NS(id, "edu1txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                column(
                  2,
                  pickerInput(
                    inputId = NS(id, "edu1municomp1"),
                    label = "Comparar Município",
                    choices = NULL,
                    width = "200px",
                    options = list(`none-selected-text` = "Selecione um município")
                  )
                ),
                column(
                  6,
                  pickerInput(
                    inputId = NS(id, "edu1ano1"),
                    label = "Ano",
                    choices = sort(unique(edu1[["ano"]]), decreasing = T),
                    width = "100px"
                  )
                ),
                column(
                  12,
                  withSpinner(
                    echarts4rOutput(NS(id, "edu1graf")),
                    type = 8,
                    color = "#f17701",
                    size = 0.5
                  )
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu1_1")))
              ),
              ## Gráfico - Taxa de Aprovação no Ensino Fundamental----
              box(
                title = textOutput(NS(id, "edu1txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "edu1municomp2"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "edu1graf2")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu1_2")))
              ),
              ## Tabela - Taxa de Aprovação no Ensino Fundamental----
              box(
                title = textOutput(NS(id, "edu1txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "edu1tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu1_3")))
              ),
              ## Tabela - Taxa de Aprovação no Ensino Fundamental----
              box(
                title = textOutput(NS(id, "edu1txt4")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "edu1ano2"),
                  label = "Ano",
                  choices = sort(unique(edu1[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "edu1tab1"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu1_4")))
              )
            )
          ),
          # 2 - Taxa de Aprovação no Ensino Médio por Esfera Administrativa----
          tabPanel(
            "Taxa de Aprovação no Ensino Médio",
            panel(
              ## Controle----
              heading =
                  h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Taxa de Aprovação no Ensino Médio"),
                  tags$div(
                    class = "seletor1",
                    pickerInput(
                      inputId = NS(id, "edu2muni"),
                      label = "Município",
                      choices = edu2 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                      width = "200px"
                    )
                  )),
            fluidRow(
              ## Gráfico de Barras - Taxa de Aprovação no Ensino Médio----
              box(
                title = textOutput(NS(id, "edu2txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                column(
                  2,
                  pickerInput(
                    inputId = NS(id, "edu2municomp1"),
                    label = "Comparar Município",
                    choices = NULL,
                    width = "200px",
                    options = list(`none-selected-text` = "Selecione um município")
                  )
                ),
                column(
                  6,
                  pickerInput(
                    inputId = NS(id, "edu2ano1"),
                    label = "Ano",
                    choices = sort(unique(edu2[["ano"]]), decreasing = T),
                    width = "100px"
                  )
                ),
                column(
                  12,
                  withSpinner(
                    echarts4rOutput(NS(id, "edu2graf")),
                    type = 8,
                    color = "#f17701",
                    size = 0.5
                  )
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu2_1")))
              ),
              ## Gráfico - Taxa de Aprovação no Ensino Médio----
              box(
                title = textOutput(NS(id, "edu2txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "edu2municomp2"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "edu2graf2")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu2_2")))
              ),
              ## Tabela - Taxa de Aprovação no Ensino Médio----
              box(
                title = textOutput(NS(id, "edu2txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "edu2tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu2_3")))
              ),
              ## Tabela - Taxa de Aprovação no Ensino Médio----
              box(
                title = textOutput(NS(id, "edu2txt4")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "edu2ano2"),
                  label = "Ano",
                  choices = sort(unique(edu2[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "edu2tab1"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu2_4")))
              )
            )
          ),
          # 3 - Taxa de Reprovação no Ensino Fundamental por Esfera Administrativa----
          tabPanel(
            "Taxa de Reprovação no Ensino Fundamental",
            panel(
              ## Controle----
              heading =
                  h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Taxa de Reprovação no Ensino Fundamental"),
                  tags$div(
                    class = "seletor1",
                    pickerInput(
                      inputId = NS(id, "edu3muni"),
                      label = "Município",
                      choices = edu3 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                      width = "200px"
                    )
                  )),
            fluidRow(
              ## Gráfico de Barras - Taxa de Reprovação no Ensino Fundamental----
              box(
                title = textOutput(NS(id, "edu3txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                column(
                  2,
                  pickerInput(
                    inputId = NS(id, "edu3municomp1"),
                    label = "Comparar Município",
                    choices = NULL,
                    width = "200px",
                    options = list(`none-selected-text` = "Selecione um município")
                  )
                ),
                column(
                  6,
                  pickerInput(
                    inputId = NS(id, "edu3ano1"),
                    label = "Ano",
                    choices = sort(unique(edu3[["ano"]]), decreasing = T),
                    width = "100px"
                  )
                ),
                column(
                  12,
                  withSpinner(
                    echarts4rOutput(NS(id, "edu3graf")),
                    type = 8,
                    color = "#f17701",
                    size = 0.5
                  )
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu3_1")))
              ),
              ## Gráfico - Taxa de Reprovação no Ensino Fundamental----
              box(
                title = textOutput(NS(id, "edu3txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "edu3municomp2"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "edu3graf2")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu3_2")))
              ),
              ## Tabela - Taxa de Reprovação no Ensino Fundamental----
              box(
                title = textOutput(NS(id, "edu3txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "edu3tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu3_3")))
              ),
              ## Tabela - Taxa de Reprovação no Ensino Fundamental----
              box(
                title = textOutput(NS(id, "edu3txt4")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "edu3ano2"),
                  label = "Ano",
                  choices = sort(unique(edu3[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "edu3tab1"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu3_4")))
              )
            )
          ),
          # 4 - Taxa de Reprovação no Ensino Médio por Esfera Administrativa----
          tabPanel(
            "Taxa de Reprovação no Ensino Médio",
            panel(
              ## Controle----
              heading =
                  h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Taxa de Reprovação no Ensino Médio"),
                  tags$div(
                    class = "seletor1",
                    pickerInput(
                      inputId = NS(id, "edu4muni"),
                      label = "Município",
                      choices = edu4 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                      width = "200px"
                    )
                  )),
            fluidRow(
              ## Gráfico de Barras - Taxa de Reprovação no Ensino Médio----
              box(
                title = textOutput(NS(id, "edu4txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                column(
                  2,
                  pickerInput(
                    inputId = NS(id, "edu4municomp1"),
                    label = "Comparar Município",
                    choices = NULL,
                    width = "200px",
                    options = list(`none-selected-text` = "Selecione um município")
                  )
                ),
                column(
                  6,
                  pickerInput(
                    inputId = NS(id, "edu4ano1"),
                    label = "Ano",
                    choices = sort(unique(edu4[["ano"]]), decreasing = T),
                    width = "100px"
                  )
                ),
                column(
                  12,
                  withSpinner(
                    echarts4rOutput(NS(id, "edu4graf")),
                    type = 8,
                    color = "#f17701",
                    size = 0.5
                  )
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu4_1")))
              ),
              ## Gráfico - Taxa de Reprovação no Ensino Médio----
              box(
                title = textOutput(NS(id, "edu4txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "edu4municomp2"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "edu4graf2")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu4_2")))
              ),
              ## Tabela - Taxa de Reprovação no Ensino Médio----
              box(
                title = textOutput(NS(id, "edu4txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "edu4tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu4_3")))
              ),
              ## Tabela - Taxa de Reprovação no Ensino Médio----
              box(
                title = textOutput(NS(id, "edu4txt4")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "edu4ano2"),
                  label = "Ano",
                  choices = sort(unique(edu4[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "edu4tab1"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu4_4")))
              )
            )
          ),
          # 5 - Taxa de Abandono no Ensino Fundamental por Esfera Administrativa----
          tabPanel(
            "Taxa de Abandono no Ensino Fundamental",
            panel(
              ## Controle----
              heading =
                  h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Taxa de Abandono no Ensino Fundamental"),
                  tags$div(
                    class = "seletor1",
                    pickerInput(
                      inputId = NS(id, "edu5muni"),
                      label = "Município",
                      choices = edu5 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                      width = "200px"
                    )
                  )),
            fluidRow(
              ## Gráfico de Barras - Taxa de Abandono no Ensino Fundamental----
              box(
                title = textOutput(NS(id, "edu5txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                column(
                  2,
                  pickerInput(
                    inputId = NS(id, "edu5municomp1"),
                    label = "Comparar Município",
                    choices = NULL,
                    width = "200px",
                    options = list(`none-selected-text` = "Selecione um município")
                  )
                ),
                column(
                  6,
                  pickerInput(
                    inputId = NS(id, "edu5ano1"),
                    label = "Ano",
                    choices = sort(unique(edu5[["ano"]]), decreasing = T),
                    width = "100px"
                  )
                ),
                column(
                  12,
                  withSpinner(
                    echarts4rOutput(NS(id, "edu5graf")),
                    type = 8,
                    color = "#f17701",
                    size = 0.5
                  )
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu5_1")))
              ),
              ## Gráfico - Taxa de Abandono no Ensino Fundamental----
              box(
                title = textOutput(NS(id, "edu5txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "edu5municomp2"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "edu5graf2")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu5_2")))
              ),
              ## Tabela - Taxa de Abandono no Ensino Fundamental----
              box(
                title = textOutput(NS(id, "edu5txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "edu5tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu_3")))
              ),
              ## Tabela - Taxa de Abandono no Ensino Fundamental----
              box(
                title = textOutput(NS(id, "edu5txt4")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "edu5ano2"),
                  label = "Ano",
                  choices = sort(unique(edu5[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "edu5tab1"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu5_4")))
              )
            )
          ),
          # 6 - Taxa de Abandono no Ensino Médio por Esfera Administrativa----
          tabPanel(
            "Taxa de Abandono no Ensino Médio",
            panel(
              ## Controle----
              heading =
                  h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Taxa de Abandono no Ensino Médio"),
                  tags$div(
                    class = "seletor1",
                    pickerInput(
                      inputId = NS(id, "edu6muni"),
                      label = "Município",
                      choices = edu6 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                      width = "200px"
                    )
                  )),
            fluidRow(
              ## Gráfico de Barras - Taxa de Abandono no Ensino Médio----
              box(
                title = textOutput(NS(id, "edu6txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                column(
                  2,
                  pickerInput(
                    inputId = NS(id, "edu6municomp1"),
                    label = "Comparar Município",
                    choices = NULL,
                    width = "200px",
                    options = list(`none-selected-text` = "Selecione um município")
                  )
                ),
                column(
                  6,
                  pickerInput(
                    inputId = NS(id, "edu6ano1"),
                    label = "Ano",
                    choices = sort(unique(edu6[["ano"]]), decreasing = T),
                    width = "100px"
                  )
                ),
                column(
                  12,
                  withSpinner(
                    echarts4rOutput(NS(id, "edu6graf")),
                    type = 8,
                    color = "#f17701",
                    size = 0.5
                  )
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu6_1")))
              ),
              ## Gráfico - Taxa de Abandono no Ensino Médio----
              box(
                title = textOutput(NS(id, "edu6txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "edu6municomp2"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "edu6graf2")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu6_2")))
              ),
              ## Tabela - Taxa de Abandono no Ensino Médio----
              box(
                title = textOutput(NS(id, "edu6txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "edu6tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu6_3")))
              ),
              ## Tabela - Taxa de Abandono no Ensino Médio----
              box(
                title = textOutput(NS(id, "edu6txt4")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "edu6ano2"),
                  label = "Ano",
                  choices = sort(unique(edu6[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "edu6tab1"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu6_4")))
              )
            )
          ),
          # 7 - Distorção Idade-Série Total por Nível de Ensino----
          tabPanel(
            "Distorção Idade-Série Total por Nível de Ensino",
            panel(
              ## Controle----
              heading =
                  h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Distorção Idade-Série Total por Nível de Ensino"),
                  tags$div(
                    class = "seletor1",
                    pickerInput(
                      inputId = NS(id, "edu7muni"),
                      label = "Município",
                      choices = edu7 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                      width = "200px"
                    )
                  )),
            fluidRow(
              ## Gráfico de Barras - Distorção Idade-Série Total por Nível de Ensino----
              box(
                title = textOutput(NS(id, "edu7txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                column(
                  2,
                  pickerInput(
                    inputId = NS(id, "edu7municomp1"),
                    label = "Comparar Município",
                    choices = NULL,
                    width = "200px",
                    options = list(`none-selected-text` = "Selecione um município")
                  )
                ),
                column(
                  6,
                  pickerInput(
                    inputId = NS(id, "edu7ano1"),
                    label = "Ano",
                    choices = sort(unique(edu7[["ano"]]), decreasing = T),
                    width = "100px"
                  )
                ),
                column(
                  12,
                  withSpinner(
                    echarts4rOutput(NS(id, "edu7graf")),
                    type = 8,
                    color = "#f17701",
                    size = 0.5
                  )
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu7_1")))
              ),
              ## Tabela - Distorção Idade-Série Total por Nível de Ensino----
              box(
                title = textOutput(NS(id, "edu7txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "edu7tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu7_2")))
              ),
              ## Tabela - Distorção Idade-Série Total por Nível de Ensino----
              box(
                title = textOutput(NS(id, "edu7txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "edu7ano2"),
                  label = "Ano",
                  choices = sort(unique(edu7[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "edu7tab1"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu7_3")))
              )
            )
          ),
          # 8 -  IDEB - Escola pública - 5ª ano (séries iniciais)----
          tabPanel(
            "IDEB - Escola pública - 5ª ano (séries iniciais)",
            panel(
              ## Controle----
              heading =
                  h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "IDEB - Escola pública - 5ª ano (séries iniciais)"),
                  tags$div(
                    class = "seletor1",
                    pickerInput(
                      inputId = NS(id, "edu8muni"),
                      label = "Município",
                      choices = edu8 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                      width = "200px"
                    )
                  )),
            fluidRow(
              ## Gráfico - IDEB - Escola pública - 5ª ano (séries iniciais)----
              box(
                title = textOutput(NS(id, "edu8txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                # Comparar muicípios
                pickerInput(
                  inputId = NS(id, "edu8municomp1"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "edu8graf")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu8_1")))
              )
            ),
            fluidRow(
              ## Tabela - IDEB - Escola pública - 5ª ano (séries iniciais)----
              box(
                title = textOutput(NS(id, "edu8txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "edu8ano"),
                  label = "Ano",
                  choices = sort(unique(edu8[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "edu8tab2"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu8_2")))
              )
            )
          ),
          # 9 -  IDEB - Escola pública - 9ª ano (séries finais)----
          tabPanel(
            "IDEB - Escola pública - 9ª ano (séries finais)",
            panel(
              ## Controle----
              heading =
                  h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "IDEB - Escola pública - 9ª ano (séries finais)"),
                  tags$div(
                    class = "seletor1",
                    pickerInput(
                      inputId = NS(id, "edu9muni"),
                      label = "Município",
                      choices = edu9 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                      width = "200px"
                    )
                  )),
            fluidRow(
              ## Gráfico - IDEB - Escola pública - 9ª ano (séries finais)----
              box(
                title = textOutput(NS(id, "edu9txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                # Comparar muicípios
                pickerInput(
                  inputId = NS(id, "edu9municomp1"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "edu9graf")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu9_1")))
              )
            ),
            fluidRow(
              ## Tabela - IDEB - Escola pública - 9ª ano (séries finais)----
              box(
                title = textOutput(NS(id, "edu9txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "edu9ano"),
                  label = "Ano",
                  choices = sort(unique(edu9[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "edu9tab2"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu9_2")))
              )
            )
          ),
          # 10 - Número de Matrículas no Ensino Pré-Escolar por Esfera Administrativa----
          tabPanel(
            "Número de Matrículas no Ensino Pré-Escolar",
            panel(
              ## Controle----
              heading =
                  h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Número de Matrículas no Ensino Pré-Escolar"),
                  tags$div(
                    class = "seletor1",
                    pickerInput(
                      inputId = NS(id, "edu10muni"),
                      label = "Município",
                      choices = edu10 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                      width = "200px"
                    )
                  )),
            fluidRow(
              ## Gráfico - Número de Matrículas no Ensino Pré-Escolar----
              box(
                title = textOutput(NS(id, "edu10txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                column(
                  2,
                  pickerInput(
                    inputId = NS(id, "edu10municomp1"),
                    label = "Comparar Município",
                    choices = NULL,
                    width = "200px",
                    options = list(`none-selected-text` = "Selecione um município")
                  )
                ),
                column(
                  6,
                  pickerInput(
                    inputId = NS(id, "edu10ano1"),
                    label = "Ano",
                    choices = sort(unique(edu10[["ano"]]), decreasing = T),
                    width = "100px"
                  )
                ),
                column(
                  12,
                  withSpinner(
                    echarts4rOutput(NS(id, "edu10graf")),
                    type = 8,
                    color = "#f17701",
                    size = 0.5
                  )
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu10_1")))
              ),
              ## Gráfico - Número de Matrículas no Ensino Pré-Escolar----
              box(
                title = textOutput(NS(id, "edu10txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "edu10municomp2"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "edu10graf2")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu10_2")))
              ),
              ## Tabela - Número de Matrículas no Ensino Pré-Escolar----
              box(
                title = textOutput(NS(id, "edu10txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "edu10tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu10_3")))
              ),
              ## Tabela - Número de Matrículas no Ensino Pré-Escolar----
              box(
                title = textOutput(NS(id, "edu10txt4")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "edu10ano2"),
                  label = "Ano",
                  choices = sort(unique(edu10[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "edu10tab1"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu10_4")))
              )
            )
          ),
          # 11 - Número de Matrículas no Ensino Fundamental por Esfera Administrativa----
          tabPanel(
            "Número de Matrículas no Ensino Fundamental",
            panel(
              ## Controle----
              heading =
                  h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Número de Matrículas no Ensino Fundamental"),
                  tags$div(
                    class = "seletor1",
                    # select Município
                    pickerInput(
                      inputId = NS(id, "edu11muni"),
                      label = "Município",
                      choices = edu11 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                      width = "200px"
                    )
                  )),
            fluidRow(
              ## Gráfico - Número de Matrículas no Ensino Fundamental----
              box(
                title = textOutput(NS(id, "edu11txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                column(
                  2,
                  pickerInput(
                    inputId = NS(id, "edu11municomp1"),
                    label = "Comparar Município",
                    choices = NULL,
                    width = "200px",
                    options = list(`none-selected-text` = "Selecione um município")
                  )
                ),
                column(
                  6,
                  pickerInput(
                    inputId = NS(id, "edu11ano1"),
                    label = "Ano",
                    choices = sort(unique(edu11[["ano"]]), decreasing = T),
                    width = "100px"
                  )
                ),
                column(
                  12,
                  withSpinner(
                    echarts4rOutput(NS(id, "edu11graf")),
                    type = 8,
                    color = "#f17701",
                    size = 0.5
                  )
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu11_1")))
              ),
              ## Gráfico - Número de Matrículas no Ensino Fundamental----
              box(
                title = textOutput(NS(id, "edu11txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "edu11municomp2"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "edu11graf2")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu11_2")))
              ),
              ## Tabela - Número de Matrículas no Ensino Fundamental----
              box(
                title = textOutput(NS(id, "edu11txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "edu11tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu11_3")))
              ),
              ## Tabela - Número de Matrículas no Ensino Fundamental----
              box(
                title = textOutput(NS(id, "edu11txt4")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "edu11ano2"),
                  label = "Ano",
                  choices = sort(unique(edu11[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "edu11tab1"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu11_4")))
              )
            )
          ),
          # 12 - Número de Matrículas no Ensino Médio por Esfera Administrativa----
          tabPanel(
            "Número de Matrículas no Ensino Médio",
            panel(
              ## Controle----
              heading =
                  h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Número de Matrículas no Ensino Médio"),
                  tags$div(
                    class = "seletor1",
                    pickerInput(
                      inputId = NS(id, "edu12muni"),
                      label = "Município",
                      choices = edu12 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                      width = "200px"
                    )
                  )),
            fluidRow(
              ## Gráfico - Número de Matrículas no Ensino Médio----
              box(
                title = textOutput(NS(id, "edu12txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                column(
                  2,
                  pickerInput(
                    inputId = NS(id, "edu12municomp1"),
                    label = "Comparar Município",
                    choices = NULL,
                    width = "200px",
                    options = list(`none-selected-text` = "Selecione um município")
                  )
                ),
                column(
                  6,
                  pickerInput(
                    inputId = NS(id, "edu12ano1"),
                    label = "Ano",
                    choices = sort(unique(edu12[["ano"]]), decreasing = T),
                    width = "100px"
                  )
                ),
                column(
                  12,
                  withSpinner(
                    echarts4rOutput(NS(id, "edu12graf")),
                    type = 8,
                    color = "#f17701",
                    size = 0.5
                  )
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu12_1")))
              ),
              ## Gráfico - Número de Matrículas no Ensino Médio----
              box(
                title = textOutput(NS(id, "edu12txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "edu12municomp2"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "edu12graf2")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu12_2")))
              ),
              ## Tabela - Número de Matrículas no Ensino Médio----
              box(
                title = textOutput(NS(id, "edu12txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "edu12tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu12_3")))
              ),
              ## Tabela - Número de Matrículas no Ensino Médio----
              box(
                title = textOutput(NS(id, "edu12txt4")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "edu12ano2"),
                  label = "Ano",
                  choices = sort(unique(edu12[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "edu12tab1"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu12_4")))
              )
            )
          ),
          # 13 - Média de Alunos por Turma por Nível de Ensino----
          tabPanel(
            "Média de Alunos por Turma por Nível de Ensino",
            panel(
              ## Controle----
              heading =
                  h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Média de Alunos por Turma por Nível de Ensino"),
                  tags$div(
                    class = "seletor1",
                    pickerInput(
                      inputId = NS(id, "edu13muni"),
                      label = "Município",
                      choices = edu13 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                      width = "200px"
                    )
                  )),
            fluidRow(
              ## Gráfico - Média de Alunos por Turma por Nível de Ensino----
              box(
                title = textOutput(NS(id, "edu13txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                column(
                  2,
                  pickerInput(
                    inputId = NS(id, "edu13municomp1"),
                    label = "Comparar Município",
                    choices = NULL,
                    width = "200px",
                    options = list(`none-selected-text` = "Selecione um município")
                  )
                ),
                column(
                  6,
                  pickerInput(
                    inputId = NS(id, "edu13ano1"),
                    label = "Ano",
                    choices = sort(unique(edu13[["ano"]]), decreasing = T),
                    width = "100px"
                  )
                ),
                column(
                  12,
                  withSpinner(
                    echarts4rOutput(NS(id, "edu13graf")),
                    type = 8,
                    color = "#f17701",
                    size = 0.5
                  )
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu13_1")))
              ),
              ## Tabela - Média de Alunos por Turma por Nível de Ensino----
              box(
                title = textOutput(NS(id, "edu13txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "edu13tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu13_2")))
              ),
              ## Tabela - Média de Alunos por Turma por Nível de Ensino----
              box(
                title = textOutput(NS(id, "edu13txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "edu13ano2"),
                  label = "Ano",
                  choices = sort(unique(edu13[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "edu13tab1"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu13_3")))
              )
            )
          ),
          # 14 - Número de Docentes no Ensino Pré-escolar por Esfera Administrativa----
          tabPanel(
            "Número de Docentes no Ensino Pré-escolar",
            panel(
              ## Controle----
              heading =
                  h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Número de Docentes no Ensino Pré-escolar"),
                  tags$div(
                    class = "seletor1",
                    pickerInput(
                      inputId = NS(id, "edu14muni"),
                      label = "Município",
                      choices = edu14 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                      width = "200px"
                    )
                  )),
            fluidRow(
              ## Gráfico - Número de Docentes no Ensino Pré-escolar----
              box(
                title = textOutput(NS(id, "edu14txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                column(
                  2,
                  pickerInput(
                    inputId = NS(id, "edu14municomp1"),
                    label = "Comparar Município",
                    choices = NULL,
                    width = "200px",
                    options = list(`none-selected-text` = "Selecione um município")
                  )
                ),
                column(
                  6,
                  pickerInput(
                    inputId = NS(id, "edu14ano1"),
                    label = "Ano",
                    choices = sort(unique(edu14[["ano"]]), decreasing = T),
                    width = "100px"
                  )
                ),
                column(
                  12,
                  withSpinner(
                    echarts4rOutput(NS(id, "edu14graf")),
                    type = 8,
                    color = "#f17701",
                    size = 0.5
                  )
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu14_1")))
              ),
              ## Gráfico - Número de Docentes no Ensino Pré-escolar----
              box(
                title = textOutput(NS(id, "edu14txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "edu14municomp2"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "edu14graf2")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu14_2")))
              ),
              
              ## Tabela - Número de Docentes no Ensino Pré-escolar----
              box(
                title = textOutput(NS(id, "edu14txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "edu14tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu14_3")))
              ),
              ## Tabela - Número de Docentes no Ensino Pré-escolar----
              box(
                title = textOutput(NS(id, "edu14txt4")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "edu14ano2"),
                  label = "Ano",
                  choices = sort(unique(edu14[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "edu14tab1"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu14_4")))
              )
            )
          ),
          # 15 - Número de Docentes no Ensino Fundamental por Esfera Administrativa----
          tabPanel(
            "Número de Docentes no Ensino Fundamental",
            panel(
              ## Controle----
              heading =
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Número de Docentes no Ensino Fundamental"),
                  tags$div(
                    class = "seletor1",
                    pickerInput(
                      inputId = NS(id, "edu15muni"),
                      label = "Município",
                      choices = edu15 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                      width = "200px"
                    )
                  )),
            fluidRow(
              ## Gráfico - Número de Docentes no Ensino Fundamental----
              box(
                title = textOutput(NS(id, "edu15txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                column(
                  2,
                  pickerInput(
                    inputId = NS(id, "edu15municomp1"),
                    label = "Comparar Município",
                    choices = NULL,
                    width = "200px",
                    options = list(`none-selected-text` = "Selecione um município")
                  )
                ),
                column(
                  6,
                  pickerInput(
                    inputId = NS(id, "edu15ano1"),
                    label = "Ano",
                    choices = sort(unique(edu15[["ano"]]), decreasing = T),
                    width = "100px"
                  )
                ),
                column(
                  12,
                  withSpinner(
                    echarts4rOutput(NS(id, "edu15graf")),
                    type = 8,
                    color = "#f17701",
                    size = 0.5
                  )
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu15_1")))
              ),
              ## Gráfico - Número de Docentes no Ensino Fundamental----
              box(
                title = textOutput(NS(id, "edu15txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "edu15municomp2"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "edu15graf2")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu15_2")))
              ),
              ## Tabela - Número de Docentes no Ensino Fundamental----
              box(
                title = textOutput(NS(id, "edu15txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "edu15tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu15_3")))
              ),
              ## Tabela - Número de Docentes no Ensino Fundamental----
              box(
                title = textOutput(NS(id, "edu15txt4")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "edu15ano2"),
                  label = "Ano",
                  choices = sort(unique(edu15[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "edu15tab1"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu15_4")))
              )
            )
          ),
          # 16 - Número de Docentes no Ensino Médio por Esfera Administrativa----
          tabPanel(
            "Número de Docentes no Ensino Médio",
            panel(
              ## Controle----
              heading =
                  h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Número de Docentes no Ensino Médio"),
                  tags$div(
                    class = "seletor1",
                    pickerInput(
                      inputId = NS(id, "edu16muni"),
                      label = "Município",
                      choices = edu16 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                      width = "200px"
                    )
                  )),
            fluidRow(
              ## Gráfico - Número de Docentes no Ensino Médio----
              box(
                title = textOutput(NS(id, "edu16txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                column(
                  2,
                  pickerInput(
                    inputId = NS(id, "edu16municomp1"),
                    label = "Comparar Município",
                    choices = NULL,
                    width = "200px",
                    options = list(`none-selected-text` = "Selecione um município")
                  )
                ),
                column(
                  6,
                  pickerInput(
                    inputId = NS(id, "edu16ano1"),
                    label = "Ano",
                    choices = sort(unique(edu16[["ano"]]), decreasing = T),
                    width = "100px"
                  )
                ),
                column(
                  12,
                  withSpinner(
                    echarts4rOutput(NS(id, "edu16graf")),
                    type = 8,
                    color = "#f17701",
                    size = 0.5
                  )
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu16_1")))
              ),
              ## Gráfico - Número de Docentes no Ensino Médio----
              box(
                title = textOutput(NS(id, "edu16txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "edu16municomp2"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "edu16graf2")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu16_2")))
              ),
              ## Tabela - Número de Docentes no Ensino Médio----
              box(
                title = textOutput(NS(id, "edu16txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "edu16tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu16_3")))
              ),
              ## Tabela - Número de Docentes no Ensino Médio----
              box(
                title = textOutput(NS(id, "edu16txt4")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "edu16ano2"),
                  label = "Ano",
                  choices = sort(unique(edu16[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "edu16tab1"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu16_4")))
              )
            )
          ),
          # 17 - Estabelecimentos de Pré-Escola por Esfera Administrativa----
          tabPanel(
            "Estabelecimentos de Pré-Escola",
            panel(
              ## Controle----
              heading =
                  h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Estabelecimentos de Pré-Escola"),
                  tags$div(
                    class = "seletor1",
                    pickerInput(
                      inputId = NS(id, "edu17muni"),
                      label = "Município",
                      choices = edu17 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                      width = "200px"
                    )
                  )),
            fluidRow(
              ## Gráfico - Estabelecimentos de Pré-Escola----
              box(
                title = textOutput(NS(id, "edu17txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                column(
                  2,
                  pickerInput(
                    inputId = NS(id, "edu17municomp1"),
                    label = "Comparar Município",
                    choices = NULL,
                    width = "200px",
                    options = list(`none-selected-text` = "Selecione um município")
                  )
                ),
                column(
                  6,
                  pickerInput(
                    inputId = NS(id, "edu17ano1"),
                    label = "Ano",
                    choices = sort(unique(edu17[["ano"]]), decreasing = T),
                    width = "100px"
                  )
                ),
                column(
                  12,
                  withSpinner(
                    echarts4rOutput(NS(id, "edu17graf")),
                    type = 8,
                    color = "#f17701",
                    size = 0.5
                  )
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu17_1")))
              ),
              ## Gráfico - Estabelecimentos de Pré-Escola----
              box(
                title = textOutput(NS(id, "edu17txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "edu17municomp2"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "edu17graf2")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu17_2")))
              ),
              ## Tabela - Estabelecimentos de Pré-Escola----
              box(
                title = textOutput(NS(id, "edu17txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "edu17tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu17_")))
              ),
              ## Tabela - Estabelecimentos de Pré-Escola----
              box(
                title = textOutput(NS(id, "edu17txt4")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "edu17ano2"),
                  label = "Ano",
                  choices = sort(unique(edu17[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "edu17tab1"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu17_3")))
              )
            )
          ),
          # 18 - Estabelecimentos de Ensino Fundamental por Esfera Administrativa----
          tabPanel(
            "Estabelecimentos de Ensino Fundamental",
            panel(
              ## Controle----
              heading =
                  h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Estabelecimentos de Ensino Fundamental"),
                  tags$div(
                    class = "seletor1",
                    pickerInput(
                      inputId = NS(id, "edu18muni"),
                      label = "Município",
                      choices = edu18 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                      width = "200px"
                    )
                  )),
            fluidRow(
              ## Gráfico - Estabelecimentos de Ensino Fundamental----
              box(
                title = textOutput(NS(id, "edu18txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                column(
                  2,
                  pickerInput(
                    inputId = NS(id, "edu18municomp1"),
                    label = "Comparar Município",
                    choices = NULL,
                    width = "200px",
                    options = list(`none-selected-text` = "Selecione um município")
                  )
                ),
                column(
                  6,
                  pickerInput(
                    inputId = NS(id, "edu18ano1"),
                    label = "Ano",
                    choices = sort(unique(edu18[["ano"]]), decreasing = T),
                    width = "100px"
                  )
                ),
                column(
                  12,
                  withSpinner(
                    echarts4rOutput(NS(id, "edu18graf")),
                    type = 8,
                    color = "#f17701",
                    size = 0.5
                  )
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu18_1")))
              ),
              ## Gráfico - Estabelecimentos de Ensino Fundamental----
              box(
                title = textOutput(NS(id, "edu18txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "edu18municomp2"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "edu18graf2")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu18_2")))
              ),
              ## Tabela - Estabelecimentos de Ensino Fundamental----
              box(
                title = textOutput(NS(id, "edu18txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "edu18tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu18_3")))
              ),
              ## Tabela - Estabelecimentos de Ensino Fundamental----
              box(
                title = textOutput(NS(id, "edu18txt4")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "edu18ano2"),
                  label = "Ano",
                  choices = sort(unique(edu18[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "edu18tab1"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu18_4")))
              )
            )
          ),
          
          # 19 - Estabelecimentos de Ensino Médio por Esfera Administrativa----
          tabPanel(
            "Estabelecimentos de Ensino Médio",
            panel(
              ## Controle----
              heading =
                  h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Estabelecimentos de Ensino Médio"),
                  tags$div(
                    class = "seletor1",
                    pickerInput(
                      inputId = NS(id, "edu19muni"),
                      label = "Município",
                      choices = edu19 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                      width = "200px"
                    )
                  )),
            fluidRow(
              ## Gráfico - Estabelecimentos de Ensino Médio----
              box(
                title = textOutput(NS(id, "edu19txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                column(
                  2,
                  pickerInput(
                    inputId = NS(id, "edu19municomp1"),
                    label = "Comparar Município",
                    choices = NULL,
                    width = "200px",
                    options = list(`none-selected-text` = "Selecione um município")
                  )
                ),
                column(
                  6,
                  pickerInput(
                    inputId = NS(id, "edu19ano1"),
                    label = "Ano",
                    choices = sort(unique(edu19[["ano"]]), decreasing = T),
                    width = "100px"
                  )
                ),
                column(
                  12,
                  withSpinner(
                    echarts4rOutput(NS(id, "edu19graf")),
                    type = 8,
                    color = "#f17701",
                    size = 0.5
                  )
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu19_1")))
              ),
              ## Gráfico - Estabelecimentos de Ensino Médio----
              box(
                title = textOutput(NS(id, "edu19txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "edu19municomp2"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "edu19graf2")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu19_2")))
              ),
              ## Tabela - Estabelecimentos de Ensino Médio----
              box(
                title = textOutput(NS(id, "edu19txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "edu19tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu19_3")))
              ),
              ## Tabela - Estabelecimentos de Ensino Médio----
              box(
                title = textOutput(NS(id, "edu19txt4")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "edu19ano2"),
                  label = "Ano",
                  choices = sort(unique(edu19[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "edu19tab1"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:"), "INEP"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "edu19_4")))
              )
            )
          )
        )
      )))
}

# Função do modulo servidor
social_educacao_mp_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    #TÍTULOS----
    # 1 - Taxa de Aprovação no Ensino Fundamental por Esfera Administrativa----
    ## Gráfico de barras - Taxa de Aprovação no Ensino Fundamental----
    t11 <- reactive({
      req(input$edu1municomp1)
      if (input$edu1municomp1 == "Selecione um município") {
        paste0(
          "Taxa de Aprovação no Ensino Fundamental por Esfera Administrativa, ",
          input$edu1muni,
          " - ",
          input$edu1ano1
        )
      } else {
        paste0(
          "Taxa de Aprovação no Ensino Fundamental por Esfera Administrativa, ",
          input$edu1muni,
          " x ",
          input$edu1municomp1,
          " - ",
          input$edu1ano1
        )
      }
    })
    ## Gráfico de linha - Taxa de Aprovação no Ensino Fundamental----
    t12 <- reactive({
      req(input$edu1municomp2)
      if (input$edu1municomp2 == "Selecione um município") {
        paste0(
          "Taxa de Aprovação Total no Ensino Fundamental, ",
          input$edu1muni,
          " - ",
          min(edu1$ano),
          " a ",
          max(edu1$ano)
        )
      } else {
        paste0(
          "Taxa de Aprovação Total no Ensino Fundamental, ",
          input$edu1muni,
          " x ",
          input$edu1municomp2,
          " - ",
          min(edu1$ano),
          " a ",
          max(edu1$ano)
        )
      }
    })
    ## Tabela - Taxa de Aprovação no Ensino Fundamental----
    t13 <- reactive({
      paste0(
        "Taxa de Aprovação no Ensino Fundamental por Esfera Administrativa, ",
        input$edu1muni,
        " - ",
        min(edu1$ano),
        " a ",
        max(edu1$ano)
      )
    })
    ## Tabela - Taxa de Aprovação no Ensino Fundamental----
    t14 <- reactive({
      ri <- edu1 %>%
        filter(ano == input$edu1ano2, localidade == input$edu1muni) %>%
        pull(ri)
      paste0(
        "Taxa de Aprovação no Ensino Fundamental
      por Esfera Administrativa  e Município, Região de Integração ",
        unique(ri),
        " - ",
        input$edu1ano2
      )
    })
    # 2 - Taxa de Aprovação no Ensino Médio por Esfera Administrativa----
    ## Gráfico de barra- Taxa de Aprovação no Ensino Médio----
    t21 <- reactive({
      req(input$edu2municomp1)
      if (input$edu2municomp1 == "Selecione um município") {
        paste0(
          "Taxa de Aprovação no Ensino Médio
        por Esfera Administrativa, ",
          input$edu2muni,
          " - ",
          input$edu2ano1
        )
      } else {
        paste0(
          "Taxa de Aprovação no Ensino
        Médio por Esfera Administrativa, ",
          input$edu2muni,
          " x ",
          input$edu2municomp1,
          " - ",
          input$edu2ano1
        )
      }
    })
    ## Gráfico de linha - Taxa de Aprovação no Ensino Médio----
    t22 <- reactive({
      req(input$edu2municomp2)
      if (input$edu2municomp2 == "Selecione um município") {
        paste0(
          "Taxa de Aprovação Total no Ensino, ",
          input$edu2muni,
          " - ",
          min(edu2$ano),
          " a ",
          max(edu2$ano)
        )
      } else {
        paste0(
          "Taxa de Aprovação no Ensino Médio, ",
          input$edu2muni,
          " x ",
          input$edu2municomp2,
          " - ",
          min(edu2$ano),
          " a ",
          max(edu2$ano)
        )
      }
    })
    ## Tabela - Taxa de Aprovação no Ensino Médio----
    t23 <- reactive({
      paste0(
        "Taxa de Aprovação no Ensino Médio por Esfera Administrativa, ",
        input$edu2muni,
        " - ",
        min(edu2$ano),
        " a ",
        max(edu2$ano)
      )
    })
    ## Tabela - Taxa de Aprovação no Ensino Médio----
    t24 <- reactive({
      ri <- edu2 %>%
        filter(ano == input$edu2ano2, localidade == input$edu2muni) %>%
        pull(ri)
      paste0(
        "Taxa de Aprovação no Ensino Fundamental por Esfera Administrativa  e Município, Região de Integração ",
        unique(ri),
        " - ",
        input$edu2ano2
      )
    })
    # 3 - Taxa de Reprovação no Ensino Fundamental por Esfera Administrativa----
    ## Gráfico de barras - Taxa de Reprovação no Ensino Fundamental----
    t31 <- reactive({
      req(input$edu3municomp1)
      if (input$edu3municomp1 == "Selecione um município") {
        paste0(
          "Taxa de Reprovação no Ensino Fundamental por Esfera Administrativa, ",
          input$edu3muni,
          " - ",
          input$edu3ano1
        )
      } else {
        paste0(
          "Taxa de Reprovação no Ensino Fundamental por Esfera Administrativa, ",
          input$edu3muni,
          " x ",
          input$edu3municomp1,
          " - ",
          input$edu3ano1
        )
      }
    })
    ## Gráfico de linha - Taxa de Reprovação no Ensino Fundamental----
    t32 <- reactive({
      req(input$edu3municomp2)
      if (input$edu3municomp2 == "Selecione um município") {
        paste0(
          "Taxa de Reprovação Total no Ensino Fundamental, ",
          input$edu3muni,
          " - ",
          min(edu3$ano),
          " a ",
          max(edu3$ano)
        )
      } else {
        paste0(
          "Taxa de Reprovação Total no Ensino Fundamental, ",
          input$edu3muni,
          " x ",
          input$edu3municomp2,
          " - ",
          min(edu3$ano),
          " a ",
          max(edu3$ano)
        )
      }
    })
    ## Tabela - Taxa de Reprovação no Ensino Fundamental----
    t33 <- reactive({
      paste0(
        "Taxa de Reprovação no Ensino Fundamental por Esfera Administrativa, ",
        input$edu3muni,
        " - ",
        min(edu3$ano),
        " a ",
        max(edu3$ano)
      )
    })
    ## Tabela - Taxa de Reprovação no Ensino Fundamental----
    t34 <- reactive({
      ri <- edu3 %>%
        filter(ano == input$edu3ano2, localidade == input$edu3muni) %>%
        pull(ri)
      paste0(
        "Taxa de Reprovação no Ensino Fundamental por Esfera Administrativa  e Município, Região de Integração ",
        unique(ri),
        " - ",
        input$edu3ano2
      )
    })
    # 4 - Taxa de Reprovação no Ensino Médio por Esfera Administrativa----
    ## Gráfico de barras - Taxa de Reprovação no Ensino Médio----
    t41 <- reactive({
      req(input$edu4municomp1)
      if (input$edu4municomp1 == "Selecione um município") {
        paste0(
          "Taxa de Reprovação no Ensino Médio por Esfera Administrativa, ",
          input$edu4muni,
          " - ",
          input$edu4ano1
        )
      } else {
        paste0(
          "Taxa de Reprovação no Ensino Médio por Esfera Administrativa, ",
          input$edu4muni,
          " x ",
          input$edu4municomp1,
          " - ",
          input$edu4ano1
        )
      }
    })
    ## Gráfico de linha - Taxa de Reprovação no Ensino Médio----
    t42 <- reactive({
      req(input$edu4municomp2)
      if (input$edu4municomp2 == "Selecione um município") {
        paste0(
          "Taxa de Reprovação Total no Ensino, ",
          input$edu4muni,
          " - ",
          input$edu4ano,
          " - ",
          min(edu4$ano),
          " a ",
          max(edu4$ano)
        )
      } else {
        paste0(
          "Taxa de Reprovação Total no Ensino, ",
          input$edu4muni,
          " x ",
          input$edu4municomp2,
          " - ",
          input$edu4ano,
          " - ",
          min(edu4$ano),
          " a ",
          max(edu4$ano)
        )
      }
    })
    ## Tabela - Taxa de Reprovação no Ensino Médio----
    t43 <- reactive({
      paste0(
        "Taxa de Reprovação no Ensino Médio por Esfera Administrativa, ",
        input$edu4muni,
        " - ",
        min(edu4$ano),
        " a ",
        max(edu4$ano)
      )
    })
    ## Tabela - Taxa de Reprovação no Ensino Médio----
    t44 <- reactive({
      ri <- edu4 %>%
        filter(ano == input$edu4ano2, localidade == input$edu4muni) %>%
        pull(ri)
      paste0(
        "Taxa de Reprovação no Ensino Médio por Esfera Administrativa  e Município, Região de Integração ",
        unique(ri),
        " - ",
        input$edu4ano2
      )
    })
    
    # 5 - Taxa de Abandono no Ensino Fundamental por Esfera Administrativa----
    ## Gráfico de barras - Taxa de Abandono no Ensino Fundamental----
    t51 <- reactive({
      req(input$edu5municomp1)
      if (input$edu5municomp1 == "Selecione um município") {
        paste0(
          "Taxa de Abandono no Ensino Fundamental por Esfera Administrativa, ",
          input$edu5muni,
          " - ",
          input$edu5ano1
        )
      } else {
        paste0(
          "Taxa de Abandono no Ensino Fundamental por Esfera Administrativa, ",
          input$edu5muni,
          " x ",
          input$edu5municomp1,
          " - ",
          input$edu5ano1
        )
      }
    })
    ## Gráfico de linha- Taxa de Abandono no Ensino Fundamental----
    t52 <- reactive({
      req(input$edu5municomp2)
      if (input$edu5municomp2 == "Selecione um município") {
        paste0(
          "Taxa de Abandono Total no Ensino Fundamental, ",
          input$edu5muni,
          " - ",
          min(edu5$ano),
          " a ",
          max(edu5$ano)
        )
      } else {
        paste0(
          "Taxa de Abandono Total no Ensino Fundamental, ",
          input$edu5muni,
          " x ",
          input$edu5municomp2,
          " - ",
          min(edu5$ano),
          " a ",
          max(edu5$ano)
        )
      }
    })
    ## Tabela - Taxa de Abandono no Ensino Fundamental----
    t53 <- reactive({
      paste0(
        "Taxa de Abandono no Ensino Fundamental por Esfera Administrativa, ",
        input$edu5muni,
        " - ",
        min(edu5$ano),
        " a ",
        max(edu5$ano)
      )
    })
    ## Tabela - Taxa de Abandono no Ensino Fundamental----
    t54 <- reactive({
      ri <- edu5 %>%
        filter(ano == input$edu5ano2, localidade == input$edu5muni) %>%
        pull(ri)
      paste0(
        "Taxa de Abandono no Ensino Fundamental por Esfera Administrativa  e Município, Região de Integração ",
        unique(ri),
        " - ",
        input$edu5ano2
      )
    })
    # 6 - Taxa de Abandono no Ensino Médio por Esfera Administrativa----
    ## Gráfico de barras - Taxa de Abandono no Ensino Médio----
    t61 <- reactive({
      req(input$edu6municomp1)
      if (input$edu6municomp1 == "Selecione um município") {
        paste0(
          "Taxa de Abandono no Ensino Médio por Esfera Administrativa, ",
          input$edu6muni,
          " - ",
          input$edu6ano1
        )
      } else {
        paste0(
          "Taxa de Abandono no Ensino Médio por Esfera Administrativa, ",
          input$edu6muni,
          " x ",
          input$edu6municomp1,
          " - ",
          input$edu6ano1
        )
      }
    })
    ## Gráfico de linha - Taxa de Abandono no Ensino Médio----
    t62 <- reactive({
      req(input$edu6municomp2)
      if (input$edu6municomp2 == "Selecione um município") {
        paste0(
          "Taxa de Abandono Total no Ensino Médio, ",
          input$edu6muni,
          " - ",
          min(edu6$ano),
          " a ",
          max(edu6$ano)
        )
      } else {
        paste0(
          "Taxa de Abandono Total no Ensino Médio, ",
          input$edu6muni,
          " x ",
          input$edu6municomp2,
          " - ",
          min(edu6$ano),
          " a ",
          max(edu6$ano)
        )
      }
    })
    ## Tabela - Taxa de Abandono no Ensino Médio----
    t63 <- reactive({
      paste0(
        "Taxa de Abandono no Ensino Médio por Esfera Administrativa, ",
        input$edu6muni,
        " - ",
        min(edu6$ano),
        " a ",
        max(edu6$ano)
      )
    })
    ## Tabela - Taxa de Abandono no Ensino Médio----
    t64 <- reactive({
      ri <- edu6 %>%
        filter(ano == input$edu6ano2, localidade == input$edu6muni) %>%
        pull(ri)
      paste0(
        "Taxa de Abandono no Ensino Médio por Esfera Administrativa  e Município, Região de Integração ",
        unique(ri),
        " - ",
        input$edu6ano2
      )
    })
    # 7 - Distorção Idade-Série Total por Nível de Ensino----
    ## Gráfico de barras - Distorção Idade-Série Total por Nível de Ensino----
    t71 <- reactive({
      req(input$edu7municomp1)
      if (input$edu7municomp1 == "Selecione um município") {
        paste0(
          "Distorção Idade-Série Total por Nível de Ensino, ",
          input$edu7muni,
          " - ",
          input$edu7ano1
        )
      } else {
        paste0(
          "Distorção Idade-Série Total por Nível de Ensino, ",
          input$edu7muni,
          " x ",
          input$edu7municomp1,
          " - ",
          input$edu7ano1
        )
      }
    })
    ## Tabela - Distorção Idade-Série Total por Nível de Ensino----
    t72 <- reactive({
      paste0(
        "Distorção Idade-Série Total por Nível de Ensino, ",
        input$edu7muni,
        " - ",
        min(edu7$ano),
        " a ",
        max(edu7$ano)
      )
    })
    ## Tabela - Distorção Idade-Série Total por Nível de Ensino----
    t73 <- reactive({
      ri <- edu7 %>%
        filter(ano == input$edu7ano2, localidade == input$edu7muni) %>%
        pull(ri)
      paste0(
        "Distorção Idade-Série Total por Nível de Ensino  e Município, Região de Integração ",
        unique(ri),
        " - ",
        input$edu7ano2
      )
    })
    # 8 -  IDEB - Escola pública - 5ª ano (séries iniciais)----
    ## Gráfico de linha - IDEB - Escola pública - 5ª ano (séries iniciais)----
    t81 <- reactive({
      req(input$edu8municomp1)
      if (input$edu8municomp1 == "Selecione um município") {
        paste0(
          "Índice de desenvonvimento da educação básica - IDEB - Escola pública - 5ª ano (séries iniciais), ",
          input$edu8muni,
          " - ",
          min(edu8$ano),
          " a ",
          max(edu8$ano)
        )
      } else {
        paste0(
          "Índice de desenvonvimento da educação básica - IDEB - Escola pública - 5ª ano (séries iniciais), ",
          input$edu8muni,
          " x ",
          input$edu8municomp1,
          " - ",
          min(edu8$ano),
          " a ",
          max(edu8$ano)
        )
      }
    })
    ## Tabela - IDEB - Escola pública - 5ª ano (séries iniciais)----
    t82 <- reactive({
      ri <- edu8 %>%
        filter(ano == input$edu8ano, localidade == input$edu8muni) %>%
        pull(ri)
      paste0(
        "Índice de desenvonvimento da educação básica - IDEB - Escola pública - 5ª ano (séries iniciais)  e Município, Região de Integração ",
        unique(ri),
        " - ",
        input$edu8ano
      )
    })
    # 9 -  IDEB - Escola pública - 9ª ano (séries finais)----
    ## Gráfico de linha - IDEB - Escola pública - 9ª ano (séries finais)----
    t91 <- reactive({
      req(input$edu9municomp1)
      if (input$edu9municomp1 == "Selecione um município") {
        paste0(
          "Índice de desenvonvimento da educação básica - IDEB - Escola pública - 9ª ano (séries finais), ",
          input$edu9muni,
          " - ",
          min(edu9$ano),
          " a ",
          max(edu9$ano)
        )
      } else {
        paste0(
          "Índice de desenvonvimento da educação básica - IDEB - Escola pública - 9ª ano (séries finais), ",
          input$edu9muni,
          " x ",
          input$edu9municomp1,
          " - ",
          min(edu9$ano),
          " a ",
          max(edu9$ano)
        )
      }
    })
    ## Tabela - IDEB - Escola pública - 9ª ano (séries finais) da mesma Região de Integração----
    t92 <- reactive({
      ri <- edu9 %>%
        filter(ano == input$edu9ano, localidade == input$edu9muni) %>%
        pull(ri)
      paste0(
        "Índice de desenvonvimento da educação básica - IDEB - Escola pública - 9ª ano (séries finais)  e Município, Região de Integração ",
        unique(ri),
        " - ",
        input$edu9ano
      )
    })
    # 10 - Número de Matrículas no Ensino Pré-Escolar por Esfera Administrativa----
    ## Gráfico de barras- Número de Matrículas no Ensino Pré-Escolar----
    t101 <- reactive({
      req(input$edu10municomp1)
      if (input$edu10municomp1 == "Selecione um município") {
        paste0(
          "Número de Matrículas no Ensino Pré-Escolar por Esfera Administrativa, ",
          input$edu10muni,
          " - ",
          input$edu10ano1
        )
      } else {
        paste0(
          "Número de Matrículas no Ensino Pré-Escolar por Esfera Administrativa, ",
          input$edu10muni,
          " x ",
          input$edu10municomp1,
          " - ",
          input$edu10ano1
        )
      }
    })
    ## Gráfico de linha- Número de Matrículas no Ensino Pré-Escolar----
    t102 <- reactive({
      req(input$edu10municomp2)
      if (input$edu10municomp2 == "Selecione um município") {
        paste0(
          "Número Total de Matrículas no Ensino Pré-Escolar, ",
          input$edu10muni,
          " - ",
          min(edu10$ano),
          " a ",
          max(edu10$ano)
        )
      } else {
        paste0(
          "Número Total de Matrículas no Ensino Pré-Escolar, ",
          input$edu10muni,
          " x ",
          input$edu10municomp2,
          " - ",
          min(edu10$ano),
          " a ",
          max(edu10$ano)
        )
      }
    })
    ## Tabela - Número de Matrículas no Ensino Pré-Escolar----
    t103 <- reactive({
      paste0(
        "Número de Matrículas no Ensino Pré-Escolar por Esfera Administrativa, ",
        input$edu10muni,
        " - ",
        min(edu10$ano),
        " a ",
        max(edu10$ano)
      )
    })
    ## Tabela - Número de Matrículas no Ensino Pré-Escolar----
    t104 <- reactive({
      ri <- edu10 %>%
        filter(ano == input$edu10ano2,
               localidade == input$edu10muni) %>%
        pull(ri)
      paste0(
        "Número de Matrículas no Ensino Pré-Escolar  e Município, Região de Integração ",
        unique(ri),
        " - ",
        input$edu10ano2
      )
    })
    # 11 - Número de Matrículas no Ensino Fundamental por Esfera Administrativa----
    ## Gráfico de barras- Número de Matrículas no Ensino Fundamental----
    t111 <- reactive({
      req(input$edu11municomp1)
      if (input$edu11municomp1 == "Selecione um município") {
        paste0(
          "Número de Matrículas no Ensino Fundamental por Esfera Administrativa, ",
          input$edu11muni,
          " - ",
          input$edu11ano1
        )
      } else {
        paste0(
          "Número de Matrículas no Ensino Fundamental por Esfera Administrativa, ",
          input$edu11muni,
          " x ",
          input$edu11municomp1,
          " - ",
          input$edu11ano1
        )
      }
    })
    ## Gráfico de linha- Número de Matrículas no Ensino Fundamental----
    t112 <- reactive({
      req(input$edu11municomp2)
      if (input$edu11municomp2 == "Selecione um município") {
        paste0(
          "Número Total de Matrículas no Ensino Fundamental, ",
          input$edu11muni,
          " - ",
          min(edu11$ano),
          " a ",
          max(edu11$ano)
        )
      } else {
        paste0(
          "Número Total de Matrículas no Ensino Fundamental, ",
          input$edu11muni,
          " x ",
          input$edu11municomp2,
          " - ",
          min(edu11$ano),
          " a ",
          max(edu11$ano)
        )
      }
    })
    ## Tabela - Número de Matrículas no Ensino Fundamental----
    t113 <- reactive({
      paste0(
        "Número de Matrículas no Ensino Fundamental por Esfera Administrativa, ",
        input$edu11muni,
        " - ",
        min(edu11$ano),
        " a ",
        max(edu11$ano)
      )
    })
    ## Tabela - Número de Matrículas no Ensino Fundamental----
    t114 <- reactive({
      ri <- edu11 %>%
        filter(ano == input$edu11ano2,
               localidade == input$edu11muni) %>%
        pull(ri)
      paste0(
        "Número de Matrículas no Ensino Fundamental  e Município, Região de Integração ",
        unique(ri),
        " - ",
        input$edu11ano2
      )
    })
    # 12 - Número de Matrículas no Ensino Médio por Esfera Administrativa----
    ## Gráfico de barras - Número de Matrículas no Ensino Médio----
    t121 <- reactive({
      req(input$edu12municomp1)
      if (input$edu12municomp1 == "Selecione um município") {
        paste0(
          "Número de Matrículas no Ensino Médio no Ensino Fundamental, ",
          input$edu12muni,
          " - ",
          input$edu12ano1
        )
      } else {
        paste0(
          "Número de Matrículas no Ensino Médio no Ensino Fundamental, ",
          input$edu12muni,
          " x ",
          input$edu12municomp1,
          " - ",
          input$edu12ano1
        )
      }
    })
    ## Gráfico de linha - Número de Matrículas no Ensino Médio----
    t122 <- reactive({
      req(input$edu12municomp2)
      if (input$edu12municomp2 == "Selecione um município") {
        paste0(
          "Número Total de Matrículas no Ensino Médio, ",
          input$edu12muni,
          " - ",
          min(edu12$ano),
          " a ",
          max(edu12$ano)
        )
      } else {
        paste0(
          "Número Total de Matrículas no Ensino Médio, ",
          input$edu12muni,
          " x ",
          input$edu12municomp2,
          " - ",
          min(edu12$ano),
          " a ",
          max(edu12$ano)
        )
      }
    })
    ## Tabela - Número de Matrículas no Ensino Médio----
    t123 <- reactive({
      paste0(
        "Número de Matrículas no Ensino Médio por Esfera Administrativa, ",
        input$edu12muni,
        " - ",
        min(edu12$ano),
        " a ",
        max(edu12$ano)
      )
    })
    ## Tabela - Número de Matrículas no Ensino Médio----
    t124 <- reactive({
      ri <- edu12 %>%
        filter(ano == input$edu12ano2,
               localidade == input$edu12muni) %>%
        pull(ri)
      paste0(
        "Número de Matrículas no Ensino Médio  e Município, Região de Integração ",
        unique(ri),
        " - ",
        input$edu12ano2
      )
    })
    # 13 - Média de Alunos por Turma por Nível de Ensino----
    ## Gráfico de barras - Média de Alunos por Turma por Nível de Ensino----
    t131 <- reactive({
      req(input$edu13municomp1)
      if (input$edu13municomp1 == "Selecione um município") {
        paste0(
          "Média de Alunos por Turma por Nível de Ensino, ",
          input$edu13muni,
          " - ",
          input$edu13ano1
        )
      } else {
        paste0(
          "Média de Alunos por Turma por Nível de Ensino, ",
          input$edu13muni,
          " x ",
          input$edu13municomp1,
          " - ",
          input$edu13ano1
        )
      }
    })
    ## Tabela - Média de Alunos por Turma por Nível de Ensino----
    t132 <- reactive({
      paste0(
        "Média de Alunos por Turma por Nível de Ensino, ",
        input$edu13muni,
        " - ",
        min(edu13$ano),
        " a ",
        max(edu13$ano)
      )
    })
    ## Tabela - Média de Alunos por Turma por Nível de Ensino----
    t133 <- reactive({
      ri <- edu13 %>%
        filter(ano == input$edu13ano2,
               localidade == input$edu13muni) %>%
        pull(ri)
      paste0(
        "Média de Alunos por Turma por Nível de Ensino  e Município, Região de Integração ",
        unique(ri),
        " - ",
        input$edu13ano2
      )
    })
    # 14 - Número de Docentes no Ensino Pré-escolar por Esfera Administrativa----
    ## Gráfico de barras - Número de Docentes no Ensino Pré-escolar----
    t141 <- reactive({
      req(input$edu14municomp1)
      if (input$edu14municomp1 == "Selecione um município") {
        paste0(
          "Número de Docentes no Ensino Pré-escolar por Esfera Administrativa, ",
          input$edu14muni,
          " - ",
          input$edu14ano1
        )
      } else {
        paste0(
          "Número de Docentes no Ensino Pré-escolar por Esfera Administrativa, ",
          input$edu14muni,
          " x ",
          input$edu14municomp1,
          " - ",
          input$edu14ano1
        )
      }
    })
    ## Gráfico de linhas - Número de Docentes no Ensino Pré-escolar----
    t142 <- reactive({
      req(input$edu14municomp2)
      if (input$edu14municomp2 == "Selecione um município") {
        paste0(
          "Número Total de Docentes no Ensino Pré-escolar, ",
          input$edu14muni,
          " - ",
          min(edu14$ano),
          " a ",
          max(edu14$ano)
        )
      } else {
        paste0(
          "Número Total de Docentes no Ensino Pré-escolar, ",
          input$edu14muni,
          " x ",
          input$edu14municomp2,
          " - ",
          min(edu14$ano),
          " a ",
          max(edu14$ano)
        )
      }
    })
    ## Tabela - Número de Docentes no Ensino Pré-escolar----
    t143 <- reactive({
      paste0(
        "Número de Docentes no Ensino Pré-escolar, ",
        input$edu14muni,
        " - ",
        min(edu14$ano),
        " a ",
        max(edu14$ano)
      )
    })
    ## Tabela - Número de Docentes no Ensino Pré-escolar----
    t144 <- reactive({
      ri <- edu14 %>%
        filter(ano == input$edu14ano2,
               localidade == input$edu14muni) %>%
        pull(ri)
      paste0(
        "Número de Docentes no Ensino Pré-escolar  e Município, Região de Integração ",
        unique(ri),
        " - ",
        input$edu14ano2
      )
    })
    # 15 - Número de Docentes no Ensino Fundamental por Esfera Administrativa----
    ## Gráfico de barras - Número de Docentes no Ensino Fundamental----
    t151 <- reactive({
      req(input$edu15municomp1)
      if (input$edu15municomp1 == "Selecione um município") {
        paste0(
          "Número de Docentes no Ensino Fundamental por Esfera Administrativa, ",
          input$edu15muni,
          " - ",
          input$edu15ano1
        )
      } else {
        paste0(
          "Número de Docentes no Ensino Fundamental por Esfera Administrativa, ",
          input$edu15muni,
          " x ",
          input$edu15municomp1,
          " - ",
          input$edu15ano1
        )
      }
    })
    ## Gráfico de linhas - Número de Docentes no Ensino Fundamental----
    t152 <- reactive({
      req(input$edu15municomp2)
      if (input$edu15municomp2 == "Selecione um município") {
        paste0(
          "Número Total de Docentes no Ensino Fundamental, ",
          input$edu15muni,
          " - ",
          min(edu15$ano),
          " a ",
          max(edu15$ano)
        )
      } else {
        paste0(
          "Número Total de Docentes no Ensino Fundamental, ",
          input$edu15muni,
          " x ",
          input$edu15municomp2,
          " - ",
          min(edu15$ano),
          " a ",
          max(edu15$ano)
        )
      }
    })
    ## Tabela - Número de Docentes no Ensino Fundamental----
    t153 <- reactive({
      paste0(
        "Número de Docentes no Ensino Fundamental por Esfera Administrativa, ",
        input$edu15muni,
        " - ",
        min(edu15$ano),
        " a ",
        max(edu15$ano)
      )
    })
    ## Tabela - Número de Docentes no Ensino Fundamental----
    t154 <- reactive({
      ri <- edu15 %>%
        filter(ano == input$edu15ano2,
               localidade == input$edu15muni) %>%
        pull(ri)
      paste0(
        "Número de Docentes no Ensino Fundamental por Esfera Administrativa  e Município, Região de Integração ",
        unique(ri),
        " - ",
        input$edu15ano2
      )
    })
    # 16 - Número de Docentes no Ensino Médio por Esfera Administrativa----
    ## Gráfico de barras- Número de Docentes no Ensino Médio----
    t161 <- reactive({
      req(input$edu16municomp1)
      if (input$edu16municomp1 == "Selecione um município") {
        paste0(
          "Número de Docentes no Ensino Médio por Esfera Administrativa, ",
          input$edu16muni,
          " - ",
          input$edu16ano1
        )
      } else {
        paste0(
          "Número de Docentes no Ensino Médio por Esfera Administrativa nos municípios de",
          input$edu16muni,
          " x ",
          input$edu16municomp1,
          " - ",
          input$edu16ano1
        )
      }
    })
    ## Gráfico de linha - Número de Docentes no Ensino Médio----
    t162 <- reactive({
      req(input$edu16municomp2)
      if (input$edu16municomp2 == "Selecione um município") {
        paste0(
          "Número Total de Docentes no Ensino Médio, ",
          input$edu16muni,
          " - ",
          min(edu16$ano),
          " a ",
          max(edu16$ano)
        )
      } else {
        paste0(
          "Número Total de Docentes no Ensino Médio, ",
          input$edu16muni,
          " x ",
          input$edu16municomp2,
          " - ",
          min(edu16$ano),
          " a ",
          max(edu16$ano)
        )
      }
    })
    ## Tabela - Número de Docentes no Ensino Médio----
    t163 <- reactive({
      paste0(
        "Número de Docentes no Ensino Médio por Esfera Administrativa, ",
        input$edu16muni,
        " - ",
        min(edu16$ano),
        " a ",
        max(edu16$ano)
      )
    })
    ## Tabela - Número de Docentes no Ensino Médio----
    t164 <- reactive({
      ri <- edu16 %>%
        filter(ano == input$edu16ano2,
               localidade == input$edu16muni) %>%
        pull(ri)
      paste0(
        "Número de Docentes no Ensino Médio por Esfera Administrativa  e Município, Região de Integração ",
        unique(ri),
        " - ",
        input$edu16ano2
      )
    })
    # 17 - Estabelecimentos de Pré-Escola por Esfera Administrativa----
    ## Gráfico de barras- Estabelecimentos de Pré-Escola----
    t171 <- reactive({
      req(input$edu17municomp1)
      if (input$edu17municomp1 == "Selecione um município") {
        paste0(
          "Quantidade de Estabelecimentos de Pré-Escola por Esfera Administrativa, ",
          input$edu17muni,
          " - ",
          input$edu17ano1
        )
      } else {
        paste0(
          "Quantidade de Estabelecimentos de Pré-Escola por Esfera Administrativa, ",
          input$edu17muni,
          " x ",
          input$edu17municomp1,
          " - ",
          input$edu17ano1
        )
      }
    })
    ## Gráfico de linha- Estabelecimentos de Pré-Escola----
    t172 <- reactive({
      req(input$edu17municomp2)
      if (input$edu17municomp2 == "Selecione um município") {
        paste0(
          "Quantidade Total de Estabelecimentos de Pré-Escola, ",
          input$edu17muni,
          " - ",
          min(edu17$ano),
          " a ",
          max(edu17$ano)
        )
      } else {
        paste0(
          "Quantidade Total de Estabelecimentos de Pré-Escola, ",
          input$edu17muni,
          " x ",
          input$edu17municomp2,
          " - ",
          min(edu17$ano),
          " a ",
          max(edu17$ano)
        )
      }
    })
    ## Tabela - Estabelecimentos de Pré-Escola----
    t173 <- reactive({
      paste0(
        "Quantidade de Estabelecimentos de Pré-Escola, ",
        input$edu17muni,
        " - ",
        min(edu17$ano),
        " a ",
        max(edu17$ano)
      )
    })
    ## Tabela - Estabelecimentos de Pré-Escola----
    t174 <- reactive({
      ri <- edu17 %>%
        filter(ano == input$edu17ano2,
               localidade == input$edu17muni) %>%
        pull(ri)
      paste0(
        "Quantidade de Estabelecimentos de Pré-Escola por Esfera Administrativa  e Município, Região de Integração ",
        unique(ri),
        " - ",
        input$edu17ano2
      )
    })
    # 18 - Estabelecimentos de Ensino Fundamental por Esfera Administrativa----
    ## Gráfico de barras - Estabelecimentos de Ensino Fundamental----
    t181 <- reactive({
      req(input$edu18municomp1)
      if (input$edu18municomp1 == "Selecione um município") {
        paste0(
          "Quantidade de Estabelecimentos de Ensino Fundamental por Esfera Administrativa, ",
          input$edu18muni,
          " - ",
          input$edu18ano1
        )
      } else {
        paste0(
          "Quantidade de Estabelecimentos de Ensino Fundamental por Esfera Administrativa, ",
          input$edu18muni,
          " x ",
          input$edu18municomp1,
          " - ",
          input$edu18ano1
        )
      }
    })
    ## Gráfico de linha - Estabelecimentos de Ensino Fundamental----
    t182 <- reactive({
      req(input$edu18municomp2)
      if (input$edu18municomp2 == "Selecione um município") {
        paste0(
          "Quantidade Total de Estabelecimentos de Ensino Fundamental, ",
          input$edu18muni,
          " - ",
          min(edu18$ano),
          " a ",
          max(edu18$ano)
        )
      } else {
        paste0(
          "Quantidade Total de Estabelecimentos de Ensino Fundamental, ",
          input$edu18muni,
          " x ",
          input$edu18municomp2,
          " - ",
          min(edu18$ano),
          " a ",
          max(edu18$ano)
        )
      }
    })
    ## Tabela - Estabelecimentos de Ensino Fundamental----
    t183 <- reactive({
      paste0(
        "Quantidade de Estabelecimentos de Ensino Fundamental por Esfera Administrativa, ",
        input$edu18muni,
        " - ",
        min(edu18$ano),
        " a ",
        max(edu18$ano)
      )
    })
    ## Tabela - Estabelecimentos de Ensino Fundamental----
    t184 <- reactive({
      ri <- edu18 %>%
        filter(ano == input$edu18ano2,
               localidade == input$edu18muni) %>%
        pull(ri)
      paste0(
        "Quantidade de Estabelecimentos de Pré-Escola por Esfera Administrativa  e Município, Região de Integração ",
        unique(ri),
        " - ",
        input$edu18ano2
      )
    })
    # 19 - Estabelecimentos de Ensino Médio por Esfera Administrativa----
    ## Gráfico de barras- Estabelecimentos de Ensino Médio----
    t191 <- reactive({
      req(input$edu19municomp1)
      if (input$edu19municomp1 == "Selecione um município") {
        paste0(
          "Quantidade de Estabelecimentos de Ensino Médio por Esfera Administrativa, ",
          input$edu19muni,
          " - ",
          input$edu19ano1
        )
      } else {
        paste0(
          "Quantidade de Estabelecimentos de Ensino Médio por Esfera Administrativa, ",
          input$edu19muni,
          " x ",
          input$edu19municomp1,
          " - ",
          input$edu19ano1
        )
      }
    })
    ## Gráfico de linha- Estabelecimentos de Ensino Médio----
    t192 <- reactive({
      req(input$edu19municomp2)
      if (input$edu19municomp2 == "Selecione um município") {
        paste0(
          "Quantidade Total de Estabelecimentos de Ensino Médio, ",
          input$edu19muni,
          " - ",
          min(edu19$ano),
          " a ",
          max(edu19$ano)
        )
      } else {
        paste0(
          "Quantidade Total de Estabelecimentos de Ensino Médio, ",
          input$edu19muni,
          " x ",
          input$edu19municomp2,
          " - ",
          min(edu19$ano),
          " a ",
          max(edu19$ano)
        )
      }
    })
    ## Tabela - Estabelecimentos de Ensino Médio----
    t193 <- reactive({
      paste0(
        "Quantidade de Estabelecimentos de Ensino Médio por Esfera Administrativa, ",
        input$edu19muni,
        " - ",
        min(edu19$ano),
        " a ",
        max(edu19$ano)
      )
    })
    ## Tabela - Estabelecimentos de Ensino Médio----
    t194 <- reactive({
      ri <- edu19 %>%
        filter(ano == input$edu19ano2,
               localidade == input$edu19muni) %>%
        pull(ri)
      paste0(
        "Quantidade de Estabelecimentos de Ensino Médio por Esfera Administrativa  e Município, Região de Integração ",
        unique(ri),
        " - ",
        input$edu19ano2
      )
    })
    #VISUALIZAÇÃO----
    # 1 - Taxa de Aprovação no Ensino Fundamental por Esfera Administrativa----
    ## Gráfico de barras - Taxa de Aprovação no Ensino Fundamental----
    # Atualização da entrada
    edu1comp1 <- reactive({
      input$edu1muni
    })
    observeEvent(edu1comp1(), {
      x <- edu1 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != edu1comp1())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "edu1municomp1",
        choices = c("Selecione um município", choices),
        session
      )
    })
    
    ## Título
    output$edu1txt1 <- renderText({
      t11()
    })
    
    output$edu1graf <- renderEcharts4r({
      req(input$edu1municomp1)
      if (input$edu1municomp1 == "Selecione um município") {
        a <- edu1 %>% filter(
          localidade == input$edu1muni,
          ano == input$edu1ano1,
          categoria != "Aprovação Total"
        )
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
            barWidth = "30%",
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal",
                                                    digits = 1, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Esfera Administrativa",
            nameLocation = "middle",
            nameTextStyle = list(
              fontWeight = "bold",
              padding = c(20, 0, 0, 0),
              fontSize = 14
            )
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
              { minimumFractionDigits: 0, maximumFractionDigits: 0 });
              }
            "
              )
            )
          ) %>%
          e_locale("pt-Br") %>%
          e_grid(show = T)
      } else {
        a <- edu1 %>% filter(
          localidade == input$edu1muni,
          ano == input$edu1ano1,
          categoria != "Aprovação Total"
        )
        b <-
          edu1 %>% filter(
            localidade == input$edu1municomp1,
            ano == input$edu1ano1,
            categoria != "Aprovação Total"
          )
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            name = input$edu1muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, categoria) %>%
          e_bar(
            serie = valor,
            name = input$edu1municomp1,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          # Barra
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal",
                                                    digits = 1, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Esfera Administrativa",
            nameLocation = "middle",
            nameTextStyle = list(
              fontWeight = "bold",
              padding = c(20, 0, 0, 0),
              fontSize = 14
            )
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
    ## Gráfico de linha - Taxa de Aprovação no Ensino Fundamental----
    # Atualização da entrada
    edu1comp2 <- reactive({
      input$edu1muni
    })
    observeEvent(edu1comp2(), {
      x <- edu1 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != edu1comp2())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "edu1municomp2",
        choices = c("Selecione um município", choices),
        session
      )
    })
    ## Título
    output$edu1txt2 <- renderText({
      t12()
    })
    output$edu1graf2 <- renderEcharts4r({
      req(input$edu1municomp2)
      if (input$edu1municomp2 == "Selecione um município") {
        a <- edu1 %>% filter(localidade == input$edu1muni,
                             categoria == "Total")
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
            formatter = e_tooltip_pointer_formatter("decimal",
                                                    digits = 1, locale = "pt-Br"),
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
          e_datazoom(toolbox = F, fillerColor = "#E5F5F9") %>%
          e_grid(show = T)
      } else {
        a <- edu1 %>%
          filter(localidade == input$edu1muni,
                 categoria == "Total")
        b <- edu1 %>%
          filter(localidade == input$edu1municomp2,
                 categoria == "Total")
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$edu1muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$edu1municomp2,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal",
                                                    digits = 1, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Esfera Administrativa",
            nameLocation = "middle",
            nameTextStyle = list(
              fontWeight = "bold",
              padding = c(20, 0, 0, 0),
              fontSize = 14
            )
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
    ## Tabela - Taxa de Aprovação no Ensino Fundamental----
    output$edu1txt3 <- renderText({
      t13()
    })
    output$edu1tab <- renderReactable({
      x <- edu1 %>%
        filter(localidade == input$edu1muni) %>%
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
          ano = colDef(name = "Ano")
        ),
        defaultColDef =
          colDef(
            footerStyle = list(fontWeight = "bold"),
            format = colFormat(separators = T),
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
    ## Tabela - Taxa de Aprovação no Ensino Fundamental----
    output$edu1txt4 <- renderText({
      t14()
    })
    output$edu1tab1 <- renderReactable({
      ri <- edu1 %>%
        filter(ano == input$edu1ano2, localidade == input$edu1muni) %>%
        pull(ri)
      x <-
        edu1 %>% filter(ano == input$edu1ano2, localidade != "Pará")
      x <- x %>%
        filter(ri == ri) %>%
        select(ri,
               localidade,
               categoria,
               valor) %>%
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
          localidade = colDef(name = "Municípios")),
        defaultColDef =
          colDef(
            footerStyle = list(fontWeight = "bold"),
            format = colFormat(separators = T),
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
    
    # 2 - Taxa de Aprovação no Ensino Médio por Esfera Administrativa----
    ## Gráfico de barra- Taxa de Aprovação no Ensino Médio----
    # Atualização da entrada
    edu2comp1 <- reactive({
      input$edu2muni
    })
    observeEvent(edu2comp1(), {
      x <- edu2 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != edu2comp1())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "edu2municomp1",
        choices = c("Selecione um município", choices),
        session
      )
    })
    
    ## Título
    output$edu2txt1 <- renderText({
      t21()
    })
    
    output$edu2graf <- renderEcharts4r({
      req(input$edu2municomp1)
      if (input$edu2municomp1 == "Selecione um município") {
        a <- edu2 %>% filter(
          localidade == input$edu2muni,
          ano == input$edu2ano1,
          categoria != "Aprovação Total"
        )
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
            barWidth = "30%",
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal",
                                                    digits = 1, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Esfera Administrativa",
            nameLocation = "middle",
            nameTextStyle = list(
              fontWeight = "bold",
              padding = c(20, 0, 0, 0),
              fontSize = 14
            )
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
              { minimumFractionDigits: 0, maximumFractionDigits: 0 });
              }
            "
              )
            )
          ) %>%
          e_locale("pt-Br") %>%
          e_grid(show = T)
      } else {
        a <- edu2 %>% filter(
          localidade == input$edu2muni,
          ano == input$edu2ano1,
          categoria != "Aprovação Total"
        )
        b <-
          edu2 %>% filter(
            localidade == input$edu2municomp1,
            ano == input$edu2ano1,
            categoria != "Aprovação Total"
          )
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            name = input$edu2muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, categoria) %>%
          e_bar(
            serie = valor,
            name = input$edu2municomp1,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 1, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Esfera Administrativa",
            nameLocation = "middle",
            nameTextStyle = list(
              fontWeight = "bold",
              padding = c(20, 0, 0, 0),
              fontSize = 14
            )
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
    ## Gráfico de linha - Taxa de Aprovação no Ensino Médio----
    # Atualização da entrada
    edu2comp2 <- reactive({
      input$edu2muni
    })
    observeEvent(edu2comp2(), {
      x <- edu2 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != edu2comp2())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "edu2municomp2",
        choices = c("Selecione um município", choices),
        session
      )
    })
    ## Título
    output$edu2txt2 <- renderText({
      t22()
    })
    output$edu2graf2 <- renderEcharts4r({
      req(input$edu2municomp2)
      if (input$edu2municomp2 == "Selecione um município") {
        a <- edu2 %>% filter(localidade == input$edu2muni,
                             categoria == "Aprovação Total")
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
            formatter = e_tooltip_pointer_formatter("decimal", digits = 1, locale = "pt-Br"),
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
      } else {
        a <- edu2 %>% filter(localidade == input$edu2muni,
                             categoria == "Aprovação Total")
        b <-
          edu2 %>% filter(localidade == input$edu2municomp2,
                          categoria == "Aprovação Total")
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$edu2muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$edu2municomp2,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 1, locale = "pt-Br"),
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
      }
    })
    ## Tabela - Taxa de Aprovação no Ensino Médio----
    output$edu2txt3 <- renderText({
      t23()
    })
    output$edu2tab <- renderReactable({
      x <- edu2 %>%
        filter(localidade == input$edu2muni) %>%
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
          ano = colDef(name = "Ano")
        ),
        defaultColDef =
          colDef(
            footerStyle = list(fontWeight = "bold"),
            format = colFormat(separators = T),
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
    
    ## Tabela - Taxa de Aprovação no Ensino Médio----
    output$edu2txt4 <- renderText({
      t24()
    })
    output$edu2tab1 <- renderReactable({
      ri <- edu2 %>%
        filter(ano == input$edu2ano2, localidade == input$edu2muni) %>%
        pull(ri)
      x <-
        edu2 %>% filter(ano == input$edu2ano2, localidade != "Pará")
      x <- x %>%
        filter(ri == ri) %>%
        select(ri,
               localidade,
               categoria,
               valor) %>%
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
          localidade = colDef(name = "Municípios")),
        defaultColDef =
          colDef(
            footerStyle = list(fontWeight = "bold"),
            format = colFormat(separators = T),
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
    
    
    # 3 - Taxa de Reprovação no Ensino Fundamental por Esfera Administrativa----
    ## Gráfico de barras - Taxa de Reprovação no Ensino Fundamental----
    # Atualização da entrada
    edu3comp1 <- reactive({
      input$edu3muni
    })
    observeEvent(edu3comp1(), {
      x <- edu3 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != edu3comp1())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "edu3municomp1",
        choices = c("Selecione um município", choices),
        session
      )
    })
    
    ## Título
    output$edu3txt1 <- renderText({
      t31()
    })
    
    output$edu3graf <- renderEcharts4r({
      req(input$edu3municomp1)
      if (input$edu3municomp1 == "Selecione um município") {
        a <- edu3 %>% filter(
          localidade == input$edu3muni,
          ano == input$edu3ano1,
          categoria != "Reprovação Total"
        )
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
            barWidth = "30%",
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 1, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Esfera Administrativa",
            nameLocation = "middle",
            nameTextStyle = list(
              fontWeight = "bold",
              padding = c(20, 0, 0, 0),
              fontSize = 14
            )
          ) %>%
          e_y_axis(
            name = "Taxa",
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
          e_grid(show = T)
      } else {
        a <- edu3 %>% filter(
          localidade == input$edu3muni,
          ano == input$edu3ano1,
          categoria != "Reprovação Total"
        )
        b <-
          edu3 %>% filter(
            localidade == input$edu3municomp1,
            ano == input$edu3ano1,
            categoria != "Reprovação Total"
          )
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            name = input$edu3muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, categoria) %>%
          e_bar(
            serie = valor,
            name = input$edu3municomp1,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 1, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Esfera Administrativa",
            nameLocation = "middle",
            nameTextStyle = list(
              fontWeight = "bold",
              padding = c(20, 0, 0, 0),
              fontSize = 14
            )
          ) %>%
          e_y_axis(
            name = "Taxa",
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
          e_grid(show = T)
      }
    })
    ## Gráfico de linha - Taxa de Reprovação no Ensino Fundamental----
    # Atualização da entrada
    edu3comp2 <- reactive({
      input$edu3muni
    })
    observeEvent(edu3comp2(), {
      x <- edu3 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != edu3comp2())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "edu3municomp2",
        choices = c("Selecione um município", choices),
        session
      )
    })
    ## Título
    output$edu3txt2 <- renderText({
      t33()
    })
    output$edu3graf2 <- renderEcharts4r({
      req(input$edu3municomp2)
      if (input$edu3municomp2 == "Selecione um município") {
        a <- edu3 %>% filter(localidade == input$edu3muni,
                             categoria == "Reprovação Total")
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
            formatter = e_tooltip_pointer_formatter("decimal", digits = 1, locale = "pt-Br"),
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
      } else {
        a <- edu3 %>% filter(localidade == input$edu3muni,
                             categoria == "Reprovação Total")
        b <-
          edu3 %>% filter(localidade == input$edu3municomp2,
                          categoria == "Reprovação Total")
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$edu3muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$edu3municomp2,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 1, locale = "pt-Br"),
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
      }
    })
    ## Tabela - Taxa de Reprovação no Ensino Fundamental----
    output$edu3txt3 <- renderText({
      t33()
    })
    output$edu3tab <- renderReactable({
      x <- edu3 %>%
        filter(localidade == input$edu3muni) %>%
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
          ano = colDef(name = "Ano")
        ),
        defaultColDef =
          colDef(
            footerStyle = list(fontWeight = "bold"),
            format = colFormat(separators = T),
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
    ## Tabela - Taxa de Reprovação no Ensino Fundamental----
    output$edu3txt4 <- renderText({
      t34()
    })
    output$edu3tab1 <- renderReactable({
      ri <- edu3 %>%
        filter(ano == input$edu3ano2, localidade == input$edu3muni) %>%
        pull(ri)
      x <-
        edu3 %>% filter(ano == input$edu3ano2, localidade != "Pará")
      x <- x %>%
        filter(ri == ri) %>%
        select(ri,
               localidade,
               categoria,
               valor) %>%
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
          localidade = colDef(name = "Municípios")),
        defaultColDef =
          colDef(
            footerStyle = list(fontWeight = "bold"),
            format = colFormat(separators = T),
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
    # 4 - Taxa de Reprovação no Ensino Médio por Esfera Administrativa----
    ## Gráfico de barras - Taxa de Reprovação no Ensino Médio----
    # Atualização da entrada
    edu4comp1 <- reactive({
      input$edu4muni
    })
    observeEvent(edu4comp1(), {
      x <- edu4 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != edu4comp1())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "edu4municomp1",
        choices = c("Selecione um município", choices),
        session
      )
    })
    
    ## Título
    output$edu4txt1 <- renderText({
      t41()
    })
    
    output$edu4graf <- renderEcharts4r({
      req(input$edu4municomp1)
      if (input$edu4municomp1 == "Selecione um município") {
        a <- edu4 %>% filter(
          localidade == input$edu4muni,
          ano == input$edu4ano1,
          categoria != "Reprovação Total"
        )
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
            barWidth = "30%",
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 1, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Esfera Administrativa",
            nameLocation = "middle",
            nameTextStyle = list(
              fontWeight = "bold",
              padding = c(20, 0, 0, 0),
              fontSize = 14
            )
          ) %>%
          e_y_axis(
            name = "Taxa",
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
          e_grid(show = T)
      } else {
        a <- edu4 %>% filter(
          localidade == input$edu4muni,
          ano == input$edu4ano1,
          categoria != "Reprovação Total"
        )
        b <-
          edu4 %>% filter(
            localidade == input$edu4municomp1,
            ano == input$edu4ano1,
            categoria != "Reprovação Total"
          )
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            name = input$edu4muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, categoria) %>%
          e_bar(
            serie = valor,
            name = input$edu4municomp1,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 1, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Esfera Administrativa",
            nameLocation = "middle",
            nameTextStyle = list(
              fontWeight = "bold",
              padding = c(20, 0, 0, 0),
              fontSize = 14
            )
          ) %>%
          e_y_axis(
            name = "Taxa",
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
          e_grid(show = T)
      }
    })
    ## Gráfico de linha - Taxa de Reprovação no Ensino Médio----
    # Atualização da entrada
    edu4comp2 <- reactive({
      input$edu4muni
    })
    observeEvent(edu4comp2(), {
      x <- edu4 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != edu4comp2())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "edu4municomp2",
        choices = c("Selecione um município", choices),
        session
      )
    })
    ## Título
    output$edu4txt2 <- renderText({
      t42()
    })
    output$edu4graf2 <- renderEcharts4r({
      req(input$edu4municomp2)
      if (input$edu4municomp2 == "Selecione um município") {
        a <- edu4 %>% filter(localidade == input$edu4muni,
                             categoria == "Reprovação Total")
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
            formatter = e_tooltip_pointer_formatter("decimal", digits = 1, locale = "pt-Br"),
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
      } else {
        a <- edu4 %>% filter(localidade == input$edu4muni,
                             categoria == "Reprovação Total")
        b <-
          edu4 %>% filter(localidade == input$edu4municomp2,
                          categoria == "Reprovação Total")
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$edu4muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$edu4municomp2,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 1, locale = "pt-Br"),
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
      }
    })
    ## Tabela - Taxa de Reprovação no Ensino Médio----
    output$edu4txt3 <- renderText({
      t43()
    })
    output$edu4tab <- renderReactable({
      x <- edu4 %>%
        filter(localidade == input$edu4muni) %>%
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
          ano = colDef(name = "Ano")
        ),
        defaultColDef =
          colDef(
            footerStyle = list(fontWeight = "bold"),
            format = colFormat(separators = T),
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
    ## Tabela - Taxa de Reprovação no Ensino Médio----
    output$edu4txt4 <- renderText({
      t44()
    })
    output$edu4tab1 <- renderReactable({
      ri <- edu4 %>%
        filter(ano == input$edu4ano2, localidade == input$edu4muni) %>%
        pull(ri)
      x <-
        edu4 %>% filter(ano == input$edu4ano2, localidade != "Pará")
      x <- x %>%
        filter(ri == ri) %>%
        select(ri,
               localidade,
               categoria,
               valor) %>%
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
          localidade = colDef(name = "Municípios")),
        defaultColDef =
          colDef(
            footerStyle = list(fontWeight = "bold"),
            format = colFormat(separators = T),
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
    
    
    # 5 - Taxa de Abandono no Ensino Fundamental por Esfera Administrativa----
    ## Gráfico de barras - Taxa de Abandono no Ensino Fundamental----
    # Atualização da entrada
    edu5comp1 <- reactive({
      input$edu5muni
    })
    observeEvent(edu5comp1(), {
      x <- edu5 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != edu5comp1())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "edu5municomp1",
        choices = c("Selecione um município", choices),
        session
      )
    })
    
    ## Título
    output$edu5txt1 <- renderText({
      t51()
    })
    
    output$edu5graf <- renderEcharts4r({
      req(input$edu5municomp1)
      if (input$edu5municomp1 == "Selecione um município") {
        a <- edu5 %>% filter(
          localidade == input$edu5muni,
          ano == input$edu5ano1,
          categoria != "Evasão Total"
        )
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
            barWidth = "30%",
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 1, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Esfera Administrativa",
            nameLocation = "middle",
            nameTextStyle = list(
              fontWeight = "bold",
              padding = c(20, 0, 0, 0),
              fontSize = 14
            )
          ) %>%
          e_y_axis(
            name = "Taxa",
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
          e_grid(show = T)
      } else {
        a <- edu5 %>% filter(
          localidade == input$edu5muni,
          ano == input$edu5ano1,
          categoria != "Evasão Total"
        )
        b <-
          edu5 %>% filter(
            localidade == input$edu5municomp1,
            ano == input$edu5ano1,
            categoria != "Evasão Total"
          )
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            name = input$edu5muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, categoria) %>%
          e_bar(
            serie = valor,
            name = input$edu5municomp1,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 1, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Esfera Administrativa",
            nameLocation = "middle",
            nameTextStyle = list(
              fontWeight = "bold",
              padding = c(20, 0, 0, 0),
              fontSize = 14
            )
          ) %>%
          e_y_axis(
            name = "Taxa",
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
          e_grid(show = T)
      }
    })
    ## Gráfico de linha- Taxa de Abandono no Ensino Fundamental----
    # Atualização da entrada
    edu5comp2 <- reactive({
      input$edu5muni
    })
    observeEvent(edu5comp2(), {
      x <- edu5 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != edu5comp2())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "edu5municomp2",
        choices = c("Selecione um município", choices),
        session
      )
    })
    ## Título
    output$edu5txt2 <- renderText({
      t52()
    })
    output$edu5graf2 <- renderEcharts4r({
      req(input$edu5municomp2)
      if (input$edu5municomp2 == "Selecione um município") {
        a <- edu5 %>% filter(localidade == input$edu5muni,
                             categoria == "Abandono Total")
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
            formatter = e_tooltip_pointer_formatter("decimal", digits = 1, locale = "pt-Br"),
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
      } else {
        a <- edu5 %>% filter(localidade == input$edu5muni,
                             categoria == "Abandono Total")
        b <-
          edu5 %>% filter(localidade == input$edu5municomp2,
                          categoria == "Abandono Total")
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$edu5muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$edu5municomp2,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 1, locale = "pt-Br"),
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
      }
    })
    ## Tabela - Taxa de Abandono no Ensino Fundamental----
    output$edu5txt3 <- renderText({
      t53()
    })
    output$edu5tab <- renderReactable({
      x <- edu5 %>%
        filter(localidade == input$edu5muni) %>%
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
          ano = colDef(name = "Ano")
        ),
        defaultColDef =
          colDef(
            footerStyle = list(fontWeight = "bold"),
            format = colFormat(separators = T),
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
    ## Tabela - Taxa de Abandono no Ensino Fundamental----
    output$edu5txt4 <- renderText({
      ri <- edu5 %>%
        filter(ano == input$edu5ano2, localidade == input$edu5muni) %>%
        pull(ri)
      paste0(
        "Taxa de Abandono no Ensino Fundamental por Esfera Administrativa  e Município, Região de Integração ",
        unique(ri),
        " - ",
        input$edu5ano2
      )
    })
    output$edu5tab1 <- renderReactable({
      ri <- edu5 %>%
        filter(ano == input$edu5ano2, localidade == input$edu5muni) %>%
        pull(ri)
      x <-
        edu5 %>% filter(ano == input$edu5ano2, localidade != "Pará")
      x <- x %>%
        filter(ri == ri) %>%
        select(ri,
               localidade,
               categoria,
               valor) %>%
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
          localidade = colDef(name = "Municípios")),
        defaultColDef =
          colDef(
            footerStyle = list(fontWeight = "bold"),
            format = colFormat(separators = T),
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
    
    
    # 6 - Taxa de Abandono no Ensino Médio por Esfera Administrativa----
    ## Gráfico de barras - Taxa de Abandono no Ensino Médio----
    # Atualização da entrada
    edu6comp1 <- reactive({
      input$edu6muni
    })
    observeEvent(edu6comp1(), {
      x <- edu6 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != edu6comp1())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "edu6municomp1",
        choices = c("Selecione um município", choices),
        session
      )
    })
    
    ## Título
    output$edu6txt1 <- renderText({
      t61()
    })
    
    output$edu6graf <- renderEcharts4r({
      req(input$edu6municomp1)
      if (input$edu6municomp1 == "Selecione um município") {
        a <- edu6 %>% filter(
          localidade == input$edu6muni,
          ano == input$edu6ano1,
          categoria != "Evasão Total"
        )
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
            barWidth = "30%",
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 1, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Esfera Administrativa",
            nameLocation = "middle",
            nameTextStyle = list(
              fontWeight = "bold",
              padding = c(20, 0, 0, 0),
              fontSize = 14
            )
          ) %>%
          e_y_axis(
            name = "Taxa",
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
          e_grid(show = T)
      } else {
        a <- edu6 %>% filter(
          localidade == input$edu6muni,
          ano == input$edu6ano1,
          categoria != "Evasão Total"
        )
        b <-
          edu6 %>% filter(
            localidade == input$edu6municomp1,
            ano == input$edu6ano1,
            categoria != "Evasão Total"
          )
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            name = input$edu6muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, categoria) %>%
          e_bar(
            serie = valor,
            name = input$edu6municomp1,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 1, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Esfera Administrativa",
            nameLocation = "middle",
            nameTextStyle = list(
              fontWeight = "bold",
              padding = c(20, 0, 0, 0),
              fontSize = 14
            )
          ) %>%
          e_y_axis(
            name = "Taxa",
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
          e_grid(show = T)
      }
    })
    ## Gráfico de linha - Taxa de Abandono no Ensino Médio----
    # Atualização da entrada
    edu6comp2 <- reactive({
      input$edu6muni
    })
    observeEvent(edu6comp2(), {
      x <- edu6 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != edu6comp2())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "edu6municomp2",
        choices = c("Selecione um município", choices),
        session
      )
    })
    ## Título
    output$edu6txt2 <- renderText({
      t62()
    })
    output$edu6graf2 <- renderEcharts4r({
      req(input$edu6municomp2)
      if (input$edu6municomp2 == "Selecione um município") {
        a <- edu6 %>% filter(localidade == input$edu6muni,
                             categoria == "Abandono Total")
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
            formatter = e_tooltip_pointer_formatter("decimal", digits = 1, locale = "pt-Br"),
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
      } else {
        a <- edu6 %>% filter(localidade == input$edu6muni,
                             categoria == "Abandono Total")
        b <-
          edu6 %>% filter(localidade == input$edu6municomp2,
                          categoria == "Abandono Total")
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$edu6muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$edu6municomp2,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 1, locale = "pt-Br"),
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
      }
    })
    ## Tabela - Taxa de Abandono no Ensino Médio----
    output$edu6txt3 <- renderText({
      t63()
    })
    output$edu6tab <- renderReactable({
      x <- edu6 %>%
        filter(localidade == input$edu6muni) %>%
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
          ano = colDef(name = "Ano")
        ),
        defaultColDef =
          colDef(
            footerStyle = list(fontWeight = "bold"),
            format = colFormat(separators = T),
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
    ## Tabela - Taxa de Abandono no Ensino Médio----
    output$edu6txt4 <- renderText({
      t64()
    })
    output$edu6tab1 <- renderReactable({
      ri <- edu6 %>%
        filter(ano == input$edu6ano2, localidade == input$edu6muni) %>%
        pull(ri)
      x <-
        edu6 %>% filter(ano == input$edu6ano2, localidade != "Pará")
      x <- x %>%
        filter(ri == ri) %>%
        select(ri,
               localidade,
               categoria,
               valor) %>%
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
          localidade = colDef(name = "Municípios")),
        defaultColDef =
          colDef(
            footerStyle = list(fontWeight = "bold"),
            format = colFormat(separators = T),
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
    # 7 - Distorção Idade-Série Total por Nível de Ensino----
    ## Gráfico de barras - Distorção Idade-Série Total por Nível de Ensino----
    # Atualização da entrada
    edu7comp1 <- reactive({
      input$edu7muni
    })
    observeEvent(edu7comp1(), {
      x <- edu7 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != edu7comp1())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "edu7municomp1",
        choices = c("Selecione um município", choices),
        session
      )
    })
    
    ## Título
    output$edu7txt1 <- renderText({
      t71()
    })
    output$edu7graf <- renderEcharts4r({
      req(input$edu7municomp1)
      if (input$edu7municomp1 == "Selecione um município") {
        a <- edu7 %>% filter(localidade == input$edu7muni,
                             ano == input$edu7ano1)
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            color = "#f17701",
            name = "Distorção",
            legend = F,
            symbol = "roundRect",
            symbolSize = 6,
            barWidth = "20%",
            legendHoverLink = T,
            barWidth = "30%",
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 1, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Esfera Administrativa",
            nameLocation = "middle",
            nameTextStyle = list(
              fontWeight = "bold",
              padding = c(20, 0, 0, 0),
              fontSize = 14
            )
          ) %>%
          e_y_axis(
            name = "Distorção",
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
          e_grid(show = T)
      } else {
        a <- edu7 %>% filter(localidade == input$edu7muni,
                             ano == input$edu7ano1)
        b <-
          edu7 %>% filter(localidade == input$edu7municomp1,
                          ano == input$edu7ano1)
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            name = input$edu7muni,
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
            name = input$edu7municomp1,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            barWidth = "20%",
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 1, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Esfera Administrativa",
            nameLocation = "middle",
            nameTextStyle = list(
              fontWeight = "bold",
              padding = c(20, 0, 0, 0),
              fontSize = 14
            )
          ) %>%
          e_y_axis(
            name = "Distorção",
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
          e_grid(show = T)
      }
    })
    ## Tabela - Distorção Idade-Série Total por Nível de Ensino----
    output$edu7txt2 <- renderText({
      t72()
    })
    output$edu7tab <- renderReactable({
      x <- edu7 %>%
        filter(localidade == input$edu7muni) %>%
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
          ano = colDef(name = "Ano")
        ),
        defaultColDef =
          colDef(
            footerStyle = list(fontWeight = "bold"),
            format = colFormat(separators = T),
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
    ## Tabela - Distorção Idade-Série Total por Nível de Ensino----
    output$edu7txt3 <- renderText({
      t73()
    })
    output$edu7tab1 <- renderReactable({
      ri <- edu7 %>%
        filter(ano == input$edu7ano2, localidade == input$edu7muni) %>%
        pull(ri)
      x <-
        edu7 %>% filter(ano == input$edu7ano2, localidade != "Pará")
      x <- x %>%
        filter(ri == ri) %>%
        select(ri,
               localidade,
               categoria,
               valor) %>%
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
          localidade = colDef(name = "Municípios")),
        defaultColDef =
          colDef(
            footerStyle = list(fontWeight = "bold"),
            format = colFormat(separators = T),
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
    # 8 -  IDEB - Escola pública - 5ª ano (séries iniciais)----
    ## Gráfico de linha - IDEB - Escola pública - 5ª ano (séries iniciais)----
    # Atualização da entrada
    edu8comp1 <- reactive({
      input$edu8muni
    })
    observeEvent(edu8comp1(), {
      x <- edu8 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != edu8comp1())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "edu8municomp1",
        choices = c("Selecione um município", choices),
        session
      )
    })
    
    output$edu8txt1 <- renderText({
      t81()
    })
    
    output$edu8graf <- renderEcharts4r({
      req(input$edu8municomp1)
      if (input$edu8municomp1 == "Selecione um município") {
        a <- edu8 %>% filter(localidade == input$edu8muni)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f17701",
            name = "Índice",
            legend = F,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 1, locale = "pt-Br"),
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
      } else {
        a <- edu8 %>% filter(localidade == input$edu8muni)
        b <-
          edu8 %>% filter(localidade == input$edu8municomp1)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$edu8muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$edu8municomp1,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 1, locale = "pt-Br"),
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
      }
    })
    ## Tabela - IDEB - Escola pública - 5ª ano (séries iniciais)----
    output$edu8txt2 <- renderText({
      t82()
    })
    output$edu8tab2 <- renderReactable({
      ris <- edu8 %>%
        filter(ano == input$edu8ano, localidade == input$edu8muni) %>%
        pull(ri)
      x <-
        edu8 %>% filter(ano == input$edu8ano, localidade != "Pará")
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
              number_fmt = scales::number_format(accuracy = 0.1,
                                                 decimal.mark = ",")
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
    # 9 -  IDEB - Escola pública - 9ª ano (séries finais)----
    ## Gráfico de linha - IDEB - Escola pública - 9ª ano (séries finais)----
    # Atualização da entrada
    edu9comp1 <- reactive({
      input$edu9muni
    })
    observeEvent(edu9comp1(), {
      x <- edu9 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != edu9comp1())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "edu9municomp1",
        choices = c("Selecione um município", choices),
        session
      )
    })
    output$edu9txt1 <- renderText({
      t91()
    })
    output$edu9graf <- renderEcharts4r({
      req(input$edu9municomp1)
      if (input$edu9municomp1 == "Selecione um município") {
        a <- edu9 %>% filter(localidade == input$edu9muni)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f17701",
            name = "índice",
            legend = F,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 1, locale = "pt-Br"),
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
      } else {
        a <- edu9 %>% filter(localidade == input$edu9muni)
        b <-
          edu9 %>% filter(localidade == input$edu9municomp1)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$edu9muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$edu9municomp1,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 1, locale = "pt-Br"),
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
      }
    })
    ## Tabela - IDEB - Escola pública - 9ª ano (séries finais) da mesma Região de Integração----
    output$edu9txt2 <- renderText({
      t92()
    })
    output$edu9tab2 <- renderReactable({
      ris <- edu9 %>%
        filter(ano == input$edu9ano, localidade == input$edu9muni) %>%
        pull(ri)
      x <-
        edu9 %>% filter(ano == input$edu9ano, localidade != "Pará")
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
              number_fmt = scales::number_format(accuracy = 0.1,
                                                 decimal.mark = ",")
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
    # 10 - Número de Matrículas no Ensino Pré-Escolar por Esfera Administrativa----
    ## Gráfico de barras- Número de Matrículas no Ensino Pré-Escolar----
    # Atualização da entrada
    edu10comp1 <- reactive({
      input$edu10muni
    })
    observeEvent(edu10comp1(), {
      x <- edu10 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != edu10comp1())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "edu10municomp1",
        choices = c("Selecione um município", choices),
        session
      )
    })
    
    ## Título
    output$edu10txt1 <- renderText({
      t101()
    })
    
    output$edu10graf <- renderEcharts4r({
      req(input$edu10municomp1)
      if (input$edu10municomp1 == "Selecione um município") {
        a <- edu10 %>% filter(
          localidade == input$edu10muni,
          ano == input$edu10ano1,
          categoria != "Total"
        )
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            color = "#f17701",
            name = "Número de Matrículas",
            legend = F,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            barWidth = "30%",
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 0, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Esfera Administrativa",
            nameLocation = "middle",
            nameTextStyle = list(
              fontWeight = "bold",
              padding = c(20, 0, 0, 0),
              fontSize = 14
            )
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
          e_grid(show = T)
      } else {
        a <- edu10 %>% filter(
          localidade == input$edu10muni,
          ano == input$edu10ano1,
          categoria != "Total"
        )
        b <-
          edu10 %>% filter(
            localidade == input$edu10municomp1,
            ano == input$edu10ano1,
            categoria != "Total"
          )
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            name = input$edu10muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, categoria) %>%
          e_bar(
            serie = valor,
            name = input$edu10municomp1,
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
            name = "Esfera Administrativa",
            nameLocation = "middle",
            nameTextStyle = list(
              fontWeight = "bold",
              padding = c(20, 0, 0, 0),
              fontSize = 14
            )
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
          e_grid(show = T)
      }
    })
    ## Gráfico de linha- Número de Matrículas no Ensino Pré-Escolar----
    # Atualização da entrada
    edu10comp2 <- reactive({
      input$edu10muni
    })
    observeEvent(edu10comp2(), {
      x <- edu10 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != edu10comp2())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "edu10municomp2",
        choices = c("Selecione um município", choices),
        session
      )
    })
    ## Título
    output$edu10txt2 <- renderText({
      t102()
    })
    output$edu10graf2 <- renderEcharts4r({
      req(input$edu10municomp2)
      if (input$edu10municomp2 == "Selecione um município") {
        a <- edu10 %>% filter(localidade == input$edu10muni,
                              categoria == "Total")
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
            name = "Número Total",
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
      } else {
        a <- edu10 %>% filter(localidade == input$edu10muni,
                              categoria == "Total")
        b <-
          edu10 %>% filter(localidade == input$edu10municomp2,
                           categoria == "Total")
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$edu10muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$edu10municomp2,
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
            name = "Número Total",
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
      }
    })
    ## Tabela - Número de Matrículas no Ensino Pré-Escolar----
    output$edu10txt3 <- renderText({
      t103()
    })
    output$edu10tab <- renderReactable({
      x <- edu10 %>%
        filter(localidade == input$edu10muni) %>%
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
          ano = colDef(name = "Ano")
        ),
        defaultColDef =
          colDef(
            footerStyle = list(fontWeight = "bold"),
            format = colFormat(separators = T),
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
    ## Tabela - Número de Matrículas no Ensino Pré-Escolar----
    output$edu10txt4 <- renderText({
      t104()
    })
    output$edu10tab1 <- renderReactable({
      ri <- edu10 %>%
        filter(ano == input$edu10ano2,
               localidade == input$edu10muni) %>%
        pull(ri)
      x <-
        edu10 %>% filter(ano == input$edu10ano2, localidade != "Pará")
      x <- x %>%
        filter(ri == ri) %>%
        select(ri,
               localidade,
               categoria,
               valor) %>%
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
          localidade = colDef(name = "Municípios")),
        defaultColDef =
          colDef(
            footerStyle = list(fontWeight = "bold"),
            format = colFormat(separators = T),
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
    # 11 - Número de Matrículas no Ensino Fundamental por Esfera Administrativa----
    ## Gráfico de barras- Número de Matrículas no Ensino Fundamental----
    # Atualização da entrada
    edu11comp1 <- reactive({
      input$edu11muni
    })
    observeEvent(edu11comp1(), {
      x <- edu11 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != edu11comp1())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "edu11municomp1",
        choices = c("Selecione um município", choices),
        session
      )
    })
    
    ## Título
    output$edu11txt1 <- renderText({
      t111()
    })
    
    output$edu11graf <- renderEcharts4r({
      req(input$edu11municomp1)
      if (input$edu11municomp1 == "Selecione um município") {
        a <- edu11 %>% filter(
          localidade == input$edu11muni,
          ano == input$edu11ano1,
          categoria != "Total"
        )
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            color = "#f17701",
            name = "Número de Matrículas",
            legend = F,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            barWidth = "30%",
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 0, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Esfera Administrativa",
            nameLocation = "middle",
            nameTextStyle = list(
              fontWeight = "bold",
              padding = c(20, 0, 0, 0),
              fontSize = 14
            )
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
          e_grid(show = T)
      } else {
        a <- edu11 %>% filter(
          localidade == input$edu11muni,
          ano == input$edu11ano1,
          categoria != "Total"
        )
        b <-
          edu11 %>% filter(
            localidade == input$edu11municomp1,
            ano == input$edu11ano1,
            categoria != "Total"
          )
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            name = input$edu11muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, categoria) %>%
          e_bar(
            serie = valor,
            name = input$edu11municomp1,
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
            name = "Esfera Administrativa",
            nameLocation = "middle",
            nameTextStyle = list(
              fontWeight = "bold",
              padding = c(20, 0, 0, 0),
              fontSize = 14
            )
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
          e_grid(show = T)
      }
    })
    ## Gráfico de linha- Número de Matrículas no Ensino Fundamental----
    # Atualização da entrada
    edu11comp2 <- reactive({
      input$edu11muni
    })
    observeEvent(edu11comp2(), {
      x <- edu11 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != edu11comp2())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "edu11municomp2",
        choices = c("Selecione um município", choices),
        session
      )
    })
    ## Título
    output$edu11txt2 <- renderText({
      t112()
    })
    output$edu11graf2 <- renderEcharts4r({
      req(input$edu11municomp2)
      if (input$edu11municomp2 == "Selecione um município") {
        a <- edu11 %>% filter(localidade == input$edu11muni,
                              categoria == "Total")
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f17701",
            name = "Número de Matrículas",
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
      } else {
        a <- edu11 %>% filter(localidade == input$edu11muni,
                              categoria == "Total")
        b <-
          edu11 %>% filter(localidade == input$edu11municomp2,
                           categoria == "Total")
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$edu11muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$edu11municomp2,
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
            name = "Taxa",
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
      }
    })
    ## Tabela - Número de Matrículas no Ensino Fundamental----
    output$edu11txt3 <- renderText({
      t113()
    })
    output$edu11tab <- renderReactable({
      x <- edu11 %>%
        filter(localidade == input$edu11muni) %>%
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
          ano = colDef(name = "Ano")
        ),
        defaultColDef =
          colDef(
            footerStyle = list(fontWeight = "bold"),
            format = colFormat(separators = T),
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
    ## Tabela - Número de Matrículas no Ensino Fundamental----
    output$edu11txt4 <- renderText({
      t114()
    })
    output$edu11tab1 <- renderReactable({
      ri <- edu11 %>%
        filter(ano == input$edu11ano2,
               localidade == input$edu11muni) %>%
        pull(ri)
      x <-
        edu11 %>% filter(ano == input$edu11ano2, localidade != "Pará")
      x <- x %>%
        filter(ri == ri) %>%
        select(ri,
               localidade,
               categoria,
               valor) %>%
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
          localidade = colDef(name = "Municípios")),
        defaultColDef =
          colDef(
            footerStyle = list(fontWeight = "bold"),
            format = colFormat(separators = T),
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
    # 12 - Número de Matrículas no Ensino Médio por Esfera Administrativa----
    ## Gráfico de barras - Número de Matrículas no Ensino Médio----
    # Atualização da entrada
    edu12comp1 <- reactive({
      input$edu12muni
    })
    observeEvent(edu12comp1(), {
      x <- edu12 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != edu12comp1())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "edu12municomp1",
        choices = c("Selecione um município", choices),
        session
      )
    })
    
    ## Título
    output$edu12txt1 <- renderText({
      t121()
    })
    
    output$edu12graf <- renderEcharts4r({
      req(input$edu12municomp1)
      if (input$edu12municomp1 == "Selecione um município") {
        a <- edu12 %>% filter(
          localidade == input$edu12muni,
          ano == input$edu12ano1,
          categoria != "Total Médio"
        )
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            color = "#f17701",
            name = "Número",
            legend = F,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            barWidth = "30%",
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 0, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Esfera Administrativa",
            nameLocation = "middle",
            nameTextStyle = list(
              fontWeight = "bold",
              padding = c(20, 0, 0, 0),
              fontSize = 14
            )
          ) %>%
          e_y_axis(
            name = "Número",
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
          e_grid(show = T)
      } else {
        a <- edu12 %>% filter(
          localidade == input$edu12muni,
          ano == input$edu12ano1,
          categoria != "Total Médio"
        )
        b <-
          edu12 %>% filter(
            localidade == input$edu12municomp1,
            ano == input$edu12ano1,
            categoria != "Total Médio"
          )
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            name = input$edu12muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, categoria) %>%
          e_bar(
            serie = valor,
            name = input$edu12municomp1,
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
            name = "Esfera Administrativa",
            nameLocation = "middle",
            nameTextStyle = list(
              fontWeight = "bold",
              padding = c(20, 0, 0, 0),
              fontSize = 14
            )
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
          e_grid(show = T)
      }
    })
    ## Gráfico de linha - Número de Matrículas no Ensino Médio----
    # Atualização da entrada
    edu12comp2 <- reactive({
      input$edu12muni
    })
    observeEvent(edu12comp2(), {
      x <- edu12 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != edu12comp2())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "edu12municomp2",
        choices = c("Selecione um município", choices),
        session
      )
    })
    ## Título
    output$edu12txt2 <- renderText({
      t122()
    })
    output$edu12graf2 <- renderEcharts4r({
      req(input$edu12municomp2)
      if (input$edu12municomp2 == "Selecione um município") {
        a <- edu12 %>% filter(localidade == input$edu12muni,
                              categoria == "Total")
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f17701",
            name = "Número de Matrículas",
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
            name = "Taxa",
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
      } else {
        a <- edu12 %>% filter(localidade == input$edu12muni,
                              categoria == "Total")
        b <-
          edu12 %>% filter(localidade == input$edu12municomp2,
                           categoria == "Total")
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$edu12muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$edu12municomp2,
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
            name = "Taxa",
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
      }
    })
    ## Tabela - Número de Matrículas no Ensino Médio----
    output$edu12txt3 <- renderText({
      t123()
    })
    output$edu12tab <- renderReactable({
      x <- edu12 %>%
        filter(localidade == input$edu12muni) %>%
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
          ano = colDef(name = "Ano")
        ),
        defaultColDef =
          colDef(
            footerStyle = list(fontWeight = "bold"),
            format = colFormat(separators = T),
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
    ## Tabela - Número de Matrículas no Ensino Médio----
    output$edu12txt4 <- renderText({
      t124()
    })
    output$edu12tab1 <- renderReactable({
      ri <- edu12 %>%
        filter(ano == input$edu12ano2,
               localidade == input$edu12muni) %>%
        pull(ri)
      x <-
        edu12 %>% filter(ano == input$edu12ano2, localidade != "Pará")
      x <- x %>%
        filter(ri == ri) %>%
        select(ri,
               localidade,
               categoria,
               valor) %>%
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
          localidade = colDef(name = "Municípios")),
        defaultColDef =
          colDef(
            footerStyle = list(fontWeight = "bold"),
            format = colFormat(separators = T),
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
    # 13 - Média de Alunos por Turma por Nível de Ensino----
    ## Gráfico de barras - Média de Alunos por Turma por Nível de Ensino----
    # Atualização da entrada
    edu13comp1 <- reactive({
      input$edu13muni
    })
    observeEvent(edu13comp1(), {
      x <- edu13 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != edu13comp1())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "edu13municomp1",
        choices = c("Selecione um município", choices),
        session
      )
    })
    ## Título
    output$edu13txt1 <- renderText({
      t131()
    })
    output$edu13graf <- renderEcharts4r({
      req(input$edu13municomp1)
      if (input$edu13municomp1 == "Selecione um município") {
        a <- edu13 %>%
          filter(localidade == input$edu13muni,
                 ano == input$edu13ano1)
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            color = "#f17701",
            name = "Média",
            legend = F,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            barWidth = "20%",
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 1, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Esfera Administrativa",
            nameLocation = "middle",
            nameTextStyle = list(
              fontWeight = "bold",
              padding = c(20, 0, 0, 0),
              fontSize = 14
            )
          ) %>%
          e_y_axis(
            name = "Média",
            nameTextStyle = list(fontWeight = "bold", fontSize = 14),
            scale = T,
            axisLabel = list(
              formatter = htmlwidgets::JS(
                "
              function (value, index) {
              return value.toLocaleString('pt-BR', { minimumFractionDigits: 1, maximumFractionDigits: 1 });
              }
            "
              )
            )
          ) %>%
          e_locale("pt-Br") %>%
          e_grid(show = T)
      } else {
        a <- edu13 %>%
          filter(localidade == input$edu13muni,
                 ano == input$edu13ano1)
        b <- edu13 %>%
          filter(localidade == input$edu13municomp1,
                 ano == input$edu13ano1)
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            name = input$edu13muni,
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
            name = input$edu13municomp1,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            barWidth = "20%",
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 1, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Esfera Administrativa",
            nameLocation = "middle",
            nameTextStyle = list(
              fontWeight = "bold",
              padding = c(20, 0, 0, 0),
              fontSize = 14
            )
          ) %>%
          e_y_axis(
            name = "Média",
            nameTextStyle = list(fontWeight = "bold", fontSize = 14),
            scale = T,
            axisLabel = list(
              formatter = htmlwidgets::JS(
                "
              function (value, index) {
              return value.toLocaleString('pt-BR', { minimumFractionDigits: 1, maximumFractionDigits: 1 });
              }
            "
              )
            )
          ) %>%
          e_locale("pt-Br") %>%
          e_grid(show = T)
      }
    })
    ## Tabela - Média de Alunos por Turma por Nível de Ensino----
    output$edu13txt2 <- renderText({
      t132()
    })
    output$edu13tab <- renderReactable({
      x <- edu13 %>%
        filter(localidade == input$edu13muni) %>%
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
          Frota = colDef(name = "Total")),
        defaultColDef =
          colDef(
            footerStyle = list(fontWeight = "bold"),
            format = colFormat(separators = T),
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
    ## Tabela - Média de Alunos por Turma por Nível de Ensino----
    output$edu13txt3 <- renderText({
      t133()
    })
    output$edu13tab1 <- renderReactable({
      ri <- edu13 %>%
        filter(ano == input$edu13ano2,
               localidade == input$edu13muni) %>%
        pull(ri)
      x <-
        edu13 %>% filter(ano == input$edu13ano2, localidade != "Pará")
      x <- x %>%
        filter(ri == ri) %>%
        select(ri,
               localidade,
               categoria,
               valor) %>%
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
          localidade = colDef(name = "Municípios")),
        defaultColDef =
          colDef(
            footerStyle = list(fontWeight = "bold"),
            format = colFormat(separators = T),
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
    
    # 14 - Número de Docentes no Ensino Pré-escolar por Esfera Administrativa----
    ## Gráfico de barras - Número de Docentes no Ensino Pré-escolar----
    # Atualização da entrada
    edu14comp1 <- reactive({
      input$edu14muni
    })
    observeEvent(edu14comp1(), {
      x <- edu14 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != edu14comp1())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "edu14municomp1",
        choices = c("Selecione um município", choices),
        session
      )
    })
    ## Título
    output$edu14txt1 <- renderText({
      t141()
    })
    output$edu14graf <- renderEcharts4r({
      req(input$edu14municomp1)
      if (input$edu14municomp1 == "Selecione um município") {
        a <- edu14 %>%
          filter(
            localidade == input$edu14muni,
            ano == input$edu14ano1,
            categoria != "Total"
          )
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            color = "#f17701",
            name = "Número de Docentes",
            legend = F,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            barWidth = "30%",
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 0, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Esfera Administrativa",
            nameLocation = "middle",
            nameTextStyle = list(
              fontWeight = "bold",
              padding = c(20, 0, 0, 0),
              fontSize = 14
            )
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
          e_grid(show = T)
      } else {
        a <- edu14 %>%
          filter(
            localidade == input$edu14muni,
            ano == input$edu14ano1,
            categoria != "Total"
          )
        b <- edu14 %>%
          filter(
            localidade == input$edu14municomp1,
            ano == input$edu14ano1,
            categoria != "Total"
          )
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            name = input$edu14muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, categoria) %>%
          e_bar(
            serie = valor,
            name = input$edu14municomp1,
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
            name = "Esfera Administrativa",
            nameLocation = "middle",
            nameTextStyle = list(
              fontWeight = "bold",
              padding = c(20, 0, 0, 0),
              fontSize = 14
            )
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
          e_grid(show = T)
      }
    })
    ## Gráfico de linhas - Número de Docentes no Ensino Pré-escolar----
    # Atualização da entrada
    edu14comp2 <- reactive({
      input$edu14muni
    })
    observeEvent(edu14comp2(), {
      x <- edu14 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != edu14comp2())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "edu14municomp2",
        choices = c("Selecione um município", choices),
        session
      )
    })
    ## Título
    output$edu14txt2 <- renderText({
      t142()
    })
    output$edu14graf2 <- renderEcharts4r({
      req(input$edu14municomp2)
      if (input$edu14municomp2 == "Selecione um município") {
        a <- edu14 %>% filter(localidade == input$edu14muni,
                              categoria == "Total")
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f17701",
            name = "Número de Docentes",
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
            name = "Taxa",
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
      } else {
        a <- edu14 %>% filter(localidade == input$edu14muni,
                              categoria == "Total")
        b <-
          edu14 %>% filter(localidade == input$edu14municomp2,
                           categoria == "Total")
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$edu14muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$edu14municomp2,
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
            name = "Taxa",
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
      }
    })
    ## Tabela - Número de Docentes no Ensino Pré-escolar----
    output$edu14txt3 <- renderText({
      t143()
    })
    output$edu14tab <- renderReactable({
      x <- edu14 %>%
        filter(localidade == input$edu14muni) %>%
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
          ano = colDef(name = "Ano")
        ),
        defaultColDef =
          colDef(
            footerStyle = list(fontWeight = "bold"),
            na = "-",
            format = colFormat(separators = T),
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
    ## Tabela - Número de Docentes no Ensino Pré-escolar----
    output$edu14txt4 <- renderText({
      t144()
    })
    output$edu14tab1 <- renderReactable({
      ri <- edu14 %>%
        filter(ano == input$edu14ano2,
               localidade == input$edu14muni) %>%
        pull(ri)
      x <-
        edu14 %>% filter(ano == input$edu14ano2, localidade != "Pará")
      x <- x %>%
        filter(ri == ri) %>%
        select(ri,
               localidade,
               categoria,
               valor) %>%
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
          localidade = colDef(name = "Municípios")),
        defaultColDef =
          colDef(
            footerStyle = list(fontWeight = "bold"),
            na = "-",
            format = colFormat(separators = T),
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
    
    # 15 - Número de Docentes no Ensino Fundamental por Esfera Administrativa----
    ## Gráfico de barras - Número de Docentes no Ensino Fundamental----
    # Atualização da entrada
    edu15comp1 <- reactive({
      input$edu15muni
    })
    observeEvent(edu15comp1(), {
      x <- edu15 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != edu15comp1())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "edu15municomp1",
        choices = c("Selecione um município", choices),
        session
      )
    })
    ## Título
    output$edu15txt1 <- renderText({
      t151()
    })
    output$edu15graf <- renderEcharts4r({
      req(input$edu15municomp1)
      if (input$edu15municomp1 == "Selecione um município") {
        a <- edu15 %>%
          filter(
            localidade == input$edu15muni,
            ano == input$edu15ano1,
            categoria != "Total"
          )
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            color = "#f17701",
            name = "Número de Docentes",
            legend = F,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            barWidth = "30%",
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 0, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Esfera Administrativa",
            nameLocation = "middle",
            nameTextStyle = list(
              fontWeight = "bold",
              padding = c(20, 0, 0, 0),
              fontSize = 14
            )
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
          e_grid(show = T)
      } else {
        a <- edu15 %>% filter(
          localidade == input$edu15muni,
          ano == input$edu15ano1,
          categoria != "Total"
        )
        b <-
          edu15 %>% filter(
            localidade == input$edu15municomp1,
            ano == input$edu15ano1,
            categoria != "Total"
          )
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            name = input$edu15muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, categoria) %>%
          e_bar(
            serie = valor,
            name = input$edu15municomp1,
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
            name = "Esfera Administrativa",
            nameLocation = "middle",
            nameTextStyle = list(
              fontWeight = "bold",
              padding = c(20, 0, 0, 0),
              fontSize = 14
            )
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
          e_grid(show = T)
      }
    })
    ## Gráfico de linhas - Número de Docentes no Ensino Fundamental----
    # Atualização da entrada
    edu15comp2 <- reactive({
      input$edu15muni
    })
    observeEvent(edu15comp2(), {
      x <- edu15 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != edu15comp2())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "edu15municomp2",
        choices = c("Selecione um município", choices),
        session
      )
    })
    ## Título
    output$edu15txt2 <- renderText({
      t152()
    })
    output$edu15graf2 <- renderEcharts4r({
      req(input$edu15municomp2)
      if (input$edu15municomp2 == "Selecione um município") {
        a <- edu15 %>% filter(localidade == input$edu15muni,
                              categoria == "Total")
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f17701",
            name = "Número de Docentes",
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
            name = "Taxa",
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
      } else {
        a <- edu15 %>% filter(localidade == input$edu15muni,
                              categoria == "Total")
        b <-
          edu15 %>% filter(localidade == input$edu15municomp2,
                           categoria == "Total")
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$edu15muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$edu15municomp2,
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
            name = "Taxa",
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
      }
    })
    ## Tabela - Número de Docentes no Ensino Fundamental----
    output$edu15txt3 <- renderText({
      t153()
    })
    output$edu15tab <- renderReactable({
      x <- edu15 %>%
        filter(localidade == input$edu15muni) %>%
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
          ano = colDef(name = "Ano")
        ),
        defaultColDef =
          colDef(
            footerStyle = list(fontWeight = "bold"),
            na = "-",
            format = colFormat(separators = T),
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
    ## Tabela - Número de Docentes no Ensino Fundamental----
    output$edu15txt4 <- renderText({
      t154()
    })
    output$edu15tab1 <- renderReactable({
      ri <- edu15 %>%
        filter(ano == input$edu15ano2,
               localidade == input$edu15muni) %>%
        pull(ri)
      x <-
        edu15 %>% filter(ano == input$edu15ano2, localidade != "Pará")
      x <- x %>%
        filter(ri == ri) %>%
        select(ri,
               localidade,
               categoria,
               valor) %>%
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
          localidade = colDef(name = "Municípios")),
        defaultColDef =
          colDef(
            footerStyle = list(fontWeight = "bold"),
            na = "-",
            format = colFormat(separators = T),
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
    
    # 16 - Número de Docentes no Ensino Médio por Esfera Administrativa----
    ## Gráfico de barras- Número de Docentes no Ensino Médio----
    # Atualização da entrada
    edu16comp1 <- reactive({
      input$edu16muni
    })
    observeEvent(edu16comp1(), {
      x <- edu16 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != edu16comp1())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "edu16municomp1",
        choices = c("Selecione um município", choices),
        session
      )
    })
    
    ## Título
    output$edu16txt1 <- renderText({
      t161()
    })
    
    output$edu16graf <- renderEcharts4r({
      req(input$edu16municomp1)
      if (input$edu16municomp1 == "Selecione um município") {
        a <- edu16 %>% filter(
          localidade == input$edu16muni,
          ano == input$edu16ano1,
          categoria != "Total"
        )
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            color = "#f17701",
            name = "Número de Docentes",
            legend = F,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            barWidth = "30%",
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 0, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Esfera Administrativa",
            nameLocation = "middle",
            nameTextStyle = list(
              fontWeight = "bold",
              padding = c(20, 0, 0, 0),
              fontSize = 14
            )
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
          e_grid(show = T)
      } else {
        a <- edu16 %>% filter(
          localidade == input$edu16muni,
          ano == input$edu16ano1,
          categoria != "Total"
        )
        b <-
          edu16 %>% filter(
            localidade == input$edu16municomp1,
            ano == input$edu16ano1,
            categoria != "Total"
          )
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            name = input$edu16muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, categoria) %>%
          e_bar(
            serie = valor,
            name = input$edu16municomp1,
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
            name = "Esfera Administrativa",
            nameLocation = "middle",
            nameTextStyle = list(
              fontWeight = "bold",
              padding = c(20, 0, 0, 0),
              fontSize = 14
            )
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
          e_grid(show = T)
      }
    })
    ## Gráfico de linha - Número de Docentes no Ensino Médio----
    # Atualização da entrada
    edu16comp2 <- reactive({
      input$edu16muni
    })
    observeEvent(edu16comp2(), {
      x <- edu16 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != edu16comp2())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "edu16municomp2",
        choices = c("Selecione um município", choices),
        session
      )
    })
    ## Título
    output$edu16txt2 <- renderText({
      t162()
    })
    output$edu16graf2 <- renderEcharts4r({
      req(input$edu16municomp2)
      if (input$edu16municomp2 == "Selecione um município") {
        a <- edu16 %>%
          filter(localidade == input$edu16muni,
                 categoria == "Total")
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f17701",
            name = "Número de Docentes",
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
            name = "Taxa",
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
      } else {
        a <- edu16 %>% filter(localidade == input$edu16muni,
                              categoria == "Total")
        b <-
          edu16 %>% filter(localidade == input$edu16municomp2,
                           categoria == "Total")
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$edu16muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$edu16municomp2,
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
            name = "Taxa",
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
      }
    })
    ## Tabela - Número de Docentes no Ensino Médio----
    output$edu16txt3 <- renderText({
      t163()
    })
    output$edu16tab <- renderReactable({
      x <- edu16 %>%
        filter(localidade == input$edu16muni) %>%
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
          ano = colDef(name = "Ano")
        ),
        defaultColDef =
          colDef(
            footerStyle = list(fontWeight = "bold"),
            na = "-",
            format = colFormat(separators = T),
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
    ## Tabela - Número de Docentes no Ensino Médio----
    output$edu16txt4 <- renderText({
      t164()
    })
    output$edu16tab1 <- renderReactable({
      ri <- edu16 %>%
        filter(ano == input$edu16ano2,
               localidade == input$edu16muni) %>%
        pull(ri)
      x <-
        edu16 %>% filter(ano == input$edu16ano2, localidade != "Pará")
      x <- x %>%
        filter(ri == ri) %>%
        select(ri,
               localidade,
               categoria,
               valor) %>%
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
          localidade = colDef(name = "Municípios")),
        defaultColDef =
          colDef(
            footerStyle = list(fontWeight = "bold"),
            na = "-",
            format = colFormat(separators = T),
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
    # 17 - Estabelecimentos de Pré-Escola por Esfera Administrativa----
    ## Gráfico de barras- Estabelecimentos de Pré-Escola----
    # Atualização da entrada
    edu17comp1 <- reactive({
      input$edu17muni
    })
    observeEvent(edu17comp1(), {
      x <- edu17 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != edu17comp1())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "edu17municomp1",
        choices = c("Selecione um município", choices),
        session
      )
    })
    ## Título
    output$edu17txt1 <- renderText({
      t171()
    })
    output$edu17graf <- renderEcharts4r({
      req(input$edu17municomp1)
      if (input$edu17municomp1 == "Selecione um município") {
        a <- edu17 %>% filter(
          localidade == input$edu17muni,
          ano == input$edu17ano1,
          categoria != "Total"
        )
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            color = "#f17701",
            name = "Quantidade",
            legend = F,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            barWidth = "30%",
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 0, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Esfera Administrativa",
            nameLocation = "middle",
            nameTextStyle = list(
              fontWeight = "bold",
              padding = c(20, 0, 0, 0),
              fontSize = 14
            )
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
          e_grid(show = T)
      } else {
        a <- edu17 %>% filter(
          localidade == input$edu17muni,
          ano == input$edu17ano1,
          categoria != "Total"
        )
        b <-
          edu17 %>% filter(
            localidade == input$edu17municomp1,
            ano == input$edu17ano1,
            categoria != "Total"
          )
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            name = input$edu17muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, categoria) %>%
          e_bar(
            serie = valor,
            name = input$edu17municomp1,
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
            name = "Esfera Administrativa",
            nameLocation = "middle",
            nameTextStyle = list(
              fontWeight = "bold",
              padding = c(20, 0, 0, 0),
              fontSize = 14
            )
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
          e_grid(show = T)
      }
    })
    ## Gráfico de linha- Estabelecimentos de Pré-Escola----
    # Atualização da entrada
    edu17comp2 <- reactive({
      input$edu17muni
    })
    observeEvent(edu17comp2(), {
      x <- edu17 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != edu17comp2())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "edu17municomp2",
        choices = c("Selecione um município", choices),
        session
      )
    })
    ## Título
    output$edu17txt2 <- renderText({
      t172()
    })
    output$edu17graf2 <- renderEcharts4r({
      req(input$edu17municomp2)
      if (input$edu17municomp2 == "Selecione um município") {
        a <- edu17 %>%
          filter(localidade == input$edu17muni,
                 categoria == "Total")
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f17701",
            name = input$edu17muni,
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
      } else {
        a <- edu17 %>% filter(localidade == input$edu17muni,
                              categoria == "Total")
        b <-
          edu17 %>% filter(localidade == input$edu17municomp2,
                           categoria == "Total")
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$edu17muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$edu17municomp2,
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
            name = "Quantidade",
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
      }
    })
    ## Tabela - Estabelecimentos de Pré-Escola----
    output$edu17txt3 <- renderText({
      t173()
    })
    output$edu17tab <- renderReactable({
      x <- edu17 %>%
        filter(localidade == input$edu17muni) %>%
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
          ano = colDef(name = "Ano")
        ),
        defaultColDef =
          colDef(
            footerStyle = list(fontWeight = "bold"),
            na = "-",
            format = colFormat(separators = T),
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
    ## Tabela - Estabelecimentos de Pré-Escola----
    output$edu17txt4 <- renderText({
      t174()
    })
    output$edu17tab1 <- renderReactable({
      ri <- edu17 %>%
        filter(ano == input$edu17ano2,
               localidade == input$edu17muni) %>%
        pull(ri)
      x <-
        edu17 %>% filter(ano == input$edu17ano2, localidade != "Pará")
      x <- x %>%
        filter(ri == ri) %>%
        select(ri,
               localidade,
               categoria,
               valor) %>%
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
          localidade = colDef(name = "Municípios")),
        defaultColDef =
          colDef(
            footerStyle = list(fontWeight = "bold"),
            na = "-",
            format = colFormat(separators = T),
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
    # 18 - Estabelecimentos de Ensino Fundamental por Esfera Administrativa----
    ## Gráfico de barras - Estabelecimentos de Ensino Fundamental----
    # Atualização da entrada
    edu18comp1 <- reactive({
      input$edu18muni
    })
    observeEvent(edu18comp1(), {
      x <- edu18 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != edu18comp1())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "edu18municomp1",
        choices = c("Selecione um município", choices),
        session
      )
    })
    
    ## Título
    output$edu18txt1 <- renderText({
      t181()
    })
    
    output$edu18graf <- renderEcharts4r({
      req(input$edu18municomp1)
      if (input$edu18municomp1 == "Selecione um município") {
        a <- edu18 %>% filter(
          localidade == input$edu18muni,
          ano == input$edu18ano1,
          categoria != "Total"
        )
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            color = "#f17701",
            name = "Quantidade",
            legend = F,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            barWidth = "30%",
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 0, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Esfera Administrativa",
            nameLocation = "middle",
            nameTextStyle = list(
              fontWeight = "bold",
              padding = c(20, 0, 0, 0),
              fontSize = 14
            )
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
          e_grid(show = T)
      } else {
        a <- edu18 %>% filter(
          localidade == input$edu18muni,
          ano == input$edu18ano1,
          categoria != "Total"
        )
        b <-
          edu18 %>% filter(
            localidade == input$edu18municomp1,
            ano == input$edu18ano1,
            categoria != "Total"
          )
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            name = input$edu18muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, categoria) %>%
          e_bar(
            serie = valor,
            name = input$edu18municomp1,
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
            name = "Esfera Administrativa",
            nameLocation = "middle",
            nameTextStyle = list(
              fontWeight = "bold",
              padding = c(20, 0, 0, 0),
              fontSize = 14
            )
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
          e_grid(show = T)
      }
    })
    ## Gráfico de linha - Estabelecimentos de Ensino Fundamental----
    # Atualização da entrada
    edu18comp2 <- reactive({
      input$edu18muni
    })
    observeEvent(edu18comp2(), {
      x <- edu18 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != edu18comp2())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "edu18municomp2",
        choices = c("Selecione um município", choices),
        session
      )
    })
    ## Título
    output$edu18txt2 <- renderText({
      t182()
    })
    output$edu18graf2 <- renderEcharts4r({
      req(input$edu18municomp2)
      if (input$edu18municomp2 == "Selecione um município") {
        a <- edu18 %>%
          filter(localidade == input$edu18muni,
                 categoria == "Total")
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f17701",
            name = input$edu18muni,
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
      } else {
        a <- edu18 %>% filter(localidade == input$edu18muni,
                              categoria == "Total")
        b <-
          edu18 %>% filter(localidade == input$edu18municomp2,
                           categoria == "Total")
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$edu18muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$edu18municomp2,
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
            name = "Quantidade",
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
      }
    })
    ## Tabela - Estabelecimentos de Ensino Fundamental----
    output$edu18txt3 <- renderText({
      t183()
    })
    output$edu18tab <- renderReactable({
      x <- edu18 %>%
        filter(localidade == input$edu18muni) %>%
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
          ano = colDef(name = "Ano")
        ),
        defaultColDef =
          colDef(
            footerStyle = list(fontWeight = "bold"),
            na = "-",
            format = colFormat(separators = T),
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
    ## Tabela - Estabelecimentos de Ensino Fundamental----
    output$edu18txt4 <- renderText({
      t184()
    })
    output$edu18tab1 <- renderReactable({
      ri <- edu18 %>%
        filter(ano == input$edu18ano2,
               localidade == input$edu18muni) %>%
        pull(ri)
      x <-
        edu18 %>% filter(ano == input$edu18ano2, localidade != "Pará")
      x <- x %>%
        filter(ri == ri) %>%
        select(ri,
               localidade,
               categoria,
               valor) %>%
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
          localidade = colDef(name = "Municípios")),
        defaultColDef =
          colDef(
            footerStyle = list(fontWeight = "bold"),
            na = "-",
            format = colFormat(separators = T),
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
    # 19 - Estabelecimentos de Ensino Médio por Esfera Administrativa----
    ## Gráfico de barras- Estabelecimentos de Ensino Médio----
    # Atualização da entrada
    edu19comp1 <- reactive({
      input$edu19muni
    })
    observeEvent(edu19comp1(), {
      x <- edu19 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != edu19comp1())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "edu19municomp1",
        choices = c("Selecione um município", choices),
        session
      )
    })
    
    ## Título
    output$edu19txt1 <- renderText({
      t191()
    })
    output$edu19graf <- renderEcharts4r({
      req(input$edu19municomp1)
      if (input$edu19municomp1 == "Selecione um município") {
        a <- edu19 %>%
          filter(
            localidade == input$edu19muni,
            ano == input$edu19ano1,
            categoria != "Total"
          )
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            color = "#f17701",
            name = "Quantidade",
            legend = F,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            barWidth = "30%",
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 0, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Esfera Administrativa",
            nameLocation = "middle",
            nameTextStyle = list(
              fontWeight = "bold",
              padding = c(20, 0, 0, 0),
              fontSize = 14
            )
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
          e_grid(show = T)
      } else {
        a <- edu19 %>%
          filter(
            localidade == input$edu19muni,
            ano == input$edu19ano1,
            categoria != "Total"
          )
        b <- edu19 %>%
          filter(
            localidade == input$edu19municomp1,
            ano == input$edu19ano1,
            categoria != "Total"
          )
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            name = input$edu19muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, categoria) %>%
          e_bar(
            serie = valor,
            name = input$edu19municomp1,
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
            name = "Esfera Administrativa",
            nameLocation = "middle",
            nameTextStyle = list(
              fontWeight = "bold",
              padding = c(20, 0, 0, 0),
              fontSize = 14
            )
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
          e_grid(show = T)
      }
    })
    ## Gráfico de linha- Estabelecimentos de Ensino Médio----
    # Atualização da entrada
    edu19comp2 <- reactive({
      input$edu19muni
    })
    observeEvent(edu19comp2(), {
      x <- edu19 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != edu19comp2())
      
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "edu19municomp2",
        choices = c("Selecione um município", choices),
        session
      )
    })
    ## Título
    output$edu19txt2 <- renderText({
      t192()
    })
    output$edu19graf2 <- renderEcharts4r({
      req(input$edu19municomp2)
      if (input$edu19municomp2 == "Selecione um município") {
        a <- edu19 %>%
          filter(localidade == input$edu19muni,
                 categoria == "Total")
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f17701",
            name = input$edu19muni,
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
      } else {
        a <- edu19 %>%
          filter(localidade == input$edu19muni,
                 categoria == "Total")
        b <- edu19 %>%
          filter(localidade == input$edu19municomp2,
                 categoria == "Total")
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$edu19muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$edu19municomp2,
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
            name = "Quantidade",
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
      }
    })
    ## Tabela - Estabelecimentos de Ensino Médio----
    output$edu19txt3 <- renderText({
      t193()
    })
    output$edu19tab <- renderReactable({
      x <- edu19 %>%
        filter(localidade == input$edu19muni) %>%
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
          ano = colDef(name = "Ano")
        ),
        defaultColDef =
          colDef(
            footerStyle = list(fontWeight = "bold"),
            na = "-",
            format = colFormat(separators = T),
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
    ## Tabela - Estabelecimentos de Ensino Médio----
    output$edu19txt4 <- renderText({
      t194()
    })
    output$edu19tab1 <- renderReactable({
      ri <- edu19 %>%
        filter(ano == input$edu19ano2,
               localidade == input$edu19muni) %>%
        pull(ri)
      x <-
        edu19 %>% filter(ano == input$edu19ano2, localidade != "Pará")
      x <- x %>%
        filter(ri == ri) %>%
        select(ri,
               localidade,
               categoria,
               valor) %>%
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
          localidade = colDef(name = "Municípios")),
        defaultColDef =
          colDef(
            footerStyle = list(fontWeight = "bold"),
            na = "-",
            format = colFormat(separators = T),
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
    # 1 - Taxa de Aprovação no Ensino Fundamental por Esfera Administrativa----
    ## Gráfico de Barras - Taxa de Aprovação no Ensino Fundamental----
    # Filtra os dados
    edu1_1 <- reactive({
      req(input$edu1municomp1)
      if (input$edu1municomp1 == "Selecione um município") {
        a <- edu1 %>% filter(
          localidade == input$edu1muni,
          ano == input$edu1ano1,
          categoria != "Aprovação Total"
        )
      } else {
        a <- edu1 %>% filter(
          localidade == input$edu1muni,
          ano == input$edu1ano1,
          categoria != "Aprovação Total"
        )
        b <-
          edu1 %>% filter(
            localidade == input$edu1municomp1,
            ano == input$edu1ano1,
            categoria != "Aprovação Total"
          )
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu1_1(), {
      downset_Server("edu1_1", edu1_1(), t11())
    })
    ## Gráfico - Taxa de Aprovação no Ensino Fundamental----
    # Filtra os dados
    edu1_2 <- reactive({
      req(input$edu1municomp2)
      if (input$edu1municomp2 == "Selecione um município") {
        a <- edu1 %>% filter(localidade == input$edu1muni,
                             categoria == "Aprovação Total")
      } else {
        a <- edu1 %>%
          filter(localidade == input$edu1muni,
                 categoria == "Aprovação Total")
        b <- edu1 %>%
          filter(localidade == input$edu1municomp2,
                 categoria == "Aprovação Total")
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu1_2(), {
      downset_Server("edu1_2", edu1_2(), t12())
    })
    ## Tabela - Taxa de Aprovação no Ensino Fundamental----
    # Filtra os dados
    edu1_3 <- reactive({
      x <- edu1 %>%
        filter(localidade == input$edu1muni)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu1_3(), {
      downset_Server("edu1_3", edu1_3(), t13())
    })
    ## Tabela - Taxa de Aprovação no Ensino Fundamental----
    # Filtra os dados
    edu1_4 <- reactive({
      ri <- edu1 %>%
        filter(ano == input$edu1ano2, localidade == input$edu1muni) %>%
        pull(ri)
      x <- edu1 %>%
        filter(ano == input$edu1ano2,
               localidade != "Pará",
               ri == ri)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu1_4(), {
      downset_Server("edu1_4", edu1_4(), t14())
    })
    
    # 2 - Taxa de Aprovação no Ensino Médio por Esfera Administrativa----
    ## Gráfico de Barras - Taxa de Aprovação no Ensino Médio----
    # Filtra os dados
    edu2_1 <- reactive({
      req(input$edu2municomp1)
      if (input$edu2municomp1 == "Selecione um município") {
        a <- edu2 %>% filter(
          localidade == input$edu2muni,
          ano == input$edu2ano1,
          categoria != "Aprovação Total"
        )
      } else {
        a <- edu2 %>% filter(
          localidade == input$edu2muni,
          ano == input$edu2ano1,
          categoria != "Aprovação Total"
        )
        b <-
          edu2 %>% filter(
            localidade == input$edu2municomp1,
            ano == input$edu2ano1,
            categoria != "Aprovação Total"
          )
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu2_1(), {
      downset_Server("edu2_1", edu2_1(), t21())
    })
    ## Gráfico - Taxa de Aprovação no Ensino Médio----
    # Filtra os dados
    edu2_2 <- reactive({
      req(input$edu2municomp2)
      if (input$edu2municomp2 == "Selecione um município") {
        a <- edu2 %>% filter(localidade == input$edu2muni,
                             categoria == "Aprovação Total")
      } else {
        a <- edu2 %>% filter(localidade == input$edu2muni,
                             categoria == "Aprovação Total")
        b <-
          edu2 %>% filter(localidade == input$edu2municomp2,
                          categoria == "Aprovação Total")
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu2_2(), {
      downset_Server("edu2_2", edu2_2(), t22())
    })
    ## Tabela - Taxa de Aprovação no Ensino Médio----
    # Filtra os dados
    edu2_3 <- reactive({
      x <- edu2 %>%
        filter(localidade == input$edu2muni)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu2_3(), {
      downset_Server("edu2_3", edu2_3(), t23())
    })
    ## Tabela - Taxa de Aprovação no Ensino Médio----
    # Filtra os dados
    edu2_4 <- reactive({
      ri <- edu2 %>%
        filter(ano == input$edu2ano2, localidade == input$edu2muni) %>%
        pull(ri)
      x <- edu2 %>%
        filter(ano == input$edu2ano2,
               localidade != "Pará",
               ri == ri)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu2_4(), {
      downset_Server("edu2_4", edu2_4(), t24())
    })
    
    # 3 - Taxa de Reprovação no Ensino Fundamental por Esfera Administrativa----
    ## Gráfico de Barras - Taxa de Reprovação no Ensino Fundamental----
    # Filtra os dados
    edu3_1 <- reactive({
      req(input$edu3municomp1)
      if (input$edu3municomp1 == "Selecione um município") {
        a <- edu3 %>% filter(
          localidade == input$edu3muni,
          ano == input$edu3ano1,
          categoria != "Reprovação Total"
        )
      } else {
        a <- edu3 %>% filter(
          localidade == input$edu3muni,
          ano == input$edu3ano1,
          categoria != "Reprovação Total"
        )
        b <-
          edu3 %>% filter(
            localidade == input$edu3municomp1,
            ano == input$edu3ano1,
            categoria != "Reprovação Total"
          )
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu3_1(), {
      downset_Server("edu3_1", edu3_1(), t31())
    })
    ## Gráfico - Taxa de Reprovação no Ensino Fundamental----
    # Filtra os dados
    edu3_2 <- reactive({
      req(input$edu3municomp2)
      if (input$edu3municomp2 == "Selecione um município") {
        a <- edu3 %>% filter(localidade == input$edu3muni,
                             categoria == "Reprovação Total")
      } else {
        a <- edu3 %>% filter(localidade == input$edu3muni,
                             categoria == "Reprovação Total")
        b <-
          edu3 %>% filter(localidade == input$edu3municomp2,
                          categoria == "Reprovação Total")
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu3_2(), {
      downset_Server("edu3_2", edu3_2(), t32())
    })
    ## Tabela - Taxa de Reprovação no Ensino Fundamental----
    # Filtra os dados
    edu3_3 <- reactive({
      x <- edu3 %>%
        filter(localidade == input$edu3muni)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu3_3(), {
      downset_Server("edu3_3", edu3_3(), t33())
    })
    ## Tabela - Taxa de Reprovação no Ensino Fundamental----
    # Filtra os dados
    edu3_4 <- reactive({
      ri <- edu3 %>%
        filter(ano == input$edu3ano2, localidade == input$edu3muni) %>%
        pull(ri)
      x <- edu3 %>%
        filter(ano == input$edu3ano2,
               localidade != "Pará",
               ri == ri)
      
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu3_4(), {
      downset_Server("edu3_4", edu3_4(), t34())
    })
    
    # 4 - Taxa de Reprovação no Ensino Médio por Esfera Administrativa----
    ## Gráfico de Barras - Taxa de Reprovação no Ensino Médio----
    # Filtra os dados
    edu4_1 <- reactive({
      req(input$edu4municomp1)
      if (input$edu4municomp1 == "Selecione um município") {
        a <- edu4 %>% filter(
          localidade == input$edu4muni,
          ano == input$edu4ano1,
          categoria != "Reprovação Total"
        )
      } else {
        a <- edu4 %>% filter(
          localidade == input$edu4muni,
          ano == input$edu4ano1,
          categoria != "Reprovação Total"
        )
        b <-
          edu4 %>% filter(
            localidade == input$edu4municomp1,
            ano == input$edu4ano1,
            categoria != "Reprovação Total"
          )
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu4_1(), {
      downset_Server("edu4_1", edu4_1(), t41())
    })
    ## Gráfico - Taxa de Reprovação no Ensino Médio----
    # Filtra os dados
    edu4_2 <- reactive({
      req(input$edu4municomp2)
      if (input$edu4municomp2 == "Selecione um município") {
        a <- edu4 %>% filter(localidade == input$edu4muni,
                             categoria == "Reprovação Total")
      } else {
        a <- edu4 %>% filter(localidade == input$edu4muni,
                             categoria == "Reprovação Total")
        b <-
          edu4 %>% filter(localidade == input$edu4municomp2,
                          categoria == "Reprovação Total")
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu4_2(), {
      downset_Server("edu4_2", edu4_2(), t42())
    })
    ## Tabela - Taxa de Reprovação no Ensino Médio----
    # Filtra os dados
    edu4_3 <- reactive({
      x <- edu4 %>%
        filter(localidade == input$edu4muni)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu4_3(), {
      t <- "edu4_3"
      downset_Server("edu4_3", edu4_3(), t43())
    })
    ## Tabela - Taxa de Reprovação no Ensino Médio----
    # Filtra os dados
    edu4_4 <- reactive({
      ri <- edu4 %>%
        filter(ano == input$edu4ano2, localidade == input$edu4muni) %>%
        pull(ri)
      x <- edu4 %>%
        filter(ano == input$edu4ano2,
               localidade != "Pará",
               ri == ri)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu4_4(), {
      downset_Server("edu4_4", edu4_4(), t44())
    })
    
    # 5 - Taxa de Abandono no Ensino Fundamental por Esfera Administrativa----
    ## Gráfico de Barras - Taxa de Abandono no Ensino Fundamental----
    # Filtra os dados
    edu5_1 <- reactive({
      req(input$edu5municomp1)
      if (input$edu5municomp1 == "Selecione um município") {
        a <- edu5 %>% filter(
          localidade == input$edu5muni,
          ano == input$edu5ano1,
          categoria != "Evasão Total"
        )
      } else {
        a <- edu5 %>% filter(
          localidade == input$edu5muni,
          ano == input$edu5ano1,
          categoria != "Evasão Total"
        )
        b <-
          edu5 %>% filter(
            localidade == input$edu5municomp1,
            ano == input$edu5ano1,
            categoria != "Evasão Total"
          )
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu5_1(), {
      downset_Server("edu5_1", edu5_1(), t51())
    })
    ## Gráfico - Taxa de Abandono no Ensino Fundamental----
    # Filtra os dados
    edu5_2 <- reactive({
      req(input$edu5municomp2)
      if (input$edu5municomp2 == "Selecione um município") {
        a <- edu5 %>% filter(localidade == input$edu5muni,
                             categoria == "Evasão Total")
      } else {
        a <- edu5 %>% filter(localidade == input$edu5muni,
                             categoria == "Evasão Total")
        b <-
          edu5 %>% filter(localidade == input$edu5municomp2,
                          categoria == "Evasão Total")
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu5_2(), {
      downset_Server("edu5_2", edu5_2(), t52())
    })
    ## Tabela - Taxa de Abandono no Ensino Fundamental----
    # Filtra os dados
    edu5_3 <- reactive({
      x <- edu5 %>%
        filter(localidade == input$edu5muni)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu5_3(), {
      downset_Server("edu5_3", edu5_3(), t53())
    })
    ## Tabela - Taxa de Abandono no Ensino Fundamental----
    # Filtra os dados
    edu5_4 <- reactive({
      ri <- edu5 %>%
        filter(ano == input$edu5ano2, localidade == input$edu5muni) %>%
        pull(ri)
      x <- edu5 %>%
        filter(ano == input$edu5ano2,
               localidade != "Pará",
               ri == ri)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu5_4(), {
      downset_Server("edu5_4", edu5_4(), t54())
    })
    
    # 6 - Taxa de Abandono no Ensino Médio por Esfera Administrativa----
    ## Gráfico de Barras - Taxa de Abandono no Ensino Médio----
    # Filtra os dados
    edu6_1 <- reactive({
      req(input$edu6municomp1)
      if (input$edu6municomp1 == "Selecione um município") {
        a <- edu6 %>% filter(
          localidade == input$edu6muni,
          ano == input$edu6ano1,
          categoria != "Evasão Total"
        )
      } else {
        a <- edu6 %>% filter(
          localidade == input$edu6muni,
          ano == input$edu6ano1,
          categoria != "Evasão Total"
        )
        b <-
          edu6 %>% filter(
            localidade == input$edu6municomp1,
            ano == input$edu6ano1,
            categoria != "Evasão Total"
          )
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu6_1(), {
      downset_Server("edu6_1", edu6_1(), t61())
    })
    ## Gráfico - Taxa de Abandono no Ensino Médio----
    # Filtra os dados
    edu6_2 <- reactive({
      req(input$edu6municomp2)
      if (input$edu6municomp2 == "Selecione um município") {
        a <- edu6 %>% filter(localidade == input$edu6muni,
                             categoria == "Evasão Total")
      } else {
        a <- edu6 %>% filter(localidade == input$edu6muni,
                             categoria == "Evasão Total")
        b <-
          edu6 %>% filter(localidade == input$edu6municomp2,
                          categoria == "Evasão Total")
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu6_2(), {
      downset_Server("edu6_2", edu6_2(), t62())
    })
    ## Tabela - Taxa de Abandono no Ensino Médio----
    # Filtra os dados
    edu6_3 <- reactive({
      x <- edu6 %>%
        filter(localidade == input$edu6muni)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu6_3(), {
      downset_Server("edu6_3", edu6_3(), t63())
    })
    ## Tabela - Taxa de Abandono no Ensino Médio----
    # Filtra os dados
    edu6_4 <- reactive({
      ri <- edu6 %>%
        filter(ano == input$edu6ano2, localidade == input$edu6muni) %>%
        pull(ri)
      x <- edu6 %>%
        filter(ano == input$edu6ano2,
               localidade != "Pará",
               ri == ri)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu6_4(), {
      downset_Server("edu6_4", edu6_4(), t64())
    })
    
    # 7 - Distorção Idade-Série Total por Nível de Ensino----
    ## Gráfico de Barras - Distorção Idade-Série Total por Nível de Ensino----
    # Filtra os dados
    edu7_1 <- reactive({
      req(input$edu7municomp1)
      if (input$edu7municomp1 == "Selecione um município") {
        a <- edu7 %>% filter(localidade == input$edu7muni,
                             ano == input$edu7ano1)
      } else {
        a <- edu7 %>% filter(localidade == input$edu7muni,
                             ano == input$edu7ano1)
        b <-
          edu7 %>% filter(localidade == input$edu7municomp1,
                          ano == input$edu7ano1)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu7_1(), {
      downset_Server("edu7_1", edu7_1(), t71())
    })
    ## Tabela - Distorção Idade-Série Total por Nível de Ensino----
    # Filtra os dados
    edu7_2 <- reactive({
      x <- edu7 %>%
        filter(localidade == input$edu7muni)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu7_2(), {
      downset_Server("edu7_2", edu7_2(), t72())
    })
    ## Tabela - Distorção Idade-Série Total por Nível de Ensino----
    # Filtra os dados
    edu7_3 <- reactive({
      ri <- edu7 %>%
        filter(ano == input$edu7ano2, localidade == input$edu7muni) %>%
        pull(ri)
      x <- edu7 %>%
        filter(ano == input$edu7ano2,
               localidade != "Pará",
               ri == ri)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu7_3(), {
      downset_Server("edu7_3", edu7_3(), t73())
    })
    
    # 8 -  IDEB - Escola pública - 5ª ano (séries iniciais)----
    ## Gráfico - IDEB - Escola pública - 5ª ano (séries iniciais)----
    # Filtra os dados
    edu8_1 <- reactive({
      req(input$edu8municomp1)
      if (input$edu8municomp1 == "Selecione um município") {
        a <- edu8 %>% filter(localidade == input$edu8muni)
      } else {
        a <- edu8 %>% filter(localidade == input$edu8muni)
        b <-
          edu8 %>% filter(localidade == input$edu8municomp1)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu8_1(), {
      downset_Server("edu8_1", edu8_1(), t81())
    })
    ## Tabela - IDEB - Escola pública - 5ª ano (séries iniciais)----
    # Filtra os dados
    edu8_2 <- reactive({
      ri <- edu8 %>%
        filter(ano == input$edu8ano, localidade == input$edu8muni) %>%
        pull(ri)
      x <- edu8 %>%
        filter(ano == input$edu8ano,
               localidade != "Pará",
               ri == ri) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu8_2(), {
      downset_Server("edu8_2", edu8_2(), t82())
    })
    
    # 9 -  IDEB - Escola pública - 9ª ano (séries finais)----
    ## Gráfico - IDEB - Escola pública - 9ª ano (séries finais)----
    # Filtra os dados
    edu9_1 <- reactive({
      req(input$edu9municomp1)
      if (input$edu9municomp1 == "Selecione um município") {
        a <- edu9 %>% filter(localidade == input$edu9muni)
      } else {
        a <- edu9 %>% filter(localidade == input$edu9muni)
        b <-
          edu9 %>% filter(localidade == input$edu9municomp1)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu9_1(), {
      downset_Server("edu9_1", edu9_1(), t91())
    })
    ## Tabela - IDEB - Escola pública - 9ª ano (séries finais)----
    # Filtra os dados
    edu9_2 <- reactive({
      ri <- edu9 %>%
        filter(ano == input$edu9ano, localidade == input$edu9muni) %>%
        pull(ri)
      x <- edu9 %>%
        filter(ano == input$edu9ano,
               localidade != "Pará",
               ri == ri) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu9_2(), {
      downset_Server("edu9_2", edu9_2(), t92())
    })
    
    # 10 - Número de Matrículas no Ensino Pré-Escolar por Esfera Administrativa----
    ## Gráfico - Número de Matrículas no Ensino Pré-Escolar----
    # Filtra os dados
    edu10_1 <- reactive({
      req(input$edu10municomp1)
      if (input$edu10municomp1 == "Selecione um município") {
        a <- edu10 %>% filter(
          localidade == input$edu10muni,
          ano == input$edu10ano1,
          categoria != "Total"
        )
      } else {
        a <- edu10 %>% filter(
          localidade == input$edu10muni,
          ano == input$edu10ano1,
          categoria != "Total"
        )
        b <-
          edu10 %>% filter(
            localidade == input$edu10municomp1,
            ano == input$edu10ano1,
            categoria != "Total"
          )
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu10_1(), {
      downset_Server("edu10_1", edu10_1(), t101())
    })
    ## Gráfico - Número de Matrículas no Ensino Pré-Escolar----
    # Filtra os dados
    edu10_2 <- reactive({
      req(input$edu10municomp2)
      if (input$edu10municomp2 == "Selecione um município") {
        a <- edu10 %>% filter(localidade == input$edu10muni,
                              categoria == "Total")
      } else {
        a <- edu10 %>% filter(localidade == input$edu10muni,
                              categoria == "Total")
        b <-
          edu10 %>% filter(localidade == input$edu10municomp2,
                           categoria == "Total")
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu10_2(), {
      downset_Server("edu10_2", edu10_2(), t102())
    })
    ## Tabela - Número de Matrículas no Ensino Pré-Escolar----
    # Filtra os dados
    edu10_3 <- reactive({
      x <- edu10 %>%
        filter(localidade == input$edu10muni)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu10_3(), {
      downset_Server("edu10_3", edu10_3(), t103())
    })
    ## Tabela - Número de Matrículas no Ensino Pré-Escolar----
    # Filtra os dados
    edu10_4 <- reactive({
      ri <- edu10 %>%
        filter(ano == input$edu10ano2,
               localidade == input$edu10muni) %>%
        pull(ri)
      x <- edu10 %>%
        filter(ano == input$edu10ano2,
               localidade != "Pará",
               ri == ri)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu10_4(), {
      downset_Server("edu10_4", edu10_4(), t104())
    })
    
    # 11 - Número de Matrículas no Ensino Fundamental por Esfera Administrativa----
    ## Gráfico - Número de Matrículas no Ensino Fundamental----
    # Filtra os dados
    edu11_1 <- reactive({
      req(input$edu11municomp1)
      if (input$edu11municomp1 == "Selecione um município") {
        a <- edu11 %>% filter(
          localidade == input$edu11muni,
          ano == input$edu11ano1,
          categoria != "Total"
        )
      } else {
        a <- edu11 %>% filter(
          localidade == input$edu11muni,
          ano == input$edu11ano1,
          categoria != "Total"
        )
        b <-
          edu11 %>% filter(
            localidade == input$edu11municomp1,
            ano == input$edu11ano1,
            categoria != "Total"
          )
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu11_1(), {
      downset_Server("edu11_1", edu11_1(), t111())
    })
    ## Gráfico - Número de Matrículas no Ensino Fundamental----
    # Filtra os dados
    edu11_2 <- reactive({
      req(input$edu11municomp2)
      if (input$edu11municomp2 == "Selecione um município") {
        a <- edu11 %>% filter(localidade == input$edu11muni,
                              categoria == "Total")
      } else {
        a <- edu11 %>% filter(localidade == input$edu11muni,
                              categoria == "Total")
        b <-
          edu11 %>% filter(localidade == input$edu11municomp2,
                           categoria == "Total")
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu11_2(), {
      downset_Server("edu11_2", edu11_2(), t112())
    })
    ## Tabela - Número de Matrículas no Ensino Fundamental----
    # Filtra os dados
    edu11_3 <- reactive({
      x <- edu11 %>%
        filter(localidade == input$edu11muni)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu11_3(), {
      downset_Server("edu11_3", edu11_3(), t113())
    })
    ## Tabela - Número de Matrículas no Ensino Fundamental----
    # Filtra os dados
    edu11_4 <- reactive({
      ri <- edu11 %>%
        filter(ano == input$edu11ano2,
               localidade == input$edu11muni) %>%
        pull(ri)
      x <- edu11 %>%
        filter(ano == input$edu11ano2,
               localidade != "Pará",
               ri == ri)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu11_4(), {
      downset_Server("edu11_4", edu11_4(), t114())
    })
    
    # 12 - Número de Matrículas no Ensino Médio por Esfera Administrativa----
    ## Gráfico - Número de Matrículas no Ensino Médio----
    # Filtra os dados
    edu12_1 <- reactive({
      req(input$edu12municomp1)
      if (input$edu12municomp1 == "Selecione um município") {
        a <- edu12 %>% filter(
          localidade == input$edu12muni,
          ano == input$edu12ano1,
          categoria != "Total Médio"
        )
      } else {
        a <- edu12 %>% filter(
          localidade == input$edu12muni,
          ano == input$edu12ano1,
          categoria != "Total Médio"
        )
        b <-
          edu12 %>% filter(
            localidade == input$edu12municomp1,
            ano == input$edu12ano1,
            categoria != "Total Médio"
          )
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu12_1(), {
      downset_Server("edu12_1", edu12_1(), t121())
    })
    ## Gráfico - Número de Matrículas no Ensino Médio----
    # Filtra os dados
    edu12_2 <- reactive({
      req(input$edu12municomp2)
      if (input$edu12municomp2 == "Selecione um município") {
        a <- edu12 %>% filter(localidade == input$edu12muni,
                              categoria == "Total Médio")
      } else {
        a <- edu12 %>% filter(localidade == input$edu12muni,
                              categoria == "Total Médio")
        b <-
          edu12 %>% filter(localidade == input$edu12municomp2,
                           categoria == "Total Médio")
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu12_2(), {
      downset_Server("edu12_2", edu12_2(), t122())
    })
    ## Tabela - Número de Matrículas no Ensino Médio----
    # Filtra os dados
    edu12_3 <- reactive({
      x <- edu12 %>%
        filter(localidade == input$edu12muni)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu12_3(), {
      downset_Server("edu12_3", edu12_3(), t123())
    })
    ## Tabela - Número de Matrículas no Ensino Médio----
    # Filtra os dados
    edu12_4 <- reactive({
      ri <- edu12 %>%
        filter(ano == input$edu12ano2,
               localidade == input$edu12muni) %>%
        pull(ri)
      x <- edu12 %>%
        filter(ano == input$edu12ano2,
               localidade != "Pará",
               ri == ri)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu12_4(), {
      downset_Server("edu12_4", edu12_4(), t124())
    })
    
    # 13 - Média de Alunos por Turma por Nível de Ensino----
    ## Gráfico - Média de Alunos por Turma por Nível de Ensino----
    # Filtra os dados
    edu13_1 <- reactive({
      req(input$edu13municomp1)
      if (input$edu13municomp1 == "Selecione um município") {
        a <- edu13 %>%
          filter(localidade == input$edu13muni,
                 ano == input$edu13ano1)
      } else {
        a <- edu13 %>%
          filter(localidade == input$edu13muni,
                 ano == input$edu13ano1)
        b <- edu13 %>%
          filter(localidade == input$edu13municomp1,
                 ano == input$edu13ano1)
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu13_1(), {
      downset_Server("edu13_1", edu13_1(), t131())
    })
    ## Tabela - Média de Alunos por Turma por Nível de Ensino----
    # Filtra os dados
    edu13_2 <- reactive({
      x <- edu13 %>%
        filter(localidade == input$edu13muni)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu13_2(), {
      downset_Server("edu13_2", edu13_2(), t132())
    })
    ## Tabela - Média de Alunos por Turma por Nível de Ensino----
    # Filtra os dados
    edu13_3 <- reactive({
      ri <- edu13 %>%
        filter(ano == input$edu13ano2,
               localidade == input$edu13muni) %>%
        pull(ri)
      x <- edu13 %>%
        filter(ano == input$edu13ano2,
               localidade != "Pará",
               ri == ri)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu13_3(), {
      downset_Server("edu13_3", edu13_3(), t133())
    })
    
    # 14 - Número de Docentes no Ensino Pré-escolar por Esfera Administrativa----
    ## Gráfico - Número de Docentes no Ensino Pré-escolar----
    # Filtra os dados
    edu14_1 <- reactive({
      req(input$edu14municomp1)
      if (input$edu14municomp1 == "Selecione um município") {
        a <- edu14 %>%
          filter(
            localidade == input$edu14muni,
            ano == input$edu14ano1,
            categoria != "Total"
          )
      } else {
        a <- edu14 %>%
          filter(
            localidade == input$edu14muni,
            ano == input$edu14ano1,
            categoria != "Total"
          )
        b <- edu14 %>%
          filter(
            localidade == input$edu14municomp1,
            ano == input$edu14ano1,
            categoria != "Total"
          )
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu14_1(), {
      downset_Server("edu14_1", edu14_1(), t141())
    })
    ## Gráfico - Número de Docentes no Ensino Pré-escolar----
    # Filtra os dados
    edu14_2 <- reactive({
      req(input$edu14municomp2)
      if (input$edu14municomp2 == "Selecione um município") {
        a <- edu14 %>% filter(localidade == input$edu14muni,
                              categoria == "Total")
      } else {
        a <- edu14 %>% filter(localidade == input$edu14muni,
                              categoria == "Total")
        b <-
          edu14 %>% filter(localidade == input$edu14municomp2,
                           categoria == "Total")
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu14_2(), {
      downset_Server("edu14_2", edu14_2(), t142())
    })
    ## Tabela - Número de Docentes no Ensino Pré-escolar----
    # Filtra os dados
    edu14_3 <- reactive({
      x <- edu14 %>%
        filter(localidade == input$edu14muni)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu14_3(), {
      downset_Server("edu14_3", edu14_3(), t143())
    })
    ## Tabela - Número de Docentes no Ensino Pré-escolar----
    # Filtra os dados
    edu14_4 <- reactive({
      ri <- edu14 %>%
        filter(ano == input$edu14ano2,
               localidade == input$edu14muni) %>%
        pull(ri)
      x <- edu14 %>%
        filter(ano == input$edu14ano2,
               localidade != "Pará",
               ri == ri)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu14_4(), {
      downset_Server("edu14_4", edu14_4(), t144())
    })
    
    # 15 - Número de Docentes no Ensino Fundamental por Esfera Administrativa----
    ## Gráfico - Número de Docentes no Ensino Fundamental----
    # Filtra os dados
    edu15_1 <- reactive({
      req(input$edu15municomp1)
      if (input$edu15municomp1 == "Selecione um município") {
        a <- edu15 %>%
          filter(
            localidade == input$edu15muni,
            ano == input$edu15ano1,
            categoria != "Total"
          )
      } else {
        a <- edu15 %>% filter(
          localidade == input$edu15muni,
          ano == input$edu15ano1,
          categoria != "Total"
        )
        b <-
          edu15 %>% filter(
            localidade == input$edu15municomp1,
            ano == input$edu15ano1,
            categoria != "Total"
          )
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu15_1(), {
      downset_Server("edu15_1", edu15_1(), t151())
    })
    ## Gráfico - Número de Docentes no Ensino Fundamental----
    # Filtra os dados
    edu15_2 <- reactive({
      req(input$edu15municomp2)
      if (input$edu15municomp2 == "Selecione um município") {
        a <- edu15 %>% filter(localidade == input$edu15muni,
                              categoria == "Total")
      } else {
        a <- edu15 %>% filter(localidade == input$edu15muni,
                              categoria == "Total")
        b <-
          edu15 %>% filter(localidade == input$edu15municomp2,
                           categoria == "Total")
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu15_2(), {
      downset_Server("edu15_2", edu15_2(), t152())
    })
    ## Tabela - Número de Docentes no Ensino Fundamental----
    # Filtra os dados
    edu15_3 <- reactive({
      x <- edu15 %>%
        filter(localidade == input$edu15muni)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu15_3(), {
      downset_Server("edu15_3", edu15_3(), t153())
    })
    ## Tabela - Número de Docentes no Ensino Fundamental----
    # Filtra os dados
    edu15_4 <- reactive({
      ri <- edu15 %>%
        filter(ano == input$edu15ano2,
               localidade == input$edu15muni) %>%
        pull(ri)
      x <- edu15 %>%
        filter(ano == input$edu15ano2,
               localidade != "Pará",
               ri == ri)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu15_4(), {
      downset_Server("edu15_4", edu15_4(), t154())
    })
    
    # 16 - Número de Docentes no Ensino Médio por Esfera Administrativa----
    ## Gráfico - Número de Docentes no Ensino Médio----
    # Filtra os dados
    edu16_1 <- reactive({
      req(input$edu16municomp1)
      if (input$edu16municomp1 == "Selecione um município") {
        a <- edu16 %>% filter(
          localidade == input$edu16muni,
          ano == input$edu16ano1,
          categoria != "Total"
        )
      } else {
        a <- edu16 %>% filter(
          localidade == input$edu16muni,
          ano == input$edu16ano1,
          categoria != "Total"
        )
        b <-
          edu16 %>% filter(
            localidade == input$edu16municomp1,
            ano == input$edu16ano1,
            categoria != "Total"
          )
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu16_1(), {
      downset_Server("edu16_1", edu16_1(), t161())
    })
    ## Gráfico - Número de Docentes no Ensino Médio----
    # Filtra os dados
    edu16_2 <- reactive({
      req(input$edu16municomp2)
      if (input$edu16municomp2 == "Selecione um município") {
        a <- edu16 %>%
          filter(localidade == input$edu16muni,
                 categoria == "Total")
      } else {
        a <- edu16 %>% filter(localidade == input$edu16muni,
                              categoria == "Total")
        b <-
          edu16 %>% filter(localidade == input$edu16municomp2,
                           categoria == "Total")
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu16_2(), {
      downset_Server("edu16_2", edu16_2(), t162())
    })
    ## Tabela - Número de Docentes no Ensino Médio----
    # Filtra os dados
    edu16_3 <- reactive({
      x <- edu16 %>%
        filter(localidade == input$edu16muni)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu16_3(), {
      downset_Server("edu16_3", edu16_3(), t163())
    })
    ## Tabela - Número de Docentes no Ensino Médio----
    # Filtra os dados
    edu16_4 <- reactive({
      ri <- edu16 %>%
        filter(ano == input$edu16ano2,
               localidade == input$edu16muni) %>%
        pull(ri)
      x <- edu16 %>%
        filter(ano == input$edu16ano2,
               localidade != "Pará",
               ri == ri)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu16_4(), {
      downset_Server("edu16_4", edu16_4(), t164())
    })
    
    # 17 - Estabelecimentos de Pré-Escola por Esfera Administrativa----
    ## Gráfico - Estabelecimentos de Pré-Escola----
    # Filtra os dados
    edu17_1 <- reactive({
      req(input$edu17municomp1)
      if (input$edu17municomp1 == "Selecione um município") {
        a <- edu17 %>% filter(
          localidade == input$edu17muni,
          ano == input$edu17ano1,
          categoria != "Total"
        )
      } else {
        a <- edu17 %>% filter(
          localidade == input$edu17muni,
          ano == input$edu17ano1,
          categoria != "Total"
        )
        b <-
          edu17 %>% filter(
            localidade == input$edu17municomp1,
            ano == input$edu17ano1,
            categoria != "Total"
          )
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu17_1(), {
      downset_Server("edu17_1", edu17_1(), t171())
    })
    ## Gráfico - Estabelecimentos de Pré-Escola----
    # Filtra os dados
    edu17_2 <- reactive({
      req(input$edu17municomp2)
      if (input$edu17municomp2 == "Selecione um município") {
        a <- edu17 %>%
          filter(localidade == input$edu17muni,
                 categoria == "Total")
      } else {
        a <- edu17 %>% filter(localidade == input$edu17muni,
                              categoria == "Total")
        b <-
          edu17 %>% filter(localidade == input$edu17municomp2,
                           categoria == "Total")
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu17_2(), {
      downset_Server("edu17_2", edu17_2(), t172())
    })
    ## Tabela - Estabelecimentos de Pré-Escola----
    # Filtra os dados
    edu17_3 <- reactive({
      x <- edu17 %>%
        filter(localidade == input$edu17muni)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu17_3(), {
      downset_Server("edu17_3", edu17_3(), t173())
    })
    ## Tabela - Estabelecimentos de Pré-Escola----
    # Filtra os dados
    edu17_4 <- reactive({
      ri <- edu17 %>%
        filter(ano == input$edu17ano2,
               localidade == input$edu17muni) %>%
        pull(ri)
      x <- edu17 %>%
        filter(ano == input$edu17ano2,
               localidade != "Pará",
               ri == ri)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu17_4(), {
      downset_Server("edu17_4", edu17_4(), t174())
    })
    
    # 18 - Estabelecimentos de Ensino Fundamental por Esfera Administrativa----
    ## Gráfico - Estabelecimentos de Ensino Fundamental----
    # Filtra os dados
    edu18_1 <- reactive({
      req(input$edu18municomp1)
      if (input$edu18municomp1 == "Selecione um município") {
        a <- edu18 %>% filter(
          localidade == input$edu18muni,
          ano == input$edu18ano1,
          categoria != "Total"
        )
      } else {
        a <- edu18 %>% filter(
          localidade == input$edu18muni,
          ano == input$edu18ano1,
          categoria != "Total"
        )
        b <-
          edu18 %>% filter(
            localidade == input$edu18municomp1,
            ano == input$edu18ano1,
            categoria != "Total"
          )
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu18_1(), {
      downset_Server("edu18_1", edu18_1(), t181())
    })
    ## Gráfico - Estabelecimentos de Ensino Fundamental----
    # Filtra os dados
    edu18_2 <- reactive({
      req(input$edu18municomp2)
      if (input$edu18municomp2 == "Selecione um município") {
        a <- edu18 %>%
          filter(localidade == input$edu18muni,
                 categoria == "Total")
      } else {
        a <- edu18 %>% filter(localidade == input$edu18muni,
                              categoria == "Total")
        b <-
          edu18 %>% filter(localidade == input$edu18municomp2,
                           categoria == "Total")
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu18_2(), {
      downset_Server("edu18_2", edu18_2(), t182())
    })
    ## Tabela - Estabelecimentos de Ensino Fundamental----
    # Filtra os dados
    edu18_3 <- reactive({
      x <- edu18 %>%
        filter(localidade == input$edu18muni)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu18_3(), {
      downset_Server("edu18_3", edu18_3(), t183())
    })
    ## Tabela - Estabelecimentos de Ensino Fundamental----
    # Filtra os dados
    edu18_4 <- reactive({
      ri <- edu18 %>%
        filter(ano == input$edu18ano2,
               localidade == input$edu18muni) %>%
        pull(ri)
      x <- edu18 %>%
        filter(ano == input$edu18ano2,
               localidade != "Pará",
               ri == ri)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu18_4(), {
      downset_Server("edu18_4", edu18_4(), t184())
    })
    
    # 19 - Estabelecimentos de Ensino Médio por Esfera Administrativa----
    ## Gráfico - Estabelecimentos de Ensino Médio----
    # Filtra os dados
    edu19_1 <- reactive({
      req(input$edu19municomp1)
      if (input$edu19municomp1 == "Selecione um município") {
        a <- edu19 %>%
          filter(
            localidade == input$edu19muni,
            ano == input$edu19ano1,
            categoria != "Total"
          )
      } else {
        a <- edu19 %>%
          filter(
            localidade == input$edu19muni,
            ano == input$edu19ano1,
            categoria != "Total"
          )
        b <- edu19 %>%
          filter(
            localidade == input$edu19municomp1,
            ano == input$edu19ano1,
            categoria != "Total"
          )
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu19_1(), {
      downset_Server("edu19_1", edu19_1(), t191())
    })
    ## Gráfico - Estabelecimentos de Ensino Médio----
    # Filtra os dados
    edu19_2 <- reactive({
      req(input$edu19municomp2)
      if (input$edu19municomp2 == "Selecione um município") {
        a <- edu19 %>%
          filter(localidade == input$edu19muni,
                 categoria == "Total")
      } else {
        a <- edu19 %>%
          filter(localidade == input$edu19muni,
                 categoria == "Total")
        b <- edu19 %>%
          filter(localidade == input$edu19municomp2,
                 categoria == "Total")
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu19_2(), {
      downset_Server("edu19_2", edu19_2(), t192())
    })
    ## Tabela - Estabelecimentos de Ensino Médio----
    # Filtra os dados
    edu19_3 <- reactive({
      x <- edu19 %>%
        filter(localidade == input$edu19muni)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu19_3(), {
      downset_Server("edu19_3", edu19_3(), t193())
    })
    ## Tabela - Estabelecimentos de Ensino Médio----
    # Filtra os dados
    edu19_4 <- reactive({
      ri <- edu19 %>%
        filter(ano == input$edu19ano2,
               localidade == input$edu19muni) %>%
        pull(ri)
      x <- edu19 %>%
        filter(ano == input$edu19ano2,
               localidade != "Pará",
               ri == ri)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu19_4(), {
      downset_Server("edu19_4", edu19_4(), t194())
    })
  })
}

# Play do Módulo
ui <- dashboardPage(
  header = dashboardHeader(disable =),
  sidebar = dashboardSidebar(),
  body = dashboardBody(fluidPage(
    social_educacao_mp_ui("social_educacao_mp")
  ))
)


server <- function(input, output) {
  social_educacao_mp_Server("social_educacao_mp")
}

shinyApp(ui, server)
