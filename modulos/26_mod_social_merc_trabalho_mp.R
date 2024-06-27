# Funções de módulo de Social - Mercado de Trabalho - Municipal
# Função de UI
social_merc_tab_mp_ui <- function(id) {
  fluidPage(
    # CAMINHO DO ARQUIVO CSS----
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "estilo.css")
    ),
    div(
      class = "navbar_social",
      navbarPage(
        tags$b("Mercado de Trabalho - Municípios"),
        navbarMenu(
          "Indicadores",
          # 1 - Vínculos Empregatícios Total no Emprego Formal----
          tabPanel(
            "Vínculos Empregatícios Total no Emprego Formal",
            panel(
              ## Controle----
              heading =
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Vínculos Empregatícios Total no Emprego Formal"),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "merc1muni"),
                  label = "Município",
                  choices = merc1 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Vínculos Empregatícios Total no Emprego Formal----
              box(
                title = textOutput(NS(id, "merc1txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "merc1municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "merc1graf")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "MTP-RAIS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "merc1_1"))
                )
              ),
              ## Tabela - Vínculos Empregatícios Total no Emprego Formal----
              box(
                title = textOutput(NS(id, "merc1txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "merc1ano"),
                  label = "Ano",
                  choices = sort(unique(merc1[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "merc1tab2"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "MTP-RAIS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "merc1_2"))
                )
              )
            )
          ),

          # 2 - Vínculos Empregatícios no Emprego Formal por Sexo----
          tabPanel(
            "Vínculos Empregatícios no Emprego Formal por Sexo",
            panel(
              ## Controle----
              heading =
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Vínculos Empregatícios no Emprego Formal por Sexo"),
              tags$div(
                class = "seletor2",
                # select Município
                pickerInput(
                  inputId = NS(id, "merc2muni"),
                  label = "Município",
                  choices = merc2 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Vínculos Empregatícios no Emprego Formal por Sexo----
              box(
                title = textOutput(NS(id, "merc2txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                column(
                  2,
                  pickerInput(
                    inputId = NS(id, "merc2municomp"),
                    label = "Comparar Município",
                    choices = NULL,
                    width = "200px",
                    options = list(`none-selected-text` = "Selecione um município")
                  )
                ),
                column(
                  6,
                  pickerInput(
                    inputId = NS(id, "merc2ano1"),
                    label = "Ano",
                    choices = sort(unique(merc2[["ano"]]), decreasing = T),
                    width = "100px"
                  )
                ),
                column(
                  12,
                  withSpinner(
                    echarts4rOutput(NS(id, "merc2graf")),
                    type = 8,
                    color = "#f17701",
                    size = 0.5
                  )
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "MTP-RAIS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "merc2_1"))
                )
              ),
              ## Tabela - Vínculos Empregatícios no Emprego Formal por Sexo----
              box(
                title = textOutput(NS(id, "merc2txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "merc2tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "MTP-RAIS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "merc2_2"))
                )
              ),
              ## Tabela - Vínculos Empregatícios no Emprego Formal por Sexo----
              box(
                title = textOutput(NS(id, "merc2txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "merc2ano"),
                  label = "Ano",
                  choices = sort(unique(merc2[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "merc2tab1"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "MTP-RAIS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "merc2_3"))
                )
              )
            )
          ),
          # 3 - Vínculos Empregatícios no Emprego Formal por Grande Setor (IBGE)----
          tabPanel(
            "Vínculos Empregatícios no Emprego Formal por Grande Setor (IBGE)",
            panel(
              ## Controle----
              heading =
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Vínculos Empregatícios no Emprego Formal por Grande Setor (IBGE)"),
              tags$div(
                class = "seletor1",
                # select Município
                pickerInput(
                  inputId = NS(id, "merc3muni"),
                  label = "Município",
                  choices = merc3 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Vínculos Empregatícios no Emprego Formal por Grande Setor (IBGE)----
              box(
                title = textOutput(NS(id, "merc3txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                column(
                  2,
                  pickerInput(
                    inputId = NS(id, "merc3municomp"),
                    label = "Comparar Município",
                    choices = NULL,
                    width = "200px",
                    options = list(`none-selected-text` = "Selecione um município")
                  )
                ),
                column(
                  6,
                  pickerInput(
                    inputId = NS(id, "merc3ano1"),
                    label = "Ano",
                    choices = sort(unique(merc3[["ano"]]), decreasing = T),
                    width = "100px"
                  )
                ),
                column(
                  12,
                  withSpinner(
                    echarts4rOutput(NS(id, "merc3graf"), height = "450px"),
                    type = 8,
                    color = "#f17701",
                    size = 0.5
                  )
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "MTP-RAIS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "merc3_1"))
                )
              ),
              ## Tabela - Vínculos Empregatícios no Emprego Formal por Grande Setor (IBGE)----
              box(
                title = textOutput(NS(id, "merc3txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "merc3tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "MTP-RAIS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "merc3_2"))
                )
              ),
              ## Tabela - Vínculos Empregatícios no Emprego Formal por Grande Setor (IBGE)----
              box(
                title = textOutput(NS(id, "merc3txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "merc3ano"),
                  label = "Ano",
                  choices = sort(unique(merc3[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "merc3tab1"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "MTP-RAIS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "merc3_3"))
                )
              )
            )
          ),

          # 4 - Vínculos Empregatícios no Emprego Formal por Setor Econômico----
          tabPanel(
            "Vínculos Empregatícios no Emprego Formal por Setor Econômico",
            panel(
              ## Controle----
              heading =
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Vínculos Empregatícios no Emprego Formal por Setor Econômico"),
              tags$div(
                class = "seletor1",
                # select Município
                pickerInput(
                  inputId = NS(id, "merc4muni"),
                  label = "Município",
                  choices = merc4 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Vínculos Empregatícios no Emprego Formal por Setor Econômico----
              box(
                title = textOutput(NS(id, "merc4txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                column(
                  2,
                  pickerInput(
                    inputId = NS(id, "merc4municomp"),
                    label = "Comparar Município",
                    choices = NULL,
                    width = "200px",
                    options = list(`none-selected-text` = "Selecione um município")
                  )
                ),
                column(
                  6,
                  pickerInput(
                    inputId = NS(id, "merc4ano1"),
                    label = "Ano",
                    choices = sort(unique(merc4[["ano"]]), decreasing = T),
                    width = "100px"
                  )
                ),
                column(
                  12,
                  withSpinner(
                    echarts4rOutput(NS(id, "merc4graf"), height = "450px"),
                    type = 8,
                    color = "#f17701",
                    size = 0.5
                  )
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "MTP-RAIS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "merc4_1"))
                )
              ),
              ## Tabela - Vínculos Empregatícios no Emprego Formal por Setor Econômico----
              box(
                title = textOutput(NS(id, "merc4txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "merc4tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "MTP-RAIS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "merc4_2"))
                )
              ),
              ## Tabela - Vínculos Empregatícios no Emprego Formal por Setor Econômico----
              box(
                title = textOutput(NS(id, "merc4txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "merc4ano"),
                  label = "Ano",
                  choices = sort(unique(merc4[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "merc4tab1"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "MTP-RAIS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "merc4_3"))
                )
              )
            )
          ),
          # 5 - Vínculos Empregatícios por Escolaridade do Trabalhador Formal----
          tabPanel(
            "Vínculos Empregatícios por Escolaridade do Trabalhador Formal",
            panel(
              ## Controle----
              heading =
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Vínculos Empregatícios por Escolaridade do Trabalhador Formal"),
              tags$div(
                class = "seletor1",
                # select Município
                pickerInput(
                  inputId = NS(id, "merc5muni"),
                  label = "Município",
                  choices = merc5 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Vínculos Empregatícios por Escolaridade do Trabalhador Formal----
              box(
                title = textOutput(NS(id, "merc5txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                column(
                  2,
                  pickerInput(
                    inputId = NS(id, "merc5municomp"),
                    label = "Comparar Município",
                    choices = NULL,
                    width = "200px",
                    options = list(`none-selected-text` = "Selecione um município")
                  )
                ),
                column(
                  6,
                  pickerInput(
                    inputId = NS(id, "merc5ano1"),
                    label = "Ano",
                    choices = sort(unique(merc5[["ano"]]), decreasing = T),
                    width = "100px"
                  )
                ),
                column(
                  12,
                  withSpinner(
                    echarts4rOutput(NS(id, "merc5graf"), height = "450px"),
                    type = 8,
                    color = "#f17701",
                    size = 0.5
                  )
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "MTP-RAIS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "merc5_1"))
                )
              ),
              ## Tabela - Vínculos Empregatícios por Escolaridade do Trabalhador Formal----
              box(
                title = textOutput(NS(id, "merc5txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "merc5tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "MTP-RAIS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "merc5_2"))
                )
              ),
              ## Tabela - Vínculos Empregatícios por Escolaridade do Trabalhador Formal----
              box(
                title = textOutput(NS(id, "merc5txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "merc5ano"),
                  label = "Ano",
                  choices = sort(unique(merc5[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "merc5tab1"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "MTP-RAIS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "merc5_3"))
                )
              )
            )
          ),

          # 6 - Remuneração Média (R$) do Trabalhador Formal----
          tabPanel(
            "Remuneração Média (R$) do Trabalhador Formal",
            panel(
              ## Controle----
              heading =
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Remuneração Média (R$) do Trabalhador Formal"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "merc6muni"),
                  label = "Município",
                  choices = merc6 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Remuneração Média (R$) do Trabalhador Formal----
              box(
                title = textOutput(NS(id, "merc6txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "merc6municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "merc6graf")),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "MTP-RAIS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "merc6_1"))
                )
              ),
              ## Tabela - Remuneração Média (R$) do Trabalhador Formal----
              box(
                title = textOutput(NS(id, "merc6txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "merc6ano"),
                  label = "Ano",
                  choices = sort(unique(merc6[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "merc6tab2"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "MTP-RAIS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "merc6_2"))
                )
              )
            )
          ),
          # 7 - Remuneração Média (R$) do Trabalhador Formal por Sexo----
          tabPanel(
            "Remuneração Média (R$) do Trabalhador Formal por Sexo",
            panel(
              ## Controle----
              heading =
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Remuneração Média (R$) do Trabalhador Formal por Sexo"),
              tags$div(
                class = "seletor1",
                # select Município
                pickerInput(
                  inputId = NS(id, "merc7muni"),
                  label = "Município",
                  choices = merc7 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Remuneração Média (R$) do Trabalhador Formal por Sexo----
              box(
                title = textOutput(NS(id, "merc7txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                column(
                  2,
                  pickerInput(
                    inputId = NS(id, "merc7municomp"),
                    label = "Comparar Município",
                    choices = NULL,
                    width = "200px",
                    options = list(`none-selected-text` = "Selecione um município")
                  )
                ),
                column(
                  6,
                  pickerInput(
                    inputId = NS(id, "merc7ano1"),
                    label = "Ano",
                    choices = sort(unique(merc7[["ano"]]), decreasing = T),
                    width = "100px"
                  )
                ),
                column(
                  12,
                  withSpinner(
                    echarts4rOutput(NS(id, "merc7graf")),
                    type = 8,
                    color = "#f17701",
                    size = 0.5
                  )
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "MTP-RAIS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "merc7_1"))
                )
              ),
              ## Tabela - Remuneração Média (R$) do Trabalhador Formal por Sexo----
              box(
                title = textOutput(NS(id, "merc7txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "merc7tab"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "MTP-RAIS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "merc7_2"))
                )
              ),
              ## Tabela - Remuneração Média (R$) do Trabalhador Formal por Sexo----
              box(
                title = textOutput(NS(id, "merc7txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "merc7ano"),
                  label = "Ano",
                  choices = sort(unique(merc7[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "merc7tab1"), height = "400px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "MTP-RAIS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "merc7_3"))
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
social_merc_tab_mp_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    #TÍTULOS----
    # 1- Vínculos Empregatícios Total no Emprego Formal----
    ## Gráfico - Vínculos Empregatícios Total no Emprego Formal----
    t11 <- reactive({
      req(input$merc1municomp)
      if (input$merc1municomp == "Selecione um município") {
        paste0("Vínculos Empregatícios Total no Emprego Formal, ",
               input$merc1muni, " - ", min(merc1$ano), " a ", max(merc1$ano))
      } else {
        paste0("Vínculos Empregatícios Total no Emprego Formal, ",
               input$merc1muni, " x ", input$merc1municomp, " - ", min(merc1$ano), " a ", max(merc1$ano))
      }
    })
    ## Tabela - Vínculos Empregatícios Total no Emprego Formal----
    t12 <- reactive({
      ri <- merc1 %>%
        filter(ano == input$merc1ano, localidade == input$merc1muni) %>%
        pull(ri)
      paste0("Vínculos Empregatícios Total no Emprego Formal por Município, Região de Integração ",
             unique(ri), " - ", input$merc1ano)
    })
    # 2 - Vínculos Empregatícios no Emprego Formal por Sexo----
    ## Gráfico - Vínculos Empregatícios no Emprego Formal por Sexo----
    t21 <- reactive({
      req(input$merc2municomp)
      if (input$merc2municomp == "Selecione um município") {
        paste0("Vínculos Empregatícios no Emprego Formal por Sexo, ",
               input$merc2muni, " - ", input$merc2ano1)
      } else {
        paste0("Vínculos Empregatícios no Emprego Formal por Sexo, ",
               input$merc2muni, " x ", input$merc2municomp, " - ", input$merc2ano1)
      }
    })
    ## Tabela - Vínculos Empregatícios no Emprego Formal por Sexo----
    t22 <- reactive({
      paste0("Vínculos Empregatícios no Emprego Formal por Sexo, ",
             input$merc2muni, " - ", min(merc2$ano), " a ", max(merc2$ano))
    })
    ## Tabela - Vínculos Empregatícios no Emprego Formal por Sexo----
    t23 <- reactive({
      ri <- merc2 %>%
        filter(ano == input$merc2ano, localidade == input$merc2muni) %>%
        pull(ri)
      paste0("Vínculos Empregatícios no Emprego Formal por Sexo por Município, Região de Integração ",
             unique(ri), " - ", input$merc2ano)
    })
    # 3 - Vínculos Empregatícios no Emprego Formal por Grande Setor (IBGE)----
    ## Gráfico - Vínculos Empregatícios no Emprego Formal por Grande Setor (IBGE)----
    # Atualização da entrada
    t31 <- reactive({
      req(input$merc3municomp)
      if (input$merc3municomp == "Selecione um município") {
        paste0("Vínculos Empregatícios no Emprego Formal por Grande Setor (IBGE), ",
               input$merc3muni, " - ", input$merc3ano1)
      } else {
        paste0("Vínculos Empregatícios no Emprego Formal por Grande Setor (IBGE), ",
               input$merc3muni, " x ", input$merc3municomp, " - ", input$merc3ano1)
      }
    })
    ## Tabela - Vínculos Empregatícios no Emprego Formal por Grande Setor (IBGE)----
    t32 <- reactive({
      paste0("Vínculos Empregatícios no Emprego Formal por Grande Setor (IBGE), ",
             input$merc3muni, " - ", min(merc3$ano), " a ", max(merc3$ano))
    })
    ## Tabela - Vínculos Empregatícios no Emprego Formal por Grande Setor (IBGE)----
    t33 <- reactive({
      ri <- merc3 %>%
        filter(ano == input$merc3ano, localidade == input$merc3muni) %>%
        pull(ri)
      paste0("Vínculos Empregatícios no Emprego Formal por Grande Setor (IBGE) e Município, Região de Integração ",
             unique(ri), " - ", input$merc3ano)
    })
    # 4 - Vínculos Empregatícios no Emprego Formal por Setor Econômico----
    ## Gráfico - Vínculos Empregatícios no Emprego Formal por Setor Econômico----
    t41 <- reactive({
      req(input$merc4municomp)
      if (input$merc4municomp == "Selecione um município") {
        paste0("Vínculos Empregatícios no Emprego Formal por Setor Econômico, ",
               input$merc4muni, " - ", input$merc4ano1)
      } else {
        paste0("Vínculos Empregatícios no Emprego Formal por Setor Econômico, ",
               input$merc4muni, " x ", input$merc4municomp, " - ", input$merc4ano1)
      }
    })
    ## Tabela - Vínculos Empregatícios no Emprego Formal por Setor Econômico----
    t42 <- reactive({
      paste0("Vínculos Empregatícios no Emprego Formal por Setor Econômico, ",
             input$merc4muni, " - ", min(merc4$ano), " a ", max(merc4$ano))
    })
    ## Tabela - Vínculos Empregatícios no Emprego Formal por Setor Econômico----
    t43 <- reactive({
      ri <- merc4 %>%
        filter(ano == input$merc4ano, localidade == input$merc4muni) %>%
        pull(ri)
      paste0("Vínculos Empregatícios no Emprego Formal por Setor Econômico e Município, Região de Integração ",
             unique(ri), " - ", input$merc4ano)
    })
    # 5 - Vínculos Empregatícios por Escolaridade do Trabalhador Formal----
    ## Gráfico - Vínculos Empregatícios por Escolaridade do Trabalhador Formal----
    t51 <- reactive({
      req(input$merc5municomp)
      if (input$merc5municomp == "Selecione um município") {
        paste0("Vínculos Empregatícios por Escolaridade do Trabalhador Formal, ",
               input$merc5muni, " - ", input$merc5ano1)
      } else {
        paste0("Vínculos Empregatícios por Escolaridade do Trabalhador Formal, ",
               input$merc5muni, " x ", input$merc5municomp, " - ", input$merc5ano1)
      }
    })
    ## Tabela - Vínculos Empregatícios por Escolaridade do Trabalhador Formal----
    t52 <- reactive({
      paste0("Vínculos Empregatícios por Escolaridade do Trabalhador Formal, ",
             input$merc5muni, " - ", min(merc5$ano), " a ", max(merc5$ano))
    })
    ## Tabela - Vínculos Empregatícios por Escolaridade do Trabalhador Formal----
    t53 <- reactive({
      ri <- merc5 %>%
        filter(ano == input$merc5ano, localidade == input$merc5muni) %>%
        pull(ri)
      paste0("Vínculos Empregatícios por Escolaridade do Trabalhador Formal por Município, Região de Integração ",
             unique(ri), " - ", input$merc5ano)
    })
    # 6 - Remuneração Média (R$) do Trabalhador Formal----
    ## Gráfico - Remuneração Média (R$) do Trabalhador Formal----
    t61 <- reactive({
      req(input$merc6municomp)
      if (input$merc6municomp == "Selecione um município") {
        paste0("Remuneração Média (R$) do Trabalhador Formal, ", input$merc6muni,
               " - ", min(merc6$ano), " a ", max(merc6$ano))
      } else {
        paste0("Remuneração Média (R$) do Trabalhador Formal, ", input$merc6muni,
               " x ", input$merc6municomp, " - ", min(merc6$ano), " a ", max(merc6$ano))
      }
    })
    ## Tabela - Remuneração Média (R$) do Trabalhador Formal----
    t62 <- reactive({
      ri <- merc6 %>%
        filter(ano == input$merc6ano, localidade == input$merc6muni) %>%
        pull(ri)
      paste0("Remuneração Média (R$) do Trabalhador Formal por Município, Região de Integração ",
             unique(ri), " - ", input$merc6ano)
    })
    # 7 - Remuneração Média (R$) do Trabalhador Formal por Sexo----
    ## Gráfico - Remuneração Média (R$) do Trabalhador Formal por Sexo----
    t71 <- reactive({
      req(input$merc7municomp)
      if (input$merc7municomp == "Selecione um município") {
        paste0("Remuneração Média (R$) do Trabalhador Formal por Sexo, ",
               input$merc7muni, " - ", input$merc7ano1)
      } else {
        paste0("Remuneração Média (R$) do Trabalhador Formal por Sexo, ",
               input$merc7muni, " x ", input$merc7municomp, " - ", input$merc7ano1)
      }
    })
    ## Tabela - Remuneração Média (R$) do Trabalhador Formal por Sexo----
    t72 <- reactive({
      paste0("Remuneração Média (R$) do Trabalhador Formal por Sexo, ",
             input$merc7muni, " - ", min(merc7$ano), " a ", max(merc7$ano))
    })
    ## Tabela - Remuneração Média (R$) do Trabalhador Formal por Sexo----
    t73 <- reactive({
      ri <- merc7 %>%
        filter(ano == input$merc7ano, localidade == input$merc7muni) %>%
        pull(ri)
      paste0("Remuneração Média (R$) do Trabalhador Formal por Sexo e Município, Região de Integração ",
             unique(ri), " - ", input$merc7ano)
    })
    #VISUALIZAÇÃO----
    # 1- Vínculos Empregatícios Total no Emprego Formal----
    ## Gráfico - Vínculos Empregatícios Total no Emprego Formal----
    # Atualização da entrada
    merc1comp <- reactive({
      input$merc1muni
    })
    observeEvent(merc1comp(), {
      x <- merc1 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != merc1comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "merc1municomp", choices = c("Selecione um município", choices), session)
    })

    output$merc1txt1 <- renderText({
      t11()
    })

    output$merc1graf <- renderEcharts4r({
      req(input$merc1municomp)
      if (input$merc1municomp == "Selecione um município") {
        a <- merc1 %>% filter(localidade == input$merc1muni)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f17701",
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
        a <- merc1 %>% filter(localidade == input$merc1muni)
        b <- merc1 %>% filter(localidade == input$merc1municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$merc1muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$merc1municomp,
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
    ## Tabela - Vínculos Empregatícios Total no Emprego Formal----
    output$merc1txt2 <- renderText({
      t12()
    })
    output$merc1tab2 <- renderReactable({
      ris <- merc1 %>%
        filter(ano == input$merc1ano, localidade == input$merc1muni) %>%
        pull(ri)
      x <- merc1 %>% filter(ano == input$merc1ano, localidade != "Pará")
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
          ri = colDef(name = "Região de Integração" ),
          localidade = colDef(name = "Municípios" ),
          valor = colDef(
            name = "Quantidade",
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
    # 2 - Vínculos Empregatícios no Emprego Formal por Sexo----
    ## Gráfico - Vínculos Empregatícios no Emprego Formal por Sexo----
    # Atualização da entrada
    merc2comp <- reactive({
      input$merc2muni
    })
    observeEvent(merc2comp(), {
      x <- merc2 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != merc2comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "merc2municomp", choices = c("Selecione um município", choices), session)
    })
    ## Título
    output$merc2txt1 <- renderText({
      t21()
    })

    output$merc2graf <- renderEcharts4r({
      req(input$merc2municomp)
      if (input$merc2municomp == "Selecione um município") {
        a <- merc2 %>% filter(localidade == input$merc2muni, 
        ano == input$merc2ano1, categoria != "Total")
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
            name = "Sexo",
            nameLocation = "middle",
            nameTextStyle = list(fontWeight = "bold", padding = c(20, 0, 0, 0), fontSize = 14)
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
          e_grid(show = T)
      } else {
        a <- merc2 %>% filter(localidade == input$merc2muni, 
        ano == input$merc2ano1, categoria != "Total")
        b <- merc2 %>% filter(localidade == input$merc2municomp, 
        ano == input$merc2ano1, categoria != "Total")
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            name = input$merc2muni,
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
            name = input$merc2municomp,
            legend = T,
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
            name = "Sexo",
            nameLocation = "middle",
            nameTextStyle = list(fontWeight = "bold", padding = c(20, 0, 0, 0), fontSize = 14)
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
          e_grid(show = T)
      }
    })
    ## Tabela - Vínculos Empregatícios no Emprego Formal por Sexo----
    output$merc2txt2 <- renderText({
      t22()
    })
    output$merc2tab <- renderReactable({
      x <- merc2 %>%
        filter(localidade == input$merc2muni) %>%
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
    ## Tabela - Vínculos Empregatícios no Emprego Formal por Sexo----
    output$merc2txt3 <- renderText({
      t23()
    })
    output$merc2tab1 <- renderReactable({
      ris <- merc2 %>%
        filter(ano == input$merc2ano, localidade == input$merc2muni) %>%
        pull(ri)
      x <- merc2 %>% filter(ano == input$merc2ano, localidade != "Pará")
      x <- x %>%
        filter(ri == ris) %>%
        select(ri, localidade, categoria, valor) %>%
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
          localidade = colDef(name = "Municípios")
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
    # 3 - Vínculos Empregatícios no Emprego Formal por Grande Setor (IBGE)----
    ## Gráfico - Vínculos Empregatícios no Emprego Formal por Grande Setor (IBGE)----
    # Atualização da entrada
    merc3comp <- reactive({
      input$merc3muni
    })
    observeEvent(merc3comp(), {
      x <- merc3 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != merc3comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "merc3municomp", choices = c("Selecione um município", choices), session)
    })

    ## Título
    output$merc3txt1 <- renderText({
      t31()
    })
    output$merc3graf <- renderEcharts4r({
      req(input$merc3municomp)
      if (input$merc3municomp == "Selecione um município") {
        a <- merc3 %>% filter(localidade == input$merc3muni, 
        ano == input$merc3ano1, categoria != "Total") %>% arrange(valor)
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
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_labels(
            position = "right",
            fontWeight = "bold",
            formatter = htmlwidgets::JS(
              glue::glue(
                "function(params) {return Intl.NumberFormat('pt-BR', { style: 'decimal'}).format(params.value[{{0}}]);}",
                .open = "{{",
                .close = "}}"
              )
            )
          ) %>%
          e_y_axis(
            name = "Quantidade",
            splitNumber = 10,
            nameTextStyle = list(fontWeight = "bold", padding = c(30, 0, 0, 0), fontSize = 14),
            scale = T,
            nameLocation = "middle",
            axisLabel = list(
              formatter = htmlwidgets::JS("
              function (value, index) {
              return value.toLocaleString('pt-BR', { minimumFractionDigits: 0, maximumFractionDigits: 0 });
              }
            ")
            )
          ) %>%
          e_locale("pt-Br") %>%
          e_grid(show = T) %>%
          e_flip_coords()
      } else {
        a <- merc3 %>% filter(localidade == input$merc3muni,
         ano == input$merc3ano1, categoria != "Total")
        b <- merc3 %>% filter(localidade == input$merc3municomp,
         ano == input$merc3ano1, categoria != "Total")
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            name = input$merc3muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, categoria) %>%
          e_bar(
            serie = valor,
            name = input$merc3municomp,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_labels(
            position = "right",
            fontWeight = "bold",
            formatter = htmlwidgets::JS(
              glue::glue(
                "function(params) {return Intl.NumberFormat('pt-BR', { style: 'decimal'}).format(params.value[{{0}}]);}",
                .open = "{{",
                .close = "}}"
              )
            )
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "",
            nameTextStyle =
              list(
                fontWeight = "bold",
                fontSize = 14
              )
          ) %>%
          e_y_axis(
            name = "Quantidade",
            splitNumber = 10,
            nameTextStyle =
              list(fontWeight = "bold", padding = c(30, 0, 0, 0), fontSize = 14),
            scale = T,
            nameLocation = "middle",
            axisLabel = list(
              formatter = htmlwidgets::JS("
              function (value, index) {
              return value.toLocaleString('pt-BR', { minimumFractionDigits: 0, maximumFractionDigits: 0 });
              }
            ")
            )
          ) %>%
          e_locale("pt-Br") %>%
          e_grid(show = T) %>%
          e_flip_coords()
      }
    })
    ## Tabela - Vínculos Empregatícios no Emprego Formal por Grande Setor (IBGE)----
    output$merc3txt2 <- renderText({
      t32()
    })
    output$merc3tab <- renderReactable({
      x <- merc3 %>%
        filter(localidade == input$merc3muni) %>%
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
    ## Tabela - Vínculos Empregatícios no Emprego Formal por Grande Setor (IBGE)----
    output$merc3txt3 <- renderText({
      t33()
    })
    output$merc3tab1 <- renderReactable({
      ri <- merc3 %>%
        filter(ano == input$merc3ano, localidade == input$merc3muni) %>%
        pull(ri)
      x <- merc3 %>% filter(ano == input$merc3ano, localidade != "Pará")
      x <- x %>%
        filter(ri == ri) %>%
        select(ri, localidade, categoria, valor) %>%
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
          localidade = colDef(name = "Municípios")
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
    # 4 - Vínculos Empregatícios no Emprego Formal por Setor Econômico----
    ## Gráfico - Vínculos Empregatícios no Emprego Formal por Setor Econômico----
    # Atualização da entrada
    merc4comp <- reactive({
      input$merc4muni
    })
    observeEvent(merc4comp(), {
      x <- merc4 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != merc4comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "merc4municomp", choices = c("Selecione um município", choices), session)
    })
    ## Título
    output$merc4txt1 <- renderText({
      t41()
    })
    output$merc4graf <- renderEcharts4r({
      req(input$merc4municomp)
      if (input$merc4municomp == "Selecione um município") {
        a <- merc4 %>% filter(localidade == input$merc4muni, 
        ano == input$merc4ano1, categoria != "Total") %>% arrange(valor)
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            color = "#f17701",
            legend = F,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_labels(
            position = "right",
            fontWeight = "bold",
            formatter = htmlwidgets::JS(
              glue::glue(
                "function(params) {return Intl.NumberFormat('pt-BR', { style: 'decimal'}).format(params.value[{{0}}]);}",
                .open = "{{",
                .close = "}}"
              )
            )
          ) %>%
          e_y_axis(
            name = "Quantidade",
            splitNumber = 10,
            nameTextStyle =
              list(fontWeight = "bold", padding = c(30, 0, 0, 0), fontSize = 14),
            scale = T,
            nameLocation = "middle",
            axisLabel = list(
              formatter = htmlwidgets::JS("
              function (value, index) {
              return value.toLocaleString('pt-BR', { minimumFractionDigits: 0, maximumFractionDigits: 0 });
              }
            ")
            )
          ) %>%
          e_locale("pt-Br") %>%
          e_grid(show = T, left = "18%") %>%
          e_flip_coords()
      } else {
        a <- merc4 %>% filter(localidade == input$merc4muni,
         ano == input$merc4ano1, categoria != "Total")
        b <- merc4 %>% filter(localidade == input$merc4municomp,
         ano == input$merc4ano1, categoria != "Total")
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            name = input$merc4muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, categoria) %>%
          e_bar(
            serie = valor,
            name = input$merc4municomp,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_labels(
            position = "right",
            fontWeight = "bold",
            formatter = htmlwidgets::JS(
              glue::glue(
                "function(params) {return Intl.NumberFormat('pt-BR', { style: 'decimal'}).format(params.value[{{0}}]);}",
                .open = "{{",
                .close = "}}"
              )
            )
          ) %>%
          e_y_axis(
            name = "Quantidade",
            splitNumber = 10,
            nameTextStyle = list(fontWeight = "bold", padding = c(30, 0, 0, 0), fontSize = 14),
            scale = T,
            nameLocation = "middle",
            axisLabel = list(
              formatter = htmlwidgets::JS("
              function (value, index) {
              return value.toLocaleString('pt-BR', { minimumFractionDigits: 0, maximumFractionDigits: 0 });
              }
            ")
            )
          ) %>%
          e_locale("pt-Br") %>%
          e_grid(show = T, left = "18%") %>%
          e_flip_coords()
      }
    })
    ## Tabela - Vínculos Empregatícios no Emprego Formal por Setor Econômico----
    output$merc4txt2 <- renderText({
      t42()
    })
    output$merc4tab <- renderReactable({
      x <- merc4 %>%
        filter(localidade == input$merc4muni) %>%
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
    ## Tabela - Vínculos Empregatícios no Emprego Formal por Setor Econômico----
    output$merc4txt3 <- renderText({
      t43()
    })
    output$merc4tab1 <- renderReactable({
      ri <- merc4 %>%
        filter(ano == input$merc4ano, localidade == input$merc4muni) %>%
        pull(ri)
      x <- merc4 %>% filter(ano == input$merc4ano, localidade != "Pará")
      x <- x %>%
        filter(ri == ri) %>%
        select(ri, localidade, categoria, valor) %>%
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
          ri = colDef(name = "Região de Integração",width = 200),
          localidade = colDef(name = "Municípios", width = 200)
        ),
        defaultColDef =
          colDef(
            footerStyle = list(fontWeight = "bold"),
            format = colFormat(separators = T),
            headerStyle = list(background = "#f7f7f8", aling = "Left"),
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
    # 5 - Vínculos Empregatícios por Escolaridade do Trabalhador Formal----
    ## Gráfico - Vínculos Empregatícios por Escolaridade do Trabalhador Formal----
    # Atualização da entrada
    merc5comp <- reactive({
      input$merc5muni
    })
    observeEvent(merc5comp(), {
      x <- merc5 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != merc5comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "merc5municomp", choices = c("Selecione um município", choices), session)
    })

    ## Título
    output$merc5txt1 <- renderText({
      t51()
    })

    output$merc5graf <- renderEcharts4r({
      req(input$merc5municomp)
      if (input$merc5municomp == "Selecione um município") {
        a <- merc5 %>% filter(localidade == input$merc5muni, 
        ano == input$merc5ano1, categoria != "Total")
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            color = "#f17701",
            name = input$merc5muni,
            legend = F,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_labels(
            position = "right",
            fontWeight = "bold",
            formatter = htmlwidgets::JS(
              glue::glue(
                "function(params) {return Intl.NumberFormat('pt-BR', { style: 'decimal'}).format(params.value[{{0}}]);}",
                .open = "{{",
                .close = "}}"
              )
            )
          ) %>%
          e_y_axis(
            name = "Quantidade",
            splitNumber = 10,
            nameTextStyle = list(fontWeight = "bold", padding = c(30, 0, 0, 0), fontSize = 14),
            scale = T,
            nameLocation = "middle",
            axisLabel = list(
              formatter = htmlwidgets::JS("
              function (value, index) {
              return value.toLocaleString('pt-BR', { minimumFractionDigits: 0, maximumFractionDigits: 0 });
              }
            ")
            )
          ) %>%
          e_locale("pt-Br") %>%
          e_grid(show = T,containLabel = T) %>%
          e_flip_coords()
      } else {
        a <- merc5 %>% filter(localidade == input$merc5muni, 
        ano == input$merc5ano1, categoria != "Total")
        b <- merc5 %>% filter(localidade == input$merc5municomp,
         ano == input$merc5ano1, categoria != "Total")
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            name = input$merc5muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, categoria) %>%
          e_bar(
            serie = valor,
            name = input$merc5municomp,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_labels(
            position = "right",
            fontWeight = "bold",
            formatter = htmlwidgets::JS(
              glue::glue(
                "function(params) {return Intl.NumberFormat('pt-BR', { style: 'decimal'}).format(params.value[{{0}}]);}",
                .open = "{{",
                .close = "}}"
              )
            )
          ) %>%
          e_y_axis(
            name = "Quantidade",
            splitNumber = 10,
            nameTextStyle = list(fontWeight = "bold", padding = c(30, 0, 0, 0), fontSize = 14),
            scale = T,
            nameLocation = "middle",
            axisLabel = list(
              formatter = htmlwidgets::JS("
              function (value, index) {
              return value.toLocaleString('pt-BR', { minimumFractionDigits: 0, maximumFractionDigits: 0 });
              }
            ")
            )
          ) %>%
          e_locale("pt-Br") %>%
          e_grid(show = T,containLabel = T) %>%
          e_flip_coords()
      }
    })
    ## Tabela - Vínculos Empregatícios por Escolaridade do Trabalhador Formal----
    output$merc5txt2 <- renderText({
      t52()
    })
    output$merc5tab <- renderReactable({
      x <- merc5 %>%
        filter(localidade == input$merc5muni) %>%
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
        columns = list(ano = colDef(
          name = "Ano",
          sticky = "left",
          width = 100
        )), 
        defaultColDef =
          colDef(
            footerStyle = list(fontWeight = "bold"), width = 200,
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
    ## Tabela - Vínculos Empregatícios por Escolaridade do Trabalhador Formal----
    output$merc5txt3 <- renderText({
      t53()
    })
    output$merc5tab1 <- renderReactable({
      ris <- merc5 %>%
        filter(ano == input$merc5ano, localidade == input$merc5muni) %>%
        pull(ri)
      x <- merc5 %>% filter(ano == input$merc5ano, localidade != "Pará")
      x <- x %>%
        filter(ri == ris) %>%
        select(ri, localidade, categoria, valor) %>%
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
          ri = colDef(name = "Região de Integração",width = 200),
          localidade = colDef(name = "Municípios", sticky = "left")
        ),
        defaultColDef =
          colDef(
            footerStyle = list(fontWeight = "bold"), width = 200,
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
    # 6 - Remuneração Média (R$) do Trabalhador Formal----
    ## Gráfico - Remuneração Média (R$) do Trabalhador Formal----
    # Atualização da entrada
    merc6comp <- reactive({
      input$merc6muni
    })
    observeEvent(merc6comp(), {
      x <- merc6 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != merc6comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "merc6municomp", choices = c("Selecione um município", choices), session)
    })

    output$merc6txt1 <- renderText({
      t61()
    })

    output$merc6graf <- renderEcharts4r({
      req(input$merc6municomp)
      if (input$merc6municomp == "Selecione um município") {
        a <- merc6 %>% filter(localidade == input$merc6muni)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f17701",
            name = "Remuneração Média (R$)",
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
            name = "Remuneração Média (R$)",
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
        a <- merc6 %>% filter(localidade == input$merc6muni)
        b <- merc6 %>% filter(localidade == input$merc6municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$merc6muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$merc6municomp,
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
            name = "Remuneração Média (R$)",
            nameTextStyle =
              list(fontWeight = "bold", fontSize = 14),
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
    ## Tabela - Remuneração Média (R$) do Trabalhador Formal----
    output$merc6txt2 <- renderText({
      t62()
    })
    output$merc6tab2 <- renderReactable({
      ris <- merc6 %>%
        filter(ano == input$merc6ano, localidade == input$merc6muni) %>%
        pull(ri)
      x <- merc6 %>% filter(ano == input$merc6ano, localidade != "Pará")
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
        borderless = TRUE,
        outlined = TRUE,
        resizable = FALSE,
        showSortable = TRUE,
        pagination = FALSE,
        columns = list(
          posicao = colDef(name = "nº", width = 50),
          ri = colDef(name = "Região de Integração"),
          localidade = colDef(name = "Municípios"),
          valor = colDef(
            name = " Remuneração Média (R$)",
            format = colFormat(digits = 4, separators = T, locales = "pt-BR"),
            cell = data_bars(x,
              text_position = "above",
              box_shadow = TRUE,
              align_bars = "right",
              number_fmt = scales::number_format(accuracy = 0.01, decimal.mark = ",", big.mark = ".")
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

    # 7 - Remuneração Média (R$) do Trabalhador Formal por Sexo----
    ## Gráfico - Remuneração Média (R$) do Trabalhador Formal por Sexo----
    # Atualização da entrada
    merc7comp <- reactive({
      input$merc7muni
    })
    observeEvent(merc7comp(), {
      x <- merc7 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != merc7comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "merc7municomp", choices = c("Selecione um município", choices), session)
    })

    ## Título
    output$merc7txt1 <- renderText({
      t71()
    })

    output$merc7graf <- renderEcharts4r({
      req(input$merc7municomp)
      if (input$merc7municomp == "Selecione um município") {
        a <- merc7 %>% filter(localidade == input$merc7muni,
         ano == input$merc7ano1, categoria != "Total")
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            color = "#f17701",
            name = "Remuneração Média (R$)",
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
            nameTextStyle = list(fontWeight = "bold", padding = c(20, 0, 0, 0), fontSize = 14)
          ) %>%
          e_y_axis(
            name = "Média (R$)",
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
          e_grid(show = T)
      } else {
        a <- merc7 %>% filter(localidade == input$merc7muni, 
        ano == input$merc7ano, categoria != "Total")
        b <- merc7 %>% filter(localidade == input$merc7municomp,
         ano == input$merc7ano, categoria != "Total")
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            name = input$merc7muni,
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
            name = input$merc7municomp,
            legend = T,
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
            name = "Sexo",
            nameLocation = "middle",
            nameTextStyle = list(fontWeight = "bold", padding = c(20, 0, 0, 0), fontSize = 14)
          ) %>%
          e_y_axis(
            name = "Média (R$)",
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
          e_grid(show = T)
      }
    })
    ## Tabela - Remuneração Média (R$) do Trabalhador Formal por Sexo----
    output$merc7txt2 <- renderText({
      t72()
    })
    output$merc7tab <- renderReactable({
      x <- merc7 %>%
        filter(localidade == input$merc7muni) %>%
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
            format = colFormat(separators = T, digits = 2),
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
    ## Tabela - Remuneração Média (R$) do Trabalhador Formal por Sexo----
    output$merc7txt3 <- renderText({
      t73()
    })
    output$merc7tab1 <- renderReactable({
      ris <- merc7 %>%
        filter(ano == input$merc7ano, localidade == input$merc7muni) %>%
        pull(ri)
      x <- merc7 %>% filter(ano == input$merc7ano, localidade != "Pará")
      x <- x %>%
        filter(ri == ris) %>%
        select(ri, localidade, categoria, valor) %>%
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
          ri = colDef(name = "Região de Instegração"),
          localidade = colDef(name = "Municípios")
        ),
        defaultColDef =
          colDef(
            footerStyle = list(fontWeight = "bold"),
            format = colFormat(separators = T, digits = 2),
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
    #DOWNLOADS----
    # 1 - Vínculos Empregatícios Total no Emprego Formal----
    ## Gráfico - Vínculos Empregatícios Total no Emprego Formal----
    # Filtra os dados
    merc1_1 <- reactive({
      req(input$merc1municomp)
      if (input$merc1municomp == "Selecione um município") {
        a <- merc1 %>% filter(localidade == input$merc1muni)
        } else {
        a <- merc1 %>% filter(localidade == input$merc1muni)
        b <- merc1 %>% filter(localidade == input$merc1municomp)
        df <- rbind(a,b)}
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(merc1_1(), {
      downset_Server("merc1_1", merc1_1(), t11())
    })
    ## Tabela - Vínculos Empregatícios Total no Emprego Formal----
    # Filtra os dados
    merc1_2 <- reactive({
      ris <- merc1 %>%
        filter(ano == input$merc1ano, localidade == input$merc1muni) %>%
        pull(ri)
      x <- merc1 %>% 
        filter(ano == input$merc1ano, 
               localidade != "Pará",
               ri == ris) %>% 
        arrange(desc(valor)) 
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(merc1_2(), {
      downset_Server("merc1_2", merc1_2(), t12())
    })
   
    # 2 - Vínculos Empregatícios no Emprego Formal por Sexo----
    ## Gráfico - Vínculos Empregatícios no Emprego Formal por Sexo----
    # Filtra os dados
    merc2_1 <- reactive({
    req(input$merc2municomp)
      if (input$merc2municomp == "Selecione um município") {
        a <- merc2 %>% filter(localidade == input$merc2muni, 
        ano == input$merc2ano1, categoria != "Total")
        } else {
        a <- merc2 %>% filter(localidade == input$merc2muni, 
        ano == input$merc2ano1, categoria != "Total")
        b <- merc2 %>% filter(localidade == input$merc2municomp, 
        ano == input$merc2ano1, categoria != "Total")
        df <- rbind(a,b)}
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(merc2_1(), {
      downset_Server("merc2_1", merc2_1(), t21())
    })
    ## Tabela - Vínculos Empregatícios no Emprego Formal por Sexo----
    # Filtra os dados
    merc2_2 <- reactive({
      x <- merc2 %>%
        filter(localidade == input$merc2muni)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(merc2_2(), {
      downset_Server("merc2_2", merc2_2(), t22())
    })
    ## Tabela - Vínculos Empregatícios no Emprego Formal por Sexo----
    # Filtra os dados
    merc2_3 <- reactive({
      ris <- merc2 %>%
        filter(ano == input$merc2ano, localidade == input$merc2muni) %>%
        pull(ri)
      x <- merc2 %>% 
        filter(ano == input$merc2ano, 
               localidade != "Pará",
               ri == ris)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(merc2_3(), {
      downset_Server("merc2_3", merc2_3(), t23())
    })

    # 3 - Vínculos Empregatícios no Emprego Formal por Grande Setor (IBGE)----
    ## Gráfico - Vínculos Empregatícios no Emprego Formal por Grande Setor (IBGE)----
    # Filtra os dados
    merc3_1 <- reactive({
      req(input$merc3municomp)
      if (input$merc3municomp == "Selecione um município") {
        a <- merc3 %>% 
          filter(localidade == input$merc3muni, 
                 ano == input$merc3ano1, categoria != "Total")
      } else {
        a <- merc3 %>% 
          filter(localidade == input$merc3muni,
                 ano == input$merc3ano1, categoria != "Total")
        b <- merc3 %>% 
          filter(localidade == input$merc3municomp,
                 ano == input$merc3ano1, categoria != "Total")
        df <- rbind(a,b)}
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(merc3_1(), {
      downset_Server("merc3_1", merc3_1(), t31())
    })
    ## Tabela - Vínculos Empregatícios no Emprego Formal por Grande Setor (IBGE)----
    # Filtra os dados
    merc3_2 <- reactive({
      x <- merc3 %>%
        filter(localidade == input$merc3muni)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(merc3_2(), {
      downset_Server("merc3_2", merc3_2(), t32())
    })
    ## Tabela - Vínculos Empregatícios no Emprego Formal por Grande Setor (IBGE)----
    # Filtra os dados
    merc3_3 <- reactive({
      ris <- merc3 %>%
        filter(ano == input$merc3ano, localidade == input$merc3muni) %>%
        pull(ri)
      x <- merc3 %>% 
        filter(ano == input$merc3ano, 
               localidade != "Pará",
               ri == ris)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(merc3_3(), {
      downset_Server("merc3_3", merc3_3(), t33())
    })

    # 4 - Vínculos Empregatícios no Emprego Formal por Setor Econômico----
    ## Gráfico - Vínculos Empregatícios no Emprego Formal por Setor Econômico----
    # Filtra os dados
    merc4_1 <- reactive({
      req(input$merc4municomp)
      if (input$merc4municomp == "Selecione um município") {
        a <- merc4 %>% filter(localidade == input$merc4muni, 
        ano == input$merc4ano1, categoria != "Total")
        } else {
        a <- merc4 %>% filter(localidade == input$merc4muni,
         ano == input$merc4ano1, categoria != "Total")
        b <- merc4 %>% filter(localidade == input$merc4municomp,
         ano == input$merc4ano1, categoria != "Total")
        df <- rbind(a,b)}
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(merc4_1(), {
      downset_Server("merc4_1", merc4_1(), t41())
    })
    ## Tabela - Vínculos Empregatícios no Emprego Formal por Setor Econômico----
    # Filtra os dados
    merc4_2 <- reactive({
       x <- merc4 %>%
        filter(localidade == input$merc4muni)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(merc4_2(), {
      downset_Server("merc4_2", merc4_2(), t42())
    })
    ## Tabela - Vínculos Empregatícios no Emprego Formal por Setor Econômico----
    # Filtra os dados
    merc4_3 <- reactive({
       ris <- merc4 %>%
        filter(ano == input$merc4ano, localidade == input$merc4muni) %>%
        pull(ri)
      x <- merc4 %>% 
        filter(ano == input$merc4ano, 
               localidade != "Pará",
               ri == ris)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(merc4_3(), {
      downset_Server("merc4_3", merc4_3(), t43())
    })

    # 5 - Vínculos Empregatícios por Escolaridade do Trabalhador Formal----
    ## Gráfico - Vínculos Empregatícios por Escolaridade do Trabalhador Formal----
    # Filtra os dados
    merc5_1 <- reactive({
      req(input$merc5municomp)
      if (input$merc5municomp == "Selecione um município") {
        a <- merc5 %>% filter(localidade == input$merc5muni, 
        ano == input$merc5ano1, categoria != "Total")
        } else {
        a <- merc5 %>% filter(localidade == input$merc5muni, 
        ano == input$merc5ano1, categoria != "Total")
        b <- merc5 %>% filter(localidade == input$merc5municomp,
         ano == input$merc5ano1, categoria != "Total")
        df <- rbind(a,b)}
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(merc5_1(), {
      downset_Server("merc5_1", merc5_1(), t51())
    })
    ## Tabela - Vínculos Empregatícios por Escolaridade do Trabalhador Formal----
    # Filtra os dados
    merc5_2 <- reactive({
      x <- merc5 %>%
        filter(localidade == input$merc5muni)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(merc5_2(), {
      downset_Server("merc5_2", merc5_2(), t52())
    })
    ## Tabela - Vínculos Empregatícios por Escolaridade do Trabalhador Formal----
    # Filtra os dados
    merc5_3 <- reactive({
      ris <- merc5 %>%
        filter(ano == input$merc5ano, localidade == input$merc5muni) %>%
        pull(ri)
      x <- merc5 %>% 
        filter(ano == input$merc5ano,
               localidade != "Pará",
               ri == ris)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(merc5_3(), {
      downset_Server("merc5_3", merc5_3(), t53())
    })

    # 6 - Remuneração Média (R$) do Trabalhador Formal----
    ## Gráfico - Remuneração Média (R$) do Trabalhador Formal----
    # Filtra os dados
    merc6_1 <- reactive({
      req(input$merc6municomp)
      if (input$merc6municomp == "Selecione um município") {
        a <- merc6 %>% filter(localidade == input$merc6muni)
        } else {
        a <- merc6 %>% filter(localidade == input$merc6muni)
        b <- merc6 %>% filter(localidade == input$merc6municomp)
        df <- rbind(a,b)}
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(merc6_1(), {
      downset_Server("merc6_1", merc6_1(), t61())
    })
    ## Tabela - Remuneração Média (R$) do Trabalhador Formal----
    # Filtra os dados
    merc6_2 <- reactive({
      ris <- merc6 %>%
        filter(ano == input$merc6ano, localidade == input$merc6muni) %>%
        pull(ri)
      x <- merc6 %>% 
        filter(ano == input$merc6ano, 
               localidade != "Pará",
               ri == ris) %>% 
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(merc6_2(), {
      downset_Server("merc6_2", merc6_2(), t62())
    })

    # 7 - Remuneração Média (R$) do Trabalhador Formal por Sexo----
    ## Gráfico - Remuneração Média (R$) do Trabalhador Formal por Sexo----
    # Filtra os dados
    merc7_1 <- reactive({
       req(input$merc7municomp)
      if (input$merc7municomp == "Selecione um município") {
        a <- merc7 %>% filter(localidade == input$merc7muni,
         ano == input$merc7ano1, categoria != "Total")
         } else {
        a <- merc7 %>% filter(localidade == input$merc7muni, 
        ano == input$merc7ano, categoria != "Total")
        b <- merc7 %>% filter(localidade == input$merc7municomp,
         ano == input$merc7ano, categoria != "Total")
        df <- rbind(a,b)}
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(merc7_1(), {
      downset_Server("merc7_1", merc7_1(), t71())
    })
    ## Tabela - Remuneração Média (R$) do Trabalhador Formal por Sexo----
    # Filtra os dados
    merc7_2 <- reactive({
      x <- merc7 %>%
        filter(localidade == input$merc7muni)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(merc7_2(), {
      downset_Server("merc7_2", merc7_2(), t72())
    })
    ## Tabela - Remuneração Média (R$) do Trabalhador Formal por Sexo----
    # Filtra os dados
    merc7_3 <- reactive({
      ris <- merc7 %>%
        filter(ano == input$merc7ano, localidade == input$merc7muni) %>%
        pull(ri)
      x <- merc7 %>% 
        filter(ano == input$merc7ano, 
               localidade != "Pará",
               ri == ris)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(merc7_3(), {
      downset_Server("merc7_3", merc7_3(), t73())
    })
  })
}

# Play do Módulo
# ui <- dashboardPage(
#   header = dashboardHeader(),
#   sidebar = dashboardSidebar(),
#   body = dashboardBody(fluidPage(
#     social_merc_tab_mp_ui("social_merc_tab_mp")
#   ))
# )
# server <- function(input, output) {
#   social_merc_tab_mp_Server("social_merc_tab_mp")
# }
# 
# shinyApp(ui, server)
