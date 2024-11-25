# Funções de módulo de Economia - Municipal
# Função de UI

economia_pec_mp_ui <- function(id) {
  fluidPage(
    # CAMINHO DO ARQUIVO CSS----
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "estilo.css")
    ),
    # Lista de Navegação lateral----
    div(
      class = "navbar_economia",
      navbarPage(
        tags$b("Pecuária - Municípios"),
        navbarMenu(
          tags$b("Escolha um Indicador"),
          # 1 - Efetivo de Rebanho bovino (Cabeça)----
          tabPanel(
            "Efetivo de Rebanho bovino (Cabeça)",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Efetivo de Rebanho bovino (Cabeça)"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "pec1muni"),
                  label = "Município",
                  choices = pec1 %>% 
                    filter(localidade != "Pará") %>% 
                    pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Efetivo de Rebanho bovino (Cabeça)----
              box(
                title = textOutput(NS(id, "pec1txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "pec1municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "pec1graf")),
                  type = 8,
                  color = "#f2a92a",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pec1_1"))
                )
              )
            ),
            fluidRow(
              ## Tabela - Efetivo de Rebanho bovino (Cabeça)----
              box(
                title = textOutput(NS(id, "pec1txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "pec1ano"),
                  label = "Ano",
                  choices = sort(unique(pec1[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "pec1tab2"), height = "400px"),
                  type = 8,
                  color = "#f2a92a",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pec1_2"))
                )
              )
            )
          ),
          # 2 - Efetivo de Rebanho bubalino (Cabeça)----
          tabPanel(
            "Efetivo de Rebanho bubalino (Cabeça)",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Efetivo de Rebanho bubalino (Cabeça)"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "pec2muni"),
                  label = "Município",
                  choices = pec2 %>% 
                    filter(localidade != "Pará") %>% 
                    pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Efetivo de Rebanho bubalino (Cabeça)----
              box(
                title = textOutput(NS(id, "pec2txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "pec2municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "pec2graf")),
                  type = 8,
                  color = "#f2a92a",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pec2_1"))
                )
              )
            ),
            fluidRow(
              ## Tabela - Efetivo de Rebanho bubalino (Cabeça)----
              box(
                title = textOutput(NS(id, "pec2txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "pec2ano"),
                  label = "Ano",
                  choices = sort(unique(pec2[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "pec2tab2"), height = "400px"),
                  type = 8,
                  color = "#f2a92a",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pec2_2"))
                )
              )
            )
          ),
          # 3 - Efetivo de Rebanho Equino----
          tabPanel(
            "Efetivo de Rebanho Equino",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Efetivo de Rebanho Equino"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "pec3muni"),
                  label = "Município",
                  choices = pec3 %>% 
                    filter(localidade != "Pará") %>% 
                    pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Efetivo de Rebanho Equino----
              box(
                title = textOutput(NS(id, "pec3txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "pec3municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "pec3graf")),
                  type = 8,
                  color = "#f2a92a",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pec3_1"))
                )
              )
            ),
            fluidRow(
              ## Tabela - Efetivo de Rebanho Equino----
              box(
                title = textOutput(NS(id, "pec3txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "pec3ano"),
                  label = "Ano",
                  choices = sort(unique(pec3[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "pec3tab2"), height = "400px"),
                  type = 8,
                  color = "#f2a92a",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pec3_2"))
                )
              )
            )
          ),

          # 4 - Efetivo de Rebanho suino total (Cabeça)----
          tabPanel(
            "Efetivo de Rebanho suino total (Cabeça)",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Efetivo de Rebanho suino total (Cabeça)"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "pec4muni"),
                  label = "Município",
                  choices = pec4 %>% 
                    filter(localidade != "Pará") %>% 
                    pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Efetivo de Rebanho suino total (Cabeça)----
              box(
                title = textOutput(NS(id, "pec4txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "pec4municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "pec4graf")),
                  type = 8,
                  color = "#f2a92a",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pec4_1"))
                )
              )
            ),
            fluidRow(
              ## Tabela - Efetivo de Rebanho suino total (Cabeça)----
              box(
                title = textOutput(NS(id, "pec4txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "pec4ano"),
                  label = "Ano",
                  choices = sort(unique(pec4[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "pec4tab2"), height = "400px"),
                  type = 8,
                  color = "#f2a92a",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pec4_2"))
                )
              )
            )
          ),
          # 5 - Efetivo de Rebanho suino total (Matrizes)----
          tabPanel(
            "Efetivo de Rebanho suino total (Matrizes)",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Efetivo de Rebanho suino total (Matrizes)"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "pec5muni"),
                  label = "Município",
                  choices = pec5 %>% 
                    filter(localidade != "Pará") %>% 
                    pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Efetivo de Rebanho suino total (Matrizes)----
              box(
                title = textOutput(NS(id, "pec5txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "pec5municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "pec5graf")),
                  type = 8,
                  color = "#f2a92a",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pec5_1"))
                )
              )
            ),
            fluidRow(
              ## Tabela - Efetivo de Rebanho suino total (Matrizes)----
              box(
                title = textOutput(NS(id, "pec5txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "pec5ano"),
                  label = "Ano",
                  choices = sort(unique(pec5[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "pec5tab2"), height = "400px"),
                  type = 8,
                  color = "#f2a92a",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pec5_2"))
                )
              )
            )
          ),
          # 6 - Efetivo de Rebanho Caprino (cabeças)----
          tabPanel(
            "Efetivo de Rebanho Caprino (cabeça)",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Efetivo de Rebanho Caprino (cabeça)"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "pec6muni"),
                  label = "Município",
                  choices = pec6 %>% 
                    filter(localidade != "Pará") %>% 
                    pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Efetivo de Rebanho Caprino (cabeça)----
              box(
                title = textOutput(NS(id, "pec6txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "pec6municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "pec6graf")),
                  type = 8,
                  color = "#f2a92a",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pec6_1"))
                )
              )
            ),
            fluidRow(
              ## Tabela - Efetivo de Rebanho Caprino (cabeça)----
              box(
                title = textOutput(NS(id, "pec6txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "pec6ano"),
                  label = "Ano",
                  choices = sort(unique(pec6[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "pec6tab2"), height = "400px"),
                  type = 8,
                  color = "#f2a92a",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pec6_2"))
                )
              )
            )
          ),
          # 7 - Efetivo de Rebanho Ovino (cabeças)----
          tabPanel(
            "Efetivo de Rebanho Ovino (cabeças)",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Efetivo de Rebanho Ovino (cabeças)"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "pec7muni"),
                  label = "Município",
                  choices = pec7 %>% 
                    filter(localidade != "Pará") %>% 
                    pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Efetivo de Rebanho Ovino (cabeças)----
              box(
                title = textOutput(NS(id, "pec7txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "pec7municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "pec7graf")),
                  type = 8,
                  color = "#f2a92a",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pec7_1"))
                )
              )
            ),
            fluidRow(
              ## Tabela - Efetivo de Rebanho Ovino (cabeças)----
              box(
                title = textOutput(NS(id, "pec7txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "pec7ano"),
                  label = "Ano",
                  choices = sort(unique(pec7[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "pec7tab2"), height = "400px"),
                  type = 8,
                  color = "#f2a92a",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pec7_2"))
                )
              )
            )
          ),
          # 8 - Efetivo de Rebanho Galináceos (Total)----
          tabPanel(
            "Efetivo de Rebanho Galináceos (Total)",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Efetivo de Rebanho Galináceos (Total)"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "pec8muni"),
                  label = "Município",
                  choices = pec8 %>% 
                    filter(localidade != "Pará") %>% 
                    pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Efetivo de Rebanho Galináceos (Total)----
              box(
                title = textOutput(NS(id, "pec8txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "pec8municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "pec8graf")),
                  type = 8,
                  color = "#f2a92a",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pec8_1"))
                )
              )
            ),
            fluidRow(
              ## Tabela - Efetivo de Rebanho Galináceos (Total)----
              box(
                title = textOutput(NS(id, "pec8txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "pec8ano"),
                  label = "Ano",
                  choices = sort(unique(pec8[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "pec8tab2"), height = "400px"),
                  type = 8,
                  color = "#f2a92a",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pec8_2"))
                )
              )
            )
          ),
          # 9 - Efetivo de Rebanho Galináceos (Galinhas)----
          tabPanel(
            "Efetivo de Rebanho Galináceos (Galinhas)",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Efetivo de Rebanho Galináceos (Galinhas)"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "pec9muni"),
                  label = "Município",
                  choices = pec9 %>% 
                    filter(localidade != "Pará") %>% 
                    pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Efetivo de Rebanho Galináceos (Galinhas)----
              box(
                title = textOutput(NS(id, "pec9txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "pec9municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "pec9graf")),
                  type = 8,
                  color = "#f2a92a",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pec9_1"))
                )
              )
            ),
            fluidRow(
              ## Tabela - Efetivo de Rebanho Galináceos (Galinhas)----
              box(
                title = textOutput(NS(id, "pec9txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "pec9ano"),
                  label = "Ano",
                  choices = sort(unique(pec9[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "pec9tab2"), height = "400px"),
                  type = 8,
                  color = "#f2a92a",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pec9_2"))
                )
              )
            )
          ),
          # 10 - Efetivo de Rebanho Codornas (cabeças)----
          tabPanel(
            "Efetivo de Rebanho Codornas (cabeças)",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Efetivo de Rebanho Codornas (cabeças)"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "pec10muni"),
                  label = "Município",
                  choices = pec10 %>% 
                    filter(localidade != "Pará") %>% 
                    pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Efetivo de Rebanho Codornas (cabeças)----
              box(
                title = textOutput(NS(id, "pec10txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "pec10municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "pec10graf")),
                  type = 8,
                  color = "#f2a92a",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pec10_1"))
                )
              )
            ),
            fluidRow(
              ## Tabela - Efetivo de Rebanho Codornas (cabeças)----
              box(
                title = textOutput(NS(id, "pec10txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "pec10ano"),
                  label = "Ano",
                  choices = sort(unique(pec10[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "pec10tab2"), height = "400px"),
                  type = 8,
                  color = "#f2a92a",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pec10_2"))
                )
              )
            )
          ),
          # 11 - Produção de Origem Animal Leite (Mil litros)----
          tabPanel(
            "Produção de Origem Animal Leite (Mil litros)",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Produção de Origem Animal Leite (Mil litros)"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "pec11muni"),
                  label = "Município",
                  choices = pec11 %>% 
                    filter(localidade != "Pará") %>% 
                    pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Produção de Origem Animal Leite (Mil litros)----
              box(
                title = textOutput(NS(id, "pec11txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "pec11municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "pec11graf")),
                  type = 8,
                  color = "#f2a92a",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pec11_1"))
                )
              )
            ),
            fluidRow(
              ## Tabela - Produção de Origem Animal Leite (Mil litros)----
              box(
                title = textOutput(NS(id, "pec11txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "pec11ano"),
                  label = "Ano",
                  choices = sort(unique(pec11[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "pec11tab2"), height = "400px"),
                  type = 8,
                  color = "#f2a92a",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pec11_2"))
                )
              )
            )
          ),
          # 12 - Produção de Origem Animal Ovos de galinha (Mil dúzias)----
          tabPanel(
            "Produção de Origem Animal Ovos de galinha (Mil dúzias)",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Produção de Origem Animal Ovos de galinha (Mil dúzias)"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "pec12muni"),
                  label = "Município",
                  choices = pec12 %>% 
                    filter(localidade != "Pará") %>% 
                    pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Produção de Origem Animal Ovos de galinha (Mil dúzias)----
              box(
                title = textOutput(NS(id, "pec12txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "pec12municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "pec12graf")),
                  type = 8,
                  color = "#f2a92a",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pec12_1"))
                )
              )
            ),
            fluidRow(
              ## Tabela - Produção de Origem Animal Ovos de galinha (Mil dúzias)----
              box(
                title = textOutput(NS(id, "pec12txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "pec12ano"),
                  label = "Ano",
                  choices = sort(unique(pec12[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "pec12tab2"), height = "400px"),
                  type = 8,
                  color = "#f2a92a",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pec12_2"))
                )
              )
            )
          ),
          # 13 - Produção de Origem Animal Ovos de codorna (Mil dúzias)----
          tabPanel(
            "Produção de Origem Animal Ovos de codorna (Mil dúzias)",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Produção de Origem Animal Ovos de codorna (Mil dúzias)"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "pec13muni"),
                  label = "Município",
                  choices = pec13 %>% 
                    filter(localidade != "Pará") %>% 
                    pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Produção de Origem Animal Ovos de codorna (Mil dúzias)----
              box(
                title = textOutput(NS(id, "pec13txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "pec13municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "pec13graf")),
                  type = 8,
                  color = "#f2a92a",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pec13_1"))
                )
              )
            ),
            fluidRow(
              ## Tabela - Produção de Origem Animal Ovos de codorna (Mil dúzias)----
              box(
                title = textOutput(NS(id, "pec13txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "pec13ano"),
                  label = "Ano",
                  choices = sort(unique(pec13[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "pec13tab2"), height = "400px"),
                  type = 8,
                  color = "#f2a92a",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pec13_2"))
                )
              )
            )
          ),
          # 14 - Produção de Origem Animal Mel de abelha (Quilogramas)----
          tabPanel(
            "Produção de Origem Animal Mel de abelha (Quilogramas)",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Produção de Origem Animal Mel de abelha (Quilogramas)"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "pec14muni"),
                  label = "Município",
                  choices = pec14 %>% 
                    filter(localidade != "Pará") %>% 
                    pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Produção de Origem Animal Mel de abelha (Quilogramas)----
              box(
                title = textOutput(NS(id, "pec14txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "pec14municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "pec14graf")),
                  type = 8,
                  color = "#f2a92a",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pec14_1"))
                )
              )
            ),
            fluidRow(
              ## Tabela - Produção de Origem Animal Mel de abelha (Quilogramas)----
              box(
                title = textOutput(NS(id, "pec14txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "pec14ano"),
                  label = "Ano",
                  choices = sort(unique(pec14[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "pec14tab2"), height = "400px"),
                  type = 8,
                  color = "#f2a92a",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pec14_2"))
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
economia_pec_mp_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    #TÍTULOS----
    # 1- Efetivo de Rebanho bovino (Cabeça) ----
    ## Gráfico - Efetivo de Rebanho bovino (Cabeça)----
    t11 <- reactive({
      req(input$pec1municomp)
      if (input$pec1municomp == "Selecione um município") {
        paste0("Efetivo de Rebanho bovino (Cabeças), ", 
               input$pec1muni, " - ", min(pec1$ano), " a ", max(pec1$ano))
      } else {
        paste0("Efetivo de Rebanho bovino (Cabeças) ", 
               input$pec1muni, " x ", input$pec1municomp, input$pec1muni, " - ", min(pec1$ano), " a ", max(pec1$ano))
      }
    })
    
    ## Tabela - Efetivo de Rebanho bovino (Cabeça) da mesma Região de Integração----
    t12 <- reactive({
      ri <- pec1 %>%
        filter(ano == input$pec1ano, localidade == input$pec1muni) %>%
        pull(ri)
      paste0("Efetivo de Rebanho bovino (Cabeça) dos Municípios, Região de Integração ",
             ri, " - ", input$pec1ano)
    })

    # 2- Efetivo de Rebanho bubalino (Cabeça) ----
    ## Gráfico - Efetivo de Rebanho bubalino (Cabeça)----
    t21 <- reactive({
      req(input$pec2municomp)
      if (input$pec2municomp == "Selecione um município") {
        paste0("Efetivo de Rebanho bubalino (Cabeça), ", 
        input$pec2muni, " - ", min(pec2$ano), " a ", max(pec2$ano))
      } else {
        paste0("Efetivo de Rebanho bubalino (Cabeça), ", 
        input$pec2muni, " x ", input$pec2municomp, input$pec1muni, " - ", min(pec2$ano), " a ", max(pec2$ano))
      }
    })
    ## Tabela - Efetivo de Rebanho bubalino (Cabeça) da mesma Região de Integração----
    t22 <- reactive({
      ri <- pec2 %>%
        filter(ano == input$pec2ano, localidade == input$pec2muni) %>%
        pull(ri)
      paste0("Efetivo de Rebanho bubalino (Cabeça) dos Municípios, Região de Integração ",
             ri, " - ", input$pec2ano)
    })
    # 3- Efetivo de Rebanho Equino----
    ## Gráfico - Efetivo de Rebanho Equino----
    t31 <- reactive({
      req(input$pec3municomp)
      if (input$pec3municomp == "Selecione um município") {
        paste0("Efetivo de Rebanho Equino, ",
         input$pec3muni, " - ", min(pec3$ano), " a ", max(pec3$ano))
      } else {
        paste0("Efetivo de Rebanho Equino, ",
         input$pec3muni, " x ", input$pec3municomp, input$pec1muni, " - ", min(pec3$ano), " a ", max(pec3$ano))
      }
    })
    
    ## Tabela - Efetivo de Rebanho Equino da mesma Região de Integração----
    t32 <- reactive({
      ri <- pec3 %>%
        filter(ano == input$pec3ano, localidade == input$pec3muni) %>%
        pull(ri)
      paste0("Efetivo de Rebanho Equino dos Municípios, Região de Integração ",
             ri, " - ", input$pec3ano)
    })
    # 4- Efetivo de Rebanho suino total (Cabeça) ----
    ## Gráfico - Efetivo de Rebanho suino total (Cabeça)----
    t41 <- reactive({
      req(input$pec4municomp)
      if (input$pec4municomp == "Selecione um município") {
        paste0("Efetivo de Rebanho suino total (Cabeça), ",
         input$pec4muni, " - ", min(pec4$ano), " a ", max(pec4$ano))
      } else {
        paste0("Efetivo de Rebanho suino total (Cabeça), ",
         input$pec4muni, " x ", input$pec4municomp, input$pec1muni, " - ", min(pec4$ano), " a ", max(pec4$ano))
      }
    })
    ## Tabela - Efetivo de Rebanho suino total (Cabeça) da mesma Região de Integração----
    t42 <- reactive({
      ri <- pec4 %>%
        filter(ano == input$pec4ano, localidade == input$pec4muni) %>%
        pull(ri)
      paste0("Efetivo de Rebanho suino total (Cabeça) dos Municípios, Região de Integração ",
             ri, " - ", input$pec4ano)
    })
    # 5- Efetivo de Rebanho suino total (Matrizes) ----
    ## Gráfico - Efetivo de Rebanho suino total (Matrizes)----
    t51 <- reactive({
      req(input$pec5municomp)
      if (input$pec5municomp == "Selecione um município") {
        paste0("Efetivo de Rebanho suino total (Matrizes), ",
         input$pec5muni, " - ", min(pec5$ano), " a ", max(pec5$ano))
      } else {
        paste0("Efetivo de Rebanho suino total (Matrizes), ",
         input$pec5muni, " x ", input$pec5municomp, input$pec1muni, " - ", min(pec5$ano), " a ", max(pec5$ano))
      }
    })
    ## Tabela - Efetivo de Rebanho suino total (Matrizes) da mesma Região de Integração----
    t52 <- reactive({
      ri <- pec5 %>%
        filter(ano == input$pec5ano, localidade == input$pec5muni) %>%
        pull(ri)
      paste0("Efetivo de Rebanho suino total (Matrizes) dos Municípios, Região de Integração ",
             ri, " - ", input$pec5ano)
    })
    # 6- Efetivo de Rebanho Caprino (cabeça) ----
    ## Gráfico - Efetivo de Rebanho Caprino (cabeça)----
    t61 <- reactive({
      req(input$pec6municomp)
      if (input$pec6municomp == "Selecione um município") {
        paste0("Efetivo de Rebanho Caprino (cabeça), ",
         input$pec6muni, " - ", min(pec6$ano), " a ", max(pec6$ano))
      } else {
        paste0("Efetivo de Rebanho Caprino (cabeça), ",
         input$pec6muni, " x ", input$pec6municomp, input$pec1muni, " - ", min(pec6$ano), " a ", max(pec6$ano))
      }
    })
    ## Tabela - Efetivo de Rebanho Caprino (cabeça) da mesma Região de Integração----
    t62 <- reactive({
      ri <- pec6 %>%
        filter(ano == input$pec6ano, localidade == input$pec6muni) %>%
        pull(ri)
      paste0("Efetivo de Rebanho Caprino (cabeça) dos municípios, Região de Integração ",
             ri, " - ", input$pec6ano)
    })
    # 7- Efetivo de Rebanho Ovino (cabeças) ----
    ## Gráfico - Efetivo de Rebanho Ovino (cabeças)----
    t71 <- reactive({
      req(input$pec7municomp)
      if (input$pec7municomp == "Selecione um município") {
        paste0("Efetivo de Rebanho Ovino (cabeças), ",
         input$pec7muni, " - ", min(pec7$ano), " a ", max(pec7$ano))
      } else {
        paste0("Efetivo de Rebanho Ovino (cabeças), ",
         input$pec7muni, " x ", input$pec7municomp, input$pec1muni, " - ", min(pec7$ano), " a ", max(pec7$ano))
      }
    })
    ## Tabela - Efetivo de Rebanho Ovino (cabeças) da mesma Região de Integração----
    t72 <- reactive({
      ri <- pec7 %>%
        filter(ano == input$pec7ano, localidade == input$pec7muni) %>%
        pull(ri)
      paste0("Efetivo de Rebanho Ovino (cabeças) dos Municípios, Região de Integração ",
             ri, " - ", input$pec7ano)
    })
    # 8- Efetivo de Rebanho Galináceos (Total) ----
    ## Gráfico - Efetivo de Rebanho Galináceos (Total)----
    t81 <- reactive({
      req(input$pec8municomp)
      if (input$pec8municomp == "Selecione um município") {
        paste0("Efetivo de Rebanho Galináceos (Total), ",
         input$pec8muni, " - ", min(pec8$ano), " a ", max(pec8$ano))
      } else {
        paste0("Efetivo de Rebanho Galináceos (Total), ",
         input$pec8muni, " x ", input$pec8municomp, input$pec1muni, " - ", min(pec8$ano), " a ", max(pec8$ano))
      }
    })
    ## Tabela - Efetivo de Rebanho Galináceos (Total) da mesma Região de Integração----
    t82 <- reactive({
      ri <- pec8 %>%
        filter(ano == input$pec8ano, localidade == input$pec8muni) %>%
        pull(ri)
      paste0("Efetivo de Rebanho Galináceos (Total) dos Municípios, Região de Integração ",
             ri, " - ", input$pec8ano)
    })
    # 9- Efetivo de Rebanho Galináceos (Galinhas) ----
    ## Gráfico - Efetivo de Rebanho Galináceos (Galinhas)----
    t91 <- reactive({
      req(input$pec9municomp)
      if (input$pec9municomp == "Selecione um município") {
        paste0("Efetivo de Rebanho Galináceos (Galinhas), ",
         input$pec9muni, " - ", min(pec9$ano), " a ", max(pec9$ano))
      } else {
        paste0("Efetivo de Rebanho Galináceos (Galinhas), ",
         input$pec9muni, " x ", input$pec9municomp, input$pec1muni, " - ", min(pec9$ano), " a ", max(pec9$ano))
      }
    })
    ## Tabela - Efetivo de Rebanho Galináceos (Galinhas) da mesma Região de Integração----
    t92 <- reactive({
      ri <- pec9 %>%
        filter(ano == input$pec9ano, localidade == input$pec9muni) %>%
        pull(ri)
      paste0("Efetivo de Rebanho Galináceos (Galinhas) dos Municípios, Região de Integração ",
             ri, " - ", input$pec9ano)
    })
    # 10- Efetivo de Rebanho Codornas (cabeças) ----
    ## Gráfico - Efetivo de Rebanho Codornas (cabeças)----
    t101 <- reactive({
      req(input$pec10municomp)
      if (input$pec10municomp == "Selecione um município") {
        paste0("Efetivo de Rebanho Codornas (Cabeças), ",
         input$pec10muni, " - ", min(pec10$ano), " a ", max(pec10$ano))
      } else {
        paste0("Efetivo de Rebanho Codornas (Cabeças), ",
         input$pec10muni, " x ", input$pec10municomp, input$pec1muni, " - ", min(pec10$ano), " a ", max(pec10$ano))
      }
    })
    ## Tabela - Efetivo de Rebanho Codornas (cabeças) da mesma Região de Integração----
    t102 <- reactive({
      ri <- pec10 %>%
        filter(ano == input$pec10ano, localidade == input$pec10muni) %>%
        pull(ri)
      paste0("Efetivo de Rebanho Codornas (cabeças) dos Municípios, Região de Integração ",
             ri, " - ", input$pec10ano)
    })

    # 11- Produção de Origem Animal Leite (Mil litros) ----
    ## Gráfico - Produção de Origem Animal Leite (Mil litros)----
    t111 <- reactive({
      req(input$pec11municomp)
      if (input$pec11municomp == "Selecione um município") {
        paste0("Produção de Origem Animal Leite (Mil litros), ",
         input$pec11muni, " - ", min(pec11$ano), " a ", max(pec11$ano))
      } else {
        paste0("Produção de Origem Animal Leite (Mil litros), ",
         input$pec11muni, " x ", input$pec11municomp, input$pec1muni, " - ", min(pec11$ano), " a ", max(pec11$ano))
      }
    })
    ## Tabela - Produção de Origem Animal Leite (Mil litros) da mesma Região de Integração----
    t112 <- reactive({
      ri <- pec11 %>%
        filter(ano == input$pec11ano, localidade == input$pec11muni) %>%
        pull(ri)
      paste0("Produção de Origem Animal Leite (Mil litros) dos Municípios, Região de Integração ",
             ri, " - ", input$pec11ano)
    })
    # 12- Produção de Origem Animal Ovos de galinha (Mil dúzias) ----
    ## Gráfico - Produção de Origem Animal Ovos de galinha (Mil dúzias)----
    t121 <- reactive({
      req(input$pec12municomp)
      if (input$pec12municomp == "Selecione um município") {
        paste0("Produção de Origem Animal Ovos de galinha (Mil dúzias), ",
         input$pec12muni, " - ", min(pec12$ano), " a ", max(pec12$ano))
      } else {
        paste0("Produção de Origem Animal Ovos de galinha (Mil dúzias), ",
         input$pec12muni, " x ", input$pec12municomp, input$pec1muni, " - ", min(pec12$ano), " a ", max(pec12$ano))
      }
    })
    ## Tabela - Produção de Origem Animal Ovos de galinha (Mil dúzias) da mesma Região de Integração----
    t122 <- reactive({
      ri <- pec12 %>%
        filter(ano == input$pec12ano, localidade == input$pec12muni) %>%
        pull(ri)
      paste0("Produção de Origem Animal Ovos de galinha (Mil dúzias) dos Municípios, Região de Integração ",
             ri, " - ", input$pec12ano)
    })
    # 13- Produção de Origem Animal Ovos de codorna (Mil dúzias) ----
    ## Gráfico - Produção de Origem Animal Ovos de codorna (Mil dúzias)----
    t131 <- reactive({
      req(input$pec13municomp)
      if (input$pec13municomp == "Selecione um município") {
        paste0("Produção de Origem Animal Ovos de codorna (Mil dúzias), ",
         input$pec13muni, " - ", min(pec13$ano), " a ", max(pec13$ano))
      } else {
        paste0("Produção de Origem Animal Ovos de codorna (Mil dúzias), ",
         input$pec13muni, " x ", input$pec13municomp, input$pec1muni, " - ", min(pec13$ano), " a ", max(pec13$ano))
      }
    })
    ## Tabela - Produção de Origem Animal Ovos de codorna (Mil dúzias) da mesma Região de Integração----
    t132 <- reactive({
      ri <- pec13 %>%
        filter(ano == input$pec13ano, localidade == input$pec13muni) %>%
        pull(ri)
      paste0("Produção de Origem Animal Ovos de codorna (Mil dúzias) dos Municípios, Região de Integração ",
             ri, " - ", input$pec13ano)
    })
    # 14- Produção de Origem Animal Mel de abelha (Quilogramas) ----
    ## Gráfico - Produção de Origem Animal Mel de abelha (Quilogramas)----
    t141 <- reactive({
      req(input$pec14municomp)
      if (input$pec14municomp == "Selecione um município") {
        paste0("Produção de Origem Animal Mel de abelha (Quilogramas), ",
         input$pec14muni, " - ", min(pec14$ano), " a ", max(pec14$ano))
      } else {
        paste0("Produção de Origem Animal Mel de abelha (Quilogramas), ",
         input$pec14muni, " x ", input$pec14municomp, input$pec1muni, " - ", min(pec14$ano), " a ", max(pec14$ano))
      }
    })
    ## Tabela - Produção de Origem Animal Mel de abelha (Quilogramas) da mesma Região de Integração----
    t142 <- reactive({
      ri <- pec14 %>%
        filter(ano == input$pec14ano, localidade == input$pec14muni) %>%
        pull(ri)
      paste0("Produção de Origem Animal Mel de abelha (Quilogramas) dos Municípios, Região de Integração ",
             ri, " - ", input$pec14ano)
    })
    #VISUALIZAÇÃO----
    # 1- Efetivo de Rebanho bovino (Cabeça) ----
    ## Gráfico - Efetivo de Rebanho bovino (Cabeça)----
    # Atualização da entrada
    pec1comp <- reactive({
      input$pec1muni
    })
    observeEvent(pec1comp(), {
      x <- pec1 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != pec1comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "pec1municomp", choices = c("Selecione um município", choices), session)
    })

    output$pec1txt1 <- renderText({
      t11()
    })

    output$pec1graf <- renderEcharts4r({
      req(input$pec1municomp)
      if (input$pec1municomp == "Selecione um município") {
        a <- pec1 %>% filter(localidade == input$pec1muni)
        a %>%
          e_charts(x = ano) %>%
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
        a <- pec1 %>% filter(localidade == input$pec1muni)
        b <- pec1 %>% filter(localidade == input$pec1municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$pec1muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$pec1municomp,
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
    ## Tabela - Efetivo de Rebanho bovino (Cabeça) da mesma Região de Integração----
    output$pec1txt3 <- renderText({
      t12()
    })
    output$pec1tab2 <- renderReactable({
      ris <- pec1 %>%
        filter(ano == input$pec1ano, localidade == input$pec1muni) %>%
        pull(ri)
      x <- pec1 %>% filter(ano == input$pec1ano, localidade != "Pará")
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
          posicao = colDef(name = "nº", width = 50),
          ri = colDef(name = "Região de Integração"),
          localidade = colDef(name = "Municípios"),
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

    # 2- Efetivo de Rebanho bubalino (Cabeça) ----
    ## Gráfico - Efetivo de Rebanho bubalino (Cabeça)----
    # Atualização da entrada
    pec2comp <- reactive({
      input$pec2muni
    })
    observeEvent(pec2comp(), {
      x <- pec2 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != pec2comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "pec2municomp", choices = c("Selecione um município", choices), session)
    })

    output$pec2txt1 <- renderText({
      t21()
    })

    output$pec2graf <- renderEcharts4r({
      req(input$pec2municomp)
      if (input$pec2municomp == "Selecione um município") {
        a <- pec2 %>% filter(localidade == input$pec2muni)
        a %>%
          e_charts(x = ano) %>%
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
        a <- pec2 %>% filter(localidade == input$pec2muni)
        b <- pec2 %>% filter(localidade == input$pec2municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$pec2muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$pec2municomp,
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
    ## Tabela - Efetivo de Rebanho bubalino (Cabeça) da mesma Região de Integração----
    output$pec2txt2 <- renderText({
      t22()
    })
    output$pec2tab2 <- renderReactable({
      ris <- pec2 %>%
        filter(ano == input$pec2ano, localidade == input$pec2muni) %>%
        pull(ri)
      x <- pec2 %>% filter(ano == input$pec2ano, localidade != "Pará")
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
          posicao = colDef(name = "nº", width = 50),
          ri = colDef(name = "Região de Integração"),
          localidade = colDef(name = "Municípios"),
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
    # 3- Efetivo de Rebanho Equino ----
    ## Gráfico - Efetivo de Rebanho Equino----
    # Atualização da entrada
    pec3comp <- reactive({
      input$pec3muni
    })
    observeEvent(pec3comp(), {
      x <- pec3 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != pec3comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "pec3municomp", choices = c("Selecione um município", choices), session)
    })

    output$pec3txt1 <- renderText({
      t31()
    })

    output$pec3graf <- renderEcharts4r({
      req(input$pec3municomp)
      if (input$pec3municomp == "Selecione um município") {
        a <- pec3 %>% filter(localidade == input$pec3muni)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f2a92a",
            name = "Efetivo",
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
        a <- pec3 %>% filter(localidade == input$pec3muni)
        b <- pec3 %>% filter(localidade == input$pec3municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$pec3muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$pec3municomp,
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
    ## Tabela - Efetivo de Rebanho Equino da mesma Região de Integração----
    output$pec3txt3 <- renderText({
      t32()
    })
    output$pec3tab2 <- renderReactable({
      ris <- pec3 %>%
        filter(ano == input$pec3ano, localidade == input$pec3muni) %>%
        pull(ri)
      x <- pec3 %>% filter(ano == input$pec3ano, localidade != "Pará")
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
          posicao = colDef(name = "nº", width = 50),
          ri = colDef(name = "Região de Integração"),
          localidade = colDef(name = "Municípios"),
          valor = colDef(
            name = "Efetivo",
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

    # 4- Efetivo de Rebanho suino total (Cabeça) ----
    ## Gráfico - Efetivo de Rebanho suino total (Cabeça)----
    # Atualização da entrada
    pec4comp <- reactive({
      input$pec4muni
    })
    observeEvent(pec4comp(), {
      x <- pec4 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != pec4comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "pec4municomp", choices = c("Selecione um município", choices), session)
    })

    output$pec4txt1 <- renderText({
      t41()
    })

    output$pec4graf <- renderEcharts4r({
      req(input$pec4municomp)
      if (input$pec4municomp == "Selecione um município") {
        a <- pec4 %>% filter(localidade == input$pec4muni)
        a %>%
          e_charts(x = ano) %>%
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
        a <- pec4 %>% filter(localidade == input$pec4muni)
        b <- pec4 %>% filter(localidade == input$pec4municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$pec4muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$pec4municomp,
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
    ## Tabela - Efetivo de Rebanho suino total (Cabeça) da mesma Região de Integração----
    output$pec4txt2 <- renderText({
      t42()
    })
    output$pec4tab2 <- renderReactable({
      ris <- pec4 %>%
        filter(ano == input$pec4ano, localidade == input$pec4muni) %>%
        pull(ri)
      x <- pec4 %>% filter(ano == input$pec4ano, localidade != "Pará")
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
          posicao = colDef(name = "nº", width = 50),
          ri = colDef(name = "Região de Integração"),
          localidade = colDef(name = "Municípios"),
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
    # 5- Efetivo de Rebanho suino total (Matrizes) ----
    ## Gráfico - Efetivo de Rebanho suino total (Matrizes)----
    # Atualização da entrada
    pec5comp <- reactive({
      input$pec5muni
    })
    observeEvent(pec5comp(), {
      x <- pec5 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != pec5comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "pec5municomp", choices = c("Selecione um município", choices), session)
    })

    output$pec5txt1 <- renderText({
      t51()
    })

    output$pec5graf <- renderEcharts4r({
      req(input$pec5municomp)
      if (input$pec5municomp == "Selecione um município") {
        a <- pec5 %>% filter(localidade == input$pec5muni)
        a %>%
          e_charts(x = ano) %>%
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
        a <- pec5 %>% filter(localidade == input$pec5muni)
        b <- pec5 %>% filter(localidade == input$pec5municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$pec5muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$pec5municomp,
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
    ## Tabela - Efetivo de Rebanho suino total (Matrizes) da mesma Região de Integração----
    output$pec5txt3 <- renderText({
      t52()
    })
    output$pec5tab2 <- renderReactable({
      ris <- pec5 %>%
        filter(ano == input$pec5ano, localidade == input$pec5muni) %>%
        pull(ri)
      x <- pec5 %>% filter(ano == input$pec5ano, localidade != "Pará")
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
          posicao = colDef(name = "nº", width = 50),
          ri = colDef(name = "Região de Integração"),
          localidade = colDef(name = "Municípios"),
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

    # 6- Efetivo de Rebanho Caprino (cabeça) ----
    ## Gráfico - Efetivo de Rebanho Caprino (cabeça)----
    # Atualização da entrada
    pec6comp <- reactive({
      input$pec6muni
    })
    observeEvent(pec6comp(), {
      x <- pec6 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != pec6comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "pec6municomp", choices = c("Selecione um município", choices), session)
    })

    output$pec6txt1 <- renderText({
      t61()
    })

    output$pec6graf <- renderEcharts4r({
      req(input$pec6municomp)
      if (input$pec6municomp == "Selecione um município") {
        a <- pec6 %>% filter(localidade == input$pec6muni)
        a %>%
          e_charts(x = ano) %>%
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
        a <- pec6 %>% filter(localidade == input$pec6muni)
        b <- pec6 %>% filter(localidade == input$pec6municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$pec6muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$pec6municomp,
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
    ## Tabela - Efetivo de Rebanho Caprino (cabeça) da mesma Região de Integração----
    output$pec6txt2 <- renderText({
      t62()
    })
    output$pec6tab2 <- renderReactable({
      ris <- pec6 %>%
        filter(ano == input$pec6ano, localidade == input$pec6muni) %>%
        pull(ri)
      x <- pec6 %>% filter(ano == input$pec6ano, localidade != "Pará")
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
          posicao = colDef(name = "nº", width = 50),
          ri = colDef(name = "Região de Integração"),
          localidade = colDef(name = "Municípios"),
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
    # 7- Efetivo de Rebanho Ovino (cabeças) ----
    ## Gráfico - Efetivo de Rebanho Ovino (cabeças)----
    # Atualização da entrada
    pec7comp <- reactive({
      input$pec7muni
    })
    observeEvent(pec7comp(), {
      x <- pec7 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != pec7comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "pec7municomp", choices = c("Selecione um município", choices), session)
    })

    output$pec7txt1 <- renderText({
      t71()
    })

    output$pec7graf <- renderEcharts4r({
      req(input$pec7municomp)
      if (input$pec7municomp == "Selecione um município") {
        a <- pec7 %>% filter(localidade == input$pec7muni)
        a %>%
          e_charts(x = ano) %>%
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
        a <- pec7 %>% filter(localidade == input$pec7muni)
        b <- pec7 %>% filter(localidade == input$pec7municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$pec7muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$pec7municomp,
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
    ## Tabela - Efetivo de Rebanho Ovino (cabeças) da mesma Região de Integração----
    output$pec7txt2 <- renderText({
      t72()
    })
    output$pec7tab2 <- renderReactable({
      ris <- pec7 %>%
        filter(ano == input$pec7ano, localidade == input$pec7muni) %>%
        pull(ri)
      x <- pec7 %>% filter(ano == input$pec7ano, localidade != "Pará")
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
          posicao = colDef(name = "nº", width = 50),
          ri = colDef(name = "Região de Integração"),
          localidade = colDef(name = "Municípios"),
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
    # 8- Efetivo de Rebanho Galináceos (Total) ----
    ## Gráfico - Efetivo de Rebanho Galináceos (Total)----
    # Atualização da entrada
    pec8comp <- reactive({
      input$pec8muni
    })
    observeEvent(pec8comp(), {
      x <- pec8 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != pec8comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "pec8municomp", choices = c("Selecione um município", choices), session)
    })

    output$pec8txt1 <- renderText({
      t81()
    })

    output$pec8graf <- renderEcharts4r({
      req(input$pec8municomp)
      if (input$pec8municomp == "Selecione um município") {
        a <- pec8 %>% filter(localidade == input$pec8muni)
        a %>%
          e_charts(x = ano) %>%
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
        a <- pec8 %>% filter(localidade == input$pec8muni)
        b <- pec8 %>% filter(localidade == input$pec8municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$pec8muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$pec8municomp,
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
    ## Tabela - Efetivo de Rebanho Galináceos (Total) da mesma Região de Integração----
    output$pec8txt2 <- renderText({
     t82()
    })
    output$pec8tab2 <- renderReactable({
      ris <- pec8 %>%
        filter(ano == input$pec8ano, localidade == input$pec8muni) %>%
        pull(ri)
      x <- pec8 %>% filter(ano == input$pec8ano, localidade != "Pará")
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
          posicao = colDef(name = "nº", width = 50),
          ri = colDef(name = "Região de Integração"),
          localidade = colDef(name = "Municípios"),
          valor = colDef(
            name = "Efetivo",
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
    # 9- Efetivo de Rebanho Galináceos (Galinhas) ----
    ## Gráfico - Efetivo de Rebanho Galináceos (Galinhas)----
    # Atualização da entrada
    pec9comp <- reactive({
      input$pec9muni
    })
    observeEvent(pec9comp(), {
      x <- pec9 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != pec9comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "pec9municomp", choices = c("Selecione um município", choices), session)
    })

    output$pec9txt1 <- renderText({
      t91()
    })

    output$pec9graf <- renderEcharts4r({
      req(input$pec9municomp)
      if (input$pec9municomp == "Selecione um município") {
        a <- pec9 %>% filter(localidade == input$pec9muni)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f2a92a",
            name = "Galinhas",
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
        a <- pec9 %>% filter(localidade == input$pec9muni)
        b <- pec9 %>% filter(localidade == input$pec9municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$pec9muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$pec9municomp,
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
    ## Tabela - Efetivo de Rebanho Galináceos (Galinhas) da mesma Região de Integração----
    output$pec9txt2 <- renderText({
      t92()
    })
    output$pec9tab2 <- renderReactable({
      ris <- pec9 %>%
        filter(ano == input$pec9ano, localidade == input$pec9muni) %>%
        pull(ri)
      x <- pec9 %>% filter(ano == input$pec9ano, localidade != "Pará")
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
          posicao = colDef(name = "nº", width = 50),
          ri = colDef(name = "Região de Integração"),
          localidade = colDef(name = "Municípios"),
          valor = colDef(
            name = "Galinhas",
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
    # 10- Efetivo de Rebanho Codornas (cabeças) ----
    ## Gráfico - Efetivo de Rebanho Codornas (cabeças)----
    # Atualização da entrada
    pec10comp <- reactive({
      input$pec10muni
    })
    observeEvent(pec10comp(), {
      x <- pec10 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != pec10comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "pec10municomp", choices = c("Selecione um município", choices), session)
    })

    output$pec10txt1 <- renderText({
     t101()
    })

    output$pec10graf <- renderEcharts4r({
      req(input$pec10municomp)
      if (input$pec10municomp == "Selecione um município") {
        a <- pec10 %>% filter(localidade == input$pec10muni)
        a %>%
          e_charts(x = ano) %>%
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
        a <- pec10 %>% filter(localidade == input$pec10muni)
        b <- pec10 %>% filter(localidade == input$pec10municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$pec10muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$pec10municomp,
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
    ## Tabela - Efetivo de Rebanho Codornas (cabeças) da mesma Região de Integração----
    output$pec10txt2 <- renderText({
      t102()
    })
    output$pec10tab2 <- renderReactable({
      ris <- pec10 %>%
        filter(ano == input$pec10ano, localidade == input$pec10muni) %>%
        pull(ri)
      x <- pec10 %>% filter(ano == input$pec10ano, localidade != "Pará")
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
          posicao = colDef(name = "nº", width = 50),
          ri = colDef(name = "Região de Integração"),
          localidade = colDef(name = "Municípios"),
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

    # 11- Produção de Origem Animal Leite (Mil litros) ----
    ## Gráfico - Produção de Origem Animal Leite (Mil litros)----
    # Atualização da entrada
    pec11comp <- reactive({
      input$pec11muni
    })
    observeEvent(pec11comp(), {
      x <- pec11 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != pec11comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "pec11municomp", choices = c("Selecione um município", choices), session)
    })

    output$pec11txt1 <- renderText({
     t111()
    })

    output$pec11graf <- renderEcharts4r({
      req(input$pec11municomp)
      if (input$pec11municomp == "Selecione um município") {
        a <- pec11 %>% filter(localidade == input$pec11muni)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f2a92a",
            name = "Produção(Mil litros)",
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
            name = "Produção(Mil litros)",
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
        a <- pec11 %>% filter(localidade == input$pec11muni)
        b <- pec11 %>% filter(localidade == input$pec11municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$pec11muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$pec11municomp,
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
            name = "Produção(Mil litros)",
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
    ## Tabela - Produção de Origem Animal Leite (Mil litros) da mesma Região de Integração----
    output$pec11txt2 <- renderText({
     t112()
    })
    output$pec11tab2 <- renderReactable({
      ris <- pec11 %>%
        filter(ano == input$pec11ano, localidade == input$pec11muni) %>%
        pull(ri)
      x <- pec11 %>% filter(ano == input$pec11ano, localidade != "Pará")
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
          posicao = colDef(name = "nº", width = 50),
          ri = colDef(name = "Região de Integração"),
          localidade = colDef(name = "Municípios"),
          valor = colDef(
            name = "Produção(Mil litros)",
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
    # 12- Produção de Origem Animal Ovos de galinha (Mil dúzias) ----
    ## Gráfico - Produção de Origem Animal Ovos de galinha (Mil dúzias)----
    # Atualização da entrada
    pec12comp <- reactive({
      input$pec12muni
    })
    observeEvent(pec12comp(), {
      x <- pec12 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != pec12comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "pec12municomp", choices = c("Selecione um município", choices), session)
    })

    output$pec12txt1 <- renderText({
      t121()
    })

    output$pec12graf <- renderEcharts4r({
      req(input$pec12municomp)
      if (input$pec12municomp == "Selecione um município") {
        a <- pec12 %>% filter(localidade == input$pec12muni)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f2a92a",
            name = "Ovos(Mil dúzias)",
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
            name = "Ovos(Mil dúzias)",
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
        a <- pec12 %>% filter(localidade == input$pec12muni)
        b <- pec12 %>% filter(localidade == input$pec12municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$pec12muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$pec12municomp,
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
            name = "Ovos(Mil dúzias)",
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
    ## Tabela - Produção de Origem Animal Ovos de galinha (Mil dúzias) da mesma Região de Integração----
    output$pec12txt2 <- renderText({
      t122()
    })
    output$pec12tab2 <- renderReactable({
      ris <- pec12 %>%
        filter(ano == input$pec12ano, localidade == input$pec12muni) %>%
        pull(ri)
      x <- pec12 %>% filter(ano == input$pec12ano, localidade != "Pará")
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
          posicao = colDef(name = "nº", width = 50),
          ri = colDef(name = "Região de Integração"),
          localidade = colDef(name = "Municípios"),
          valor = colDef(
            name = "Mil dúzias",
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
    # 13- Produção de Origem Animal Ovos de codorna (Mil dúzias) ----
    ## Gráfico - Produção de Origem Animal Ovos de codorna (Mil dúzias)----
    # Atualização da entrada
    pec13comp <- reactive({
      input$pec13muni
    })
    observeEvent(pec13comp(), {
      x <- pec13 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != pec13comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "pec13municomp", choices = c("Selecione um município", choices), session)
    })

    output$pec13txt1 <- renderText({
     t131()
    })

    output$pec13graf <- renderEcharts4r({
      req(input$pec13municomp)
      if (input$pec13municomp == "Selecione um município") {
        a <- pec13 %>% filter(localidade == input$pec13muni)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f2a92a",
            name = "Ovos(Mil dúzias)",
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
            name = "Ovos(Mil dúzias)",
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
        a <- pec13 %>% filter(localidade == input$pec13muni)
        b <- pec13 %>% filter(localidade == input$pec13municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$pec13muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$pec13municomp,
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
            name = "Ovos(Mil dúzias)",
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
    ## Tabela - Produção de Origem Animal Ovos de codorna (Mil dúzias) da mesma Região de Integração----
    output$pec13txt2 <- renderText({
      t132()
    })
    output$pec13tab2 <- renderReactable({
      ris <- pec13 %>%
        filter(ano == input$pec13ano, localidade == input$pec13muni) %>%
        pull(ri)
      x <- pec13 %>% filter(ano == input$pec13ano, localidade != "Pará")
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
          posicao = colDef(name = "nº", width = 50),
          ri = colDef(name = "Região de Integração"),
          localidade = colDef(name = "Municípios"),
          valor = colDef(
            name = "Ovos (Mil dúzias)",
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
    # 14- Produção de Origem Animal Mel de abelha (Quilogramas) ----
    ## Gráfico - Produção de Origem Animal Mel de abelha (Quilogramas)----
    # Atualização da entrada
    pec14comp <- reactive({
      input$pec14muni
    })
    observeEvent(pec14comp(), {
      x <- pec14 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != pec14comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "pec14municomp", choices = c("Selecione um município", choices), session)
    })

    output$pec14txt1 <- renderText({
     t141()
    })

    output$pec14graf <- renderEcharts4r({
      req(input$pec14municomp)
      if (input$pec14municomp == "Selecione um município") {
        a <- pec14 %>% filter(localidade == input$pec14muni)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f2a92a",
            name = "Quilogramas",
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
            name = "Quilogramas",
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
        a <- pec14 %>% filter(localidade == input$pec14muni)
        b <- pec14 %>% filter(localidade == input$pec14municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$pec14muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$pec14municomp,
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
            name = "Quilogramas",
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
    ## Tabela - Produção de Origem Animal Mel de abelha (Quilogramas) da mesma Região de Integração----
    output$pec14txt2 <- renderText({
     t142()
    })
    output$pec14tab2 <- renderReactable({
      ris <- pec14 %>%
        filter(ano == input$pec14ano, localidade == input$pec14muni) %>%
        pull(ri)
      x <- pec14 %>% filter(ano == input$pec14ano, localidade != "Pará")
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
          posicao = colDef(name = "nº", width = 50),
          ri = colDef(name = "Região de Integração"),
          localidade = colDef(name = "Municípios"),
          valor = colDef(
            name = "Quilogramas",
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
    #DOWNLOADS----
    # 1 - Efetivo de Rebanho bovino (Cabeça)----
    ## - Gráfico - Efetivo de Rebanho bovino (Cabeças) no município----
    # Filtra os dados
    pec1_1 <- reactive({
      req(input$pec1municomp)
      if (input$pec1municomp == "Selecione um município") {
        a <- pec1 %>% filter(localidade == input$pec1muni)
        } else {
        a <- pec1 %>% filter(localidade == input$pec1muni)
        b <- pec1 %>% filter(localidade == input$pec1municomp)
        df <- rbind(a,b)}
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec1_1(), {
      t11()
      downset_Server("pec1_1", pec1_1(), t11())
    })
    ## - Tabela - Efetivo de Rebanho bovino (Cabeça) nos municípios----
    # Filtra os dados
    pec1_2 <- reactive({
      ris <- pec1 %>%
        filter(ano == input$pec1ano, localidade == input$pec1muni) %>%
        pull(ri)
      x <- pec1 %>%
        filter(ano == input$pec1ano,
               localidade != "Pará",
               ri == ri) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec1_2(), {
      t12()
      downset_Server("pec1_2", pec1_2(), t12())
    })

    # 2 - Efetivo de Rebanho bubalino (Cabeça)----
    ## - Gráfico - Efetivo de Rebanho bubalino (Cabeça) no município----
    # Filtra os dados
    pec2_1 <- reactive({
      req(input$pec2municomp)
      if (input$pec2municomp == "Selecione um município") {
        a <- pec2 %>% filter(localidade == input$pec2muni)
        } else {
        a <- pec2 %>% filter(localidade == input$pec2muni)
        b <- pec2 %>% filter(localidade == input$pec2municomp)
        df <- rbind(a,b)}
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec2_1(), {
      t21()
      downset_Server("pec2_1", pec2_1(), t21())
    })
    ## - Tabela - Efetivo de Rebanho bubalino (Cabeça) nos municípios----
    # Filtra os dados
    pec2_2 <- reactive({
      ris <- pec2 %>%
        filter(ano == input$pec2ano, localidade == input$pec2muni) %>%
        pull(ri)
      x <- pec2 %>%
        filter(ano == input$pec2ano,
               localidade != "Pará",
               ri == ri) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec2_2(), {
      t22()
      downset_Server("pec2_2", pec2_2(), t22())
    })

    # 3 - Efetivo de Rebanho Equino----
    ## - Gráfico - Efetivo de Rebanho Equino no município----
    # Filtra os dados
    pec3_1 <- reactive({
      req(input$pec3municomp)
      if (input$pec3municomp == "Selecione um município") {
        a <- pec3 %>% filter(localidade == input$pec3muni)
        } else {
        a <- pec3 %>% filter(localidade == input$pec3muni)
        b <- pec3 %>% filter(localidade == input$pec3municomp)
        df <- rbind(a,b)
        }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec3_1(), {
      t31()
      downset_Server("pec3_1", pec3_1(), t31())
    })
    ## - Tabela - Efetivo de Rebanho Equino nos municípios----
    # Filtra os dados
    pec3_2 <- reactive({
      ris <- pec3 %>%
        filter(ano == input$pec3ano, localidade == input$pec3muni) %>%
        pull(ri)
      x <- pec3 %>%
        filter(ano == input$pec3ano,
               localidade != "Pará",
               ri == ri) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec3_2(), {
      t32()
      downset_Server("pec3_2", pec3_2(), t32())
    })

    # 4 - Efetivo de Rebanho suino total (Cabeça)----
    ## - Gráfico - Efetivo de Rebanho suino total (Cabeça) no município----
    # Filtra os dados
    pec4_1 <- reactive({
      req(input$pec4municomp)
      if (input$pec4municomp == "Selecione um município") {
        a <- pec4 %>% filter(localidade == input$pec4muni)
        } else {
        a <- pec4 %>% filter(localidade == input$pec4muni)
        b <- pec4 %>% filter(localidade == input$pec4municomp)
        df <- rbind(a,b)
        }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec4_1(), {
      t41()
      downset_Server("pec4_1", pec4_1(), t41())
    })
    ## - Tabela - Efetivo de Rebanho suino total (Cabeça) nos municípios----
    # Filtra os dados
    pec4_2 <- reactive({
      ris <- pec4 %>%
        filter(ano == input$pec4ano, localidade == input$pec4muni) %>%
        pull(ri)
      x <- pec4 %>%
        filter(ano == input$pec4ano,
               localidade != "Pará",
               ri == ri) %>%
        arrange(desc(valor))
    })
      # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec4_2(), {
      t42()
      downset_Server("pec4_2", pec4_2(), t42())
    })

    # 5 - Efetivo de Rebanho suino total (Matrizes)----
    ## - Gráfico - Efetivo de Rebanho suino total (Matrizes) no município----
    # Filtra os dados
    pec5_1 <- reactive({
      req(input$pec5municomp)
      if (input$pec5municomp == "Selecione um município") {
        a <- pec5 %>% filter(localidade == input$pec5muni)
        } else {
        a <- pec5 %>% filter(localidade == input$pec5muni)
        b <- pec5 %>% filter(localidade == input$pec5municomp)
        df <- rbind(a,b)}
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec5_1(), {
      t51()
      downset_Server("pec5_1", pec5_1(), t51())
    })
    ## - Tabela - Efetivo de Rebanho suino total (Matrizes) nos municípios----
    # Filtra os dados
    pec5_2 <- reactive({
      ris <- pec5 %>%
        filter(ano == input$pec5ano, localidade == input$pec5muni) %>%
        pull(ri)
      x <- pec5 %>%
        filter(ano == input$pec5ano,
               localidade != "Pará",
               ri == ri) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec5_2(), {
      t52()
      downset_Server("pec5_2", pec5_2(), t52())
    })

    # 6 - Efetivo de Rebanho Caprino (cabeças)----
    ## - Gráfico - Efetivo de Rebanho Caprino (cabeça) no município----
    # Filtra os dados
    pec6_1 <- reactive({
      req(input$pec6municomp)
      if (input$pec6municomp == "Selecione um município") {
        a <- pec6 %>% filter(localidade == input$pec6muni)
        } else {
        a <- pec6 %>% filter(localidade == input$pec6muni)
        b <- pec6 %>% filter(localidade == input$pec6municomp)
        df <- rbind(a,b)
        }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec6_1(), {
      t61()
      downset_Server("pec6_1", pec6_1(), t61())
    })
    ## - Tabela - Efetivo de Rebanho Caprino (cabeça) nos municípios----
    # Filtra os dados
    pec6_2 <- reactive({
    ris <- pec6 %>%
        filter(ano == input$pec6ano, localidade == input$pec6muni) %>%
        pull(ri)
      x <- pec6 %>%
        filter(ano == input$pec6ano,
               localidade != "Pará",
               ri == ri) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec6_2(), {
      t62()
      downset_Server("pec6_2", pec6_2(), t62())
    })

    # 7 - Efetivo de Rebanho Ovino (cabeças)----
    ## - Gráfico - Efetivo de Rebanho Ovino (cabeças) no município----
    # Filtra os dados
    pec7_1 <- reactive({
      req(input$pec7municomp)
      if (input$pec7municomp == "Selecione um município") {
        a <- pec7 %>% filter(localidade == input$pec7muni)
        } else {
        a <- pec7 %>% filter(localidade == input$pec7muni)
        b <- pec7 %>% filter(localidade == input$pec7municomp)
        df <- rbind(a,b)
        }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec7_1(), {
      t71()
      downset_Server("pec7_1", pec7_1(), t71())
    })
    ## - Tabela - Efetivo de Rebanho Ovino (cabeças) nos municípios----
    # Filtra os dados
    pec7_2 <- reactive({
      ris <- pec7 %>%
        filter(ano == input$pec7ano, localidade == input$pec7muni) %>%
        pull(ri)
      x <- pec7 %>%
        filter(ano == input$pec7ano,
               localidade != "Pará",
               ri == ri) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec7_2(), {
      t72()
      downset_Server("pec7_2", pec7_2(), t72())
    })

    # 8 - Efetivo de Rebanho Galináceos (Total)----
    ## - Gráfico - Efetivo de Rebanho Galináceos (Total) no município----
    # Filtra os dados
    pec8_1 <- reactive({
      req(input$pec8municomp)
      if (input$pec8municomp == "Selecione um município") {
        a <- pec8 %>% filter(localidade == input$pec8muni)
        } else {
        a <- pec8 %>% filter(localidade == input$pec8muni)
        b <- pec8 %>% filter(localidade == input$pec8municomp)
        df <- rbind(a,b)
        }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec8_1(), {
      t81()
      downset_Server("pec8_1", pec8_1(), t81())
    })
    ## - Tabela - Efetivo de Rebanho Galináceos (Total) nos municípios----
    # Filtra os dados
    pec8_2 <- reactive({
      ris <- pec8 %>%
        filter(ano == input$pec8ano, localidade == input$pec8muni) %>%
        pull(ri)
      x <- pec8 %>%
        filter(ano == input$pec8ano,
               localidade != "Pará",
               ri == ri) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec8_2(), {
      t82()
      downset_Server("pec8_2", pec8_2(), t82())
    })

    # 9 - Efetivo de Rebanho Galináceos (Galinhas)----
    ## - Gráfico - Efetivo de Rebanho Galináceos (Galinhas) no município----
    # Filtra os dados
    pec9_1 <- reactive({
      req(input$pec9municomp)
      if (input$pec9municomp == "Selecione um município") {
        a <- pec9 %>% filter(localidade == input$pec9muni)
        } else {
        a <- pec9 %>% filter(localidade == input$pec9muni)
        b <- pec9 %>% filter(localidade == input$pec9municomp)
        df <- rbind(a,b)
        }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec9_1(), {
     t91()
      downset_Server("pec9_1", pec9_1(), t91())
    })
    ## - Tabela - Efetivo de Rebanho Galináceos (Galinhas) nos municípios----
    # Filtra os dados
    pec9_2 <- reactive({
      ris <- pec9 %>%
        filter(ano == input$pec9ano, localidade == input$pec9muni) %>%
        pull(ri)
      x <- pec9 %>%
        filter(ano == input$pec9ano,
               localidade != "Pará",
               ri == ri) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec9_2(), {
      t92()
      downset_Server("pec9_2", pec9_2(), t92())
    })

    # 10 - Efetivo de Rebanho Codornas (cabeças)----
    ## - Gráfico - Efetivo de Rebanho Codornas (Cabeças) no município----
    # Filtra os dados
    pec10_1 <- reactive({
      req(input$pec10municomp)
      if (input$pec10municomp == "Selecione um município") {
        a <- pec10 %>% filter(localidade == input$pec10muni)
        } else {
        a <- pec10 %>% filter(localidade == input$pec10muni)
        b <- pec10 %>% filter(localidade == input$pec10municomp)
        df <- rbind(a,b)
        }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec10_1(), {
      t101()
      downset_Server("pec10_1", pec10_1(), t101())
    })
    ## - Tabela - Efetivo de Rebanho Codornas (cabeças) nos municípios----
    # Filtra os dados
    pec10_2 <- reactive({
      ris <- pec10 %>%
        filter(ano == input$pec10ano, localidade == input$pec10muni) %>%
        pull(ri)
      x <- pec10 %>%
        filter(ano == input$pec10ano,
               localidade != "Pará",
               ri == ri) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec10_2(), {
      t102()
      downset_Server("pec10_2", pec10_2(), t102())
    })

    # 11 - Produção de Origem Animal Leite (Mil litros)----
    ## - Gráfico - Produção de Origem Animal Leite (Mil litros) no município----
    # Filtra os dados
    pec11_1 <- reactive({
      req(input$pec11municomp)
      if (input$pec11municomp == "Selecione um município") {
        a <- pec11 %>% filter(localidade == input$pec11muni)
        } else {
        a <- pec11 %>% filter(localidade == input$pec11muni)
        b <- pec11 %>% filter(localidade == input$pec11municomp)
        df <- rbind(a,b)
        }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec11_1(), {
      t111()
      downset_Server("pec11_1", pec11_1(), t111())
    })
    ## - Tabela - Produção de Origem Animal Leite (Mil litros) nos municípios----
    # Filtra os dados
    pec11_2 <- reactive({
      ris <- pec11 %>%
        filter(ano == input$pec11ano, localidade == input$pec11muni) %>%
        pull(ri)
      x <- pec11 %>%
        filter(ano == input$pec11ano,
               localidade != "Pará",
               ri == ri) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec11_2(), {
      t112()
      downset_Server("pec11_2", pec11_2(), t112())
    })

    # 12 - Produção de Origem Animal Ovos de galinha (Mil dúzias)----
    ## - Gráfico - Produção de Origem Animal Ovos de galinha (Mil dúzias) no município----
    # Filtra os dados
    pec12_1 <- reactive({
       req(input$pec12municomp)
      if (input$pec12municomp == "Selecione um município") {
        a <- pec12 %>% filter(localidade == input$pec12muni)
        } else {
        a <- pec12 %>% filter(localidade == input$pec12muni)
        b <- pec12 %>% filter(localidade == input$pec12municomp)
        df <- rbind(a,b)
        }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec12_1(), {
      t121()
      downset_Server("pec12_1", pec12_1(), t121())
    })
    ## - Tabela - Produção de Origem Animal Ovos de galinha (Mil dúzias) nos municípios----
    # Filtra os dados
    pec12_2 <- reactive({
      ris <- pec12 %>%
        filter(ano == input$pec12ano, localidade == input$pec12muni) %>%
        pull(ri)
      x <- pec12 %>%
        filter(ano == input$pec12ano,
               localidade != "Pará",
               ri == ri) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec12_2(), {
     t122()
      downset_Server("pec12_2", pec12_2(), t122())
    })

    # 13 - Produção de Origem Animal Ovos de codorna (Mil dúzias)----
    ## - Gráfico - Produção de Origem Animal Ovos de codorna (Mil dúzias) no município----
    # Filtra os dados
    pec13_1 <- reactive({
       req(input$pec13municomp)
      if (input$pec13municomp == "Selecione um município") {
        a <- pec13 %>% filter(localidade == input$pec13muni)
        } else {
        a <- pec13 %>% filter(localidade == input$pec13muni)
        b <- pec13 %>% filter(localidade == input$pec13municomp)
        df <- rbind(a,b)
        }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec13_1(), {
      t131()
      downset_Server("pec13_1", pec13_1(), t131())
    })
    ## - Tabela - Produção de Origem Animal Ovos de codorna (Mil dúzias) nos municípios----
    # Filtra os dados
    pec13_2 <- reactive({
    ris <- pec13 %>%
        filter(ano == input$pec13ano, localidade == input$pec13muni) %>%
        pull(ri)
      x <- pec13 %>%
        filter(ano == input$pec13ano,
               localidade != "Pará",
               ri == ri) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec13_2(), {
      t132()
      downset_Server("pec13_2", pec13_2(), t132())
    })

    # 14 - Produção de Origem Animal Mel de abelha (Quilogramas)----
    ## - Gráfico - Produção de Origem Animal Mel de abelha (Quilogramas) no município----
    # Filtra os dados
    pec14_1 <- reactive({
      req(input$pec14municomp)
      if (input$pec14municomp == "Selecione um município") {
        a <- pec14 %>% filter(localidade == input$pec14muni)
        } else {
        a <- pec14 %>% filter(localidade == input$pec14muni)
        b <- pec14 %>% filter(localidade == input$pec14municomp)
        df <- rbind(a,b)
        }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec14_1(), {
      t141()
      downset_Server("pec14_1", pec14_1(), t141())
    })
    ## - Tabela - Produção de Origem Animal Mel de abelha (Quilogramas) nos municípios----
    # Filtra os dados
    pec14_2 <- reactive({
      ris <- pec14 %>%
        filter(ano == input$pec14ano, localidade == input$pec14muni) %>%
        pull(ri)
      x <- pec14 %>%
        filter(ano == input$pec14ano,
               localidade != "Pará",
               ri == ri) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec14_2(), {
      t142()
      downset_Server("pec14_2", pec14_2(), t142())
    })
    # fim----
  })
}

# Play do Módulo
# ui <- dashboardPage(
#   header = dashboardHeader(),
#   sidebar = dashboardSidebar(),
#   body = dashboardBody(fluidPage(economia_pec_mp_ui("economia_pec_mp")))
# )
# 
# server <- function(input, output) {
#   economia_pec_mp_Server("economia_pec_mp")
# }
# 
# shinyApp(ui, server)
