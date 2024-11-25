# Funções de módulo de Economia Estadual
# Função de UI

economia_pec_pa_ui <- function(id) {
  fluidPage(# CAMINHO DO ARQUIVO CSS----
            tags$head(
              tags$link(rel = "stylesheet", type = "text/css", href = "estilo.css")
            ),
            # Lista de Navegação lateral----
            div(class = "navbar_economia", navbarPage(
              tags$b("Pecuária - Pará"),
              navbarMenu(
                tags$b("Escolha um Indicador"),
                # 1 - Efetivo de Rebanho bovino (Cabeça)----
                tabPanel(
                  "Efetivo de Rebanho bovino (Cabeça)",
                  panel(
                    ## Controle----
                    heading =
                      h4(
                        style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                        "Efetivo de Rebanho bovino (Cabeça)"
                      ),
                    tags$div(
                      class = "seletor1",
                      pickerInput(
                        inputId = NS(id, "pec1ano"),
                        label = "Ano",
                        choices = sort(unique(pec1[["ano"]]), decreasing = T),
                        width = "100px"
                      )
                    ),
                    tags$div(
                      class = "seletor2",
                      pickerInput(
                        inputId = NS(id, "pec1ri"),
                        label = "Pará/Região de Integração",
                        choices = unique(pec1[["ri"]]),
                        width = "200px"
                      )
                    )
                  ),
                  fluidRow(
                    ## Mapa - Efetivo de Rebanho bovino (Cabeça)----
                    box(
                      title = textOutput(NS(id, "pec1txt1")),
                      collapsible = T,
                      collapsed = F,
                      headerBorder = T,
                      width = 12,
                      withSpinner(
                        leafletOutput(NS(id, "pec1map"), height = "600px"),
                        type = 8,
                        color = "#f2a92a",
                        size = 0.5
                      ),
                      footer = list(tags$h6(tags$b("Fonte:"), "IBGE/PPM"), tags$h6(tags$b("Elaboração:"), "FAPESPA"))
                    ),
                    ## Tabela - Efetivo de Rebanho bovino (Cabeça)----
                    box(
                      title = textOutput(NS(id, "pec1txt2")),
                      collapsible = T,
                      collapsed = F,
                      width = 12,
                      headerBorder = T,
                      solidHeader = F,
                      withSpinner(
                        reactableOutput(NS(id, "pec1tab"), height = "400px"),
                        type = 8,
                        color = "#f2a92a",
                        size = 0.5
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA")
                      ), downset_ui(NS(id, "pec1_1")))
                    )
                  ),
                  ## Gráfico - Efetivo de Rebanho bovino (Cabeça)----
                  fluidRow(
                    box(
                      title = textOutput(NS(id, "pec1txt3")),
                      collapsible = T,
                      collapsed = F,
                      width = 12,
                      headerBorder = T,
                      solidHeader = F,
                      withSpinner(
                        echarts4rOutput(NS(id, "pec1graf")),
                        type = 8,
                        color = "#f2a92a",
                        size = 0.5
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA")
                      ), downset_ui(NS(id, "pec1_2")))
                    )
                  )
                ),
                # 2 - Efetivo de Rebanho bubalino (Cabeça)----
                tabPanel(
                  "Efetivo de Rebanho bubalino (Cabeça)",
                  panel(
                    ## Controle----
                    heading =
                      h4(
                        style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                        "Efetivo de Rebanho bubalino (Cabeça)"
                      ),
                    tags$div(
                      class = "seletor1",
                      pickerInput(
                        inputId = NS(id, "pec2ano"),
                        label = "Ano",
                        choices = sort(unique(pec2[["ano"]]), decreasing = T),
                        width = "100px"
                      )
                    ),
                    tags$div(
                      class = "seletor2",
                      pickerInput(
                        inputId = NS(id, "pec2ri"),
                        label = "Pará/Região de Integração",
                        choices = unique(pec2[["ri"]]),
                        width = "200px"
                      )
                    )
                  ),
                  fluidRow(
                    ## Mapa - Efetivo de Rebanho bubalino (Cabeça)----
                    box(
                      title = textOutput(NS(id, "pec2txt1")),
                      collapsible = T,
                      collapsed = F,
                      headerBorder = T,
                      width = 12,
                      withSpinner(
                        leafletOutput(NS(id, "pec2map"), height = "600px"),
                        type = 8,
                        color = "#f2a92a",
                        size = 0.5
                      ),
                      footer = list(tags$h6(tags$b("Fonte:"), "IBGE/PPM"), tags$h6(tags$b("Elaboração:"), "FAPESPA"))
                    ),
                    ## Tabela - Efetivo de Rebanho bubalino (Cabeça)----
                    box(
                      title = textOutput(NS(id, "pec2txt2")),
                      collapsible = T,
                      collapsed = F,
                      width = 12,
                      headerBorder = T,
                      solidHeader = F,
                      withSpinner(
                        reactableOutput(NS(id, "pec2tab"), height = "400px"),
                        type = 8,
                        color = "#f2a92a",
                        size = 0.5
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA")
                      ), downset_ui(NS(id, "pec2_1")))
                    )
                  ),
                  ## Gráfico - Efetivo de Rebanho bubalino (Cabeça)----
                  fluidRow(
                    box(
                      title = textOutput(NS(id, "pec2txt3")),
                      collapsible = T,
                      collapsed = F,
                      width = 12,
                      headerBorder = T,
                      solidHeader = F,
                      withSpinner(
                        echarts4rOutput(NS(id, "pec2graf")),
                        type = 8,
                        color = "#f2a92a",
                        size = 0.5
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA")
                      ), downset_ui(NS(id, "pec2_2")))
                    )
                  )
                ),
                # 3 - Efetivo de Rebanho Equino----
                tabPanel(
                  "Efetivo de Rebanho Equino",
                  panel(
                    ## Controle----
                    heading =
                      h4(
                        style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                        "Efetivo de Rebanho Equino"
                      ),
                    tags$div(
                      class = "seletor1",
                      pickerInput(
                        inputId = NS(id, "pec3ano"),
                        label = "Ano",
                        choices = sort(unique(pec3[["ano"]]), decreasing = T),
                        width = "100px"
                      )
                    ),
                    tags$div(
                      class = "seletor2",
                      pickerInput(
                        inputId = NS(id, "pec3ri"),
                        label = "Pará/Região de Integração",
                        choices = unique(pec3[["ri"]]),
                        width = "200px"
                      )
                    )
                  ),
                  fluidRow(
                    ## Mapa - Efetivo de Rebanho Equino----
                    box(
                      title = textOutput(NS(id, "pec3txt1")),
                      collapsible = T,
                      collapsed = F,
                      headerBorder = T,
                      width = 12,
                      withSpinner(
                        leafletOutput(NS(id, "pec3map"), height = "600px"),
                        type = 8,
                        color = "#f2a92a",
                        size = 0.5
                      ),
                      footer = list(tags$h6(tags$b("Fonte:"), "IBGE/PPM"), tags$h6(tags$b("Elaboração:"), "FAPESPA"))
                    ),
                    ## Tabela - Efetivo de Rebanho Equino----
                    box(
                      title = textOutput(NS(id, "pec3txt2")),
                      collapsible = T,
                      collapsed = F,
                      width = 12,
                      headerBorder = T,
                      solidHeader = F,
                      withSpinner(
                        reactableOutput(NS(id, "pec3tab"), height = "400px"),
                        type = 8,
                        color = "#f2a92a",
                        size = 0.5
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA")
                      ), downset_ui(NS(id, "pec3_1")))
                    )
                  ),
                  ## Gráfico - Efetivo de Rebanho Equino----
                  fluidRow(
                    box(
                      title = textOutput(NS(id, "pec3txt3")),
                      collapsible = T,
                      collapsed = F,
                      width = 12,
                      headerBorder = T,
                      solidHeader = F,
                      withSpinner(
                        echarts4rOutput(NS(id, "pec3graf")),
                        type = 8,
                        color = "#f2a92a",
                        size = 0.5
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA")
                      ), downset_ui(NS(id, "pec3_2")))
                    )
                  )
                ),
                # 4 - Efetivo de Rebanho suino total (Cabeça)----
                tabPanel(
                  "Efetivo de Rebanho suino total (Cabeça)",
                  panel(
                    ## Controle----
                    heading =
                      h4(
                        style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                        "Efetivo de Rebanho suino total (Cabeça)"
                      ),
                    tags$div(
                      class = "seletor1",
                      pickerInput(
                        inputId = NS(id, "pec4ano"),
                        label = "Ano",
                        choices = sort(unique(pec4[["ano"]]), decreasing = T),
                        width = "100px"
                      )
                    ),
                    tags$div(
                      class = "seletor2",
                      pickerInput(
                        inputId = NS(id, "pec4ri"),
                        label = "Pará/Região de Integração",
                        choices = unique(pec4[["ri"]]),
                        width = "200px"
                      )
                    )
                  ),
                  fluidRow(
                    ## Mapa - Efetivo de Rebanho suino total (Cabeça)----
                    box(
                      title = textOutput(NS(id, "pec4txt1")),
                      collapsible = T,
                      collapsed = F,
                      headerBorder = T,
                      width = 12,
                      withSpinner(
                        leafletOutput(NS(id, "pec4map"), height = "600px"),
                        type = 8,
                        color = "#f2a92a",
                        size = 0.5
                      ),
                      footer = list(tags$h6(tags$b("Fonte:"), "IBGE/PPM"), tags$h6(tags$b("Elaboração:"), "FAPESPA"))
                    ),
                    ## Tabela - Efetivo de Rebanho suino total (Cabeça)----
                    box(
                      title = textOutput(NS(id, "pec4txt2")),
                      collapsible = T,
                      collapsed = F,
                      width = 12,
                      headerBorder = T,
                      solidHeader = F,
                      withSpinner(
                        reactableOutput(NS(id, "pec4tab"), height = "400px"),
                        type = 8,
                        color = "#f2a92a",
                        size = 0.5
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA")
                      ), downset_ui(NS(id, "pec4_1")))
                    )
                  ),
                  ## Gráfico - Efetivo de Rebanho suino total (Cabeça)----
                  fluidRow(
                    box(
                      title = textOutput(NS(id, "pec4txt3")),
                      collapsible = T,
                      collapsed = F,
                      width = 12,
                      headerBorder = T,
                      solidHeader = F,
                      withSpinner(
                        echarts4rOutput(NS(id, "pec4graf")),
                        type = 8,
                        color = "#f2a92a",
                        size = 0.5
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA")
                      ), downset_ui(NS(id, "pec4_2")))
                    )
                  )
                ),
                # 5 - Efetivo de Rebanho suino total (Matrizes)----
                tabPanel(
                  "Efetivo de Rebanho suino total (Matrizes)",
                  panel(
                    ## Controle----
                    heading =
                      h4(
                        style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                        "Efetivo de Rebanho suino total (Matrizes)"
                      ),
                    tags$div(
                      class = "seletor1",
                      pickerInput(
                        inputId = NS(id, "pec5ano"),
                        label = "Ano",
                        choices = sort(unique(pec5[["ano"]]), decreasing = T),
                        width = "100px"
                      )
                    ),
                    tags$div(
                      class = "seletor2",
                      pickerInput(
                        inputId = NS(id, "pec5ri"),
                        label = "Pará/Região de Integração",
                        choices = unique(pec5[["ri"]]),
                        width = "200px"
                      )
                    )
                  ),
                  fluidRow(
                    ## Mapa - Efetivo de Rebanho suino total (Matrizes)----
                    box(
                      title = textOutput(NS(id, "pec5txt1")),
                      collapsible = T,
                      collapsed = F,
                      headerBorder = T,
                      width = 12,
                      withSpinner(
                        leafletOutput(NS(id, "pec5map"), height = "600px"),
                        type = 8,
                        color = "#f2a92a",
                        size = 0.5
                      ),
                      footer = list(tags$h6(tags$b("Fonte:"), "IBGE/PPM"), tags$h6(tags$b("Elaboração:"), "FAPESPA"))
                    ),
                    ## Tabela - Efetivo de Rebanho suino total (Matrizes)----
                    box(
                      title = textOutput(NS(id, "pec5txt2")),
                      collapsible = T,
                      collapsed = F,
                      width = 12,
                      headerBorder = T,
                      solidHeader = F,
                      withSpinner(
                        reactableOutput(NS(id, "pec5tab"), height = "400px"),
                        type = 8,
                        color = "#f2a92a",
                        size = 0.5
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA")
                      ), downset_ui(NS(id, "pec5_1")))
                    )
                  ),
                  ## Gráfico - Efetivo de Rebanho suino total (Matrizes)----
                  fluidRow(
                    box(
                      title = textOutput(NS(id, "pec5txt3")),
                      collapsible = T,
                      collapsed = F,
                      width = 12,
                      headerBorder = T,
                      solidHeader = F,
                      withSpinner(
                        echarts4rOutput(NS(id, "pec5graf")),
                        type = 8,
                        color = "#f2a92a",
                        size = 0.5
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA")
                      ), downset_ui(NS(id, "pec5_2")))
                    )
                  )
                ),
                # 6 - Efetivo de Rebanho Caprino (cabeças)----
                tabPanel(
                  "Efetivo de Rebanho Caprino (cabeças)",
                  panel(
                    ## Controle----
                    heading =
                      h4(
                        style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                        "Efetivo de Rebanho Caprino (cabeças)"
                      ),
                    tags$div(
                      class = "seletor1",
                      pickerInput(
                        inputId = NS(id, "pec6ano"),
                        label = "Ano",
                        choices = sort(unique(pec6[["ano"]]), decreasing = T),
                        width = "100px"
                      )
                    ),
                    tags$div(
                      class = "seletor2",
                      pickerInput(
                        inputId = NS(id, "pec6ri"),
                        label = "Pará/Região de Integração",
                        choices = unique(pec6[["ri"]]),
                        width = "200px"
                      )
                    )
                  ),
                  fluidRow(
                    ## Mapa - Efetivo de Rebanho Caprino (cabeças)----
                    box(
                      title = textOutput(NS(id, "pec6txt1")),
                      collapsible = T,
                      collapsed = F,
                      headerBorder = T,
                      width = 12,
                      withSpinner(
                        leafletOutput(NS(id, "pec6map"), height = "600px"),
                        type = 8,
                        color = "#f2a92a",
                        size = 0.5
                      ),
                      footer = list(tags$h6(tags$b("Fonte:"), "IBGE/PPM"), tags$h6(tags$b("Elaboração:"), "FAPESPA"))
                    ),
                    ## Tabela - Efetivo de Rebanho Caprino (cabeças)----
                    box(
                      title = textOutput(NS(id, "pec6txt2")),
                      collapsible = T,
                      collapsed = F,
                      width = 12,
                      headerBorder = T,
                      solidHeader = F,
                      withSpinner(
                        reactableOutput(NS(id, "pec6tab"), height = "400px"),
                        type = 8,
                        color = "#f2a92a",
                        size = 0.5
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA")
                      ), downset_ui(NS(id, "pec6_1")))
                    )
                  ),
                  ## Gráfico - Efetivo de Rebanho Caprino (cabeças)----
                  fluidRow(
                    box(
                      title = textOutput(NS(id, "pec6txt3")),
                      collapsible = T,
                      collapsed = F,
                      width = 12,
                      headerBorder = T,
                      solidHeader = F,
                      withSpinner(
                        echarts4rOutput(NS(id, "pec6graf")),
                        type = 8,
                        color = "#f2a92a",
                        size = 0.5
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA")
                      ), downset_ui(NS(id, "pec6_2")))
                    )
                  )
                ),
                # 7 - Efetivo de Rebanho Ovino (cabeças)----
                tabPanel(
                  "Efetivo de Rebanho Ovino (cabeças)",
                  panel(
                    ## Controle----
                    heading =
                      h4(
                        style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                        "Efetivo de Rebanho Ovino (cabeças)"
                      ),
                    tags$div(
                      class = "seletor1",
                      pickerInput(
                        inputId = NS(id, "pec7ano"),
                        label = "Ano",
                        choices = sort(unique(pec7[["ano"]]), decreasing = T),
                        width = "100px"
                      )
                    ),
                    tags$div(
                      class = "seletor2",
                      pickerInput(
                        inputId = NS(id, "pec7ri"),
                        label = "Pará/Região de Integração",
                        choices = unique(pec7[["ri"]]),
                        width = "200px"
                      )
                    )
                  ),
                  fluidRow(
                    ## Mapa - Efetivo de Rebanho Ovino (cabeças)----
                    box(
                      title = textOutput(NS(id, "pec7txt1")),
                      collapsible = T,
                      collapsed = F,
                      headerBorder = T,
                      width = 12,
                      withSpinner(
                        leafletOutput(NS(id, "pec7map"), height = "600px"),
                        type = 8,
                        color = "#f2a92a",
                        size = 0.5
                      ),
                      footer = list(tags$h6(tags$b("Fonte:"), "IBGE/PPM"), tags$h6(tags$b("Elaboração:"), "FAPESPA"))
                    ),
                    ## Tabela - Efetivo de Rebanho Ovino (cabeças)----
                    box(
                      title = textOutput(NS(id, "pec7txt2")),
                      collapsible = T,
                      collapsed = F,
                      width = 12,
                      headerBorder = T,
                      solidHeader = F,
                      withSpinner(
                        reactableOutput(NS(id, "pec7tab"), height = "400px"),
                        type = 8,
                        color = "#f2a92a",
                        size = 0.5
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA")
                      ), downset_ui(NS(id, "pec7_1")))
                    )
                  ),
                  ## Gráfico - Efetivo de Rebanho Ovino (cabeças)----
                  fluidRow(
                    box(
                      title = textOutput(NS(id, "pec7txt3")),
                      collapsible = T,
                      collapsed = F,
                      width = 12,
                      headerBorder = T,
                      solidHeader = F,
                      withSpinner(
                        echarts4rOutput(NS(id, "pec7graf")),
                        type = 8,
                        color = "#f2a92a",
                        size = 0.5
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA")
                      ), downset_ui(NS(id, "pec7_2")))
                    )
                  )
                ),
                # 8 - Efetivo de Rebanho Galináceos (Total)----
                tabPanel(
                  "Efetivo de Rebanho Galináceos (Total)",
                  panel(
                    ## Controle----
                    heading =
                      h4(
                        style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                        "Efetivo de Rebanho Galináceos (Total)"
                      ),
                    tags$div(
                      class = "seletor1",
                      pickerInput(
                        inputId = NS(id, "pec8ano"),
                        label = "Ano",
                        choices = sort(unique(pec8[["ano"]]), decreasing = T),
                        width = "100px"
                      )
                    ),
                    tags$div(
                      class = "seletor2",
                      pickerInput(
                        inputId = NS(id, "pec8ri"),
                        label = "Pará/Região de Integração",
                        choices = unique(pec8[["ri"]]),
                        width = "200px"
                      )
                    )
                  ),
                  fluidRow(
                    ## Mapa - Efetivo de Rebanho Galináceos (Total)----
                    box(
                      title = textOutput(NS(id, "pec8txt1")),
                      collapsible = T,
                      collapsed = F,
                      headerBorder = T,
                      width = 12,
                      withSpinner(
                        leafletOutput(NS(id, "pec8map"), height = "600px"),
                        type = 8,
                        color = "#f2a92a",
                        size = 0.5
                      ),
                      footer = list(tags$h6(tags$b("Fonte:"), "IBGE/PPM"), tags$h6(tags$b("Elaboração:"), "FAPESPA"))
                    ),
                    ## Tabela - Efetivo de Rebanho Galináceos (Total)----
                    box(
                      title = textOutput(NS(id, "pec8txt2")),
                      collapsible = T,
                      collapsed = F,
                      width = 12,
                      headerBorder = T,
                      solidHeader = F,
                      withSpinner(
                        reactableOutput(NS(id, "pec8tab"), height = "400px"),
                        type = 8,
                        color = "#f2a92a",
                        size = 0.5
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA")
                      ), downset_ui(NS(id, "pec8_1")))
                    )
                  ),
                  ## Gráfico - Efetivo de Rebanho Galináceos (Total)----
                  fluidRow(
                    box(
                      title = textOutput(NS(id, "pec8txt3")),
                      collapsible = T,
                      collapsed = F,
                      width = 12,
                      headerBorder = T,
                      solidHeader = F,
                      withSpinner(
                        echarts4rOutput(NS(id, "pec8graf")),
                        type = 8,
                        color = "#f2a92a",
                        size = 0.5
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA")
                      ), downset_ui(NS(id, "pec8_2")))
                    )
                  )
                ),
                # 9 - Efetivo de Rebanho Galináceos (Galinhas)----
                tabPanel(
                  "Efetivo de Rebanho Galináceos (Galinhas)",
                  panel(
                    ## Controle----
                    heading =
                      h4(
                        style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                        "Efetivo de Rebanho Galináceos (Galinhas)"
                      ),
                    tags$div(
                      class = "seletor1",
                      pickerInput(
                        inputId = NS(id, "pec9ano"),
                        label = "Ano",
                        choices = sort(unique(pec9[["ano"]]), decreasing = T),
                        width = "100px"
                      )
                    ),
                    tags$div(
                      class = "seletor2",
                      pickerInput(
                        inputId = NS(id, "pec9ri"),
                        label = "Pará/Região de Integração",
                        choices = unique(pec9[["ri"]]),
                        width = "200px"
                      )
                    )
                  ),
                  fluidRow(
                    ## Mapa - Efetivo de Rebanho Galináceos (Galinhas)----
                    box(
                      title = textOutput(NS(id, "pec9txt1")),
                      collapsible = T,
                      collapsed = F,
                      headerBorder = T,
                      width = 12,
                      withSpinner(
                        leafletOutput(NS(id, "pec9map"), height = "600px"),
                        type = 8,
                        color = "#f2a92a",
                        size = 0.5
                      ),
                      footer = list(tags$h6(tags$b("Fonte:"), "IBGE/PPM"), tags$h6(tags$b("Elaboração:"), "FAPESPA"))
                    ),
                    ## Tabela - Efetivo de Rebanho Galináceos (Galinhas)----
                    box(
                      title = textOutput(NS(id, "pec9txt2")),
                      collapsible = T,
                      collapsed = F,
                      width = 12,
                      headerBorder = T,
                      solidHeader = F,
                      withSpinner(
                        reactableOutput(NS(id, "pec9tab"), height = "400px"),
                        type = 8,
                        color = "#f2a92a",
                        size = 0.5
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA")
                      ), downset_ui(NS(id, "pec9_1")))
                    )
                  ),
                  ## Gráfico - Efetivo de Rebanho Galináceos (Galinhas)----
                  fluidRow(
                    box(
                      title = textOutput(NS(id, "pec9txt3")),
                      collapsible = T,
                      collapsed = F,
                      width = 12,
                      headerBorder = T,
                      solidHeader = F,
                      withSpinner(
                        echarts4rOutput(NS(id, "pec9graf")),
                        type = 8,
                        color = "#f2a92a",
                        size = 0.5
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA")
                      ), downset_ui(NS(id, "pec9_2")))
                    )
                  )
                ),
                # 10 - Efetivo de Rebanho Codornas (cabeças)----
                tabPanel(
                  "Efetivo de Rebanho Codornas (cabeças)",
                  panel(
                    ## Controle----
                    heading =
                      h4(
                        style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                        "Efetivo de Rebanho Codornas (cabeças)"
                      ),
                    tags$div(
                      class = "seletor1",
                      pickerInput(
                        inputId = NS(id, "pec10ano"),
                        label = "Ano",
                        choices = sort(unique(pec9[["ano"]]), decreasing = T),
                        width = "100px"
                      )
                    ),
                    tags$div(
                      class = "seletor2",
                      pickerInput(
                        inputId = NS(id, "pec10ri"),
                        label = "Pará/Região de Integração",
                        choices = unique(pec10[["ri"]]),
                        width = "200px"
                      )
                    )
                  ),
                  fluidRow(
                    ## Mapa - Efetivo de Rebanho Codornas (cabeças)----
                    box(
                      title = textOutput(NS(id, "pec10txt1")),
                      collapsible = T,
                      collapsed = F,
                      headerBorder = T,
                      width = 12,
                      withSpinner(
                        leafletOutput(NS(id, "pec10map"), height = "600px"),
                        type = 8,
                        color = "#f2a92a",
                        size = 0.5
                      ),
                      footer = list(tags$h6(tags$b("Fonte:"), "IBGE/PPM"), tags$h6(tags$b("Elaboração:"), "FAPESPA"))
                    ),
                    ## Tabela - Efetivo de Rebanho Codornas (cabeças)----
                    box(
                      title = textOutput(NS(id, "pec10txt2")),
                      collapsible = T,
                      collapsed = F,
                      width = 12,
                      headerBorder = T,
                      solidHeader = F,
                      withSpinner(
                        reactableOutput(NS(id, "pec10tab"), height = "400px"),
                        type = 8,
                        color = "#f2a92a",
                        size = 0.5
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA")
                      ), downset_ui(NS(id, "pec10_1")))
                    )
                  ),
                  ## Gráfico - Efetivo de Rebanho Codornas (cabeças)----
                  fluidRow(
                    box(
                      title = textOutput(NS(id, "pec10txt3")),
                      collapsible = T,
                      collapsed = F,
                      width = 12,
                      headerBorder = T,
                      solidHeader = F,
                      withSpinner(
                        echarts4rOutput(NS(id, "pec10graf")),
                        type = 8,
                        color = "#f2a92a",
                        size = 0.5
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA")
                      ), downset_ui(NS(id, "pec10_2")))
                    )
                  )
                ),
                # 11 - Produção de Origem Animal Leite (Mil litros)----
                tabPanel(
                  "Produção de Origem Animal Leite (Mil litros)",
                  panel(
                    ## Controle----
                    heading =
                      h4(
                        style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                        "Produção de Origem Animal Leite (Mil litros)"
                      ),
                    tags$div(
                      class = "seletor1",
                      pickerInput(
                        inputId = NS(id, "pec11ano"),
                        label = "Ano",
                        choices = sort(unique(pec11[["ano"]]), decreasing = T),
                        width = "100px"
                      )
                    ),
                    tags$div(
                      class = "seletor2",
                      pickerInput(
                        inputId = NS(id, "pec11ri"),
                        label = "Pará/Região de Integração",
                        choices = unique(pec11[["ri"]]),
                        width = "200px"
                      )
                    )
                  ),
                  fluidRow(
                    ## Mapa - Produção de Origem Animal Leite (Mil litros)----
                    box(
                      title = textOutput(NS(id, "pec11txt1")),
                      collapsible = T,
                      collapsed = F,
                      headerBorder = T,
                      width = 12,
                      withSpinner(
                        leafletOutput(NS(id, "pec11map"), height = "600px"),
                        type = 8,
                        color = "#f2a92a",
                        size = 0.5
                      ),
                      footer = list(tags$h6(tags$b("Fonte:"), "IBGE/PPM"), tags$h6(tags$b("Elaboração:"), "FAPESPA"))
                    ),
                    ## Tabela - Produção de Origem Animal Leite (Mil litros)----
                    box(
                      title = textOutput(NS(id, "pec11txt2")),
                      collapsible = T,
                      collapsed = F,
                      width = 12,
                      headerBorder = T,
                      solidHeader = F,
                      withSpinner(
                        reactableOutput(NS(id, "pec11tab"), height = "400px"),
                        type = 8,
                        color = "#f2a92a",
                        size = 0.5
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA")
                      ), downset_ui(NS(id, "pec11_1")))
                    )
                  ),
                  ## Gráfico - Produção de Origem Animal Leite (Mil litros)----
                  fluidRow(
                    box(
                      title = textOutput(NS(id, "pec11txt3")),
                      collapsible = T,
                      collapsed = F,
                      width = 12,
                      headerBorder = T,
                      solidHeader = F,
                      withSpinner(
                        echarts4rOutput(NS(id, "pec11graf")),
                        type = 8,
                        color = "#f2a92a",
                        size = 0.5
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA")
                      ), downset_ui(NS(id, "pec11_2")))
                    )
                  )
                ),
                # 12 - Produção de Origem Animal Ovos de galinha (Mil dúzias)----
                tabPanel(
                  "Produção de Origem Animal Ovos de galinha (Mil dúzias)",
                  panel(
                    ## Controle----
                    heading =
                      h4(
                        style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                        "Produção de Origem Animal Ovos de galinha (Mil dúzias)"
                      ),
                    tags$div(
                      class = "seletor1",
                      pickerInput(
                        inputId = NS(id, "pec12ano"),
                        label = "Ano",
                        choices = sort(unique(pec12[["ano"]]), decreasing = T),
                        width = "100px"
                      )
                    ),
                    tags$div(
                      class = "seletor2",
                      pickerInput(
                        inputId = NS(id, "pec12ri"),
                        label = "Pará/Região de Integração",
                        choices = unique(pec12[["ri"]]),
                        width = "200px"
                      )
                    )
                  ),
                  fluidRow(
                    ## Mapa - Produção de Origem Animal Ovos de galinha (Mil dúzias)----
                    box(
                      title = textOutput(NS(id, "pec12txt1")),
                      collapsible = T,
                      collapsed = F,
                      headerBorder = T,
                      width = 12,
                      withSpinner(
                        leafletOutput(NS(id, "pec12map"), height = "600px"),
                        type = 8,
                        color = "#f2a92a",
                        size = 0.5
                      ),
                      footer = list(tags$h6(tags$b("Fonte:"), "IBGE/PPM"), tags$h6(tags$b("Elaboração:"), "FAPESPA"))
                    ),
                    ## Tabela - Produção de Origem Animal Ovos de galinha (Mil dúzias)----
                    box(
                      title = textOutput(NS(id, "pec12txt2")),
                      collapsible = T,
                      collapsed = F,
                      width = 12,
                      headerBorder = T,
                      solidHeader = F,
                      withSpinner(
                        reactableOutput(NS(id, "pec12tab"), height = "400px"),
                        type = 8,
                        color = "#f2a92a",
                        size = 0.5
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA")
                      ), downset_ui(NS(id, "pec12_1")))
                    )
                  ),
                  ## Gráfico - Produção de Origem Animal Ovos de galinha (Mil dúzias)----
                  fluidRow(
                    box(
                      title = textOutput(NS(id, "pec12txt3")),
                      collapsible = T,
                      collapsed = F,
                      width = 12,
                      headerBorder = T,
                      solidHeader = F,
                      withSpinner(
                        echarts4rOutput(NS(id, "pec12graf")),
                        type = 8,
                        color = "#f2a92a",
                        size = 0.5
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA")
                      ), downset_ui(NS(id, "pec12_2")))
                    )
                  )
                ),
                # 13 - Produção de Origem Animal Ovos de codorna (Mil dúzias)----
                tabPanel(
                  "Produção de Origem Animal Ovos de codorna (Mil dúzias)",
                  panel(
                    ## Controle----
                    heading =
                      h4(
                        style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                        "Produção de Origem Animal Ovos de codorna (Mil dúzias)"
                      ),
                    tags$div(
                      class = "seletor1",
                      pickerInput(
                        inputId = NS(id, "pec13ano"),
                        label = "Ano",
                        choices = sort(unique(pec13[["ano"]]), decreasing = T),
                        width = "100px"
                      )
                    ),
                    tags$div(
                      class = "seletor2",
                      pickerInput(
                        inputId = NS(id, "pec13ri"),
                        label = "Pará/Região de Integração",
                        choices = unique(pec13[["ri"]]),
                        width = "200px"
                      )
                    )
                  ),
                  fluidRow(
                    ## Mapa - Produção de Origem Animal Ovos de codorna (Mil dúzias)----
                    box(
                      title = textOutput(NS(id, "pec13txt1")),
                      collapsible = T,
                      collapsed = F,
                      headerBorder = T,
                      width = 12,
                      withSpinner(
                        leafletOutput(NS(id, "pec13map"), height = "600px"),
                        type = 8,
                        color = "#f2a92a",
                        size = 0.5
                      ),
                      footer = list(tags$h6(tags$b("Fonte:"), "IBGE/PPM"), tags$h6(tags$b("Elaboração:"), "FAPESPA"))
                    ),
                    ## Tabela - Produção de Origem Animal Ovos de codorna (Mil dúzias)----
                    box(
                      title = textOutput(NS(id, "pec13txt2")),
                      collapsible = T,
                      collapsed = F,
                      width = 12,
                      headerBorder = T,
                      solidHeader = F,
                      withSpinner(
                        reactableOutput(NS(id, "pec13tab"), height = "400px"),
                        type = 8,
                        color = "#f2a92a",
                        size = 0.5
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA")
                      ), downset_ui(NS(id, "pec13_1")))
                    )
                  ),
                  ## Gráfico - Produção de Origem Animal Ovos de codorna (Mil dúzias)----
                  fluidRow(
                    box(
                      title = textOutput(NS(id, "pec13txt3")),
                      collapsible = T,
                      collapsed = F,
                      width = 12,
                      headerBorder = T,
                      solidHeader = F,
                      withSpinner(
                        echarts4rOutput(NS(id, "pec13graf")),
                        type = 8,
                        color = "#f2a92a",
                        size = 0.5
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA")
                      ), downset_ui(NS(id, "pec13_2")))
                    )
                  )
                ),
                # 14 - Produção de Origem Animal Mel de abelha (Quilogramas)----
                tabPanel(
                  "Produção de Origem Animal Mel de abelha (Quilogramas)",
                  panel(
                    ## Controle----
                    heading =
                      h4(
                        style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                        "Produção de Origem Animal Mel de abelha (Quilogramas)"
                      ),
                    tags$div(
                      class = "seletor1",
                      pickerInput(
                        inputId = NS(id, "pec14ano"),
                        label = "Ano",
                        choices = sort(unique(pec14[["ano"]]), decreasing = T),
                        width = "100px"
                      )
                    ),
                    tags$div(
                      class = "seletor2",
                      pickerInput(
                        inputId = NS(id, "pec14ri"),
                        label = "Pará/Região de Integração",
                        choices = unique(pec14[["ri"]]),
                        width = "200px"
                      )
                    )
                  ),
                  fluidRow(
                    ## Mapa - Produção de Origem Animal Mel de abelha (Quilogramas)----
                    box(
                      title = textOutput(NS(id, "pec14txt1")),
                      collapsible = T,
                      collapsed = F,
                      headerBorder = T,
                      width = 12,
                      withSpinner(
                        leafletOutput(NS(id, "pec14map"), height = "600px"),
                        type = 8,
                        color = "#f2a92a",
                        size = 0.5
                      ),
                      footer = list(tags$h6(tags$b("Fonte:"), "IBGE/PPM"), tags$h6(tags$b("Elaboração:"), "FAPESPA"))
                    ),
                    ## Tabela - Produção de Origem Animal Mel de abelha (Quilogramas)----
                    box(
                      title = textOutput(NS(id, "pec14txt2")),
                      collapsible = T,
                      collapsed = F,
                      width = 12,
                      headerBorder = T,
                      solidHeader = F,
                      withSpinner(
                        reactableOutput(NS(id, "pec14tab"), height = "400px"),
                        type = 8,
                        color = "#f2a92a",
                        size = 0.5
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA")
                      ), downset_ui(NS(id, "pec14_1")))
                    )
                  ),
                  ## Gráfico - Produção de Origem Animal Mel de abelha (Quilogramas)----
                  fluidRow(
                    box(
                      title = textOutput(NS(id, "pec14txt3")),
                      collapsible = T,
                      collapsed = F,
                      width = 12,
                      headerBorder = T,
                      solidHeader = F,
                      withSpinner(
                        echarts4rOutput(NS(id, "pec14graf")),
                        type = 8,
                        color = "#f2a92a",
                        size = 0.5
                      ),
                      footer = list(column(
                        11,
                        tags$h6(tags$b("Fonte:"), "IBGE/PPM"),
                        tags$h6(tags$b("Elaboração:"), "FAPESPA")
                      ), downset_ui(NS(id, "pec14_2")))
                    )
                  )
                )
              )
            )))
}
# Função do modulo servidor
economia_pec_pa_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    #TÍTULOS----
    # 1- Efetivo de Rebanho bovino (Cabeça) ----
    ## Mapa - Efetivo de Rebanho bovino (Cabeça)----
    t11 <- reactive({
      if (input$pec1ri == "Pará") {
        paste0("Efetivo de Rebanho bovino (Cabeça), ",
               input$pec1ri,
               " - ",
               input$pec1ano)
      } else{
        paste0(
          "Efetivo de Rebanho bovino (Cabeça), Região de Integração ",
          input$pec1ri,
          " - ",
          input$pec1ano
        )
      }
    })
    ## Tabela - Efetivo de Rebanho bovino (Cabeça)----
    t12 <- reactive({
      if (input$pec1ri == "Pará") {
        paste0(
          "Efetivo de Rebanho bovino (Cabeça) dos Municípios, ",
          input$pec1ri,
          " - ",
          input$pec1ano
        )
      } else{
        paste0(
          "Efetivo de Rebanho bovino (Cabeça) dos Municípios, Região de Integração ",
          input$pec1ri,
          " - ",
          input$pec1ano
        )
      }
    })
    ## Gráfico - Efetivo de Rebanho bovino (Cabeça)----
    t13 <- reactive({
      paste0("Efetivo de Rebanho bovino (Cabeça), Pará - ",
             min(pec1$ano),
             " a ",
             max(pec1$ano))
    })
    # 2- Efetivo de Rebanho bubalino (Cabeça) ----
    ## Mapa - Efetivo de Rebanho bubalino (Cabeça)----
    t21 <- reactive({
      if (input$pec2ri == "Pará") {
        paste0("Efetivo de Rebanho bubalino (Cabeça), ",
               input$pec2ri,
               " - ",
               input$pec2ano)
      } else{
        paste0(
          "Efetivo de Rebanho bubalino (Cabeça), Região de Integração ",
          input$pec2ri,
          " - ",
          input$pec2ano
        )
      }
    })
    ## Tabela - Efetivo de Rebanho bubalino (Cabeça)----
    t22 <- reactive({
      if (input$pec2ri == "Pará") {
        paste0(
          "Efetivo de Rebanho bubalino (Cabeça) dos Municípios, ",
          input$pec2ri,
          " - ",
          input$pec2ano
        )
      } else{
        paste0(
          "Efetivo de Rebanho bubalino (Cabeça) dos Municípios, Região de Integração ",
          input$pec2ri,
          " - ",
          input$pec2ano
        )
      }
    })
    ## Gráfico - Efetivo de Rebanho bubalino (Cabeça)----
    t23 <- reactive({
      paste0("Efetivo de Rebanho bubalino (Cabeça), Pará - ",
             min(pec2$ano),
             " a ",
             max(pec2$ano))
    })
    # 3- Efetivo de Rebanho Equino ----
    ## Mapa - Efetivo de Rebanho Equino----
    t31 <- reactive({
      if (input$pec3ri == "Pará") {
        paste0("Efetivo de Rebanho Equino, ",
               input$pec3ri,
               " - ",
               input$pec3ano)
      } else{
        paste0(
          "Efetivo de Rebanho Equino, Região de Integração ",
          input$pec3ri,
          " - ",
          input$pec3ano
        )
      }
    })
    ## Tabela - Efetivo de Rebanho Equino----
    t32 <- reactive({
      if (input$pec3ri == "Pará") {
        paste0("Efetivo de Rebanho Equinodos Municípios, ",
               input$pec3ri,
               " - ",
               input$pec3ano)
      } else{
        paste0(
          "Efetivo de Rebanho Equinodos Municípios, Região de Integração ",
          input$pec3ri,
          " - ",
          input$pec3ano
        )
      }
    })
    ## Gráfico - Efetivo de Rebanho Equino----
    t33 <- reactive({
      paste0("Efetivo de Rebanho Equino, Pará - ",
             min(pec3$ano),
             " a ",
             max(pec3$ano))
    })
    # 4- Efetivo de Rebanho suino total (Cabeça) ----
    ## Mapa - Efetivo de Rebanho suino total (Cabeça)----
    t41 <- reactive({
      if (input$pec4ri == "Pará") {
        paste0("Efetivo de Rebanho suino total (Cabeça), ",
               input$pec4ri,
               " - ",
               input$pec4ano)
      } else{
        paste0(
          "Efetivo de Rebanho suino total (Cabeça), Região de Integração ",
          input$pec4ri,
          " - ",
          input$pec4ano
        )
      }
    })
    ## Tabela - Efetivo de Rebanho suino total (Cabeça)----
    t42 <- reactive({
      if (input$pec4ri == "Pará") {
        paste0(
          "Efetivo de Rebanho suino total (Cabeça) dos Municípios, ",
          input$pec4ri,
          " - ",
          input$pec4ano
        )
      } else{
        paste0(
          "Efetivo de Rebanho suino total (Cabeça) dos Municípios, Região de Integração ",
          input$pec4ri,
          " - ",
          input$pec4ano
        )
      }
    })
    ## Gráfico - Efetivo de Rebanho suino total (Cabeça)----
    t43 <- reactive({
      paste0(
        "Efetivo de Rebanho suino total (Cabeça), Pará - ",
        min(pec4$ano),
        " a ",
        max(pec4$ano)
      )
    })
    # 5- Efetivo de Rebanho suino total (Matrizes) ----
    ## Mapa - Efetivo de Rebanho suino total (Matrizes)----
    t51 <- reactive({
      if (input$pec5ri == "Pará") {
        paste0("Efetivo de Rebanho suino total (Matrizes), ",
               input$pec5ri,
               " - ",
               input$pec5ano)
      } else{
        paste0(
          "Efetivo de Rebanho suino total (Matrizes), Região de Integração ",
          input$pec5ri,
          " - ",
          input$pec5ano
        )
      }
    })
    ## Tabela - Efetivo de Rebanho suino total (Matrizes)----
    t52 <- reactive({
      if (input$pec5ri == "Pará") {
        paste0(
          "Efetivo de Rebanho suino total (Matrizes) dos Municípios, ",
          input$pec5ri,
          " - ",
          input$pec5ano
        )
      } else{
        paste0(
          "Efetivo de Rebanho suino total (Matrizes) dos Municípios, Região de Integração ",
          input$pec5ri,
          " - ",
          input$pec5ano
        )
      }
    })
    ## Gráfico - Efetivo de Rebanho suino total (Matrizes)----
    t53 <- reactive({
      paste0(
        "Efetivo de Rebanho suino total (Matrizes), Pará - ",
        min(pec5$ano),
        " a ",
        max(pec5$ano)
      )
    })
    # 6- Efetivo de Rebanho Caprino (cabeça) ----
    ## Mapa - Efetivo de Rebanho Caprino (cabeças)----
    t61 <- reactive({
      if (input$pec6ri == "Pará") {
        paste0("Efetivo de Rebanho Caprino (cabeças), ",
               input$pec6ri,
               " - ",
               input$pec6ano)
      } else{
        paste0(
          "Efetivo de Rebanho Caprino (cabeças), Região de Integração ",
          input$pec6ri,
          " - ",
          input$pec6ano
        )
      }
    })
    ## Tabela - Efetivo de Rebanho Caprino (cabeças)----
    t62 <- reactive({
      if (input$pec6ri == "Pará") {
        paste0(
          "Efetivo de Rebanho Caprino (cabeças) dos Municípios, ",
          input$pec6ri,
          " - ",
          input$pec6ano
        )
      } else{
        paste0(
          "Efetivo de Rebanho Caprino (cabeças)  dos Municípios, Região de Integração ",
          input$pec6ri,
          " - ",
          input$pec6ano
        )
      }
    })
    ## Gráfico - Efetivo de Rebanho Caprino (cabeças)----
    t63 <- reactive({
      paste0("Efetivo de Rebanho Caprino (cabeças), Pará - ",
             min(pec6$ano),
             " a ",
             max(pec6$ano))
    })
    # 7- Efetivo de Rebanho Ovino (cabeças) ----
    ## Mapa - Efetivo de Rebanho Ovino (cabeças)----
    t71 <- reactive({
      if (input$pec7ri == "Pará") {
        paste0("Efetivo de Rebanho Ovino (cabeças), ",
               input$pec7ri,
               " - ",
               input$pec7ano)
      } else{
        paste0(
          "Efetivo de Rebanho Ovino (cabeças), Região de Integração ",
          input$pec7ri,
          " - ",
          input$pec7ano
        )
      }
    })
    ## Tabela - Efetivo de Rebanho Ovino (cabeças)----
    t72 <- reactive({
      paste0("Efetivo de Rebanho Ovino (cabeças) - ",
             input$pec7ri,
             " - ",
             input$pec7ano)
    })
    ## Gráfico - Efetivo de Rebanho Ovino (cabeças)----
    t73 <- reactive({
      paste0("Efetivo de Rebanho Ovino (cabeças), Pará - ",
             min(pec7$ano),
             " a ",
             max(pec7$ano))
    })
    # 8- Efetivo de Rebanho Galináceos (Total) ----
    ## Mapa - Efetivo de Rebanho Galináceos (Total)----
    t81 <- reactive({
      if (input$pec8ri == "Pará") {
        paste0("Efetivo de Rebanho Galináceos (Total), ",
               input$pec8ri,
               " - ",
               input$pec8ano)
      } else{
        paste0(
          "Efetivo de Rebanho Galináceos (Total), Região de Integração ",
          input$pec8ri,
          " - ",
          input$pec8ano
        )
      }
    })
    ## Tabela - Efetivo de Rebanho Galináceos (Total)----
    t82 <- reactive({
      if (input$pec8ri == "Pará") {
        paste0(
          "Efetivo de Rebanho Galináceos (Total) dos Municípios, ",
          input$pec8ri,
          " - ",
          input$pec8ano
        )
      } else{
        paste0(
          "Efetivo de Rebanho Galináceos (Total) dos Municípios, Região de Integração ",
          input$pec8ri,
          " - ",
          input$pec8ano
        )
      }
    })
    ## Gráfico - Efetivo de Rebanho Galináceos (Total)----
    t83 <- reactive({
      paste0(
        "Efetivo de Rebanho Galináceos (Total), Pará - ",
        min(pec8$ano),
        " a ",
        max(pec8$ano)
      )
    })
    # 9- Efetivo de Rebanho Galináceos (Galinhas) ----
    ## Mapa - Efetivo de Rebanho Galináceos (Galinhas)----
    t91 <- reactive({
      if (input$pec9ri == "Pará") {
        paste0("Efetivo de Rebanho Galináceos (Galinhas), ",
               input$pec9ri,
               " - ",
               input$pec9ano)
      } else{
        paste0(
          "Efetivo de Rebanho Galináceos (Galinhas), Região de Integração ",
          input$pec9ri,
          " - ",
          input$pec9ano
        )
      }
    })
    ## Tabela - Efetivo de Rebanho Galináceos (Galinhas)----
    t92 <- reactive({
      if (input$pec9ri == "Pará") {
        paste0(
          "Efetivo de Rebanho Galináceos (Galinhas) dos Municípios, ",
          input$pec9ri,
          " - ",
          input$pec9ano
        )
      } else{
        paste0(
          "Efetivo de Rebanho Galináceos (Galinhas) dos Municípios, Região de Integração ",
          input$pec9ri,
          " - ",
          input$pec9ano
        )
      }
    })
    ## Gráfico - Efetivo de Rebanho Galináceos (Galinhas)----
    t93 <- reactive({
      paste0(
        "Efetivo de Rebanho Galináceos (Galinhas), Pará - ",
        min(pec9$ano),
        " a ",
        max(pec9$ano)
      )
    })
    # 10- Efetivo de Rebanho Codornas (cabeças) ----
    ## Mapa - Efetivo de Rebanho Codornas (cabeças)----
    t101 <- reactive({
      if (input$pec10ri == "Pará") {
        paste0("Efetivo de Rebanho Codornas (cabeças), ",
               input$pec10ri,
               " - ",
               input$pec10ano)
      } else{
        paste0(
          "Efetivo de Rebanho Codornas (cabeças), Região de Integração ",
          input$pec10ri,
          " - ",
          input$pec10ano
        )
      }
    })
    ## Tabela - Efetivo de Rebanho Codornas (cabeças)----
    t102 <- reactive({
      if (input$pec10ri == "Pará") {
        paste0(
          "Efetivo de Rebanho Codornas (cabeças) dos Municípios, ",
          input$pec10ri,
          " - ",
          input$pec10ano
        )
      } else{
        paste0(
          "Efetivo de Rebanho Codornas (cabeças)dos Municípios, Região de Integração ",
          input$pec10ri,
          " - ",
          input$pec10ano
        )
      }
    })
    ## Gráfico - Efetivo de Rebanho Codornas (cabeças)----
    t103 <- reactive({
      paste0(
        "Efetivo de Rebanho Codornas (cabeças), Pará - ",
        min(pec10$ano),
        " a ",
        max(pec10$ano)
      )
    })
    # 11- Produção de Origem Animal Leite (Mil litros) ----
    ## Mapa - Produção de Origem Animal Leite (Mil litros)----
    t111 <- reactive({
      if (input$pec11ri == "Pará") {
        paste0(
          "Produção de Origem Animal Leite (Mil litros), ",
          input$pec11ri,
          " - ",
          input$pec11ano
        )
      } else{
        paste0(
          "Produção de Origem Animal Leite (Mil litros), Região de Integração ",
          input$pec11ri,
          " - ",
          input$pec11ano
        )
      }
    })
    ## Tabela - Produção de Origem Animal Leite (Mil litros)----
    t112 <- reactive({
      if (input$pec11ri == "Pará") {
        paste0(
          "Produção de Origem Animal Leite (Mil litros) dos Municípios, ",
          input$pec11ri,
          " - ",
          input$pec11ano
        )
      } else{
        paste0(
          "Produção de Origem Animal Leite (Mil litros) dos Municípios, Região de Integração ",
          input$pec11ri,
          " - ",
          input$pec11ano
        )
      }
    })
    ## Gráfico - Produção de Origem Animal Leite (Mil litros)----
    t113 <- reactive({
      paste0(
        "Efetivo de Rebanho Codornas (cabeças), Pará - ",
        min(pec11$ano),
        " a ",
        max(pec11$ano)
      )
    })
    # 12- Produção de Origem Animal Ovos de galinha (Mil dúzias) ----
    ## Mapa - Produção de Origem Animal Ovos de galinha (Mil dúzias)----
    t112 <- reactive({
      if (input$pec12ri == "Pará") {
        paste0(
          "Produção de Origem Animal Ovos de galinha (Mil dúzias), ",
          input$pec12ri,
          " - ",
          input$pec12ano
        )
      } else{
        paste0(
          "Produção de Origem Animal Ovos de galinha (Mil dúzias), Região de Integração ",
          input$pec12ri,
          " - ",
          input$pec12ano
        )
      }
    })
    ## Tabela - Produção de Origem Animal Ovos de galinha (Mil dúzias)----
    t122 <- reactive({
      if (input$pec12ri == "Pará") {
        paste0(
          "Produção de Origem Animal Ovos de galinha (Mil dúzias) dos Municípios, ",
          input$pec12ri,
          " - ",
          input$pec12ano
        )
      } else{
        paste0(
          "Produção de Origem Animal Ovos de galinha (Mil dúzias) dos Municípios, Região de Integração ",
          input$pec12ri,
          " - ",
          input$pec12ano
        )
      }
    })
    ## Gráfico - Produção de Origem Animal Ovos de galinha (Mil dúzias)----
    t123 <- reactive({
      paste0(
        "Produção de Origem Animal Ovos de galinha (Mil dúzias), Pará - ",
        min(pec12$ano),
        " a ",
        max(pec12$ano)
      )
    })
    # 13- Produção de Origem Animal Ovos de codorna (Mil dúzias) ----
    ## Mapa - Produção de Origem Animal Ovos de codorna (Mil dúzias)----
    t131 <- reactive({
      if (input$pec13ri == "Pará") {
        paste0(
          "Produção de Origem Animal Ovos de codorna (Mil dúzias), ",
          input$pec13ri,
          " - ",
          input$pec13ano
        )
      } else{
        paste0(
          "Produção de Origem Animal Ovos de codorna (Mil dúzias), Região de Integração ",
          input$pec13ri,
          " - ",
          input$pec13ano
        )
      }
    })
    ## Tabela - Produção de Origem Animal Ovos de codorna (Mil dúzias)----
    t132 <- reactive({
      paste0(
        "Produção de Origem Animal Ovos de codorna (Mil dúzias) - ",
        input$pec13ri,
        " - ",
        input$pec13ano
      )
    })
    ## Gráfico - Produção de Origem Animal Ovos de codorna (Mil dúzias)----
    t133 <- reactive({
      paste0(
        "Produção de Origem Animal Ovos de codorna (Mil dúzias), Pará - ",
        min(pec13$ano),
        " a ",
        max(pec13$ano)
      )
    })
    # 14- Produção de Origem Animal Mel de abelha (Quilogramas) ----
    ## Mapa - Produção de Origem Animal Mel de abelha (Quilogramas)----
    t141 <- reactive({
      if (input$pec14ri == "Pará") {
        paste0(
          "Produção de Origem Animal Mel de abelha (Quilogramas), ",
          input$pec14ri,
          " - ",
          input$pec14ano
        )
      } else{
        paste0(
          "Produção de Origem Animal Mel de abelha (Quilogramas), Região de Integração ",
          input$pec14ri,
          " - ",
          input$pec14ano
        )
      }
    })
    ## Tabela - Produção de Origem Animal Mel de abelha (Quilogramas)----
    t142 <- reactive({
      if (input$pec14ri == "Pará") {
        paste0(
          "Produção de Origem Animal Mel de abelha (Quilogramas) dos Municípios, ",
          input$pec14ri,
          " - ",
          input$pec14ano
        )
      } else{
        paste0(
          "Produção de Origem Animal Mel de abelha (Quilogramas) dos Municípios, Região de Integração ",
          input$pec14ri,
          " - ",
          input$pec14ano
        )
      }
    })
    ## Gráfico - Produção de Origem Animal Mel de abelha (Quilogramas)----
    t143 <- reactive({
      paste0(
        "Produção de Origem Animal Mel de abelha (Quilogramas), Pará - ",
        min(pec14$ano),
        " a ",
        max(pec14$ano)
      )
    })
    #VISUALIZAÇÃO----
    # 1- Efetivo de Rebanho bovino (Cabeça) ----
    ## Mapa - Efetivo de Rebanho bovino (Cabeça)----
    output$pec1txt1 <- renderText({
      t11()
    })
    output$pec1map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$pec1ri == "Pará") {
        df <- pec1 %>%
          filter(localidade != "Pará", ano == input$pec1ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <-
          pec1 %>% filter(localidade != "Pará", ano == input$pec1ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$pec1ri)
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
          "<strong>%s</strong><br/> <b>Quantidade:</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Sem Efetivo",
            format(x$valor, big.mark = ".", decimal.mark = ",")
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
          values = ~ valor,
          opacity = 0.7,
          title = "Quantidade",
          position = "bottomright",
          na.label = "Sem Efetivo",
          labFormat = labelFormat_decimal(
            big.mark = ".",
            decimal.mark = ",",
            digits = 2
          )
        )
    })
    ## Tabela - Efetivo de Rebanho bovino (Cabeça)----
    output$pec1txt2 <- renderText({
      t12()
    })
    # Renderização da base pec1
    output$pec1tab <- renderReactable({
      if (input$pec1ri == "Pará") {
        x <- pec1 %>%
          filter(localidade != "Pará", ano == input$pec1ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- pec1 %>%
          filter(localidade != "Pará", ano == input$pec1ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$pec1ri)
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
            name = "Quantidade",
            na = "-",
            format = colFormat(separators = T, locales = "pt-BR")
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
    ## Gráfico - Efetivo de Rebanho bovino (Cabeça)----
    output$pec1txt3 <- renderText({
      t13()
    })
    output$pec1graf <- renderEcharts4r({
      pec1 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
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
    # 2- Efetivo de Rebanho bubalino (Cabeça) ----
    ## Mapa - Efetivo de Rebanho bubalino (Cabeça)----
    output$pec2txt1 <- renderText({
      t21()
    })
    output$pec2map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$pec2ri == "Pará") {
        df <- pec2 %>%
          filter(localidade != "Pará", ano == input$pec2ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- pec2 %>%
          filter(localidade != "Pará", ano == input$pec2ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$pec2ri)
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
          "<strong>%s</strong><br/> <b>Quantidade:</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Sem Efetivo",
            format(x$valor, big.mark = ".", decimal.mark = ",")
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
          values = ~ valor,
          opacity = 0.7,
          title = "Quantidade",
          position = "bottomright",
          na.label = "Sem Efetivo",
          labFormat = labelFormat_decimal(
            big.mark = ".",
            decimal.mark = ",",
            digits = 2
          )
        )
    })
    ## Tabela - Efetivo de Rebanho bubalino (Cabeça)----
    output$pec2txt2 <- renderText({
      t22()
    })
    # Renderização da base pec2
    output$pec2tab <- renderReactable({
      if (input$pec2ri == "Pará") {
        x <- pec2 %>%
          filter(localidade != "Pará", ano == input$pec2ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- pec2 %>%
          filter(localidade != "Pará", ano == input$pec2ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$pec2ri)
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
            name = "Quantidade",
            na = "-",
            format = colFormat(separators = T, locales = "pt-BR")
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
    ## Gráfico - Efetivo de Rebanho bubalino (Cabeça)----
    output$pec2txt3 <- renderText({
      t23()
    })
    output$pec2graf <- renderEcharts4r({
      pec2 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
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
    # 3- Efetivo de Rebanho Equino ----
    ## Mapa - Efetivo de Rebanho Equino----
    output$pec3txt1 <- renderText({
      t31()
    })
    output$pec3map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$pec3ri == "Pará") {
        df <- pec3 %>%
          filter(localidade != "Pará", ano == input$pec3ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- pec3 %>%
          filter(localidade != "Pará", ano == input$pec3ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$pec3ri)
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
          "<strong>%s</strong><br/> <b>Quantidade:</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Sem Efetivo",
            format(x$valor, big.mark = ".", decimal.mark = ",")
          )
        ) %>% lapply(htmltools::HTML)
      
      # Mapas com leafleft
      leaflet(x, options =
                leafletOptions(minZoom = 0, maxZoom = 15)) %>%
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
          na.label = "Sem Efetivo",
          labFormat = labelFormat_decimal(
            big.mark = ".",
            decimal.mark = ",",
            digits = 2
          )
        )
    })
    ## Tabela - Efetivo de Rebanho Equino----
    output$pec3txt2 <- renderText({
      t32()
    })
    # Renderização da base pec3
    output$pec3tab <- renderReactable({
      if (input$pec3ri == "Pará") {
        x <- pec3 %>%
          filter(localidade != "Pará", ano == input$pec3ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- pec3 %>%
          filter(localidade != "Pará", ano == input$pec3ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$pec3ri)
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
            name = "Quantidade",
            na = "-",
            format = colFormat(separators = T, locales = "pt-BR")
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
    ## Gráfico - Efetivo de Rebanho Equino----
    output$pec3txt3 <- renderText({
      t33()
    })
    
    output$pec3graf <- renderEcharts4r({
      pec3 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
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
    # 4- Efetivo de Rebanho suino total (Cabeça) ----
    ## Mapa - Efetivo de Rebanho suino total (Cabeça)----
    output$pec4txt1 <- renderText({
      t41()
    })
    output$pec4map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$pec4ri == "Pará") {
        df <- pec4 %>%
          filter(localidade != "Pará", ano == input$pec4ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <-
          pec4 %>% filter(localidade != "Pará", ano == input$pec4ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$pec4ri)
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
          "<strong>%s</strong><br/> <b>Quantidade:</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Sem Efetivo",
            format(x$valor, big.mark = ".", decimal.mark = ",")
          )
        ) %>% lapply(htmltools::HTML)
      # Mapas com leafleft
      leaflet(x, options =
                leafletOptions(minZoom = 0, maxZoom = 15)) %>%
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
          na.label = "Sem Efetivo",
          labFormat = labelFormat_decimal(
            big.mark = ".",
            decimal.mark = ",",
            digits = 2
          )
        )
    })
    ## Tabela - Efetivo de Rebanho suino total (Cabeça)----
    output$pec4txt2 <- renderText({
      t42()
    })
    # Renderização da base pec4
    output$pec4tab <- renderReactable({
      if (input$pec4ri == "Pará") {
        x <- pec4 %>%
          filter(localidade != "Pará", ano == input$pec4ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- pec4 %>%
          filter(localidade != "Pará", ano == input$pec4ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$pec4ri)
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
            name = "Quantidade",
            na = "-",
            format = colFormat(separators = T, locales = "pt-BR")
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
    ## Gráfico - Efetivo de Rebanho suino total (Cabeça)----
    output$pec4txt3 <- renderText({
      t43()
    })
    output$pec4graf <- renderEcharts4r({
      pec4 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
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
    # 5- Efetivo de Rebanho suino total (Matrizes) ----
    ## Mapa - Efetivo de Rebanho suino total (Matrizes)----
    output$pec5txt1 <- renderText({
      t51()
    })
    output$pec5map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$pec5ri == "Pará") {
        df <- pec5 %>%
          filter(localidade != "Pará", ano == input$pec5ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- pec5 %>%
          filter(localidade != "Pará", ano == input$pec5ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$pec5ri)
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
          "<strong>%s</strong><br/> <b>Quantidade:</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Sem Efetivo",
            format(x$valor, big.mark = ".", decimal.mark = ",")
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
          values = ~ valor,
          opacity = 0.7,
          title = "Quantidade",
          position = "bottomright",
          na.label = "Sem Efetivo",
          labFormat = labelFormat_decimal(
            big.mark = ".",
            decimal.mark = ",",
            digits = 2
          )
        )
    })
    ## Tabela - Efetivo de Rebanho suino total (Matrizes)----
    output$pec5txt2 <- renderText({
      t52()
    })
    # Renderização da base pec5
    output$pec5tab <- renderReactable({
      if (input$pec5ri == "Pará") {
        x <- pec5 %>%
          filter(localidade != "Pará", ano == input$pec5ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- pec5 %>%
          filter(localidade != "Pará", ano == input$pec5ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$pec5ri)
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
            name = "Quantidade",
            na = "-",
            format = colFormat(separators = T, locales = "pt-BR")
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
    ## Gráfico - Efetivo de Rebanho suino total (Matrizes)----
    output$pec5txt3 <- renderText({
      t53()
    })
    
    output$pec5graf <- renderEcharts4r({
      pec5 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
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
    # 6- Efetivo de Rebanho Caprino (cabeça) ----
    ## Mapa - Efetivo de Rebanho Caprino (cabeças)----
    output$pec6txt1 <- renderText({
      t61()
    })
    output$pec6map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$pec6ri == "Pará") {
        df <- pec6 %>%
          filter(localidade != "Pará", ano == input$pec6ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- pec6 %>%
          filter(localidade != "Pará", ano == input$pec6ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$pec6ri)
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
          "<strong>%s</strong><br/> <b>Quantidade:</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Sem Efetivo",
            format(x$valor, big.mark = ".", decimal.mark = ",")
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
          values = ~ valor,
          opacity = 0.7,
          title = "Quantidade",
          position = "bottomright",
          na.label = "Sem Efetivo",
          labFormat = labelFormat_decimal(
            big.mark = ".",
            decimal.mark = ",",
            digits = 2
          )
        )
    })
    ## Tabela - Efetivo de Rebanho Caprino (cabeças)----
    output$pec6txt2 <- renderText({
      t62()
    })
    # Renderização da base pec6
    output$pec6tab <- renderReactable({
      if (input$pec6ri == "Pará") {
        x <- pec6 %>%
          filter(localidade != "Pará", ano == input$pec6ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- pec6 %>%
          filter(localidade != "Pará", ano == input$pec6ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$pec6ri)
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
            name = "Quantidade",
            na = "-",
            format = colFormat(separators = T, locales = "pt-BR")
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
    ## Gráfico - Efetivo de Rebanho Caprino (cabeças)----
    output$pec6txt3 <- renderText({
      t63()
    })
    
    output$pec6graf <- renderEcharts4r({
      pec6 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
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
    # 7- Efetivo de Rebanho Ovino (cabeças) ----
    ## Mapa - Efetivo de Rebanho Ovino (cabeças)----
    output$pec7txt1 <- renderText({
      t71()
    })
    output$pec7map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$pec7ri == "Pará") {
        df <- pec7 %>%
          filter(localidade != "Pará", ano == input$pec7ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- pec7 %>%
          filter(localidade != "Pará", ano == input$pec7ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$pec7ri)
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
          "<strong>%s</strong><br/> <b>Quantidade</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Sem Efetivo",
            format(x$valor, big.mark = ".", decimal.mark = ",")
          )
        ) %>% lapply(htmltools::HTML)
      # Mapas com leafleft
      leaflet(x, options =
                leafletOptions(minZoom = 0, maxZoom = 15)) %>%
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
          na.label = "Sem Efetivo",
          labFormat = labelFormat_decimal(
            big.mark = ".",
            decimal.mark = ",",
            digits = 2
          )
        )
    })
    ## Tabela - Efetivo de Rebanho Ovino (cabeças)----
    output$pec7txt2 <- renderText({
      t72()
    })
    # Renderização da base pec7
    output$pec7tab <- renderReactable({
      if (input$pec7ri == "Pará") {
        x <- pec7 %>%
          filter(localidade != "Pará", ano == input$pec7ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- pec7 %>%
          filter(localidade != "Pará", ano == input$pec7ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$pec7ri)
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
            name = "Quantidade",
            na = "-",
            format = colFormat(separators = T, locales = "pt-BR")
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
    ## Gráfico - Efetivo de Rebanho Ovino (cabeças)----
    output$pec7txt3 <- renderText({
      t73()
    })
    
    output$pec7graf <- renderEcharts4r({
      pec7 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
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
    # 8- Efetivo de Rebanho Galináceos (Total) ----
    ## Mapa - Efetivo de Rebanho Galináceos (Total)----
    output$pec8txt1 <- renderText({
      t81()
    })
    output$pec8map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$pec8ri == "Pará") {
        df <- pec8 %>%
          filter(localidade != "Pará", ano == input$pec8ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- pec8 %>%
          filter(localidade != "Pará", ano == input$pec8ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$pec8ri)
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
          "<strong>%s</strong><br/> <b>Quantidade:</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Sem Efetivo",
            format(x$valor, big.mark = ".", decimal.mark = ",")
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
          values = ~ valor,
          opacity = 0.7,
          title = "Quantidade",
          position = "bottomright",
          na.label = "Sem Efetivo",
          labFormat = labelFormat_decimal(
            big.mark = ".",
            decimal.mark = ",",
            digits = 2
          )
        )
    })
    ## Tabela - Efetivo de Rebanho Galináceos (Total)----
    output$pec8txt2 <- renderText({
      t82()
    })
    # Renderização da base pec8
    output$pec8tab <- renderReactable({
      if (input$pec8ri == "Pará") {
        x <- pec8 %>%
          filter(localidade != "Pará", ano == input$pec8ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- pec8 %>%
          filter(localidade != "Pará", ano == input$pec8ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$pec8ri)
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
            name = "Quantidade",
            na = "-",
            format = colFormat(separators = T, locales = "pt-BR")
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
    ## Gráfico - Efetivo de Rebanho Galináceos (Total)----
    output$pec8txt3 <- renderText({
      t83()
    })
    
    output$pec8graf <- renderEcharts4r({
      pec8 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
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
    # 9- Efetivo de Rebanho Galináceos (Galinhas) ----
    ## Mapa - Efetivo de Rebanho Galináceos (Galinhas)----
    output$pec9txt1 <- renderText({
      t91()
    })
    
    output$pec9map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$pec9ri == "Pará") {
        df <- pec9 %>%
          filter(localidade != "Pará", ano == input$pec9ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- pec9 %>%
          filter(localidade != "Pará", ano == input$pec9ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$pec9ri)
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
          "<strong>%s</strong><br/> <b>Quantidade:</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Sem Efetivo",
            format(x$valor, big.mark = ".", decimal.mark = ",")
          )
        ) %>% lapply(htmltools::HTML)
      
      # Mapas com leafleft
      leaflet(x, options =
                leafletOptions(minZoom = 0, maxZoom = 15)) %>%
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
          na.label = "Sem Efetivo",
          labFormat = labelFormat_decimal(
            big.mark = ".",
            decimal.mark = ",",
            digits = 2
          )
        )
    })
    ## Tabela - Efetivo de Rebanho Galináceos (Galinhas)----
    output$pec9txt2 <- renderText({
      t92()
    })
    # Renderização da base pec9
    output$pec9tab <- renderReactable({
      if (input$pec9ri == "Pará") {
        x <- pec9 %>%
          filter(localidade != "Pará", ano == input$pec9ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- pec9 %>%
          filter(localidade != "Pará", ano == input$pec9ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$pec9ri)
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
            name = "Quantidade",
            na = "-",
            format = colFormat(separators = T, locales = "pt-BR")
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
    ## Gráfico - Efetivo de Rebanho Galináceos (Galinhas)----
    output$pec9txt3 <- renderText({
      t93()
    })
    
    output$pec9graf <- renderEcharts4r({
      pec9 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
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
    # 10- Efetivo de Rebanho Codornas (cabeças) ----
    ## Mapa - Efetivo de Rebanho Codornas (cabeças)----
    output$pec10txt1 <- renderText({
      t101()
    })
    
    output$pec10map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$pec10ri == "Pará") {
        df <- pec10 %>%
          filter(localidade != "Pará", ano == input$pec10ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- pec10 %>%
          filter(localidade != "Pará", ano == input$pec10ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$pec10ri)
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
          "<strong>%s</strong><br/> <b>Quantidade:</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Sem Efetivo",
            format(x$valor, big.mark = ".", decimal.mark = ",")
          )
        ) %>% lapply(htmltools::HTML)
      
      # Mapas com leafleft
      leaflet(x, options =
                leafletOptions(minZoom = 0, maxZoom = 15)) %>%
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
          na.label = "Sem Efetivo",
          labFormat = labelFormat_decimal(
            big.mark = ".",
            decimal.mark = ",",
            digits = 2
          )
        )
    })
    ## Tabela - Efetivo de Rebanho Codornas (cabeças)----
    output$pec10txt2 <- renderText({
      t102()
    })
    # Renderização da base pec10
    output$pec10tab <- renderReactable({
      if (input$pec10ri == "Pará") {
        x <- pec10 %>%
          filter(localidade != "Pará", ano == input$pec10ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- pec10 %>%
          filter(localidade != "Pará", ano == input$pec10ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$pec10ri)
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
            name = "Quantidade",
            na = "-",
            format = colFormat(separators = T, locales = "pt-BR")
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
    ## Gráfico - Efetivo de Rebanho Codornas (cabeças)----
    output$pec10txt3 <- renderText({
      t103()
    })
    
    output$pec10graf <- renderEcharts4r({
      pec10 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
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
    # 11- Produção de Origem Animal Leite (Mil litros) ----
    ## Mapa - Produção de Origem Animal Leite (Mil litros)----
    output$pec11txt1 <- renderText({
      t111()
    })
    
    output$pec11map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$pec11ri == "Pará") {
        df <- pec11 %>%
          filter(localidade != "Pará", ano == input$pec11ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- pec11 %>%
          filter(localidade != "Pará", ano == input$pec11ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$pec11ri)
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
          "<strong>%s</strong><br/> <b>Produção(Mil litros):</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Sem Produção",
            format(x$valor, big.mark = ".", decimal.mark = ",")
          )
        ) %>% lapply(htmltools::HTML)
      
      # Mapas com leafleft
      leaflet(x, options =
                leafletOptions(minZoom = 0, maxZoom = 15)) %>%
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
          title = "Produção(Mil litros)",
          position = "bottomright",
          na.label = "Sem Produção",
          labFormat = labelFormat_decimal(
            big.mark = ".",
            decimal.mark = ",",
            digits = 2
          )
        )
    })
    ## Tabela - Produção de Origem Animal Leite (Mil litros)----
    output$pec11txt2 <- renderText({
      t112()
    })
    # Renderização da base pec11
    output$pec11tab <- renderReactable({
      if (input$pec11ri == "Pará") {
        x <- pec11 %>%
          filter(localidade != "Pará", ano == input$pec11ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- pec11 %>%
          filter(localidade != "Pará", ano == input$pec11ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$pec11ri)
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
            name = "Produção(Mil litros)",
            na = "-",
            format = colFormat(separators = T, locales = "pt-BR")
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
    ## Gráfico - Produção de Origem Animal Leite (Mil litros)----
    output$pec11txt3 <- renderText({
      t113()
    })
    
    output$pec11graf <- renderEcharts4r({
      pec11 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
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
          name = "Produção(Mil litros)",
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
    # 12- Produção de Origem Animal Ovos de galinha (Mil dúzias) ----
    ## Mapa - Produção de Origem Animal Ovos de galinha (Mil dúzias)----
    output$pec12txt1 <- renderText({
      t112()
    })
    
    output$pec12map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$pec12ri == "Pará") {
        df <- pec12 %>%
          filter(localidade != "Pará", ano == input$pec12ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- pec12 %>%
          filter(localidade != "Pará", ano == input$pec12ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$pec12ri)
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
          "<strong>%s</strong><br/> <b>Ovos(Mil dúzias):</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Sem Produção",
            format(x$valor, big.mark = ".", decimal.mark = ",")
          )
        ) %>% lapply(htmltools::HTML)
      
      # Mapas com leafleft
      leaflet(x, options =
                leafletOptions(minZoom = 0, maxZoom = 15)) %>%
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
          title = "Ovos(Mil dúzias)",
          position = "bottomright",
          na.label = "Sem Produção",
          labFormat = labelFormat_decimal(
            big.mark = ".",
            decimal.mark = ",",
            digits = 2
          )
        )
    })
    ## Tabela - Produção de Origem Animal Ovos de galinha (Mil dúzias)----
    output$pec12txt2 <- renderText({
      t122()
    })
    
    # Renderização da base pec12
    output$pec12tab <- renderReactable({
      if (input$pec12ri == "Pará") {
        x <- pec12 %>%
          filter(localidade != "Pará", ano == input$pec12ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- pec12 %>%
          filter(localidade != "Pará", ano == input$pec12ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$pec12ri)
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
            name = "Ovos(Mil dúzias)",
            na = "-",
            format = colFormat(separators = T, locales = "pt-BR")
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
    ## Gráfico - Produção de Origem Animal Ovos de galinha (Mil dúzias)----
    output$pec12txt3 <- renderText({
      t123()
    })
    
    output$pec12graf <- renderEcharts4r({
      pec12 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
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
          name = "Ovos(Mil dúzias)",
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
    # 13- Produção de Origem Animal Ovos de codorna (Mil dúzias) ----
    ## Mapa - Produção de Origem Animal Ovos de codorna (Mil dúzias)----
    output$pec13txt1 <- renderText({
      t131()
    })
    output$pec13map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$pec13ri == "Pará") {
        df <- pec13 %>%
          filter(localidade != "Pará", ano == input$pec13ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- pec13 %>%
          filter(localidade != "Pará", ano == input$pec13ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$pec13ri)
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
          "<strong>%s</strong><br/> <b>Ovos(Mil dúzias):</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Sem Produção",
            format(x$valor, big.mark = ".", decimal.mark = ",")
          )
        ) %>% lapply(htmltools::HTML)
      
      # Mapas com leafleft
      leaflet(x, options =
                leafletOptions(minZoom = 0, maxZoom = 15)) %>%
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
          title = "Ovos(Mil dúzias)",
          position = "bottomright",
          na.label = "Sem Produção",
          labFormat = labelFormat_decimal(
            big.mark = ".",
            decimal.mark = ",",
            digits = 2
          )
        )
    })
    ## Tabela - Produção de Origem Animal Ovos de codorna (Mil dúzias)----
    output$pec13txt2 <- renderText({
      t132()
    })
    # Renderização da base pec13
    output$pec13tab <- renderReactable({
      if (input$pec13ri == "Pará") {
        x <- pec13 %>%
          filter(localidade != "Pará", ano == input$pec13ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- pec13 %>%
          filter(localidade != "Pará", ano == input$pec13ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$pec13ri)
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
            name = "Ovos(Mil dúzias)",
            na = "-",
            format = colFormat(separators = T, locales = "pt-BR")
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
    ## Gráfico - Produção de Origem Animal Ovos de codorna (Mil dúzias)----
    output$pec13txt3 <- renderText({
      t133()
    })
    
    output$pec13graf <- renderEcharts4r({
      pec13 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
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
          name = "Ovos(Mil dúzias)",
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
    # 14- Produção de Origem Animal Mel de abelha (Quilogramas) ----
    ## Mapa - Produção de Origem Animal Mel de abelha (Quilogramas)----
    output$pec14txt1 <- renderText({
      t141()
    })
    
    output$pec14map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$pec14ri == "Pará") {
        df <- pec14 %>%
          filter(localidade != "Pará", ano == input$pec14ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      } else {
        df <- pec14 %>%
          filter(localidade != "Pará", ano == input$pec14ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$pec14ri)
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
          "<strong>%s</strong><br/> <b>Quilogramas:</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Sem Produção",
            format(x$valor, big.mark = ".", decimal.mark = ",")
          )
        ) %>% lapply(htmltools::HTML)
      
      # Mapas com leafleft
      leaflet(x, options =
                leafletOptions(minZoom = 0, maxZoom = 15)) %>%
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
          title = "Quilogramas",
          position = "bottomright",
          na.label = "Sem Produção",
          labFormat = labelFormat_decimal(
            big.mark = ".",
            decimal.mark = ",",
            digits = 2
          )
        )
    })
    ## Tabela - Produção de Origem Animal Mel de abelha (Quilogramas)----
    output$pec14txt2 <- renderText({
      t142()
    })
    
    # Renderização da base pec14
    output$pec14tab <- renderReactable({
      if (input$pec14ri == "Pará") {
        x <- pec14 %>%
          filter(localidade != "Pará", ano == input$pec14ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- pec14 %>%
          filter(localidade != "Pará", ano == input$pec14ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$pec14ri)
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
            name = "Quilogramas",
            na = "-",
            format = colFormat(separators = T, locales = "pt-BR")
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
    ## Gráfico - Produção de Origem Animal Mel de abelha (Quilogramas)----
    output$pec14txt3 <- renderText({
      t143()
    })
    
    output$pec14graf <- renderEcharts4r({
      pec14 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
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
          name = "Quilogramas",
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
    #DOWNLOADS----
    # 1 - Efetivo de Rebanho bovino (Cabeça)----
    ## - Tabela - Efetivo de Rebanho bovino----
    # Filtra os dados
    pec1_1 <- reactive({
      if (input$pec1ri == "Pará") {
        x <- pec1 %>%
          filter(localidade != "Pará", ano == input$pec1ano)
      } else {
        x <- pec1 %>%
          filter(localidade != "Pará",
                 ano == input$pec1ano,
                 ri == input$pec1ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec1_1(), {
      t12()
      downset_Server("pec1_1", pec1_1(), t12())
    })
    ## - Gráfico - Efetivo de Rebanho bovino----
    # Filtra os dados
    pec1_2 <- reactive({
      pec1 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec1_2(), {
      t13()
      downset_Server("pec1_2", pec1_2(), t13())
    })
    
    # 2 - Efetivo de Rebanho bubalino (Cabeça)----
    ## - Tabela - Efetivo de Rebanho bubalino----
    # Filtra os dados
    pec2_1 <- reactive({
      if (input$pec2ri == "Pará") {
        x <- pec2 %>%
          filter(localidade != "Pará", ano == input$pec2ano)
      } else {
        x <- pec2 %>%
          filter(localidade != "Pará",
                 ano == input$pec2ano,
                 ri == input$pec2ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec2_1(), {
      t22()
      downset_Server("pec2_1", pec2_1(), t22())
    })
    ## - Gráfico - Efetivo de Rebanho bubalino----
    # Filtra os dados
    pec2_2 <- reactive({
      pec2 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec2_2(), {
      t23()
      downset_Server("pec2_2", pec2_2(), t23())
    })
    
    # 3 - Efetivo de Rebanho Equino----
    ## - Tabela - Efetivo de Rebanho Equino----
    # Filtra os dados
    pec3_1 <- reactive({
      if (input$pec3ri == "Pará") {
        x <- pec3 %>%
          filter(localidade != "Pará", ano == input$pec3ano)
      } else {
        x <- pec3 %>%
          filter(localidade != "Pará",
                 ano == input$pec3ano,
                 ri == input$pec3ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec3_1(), {
      t32()
      downset_Server("pec3_1", pec3_1(), t32())
    })
    ## - Gráfico - Efetivo de Rebanho Equino----
    # Filtra os dados
    pec3_2 <- reactive({
      pec3 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec3_2(), {
      t33()
      downset_Server("pec3_2", pec3_2(), t33())
    })
    
    # 4 - Efetivo de Rebanho suino total (Cabeça)----
    ## - Tabela - Efetivo de Rebanho suino total----
    # Filtra os dados
    pec4_1 <- reactive({
      if (input$pec4ri == "Pará") {
        x <- pec4 %>%
          filter(localidade != "Pará", ano == input$pec4ano)
      } else {
        x <- pec4 %>%
          filter(localidade != "Pará",
                 ano == input$pec4ano,
                 ri == input$pec4ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec4_1(), {
      t42()
      downset_Server("pec4_1", pec4_1(), t42())
    })
    ## - Gráfico - Efetivo de Rebanho suino total----
    # Filtra os dados
    pec4_2 <- reactive({
      pec4 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec4_2(), {
      t43()
      downset_Server("pec4_2", pec4_2(), t43())
    })
    
    # 5 - Efetivo de Rebanho suino total (Matrizes)----
    ## - Tabela - Efetivo de Rebanho suino total (Matrizes)----
    # Filtra os dados
    pec5_1 <- reactive({
      if (input$pec5ri == "Pará") {
        x <- pec5 %>%
          filter(localidade != "Pará", ano == input$pec5ano)
      } else {
        x <- pec5 %>%
          filter(localidade != "Pará",
                 ano == input$pec5ano,
                 ri == input$pec5ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec5_1(), {
      t52()
      downset_Server("pec5_1", pec5_1(), t52())
    })
    ## - Gráfico - Efetivo de Rebanho suino total (Matrizes)----
    # Filtra os dados
    pec5_2 <- reactive({
      pec5 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec5_2(), {
      t53()
      downset_Server("pec5_2", pec5_2(), t53())
    })
    
    # 6 - Efetivo de Rebanho Caprino (cabeças)----
    ## - Tabela - Efetivo de Rebanho Caprino (cabeças)----
    # Filtra os dados
    pec6_1 <- reactive({
      if (input$pec6ri == "Pará") {
        x <- pec6 %>%
          filter(localidade != "Pará", ano == input$pec6ano)
      } else {
        x <- pec6 %>%
          filter(localidade != "Pará",
                 ano == input$pec6ano,
                 ri == input$pec6ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec6_1(), {
      t62()
      downset_Server("pec6_1", pec6_1(), t62())
    })
    ## - Gráfico - Efetivo de Rebanho Caprino (cabeças)----
    # Filtra os dados
    pec6_2 <- reactive({
      pec6 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec6_2(), {
      t63()
      downset_Server("pec6_2", pec6_2(), t63())
    })
    
    # 7 - Efetivo de Rebanho Ovino (cabeças)----
    ## - Tabela - Efetivo de Rebanho Ovino (cabeças)----
    # Filtra os dados
    pec7_1 <- reactive({
      if (input$pec7ri == "Pará") {
        x <- pec7 %>%
          filter(localidade != "Pará", ano == input$pec7ano)
      } else {
        x <- pec7 %>%
          filter(localidade != "Pará",
                 ano == input$pec7ano,
                 ri == input$pec7ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec7_1(), {
      t72()
      downset_Server("pec7_1", pec7_1(), t72())
    })
    ## - Gráfico - Efetivo de Rebanho Ovino (cabeças)----
    # Filtra os dados
    pec7_2 <- reactive({
      pec7 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec7_2(), {
      t73()
      downset_Server("pec7_2", pec7_2(), t73())
    })
    
    # 8 - Efetivo de Rebanho Galináceos (Total)----
    ## - Tabela - Efetivo de Rebanho Galináceos (Total)----
    # Filtra os dados
    pec8_1 <- reactive({
      if (input$pec8ri == "Pará") {
        x <- pec8 %>%
          filter(localidade != "Pará", ano == input$pec8ano)
      } else {
        x <- pec8 %>%
          filter(localidade != "Pará",
                 ano == input$pec8ano,
                 ri == input$pec8ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec8_1(), {
      t82()
      downset_Server("pec8_1", pec8_1(), t82())
    })
    ## - Gráfico - Efetivo de Rebanho Galináceos (Total)----
    # Filtra os dados
    pec8_2 <- reactive({
      pec8 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec8_2(), {
      t83()
      downset_Server("pec8_2", pec8_2(), t83())
    })
    
    # 9 - Efetivo de Rebanho Galináceos (Galinhas)----
    ## - Tabela - Efetivo de Rebanho Galináceos (Galinhas)----
    # Filtra os dados
    pec9_1 <- reactive({
      if (input$pec9ri == "Pará") {
        x <- pec9 %>%
          filter(localidade != "Pará", ano == input$pec9ano)
      } else {
        x <- pec9 %>%
          filter(localidade != "Pará",
                 ano == input$pec9ano,
                 ri == input$pec9ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec9_1(), {
      t92()
      downset_Server("pec9_1", pec9_1(), t92())
    })
    ## - Gráfico - Efetivo de Rebanho Galináceos (Galinhas)----
    # Filtra os dados
    pec9_2 <- reactive({
      pec9 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec9_2(), {
      t93()
      downset_Server("pec9_2", pec9_2(), t93())
    })
    
    # 10 - Efetivo de Rebanho Codornas (cabeças)----
    ## - Tabela - Efetivo de Rebanho Codornas (cabeças)----
    # Filtra os dados
    pec10_1 <- reactive({
      if (input$pec10ri == "Pará") {
        x <- pec10 %>%
          filter(localidade != "Pará", ano == input$pec10ano)
      } else {
        x <- pec10 %>%
          filter(localidade != "Pará",
                 ano == input$pec10ano,
                 ri == input$pec10ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec10_1(), {
      t102()
      downset_Server("pec10_1", pec10_1(), t102())
    })
    ## - Gráfico - Efetivo de Rebanho Codornas (cabeças)----
    # Filtra os dados
    pec10_2 <- reactive({
      pec10 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec10_2(), {
      t103()
      downset_Server("pec10_2", pec10_2(), t103())
    })
    
    # 11 - Produção de Origem Animal Leite (Mil litros)----
    ## - Tabela - Produção de Origem Animal Leite (Mil litros)----
    # Filtra os dados
    pec11_1 <- reactive({
      if (input$pec11ri == "Pará") {
        x <- pec11 %>%
          filter(localidade != "Pará", ano == input$pec11ano)
      } else {
        x <- pec11 %>%
          filter(localidade != "Pará",
                 ano == input$pec11ano,
                 ri == input$pec11ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec11_1(), {
      t112()
      downset_Server("pec11_1", pec11_1(), t112())
    })
    ## - Gráfico - Produção de Origem Animal Leite (Mil litros)----
    # Filtra os dados
    pec11_2 <- reactive({
      pec11 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec11_2(), {
      t113()
      downset_Server("pec11_2", pec11_2(), t113())
    })
    
    # 12 - Produção de Origem Animal Ovos de galinha (Mil dúzias)----
    ## - Tabela - Produção de Origem Animal Ovos de galinha (Mil dúzias)----
    # Filtra os dados
    pec12_1 <- reactive({
      if (input$pec12ri == "Pará") {
        x <- pec12 %>%
          filter(localidade != "Pará", ano == input$pec12ano)
      } else {
        x <- pec12 %>%
          filter(localidade != "Pará",
                 ano == input$pec12ano,
                 ri == input$pec12ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec12_1(), {
      t122()
      downset_Server("pec12_1", pec12_1(), t122())
    })
    ## - Gráfico - Produção de Origem Animal Ovos de galinha (Mil dúzias)----
    # Filtra os dados
    pec12_2 <- reactive({
      pec12 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec12_2(), {
      t123()
      downset_Server("pec12_2", pec12_2(), t123())
    })
    
    # 13 - Produção de Origem Animal Ovos de codorna (Mil dúzias)----
    ## - Tabela - Produção de Origem Animal Ovos de codorna (Mil dúzias)----
    # Filtra os dados
    pec13_1 <- reactive({
      if (input$pec13ri == "Pará") {
        x <- pec13 %>%
          filter(localidade != "Pará", ano == input$pec13ano)
      } else {
        x <- pec13 %>%
          filter(localidade != "Pará",
                 ano == input$pec13ano,
                 ri == input$pec13ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec13_1(), {
      t132()
      downset_Server("pec13_1", pec13_1(), t132())
    })
    ## - Gráfico - Produção de Origem Animal Ovos de codorna (Mil dúzias)----
    # Filtra os dados
    pec13_2 <- reactive({
      pec13 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec13_2(), {
      t133()
      downset_Server("pec13_2", pec13_2(), t133())
    })
    
    # 14 - Produção de Origem Animal Mel de abelha (Quilogramas)----
    ## - Tabela - Produção de Origem Animal Mel de abelha (Quilogramas)----
    # Filtra os dados
    pec14_1 <- reactive({
      if (input$pec14ri == "Pará") {
        x <- pec14 %>%
          filter(localidade != "Pará", ano == input$pec14ano)
      } else {
        x <- pec14 %>%
          filter(localidade != "Pará",
                 ano == input$pec14ano,
                 ri == input$pec14ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec14_1(), {
      t142()
      downset_Server("pec14_1", pec14_1(), t())
    })
    ## - Gráfico - Produção de Origem Animal Mel de abelha (Quilogramas)----
    # Filtra os dados
    pec14_2 <- reactive({
      pec14 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pec14_2(), {
      t143()
      downset_Server("pec14_2", pec14_2(), t143())
    })
    
    # fim----
  })
}

# Play do Módulo
# ui <- dashboardPage(header = dashboardHeader(),
#                     sidebar = dashboardSidebar(),
#                     body = dashboardBody(fluidPage(economia_pec_pa_ui("economia_pec_pa"))))
# 
# server <- function(input, output) {
#   economia_pec_pa_Server("economia_pec_pa")
# }
# 
# shinyApp(ui, server)
