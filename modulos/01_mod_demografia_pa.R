#Funções de módulo de Demografia Estadual
#Função de UI
demografia_pa_ui <- function(id) {
  fluidPage(
    tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "estilo.css")
  ),
  div(class = "navbar_demografia",
      navbarPage(
        tags$b("Demografia - Pará"),
        navbarMenu( 
          tags$b("Escolha um Indicador"),
          #1 - População Total e Estimativas poplulacionais----
          tabPanel(
            "Estimativas Populacionais",
            panel(
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Estimativas Populacionais"), 
              ##Controle----
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "demo1ano"),
                  label = "Ano",
                  choices = sort(unique(demo1[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "demo1ri"),
                  label = "Pará/Região de Integração",
                  choices <- c("Pará",unique(demo1[["ri"]]) %>% sort() %>% setdiff("Pará")),
                  width = "200px"
                )
              )
            ),
            ##Mapa - População Total e Estimativas poplulacionais----
            fluidRow(
              box(
                title = textOutput(NS(id, "demo1txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "demo1map"), height = "600px"),
                  type = 8,
                  color = "#3C8DBD",
                  size = 0.5
                ),
                footer = list(tags$h6(
                  tags$b("Fonte:", style = 'font-family: sans-serif;'), "IBGE"
                ),
                tags$h6(
                  tags$b("Elaboração:", style = 'font-family: sans-serif;'),
                  "FAPESPA"
                ))
              ),
              ##Tabela - População Total e Estimativas poplulacionais----
              box(
                title = textOutput(NS(id, "demo1txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "demo1tab"), height = "400px"),
                  type = 8,
                  color = "#3C8DBD",
                  size = 0.5
                ),
                footer = 
                  list(
                    column(
                  11,
                  tags$h6(tags$b("Fonte:", 
                                 style = 'font-family: sans-serif;'), "IBGE"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "demo1_1")))
              )
            ),
            ##Gráfico - População Total e Estimativas poplulacionais----
            fluidRow(
              box(
                title = textOutput(NS(id, "demo1txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "demo1graf")),
                  type = 8,
                  color = "#3C8DBD",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:", style = 'font-family: sans-serif;'), "IBGE"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "demo1_2")))
              )
            )
          ),
          #2 - População por Faixa Etária----
          tabPanel (
            "População por Faixa Etária",
            panel(
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "População por Faixa Etária"), 
              ##Controle----
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "demo2ano"),
                  label = "Ano",
                  choices = sort(unique(demo2[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "demo2ri"),
                  label = "Pará/Região de Integração",
                  choices <- c("Pará",unique(demo1[["ri"]]) %>% sort() %>% setdiff("Pará")),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ##Mapa - População por Faixa Etária----
              box(
                title = textOutput(NS(id, "demo2txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                
                pickerInput(
                  inputId = NS(id, "demo2cat"),
                  label = "Faixa Etária",
                  choices = c("Predominância",unique(demo2[["categoria"]])),
                  width = "200px"
                ),
                withSpinner(
                  leafletOutput(NS(id, "demo2map"), height = "600px"),
                  type = 8,
                  color = "#3C8DBD",
                  size = 0.5
                ),
                footer = list(tags$h6(
                  tags$b("Fonte:", style = 'font-family: sans-serif;'), "IBGE"
                ),
                tags$h6(tags$b("Elaboração:"), "FAPESPA"))
              ),
              ##Tabela - População por Faixa Etária----
              box(
                title = textOutput(NS(id, "demo2txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "demo2tab"), height = "600px"),
                  type = 8,
                  color = "#3C8DBD",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:", 
                                 style = 'font-family: sans-serif;'), "IBGE"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "demo2_1")))
              )
            ),
            fluidRow(
              ##Gráfico - População por Faixa Etária----
              box(
                title = textOutput(NS(id, "demo2txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "demo2graf1")),
                  type = 8,
                  color = "#3C8DBD",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:", 
                                 style = 'font-family: sans-serif;'), "IBGE"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "demo2_2")))
              )
            )
          ),
          #3 - População por sexo----
          tabPanel(
            "População por sexo",
            panel(
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "População por sexo"), 
              ##Controle----
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "demo3ano"),
                  label = "Ano",
                  choices = sort(unique(demo3[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "demo3ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(demo3[["ri"]]),
                  width = "200px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "demo3cat"),
                  label = "Sexo",
                  choices = unique(demo3[["categoria"]]),
                  width = "200px"
                )
              )
            ),
            ##Mapas População por sexo----
            fluidRow(
              box(
                title = textOutput(NS(id, "demo3txt1")),
                collapsible = T,
                collapsed = F,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "demo3map"), height = "600px"),
                  type = 8,
                  color = "#3C8DBD",
                  size = 0.5
                ),
                footer = list(tags$h6(
                  tags$b("Fonte:", style = 'font-family: sans-serif;'), "IBGE"
                ),
                tags$h6(tags$b("Elaboração:"), "FAPESPA"))
              ),
              ## Tabela - População por sexo----
              box(
                title = textOutput(NS(id, "demo3txt2")),
                collapsible = T,
                collapsed = F,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "demo3tab"), height = "400px"),
                  type = 8,
                  color = "#3C8DBD",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:", 
                                 style = 'font-family: sans-serif;'), "IBGE"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "demo3_1")))
              )
            ),
            ## Gráfico - População por sexo----
            fluidRow(
              box(
                title = textOutput(NS(id, "demo3txt3")),
                collapsible = T,
                collapsed = F,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "demo3graf")),
                  type = 8,
                  color = "#3C8DBD",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:", 
                                 style = 'font-family: sans-serif;'), "IBGE"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "demo3_2")))
              )
            )
          ),
          # 4 - Razão de sexo----
          tabPanel (
            "Razão de sexo",
            panel(
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Razão de sexo"), 
              ##Controle----
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "demo4ano"),
                  label = "Ano",
                  choices = sort(unique(demo4[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "demo4ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(demo4[["ri"]]),
                  width = "200px"
                )
              )
            ),
            ## Mapa - Razão de sexo----
            fluidRow(
              box(
                title = textOutput(NS(id, "demo4txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "demo4map"), height = "600px"),
                  type = 8,
                  color = "#3C8DBD",
                  size = 0.5
                ),
                footer = list(tags$h6(
                  tags$b("Fonte:", style = 'font-family: sans-serif;'), "IBGE"
                ),
                tags$h6(tags$b("Elaboração:"), "FAPESPA"))
              ),
              ##Tabela - Razão de sexo----
              box(
                title = textOutput(NS(id, "demo4txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "demo4tab"), height = "400px"),
                  type = 8,
                  color = "#3C8DBD",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:", style = 'font-family: sans-serif;'), "IBGE"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "demo4_1")))
              )
            ),
            ##Gráfico - Razão de sexo----
            fluidRow(
              box(
                title = textOutput(NS(id, "demo4txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "demo4graf")),
                  type = 8,
                  color = "#3C8DBD",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:", style = 'font-family: sans-serif;'), "IBGE"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "demo4_2")))
              )
            )
          ),
          #5 - Proporção de idosos----
          tabPanel (
            "Proporção de idosos",
            panel(
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Proporção de idosos"), 
              ##Controle----
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "demo5ano"),
                  label = "Ano",
                  choices = sort(unique(demo5[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "demo5ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(demo5[["ri"]]),
                  width = "200px"
                )
              )
            ),
            ## Mapa Proporção de idosos----
            fluidRow(
              box(
                title = textOutput(NS(id, "demo5txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "demo5map"), height = "600px"),
                  type = 8,
                  color = "#3C8DBD",
                  size = 0.5
                ),
                tags$h6(tags$b("Fonte:", style = 'font-family: sans-serif;'), "IBGE"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")
              ),
              ##Tabela - Proporção de idosos----
              box(
                title = textOutput(NS(id, "demo5txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "demo5tab"), height = "400px"),
                  type = 8,
                  color = "#3C8DBD",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:", style = 'font-family: sans-serif;'), "IBGE"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "demo5_1")))
              )
            ),
            ##Gráfico - Proporção de idosos----
            fluidRow(
              box(
                title = textOutput(NS(id, "demo5txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "demo5graf")),
                  type = 8,
                  color = "#3C8DBD",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:", style = 'font-family: sans-serif;'), "IBGE"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "demo5_2")))
              )
            )
          ),
          #6 - Índice de envelhecimento----
          tabPanel (
            "Índice de envelhecimento",
            panel(
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Índice de envelhecimento"), 
              ##Controle----
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "demo6ano"),
                  label = "Ano",
                  choices = sort(unique(demo6[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "demo6ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(demo6[["ri"]]),
                  width = "200px"
                )
              )
            ),
            ##Mapa Índice de envelhecimento----
            fluidRow(
              box(
                title = textOutput(NS(id, "demo6txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "demo6map"), height = "600px"),
                  type = 8,
                  color = "#3C8DBD",
                  size = 0.5
                ),
                footer = list(tags$h6(
                  tags$b("Fonte:", style = 'font-family: sans-serif;'), "IBGE"
                ),
                tags$h6(tags$b("Elaboração:"), "FAPESPA"))
              ),
              ##Tabela - Índice de envelhecimento----
              box(
                title = textOutput(NS(id, "demo6txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "demo6tab"), height = "400px"),
                  type = 8,
                  color = "#3C8DBD",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:", style = 'font-family: sans-serif;'), "IBGE"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "demo6_1")))
              )
            ),
            ##Gráfico - Índice de envelhecimento----
            fluidRow(
              box(
                title = textOutput(NS(id, "demo6txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "demo6graf")),
                  type = 8,
                  color = "#3C8DBD",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:", style = 'font-family: sans-serif;'), "IBGE"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "demo6_2")))
              )
            )
          ),
          #7 - Razão de dependência----
          tabPanel(
            "Razão de dependência",
            panel(
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Razão de dependência"), 
              ##Controle----
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "demo7ano"),
                  label = "Ano",
                  choices = sort(unique(demo7[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "demo7ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(demo7[["ri"]]),
                  width = "200px"
                )
              )
            ),
            ##Mapa Razão de dependência----
            fluidRow(
              box(
                title = textOutput(NS(id, "demo7txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "demo7map"), height = "600px"),
                  type = 8,
                  color = "#3C8DBD",
                  size = 0.5
                ),
                footer = list(tags$h6(
                  tags$b("Fonte:", style = 'font-family: sans-serif;'), "IBGE"
                ),
                tags$h6(tags$b("Elaboração:"), "FAPESPA"))
              ),
              ##Tabela - Razão de dependência----
              box(
                title = textOutput(NS(id, "demo7txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "demo7tab"), height = "400px"),
                  type = 8,
                  color = "#3C8DBD",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:", style = 'font-family: sans-serif;'), "IBGE"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "demo7_1")))
              )
            ),
            ##Gráfico - Razão de dependência----
            fluidRow(
              box(
                collapsible = T,
                collapsed = F,
                title = textOutput(NS(id, "demo7txt3")),
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "demo7graf")),
                  type = 8,
                  color = "#3C8DBD",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:", style = 'font-family: sans-serif;'), "IBGE"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "demo7_2")))
              )
            )
          ),
          #8 - Taxa de fecundidade Total----
          tabPanel(
            "Taxa de fecundidade Total",
            panel(
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Taxa de fecundidade Total"), 
              ##Controle----
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "demo8ano"),
                  label = "Ano",
                  choices = sort(unique(demo8[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "demo8ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(demo8[["ri"]]),
                  width = "200px"
                )
              )
            ),
            ##Mapa Taxa de fecundidade Total----
            fluidRow(
              box(
                title = textOutput(NS(id, "demo8txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "demo8map"), height = "600px"),
                  type = 8,
                  color = "#3C8DBD",
                  size = 0.5
                ),
                footer = list(tags$h6(
                  tags$b("Fonte:", style = 'font-family: sans-serif;'), "IBGE"
                ),
                tags$h6(tags$b("Elaboração:"), "FAPESPA"))
              ),
              ##Tabela - Taxa de fecundidade Total----
              box(
                title = textOutput(NS(id, "demo8txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "demo8tab"), height = "400px"),
                  type = 8,
                  color = "#3C8DBD",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:", style = 'font-family: sans-serif;'), "IBGE"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "demo8_1")))
              )
            ),
            ##Gráfico de Barras Taxa de fecundidade Total----
            fluidRow(
              box(
                title = textOutput(NS(id, "demo8txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "demo8graf")),
                  type = 8,
                  color = "#3C8DBD",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:", style = 'font-family: sans-serif;'), "IBGE"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "demo8_2")))
              )
            )
          ),
          # 9 - Taxa específica de fecundidade por faixa etária----
          tabPanel (
            "Taxa específica de fecundidade por faixa etária",
            panel(
              heading = 
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                 "Taxa específica de fecundidade por faixa etária"), 
              ##Controle----
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "demo9ano"),
                  label = "Ano",
                  choices = sort(unique(demo9[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "demo9ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(demo9[["ri"]]),
                  width = "200px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "demo9cat"),
                  label = "Faixa Etária",
                  choices = unique(demo9[["categoria"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Taxa específica de fecundidade por faixa etária----
              box(
                title = textOutput(NS(id, "demo9txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "demo9map"), height = "600px"),
                  type = 8,
                  color = "#3C8DBD",
                  size = 0.5
                ),
                footer = list(tags$h6(
                  tags$b("Fonte:", style = 'font-family: sans-serif;'), "IBGE"
                ),
                tags$h6(tags$b("Elaboração:"), "FAPESPA"))
              ),
              ## Tabela - Taxa específica de fecundidade por faixa etária----
              box(
                title = textOutput(NS(id, "demo9txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "demo9tab"), height = "400px"),
                  type = 8,
                  color = "#3C8DBD",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:", style = 'font-family: sans-serif;'), "IBGE"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "demo9_1")))
              )
            ),
            fluidRow(
              ## Gráfico - Taxa específica de fecundidade por faixa etária----
              box(
                title = textOutput(NS(id, "demo9txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                #Seletor Ano
                withSpinner(
                  echarts4rOutput(NS(id, "demo9graf1")),
                  type = 8,
                  color = "#3C8DBD",
                  size = 0.5
                ),
                footer = list(column(
                  11,
                  tags$h6(tags$b("Fonte:", style = 'font-family: sans-serif;'), "IBGE"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                ),
                downset_ui(NS(id, "demo9_2")))
              )
            )
          )
        )
      )))
}

#Função do modulo servidor
demografia_pa_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    #TÍTULOS----
    #1 - População Total e Estimativas poplulacionais----
    ## Mapa - População Total e Estimativas poplulacionais----
    t11 <- reactive({
      if (input$demo1ri == "Pará") {
        paste0(
          "População Total e Estimativas Populacionais, Pará - ",
          input$demo1ano
        )
      } else{
        paste0(
          "População Total e Estimativas Populacionais, Região de Integração ",
          input$demo1ri,
          " - ",
          input$demo1ano
        )
      }})
    ## Tabela - População Total e Estimativas poplulacionais----
    t12 <- reactive({
      if (input$demo1ri == "Pará") {
        paste0(
          "População Total e Estimativas Populacionais por Município, Pará - ",
          input$demo1ano
        )
      } else{
        paste0(
          "População Total e Estimativas Populacionais por Município, Região de Integração ",
          input$demo1ri,
          " - ",
          input$demo1ano
        )
      }
    })
    ## Gráfico - População Total e Estimativas poplulacionais----
    t13 <- reactive({
      paste0(
        "População Total e Estimativas Populacionais, Pará - ",
        min(demo1$ano),
        " a ",
        max(demo1$ano)
      )})
    
    #2 - População por Faixa Etária----
    ## Mapa - População por Faixa Etária----
    t21 <- reactive({
      if (input$demo2cat == "Predominância") {
        if (input$demo2ri == "Pará") {
          paste0(
            "População Predominante por Faixa Etária - Pará - ",
            input$demo2ano
          )
        } else{
          paste0(
            "População Predominante por Faixa Etária - Região de Integração ",
            input$demo2ri,
            " - ",
            input$demo2ano
          )
        }  
      } else {
      if (input$demo2ri == "Pará") {
        paste0(
          "População de ",
          input$demo2cat,
          ", Pará - ",
          input$demo2ano
        )
      } else{
        paste0(
          "População de ",
          input$demo2cat,
          ", Região de Integração ",
          input$demo2ri,
          " - ",
          input$demo2ano
        )
      }}
    })
    ## Tabela - População por Faixa Etária----
    t22 <- reactive({
      if (input$demo2ri == "Pará") {
        paste0(
          "População por Faixa Etária por Município, Pará - ",
          input$demo2ano
        )
      } else{
        paste0(
          "População por Faixa Etária por Município, Região de Integração ",
          input$demo2ri,
          " - ",
          input$demo2ano
        )
      }
    })
    ## Gráfico - População por Faixa Etária----
    t23 <- reactive({
      paste0("População por Faixa Etária, Pará - ",
             input$demo2ano)
    })
    
    #3 - População por sexo----
    ## Mapas por sexo----
    t31 <- reactive({
      if (input$demo3ri == "Pará") {
        paste0(
          "População do sexo " ,
          input$demo3cat,
          ", Pará - ",
          input$demo3ano
        )
      } else{
        paste0(
          "População do sexo ",
          input$demo3cat,
          ", Região de Integração ",
          input$demo3ri,
          " - ",
          input$demo3ano
        )
      }
    })
    ## Tabela - População por sexo----
    t32 <- reactive({
      if (input$demo3ri == "Pará") {
        paste0(
          "População por sexo por Município, ",
          input$demo3ri,
          " - ",
          input$demo3ano
        )
      } else{
        paste0(
          "População por sexo, Região de Integração ",
          input$demo3ri,
          " - ",
          input$demo3ano
        )
      }
    })
    ## Gráfico - População por sexo----
    t33 <- reactive({
      paste0(
        "População por sexo, Pará - ",
        min(demo3$ano),
        " a ",
        max(demo3$ano)
      )
    })
    
    # 4 - Razão de sexo----
    ## Mapa - Razão de sexo----
    t41 <- reactive({
      if (input$demo4ri == "Pará") {
        paste0(
          "Razão de sexo, Pará - ",
          input$demo4ano
        )
      } else{
        paste0(
          "Razão de sexo, Região de Integração ",
          input$demo4ri,
          " - ",
          input$demo4ano
        )
      }
    })
    ## Tabela - Razão de sexo----
    t42 <- reactive({
      if (input$demo4ri == "Pará") {
        paste0(
          "Razão de sexo por Município, Pará - ",
          input$demo4ano
        )
      } else{
        paste0(
          "Razão de sexo por Município, Região de Integração ",
          input$demo4ri,
          " - ",
          input$demo4ano
        )
      }
    })
    ## Gráfico - Razão de sexo----
    t43 <- reactive({
      paste0("Razão de sexo, Pará - ",
             min(demo4$ano),
             " a ",
             max(demo4$ano))
    })
    
    #5 - Proporção de idosos----
    ## Mapa Proporção de idosos----
    t51 <- reactive({
      if (input$demo5ri == "Pará") {
        paste0(
          "Proporção de idosos, ",
          " Pará - ",
          input$demo5ano
        )
      } else{
        paste0(
          "Proporção de idosos, Região de Integração ",
          input$demo5ri,
          " - ",
          input$demo5ano
        )
      }})
    ## Tabela - Proporção de idosos----
    t52 <- reactive({
      if (input$demo5ri == "Pará") {
        paste0(
          "Proporção de idosos por Município, Pará - ",
          input$demo5ano
        )
      } else{
        paste0(
          "Proporção de idosos por Município, Região de Integração ",
          input$demo5ri,
          " - ",
          input$demo5ano
        )
      }
    })
    ## Gráfico - Proporção de idosos----
    t53 <- reactive({
       paste0(
        "Proporção de idosos, Pará - ",
        min(demo5$ano),
        " a ",
        max(demo5$ano)
      )
    })
    
    #6 - Índice de envelhecimento----
    ## Mapa Índice de envelhecimento----
    t61 <- reactive({
      if (input$demo6ri == "Pará") {
        paste0(
          "Índice de envelhecimento, Pará - ",
          input$demo6ano
        )
      } else{
        paste0(
          "Índice de envelhecimento, Região de Integração ",
          input$demo6ri,
          " - ",
          input$demo6ano
        )
      }
    })
    ## Tabela - Índice de envelhecimento----
    t62 <- reactive({
      if (input$demo6ri == "Pará") {
        paste0(
          "Índice de envelhecimento por Município, Pará - ",
          input$demo6ano
        )
      } else{
        paste0(
          "Índice de envelhecimento por Município, Região de Integração ",
          input$demo6ri,
          " - ",
          input$demo6ano
        )
      }
    })
    ## Gráfico - Índice de envelhecimento----
    t63 <- reactive({
       paste0(
        "Índice de envelhecimento, Pará - ",
        min(demo6$ano),
        " a ",
        max(demo6$ano)
      )
    })
    
    #7 - Razão de dependência----
    ## Mapa Razão de dependência----
    t71 <- reactive({
      if (input$demo7ri == "Pará") {
        paste0(
          "Razão de dependência, Pará - ",
          input$demo7ano
        )
      } else{
        paste0(
          "Razão de dependência, Região de Integração ",
          input$demo7ri,
          " - ",
          input$demo7ano
        )
      }
    })
    ## Tabela - Razão de dependência----
    t72 <- reactive({
      if (input$demo7ri == "Pará") {
        paste0(
          "Razão de dependência por Município, Pará - ",
          input$demo7ano
        )
      } else{
        paste0(
          "Razão de dependência por Município, Região de Integração ",
          input$demo7ri,
          " - ",
          input$demo7ano
        )
      }
    })
    ## Gráfico - Razão de dependência----
    t73 <- reactive({
      paste0(
        "Razão de dependência, Pará - ",
        min(demo7$ano),
        " a ",
        max(demo7$ano)
      )
    })
    
    #8 - Taxa de fecundidade Total----
    ## Mapa Taxa de fecundidade Total----
    t81 <- reactive({
      if (input$demo8ri == "Pará") {
        paste0(
          "Taxa de fecundidade, Pará - ",
          input$demo8ano
        )
      } else{
        paste0(
          "Taxa de fecundidade, Região de Integração ",
          input$demo8ri,
          " - ",
          input$demo8ano
        )
      }
    })
    ## Tabela - Taxa de fecundidade Total----
    t82 <- reactive({
      if (input$demo8ri == "Pará") {
        paste0(
          "Taxa de fecundidade por Município, Pará - ",
          input$demo8ano
        )
      } else{
        paste0(
          "Taxa de fecundidade por Município, Região de Integração ",
          input$demo8ri,
          " - ",
          input$demo8ano
        )
      }
    })
    ## Gráfico de Barras Taxa de fecundidade Total----
    t83 <- reactive({
      paste0(
        "Taxa de fecundidade, Pará - ",
        min(demo8$ano),
        " a ",
        max(demo8$ano)
      )
    })
    
    # 9 - Taxa específica de fecundidade por faixa etária----
    ## Mapa - Taxa específica de fecundidade por faixa etária----
    t91 <- reactive({
      if (input$demo9ri == "Pará") {
        paste0(
          "Taxa específica de fecundidade da População de ",
          input$demo9cat,
          ", Pará - ",
          input$demo9ano
        )
      } else{
        paste0(
          "Taxa específica de fecundidade da População de ",
          input$demo9cat,
          ", Região de Integração ",
          input$demo9ri,
          " - ",
          input$demo9ano
        )
      }
    })
    ## Tabela - Taxa específica de fecundidade por faixa etária----
    t92 <- reactive({
      if (input$demo9ri == "Pará") {
        paste0(
          "Taxa específica de fecundidade por faixa etária por Município, Pará - ",
          input$demo9ano
        )
      } else{
        paste0(
          "Taxa específica de fecundidade por faixa etária por Município, Região de Integração ",
          input$demo9ri,
          " - ",
          input$demo9ano
        )
      }
    })
    ## Gráfico - Taxa específica de fecundidade por faixa etária----
    t93 <- reactive({
      paste0(
        "Taxa específica de fecundidade por faixa etária, Pará - ",
        input$demo9ano
      )
    })

    #VISUALIZAÇÃO----
    #1 - População Total e Estimativas Populacionais ----
    ##Mapa - População Total e Estimativas poplulacionais----
    output$demo1txt1 <- renderText({
    t11()  
    })
    output$demo1map <- renderLeaflet({
      #Tratamento da informação
      ##Filtrando informação
      if (input$demo1ri == "Pará") {
        df <-
          demo1 %>% filter(localidade != "Pará", ano == input$demo1ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      }else{
        df <-
          demo1 %>% filter(localidade != "Pará", ano == input$demo1ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$demo1ri)
        
      }
      
      z <- x$valor

      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }
      
      pal <-
        colorBin(
          c("#B6EDF0", "#74B4E8", "#1F83E0", "#1D44B8", "#090991"),
          domain = x$valor,
          bins = bins
        )
      conteudo <-
        sprintf(
          "<strong>Município de %s</strong><br/> <b>População:</b> %s<br/> 
          <b>Percentual:</b> %s%% ",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
            format(x$valor, big.mark = ".", decimal.mark = ",")
            
          ),
          format(round((x$valor / sum(x$valor, na.rm = TRUE)) * 100, 2), decimal.mark = ",")
        ) %>% lapply(htmltools::HTML)
      #Mapas com leafleft
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
        ) %>% addLegend(
          pal = pal,
          values = ~ valor,
          opacity = 0.7,
          title = "População",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(
            big.mark = ".",
            decimal.mark = ",",
          )
        )
    })
    ##Tabela - População Total e Estimativas Populacionais ----
    output$demo1txt2 <- renderText({
    t12()  
    })
    output$demo1tab <- renderReactable({
      if (input$demo1ri == "Pará") {
        x <-
          demo1 %>% 
          filter(localidade != "Pará", ano == input$demo1ano) %>%
          select(ri, localidade, valor) %>%
          mutate(Percentual = (valor / sum(valor)) * 100)
      }      else{
        x <-
          demo1 %>% 
          filter(localidade != "Pará", ano == input$demo1ano) %>%
          select(ri, localidade, valor)
        x <-
          x %>% filter(ri == input$demo1ri) %>%
          mutate(Percentual = (valor / sum(valor)) * 100)
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
        columns =  list(
          ri = colDef(name = "Região de Integração"),
          localidade = colDef(name = "Municípios"),
          valor = colDef(
            name = "População",
            format = colFormat(separators = T, locales = "pt-BR")
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
    ## Gráfico - População Total e Estimativas poplulacionais----
    output$demo1txt3 <- renderText({
    t13()  
    })
    output$demo1graf <- renderEcharts4r({
      demo1 %>% filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#3C8DBD",
          name = "População",
          legend = F,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
          itemStyle = list (barBorderRadius =  5)
        ) %>%
        e_tooltip(
          trigger = "axis",
          formatter =
            e_tooltip_pointer_formatter("decimal", digits = 0, locale = "pt-BR"),
          axisPointer = list(type = 'shadow')
        ) %>%
        e_x_axis(
          axisLabel = list(show = T, fontSize = 11),
          splitLine = list(show = T),
          name = "Ano",
          splitLine = list(show = T),
          nameTextStyle = list(fontWeight = "bold", fontSize = 14)
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
        e_datazoom(toolbox = F,  fillerColor = "#E5F5F9") %>%
        e_grid(show = T)
    })
    #2-População por Faixa Etária----
    ##Mapa - População por Faixa Etária----
    output$demo2txt1 <- renderText({
    t21()  
    })
    output$demo2map <- renderLeaflet({
      if (input$demo2cat == "Predominância") {
        
        if (input$demo2ri == "Pará") {
          df <- demo2 %>% filter(localidade != "Pará", ano == input$demo2ano) %>%
            pivot_wider(names_from = "categoria", values_from = "valor")
            
            x <- cbind(geopa, df)
        }else{
          df <- demo2 %>% filter(localidade != "Pará", ano == input$demo2ano) %>%
            pivot_wider(names_from = "categoria", values_from = "valor")
            
            x <- cbind(geopa, df)
            x <- x %>% filter(ri == input$demo2ri)
        }
        
        # Criar uma coluna que define a cor com base na faixa etária predominante
        
        # Ajuste dos dados com mutate
        x <- x %>% 
          mutate(
            valor = pmax(X0.a.4.anos, X5.a.9.anos, X10.a.14.anos, X15.a.19.anos, X20.a.29.anos,  
                         X30.a.39.anos, X40.a.49.anos, X50.a.59.anos, X60.a.69.anos, X70.a.79.anos, X80.anos.e.mais, na.rm = TRUE),
            faixa_predominante = case_when(
              valor == 0 ~ "Nenhum",
              X0.a.4.anos == valor ~ "0 a 4 anos",
              X5.a.9.anos == valor ~ "5 a 9 anos",
              X10.a.14.anos == valor ~ "10 a 14 anos",
              X15.a.19.anos == valor ~ "15 a 19 anos",
              X20.a.29.anos == valor ~ "20 a 29 anos",
              X30.a.39.anos == valor ~ "30 a 39 anos",
              X40.a.49.anos == valor ~ "40 a 49 anos",
              X50.a.59.anos == valor ~ "50 a 59 anos",
              X60.a.69.anos == valor ~ "60 a 69 anos",
              X70.a.79.anos == valor ~ "70 a 79 anos",
              X80.anos.e.mais == valor ~ "80 anos e mais",
              TRUE ~ "Indefinido"
            ),
            cor = case_when(
              faixa_predominante == "Nenhum" ~ "gray",
              faixa_predominante == "0 a 4 anos" ~ "#fdae61",
              faixa_predominante == "5 a 9 anos" ~ "#fee08b",
              faixa_predominante == "10 a 14 anos" ~ "#d73027",
              faixa_predominante == "15 a 19 anos" ~ "#91bfdb",
              faixa_predominante == "20 a 29 anos" ~ "#23eb70",
              faixa_predominante == "30 a 39 anos" ~ "#953184",
              faixa_predominante == "40 a 49 anos" ~ "#542788",
              faixa_predominante == "50 a 59 anos" ~ "#f0170f",
              faixa_predominante == "60 a 69 anos" ~ "#8b7191",
              faixa_predominante == "70 a 79 anos" ~ "#0fe5f5",
              faixa_predominante == "80 anos e mais" ~ "#d6b3ff",
              TRUE ~ "gray"
            )
          )
        
        # Configuração do conteúdo das labels
        conteudo <- sprintf(
          "<strong>Município de %s</strong><br/>
       <b>0 a 4 anos: </b> %s<br/>
       <b>5 a 9 anos:</b> %s<br/>
       <b>10 a 14 anos:</b> %s<br/>
       <b>15 a 19 anos:</b> %s<br/>
       <b>20 a 29 anos:</b> %s<br/>
       <b>30 a 39 anos:</b> %s<br/>
       <b>40 a 49 anos:</b> %s<br/>
       <b>50 a 59 anos:</b> %s<br/>
       <b>60 a 69 anos:</b> %s<br/>
       <b>70 a 79 anos:</b> %s<br/>
       <b>80 anos e mais:</b> %s<br/>
       <b>Predominância:</b> %s",
          x$name_muni,
          format(x$X0.a.4.anos, big.mark = ".", decimal.mark = ","),
          format(x$X5.a.9.anos, big.mark = ".", decimal.mark = ","),
          format(x$X10.a.14.anos, big.mark = ".", decimal.mark = ","),
          format(x$X15.a.19.anos, big.mark = ".", decimal.mark = ","),
          format(x$X20.a.29.anos, big.mark = ".", decimal.mark = ","),
          format(x$X30.a.39.anos, big.mark = ".", decimal.mark = ","),
          format(x$X40.a.49.anos, big.mark = ".", decimal.mark = ","),
          format(x$X50.a.59.anos, big.mark = ".", decimal.mark = ","),
          format(x$X60.a.69.anos, big.mark = ".", decimal.mark = ","),
          format(x$X70.a.79.anos, big.mark = ".", decimal.mark = ","),
          format(x$X80.anos.e.mais, big.mark = ".", decimal.mark = ","),
          x$faixa_predominante
        ) %>% lapply(htmltools::HTML)
        
        # Criação do mapa interativo com Leaflet
        leaflet(x, options = leafletOptions(minZoom = 0, maxZoom = 15, zoomControl = FALSE)) %>%
          addTiles() %>%
          htmlwidgets::onRender(
            "function(el, x) {
            L.control.zoom({ position: 'topright' }).addTo(this);
            }"
          ) %>%
          addPolygons(
            weight = 2,
            opacity = 1,
            color = "black",
            fillColor = ~cor,
            fillOpacity = 1,
            dashArray = 1,
            smoothFactor = 1.5,
            highlightOptions = highlightOptions(
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
            colors = c("#fdae61","#fee08b","#d73027","#91bfdb",
            "#23eb70","#953184","#542788","#f0170f","#8b7191",
            "#0fe5f5","#d6b3ff"),
            labels = c("0 a 4 anos", "5 a 9 anos", "10 a 14 anos", "15 a 19 anos", 
                       "20 a 29 anos", "30 a 39 anos", "40 a 49 anos", "50 a 59 anos",
                       "60 a 69 anos","70 a 79 anos","80 anos e mais"),
            opacity = 0.7,
            title = "Predominância por Faixa Etária",
            position = "bottomright"
          )
        
        
      }else{ #Mapa por faixa etária
        #Tratamento da informação
        ##Filtrando informação
        if (input$demo2ri == "Pará") {
          df <-
            demo2 %>% filter(localidade != "Pará",
                             ano == input$demo2ano,
                             categoria == input$demo2cat) %>%
            select(ri, localidade, ano, valor)
          x <- cbind(geopa, df)
        }      else{
          df <-
            demo2 %>% filter(localidade != "Pará",
                             ano == input$demo2ano,
                             categoria == input$demo2cat) %>%
            select(ri, localidade, ano, valor)
          x <- cbind(geopa, df)
          x <- x %>% filter(ri == input$demo2ri)
          
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
            c("#B6EDF0", "#74B4E8", "#1F83E0", "#1D44B8", "#090991"),
            domain = x$valor,
            bins = bins
          )
        conteudo <-
          sprintf(
            "<strong>%s</strong><br/> <b>População:</b> %s",
            x$name_muni,
            ifelse(
              is.na(x$valor),
              "Não disponível",
              format(x$valor, big.mark = ".", decimal.mark = ",")
            )
          ) %>% lapply(htmltools::HTML)
        #Mapas com leafleft
        leaflet(x, options = leafletOptions(minZoom = 0, maxZoom = 15,zoomControl = FALSE)) %>%
          addTiles() %>%
          htmlwidgets::onRender(
            "function(el, x) {
            L.control.zoom({ position: 'topright' }).addTo(this);
            }"
          ) %>%
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
          ) %>% addLegend(
            pal = pal,
            values = ~ valor,
            opacity = 0.7,
            title = "População",
            position = "bottomright",
            na.label = "Não disponível",
            labFormat = labelFormat_decimal(
              big.mark = ".",
              decimal.mark = ",",
              digits = 2
            )
          )  
      }
      
      
    })
    ##Tabela - População por Faixa Etária----
    output$demo2txt2 <- renderText({
    t22()     
    })
    output$demo2tab <- renderReactable({
      if (input$demo2ri == "Pará") {
        x <-
          demo2 %>% 
          filter(localidade != "Pará", ano == input$demo2ano) %>%
          select(ri,
                 localidade,
                 categoria,
                 valor) %>%
          pivot_wider(names_from = categoria, values_from = valor)
      }      else{
        x <-
          demo2 %>% 
          filter(localidade != "Pará", ano == input$demo2ano) %>%
          select(ri,
                 localidade,
                 categoria,
                 valor) %>%
          pivot_wider(names_from = categoria, values_from = valor)
        x <- x %>% filter(ri == input$demo2ri)
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
        columns =  list(
          ri = colDef(name = "Região de Integração"),
          localidade = colDef(name = "Municípios", sticky = "left")
        ),
        defaultColDef =
          colDef(
            footerStyle = list(fontWeight = "bold"),
            width = 200,
            headerStyle = list(background = "#f7f7f8"),
            format = colFormat(separators = T),
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
    ##Gráfico - População por Faixa Etária----
    output$demo2txt3 <- renderText({
      t23()
    })
    output$demo2graf1 <- renderEcharts4r({
      demo2 %>% filter(localidade == "Pará", ano == input$demo2ano) %>%
        e_chart(x = categoria) %>%
        e_bar(
          serie = valor,
          color = "#3C8DBD",
          name = "População",
          legend = F,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
          itemStyle = list (barBorderRadius =  5)
        ) %>%
        e_tooltip(
          trigger = "axis",
          formatter =
            e_tooltip_pointer_formatter("decimal", digits = 0, locale = "pt-BR"),
          axisPointer = list(type = 'shadow')
        ) %>%
        e_x_axis(
          axisLabel = list(show = T, fontSize = 11),
          splitLine = list(show = T),
          name = "Faixa Etária",
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
    })
    #3-População por sexo----
    ##Mapa - População de sexo----
    output$demo3txt1 <- renderText({
      t31()
    })
    output$demo3map <- renderLeaflet({
      #Tratamento da informação
      ##Filtrando informação
      if (input$demo3ri == "Pará") {
        df <-
          demo3 %>% filter(localidade != "Pará",
                           ano == input$demo3ano,
                           categoria == input$demo3cat) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      }      else{
        df <-
          demo3 %>% filter(localidade != "Pará",
                           ano == input$demo3ano,
                           categoria == input$demo3cat) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$demo3ri)
        
      }
      
      z <- x$valor
      
      # z <- x$valor[x$valor > 0]
      
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      
      # x$valor[x$valor == 0] <- NA
      
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }
      
      pal <-
        colorBin(
          c("#B6EDF0", "#74B4E8", "#1F83E0", "#1D44B8", "#090991"),
          domain = x$valor,
          bins = bins
        )
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>População:</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
            format(x$valor, big.mark = ".", decimal.mark = ",")
          )
        ) %>% lapply(htmltools::HTML)
      #Mapas com leafleft
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
        ) %>% addLegend(
          pal = pal,
          values = ~ valor,
          opacity = 0.7,
          title = "População",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(
            big.mark = ".",
            decimal.mark = ",",
            digits = 2
          )
        )
    })
    ##Tabela - População por sexo----
    output$demo3txt2 <- renderText({
      t32()
    })
    output$demo3tab <- renderReactable({
      if (input$demo3ri == "Pará") {
        x <-
          demo3 %>% filter(localidade != "Pará", ano == input$demo3ano) %>%
          select(ri,
                 localidade,
                 categoria,
                 valor) %>%
          pivot_wider(names_from = categoria, values_from = valor) %>%
          mutate(Masc_percentual = (Masculino / (Masculino + Feminino)) *
                   100) %>%
          mutate(Fem_percentual = (Feminino / (Masculino + Feminino)) *
                   100)
      }      else{
        x <-
          demo3 %>% filter(localidade != "Pará", ano == input$demo3ano) %>%
          select(ri,
                 localidade,
                 categoria,
                 valor) %>%
          pivot_wider(names_from = categoria, values_from = valor) %>%
          mutate(Masc_percentual = (Masculino / (Masculino + Feminino)) *
                   100) %>%
          mutate(Fem_percentual = (Feminino / (Masculino + Feminino)) *
                   100)
        x <- x %>% filter(ri == input$demo3ri)
      }
      x <-
        x %>% select(
          ri,
          localidade,
          Masculino,
          Feminino,
          Masc_percentual,
          Fem_percentual
        )
      
      x %>% reactable(
        defaultPageSize = 10,
        striped = FALSE,
        highlight = TRUE,
        bordered = TRUE,
        outlined = TRUE,
        resizable = TRUE,
        showSortable = TRUE,
        pagination = F,
        columns =  list(
          ri = colDef(name = "Região de Integração"),
          localidade = 
            colDef(name = "Municípios", sticky = "left"),
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
    
    ##Gráfico - População por sexo----
    output$demo3txt3 <- renderText({
      t33()
    })
    output$demo3graf <- renderEcharts4r({
      demo3 %>% filter(localidade == "Pará") %>%
        pivot_wider(names_from = categoria, values_from = valor) %>%
        e_chart(x = ano) %>%
        e_line(
          serie = Masculino,
          color = "#3C8DBD",
          name = "Masculino",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
          itemStyle = list (barBorderRadius =  5)
        ) %>%
        e_line(
          serie = Feminino,
          color = "#91cc75",
          name = "Feminino",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
          itemStyle = list (barBorderRadius =  5)
        ) %>%
        e_tooltip(
          trigger = "axis",
          formatter =
            e_tooltip_pointer_formatter("decimal", digits = 0, locale = "pt-BR"),
          axisPointer = list(type = 'shadow')
        ) %>%
        e_x_axis(
          axisLabel = list(show = T, fontSize = 11),
          splitLine = list(show = T),
          name = "Ano",
          splitLine = list(show = T),
          nameTextStyle = list(fontWeight = "bold", fontSize = 14)
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
        e_datazoom(toolbox = F,  fillerColor = "#E5F5F9") %>%
        e_grid(show = T)
    })
    
    #4-Razão de sexo  ----
    ##Mapa - Razão de sexo----
    output$demo4txt1 <- renderText({
    t41()  
    })
    output$demo4map <- renderLeaflet({
      #Tratamento da informação
      ##Filtrando informação
      if (input$demo4ri == "Pará") {
        df <-
          demo4 %>% filter(localidade != "Pará", ano == input$demo4ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      }      else{
        df <-
          demo4 %>% filter(localidade != "Pará", ano == input$demo4ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$demo4ri)
        
      }
      
      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }
      
      
      # Criar uma coluna que define a cor com base no sexo predominante
      x <- x %>%
        mutate(
          cor = case_when(
            valor < 100 ~ "#e616d4",   # Rosa para predominância de mulheres
            valor > 100 ~ "#1159df",   # Azul para predominância de homens
            valor == 100 ~ "#33b60b"   # Verde para empate
          )
        )
      
      # Criar conteúdo para as labels, mostrando o número de condutores por sexo e o sexo predominante
      conteudo <- sprintf(
        "<strong>Município de %s</strong><br/>
           <b>Predominância:</b> %s<br/>
           <b>Razão:</b> %s",
        x$name_muni,
        ifelse(x$valor < 100, "Mulheres","Homens"),
        format(round(x$valor, 2), big.mark = ".", decimal.mark = ",")
      ) %>% lapply(htmltools::HTML)
      
      #Mapas com leafleft
      leaflet(x, options = leafletOptions(minZoom = 0, maxZoom = 15)) %>%
        addTiles() %>%
        addPolygons(
          weight = 2,
          opacity = 1,
          color = "black",
          fillOpacity = 1,
          fillColor = ~cor,
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
          colors = c("#1159df", "#e616d4", "#33b60b"),  # Incluir a cor verde e cinza na legenda
          labels = c("Predominância Masculina", "Predominância Feminina", "Empate"),
          opacity = 0.7,
          title = "Predominância por Sexo",
          position = "bottomright"
        )  
    })
    ##Tabela Razão de sexo----
    output$demo4txt2 <- renderText({
      t42()
    })
    output$demo4tab <- renderReactable({
      if (input$demo4ri == "Pará") {
        x <-
          demo4 %>% filter(localidade != "Pará", ano == input$demo4ano) %>%
          select(ri, localidade, ano, valor)
      }      else{
        x <-
          demo4 %>% filter(localidade != "Pará", ano == input$demo4ano) %>%
          select(ri, localidade, ano, valor)
        x <- x %>% filter(ri == input$demo4ri)
      }
      x <-
        x %>% select(ri, localidade, valor)
      x %>% reactable(
        defaultPageSize = 10,
        striped = FALSE,
        highlight = TRUE,
        bordered = TRUE,
        outlined = TRUE,
        resizable = TRUE,
        showSortable = TRUE,
        pagination = F,
        columns =  list(
          ri = colDef(name = "Região de Integração"),
          localidade = colDef(name = "Municípios"),
          valor = colDef(
            name = "Razão",
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
    ##Gráfico - Razão de sexo----
    output$demo4txt3 <- renderText({
    t43()  
    })
    output$demo4graf <- renderEcharts4r({
      demo4 %>% filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#3C8DBD",
          name = "Razão",
          legend = F,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
          itemStyle = list (barBorderRadius =  5)
        ) %>%
        e_tooltip(
          trigger = "axis",
          formatter =
            e_tooltip_pointer_formatter("decimal", digits = 2, locale = "pt-BR"),
          axisPointer = list(type = 'shadow')
        ) %>%
        e_x_axis(
          axisLabel = list(show = T, fontSize = 11),
          splitLine = list(show = T),
          name = "Ano",
          splitLine = list(show = T),
          nameTextStyle = list(fontWeight = "bold", fontSize = 14)
        ) %>%
        e_y_axis(
          name = "Razão",
          nameTextStyle =
            list(fontWeight = "bold", fontSize = 14),
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
        e_datazoom(toolbox = F,  fillerColor = "#E5F5F9") %>%
        e_grid(show = T)
    })
    #5-Proporção de idosos  ----
    ##Mapa - Proporção de idosos----
    output$demo5txt1 <- renderText({
    t51()  
    })
    output$demo5map <- renderLeaflet({
      #Tratamento da informação
      ##Filtrando informação
      if (input$demo5ri == "Pará") {
        df <-
          demo5 %>% filter(localidade != "Pará", ano == input$demo5ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      }      else{
        df <-
          demo5 %>% filter(localidade != "Pará", ano == input$demo5ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$demo5ri)
        
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
          c("#B6EDF0", "#74B4E8", "#1F83E0", "#1D44B8", "#090991"),
          domain = x$valor,
          bins = bins
        )
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Proporção:</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
            format(
              x$valor,
              big.mark = ".",
              decimal.mark = ",",
              digits = 3
            )
          )
        ) %>% lapply(htmltools::HTML)
      #Mapas com leafleft
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
        ) %>% addLegend(
          pal = pal,
          values = ~ valor,
          opacity = 0.7,
          title = "Proporção",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(
            big.mark = ".",
            decimal.mark = ",",
            digits = 2
          )
        )
    })
    ##Tabela - Proporção de idosos----
    output$demo5txt2 <- renderText({
      t52()
    })
    output$demo5tab <- renderReactable({
      if (input$demo5ri == "Pará") {
        x <-
          demo5 %>% filter(localidade != "Pará", ano == input$demo5ano) %>%
          select(ri, localidade, ano, valor)
      }      else{
        x <-
          demo5 %>% filter(localidade != "Pará", ano == input$demo5ano) %>%
          select(ri, localidade, ano, valor)
        x <- x %>% filter(ri == input$demo5ri)
      }
      x <-
        x %>% select(ri, localidade, valor)
      x %>% reactable(
        defaultPageSize = 10,
        striped = FALSE,
        highlight = TRUE,
        bordered = TRUE,
        outlined = TRUE,
        resizable = TRUE,
        showSortable = TRUE,
        pagination = F,
        columns =  list(
          ri = colDef(name = "Região de Integração"),
          localidade = colDef(name = "Municípios"),
          valor = colDef(
            name = "Proporção",
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
    ##Gráfico - Proporção de idosos----
    output$demo5txt3 <- renderText({
     t53()
    })
    output$demo5graf <- renderEcharts4r({
      demo5 %>% filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#3C8DBD",
          name = "Proporção",
          legend = F,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
          itemStyle = list (barBorderRadius =  5)
        ) %>%
        e_tooltip(
          trigger = "axis",
          formatter =
            e_tooltip_pointer_formatter("decimal", digits = 2, locale = "pt-BR"),
          axisPointer = list(type = 'shadow')
        ) %>%
        e_x_axis(
          axisLabel = list(show = T, fontSize = 11),
          splitLine = list(show = T),
          name = "Ano",
          splitLine = list(show = T),
          nameTextStyle = list(fontWeight = "bold", fontSize = 14)
        ) %>%
        e_y_axis(
          name = "Proporção",
          nameTextStyle =
            list(fontWeight = "bold", fontSize = 14),
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
        e_datazoom(toolbox = F,  fillerColor = "#E5F5F9") %>%
        e_grid(show = T)
    })
    #6-Índice de envelhecimento ----
    ##Mapa - Índice de envelhecimento----
    output$demo6txt1 <- renderText({
    t61()  
    })
    output$demo6map <- renderLeaflet({
      #Tratamento da informação
      ##Filtrando informação
      if (input$demo6ri == "Pará") {
        df <-
          demo6 %>% filter(localidade != "Pará", ano == input$demo6ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      }      else{
        df <-
          demo6 %>% filter(localidade != "Pará", ano == input$demo6ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$demo6ri)
        
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
          c("#B6EDF0", "#74B4E8", "#1F83E0", "#1D44B8", "#090991"),
          domain = x$valor,
          bins = bins
        )
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Índice:</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
            format(
              x$valor,
              big.mark = ".",
              decimal.mark = ",",
              digits = 4
            )
          )
        ) %>% lapply(htmltools::HTML)
      #Mapas com leafleft
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
        ) %>% addLegend(
          pal = pal,
          values = ~ valor,
          opacity = 0.7,
          title = "Índice",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(
            big.mark = ".",
            decimal.mark = ",",
            digits = 2
          )
        )
    })
    
    ##Tabela - Índice de envelhecimento----
    output$demo6txt2 <- renderText({
    t62()  
    })
    output$demo6tab <- renderReactable({
      if (input$demo6ri == "Pará") {
        x <-
          demo6 %>% filter(localidade != "Pará", ano == input$demo6ano) %>%
          select(ri, localidade, ano, valor)
      }      else{
        x <-
          demo6 %>% filter(localidade != "Pará", ano == input$demo6ano) %>%
          select(ri, localidade, ano, valor)
        x <- x %>% filter(ri == input$demo6ri)
      }
      x <-
        x %>% select(ri, localidade, valor)
      x %>% reactable(
        defaultPageSize = 10,
        striped = FALSE,
        highlight = TRUE,
        bordered = TRUE,
        outlined = TRUE,
        resizable = TRUE,
        showSortable = TRUE,
        pagination = F,
        columns =  list(
          ri = colDef(name = "Região de Integração"),
          localidade = colDef(name = "Municípios"),
          valor = colDef(
            name = "Índice",
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
    ##Gráfico - Índice de envelhecimento--------
    output$demo6txt3 <- renderText({
    t63() 
    })
    output$demo6graf <- renderEcharts4r({
      demo6 %>% filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#3C8DBD",
          name = "Índice",
          legend = F,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
          itemStyle = list (barBorderRadius =  5)
        ) %>%
        e_tooltip(
          trigger = "axis",
          formatter =
            e_tooltip_pointer_formatter("decimal", digits = 2, locale = "pt-BR"),
          axisPointer = list(type = 'shadow')
        ) %>%
        e_x_axis(
          axisLabel = list(show = T, fontSize = 11),
          splitLine = list(show = T),
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
              return value.toLocaleString('pt-BR', 
              { minimumFractionDigits: 2, maximumFractionDigits: 2 });
              }
            "
            )
          )
        ) %>%
        e_locale("pt-Br") %>%
        e_datazoom(toolbox = F,  fillerColor = "#E5F5F9") %>%
        e_grid(show = T)
    })
    
    #7-Razão de dependência ----
    ##Mapa - Razão de dependência----
    output$demo7txt1 <- renderText({
    t71()  
    })
    output$demo7map <- renderLeaflet({
      #Tratamento da informação
      ##Filtrando informação
      if (input$demo7ri == "Pará") {
        df <-
          demo7 %>% filter(localidade != "Pará", ano == input$demo7ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      }      else{
        df <-
          demo7 %>% filter(localidade != "Pará", ano == input$demo7ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$demo7ri)
        
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
          c("#B6EDF0", "#74B4E8", "#1F83E0", "#1D44B8", "#090991"),
          domain = x$valor,
          bins = bins
        )
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Razão:</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
            format(
              x$valor,
              big.mark = ".",
              decimal.mark = ",",
              digits = 4
            )
          )
        ) %>% lapply(htmltools::HTML)
      #Mapas com leafleft
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
        ) %>% addLegend(
          pal = pal,
          values = ~ valor,
          opacity = 0.7,
          title = "Razão",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(
            big.mark = ".",
            decimal.mark = ",",
            digits = 2
          )
        )
    })
    ##Tabela - Razão de dependência----
    output$demo7txt2 <- renderText({
    t72()  
    })
    output$demo7tab <- renderReactable({
      if (input$demo7ri == "Pará") {
        x <-
          demo7 %>% filter(localidade != "Pará", ano == input$demo7ano) %>%
          select(ri, localidade, ano, valor)
      }      else{
        x <-
          demo7 %>% filter(localidade != "Pará", ano == input$demo7ano) %>%
          select(ri, localidade, ano, valor)
        x <- x %>% filter(ri == input$demo7ri)
      }
      x <-
        x %>% select(ri, localidade, valor)
      x %>% reactable(
        defaultPageSize = 10,
        striped = FALSE,
        highlight = TRUE,
        bordered = TRUE,
        outlined = TRUE,
        resizable = TRUE,
        showSortable = TRUE,
        pagination = F,
        columns =  list(
          ri = colDef(name = "Região de Integração"),
          localidade = colDef(name = "Municípios"),
          valor = colDef(
            name = "Razão",
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
    ##Gráfico - Razão de dependência----
    output$demo7txt3 <- renderText({
    t73()  
    })
    output$demo7graf <- renderEcharts4r({
      demo7 %>% filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#3C8DBD",
          name = "Razão",
          legend = F,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
          itemStyle = list (barBorderRadius =  5)
        ) %>%
        e_tooltip(
          trigger = "axis",
          formatter =
            e_tooltip_pointer_formatter("decimal", digits = 2, locale = "pt-BR"),
          axisPointer = list(type = 'shadow')
        ) %>%
        e_x_axis(
          axisLabel = list(show = T, fontSize = 11),
          splitLine = list(show = T),
          name = "Ano",
          splitLine = list(show = T),
          nameTextStyle = list(fontWeight = "bold", fontSize = 14)
        ) %>%
        e_y_axis(
          name = "Razão",
          nameTextStyle =
            list(fontWeight = "bold", fontSize = 14),
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
        e_datazoom(toolbox = F,  fillerColor = "#E5F5F9") %>%
        e_grid(show = T)
    })
    #8-Taxa de fecundidade Total ----
    ##Mapa - Taxa de fecundidade Total----
    output$demo8txt1 <- renderText({
      t81()
    })
    output$demo8map <- renderLeaflet({
      #Tratamento da informação
      ##Filtrando informação
      if (input$demo8ri == "Pará") {
        df <-
          demo8 %>% filter(localidade != "Pará", ano == input$demo8ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      }      else{
        df <-
          demo8 %>% filter(localidade != "Pará", ano == input$demo8ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$demo8ri)
        
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
          c("#B6EDF0", "#74B4E8", "#1F83E0", "#1D44B8", "#090991"),
          domain = x$valor,
          bins = bins
        )
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Taxa:</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
            format(
              x$valor,
              big.mark = ".",
              decimal.mark = ",",
              digits = 2
            )
          )
        ) %>% lapply(htmltools::HTML)
      #Mapas com leafleft
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
        ) %>% addLegend(
          pal = pal,
          values = ~ valor,
          opacity = 0.7,
          title = "Taxa",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(
            big.mark = ".",
            decimal.mark = ",",
            digits = 2
          )
        )
    })
    
    ##Tabela - Taxa de fecundidade Total----
    output$demo8txt2 <- renderText({
    t82()  
    })
    output$demo8tab <- renderReactable({
      if (input$demo8ri == "Pará") {
        x <-
          demo8 %>% filter(localidade != "Pará", ano == input$demo8ano) %>%
          select(ri, localidade, ano, valor)
      }      else{
        x <-
          demo8 %>% filter(localidade != "Pará", ano == input$demo8ano) %>%
          select(ri, localidade, ano, valor)
        x <- x %>% filter(ri == input$demo8ri)
      }
      x <-
        x %>% select(ri, localidade, valor)
      x %>% reactable(
        defaultPageSize = 10,
        striped = FALSE,
        highlight = TRUE,
        bordered = TRUE,
        outlined = TRUE,
        resizable = TRUE,
        showSortable = TRUE,
        pagination = F,
        columns =  list(
          ri = colDef(name = "Região de Integração"),
          localidade = colDef(name = "Municípios"),
          valor = colDef(
            name = "Taxa",
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
    ##Gráfico - Taxa de fecundidade Total--------
    output$demo8txt3 <- renderText({
    t83()  
    })
    output$demo8graf <- renderEcharts4r({
      demo8 %>% filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#3C8DBD",
          name = "Taxa",
          legend = F,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
          itemStyle = list (barBorderRadius =  5)
        ) %>%
        e_tooltip(
          trigger = "axis",
          formatter =
            e_tooltip_pointer_formatter("decimal", digits = 2, locale = "pt-BR"),
          axisPointer = list(type = 'shadow')
        ) %>%
        e_x_axis(
          axisLabel = list(show = T, fontSize = 11),
          splitLine = list(show = T),
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
              { minimumFractionDigits: 2, maximumFractionDigits: 2 });
              }
            "
            )
          )
        ) %>%
        e_locale("pt-Br") %>%
        e_datazoom(toolbox = F,  fillerColor = "#E5F5F9") %>%
        e_grid(show = T)
    })
    
    #9-Taxa específica de fecundidade por faixa etária----
    ##Mapa - Taxa específica de fecundidade por faixa etária----
    output$demo9txt1 <- renderText({
      t91()
    })
    output$demo9map <- renderLeaflet({
      #Tratamento da informação
      ##Filtrando informação
      if (input$demo9ri == "Pará") {
        df <-
          demo9 %>% filter(localidade != "Pará",
                           ano == input$demo9ano,
                           categoria == input$demo9cat) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
      }      else{
        df <-
          demo9 %>% filter(localidade != "Pará",
                           ano == input$demo9ano,
                           categoria == input$demo9cat) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$demo9ri)
        
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
          c("#B6EDF0", "#74B4E8", "#1F83E0", "#1D44B8", "#090991"),
          domain = x$valor,
          bins = bins
        )
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Taxa:</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
            format(
              x$valor,
              big.mark = ".",
              decimal.mark = ",",
              digits = 2
            )
          )
        ) %>% lapply(htmltools::HTML)
      #Mapas com leafleft
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
        ) %>% addLegend(
          pal = pal,
          values = ~ valor,
          opacity = 0.7,
          title = "Taxa",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(
            big.mark = ".",
            decimal.mark = ",",
            digits = 3
          )
        )
    })
    ##Tabela - Taxa específica de fecundidade por faixa etária----
    output$demo9txt2 <- renderText({
      t92()
    })
    output$demo9tab <- renderReactable({
      if (input$demo9ri == "Pará") {
        x <-
          demo9 %>% filter(localidade != "Pará", ano == input$demo9ano) %>%
          select(ri,
                 localidade,
                 categoria,
                 valor) %>%
          pivot_wider(names_from = categoria, values_from = valor)
      }      else{
        x <-
          demo9 %>% filter(localidade != "Pará", ano == input$demo9ano) %>%
          select(ri,
                 localidade,
                 categoria,
                 valor) %>%
          pivot_wider(names_from = categoria, values_from = valor)
        x <- x %>% filter(ri == input$demo9ri)
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
        columns =  list(
          ri = colDef(name = "Região de Integração"),
          localidade = colDef(name = "Municípios", sticky = "left")
        ),
        defaultColDef =
          colDef(
            footerStyle = list(fontWeight = "bold"),
            headerStyle = list(background = "#f7f7f8"),
            format = colFormat(separators = T, digits = 3)
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
    ##Gráfico - Taxa específica de fecundidade por faixa etária----
    output$demo9txt3 <- renderText({
      t93()
    })
    output$demo9graf1 <- renderEcharts4r({
      demo9 %>% filter(localidade == "Pará", ano == input$demo9ano) %>%
        e_chart(x = categoria) %>%
        e_bar(
          serie = valor,
          color = "#3C8DBD",
          name = "Taxa",
          legend = F,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
          barWidth = "50%",
          itemStyle = list (barBorderRadius =  5)
        ) %>%
        e_tooltip(
          trigger = "axis",
          formatter =
            e_tooltip_pointer_formatter("decimal", digits = 2, locale = "pt-BR"),
          axisPointer = list(type = 'shadow')
        ) %>%
        e_x_axis(
          axisLabel = list(show = T, fontSize = 11),
          splitLine = list(show = T),
          name = "Faixa Etária",
          nameLocation = "middle",
          nameTextStyle = list(
            fontWeight = "bold",
            padding = c(20, 0, 0, 0),
            fontSize = 14
          )
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
              { minimumFractionDigits: 2, maximumFractionDigits: 2 });
              }
            "
            )
          )
        ) %>%
        e_locale("pt-Br") %>%
        e_grid(show = T)
    })
    
    #DOWNLOAD----
    ##Demo1_1 - Tabela - População Total e Estimativas poplulacionais----
    #Filtra os dados
    d1_1 <- reactive({
      if (input$demo1ri == "Pará") {
        x <-
          demo1 %>% filter(localidade != "Pará", ano == input$demo1ano) %>%  
          mutate(Percentual = (valor / sum(valor)) * 100)
      } else{
        x <-
          demo1 %>% filter(localidade != "Pará", ano == input$demo1ano) %>%
          filter(ri == input$demo1ri) %>%
          mutate(Percentual = (valor / sum(valor)) * 100)
      }
    })
    #Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(d1_1(),{
      t12()
    downset_Server("demo1_1", d1_1(), t12())  
    })
    
        
    ##Demo1_2 - Gráfico - População Total e Estimativas poplulacionais----
    #Filtra os dados
    d1_2 <- reactive({
      demo1 %>% filter(localidade == "Pará")
    })
    #Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(d1_2(),{
      t13()
    downset_Server("demo1_2", d1_2(), t13())  
    })
    
        
    ##Demo2_1 - Tabela - População por Faixa Etária----
    d2_1 <- reactive({
      if (input$demo2ri == "Pará") {
        x <-
          demo2 %>% filter(localidade != "Pará", ano == input$demo2ano) %>%
          pivot_wider(names_from = categoria,values_from = valor)
      }      else{
        x <-
          demo2 %>% filter(localidade != "Pará", ano == input$demo2ano) %>% 
          pivot_wider(names_from = categoria,values_from = valor) %>%
          filter(ri == input$demo2ri)
      }
      
    })
    #Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(d2_1(),{
      t22()
    downset_Server("demo2_1", d2_1(), t22())  
    })
    
        
    ##Demo2_2 - Gráfico - População por Faixa Etária----
    #Filtra os dados
    d2_2 <- reactive({
      demo2 %>% filter(localidade == "Pará", ano == input$demo2ano)
    })
    #Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(d2_2(),{
      t23()
    downset_Server("demo2_2", d2_2(), t23())  
    })
    
       
    ##Demo3_1 - Tabela - População por sexo----
    #Filtra os dados
    d3_1 <- reactive({
      if (input$demo3ri == "Pará") {
        x <- demo3 %>% 
          filter(localidade != "Pará",ano == input$demo3ano) %>%
          pivot_wider(names_from = categoria, values_from = valor) %>%
          mutate(Masc_percentual = (Masculino / (Masculino + Feminino)) *
                   100) %>%
          mutate(Fem_percentual = (Feminino / (Masculino + Feminino)) *
                   100)
      }      else{
        x <-
          demo3 %>% 
          filter(localidade != "Pará",ano == input$demo3ano) %>%
          pivot_wider(names_from = categoria, values_from = valor) %>%
          mutate(Masc_percentual = (Masculino / (Masculino + Feminino)) *
                   100) %>%
          mutate(Fem_percentual = (Feminino / (Masculino + Feminino)) *
                   100) %>%
          filter(ri == input$demo3ri)
      }
    })
    #Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(d3_1(),{
      t32()
    downset_Server("demo3_1", d3_1(), t32())  
    })
    
        
    ##Demo3_2 - Gráfico de linha - População por sexo----
    #Filtra os dados
    d3_2 <- reactive({
      demo3 %>% filter(localidade == "Pará") %>%
        pivot_wider(names_from = categoria, values_from = valor)
    })
    #Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(d3_2(),{
      t33()
      downset_Server("demo3_2", d3_2(), t33())
    })
    
        
    ##Demo4_1 - Tabela - Razão de sexo-----
    #Filtra os dados
    d4_1 <- reactive({
      if (input$demo4ri == "Pará") {
        x <-
          demo4 %>% filter(localidade != "Pará", ano == input$demo4ano)
      }else{
        x <-
          demo4 %>% filter(localidade != "Pará",ano == input$demo4ano) %>%
          filter(ri == input$demo4ri)
      }
    })
    #Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(d4_1(),{
      t42()
    downset_Server("demo4_1", d4_1(), t42())  
    })
    
        
    ##Demo4_2 - Gráfico - Razão de sexo----
    #Filtra os dados
    d4_2 <-
      reactive({
        demo4 %>% filter(localidade == "Pará") 
      })
    #Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(d4_2(),{
      t43()
    downset_Server("demo4_2", d4_2(), t43())  
    })
    
    ##Demo5_1 - Tabela - Proporção de idosos----
    #Filtra os dados
    d5_1 <- reactive({
      if (input$demo5ri == "Pará") {
        x <-
          demo5 %>% filter(localidade != "Pará", ano == input$demo5ano)
      }      else{
        x <-
          demo5 %>% filter(localidade != "Pará", ano == input$demo5ano) %>%
          filter(ri == input$demo5ri)
          }
    })
    #Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(d5_1(),{
      t52()
    downset_Server("demo5_1", d5_1(), t52())  
    })
        
    ##Demo5_2 - Gráfico - Proporção de idosos----
    #Filtra os dados
    d5_2 <-
      reactive({
        demo5 %>% filter(localidade == "Pará")
      })
    #Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(d5_2(),{
      t53()
    downset_Server("demo5_2", d5_2(), t53())  
    })
    
        
    ##Demo6_1 - Tabela - Índice de envelhecimento----
    #Filtra os dados
    d6_1 <- reactive({
      if (input$demo6ri == "Pará") {
        x <-
          demo6 %>% filter(localidade != "Pará", ano == input$demo6ano)
      }else{
        x <-
          demo6 %>% filter(localidade != "Pará", ano == input$demo6ano) %>%
          filter(ri == input$demo6ri)
      }
    })
    #Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(d6_1(),{
      t62()
    downset_Server("demo6_1", d6_1(), t62())  
    })
    
        
    ##Demo6_2 - Gráfico - Índice de envelhecimento----
    #Filtra os dados
    d6_2 <- reactive({
      demo6 %>% filter(localidade == "Pará") 
    })
    #Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(d6_2(),{
      t63()
     downset_Server("demo6_2", d6_2(), t63()) 
    })
    
    ##Demo7_1 - Tabela - Razão de dependência----
    #Filtra os dados
    d7_1 <- reactive({
      if (input$demo7ri == "Pará") {
        x <-
          demo7 %>% filter(localidade != "Pará", 
                           ano == input$demo7ano)
      }      else{
        x <-
          demo7 %>% filter(localidade != "Pará", 
                           ano == input$demo7ano,
                           ri == input$demo7ri)

      }
    })
    #Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(d7_1(),{
      t72()
      downset_Server("demo7_1", d7_1(), t72())
    })
    
        
    ##Demo7_2 - Gráfico - Razão de dependência----
    #Filtra os dados
    d7_2 <- reactive({
      demo7 %>% filter(localidade == "Pará")
    })
    #Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(d7_2(),{
      t73()
    downset_Server("demo7_2", d7_2(), t73())  
    })
    
        
    ##Demo8_1 - Tabela - Taxa de fecundidade Total----
    #Filtra os dados
    d8_1 <- reactive({
      if (input$demo8ri == "Pará") {
        x <-
          demo8 %>% 
          filter(localidade != "Pará",ano == input$demo8ano) 
      }      else{
        x <-
          demo8 %>% 
          filter(localidade != "Pará",ano == input$demo8ano,
                 ri == input$demo8ri) 
      }
    })
    #Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(d8_1(),{
      t82()
    downset_Server("demo8_1", d8_1(), t82())  
    })
    
        
    ##Demo8_2 - Gráfico - Taxa de fecundidade Total----
    #Filtra os dados
    d8_2 <- reactive({
      demo8 %>% filter(localidade == "Pará")
    })
    #Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(d8_2(),{
      t83()
     downset_Server("demo8_2", d8_2(), t83()) 
    })
    
        
    ##Demo9_1 - Tabela - Taxa específica de fecundidade por faixa etária----
    #Filtra os dados
    d9_1 <- reactive({
      if (input$demo9ri == "Pará") {
        x <-
          demo9 %>% filter(localidade != "Pará", ano == input$demo9ano) %>%
          pivot_wider(names_from = categoria, values_from = valor)
      }      else{
        x <-
          demo9 %>% filter(localidade != "Pará", ano == input$demo9ano) %>%
          pivot_wider(names_from = categoria, values_from = valor) %>% 
          filter(ri == input$demo9ri)
      }
    })
    #Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(d9_1(),{
      t92()
     downset_Server("demo9_1", d9_1(), t92()) 
    })
    
        
    ##Demo9_2 - Gráfico - Taxa específica de fecundidade por faixa etária----
    #Filtra os dados
    d9_2 <- reactive({
      demo9 %>% filter(localidade == "Pará", ano == input$demo9ano)
    })
    #Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(d9_2(),{
      t93()
     downset_Server("demo9_2", d9_2(), t93()) 
    })
    
  })
}

#Play do Módulo
ui <- dashboardPage(header = dashboardHeader(),
                    sidebar = dashboardSidebar(),
                    body = dashboardBody(fluidPage(demografia_pa_ui("demografia_pa"))))

server <- function(input, output) {
  demografia_pa_Server("demografia_pa")
}
shinyApp(ui, server)
