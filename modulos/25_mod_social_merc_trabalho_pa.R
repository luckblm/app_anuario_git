# Funções de módulo de Social - Mercado de Trabalho - Estadual
# Função de UI
social_merc_tab_pa_ui <- function(id) {
  fluidPage(
    # CAMINHO DO ARQUIVO CSS----
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "estilo.css")
    ),
    div(
      class = "navbar_social",
      navbarPage(
        tags$b("Mercado de Trabalho - Pará"),
        navbarMenu(
          tags$b("Escolha um Indicador"),
          # 1- Vínculos Empregatícios Total no Emprego Formal----
          tabPanel(
            "Vínculos Empregatícios Total no Emprego Formal",
            panel(
              ## Controle----
              heading =
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Vínculos Empregatícios Total no Emprego Formal"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "merc1ano"),
                  label = "Ano",
                  choices = sort(unique(merc1[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "merc1ri"),
                  label = "Região de Integração",
                  choices = unique(merc1[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Vínculos Empregatícios Total no Emprego Formal----
              box(
                title = textOutput(NS(id, "merc1txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "merc1map"), height = "600px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  tags$h6(tags$b("Fonte:"), "MTP-RAIS"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                )
              ),
              ## Tabela - Vínculos Empregatícios Total no Emprego Formal----
              box(
                title = textOutput(NS(id, "merc1txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "merc1tab"), height = "400px"),
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
              ),
              ## Gráfico - Vínculos Empregatícios Total no Emprego Formal----
              box(
                title = textOutput(NS(id, "merc1txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
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
                  downset_ui(NS(id, "merc1_3"))
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
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "merc2ano"),
                  label = "Ano",
                  choices = sort(unique(merc2[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "merc2ri"),
                  label = "Região de Integração",
                  choices = unique(merc2[["ri"]]),
                  width = "200px"
                )
              ),
              tags$div(
                class = "seletor3", style = "width: 382px;",
                pickerInput(
                  inputId = NS(id, "merc2cat"),
                  label = "Sexo",
                  choices = merc2 %>% filter(categoria != "Total") %>% pull(categoria) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Vínculos Empregatícios no Emprego Formal por Sexo----
              box(
                title = textOutput(NS(id, "merc2txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "merc2map"), height = "600px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  tags$h6(tags$b("Fonte:"), "MTP-RAIS"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
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
              )
            ),
            fluidRow(
              ## Gráfico - Vínculos Empregatícios no Emprego Formal por Sexo----
              box(
                title = textOutput(NS(id, "merc2txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "merc2graf")),
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
                pickerInput(
                  inputId = NS(id, "merc3ano"),
                  label = "Ano",
                  choices = sort(unique(merc3[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "merc3ri"),
                  label = "Região de Integração",
                  choices = unique(merc3[["ri"]]),
                  width = "200px"
                )
              ),
              tags$div(
                class = "seletor3", style = "width: 382px;",
                pickerInput(
                  inputId = NS(id, "merc3cat"),
                  label = "Grande Setor",
                  choices = merc3 %>% pull(categoria) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Vínculos Empregatícios no Emprego Formal por Grande Setor (IBGE)----
              box(
                title = textOutput(NS(id, "merc3txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "merc3map"), height = "600px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  tags$h6(tags$b("Fonte:"), "MTP-RAIS"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
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
              )
            ),
            fluidRow(
              ## Gráfico - Vínculos Empregatícios no Emprego Formal por Grande Setor (IBGE)----
              box(
                title = textOutput(NS(id, "merc3txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "merc3graf")),
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
                pickerInput(
                  inputId = NS(id, "merc4ano"),
                  label = "Ano",
                  choices = sort(unique(merc4[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "merc4ri"),
                  label = "Região de Integração",
                  choices = unique(merc4[["ri"]]),
                  width = "200px"
                )
              ),
              tags$div(
                class = "seletor3",
                pickerInput(
                  inputId = NS(id, "merc4cat"),
                  label = "Setor Econômico",
                  choices = merc4 %>% pull(categoria) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Vínculos Empregatícios no Emprego Formal por Setor Econômico----
              box(
                title = textOutput(NS(id, "merc4txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "merc4map"), height = "600px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  tags$h6(tags$b("Fonte:"), "MTP-RAIS"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
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
              )
            ),
            fluidRow(
              ## Gráfico - Vínculos Empregatícios no Emprego Formal por Setor Econômico----
              box(
                title = textOutput(NS(id, "merc4txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "merc4graf")),
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
                pickerInput(
                  inputId = NS(id, "merc5ano"),
                  label = "Ano",
                  choices = sort(unique(merc5[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "merc5ri"),
                  label = "Região de Integração",
                  choices = unique(merc5[["ri"]]),
                  width = "200px"
                )
              ),
              tags$div(
                class = "seletor3",
                pickerInput(
                  inputId = NS(id, "merc5cat"),
                  label = "Escolaridade/Total",
                  choices = unique(merc5[["categoria"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Vínculos Empregatícios por Escolaridade do Trabalhador Formal----
              box(
                title = textOutput(NS(id, "merc5txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "merc5map"), height = "600px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  tags$h6(tags$b("Fonte:"), "MTP-RAIS"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
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
              )
            ),
            fluidRow(
              ## Gráfico - Vínculos Empregatícios por Escolaridade do Trabalhador Formal----
              box(
                title = textOutput(NS(id, "merc5txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "merc5graf")),
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
                  inputId = NS(id, "merc6ano"),
                  label = "Ano",
                  choices = sort(unique(merc6[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "merc6ri"),
                  label = "Região de Integração",
                  choices = unique(merc6[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Remuneração Média (R$) do Trabalhador Formal----
              box(
                title = textOutput(NS(id, "merc6txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "merc6map"), height = "600px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  tags$h6(tags$b("Fonte:"), "MTP-RAIS"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                )
              ),
              ## Tabela - Remuneração Média (R$) do Trabalhador Formal----
              box(
                title = textOutput(NS(id, "merc6txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "merc6tab"), height = "400px"),
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
            ),
            fluidRow(
              ## Gráfico - Remuneração Média (R$) do Trabalhador Formal----
              box(
                title = textOutput(NS(id, "merc6txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
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
                  downset_ui(NS(id, "merc6_3"))
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
                pickerInput(
                  inputId = NS(id, "merc7ano"),
                  label = "Ano",
                  choices = sort(unique(merc7[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "merc7ri"),
                  label = "Região de Integração",
                  choices = unique(merc7[["ri"]]),
                  width = "200px"
                )
              ),
              tags$div(
                class = "seletor3",
                pickerInput(
                  inputId = NS(id, "merc7cat"),
                  label = "Sexo",
                  choices = unique(merc7[["categoria"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Remuneração Média (R$) do Trabalhador Formal por Sexo----
              box(
                title = textOutput(NS(id, "merc7txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "merc7map"), height = "600px"),
                  type = 8,
                  color = "#f17701",
                  size = 0.5
                ),
                footer = list(
                  tags$h6(tags$b("Fonte:"), "MTP-RAIS"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
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
              )
            ),
            fluidRow(
              ## Gráfico - Remuneração Média (R$) do Trabalhador Formal por Sexo----
              box(
                title = textOutput(NS(id, "merc7txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "merc7graf")),
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
social_merc_tab_pa_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    #TÍTULOS----
    # 1- Vínculos Empregatícios Total no Emprego Formal----
    ## Mapa - Vínculos Empregatícios Total no Emprego Formal----
    t11 <- reactive({
      if (input$merc1ri == "Pará") {
        paste0(
          "Vínculos Empregatícios Total no Emprego Formal, Pará - ",
          input$merc1ano
        )
      } else{
        paste0(
          "Vínculos Empregatícios Total no Emprego Formal, Região de Integração ",
          input$merc1ri,
          " - ",
          input$merc1ano
        )
      }
    })

    ## Tabela - Vínculos Empregatícios Total no Emprego Formal----
    t12 <- reactive({
      if (input$merc1ri == "Pará") {
        paste0(
          "Vínculos Empregatícios Total no Emprego Formal por Município, Pará - ",
          input$merc1ano
        )
      } else{
        paste0(
          "Vínculos Empregatícios Total no Emprego Formal por Município, Região de Integração ",
          input$merc1ri,
          " - ",
          input$merc1ano
        )
      }
    })
    
    ## Gráfico - Vínculos Empregatícios Total no Emprego Formal----
    t13 <- reactive({
      paste0("Vínculos Empregatícios Total no Emprego Formal, Pará - ",
             min(merc1$ano), " a ", max(merc1$ano))
    })

    # 2 - Vínculos Empregatícios no Emprego Formal por Sexo----
    ## Mapas - Vínculos Empregatícios no Emprego Formal por Sexo----
    t21 <- reactive({
        if (input$merc2ri == "Pará") {
          paste0(
            "Vínculos Empregatícios no Emprego Formal - ",
            input$merc2cat,
            ", Pará - ",
            input$merc2ano
          )
        } else{
          paste0(
            "Vínculos Empregatícios no Emprego Formal - ",
            input$merc2cat,
            ", Região de Integração ",
            input$merc2ri,
            " - ",
            input$merc2ano
          )
        }
    })
    
    ## Tabela - Vínculos Empregatícios no Emprego Formal por Sexo----
    t22 <- reactive({
      if (input$merc2ri == "Pará") {
        paste0(
          "Vínculos Empregatícios no Emprego Formal por Sexo e Município, Pará - ",
          input$merc2ano
        )
      } else{
        paste0(
          "Vínculos Empregatícios no Emprego Formal por Sexo e Município, Região de Integração ",
          input$merc2ri,
          " - ",
          input$merc2ano
        )
      }
    })
    
    ## Gráfico - Vínculos Empregatícios no Emprego Formal por Sexo----
    t23 <- reactive({
      paste0("Vínculos Empregatícios no Emprego Formal por Sexo, Pará - ", min(merc2$ano), " a ", max(merc2$ano))
    })

    # 3 - Vínculos Empregatícios no Emprego Formal por Grande Setor (IBGE)----
    ## Mapa - Vínculos Empregatícios no Emprego Formal por Grande Setor (IBGE)----
    t31 <- reactive({
        if (input$merc3ri == "Pará") {
          paste0(
            "Vínculos Empregatícios no Emprego Formal por Grande Setor (IBGE) - ",
            input$merc3cat,
            ", Pará - ",
            input$merc3ano
          )
        } else{
          paste0(
            "Vínculos Empregatícios no Emprego Formal por Grande Setor (IBGE) - ",
            input$merc3cat,
            ", Região de Integração ",
            input$merc3ri,
            " - ",
            input$merc3ano
          )
        }
    })
    
    ## Tabela - Vínculos Empregatícios no Emprego Formal por Grande Setor (IBGE)----
    t32 <- reactive({
      if (input$merc3ri == "Pará") {
        paste0(
          "Vínculos Empregatícios no Emprego Formal por Grande Setor (IBGE) e Município, Pará - ",
          input$merc3ano
        )
      } else{
        paste0(
          "Vínculos Empregatícios no Emprego Formal por Grande Setor (IBGE) e Município, Região de Integração ",
          input$merc3ri,
          " - ",
          input$merc3ano
        )
      }
    })
    
    ## Gráfico - Vínculos Empregatícios no Emprego Formal por Grande Setor (IBGE)----
    t33 <- reactive({
      paste0("Vínculos Empregatícios no Emprego Formal por Grande Setor (IBGE), Pará - ", min(merc3$ano), " a ", max(merc3$ano))
    })

    # 4 - Vínculos Empregatícios no Emprego Formal por Setor Econômico----
    ## Mapa - Vínculos Empregatícios no Emprego Formal por Setor Econômico----
    t41 <- reactive({
        if (input$merc4ri == "Pará") {
          paste0(
            "Vínculos Empregatícios no Emprego Formal por Setor Econômico - ",
            input$merc4cat,
            ", Pará - ",
            input$merc4ano
          )
        } else{
          paste0(
            "Vínculos Empregatícios no Emprego Formal por Setor Econômico - ",
            input$merc4cat,
            ", Região de Integração ",
            input$merc4ri,
            " - ",
            input$merc4ano
          )
        }
    })
    
    ## Tabela - Vínculos Empregatícios no Emprego Formal por Setor Econômico----
    t42 <- reactive({
      if (input$merc4ri == "Pará") {
        paste0(
          "Vínculos Empregatícios no Emprego Formal por Setor Econômico e Município, Pará - ",
          input$merc4ano
        )
      } else{
        paste0(
          "Vínculos Empregatícios no Emprego Formal por Setor Econômico e Município, Região de Integração ",
          input$merc4ri,
          " - ",
          input$merc4ano
        )
      }
    })
    
    ## Gráfico - Vínculos Empregatícios no Emprego Formal por Setor Econômico----
    t43 <- reactive({
      paste0("Vínculos Empregatícios no Emprego Formal por Setor Econômico, Pará - ", min(merc4$ano), " a ", max(merc4$ano))
    })

    
    # 5 - Vínculos Empregatícios por Escolaridade do Trabalhador Formal----
    ## Mapa - Vínculos Empregatícios por Escolaridade do Trabalhador Formal----
    t51 <- reactive({
        if (input$merc5ri == "Pará") {
          paste0(
            "Vínculos Empregatícios por Escolaridade do Trabalhador Formal - ",
            input$merc5cat,
            ", Pará - ",
            input$merc5ano
          )
        } else{
          paste0(
            "Vínculos Empregatícios por Escolaridade do Trabalhador Formal - ",
            input$merc5cat,
            ", Região de Integração ",
            input$merc5ri,
            " - ",
            input$merc5ano
          )
        }
    })
    
    ## Tabela - Vínculos Empregatícios por Escolaridade do Trabalhador Formal----
    t52 <- reactive({
      if (input$merc5ri == "Pará") {
        paste0(
          "Vínculos Empregatícios por Escolaridade do Trabalhador Formal por Município, Pará - ",
          input$merc5ano
        )
      } else{
        paste0(
          "Vínculos Empregatícios por Escolaridade do Trabalhador Formal por Município, Região de Integração  ",
          input$merc5ri,
          " - ",
          input$merc5ano
        )
      }
    })
    
    ## Gráfico - Vínculos Empregatícios por Escolaridade do Trabalhador Formal----
    t53 <- reactive({
      paste0("Vínculos Empregatícios por Escolaridade do Trabalhador Formal, Pará - ", min(merc5$ano), " a ", max(merc5$ano))
    })
    
    # 6 - Remuneração Média (R$) do Trabalhador Formal----
    ## Mapa - Remuneração Média (R$) do Trabalhador Formal----
    t61 <- reactive({
      if (input$merc6ri == "Pará") {
        paste0(
          "Remuneração Média (R$) do Trabalhador Formal, Pará - ",
          input$merc6ano
        )
      } else{
        paste0(
          "Remuneração Média (R$) do Trabalhador Formal, Região de Integração ",
          input$merc6ri,
          " - ",
          input$merc6ano
        )
      }
    })
    
    ## Tabela - Remuneração Média (R$) do Trabalhador Formal----
    t62 <- reactive({
      if (input$merc6ri == "Pará") {
        paste0(
          "Remuneração Média (R$) do Trabalhador Formal por Município, Pará - ",
          input$merc6ano
        )
      } else{
        paste0(
          "Remuneração Média (R$) do Trabalhador Formal por Município, Região de Integração ",
          input$merc6ri,
          " - ",
          input$merc6ano
        )
      }
    })
    
    ## Gráfico - Remuneração Média (R$) do Trabalhador Formal----
    t63 <- reactive({
      paste0("Remuneração Média (R$) do Trabalhador Formal, Pará - ", min(merc6$ano), " a ", max(merc6$ano))
    })

    # 7 - Remuneração Média (R$) do Trabalhador Formal por Sexo----
    ## Mapa - Remuneração Média (R$) do Trabalhador Formal por Sexo----
    t71 <- reactive({
        if (input$merc7ri == "Pará") {
          paste0(
            "Remuneração Média (R$) do Trabalhador Formal por Sexo - ",
            input$merc7cat,
            ", Pará - ",
            input$merc7ano
          )
        } else{
          paste0(
            "Remuneração Média (R$) do Trabalhador Formal por Sexo - ",
            input$merc7cat,
            ", Região de Integração ",
            input$merc7ri,
            " - ",
            input$merc7ano
          )
        }
    })
    
    ## Tabela - Remuneração Média (R$) do Trabalhador Formal por Sexo----
    t72 <- reactive({
      if (input$merc7ri == "Pará") {
        paste0(
          "Remuneração Média (R$) do Trabalhador Formal por Sexo e Município, Pará -  ",
          input$merc7ano
        )
      } else{
        paste0(
          "Remuneração Média (R$) do Trabalhador Formal por Sexo e Município, Região de Integração  ",
          input$merc7ri,
          " - ",
          input$merc7ano
        )
      }
    })
    
    ## Gráfico - Remuneração Média (R$) do Trabalhador Formal por Sexo----
    t73 <- reactive({
      paste0("Remuneração Média (R$) do Trabalhador Formal por Sexo, Pará - ", min(merc7$ano), " a ", max(merc7$ano))
    })
    
    #VISUALIZAÇÃO----
    # 1- Vínculos Empregatícios Total no Emprego Formal----
    ## Mapa - Vínculos Empregatícios Total no Emprego Formal----
    output$merc1txt1 <- renderText({
      t11()
    })
    output$merc1map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$merc1ri == "Pará") {
        df <- merc1 %>%
          filter(localidade != "Pará", ano == input$merc1ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x$valor[x$valor == 0] <- NA
      } else {
        df <- merc1 %>%
          filter(localidade != "Pará", ano == input$merc1ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$merc1ri)
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
          "<strong>%s</strong><br/> <b>Total:</b> %s",
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
          title = "Total",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 2)
        )
    })

    ## Tabela - Vínculos Empregatícios Total no Emprego Formal----
    output$merc1txt2 <- renderText({
      t12()
    })

    output$merc1tab <- renderReactable({
      if (input$merc1ri == "Pará") {
        x <- merc1 %>%
          filter(localidade != "Pará", ano == input$merc1ano) %>%
          select(ri, localidade, valor) %>%
          mutate(Percentual = (valor / sum(valor)) * 100)
      } else {
        x <- merc1 %>%
          filter(localidade != "Pará", ano == input$merc1ano) %>%
          select(ri, localidade, valor)
        x <- x %>%
          filter(ri == input$merc1ri) %>%
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
        columns = list(
          ri = colDef(name = "Região de integração"),
          localidade = colDef(name = "Municípios"),
          valor = colDef(name = "Total", format = colFormat(separators = T, locales = "pt-BR")),
          Percentual = colDef(name = "Precentual", format = colFormat(digits = 2, separators = T, locales = "pt-BR"))
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
    ## Gráfico - Vínculos Empregatícios Total no Emprego Formal----
    output$merc1txt3 <- renderText({
      t13()
    })
    output$merc1graf <- renderEcharts4r({
      merc1 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#f17701",
          name = "Total",
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

    # 2 - Vínculos Empregatícios no Emprego Formal por Sexo----
    ## Mapas - Vínculos Empregatícios no Emprego Formal por Sexo----
    output$merc2txt1 <- renderText({
      t21()
    })

    output$merc2map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$merc2ri == "Pará") {
        df <- merc2 %>%
          filter(localidade != "Pará", ano == input$merc2ano, categoria == input$merc2cat) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x$valor[x$valor == 0] <- NA
      } else {
        df <- merc2 %>%
          filter(localidade != "Pará", ano == input$merc2ano, categoria == input$merc2cat) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$merc2ri)
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
          "<strong>%s</strong><br/> <b>%s:</b> %s",
          x$name_muni,
          input$merc2cat,
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
          title = input$merc2cat,
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 2)
        )
    })
    ## Tabela - Vínculos Empregatícios no Emprego Formal por Sexo----
    output$merc2txt2 <- renderText({
      t22()
    })

    output$merc2tab <- renderReactable({
      if (input$merc2ri == "Pará") {
        x <- merc2 %>%
          filter(localidade != "Pará", ano == input$merc2ano) %>%
          select(ri, localidade, categoria, valor) %>%
          pivot_wider(names_from = categoria, values_from = valor) %>%
          mutate(Masc_percentual = Masculino / (Masculino + Feminino)) %>%
          mutate(Fem_percentual = Feminino / (Masculino + Feminino))
      } else {
        x <- merc2 %>%
          filter(localidade != "Pará", ano == input$merc2ano) %>%
          select(ri, localidade, categoria, valor) %>%
          pivot_wider(names_from = categoria, values_from = valor)
        x <- x %>%
          filter(ri == input$merc2ri) %>%
          mutate(Masc_percentual = Masculino / (Masculino + Feminino)) %>%
          mutate(Fem_percentual = Feminino / (Masculino + Feminino))
      }
      x %>%
        select(ri,
               localidade,
               Masculino,
               Masc_percentual,
               Feminino,
               Fem_percentual) %>% 
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
            ri = colDef(name = "Região de integração"),
            localidade = colDef(name = "Municípios"),
            valor = colDef(name = "Total", format = colFormat(separators = T, locales = "pt-BR")),
            Masc_percentual = colDef(name = "Masculino(%)", format = colFormat(percent = TRUE, digits = 1)),
            Fem_percentual = colDef(name = "Feminino(%)", format = colFormat(percent = TRUE, digits = 1))
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
    ## Gráfico - Vínculos Empregatícios no Emprego Formal por Sexo----
    output$merc2txt3 <- renderText({
      t23()
    })
    output$merc2graf <- renderEcharts4r({
      merc2 %>%
        filter(localidade == "Pará") %>%
        pivot_wider(names_from = categoria, values_from = valor) %>%
        e_chart(x = ano) %>%
        e_line(
          serie = Masculino,
          name = "Masculino",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T, itemStyle = list(barBorderRadius = 5)
        ) %>%
        e_line(
          serie = Feminino,
          name = "Feminino",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T, itemStyle = list(barBorderRadius = 5)
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

    # 3 - Vínculos Empregatícios no Emprego Formal por Grande Setor (IBGE)----
    ## Mapa - Vínculos Empregatícios no Emprego Formal por Grande Setor (IBGE)----
    output$merc3txt1 <- renderText({
      t31()
    })

    output$merc3map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$merc3ri == "Pará") {
        df <- merc3 %>%
          filter(localidade != "Pará", ano == input$merc3ano, categoria == input$merc3cat) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x$valor[x$valor == 0] <- NA
      } else {
        df <- merc3 %>%
          filter(localidade != "Pará", ano == input$merc3ano, categoria == input$merc3cat) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$merc3ri)
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
          "<strong>%s</strong><br/> <b>%s:</b> %s",
          x$name_muni,
          input$merc3cat,
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
          title = "Quantidade",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 2)
        )
    })
    ## Tabela - Vínculos Empregatícios no Emprego Formal por Grande Setor (IBGE)----
    output$merc3txt2 <- renderText({
      t32()
    })

    output$merc3tab <- renderReactable({
      if (input$merc3ri == "Pará") {
        x <- merc3 %>%
          filter(localidade != "Pará", ano == input$merc3ano) %>%
          select(ri, localidade, categoria, valor) %>%
          pivot_wider(names_from = categoria, values_from = valor)
      } else {
        x <- merc3 %>%
          filter(localidade != "Pará", ano == input$merc3ano) %>%
          select(ri, localidade, categoria, valor) %>%
          pivot_wider(names_from = categoria, values_from = valor)
        x <- x %>% filter(ri == input$merc3ri)
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
          ri = colDef(name = "Região de integração"),
          localidade = colDef(sticky = "left", name = "Municípios"),
          `Construção Civil` = colDef(),
          Agropecuária = colDef()
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
    ## Gráfico - Vínculos Empregatícios no Emprego Formal por Grande Setor (IBGE)----
    output$merc3txt3 <- renderText({
      t33()
    })
    output$merc3graf <- renderEcharts4r({
      merc3 %>%
        filter(localidade == "Pará") %>%
        pivot_wider(names_from = categoria, values_from = valor) %>%
        e_chart(x = ano) %>%
        e_line(
          serie = Indústria,
          name = "Indústria",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T, itemStyle = list(barBorderRadius = 5)
        ) %>%
        e_line(
          serie = `Construção Civil`,
          name = "Construção Civil",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T, itemStyle = list(barBorderRadius = 5)
        ) %>%
        e_line(
          serie = Comércio,
          name = "Comércio",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T, itemStyle = list(barBorderRadius = 5)
        ) %>%
        e_line(
          serie = Serviços,
          name = "Serviços",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T, itemStyle = list(barBorderRadius = 5)
        ) %>%
        e_line(
          serie = Agropecuária,
          name = "Agropecuária",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T, itemStyle = list(barBorderRadius = 5)
        ) %>%
        e_line(
          serie = Total,
          name = "Total",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T, itemStyle = list(barBorderRadius = 5)
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

    # 4 - Vínculos Empregatícios no Emprego Formal por Setor Econômico----
    ## Mapa - Vínculos Empregatícios no Emprego Formal por Setor Econômico----
    output$merc4txt1 <- renderText({
      t41()
    })

    output$merc4map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$merc4ri == "Pará") {
        df <- merc4 %>%
          filter(localidade != "Pará", ano == input$merc4ano, categoria == input$merc4cat) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x$valor[x$valor == 0] <- NA
      } else {
        df <- merc4 %>%
          filter(localidade != "Pará", ano == input$merc4ano, categoria == input$merc4cat) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$merc4ri)
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
          "<strong>%s</strong><br/> <b>%s:</b> %s",
          x$name_muni,
          input$merc4cat,
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
          title = "Quantidade",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 2)
        )
    })
    ## Tabela - Vínculos Empregatícios no Emprego Formal por Setor Econômico----
    output$merc4txt2 <- renderText({
      t42()
    })

    output$merc4tab <- renderReactable({
      if (input$merc4ri == "Pará") {
        x <- merc4 %>%
          filter(localidade != "Pará", ano == input$merc4ano) %>%
          select(ri, localidade, categoria, valor) %>%
          pivot_wider(names_from = categoria, values_from = valor)
      } else {
        x <- merc4 %>%
          filter(localidade != "Pará", ano == input$merc4ano) %>%
          select(ri, localidade, categoria, valor) %>%
          pivot_wider(names_from = categoria, values_from = valor)
        x <- x %>% filter(ri == input$merc4ri)
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
          ri = colDef(name = "Região de integração"),
          localidade = colDef(sticky = "left", name = "Municípios")
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

    ## Gráfico - Vínculos Empregatícios no Emprego Formal por Setor Econômico----
    output$merc4txt3 <- renderText({
      t43()
    })
    output$merc4graf <- renderEcharts4r({
      merc4 %>%
        filter(localidade == "Pará") %>%
        pivot_wider(names_from = categoria, values_from = valor) %>%
        e_chart(x = ano) %>%
        e_line(
          serie = `Extrativa mineral`,
          name = "Extrativa mineral",
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T, itemStyle = list(barBorderRadius = 5)
        ) %>%
        e_line(
          serie = `Indústria de transformação`,
          name = "Indústria de transformação",
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T, itemStyle = list(barBorderRadius = 5)
        ) %>%
        e_line(
          serie = `Servicos industriais de utilidade pública`,
          name = "Servicos industriais de utilidade pública",
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T, itemStyle = list(barBorderRadius = 5)
        ) %>%
        e_line(
          serie = `Construção Civil`,
          name = "Construção Civil",
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T, itemStyle = list(barBorderRadius = 5)
        ) %>%
        e_line(
          serie = Comércio,
          name = "Comércio",
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T, itemStyle = list(barBorderRadius = 5)
        ) %>%
        e_line(
          serie = Serviços,
          name = "Serviços",
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T, itemStyle = list(barBorderRadius = 5)
        ) %>%
        e_line(
          serie = `Administração Pública`,
          name = "Administração Pública",
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T, itemStyle = list(barBorderRadius = 5)
        ) %>%
        e_line(
          serie = `Agropecuária, extração vegetal, caça e pesca`,
          name = "Agropecuária, extração vegetal, caça e pesca",
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T, itemStyle = list(barBorderRadius = 5)
        ) %>%
        e_line(
          serie = Total,
          name = "Total",
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T, itemStyle = list(barBorderRadius = 5)
        ) %>%
        e_legend(
          show = T, orient = "vertical", left = "80%",
          top = "15%", borderColor = "#C8C8C8",
          borderWidth = 1,
          borderRadius = 0.5
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
            formatter = htmlwidgets::JS("
              function (value, index) {
              return value.toLocaleString('pt-BR', { minimumFractionDigits: 0, maximumFractionDigits: 0 });
              }
            ")
          )
        ) %>%
        e_locale("pt-Br") %>%
        e_datazoom(toolbox = F, fillerColor = "#E5F5F9") %>%
        e_grid(show = T, width = "65%", height = "60%", left = "8%")
    })

    # 5 - Vínculos Empregatícios por Escolaridade do Trabalhador Formal----
    ## Mapa - Vínculos Empregatícios por Escolaridade do Trabalhador Formal----
    output$merc5txt1 <- renderText({
      t51()
    })

    output$merc5map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$merc5ri == "Pará") {
        df <- merc5 %>%
          filter(localidade != "Pará", ano == input$merc5ano, categoria == input$merc5cat) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x$valor[x$valor == 0] <- NA
      } else {
        df <- merc5 %>%
          filter(localidade != "Pará", ano == input$merc5ano, categoria == input$merc5cat) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$merc5ri)
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
          "<strong>%s</strong><br/> <b>%s:</b> %s",
          x$name_muni,
          input$merc5cat,
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
          title = "Quantidade",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 2)
        )
    })
    ## Tabela - Vínculos Empregatícios por Escolaridade do Trabalhador Formal----
    output$merc5txt2 <- renderText({
      t52()
    })

    output$merc5tab <- renderReactable({
      if (input$merc5ri == "Pará") {
        x <- merc5 %>%
          filter(localidade != "Pará", ano == input$merc5ano) %>%
          select(ri, localidade, categoria, valor) %>%
          pivot_wider(names_from = categoria, values_from = valor)
      } else {
        x <- merc5 %>%
          filter(localidade != "Pará", ano == input$merc5ano) %>%
          select(ri, localidade, categoria, valor) %>%
          pivot_wider(names_from = categoria, values_from = valor)
        x <- x %>% filter(ri == input$merc5ri)
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
          ri = colDef(name = "Região de integração"),
          localidade = colDef(sticky = "left", name = "Municípios")
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
    ## Gráfico - Vínculos Empregatícios por Escolaridade do Trabalhador Formal----
    output$merc5txt3 <- renderText({
      t53()
    })
    
    output$merc5graf <- renderEcharts4r({
      merc5 %>%
        filter(localidade == "Pará") %>%
        pivot_wider(names_from = categoria, values_from = valor) %>%
        e_chart(x = ano) %>%
        e_line(
          serie = Analfabeto,
          name = "Analfabeto",
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T, itemStyle = list(barBorderRadius = 5)
        ) %>%
        e_line(
          serie = `Até 5ª Incompleto`,
          name = "Até 5ª Incompleto",
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T, itemStyle = list(barBorderRadius = 5)
        ) %>%
        e_line(
          serie = `5ª Completo Fundamental`,
          name = "5ª Completo Fundamental",
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T, itemStyle = list(barBorderRadius = 5)
        ) %>%
        e_line(
          serie = `6ª a 9ª Fundamental`,
          name = "6ª a 9ª Fundamental",
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T, itemStyle = list(barBorderRadius = 5)
        ) %>%
        e_line(
          serie = `Fundamental Completo`,
          name = "Fundamental Completo",
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T, itemStyle = list(barBorderRadius = 5)
        ) %>%
        e_line(
          serie = `Médio Incompleto`,
          name = "Médio Incompleto",
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T, itemStyle = list(barBorderRadius = 5)
        ) %>%
        e_line(
          serie = `Médio Completo`,
          name = "Médio Completo",
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T, itemStyle = list(barBorderRadius = 5)
        ) %>%
        e_line(
          serie = `Superior Incompleto`,
          name = "Superior Incompleto",
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T, itemStyle = list(barBorderRadius = 5)
        ) %>%
        e_line(
          serie = `Superior Completo`,
          name = "Superior Completo",
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T, itemStyle = list(barBorderRadius = 5)
        ) %>%
        e_line(
          serie = Total,
          name = "Total",
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T, itemStyle = list(barBorderRadius = 5)
        ) %>%
        e_legend(
          show = T, orient = "vertical", left = "80%",
          top = "15%", borderColor = "#C8C8C8",
          borderWidth = 1,
          borderRadius = 0.5
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
            formatter = htmlwidgets::JS("
              function (value, index) {
              return value.toLocaleString('pt-BR', { minimumFractionDigits: 0, maximumFractionDigits: 0 });
              }
            ")
          )
        ) %>%
        e_locale("pt-Br") %>%
        e_datazoom(toolbox = F, fillerColor = "#E5F5F9") %>%
        e_grid(show = T, width = "65%", height = "60%", left = "8%")
    })
    # 6 - Remuneração Média (R$) do Trabalhador Formal----
    ## Mapa - Remuneração Média (R$) do Trabalhador Formal----
    output$merc6txt1 <- renderText({
      t61()
    })

    output$merc6map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$merc6ri == "Pará") {
        df <- merc6 %>%
          filter(localidade != "Pará", ano == input$merc6ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x$valor[x$valor == 0] <- NA
      } else {
        df <- merc6 %>%
          filter(localidade != "Pará", ano == input$merc6ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$merc6ri)
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
          "<strong>%s</strong><br/> <b>Remuneração Média (R$):</b> %s",
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
          title = "Remuneração Média (R$)",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 2)
        )
    })

    ## Tabela - Remuneração Média (R$) do Trabalhador Formal----
    output$merc6txt2 <- renderText({
      t62()
    })

    output$merc6tab <- renderReactable({
      if (input$merc6ri == "Pará") {
        x <- merc6 %>%
          filter(localidade != "Pará", ano == input$merc6ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- merc6 %>%
          filter(localidade != "Pará", ano == input$merc6ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$merc6ri)
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
          ri = colDef(name = "Região de integração"),
          localidade = colDef(name = "Municípios"),
          valor = colDef(name = "Remuneração Média(R$)", format = colFormat(separators = T, locales = "pt-BR"))
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

    ## Gráfico - Remuneração Média (R$) do Trabalhador Formal----
    output$merc6txt3 <- renderText({
      t63()
    })
    output$merc6graf <- renderEcharts4r({
      merc6 %>%
        filter(localidade == "Pará") %>%
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          color = "#f17701",
          name = "Média (R$)",
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
        e_datazoom(toolbox = F, fillerColor = "#E5F5F9") %>%
        e_grid(show = T)
    })

    # 7 - Remuneração Média (R$) do Trabalhador Formal por Sexo----
    ## Mapa - Remuneração Média (R$) do Trabalhador Formal por Sexo----
    output$merc7txt1 <- renderText({
      t71()
    })

    output$merc7map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$merc7ri == "Pará") {
        df <- merc7 %>%
          filter(localidade != "Pará", ano == input$merc7ano, categoria == input$merc7cat) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x$valor[x$valor == 0] <- NA
      } else {
        df <- merc7 %>%
          filter(localidade != "Pará", ano == input$merc7ano, categoria == input$merc7cat) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$merc7ri)
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
          "<strong>%s</strong><br/> <b>%s:</b> %s",
          x$name_muni,
          input$merc7cat,
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
          title = "Remuneração Média (R$)",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 0)
        )
    })
    ## Tabela - Remuneração Média (R$) do Trabalhador Formal por Sexo----
    output$merc7txt2 <- renderText({
      t72()
    })

    output$merc7tab <- renderReactable({
      if (input$merc7ri == "Pará") {
        x <- merc7 %>%
          filter(localidade != "Pará", ano == input$merc7ano) %>%
          select(ri, localidade, categoria, valor) %>%
          pivot_wider(names_from = categoria, values_from = valor)
      } else {
        x <- merc7 %>%
          filter(localidade != "Pará", ano == input$merc7ano) %>%
          select(ri, localidade, categoria, valor) %>%
          pivot_wider(names_from = categoria, values_from = valor)
        x <- x %>% filter(ri == input$merc7ri)
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
          ri = colDef(name = "Região de integração"),
          localidade = colDef(sticky = "left", name = "Municípios"),
          Masculino = colDef(format = colFormat(separators = T,digits = 0, locales = "pt-BR")),
          Feminino = colDef(format = colFormat(separators = T,digits = 0, locales = "pt-BR")),
          Total = colDef(format = colFormat(separators = T,digits = 0, locales = "pt-BR"))
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
    ## Gráfico - Remuneração Média (R$) do Trabalhador Formal por Sexo----
    output$merc7txt3 <- renderText({
      t73()
    })
    output$merc7graf <- renderEcharts4r({
      merc7 %>%
        filter(localidade == "Pará") %>%
        pivot_wider(names_from = categoria, values_from = valor) %>%
        e_chart(x = ano) %>%
        e_line(
          serie = Masculino,
          name = "Masculino",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T, itemStyle = list(barBorderRadius = 5)
        ) %>%
        e_line(
          serie = Feminino,
          name = "Feminino",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T, itemStyle = list(barBorderRadius = 5)
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
          name = "Média (R$)",
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
    #DOWNLOADS----
    # 1 - Vínculos Empregatícios Total no Emprego Formal----
    ## Tabela - Vínculos Empregatícios Total no Emprego Formal----
    # Filtra os dados
    merc1_2 <- reactive({
      if (input$merc1ri == "Pará") {
        x <- merc1 %>%
          filter(localidade != "Pará", ano == input$merc1ano) %>% 
          mutate(Percentual = (valor / sum(valor)) * 100)
      } else {
        x <- merc1 %>%
          filter(localidade != "Pará", 
                 ano == input$merc1ano,
                 ri == input$merc1ri) %>%
          mutate(Percentual = (valor / sum(valor)) * 100)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(merc1_2(), {
      downset_Server("merc1_2", merc1_2(), t12())
    })
    ## Gráfico - Vínculos Empregatícios Total no Emprego Formal----
    # Filtra os dados
    merc1_3 <- reactive({
      merc1 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(merc1_3(), {
      downset_Server("merc1_3", merc1_3(), t13())
    })

    # 2 - Vínculos Empregatícios no Emprego Formal por Sexo----
    ## Tabela - Vínculos Empregatícios no Emprego Formal por Sexo----
    # Filtra os dados
    merc2_2 <- reactive({
      if (input$merc2ri == "Pará") {
        x <- merc2 %>%
          filter(localidade != "Pará", ano == input$merc2ano) %>%
          pivot_wider(names_from = categoria, values_from = valor) %>%
          mutate(Masc_percentual = Masculino / (Masculino + Feminino)) %>%
          mutate(Fem_percentual = Feminino / (Masculino + Feminino))
      } else {
        x <- merc2 %>%
          filter(localidade != "Pará", 
                 ano == input$merc2ano,
                 ri == input$merc2ri) %>%
          pivot_wider(names_from = categoria, values_from = valor) %>% 
          mutate(Masc_percentual = Masculino / (Masculino + Feminino)) %>%
          mutate(Fem_percentual = Feminino / (Masculino + Feminino))
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(merc2_2(), {
      t <- "merc2_2"
      downset_Server("merc2_2", merc2_2(), t22())
    })
    ## Gráfico - Vínculos Empregatícios no Emprego Formal por Sexo----
    # Filtra os dados
    merc2_3 <- reactive({
       merc2 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(merc2_3(), {
      downset_Server("merc2_3", merc2_3(), t23())
    })

    # 3 - Vínculos Empregatícios no Emprego Formal por Grande Setor (IBGE)----
    ## Tabela - Vínculos Empregatícios no Emprego Formal por Grande Setor (IBGE)----
    # Filtra os dados
    merc3_2 <- reactive({
      if (input$merc3ri == "Pará") {
        x <- merc3 %>%
          filter(localidade != "Pará", ano == input$merc3ano) 
      } else {
        x <- merc3 %>%
          filter(localidade != "Pará",
                 ano == input$merc3ano,
                 ri == input$merc3ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(merc3_2(), {
      downset_Server("merc3_2", merc3_2(), t32())
    })
    ## Gráfico - Vínculos Empregatícios no Emprego Formal por Grande Setor (IBGE)----
    # Filtra os dados
    merc3_3 <- reactive({
      merc3 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(merc3_3(), {
      downset_Server("merc3_3", merc3_3(), t33())
    })

    # 4 - Vínculos Empregatícios no Emprego Formal por Setor Econômico----
    ## Tabela - Vínculos Empregatícios no Emprego Formal por Setor Econômico----
    # Filtra os dados
    merc4_2 <- reactive({
      if (input$merc4ri == "Pará") {
        x <- merc4 %>%
          filter(localidade != "Pará", ano == input$merc4ano)
      } else {
        x <- merc4 %>%
          filter(localidade != "Pará", 
                 ano == input$merc4ano,
                 ri == input$merc4ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(merc4_2(), {
      downset_Server("merc4_2", merc4_2(), t42())
    })
    ## Gráfico - Vínculos Empregatícios no Emprego Formal por Setor Econômico----
    # Filtra os dados
    merc4_3 <- reactive({
      merc4 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(merc4_3(), {
      downset_Server("merc4_3", merc4_3(), t43())
    })

    # 5 - Vínculos Empregatícios por Escolaridade do Trabalhador Formal----
    ## Tabela - Vínculos Empregatícios por Escolaridade do Trabalhador Formal----
    # Filtra os dados
    merc5_2 <- reactive({
      if (input$merc5ri == "Pará") {
        x <- merc5 %>%
          filter(localidade != "Pará", ano == input$merc5ano)
      } else {
        x <- merc5 %>%
          filter(localidade != "Pará", 
                 ano == input$merc5ano,
                 ri == input$merc5ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(merc5_2(), {
      downset_Server("merc5_2", merc5_2(), t52())
    })
    ## Gráfico - Vínculos Empregatícios por Escolaridade do Trabalhador Formal----
    # Filtra os dados
    merc5_3 <- reactive({
      merc5 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(merc5_3(), {
      downset_Server("merc5_3", merc5_3(), t53())
    })

    # 6 - Remuneração Média (R$) do Trabalhador Formal----
    ## Tabela - Remuneração Média (R$) do Trabalhador Formal----
    # Filtra os dados
    merc6_2 <- reactive({
      if (input$merc6ri == "Pará") {
        x <- merc6 %>%
          filter(localidade != "Pará", ano == input$merc6ano)
      } else {
        x <- merc6 %>%
          filter(localidade != "Pará", 
                 ano == input$merc6ano,
                 ri == input$merc6ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(merc6_2(), {
      downset_Server("merc6_2", merc6_2(), t62())
    })
    ## Gráfico - Remuneração Média (R$) do Trabalhador Formal----
    # Filtra os dados
    merc6_3 <- reactive({
      merc6 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(merc6_3(), {
      downset_Server("merc6_3", merc6_3(), t63())
    })

    # 7 - Remuneração Média (R$) do Trabalhador Formal por Sexo----
    ## Tabela - Remuneração Média (R$) do Trabalhador Formal por Sexo----
    # Filtra os dados
    merc7_2 <- reactive({
      if (input$merc7ri == "Pará") {
        x <- merc7 %>%
          filter(localidade != "Pará", ano == input$merc7ano) 
      } else {
        x <- merc7 %>%
          filter(localidade != "Pará", 
                 ano == input$merc7ano,
                 ri == input$merc7ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(merc7_2(), {
      downset_Server("merc7_2", merc7_2(), t72())
    })
    ## Gráfico - Remuneração Média (R$) do Trabalhador Formal por Sexo----
    # Filtra os dados
    merc7_3 <- reactive({
       merc7 %>%
        filter(localidade == "Pará")
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
#     social_merc_tab_pa_ui("social_merc_tab_pa")
#   ))
# )
# 
# 
# server <- function(input, output) {
#   social_merc_tab_pa_Server("social_merc_tab_pa")
# }
# 
# shinyApp(ui, server)
