# Funções de módulo de Economia - Lavoura Permanente - Municípal
# Função de UI
economia_lp_mp_ui <- function(id) {
  fluidPage(
    # CAMINHO DO ARQUIVO CSS----
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "estilo.css")
    ),
    # Lista de Navegação lateral----
    div(
      class = "navbar_economia",
      navbarPage(
        tags$b("Lavoura Permanente - Municípios"),
        navbarMenu(
          tags$b("Escolha um Indicador"),
          # 1 - Área Destinada à Colheita----
          tabPanel(
            "Área Destinada à Colheita",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Área Destinada à Colheita"),
              tags$div(
                class = "seletor2",
                # select municípios
                pickerInput(
                  inputId = NS(id, "lp1muni"),
                  label = "Municípios",
                  choices = lp1 %>%
                    filter(localidade != "Pará") %>%
                    pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Área Destinada à Colheita----
              box(
                title = textOutput(NS(id, "lp1txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "lp1ano"),
                  label = "Ano",
                  choices = sort(unique(lp1[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "lp1graf"), height = "600px"),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "lp1_1"))
                )
              ),
              ## Gráfico - Área Destinada à Colheita - Total----
              box(
                title = textOutput(NS(id, "lp1txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "lp1municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "lp1graf1")),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "lp1_2"))
                )
              )
            ),
            fluidRow(
              ## Tabela - Área Destinada à Colheita----
              box(
                title = textOutput(NS(id, "lp1txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "lp1tab"), height = "400px"),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "lp1_3"))
                )
              ),
              ## Tabela - Área Destinada à Colheita por municipio----
              box(
                title = textOutput(NS(id, "lp1txt4")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "lp1ano2"),
                  label = "Ano",
                  choices = sort(unique(lp1[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "lp1tab1"),height = "400px"),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "lp1_4"))
                )
              )
            )
          ),
          # 2 - Área Colhida à Colheita----
          tabPanel(
            "Área Colhida à Colheita",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Área Colhida à Colheita"),
              tags$div(
                class = "seletor2",
                # select municípios
                pickerInput(
                  inputId = NS(id, "lp2muni"),
                  label = "Municípios",
                  choices = lp5 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Área Colhida à Colheita----
              box(
                title = textOutput(NS(id, "lp2txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "lp2ano"),
                  label = "Ano",
                  choices = sort(unique(lp2[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "lp2graf"), height = "600px"),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "lp2_1"))
                )
              ),
              ## Gráfico - Área Colhida à Colheita - Total----
              box(
                title = textOutput(NS(id, "lp2txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "lp2municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "lp2graf1")),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "lp2_2"))
                )
              )
            ),
            fluidRow(
              ## Tabela - Área Colhida à Colheita----
              box(
                title = textOutput(NS(id, "lp2txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "lp2tab"), height = "400px"),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "lp2_3"))
                )
              ),
              ## Tabela - Área Colhida à Colheita por municipio----
              box(
                title = textOutput(NS(id, "lp2txt4")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "lp2ano2"),
                  label = "Ano",
                  choices = sort(unique(lp2[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "lp2tab1"),height = "400px"),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "lp2_4"))
                )
              )
            )
          ),
          # 3 - Quantidade Produzida----
          tabPanel(
            "Quantidade Produzida",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Quantidade Produzida"),
              tags$div(
                class = "seletor3",
                # select categoria
                pickerInput(
                  inputId = NS(id, "lp3cat"),
                  label = "Produto",
                  choices = unique(lp3[["categoria"]]),
                  width = "250px"
                )
              ),
              tags$div(
                class = "seletor2",
                # select municípios
                pickerInput(
                  inputId = NS(id, "lp3muni"),
                  label = "Municípios",
                  choices = NULL,
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Quantidade Produzida----
              box(
                title = textOutput(NS(id, "lp3txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                # Comparar municípios
                pickerInput(
                  inputId = NS(id, "lp3municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "lp3graf")),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PEVS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "lp3_1"))
                )
              )
            ),
            fluidRow(
              ## Tabela - Quantidade Produzida, por Tipo de Produto----
              box(
                title = textOutput(NS(id, "lp3txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "lp3tab"),height = "400px"),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PEVS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "lp3_2"))
                )
              )
            )
          ),
          # 4 - Valor da Produção----
          tabPanel(
            "Valor da Produção",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Valor da Produção"),
              tags$div(
                class = "seletor2",
                # select municípios
                pickerInput(
                  inputId = NS(id, "lp4muni"),
                  label = "Municípios",
                  choices = lp4 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Valor da Produção----
              box(
                title = textOutput(NS(id, "lp4txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "lp4ano"),
                  label = "Ano",
                  choices = sort(unique(lp4[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "lp4graf"), height = "600px"),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "lp4_1"))
                )
              ),
              ## Gráfico - Valor da Produção - Total----
              box(
                title = textOutput(NS(id, "lp4txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "lp4municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "lp4graf1")),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "lp4_2"))
                )
              )
            ),
            fluidRow(
              ## Tabela - Valor da Produção----
              box(
                title = textOutput(NS(id, "lp4txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "lp4tab"), height = "400px"),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "lp4_3"))
                )
              ),
              ## Tabela - Valor da Produção por municipio----
              box(
                title = textOutput(NS(id, "lp4txt4")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "lp4ano2"),
                  label = "Ano",
                  choices = sort(unique(lp4[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "lp4tab1"),height = "400px"),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "lp4_4"))
                )
              )
            )
          ),
          # 5 - Rendimento médio da produção----
          tabPanel(
            "Rendimento médio da produção",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Rendimento médio da produção"),
              tags$div(
                class = "seletor3",
                # select categoria
                pickerInput(
                  inputId = NS(id, "lp5cat"),
                  label = "Produto",
                  choices = unique(lp5[["categoria"]]),
                  width = "250px"
                )
              ),
              tags$div(
                class = "seletor2",
                # select municípios
                pickerInput(
                  inputId = NS(id, "lp5muni"),
                  label = "Municípios",
                  choices = NULL,
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Rendimento médio da produção----
              box(
                title = textOutput(NS(id, "lp5txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                # Comparar municípios
                pickerInput(
                  inputId = NS(id, "lp5municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "lp5graf")),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "lp5_1"))
                )
              )
            ),
            fluidRow(
              ## Tabela - Rendimento médio da produção, por Tipo de Produto----
              box(
                title = textOutput(NS(id, "lp5txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "lp5tab")),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/PAM"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "lp5_2"))
                )
              )
            )
          ),
          # fim
        )
      )
    )
  )
}
# Função do modulo servidor
economia_lp_mp_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # TÍTULOS----
    # 1 - Área Destinada à Colheita----
    ## Gráfico de barras- Área Destinada à Colheita ----
    t11 <- reactive({
      paste0(
        "Área Destinada à Colheita (hectares) por Tipo de Lavoura Permanete, ",
        input$lp1muni, " - ", input$lp1ano
      )
    })
    ## Gráfico de linha- Área Destinada à Colheita  - Total----
    t12 <- reactive({
      req(input$lp1municomp)
      if (input$lp1municomp == "Seleconar Município") {
        paste0(
          "Área Total Destinada à Colheita (hectares) da Lavoura Permanente, ",
          input$lp1muni, " - ", min(lp1$ano), " a ", max(lp1$ano)
        )
      } else {
        paste0(
          "Área Total Destinada à Colheita (hectares) da Lavoura Permanente, ",
          input$lp1muni, " x ", input$lp1municomp, " - ", min(lp1$ano), " a ", max(lp1$ano)
        )
      }
    })
    ## Tabela - Área Destinada à Colheita por produto----
    t13 <- reactive({
      paste0(
        "Área Destinada à Colheita (hectares) por Tipo de Lavoura Permanete, ",
        input$lp1muni, " - ", min(lp1$ano), " a ", max(lp1$ano)
      )
    })
    ## Tabela - Área Destinada à Colheita por produto----
    t14 <- reactive({
      ri <- lp1 %>%
        filter(localidade == input$lp1muni) %>%
        select(ri) %>%
        pull() %>%
        unique()
      paste0(
        "Área Total Destinada à Colheita (hectares) da Lavoura Permanete por Município, Região de Integração ",
        ri, " - ", input$lp1ano2
      )
    })

    # 2 - Área Colhida à Colheita----
    ## Gráfico de barras- Área Colhida à Colheita ----
    t21 <- reactive({
      paste0(
        "Área Colhida (Hectares) por Tipo de Lavoura Permanente, ",
        input$lp2muni, " - ", input$lp2ano
      )
    })
    ## Gráfico de linha- Área Colhida à Colheita  - Total----
    t22 <- reactive({
      req(input$lp2municomp)
      if (input$lp2municomp == "Seleconar Município") {
        paste0(
          "Área Total Colhida (hectares) da Lavoura Permanente, ",
          input$lp2muni, " - ", min(lp2$ano), " a ", max(lp2$ano)
        )
      } else {
        paste0(
          "Área Total Colhida (hectares) da Lavoura Permanente, ",
          input$lp2muni, " x ", input$lp2municomp, " - ", min(lp2$ano), " a ", max(lp2$ano)
        )
      }
    })
    ## Tabela - Área Colhida à Colheita por produto----
    t23 <- reactive({
      paste0(
        "Área Total Colhida (Hectares) por Tipo de Lavoura Permanente, ",
        input$lp2muni, " - ", min(lp2$ano), " a ", max(lp2$ano)
      )
    })
    ## Tabela - Área Colhida à Colheita por produto----
    t24 <- reactive({
      ri <- lp2 %>%
        filter(localidade == input$lp2muni) %>%
        select(ri) %>%
        pull() %>%
        unique()
      paste0(
        "Área Total Colhida (Hectares) da Lavoura Permanente por Município, Região de Integração ",
        ri, " - ", input$lp2ano
      )
    })

    # 3 - Quantidade Produzida----
    ## Gráfico de linha - Quantidade Produzida por Tipo de Lavoura Permanente----
    t31 <- reactive({
      req(input$lp3municomp)
      if (input$lp3municomp == "Seleconar Município") {
        paste0(
          "Quantidade Produzida na Lavoura Permanente de ", input$lp3cat,
          ", ", input$lp3muni, " - ", min(lp3$ano), " a ", max(lp3$ano)
        )
      } else {
        paste0(
          "Quantidade Produzida na Lavoura Permanente de ",
          input$lp3cat, ", ", input$lp3muni, " x ",
          input$lp3municomp, " - ", min(lp3$ano), " a ", max(lp3$ano)
        )
      }
    })
    ##Quantidade Produzida por Tipo de Lavoura Permanente----
    t32 <- reactive({
      paste0(
        "Quantidade Produzida por Tipo de Lavoura Permanente, ",
        input$lp3muni, " - ", min(lp3$ano), " a ", max(lp3$ano)
      )
    })

    # 4 - Valor da Produção----
    ## Gráfico de barras - Valor da Produção ----
    t41 <- reactive({
      paste0(
        "Valor da Produção (em mil reais) por Tipo de Lavoura Permanente, ",
        input$lp4muni, " - ", input$lp4ano
      )
    })
    ## Gráfico de linha - Valor da Produção  - Total----
    t42 <- reactive({
      req(input$lp4municomp)
      if (input$lp4municomp == "Seleconar Município") {
        paste0(
          "Valor Total da Produção (em mil reais) da Lavoura Permanente, ",
          input$lp4muni, " - ", min(lp4$ano), " a ", max(lp4$ano)
        )
      } else {
        paste0(
          "Valor Total da Produção (em mil reais) da Lavoura Permanente, ",
          input$lp4muni, " x ", input$lp4municomp, " - ", min(lp4$ano), " a ", max(lp4$ano)
        )
      }
    })
    ## Tabela - Valor da Produção por produto----
    t43 <- reactive({
      paste0(
        " Valor Total da Produção (em mil reais) por Tipo de Lavoura Permanente, ",
        input$lp4muni, " - ", min(lp4$ano), " a ", max(lp4$ano)
      )
    })
    ## Tabela - Valor da Produção por produto----
    t44 <- reactive({
      ri <- lp4 %>%
        filter(localidade == input$lp4muni) %>%
        select(ri) %>%
        pull() %>%
        unique()
      paste0(
        "Valor Total da Produção (em mil reais) por Município, Região de Integração ",
        ri, " - ", input$lp4ano
      )
    })
    # 5 - Rendimento médio da produção----
    ## Gráfico de linha - Rendimento médio da produção, por Tipo de Produto----
    t51 <- reactive({
      req(input$lp5municomp)
      if (input$lp5municomp == "Seleconar Município") {
        paste0(
          "Rendimento médio da produção da Lavoura Permanente de ", input$lp5cat,
          ", ", input$lp5muni, " - ", min(lp5$ano), " a ", max(lp5$ano)
        )
      } else {
        paste0(
          "Rendimento médio da produção da Lavoura Permanente de ", input$lp5cat,
          ", ", input$lp5muni, " x ", input$lp5municomp, " - ", min(lp5$ano), " a ", max(lp5$ano)
        )
      }
    })
    ## Tabela - Rendimento médio da produção, por Tipo de Produto----
    t52 <- reactive({
      paste0(
        "Rendimento médio da produção por Tipo de Lavoura Permanente, ",
        input$lp5muni, " - ", min(lp5$ano), " a ", max(lp5$ano)
      )
    })

    # VISUALIZAÇÃO----
    # 1 - Área Destinada à Colheita----
    ## Gráfico de barras- Área Destinada à Colheita ----
    output$lp1txt1 <- renderText({
      t11()
    })
    output$lp1graf <- renderEcharts4r({
      a <- lp1 %>%
        filter(
          localidade == input$lp1muni,
          ano == input$lp1ano
        ) %>%
        arrange(valor)
      a %>%
        e_charts(categoria) %>%
        e_bar(
          serie = valor,
          color = "#f2c94e",
          name = "Área (ha)",
          legend = F,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
          itemStyle = list(barBorderRadius = 0)
        ) %>%
        e_labels(
          position = "right",
          fontWeight = "bold",
          formatter = htmlwidgets::JS(
            glue::glue(
              "function(params) {return Intl.NumberFormat('pt-BR',
              { style: 'decimal'}).format(params.value[{{0}}]);}",
              .open = "{{",
              .close = "}}"
            )
          )
        ) %>%
        e_y_axis(
          name = " Área (ha)",
          nameTextStyle =
            list(
              fontWeight = "bold",
              padding = c(30, 0, 0, 0),
              fontSize = 14
            ),
          scale = T,
          splitNumber = 15,
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
        e_grid(show = F, width = "80%", height = "80%", left = "30%") %>%
        e_flip_coords()
    })
    ## Gráfico de linha- Área Destinada à Colheita  - Total----
    # Comparador
    lp1locomp <- reactive({
      req(input$lp1muni)
      x <- lp1 %>%
        filter(localidade != "Pará", valor > 0) %>%
        select(localidade)
      x <- x %>% filter(localidade != input$lp1muni)
    })
    observeEvent(lp1locomp(), {
      choices <- unique(pull(lp1locomp()))
      updatePickerInput(inputId = "lp1municomp", choices = c("Seleconar Município", choices), session)
    })

    output$lp1txt2 <- renderText({
      t12()
    })

    output$lp1graf1 <- renderEcharts4r({
      req(input$lp1municomp)
      if (input$lp1municomp == "Seleconar Município") {
        a <- lp1 %>%
          filter(localidade == input$lp1muni) %>%
          group_by(ano) %>%
          summarise(valor = sum(valor))
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f2c94e",
            name = "Área (ha)",
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
            name = "Área (ha)",
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
        a <- lp1 %>%
          filter(localidade == input$lp1muni) %>%
          group_by(ano) %>%
          summarise(valor = sum(valor))
        b <- lp1 %>%
          filter(localidade == input$lp1municomp) %>%
          group_by(ano) %>%
          summarise(valor = sum(valor))
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$lp1muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$lp1municomp,
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
            name = "Área (ha)",
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
    ## Tabela - Área Destinada à Colheita por produto----
    output$lp1txt3 <- renderText({
      t13()
    })
    output$lp1tab <- renderReactable({
      req(input$lp1muni)
      x <- lp1 %>%
        filter(localidade == input$lp1muni) %>%
        select(categoria, ano, valor) %>%
        pivot_wider(names_from = ano, values_from = valor)
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
          categoria = colDef(name = "Produtos", width = 200)
        ),
        defaultColDef = colDef(
          footerStyle = list(fontWeight = "bold"), na = "0",
          headerStyle = list(background = "#f7f7f8"),
          format = colFormat(separators = T)
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
    
    ## Tabela - Área Destinada à Colheita por produto----
    output$lp1txt4 <- renderText({
      t14()
    })
    output$lp1tab1 <- renderReactable({
      req(input$lp1muni)
      ris <- lp1 %>%
        filter(localidade == input$lp1muni) %>%
        select(ri) %>%
        pull()
      x <- lp1 %>%
        filter(
          ano == input$lp1ano2, localidade != "Pará",
          ri == ris
        ) %>%
        group_by(ri, localidade) %>%
        summarise(valor = sum(valor), .groups = "drop")
      x <- x %>% arrange(desc(valor))
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
          localidade = colDef(name = "Múnicípios"),
          valor = colDef(
            name = "Área (ha)", format = colFormat(separators = T),
            cell = data_bars(x,
              text_position = "above",
              box_shadow = TRUE,
              align_bars = "right",
              number_fmt = scales::number_format(
                big.mark = ".",
                decimal.mark = ","
              )
            )
          ),
          posicao = colDef(name = "nº", width = 50)
        ),
        defaultColDef = colDef(
          footerStyle = list(fontWeight = "bold"), na = "0",
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
    # 2 - Área Colhida à Colheita----
    ## Gráfico de barras- Área Colhida à Colheita ----

    output$lp2txt1 <- renderText({
      t21()
    })

    output$lp2graf <- renderEcharts4r({
      a <- lp2 %>%
        filter(localidade == input$lp2muni, ano == input$lp2ano) %>%
        arrange(valor)
      a %>%
        e_charts(categoria) %>%
        e_bar(
          serie = valor,
          color = "#f2c94e",
          name = "Área (ha)",
          legend = F,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
          itemStyle = list(barBorderRadius = 0)
        ) %>%
        e_labels(
          position = "right",
          fontWeight = "bold",
          formatter = htmlwidgets::JS(
            glue::glue(
              "function(params) {return Intl.NumberFormat('pt-BR',
              { style: 'decimal'}).format(params.value[{{0}}]);}",
              .open = "{{",
              .close = "}}"
            )
          )
        ) %>%
        e_y_axis(
          name = " Área (ha)",
          nameTextStyle =
            list(
              fontWeight = "bold",
              padding = c(30, 0, 0, 0),
              fontSize = 14
            ),
          scale = T,
          splitNumber = 15,
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
        e_grid(show = T, width = "80%", height = "80%", left = "10%") %>%
        e_flip_coords()
    })
    ## Gráfico de linha- Área Colhida à Colheita  - Total----
    # Comparador_________________________________________________
    lp2locomp <- reactive({
      req(input$lp2muni)
      x <- lp2 %>%
        filter(localidade != "Pará", valor > 0) %>%
        select(localidade)
      x <- x %>% filter(localidade != input$lp2muni)
    })
    observeEvent(lp2locomp(), {
      choices <- unique(pull(lp2locomp()))
      updatePickerInput(inputId = "lp2municomp", choices = c("Seleconar Município", choices), session)
    })
    # ____________________________________________________________

    output$lp2txt2 <- renderText({
      t22()
    })

    output$lp2graf1 <- renderEcharts4r({
      req(input$lp2municomp)
      if (input$lp2municomp == "Seleconar Município") {
        a <- lp2 %>%
          filter(localidade == input$lp2muni) %>%
          group_by(ano) %>%
          summarise(valor = sum(valor))
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f2c94e",
            name = "Área (ha)",
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
            name = "Área (ha)",
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
        a <- lp2 %>%
          filter(localidade == input$lp2muni) %>%
          group_by(ano) %>%
          summarise(valor = sum(valor))
        b <- lp2 %>%
          filter(localidade == input$lp2municomp) %>%
          group_by(ano) %>%
          summarise(valor = sum(valor))
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$lp2muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$lp2municomp,
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
            name = "Área (ha)",
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
    ## Tabela - Área Colhida à Colheita por produto----
    output$lp2txt3 <- renderText({
      t23()
    })
    output$lp2tab <- renderReactable({
      req(input$lp2muni)
      x <- lp2 %>%
        filter(localidade == input$lp2muni) %>%
        select(categoria, ano, valor) %>%
        pivot_wider(names_from = ano, values_from = valor)
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
          categoria = colDef(name = "Produtos", width = 200)
        ),
        defaultColDef = colDef(
          footerStyle = list(fontWeight = "bold"), na = "0",
          headerStyle = list(background = "#f7f7f8"),
          format = colFormat(separators = T)
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
    ## Tabela - Área Colhida à Colheita por produto----
    output$lp2txt4 <- renderText({
      t24() 
    })
    output$lp2tab1 <- renderReactable({
      req(input$lp2muni)
      ris <- lp2 %>%
        filter(localidade == input$lp2muni) %>%
        select(ri) %>%
        pull()
      x <- lp2 %>%
        filter(
          ano == input$lp2ano, localidade != "Pará",
          ri == ris
        ) %>%
        group_by(ri, localidade) %>%
        summarise(valor = sum(valor), .groups = "drop")
      x <- x %>% arrange(desc(valor))
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
          localidade = colDef(name = "Múnicípios"),
          valor = colDef(
            name = "Área (ha)", format = colFormat(separators = T),
            cell = data_bars(x,
              text_position = "above",
              box_shadow = TRUE,
              align_bars = "right",
              number_fmt = scales::number_format(
                big.mark = ".",
                decimal.mark = ","
              )
            )
          ),
          posicao = colDef(name = "nº", width = 50)
        ),
        defaultColDef = colDef(
          footerStyle = list(fontWeight = "bold"), na = "0",
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
    # 3 - Quantidade Produzida----
    ## Controle
    lp3loc <- reactive({
      lp3 %>%
        filter(categoria == input$lp3cat, localidade != "Pará", valor > 0) %>%
        select(localidade)
    })
    observeEvent(lp3loc(), {
      choices <- unique(pull(lp3loc()))
      updatePickerInput(inputId = "lp3muni", choices = choices, session)
    })
    lp3locomp <- reactive({
      req(input$lp3muni)
      x <- lp3 %>%
        filter(categoria == input$lp3cat, localidade != "Pará", valor > 0) %>%
        select(localidade)
      x <- x %>% filter(localidade != input$lp3muni)
    })
    observeEvent(lp3locomp(), {
      choices <- unique(pull(lp3locomp()))
      updatePickerInput(inputId = "lp3municomp", choices = c("Seleconar Município", choices), session)
    })
    ## Gráfico de linha - Quantidade Produzida por Tipo de Lavoura Permanente----
    output$lp3txt1 <- renderText({
      t31() 
    })

    output$lp3graf <- renderEcharts4r({
      req(input$lp3municomp)
      if (input$lp3municomp == "Seleconar Município") {
        a <- lp3 %>% filter(localidade == input$lp3muni, categoria == input$lp3cat)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f2c94e",
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
        a <- lp3 %>% filter(localidade == input$lp3muni, categoria == input$lp3cat)
        b <- lp3 %>% filter(localidade == input$lp3municomp, categoria == input$lp3cat)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$lp3muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$lp3municomp,
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

    ## Tabela - Quantidade Produzida, por Tipo de Produto----
    output$lp3txt2 <- renderText({
      t32()
    })
    output$lp3tab <- renderReactable({
      req(input$lp3muni)
      x <- lp3 %>%
        filter(localidade == input$lp3muni) %>%
        select(categoria, ano, valor) %>%
        pivot_wider(names_from = ano, values_from = valor)
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
          categoria = colDef(name = "Produtos", width = 250)
        ),
        defaultColDef = colDef(
          footerStyle = list(fontWeight = "bold"), na = "0",
          headerStyle = list(background = "#f7f7f8"),
          format = colFormat(separators = T)
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
    # 4 - Valor da Produção----
    ## Gráfico de barras - Valor da Produção ----
    output$lp4txt1 <- renderText({
      t41()
    })
    output$lp4graf <- renderEcharts4r({
      a <- lp4 %>%
        filter(localidade == input$lp4muni, ano == input$lp4ano) %>%
        arrange(valor)
      a %>%
        e_charts(categoria) %>%
        e_bar(
          serie = valor,
          color = "#f2c94e",
          name = "Valor (R$)",
          legend = F,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
          itemStyle = list(barBorderRadius = 0)
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
          name = " Valor (R$)",
          nameTextStyle =
            list(
              fontWeight = "bold",
              padding = c(30, 0, 0, 0),
              fontSize = 14
            ),
          scale = T,
          splitNumber = 15,
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
        e_grid(show = T, width = "80%", height = "80%", left = "10%") %>%
        e_flip_coords()
    })
    ## Gráfico de linha - Valor da Produção  - Total----
    # Comparador_________________________________________________
    lp4locomp <- reactive({
      req(input$lp4muni)
      x <- lp4 %>%
        filter(localidade != "Pará", valor > 0) %>%
        select(localidade)
      x <- x %>% filter(localidade != input$lp4muni)
    })
    observeEvent(lp4locomp(), {
      choices <- unique(pull(lp4locomp()))
      updatePickerInput(inputId = "lp4municomp", choices = c("Seleconar Município", choices), session)
    })
    # ____________________________________________________________
    output$lp4txt2 <- renderText({
    t42()
    })

    output$lp4graf1 <- renderEcharts4r({
      req(input$lp4municomp)
      if (input$lp4municomp == "Seleconar Município") {
        a <- lp4 %>%
          filter(localidade == input$lp4muni) %>%
          group_by(ano) %>%
          summarise(valor = sum(valor))
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f2c94e",
            name = "Valor (R$)",
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
            name = "Valor (R$)",
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
        a <- lp4 %>%
          filter(localidade == input$lp4muni) %>%
          group_by(ano) %>%
          summarise(valor = sum(valor))
        b <- lp4 %>%
          filter(localidade == input$lp4municomp) %>%
          group_by(ano) %>%
          summarise(valor = sum(valor))
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$lp4muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$lp4municomp,
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
            name = "Valor (R$)",
            nameTextStyle = list(fontWeight = "bold", fontSize = 14),
            scale = T,
            axisLabel = list(
              formatter = htmlwidgets::JS("
              function (value, index) {
              return value.toLocaleString('pt-BR', { minimumFractionDigits: 1, maximumFractionDigits: 1 });
              }
            ")
            )
          ) %>%
          e_locale("pt-Br") %>%
          e_datazoom(toolbox = F, fillerColor = "#E5F5F9") %>%
          e_grid(show = T)
      }
    })
    ## Tabela - Valor da Produção por produto----
    output$lp4txt3 <- renderText({
    t43()
    })
    output$lp4tab <- renderReactable({
      req(input$lp4muni)
      x <- lp4 %>%
        filter(localidade == input$lp4muni) %>%
        select(categoria, ano, valor) %>%
        pivot_wider(names_from = ano, values_from = valor)
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
          categoria = colDef(name = "Produtos", width = 200)
        ),
        defaultColDef = colDef(
          footerStyle = list(fontWeight = "bold"), na = "0",
          headerStyle = list(background = "#f7f7f8"),
          format = colFormat(separators = T)
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
    ## Tabela - Valor da Produção por produto----
    output$lp4txt4 <- renderText({
    t44()
    })
    output$lp4tab1 <- renderReactable({
      req(input$lp4muni)
      ris <- lp4 %>%
        filter(localidade == input$lp4muni) %>%
        select(ri) %>%
        pull()
      x <- lp4 %>%
        filter(ano == input$lp4ano, localidade != "Pará", ri == ris) %>%
        group_by(ri, localidade) %>%
        summarise(valor = sum(valor), .groups = "drop")
      x <- x %>% arrange(desc(valor))
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
          localidade = colDef(name = "Múnicípios"),
          valor = colDef(
            name = "Valor (R$)", format = colFormat(separators = T),
            cell = data_bars(x,
              text_position = "above",
              box_shadow = TRUE,
              align_bars = "right",
              number_fmt = scales::number_format(
                big.mark = ".",
                decimal.mark = ","
              )
            )
          ),
          posicao = colDef(name = "nº", width = 50)
        ),
        defaultColDef = colDef(
          footerStyle = list(fontWeight = "bold"), na = "0",
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
    # 5 - Rendimento médio da produção----
    ## Controle
    lp5loc <- reactive({
      lp5 %>%
        filter(categoria == input$lp5cat, localidade != "Pará", valor > 0) %>%
        select(localidade)
    })
    observeEvent(lp5loc(), {
      choices <- unique(pull(lp5loc()))
      updatePickerInput(inputId = "lp5muni", choices = choices, session)
    })
    lp5locomp <- reactive({
      req(input$lp5muni)
      x <- lp5 %>%
        filter(categoria == input$lp5cat, localidade != "Pará", valor > 0) %>%
        select(localidade)
      x <- x %>% filter(localidade != input$lp5muni)
    })
    observeEvent(lp5locomp(), {
      choices <- unique(pull(lp5locomp()))
      updatePickerInput(inputId = "lp5municomp", choices = c("Seleconar Município", choices), session)
    })
    ## Gráfico de linha - Rendimento médio da produção, por Tipo de Produto----
    output$lp5txt1 <- renderText({
    t51()
    })

    output$lp5graf <- renderEcharts4r({
      req(input$lp5municomp)
      if (input$lp5municomp == "Seleconar Município") {
        a <- lp5 %>% filter(localidade == input$lp5muni, categoria == input$lp5cat)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f2c94e",
            name = input$lp5cat,
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
            name = "Redimento Médio",
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
        a <- lp5 %>% filter(localidade == input$lp5muni, categoria == input$lp5cat)
        b <- lp5 %>% filter(localidade == input$lp5municomp, categoria == input$lp5cat)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$lp5muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$lp5municomp,
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
            name = "Redimento Médio",
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

    ## Tabela - Rendimento médio da produção, por Tipo de Produto----
    output$lp5txt2 <- renderText({
    t52()
    })
    output$lp5tab <- renderReactable({
      req(input$lp5muni)
      x <- lp5 %>%
        filter(localidade == input$lp5muni) %>%
        select(categoria, ano, valor) %>%
        pivot_wider(names_from = ano, values_from = valor)
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
          categoria = colDef(name = "Produtos", width = 250)
        ),
        defaultColDef = colDef(
          footerStyle = list(fontWeight = "bold"), na = "0",
          headerStyle = list(background = "#f7f7f8"),
          format = colFormat(separators = T)
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
    # DOWNLOADS----
    # 1 - Área Destinada à Colheita----
    ## Gráfico - Área Destinada à Colheita----
    # Filtra os dados
    lp1_1 <- reactive({
      a <-
        lp1 %>%
        filter(
          localidade == input$lp1muni,
          ano == input$lp1ano
        ) %>%
        arrange(valor)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    downset_Server("lp1_1", lp1_1(), t11())

    ## Gráfico - Área Destinada à Colheita - Total----
    # Filtra os dados
    lp1_2 <- reactive({
      req(input$lp1municomp)
      if (input$lp1municomp == "Seleconar Município") {
        a <- lp1 %>%
          filter(localidade == input$lp1muni) %>%
          group_by(
            Temática,
            `Sub temática`,
            Indicador,
            ri,
            localidade,
            Variável,
            Categoria,
            ano
          ) %>%
          summarise(valor = sum(valor), .groups = "drop")
      } else {
        a <- lp1 %>%
          filter(localidade == input$lp1muni) %>%
          group_by(
            Temática,
            `Sub temática`,
            Indicador,
            ri,
            localidade,
            Variável,
            Categoria,
            ano
          ) %>%
          summarise(valor = sum(valor), .groups = "drop")
        b <- lp1 %>%
          filter(localidade == input$lp1municomp) %>%
          group_by(
            Temática,
            `Sub temática`,
            Indicador,
            ri,
            localidade,
            Variável,
            Categoria,
            ano
          ) %>%
          summarise(valor = sum(valor), .groups = "drop")
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    downset_Server("lp1_2", lp1_2(),t12())
    
    ## Tabela - Área Destinada à Colheita----
    # Filtra os dados
    lp1_3 <- reactive({
      req(input$lp1muni)
      x <- lp1 %>%
        filter(localidade == input$lp1muni) %>%
        pivot_wider(names_from = ano, values_from = valor)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    downset_Server("lp1_3", lp1_3(),t13())
    
    ## Tabela - Área Destinada à Colheita por municipio----
    # Filtra os dados
    lp1_4 <- reactive({
      req(input$lp1muni)
      ris <- lp1 %>%
        filter(localidade == input$lp1muni) %>%
        select(ri) %>%
        pull()
      x <- lp1 %>%
        filter(
          ano == input$lp1ano2,
          localidade != "Pará",
          ri == ris
        ) %>%
        group_by(
          Temática,
          `Sub temática`,
          Indicador,
          ri,
          localidade,
          Variável,
          ano
        ) %>%
        summarise(
          valor = sum(valor),
          .groups = "drop"
        )
      x <- x %>% arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    downset_Server("lp1_4", lp1_4(),t14())
    
    # 2 - Área Colhida à Colheita----
    ## Gráfico - Área Colhida à Colheita----
    # Filtra os dados
    lp2_1 <- reactive({
      a <- lp2 %>%
        filter(
          localidade == input$lp2muni,
          ano == input$lp2ano
        ) %>%
        arrange(valor)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    downset_Server("lp2_1", lp2_1(),t21())
    
    ## Gráfico - Área Colhida à Colheita - Total----
    # Filtra os dados
    lp2_2 <- reactive({
      req(input$lp2municomp)
      if (input$lp2municomp == "Seleconar Município") {
        a <- lp2 %>%
          filter(localidade == input$lp2muni) %>%
          group_by(
            Temática,
            `Sub temática`,
            Indicador,
            ri,
            localidade,
            Variável,
            Categoria,
            ano
          ) %>%
          summarise(valor = sum(valor), .groups = "drop")
      } else {
        a <- lp2 %>%
          filter(localidade == input$lp2muni) %>%
          group_by(
            Temática,
            `Sub temática`,
            Indicador,
            ri,
            localidade,
            Variável,
            Categoria,
            ano
          ) %>%
          summarise(valor = sum(valor), .groups = "drop")
        b <- lp2 %>%
          filter(localidade == input$lp2municomp) %>%
          group_by(
            Temática,
            `Sub temática`,
            Indicador,
            ri,
            localidade,
            Variável,
            Categoria,
            ano
          ) %>%
          summarise(valor = sum(valor), .groups = "drop")
        dt <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    downset_Server("lp2_2", lp2_2(),t22())
    
    ## Tabela - Área Colhida à Colheita----
    # Filtra os dados
    lp2_3 <- reactive({
      req(input$lp2muni)
      x <- lp2 %>%
        filter(localidade == input$lp2muni) %>%
        pivot_wider(names_from = ano, values_from = valor)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    downset_Server("lp2_3", lp2_3(),t23())
    
    ## Tabela - Área Colhida à Colheita por municipio----
    # Filtra os dados
    lp2_4 <- reactive({
      req(input$lp2muni)
      ris <- lp2 %>%
        filter(localidade == input$lp2muni) %>%
        select(ri) %>%
        pull()
      x <- lp2 %>%
        filter(
          ano == input$lp2ano, localidade != "Pará",
          ri == ris
        ) %>%
        group_by(
          Temática,
          `Sub temática`,
          Indicador,
          ri,
          localidade,
          Variável,
          ano
        ) %>%
        summarise(valor = sum(valor), .groups = "drop")
      x <- x %>% arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    downset_Server("lp2_4", lp2_4(),t24())
    
    # 3 - Quantidade Produzida----
    ## Gráfico - Quantidade Produzida----
    # Filtra os dados
    lp3_1 <- reactive({
      req(input$lp3municomp)
      if (input$lp3municomp == "Seleconar Município") {
        a <- lp3 %>%
          filter(
            localidade == input$lp3muni,
            categoria == input$lp3cat
          )
      } else {
        a <- lp3 %>%
          filter(
            localidade == input$lp3muni,
            categoria == input$lp3cat
          )
        b <- lp3 %>%
          filter(
            localidade == input$lp3municomp,
            categoria == input$lp3cat
          )
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    downset_Server("lp3_1", lp3_1(),t31())
    
    ## Tabela - Quantidade Produzida, por Tipo de Produto----
    # Filtra os dados
    lp3_2 <- reactive({
      req(input$lp3muni)
      x <- lp3 %>%
        filter(localidade == input$lp3muni) %>%
        pivot_wider(names_from = ano, values_from = valor)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    downset_Server("lp3_2", lp3_2(), t32)
    
    # 4 - Valor da Produção----
    ## Gráfico - Valor da Produção----
    # Filtra os dados
    lp4_1 <- reactive({
      a <- lp4 %>%
        filter(
          localidade == input$lp4muni,
          ano == input$lp4ano
        ) %>%
        arrange(valor)
    })
    # Monitora a base filtrada, define o texto a ser baixado
    downset_Server("lp4_1", lp4_1(), t41())
    
    ## Gráfico - Valor da Produção - Total----
    # Filtra os dados
    lp4_2 <- reactive({
      req(input$lp4municomp)
      if (input$lp4municomp == "Seleconar Município") {
        a <- lp4 %>%
          filter(localidade == input$lp4muni) %>%
          group_by(
            Temática,
            `Sub temática`,
            Indicador,
            ri,
            localidade,
            Variável,
            Categoria,
            ano
          ) %>%
          summarise(valor = sum(valor), .groups = "drop")
      } else {
        a <- lp4 %>%
          filter(localidade == input$lp4muni) %>%
          group_by(
            Temática,
            `Sub temática`,
            Indicador,
            ri,
            localidade,
            Variável,
            Categoria,
            ano
          ) %>%
          summarise(valor = sum(valor), .groups = "drop")
        b <- lp4 %>%
          filter(localidade == input$lp4municomp) %>%
          group_by(
            Temática,
            `Sub temática`,
            Indicador,
            ri,
            localidade,
            Variável,
            Categoria,
            ano
          ) %>%
          summarise(valor = sum(valor), .groups = "drop")
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, define o texto a ser baixado
    downset_Server("lp4_2", lp4_2(), t42())
    
    ## Tabela - Valor da Produção----
    # Filtra os dados
    lp4_3 <- reactive({
      req(input$lp4muni)
      x <- lp4 %>%
        filter(localidade == input$lp4muni) %>%
        pivot_wider(names_from = ano, values_from = valor)
    })
    # Monitora a base filtrada, define o texto a ser baixado
    downset_Server("lp4_3", lp4_3(), t43())
    
    ## Tabela - Valor da Produção por municipio----
    # Filtra os dados
    lp4_4 <- reactive({
      req(input$lp4muni)
      ris <- lp4 %>%
        filter(localidade == input$lp4muni) %>%
        select(ri) %>%
        pull()
      x <- lp4 %>%
        filter(
          ano == input$lp4ano, localidade != "Pará",
          ri == ris
        ) %>%
        group_by(
          Temática,
          `Sub temática`,
          Indicador,
          ri,
          localidade,
          Variável,
          ano
        ) %>%
        summarise(
          valor = sum(valor),
          .groups = "drop"
        ) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, define o texto a ser baixado
    downset_Server("lp4_4", lp4_4(), t44())
    
    # 5 - Rendimento médio da produção----
    ## Gráfico - Rendimento médio da produção, por Tipo de Produto----
    # Filtra os dados
    lp5_1 <- reactive({
      req(input$lp5municomp)
      if (input$lp5municomp == "Seleconar Município") {
        a <- lp5 %>%
          filter(
            localidade == input$lp5muni,
            categoria == input$lp5cat
          )
      } else {
        a <- lp5 %>%
          filter(
            localidade == input$lp5muni,
            categoria == input$lp5cat
          )
        b <- lp5 %>%
          filter(
            localidade == input$lp5municomp,
            categoria == input$lp5cat
          )
        df <- rbind(a, b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    downset_Server("lp5_1", lp5_1(), t51())
    
    ## Tabela - Rendimento médio da produção, por Tipo de Produto----
    # Filtra os dados
    lp5_2 <- reactive({
      req(input$lp5muni)
      x <- lp5 %>%
        filter(localidade == input$lp5muni) %>%
        pivot_wider(names_from = ano, values_from = valor)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    downset_Server("lp5_2", lp5_2(), t52())
    
  })
}
#Play do Módulo
# ui <- dashboardPage(
#   header = dashboardHeader(),
#   sidebar = dashboardSidebar(),
#   body = dashboardBody(fluidPage(economia_lp_mp_ui("economia_lp_mp")))
# )
# 
# 
# server <- function(input, output) {
#   economia_lp_mp_Server("economia_lp_mp")
# }
# 
# shinyApp(ui, server)
