# Funções de módulo de Economia - PIB - Municipal
# Função de UI
economia_pib_mp_ui <- function(id) {
  fluidPage(
    # CAMINHO DO ARQUIVO CSS----
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "estilo.css")
    ),
    # Lista de Navegação lateral----
    div(
      class = "navbar_economia",
      navbarPage(
        tags$b("PIB - Municipios"),
        navbarMenu(
          tags$b("Escolha um Indicador"),
          # 1 - Produto Interno Bruto a Preços Correntes (Mil Reais)----
          tabPanel(
            "Produto Interno Bruto a Preços Correntes (Mil Reais)",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Produto Interno Bruto a Preços Correntes (Mil Reais)"),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "pib1muni"),
                  label = "Município",
                  choices = pib1 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Produto Interno Bruto a Preços Correntes (Mil Reais)----
              box(
                title = textOutput(NS(id, "pib1txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "pib1municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "pib1graf")),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/FAPESPA"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pib1_1"))
                )
              )
            ),
            fluidRow(
              ## Tabela - Produto Interno Bruto a Preços Correntes (Mil Reais)----
              box(
                title = textOutput(NS(id, "pib1txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "pib1ano"),
                  label = "Ano",
                  choices = sort(unique(pib1[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "pib1tab2"), height = "400px"),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/FAPESPA"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pib1_2"))
                )
              )
            )
          ),

          # 2 - Valor Adicionado Bruto a Preços Correntes Total (Mil Reais)----
          tabPanel(
            "Valor Adicionado Bruto a Preços Correntes Total (Mil Reais)",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Valor Adicionado Bruto a Preços Correntes Total (Mil Reais)"),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "pib2muni"),
                  label = "Município",
                  choices = pib2 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Valor Adicionado Bruto a Preços Correntes Total (Mil Reais)----
              box(
                title = textOutput(NS(id, "pib2txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "pib2municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "pib2graf")),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/FAPESPA"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pib2_1"))
                )
              )
            ),
            fluidRow(
              ## Tabela - Valor Adicionado Bruto a Preços Correntes Total (Mil Reais)----
              box(
                title = textOutput(NS(id, "pib2txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "pib2ano"),
                  label = "Ano",
                  choices = sort(unique(pib2[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "pib2tab2"), height = "400px"),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/FAPESPA"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pib2_2"))
                )
              )
            )
          ),

          # 3 - Valor Adicionado Bruto a Preços Correntes da Agropecuária (Mil Reais)----
          tabPanel(
            "Valor Adicionado Bruto a Preços Correntes da Agropecuária (Mil Reais)",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Valor Adicionado Bruto a Preços Correntes da Agropecuária (Mil Reais)"),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "pib3muni"),
                  label = "Município",
                  choices = pib3 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Valor Adicionado Bruto a Preços Correntes da Agropecuária (Mil Reais)----
              box(
                title = textOutput(NS(id, "pib3txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "pib3municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "pib3graf")),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/FAPESPA"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pib3_1"))
                )
              )
            ),
            fluidRow(
              ## Tabela - Valor Adicionado Bruto a Preços Correntes da Agropecuária (Mil Reais)----
              box(
                title = textOutput(NS(id, "pib3txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "pib3ano"),
                  label = "Ano",
                  choices = sort(unique(pib3[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "pib3tab2"), height = "400px"),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/FAPESPA"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pib3_2"))
                )
              )
            )
          ),

          # 4 - Valor adicionado bruto a preços correntes da indústria (Mil Reais)----
          tabPanel(
            "Valor adicionado bruto a preços correntes da indústria (Mil Reais)",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Valor adicionado bruto a preços correntes da indústria (Mil Reais)"),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "pib4muni"),
                  label = "Município",
                  choices = pib4 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Valor adicionado bruto a preços correntes da indústria (Mil Reais)----
              box(
                title = textOutput(NS(id, "pib4txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "pib4municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "pib4graf")),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/FAPESPA"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pib4_1"))
                )
              )
            ),
            fluidRow(
              ## Tabela - Valor adicionado bruto a preços correntes da indústria (Mil Reais)----
              box(
                title = textOutput(NS(id, "pib4txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "pib4ano"),
                  label = "Ano",
                  choices = sort(unique(pib4[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "pib4tab2"), height = "400px"),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/FAPESPA"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pib4_2"))
                )
              )
            )
          ),

          # 5 - Valor Adicionado Bruto a Preços Correntes dos Serviços, Exclusive Administração, Saúde e Educação Públicas e Seguridade Social (Mil Reais)----
          tabPanel(
            "Valor Adicionado Bruto a Preços Correntes dos Serviços, Exclusive Administração, Saúde e Educação Públicas e Seguridade Social (Mil Reais)",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Valor Adicionado Bruto a Preços Correntes dos Serviços, Exclusive Administração, Saúde e Educação Públicas e Seguridade Social (Mil Reais)"),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "pib5muni"),
                  label = "Município",
                  choices = pib5 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Valor Adicionado Bruto a Preços Correntes dos Serviços, Exclusive Administração, Saúde e Educação Públicas e Seguridade Social (Mil Reais)----
              box(
                title = textOutput(NS(id, "pib5txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "pib5municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "pib5graf")),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/FAPESPA"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pib5_1"))
                )
              )
            ),
            fluidRow(
              ## Tabela - Valor Adicionado Bruto a Preços Correntes dos Serviços, Exclusive Administração, Saúde e Educação Públicas e Seguridade Social (Mil Reais)----
              box(
                title = textOutput(NS(id, "pib5txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "pib5ano"),
                  label = "Ano",
                  choices = sort(unique(pib5[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "pib5tab2"), height = "400px"),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/FAPESPA"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pib5_2"))
                )
              )
            )
          ),

          # 6 - Valor Adicionado Bruto a Preços Correntes da Administração, Saúde e Educação Públicas e Seguridade Social (Mil Reais)----
          tabPanel(
            "Valor Adicionado Bruto a Preços Correntes da Administração, Saúde e Educação Públicas e Seguridade Social (Mil Reais)",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Valor Adicionado Bruto a Preços Correntes da Administração, Saúde e Educação Públicas e Seguridade Social (Mil Reais)"),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "pib6muni"),
                  label = "Município",
                  choices = pib6 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Valor Adicionado Bruto a Preços Correntes da Administração, Saúde e Educação Públicas e Seguridade Social (Mil Reais)----
              box(
                title = textOutput(NS(id, "pib6txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "pib6municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "pib6graf")),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/FAPESPA"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pib6_1"))
                )
              )
            ),
            fluidRow(
              ## Tabela - Valor Adicionado Bruto a Preços Correntes da Administração, Saúde e Educação Públicas e Seguridade Social (Mil Reais)----
              box(
                title = textOutput(NS(id, "pib6txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "pib6ano"),
                  label = "Ano",
                  choices = sort(unique(pib6[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "pib6tab2"), height = "400px"),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/FAPESPA"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pib6_2"))
                )
              )
            )
          ),

          # 7 - Impostos, Líquidos de Subsídios, Sobre Produtos a Preços Correntes (Mil Reais)----
          tabPanel(
            "Impostos, Líquidos de Subsídios, Sobre Produtos a Preços Correntes (Mil Reais)",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Impostos, Líquidos de Subsídios, Sobre Produtos a Preços Correntes (Mil Reais)"),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "pib7muni"),
                  label = "Município",
                  choices = pib7 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Impostos, Líquidos de Subsídios, Sobre Produtos a Preços Correntes (Mil Reais)----
              box(
                title = textOutput(NS(id, "pib7txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "pib7municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "pib7graf")),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/FAPESPA"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pib7_1"))
                )
              )
            ),
            fluidRow(
              ## Tabela - Impostos, Líquidos de Subsídios, Sobre Produtos a Preços Correntes (Mil Reais)----
              box(
                title = textOutput(NS(id, "pib7txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "pib7ano"),
                  label = "Ano",
                  choices = sort(unique(pib7[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "pib7tab2"), height = "400px"),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/FAPESPA"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pib7_2"))
                )
              )
            )
          ),

          # 8 - Evolução do Produto Interno Bruto per Capita----
          tabPanel(
            "Produto Interno Bruto per Capita",
            panel(
              ## Controle----
              heading =
                h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                   "Produto Interno Bruto per Capita"),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "pib8muni"),
                  label = "Município",
                  choices = pib8 %>% filter(localidade != "Pará") %>% pull(localidade) %>% unique(),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Gráfico - Evolução do Produto Interno Bruto per Capita----
              box(
                title = textOutput(NS(id, "pib8txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "pib8municomp"),
                  label = "Comparar Município",
                  choices = NULL,
                  width = "200px",
                  options = list(`none-selected-text` = "Selecione um município")
                ),
                withSpinner(
                  echarts4rOutput(NS(id, "pib8graf")),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/FAPESPA"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pib8_1"))
                )
              )
            ),
            fluidRow(
              ## Tabela - Evolução do Produto Interno Bruto per Capita----
              box(
                title = textOutput(NS(id, "pib8txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                pickerInput(
                  inputId = NS(id, "pib8ano"),
                  label = "Ano",
                  choices = sort(unique(pib8[["ano"]]), decreasing = T),
                  width = "100px"
                ),
                withSpinner(
                  reactableOutput(NS(id, "pib8tab2"), height = "400px"),
                  type = 8,
                  color = "#f2c94e",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "IBGE/FAPESPA"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "pib8_2"))
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
economia_pib_mp_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # TÍTULOS----
    # 1 - Produto Interno Bruto a Preços Correntes (Mil Reais)----
    ## Gráfico de linha- Produto Interno Bruto a Preços Correntes (Mil Reais)----
    t11 <- reactive({
      req(input$pib1municomp)
      if (input$pib1municomp == "Selecione um município") {
        paste0("Produto Interno Bruto a Preços Correntes (Mil Reais), ",
               input$pib1muni, " - ", min(pib1$ano), " a ", max(pib1$ano))
      } else {
        paste0("Produto Interno Bruto a Preços Correntes (Mil Reais), ",
               input$pib1muni, " x ", input$pib1municomp, " - ", min(pib1$ano), " a ", max(pib1$ano))
      }
    })
    
    ## Tabela - Produto Interno Bruto a Preços Correntes (Mil Reais) da mesma Região de Integração----
    t12 <- reactive({
      ri <- pib1 %>%
        filter(ano == input$pib1ano, localidade == input$pib1muni) %>%
        pull(ri)
      paste0("Produto Interno Bruto a Preços Correntes (Mil Reais) dos municípios, Região de Integração ",
             ri, " - ", input$pib1ano)
    })
    # 2 - Valor Adicionado Bruto a Preços Correntes Total (Mil Reais)----
    ## Gráfico - Valor Adicionado Bruto a Preços Correntes Total (Mil Reais)----
    t21 <- reactive({
      req(input$pib2municomp)
      if (input$pib2municomp == "Selecione um município") {
        paste0("Valor Adicionado Bruto a Preços Correntes Total (Mil Reais), ",
               input$pib2muni, " - ", min(pib2$ano), " a ", max(pib2$ano))
      } else {
        paste0("Valor Adicionado Bruto a Preços Correntes Total (Mil Reais), "
               , input$pib2muni, " x ", input$pib2municomp, " - ", min(pib2$ano), " a ", max(pib2$ano))
      }
    })
    ## Tabela - Valor Adicionado Bruto a Preços Correntes Total (Mil Reais) da mesma Região de Integração----
    t22 <- reactive({
      ri <- pib2 %>%
        filter(ano == input$pib2ano, localidade == input$pib2muni) %>%
        pull(ri)
      paste0("Valor Adicionado Bruto a Preços Correntes Total (Mil Reais) dos municípios, Região de Integração ",
             ri, " - ", input$pib2ano)
    })
    # 3 - Valor Adicionado Bruto a Preços Correntes da Agropecuária (Mil Reais)----
    ## Gráfico - Valor Adicionado Bruto a Preços Correntes da Agropecuária (Mil Reais)----
    t31 <- reactive({
      req(input$pib3municomp)
      if (input$pib3municomp == "Selecione um município") {
        paste0("Valor Adicionado Bruto a Preços Correntes da Agropecuária (Mil Reais), ",
               input$pib3muni, " - ", min(pib3$ano), " a ", max(pib3$ano))
      } else {
        paste0("Valor Adicionado Bruto a Preços Correntes da Agropecuária (Mil Reais), ",
               input$pib3muni, " x ", input$pib3municomp, " - ", min(pib3$ano), " a ", max(pib3$ano))
      }
    })
    ## Tabela - Valor Adicionado Bruto a Preços Correntes da Agropecuária (Mil Reais) da mesma Região de Integração----
    t32 <- reactive({
      ri <- pib3 %>%
        filter(ano == input$pib3ano, localidade == input$pib3muni) %>%
        pull(ri)
      paste0("Valor Adicionado Bruto a Preços Correntes da Agropecuária (Mil Reais) dos municípios, Região de Integração ",
             ri, " - ", input$pib3ano)
    })
    # 4 - Valor adicionado bruto a preços correntes da indústria (Mil Reais)----
    ## Gráfico - Valor adicionado bruto a preços correntes da indústria (Mil Reais)----
    t41 <- reactive({
      req(input$pib4municomp)
      if (input$pib4municomp == "Selecione um município") {
        paste0("Valor adicionado bruto a preços correntes da indústria (Mil Reais), ",
               input$pib4muni, " - ", min(pib4$ano), " a ", max(pib4$ano))
      } else {
        paste0("Valor adicionado bruto a preços correntes da indústria (Mil Reais), ",
               input$pib4muni, " x ", input$pib4municomp, " - ", min(pib4$ano), " a ", max(pib4$ano))
      }
    })
    ## Tabela - Valor adicionado bruto a preços correntes da indústria (Mil Reais) da mesma Região de Integração----
    t42 <- reactive({
      ri <- pib4 %>%
        filter(ano == input$pib4ano, localidade == input$pib4muni) %>%
        pull(ri)
      paste0("Valor adicionado bruto a preços correntes da indústria (Mil Reais) dos municípios, Região de Integração ",
             ri, " - ", input$pib4ano)
    })
    # 5 - Valor Adicionado Bruto a Preços Correntes dos Serviços, Exclusive Administração, Saúde e Educação Públicas e Seguridade Social (Mil Reais)----
    ## Gráfico - Valor Adicionado Bruto a Preços Correntes dos Serviços, Exclusive Administração, Saúde e Educação Públicas e Seguridade Social (Mil Reais)----
    t51 <- reactive({
      req(input$pib5municomp)
      if (input$pib5municomp == "Selecione um município") {
        paste0("Valor Adicionado Bruto a Preços Correntes dos Serviços, Exclusive Administração, Saúde e Educação Públicas e Seguridade Social (Mil Reais), ", 
               input$pib5muni, " - ", min(pib5$ano), " a ", max(pib5$ano))
      } else {
        paste0("Valor Adicionado Bruto a Preços Correntes dos Serviços, Exclusive Administração, Saúde e Educação Públicas e Seguridade Social (Mil Reais), ", 
               input$pib5muni, " x ", input$pib5municomp, " - ", min(pib5$ano), " a ", max(pib5$ano))
      }
    })
    ## Tabela - Valor Adicionado Bruto a Preços Correntes dos Serviços, Exclusive Administração, Saúde e Educação Públicas e Seguridade Social (Mil Reais) da mesma Região de Integração----
    t52 <- reactive({
      ri <- pib5 %>%
        filter(ano == input$pib5ano, localidade == input$pib5muni) %>%
        pull(ri)
      paste0("Valor Adicionado Bruto a Preços Correntes dos Serviços, Exclusive Administração, Saúde e Educação Públicas e Seguridade Social (Mil Reais) dos municípios, Região de Integração ",
             ri, " - ", input$pib5ano)
    })
    # 6 - Valor Adicionado Bruto a Preços Correntes da Administração, Saúde e Educação Públicas e Seguridade Social (Mil Reais)----
    ## Gráfico - Valor Adicionado Bruto a Preços Correntes dos Serviços, Exclusive Administração, Saúde e Educação Públicas e Seguridade Social (Mil Reais)----
    t61 <- reactive({
      req(input$pib6municomp)
      if (input$pib6municomp == "Selecione um município") {
        paste0("Valor Adicionado Bruto a Preços Correntes dos Serviços, Exclusive Administração, Saúde e Educação Públicas e Seguridade Social (Mil Reais), ", 
               input$pib6muni, " - ", min(pib6$ano), " a ", max(pib6$ano))
      } else {
        paste0("Valor Adicionado Bruto a Preços Correntes dos Serviços, Exclusive Administração, Saúde e Educação Públicas e Seguridade Social (Mil Reais), ",
               input$pib6muni, " x ", input$pib6municomp, " - ", min(pib6$ano), " a ", max(pib6$ano))
      }
    })
    ## Tabela - Valor Adicionado Bruto a Preços Correntes dos Serviços, Exclusive Administração, Saúde e Educação Públicas e Seguridade Social (Mil Reais) da mesma Região de Integração----
    t62 <- reactive({
      ri <- pib6 %>%
        filter(ano == input$pib6ano, localidade == input$pib6muni) %>%
        pull(ri)
      paste0("Valor Adicionado Bruto a Preços Correntes dos Serviços, Exclusive Administração, Saúde e Educação Públicas e Seguridade Social (Mil Reais) dos municípios, Região de Integração ",
             ri, " - ", input$pib6ano)
    })
    # 7 - Impostos, Líquidos de Subsídios, Sobre Produtos a Preços Correntes (Mil Reais)----
    ## Gráfico - Impostos, Líquidos de Subsídios, Sobre Produtos a Preços Correntes (Mil Reais)----
    t71 <- reactive({
      req(input$pib7municomp)
      if (input$pib7municomp == "Selecione um município") {
        paste0("Impostos, Líquidos de Subsídios, Sobre Produtos a Preços Correntes (Mil Reais), ",
               input$pib7muni, " - ", min(pib7$ano), " a ", max(pib7$ano))
      } else {
        paste0("Impostos, Líquidos de Subsídios, Sobre Produtos a Preços Correntes (Mil Reais), ",
               input$pib7muni, " x ", input$pib7municomp, " - ", min(pib7$ano), " a ", max(pib7$ano))
      }
    })
    ## Tabela - Impostos, Líquidos de Subsídios, Sobre Produtos a Preços Correntes (Mil Reais) da mesma Região de Integração----
    t72 <- reactive({
      ri <- pib7 %>%
        filter(ano == input$pib7ano, localidade == input$pib7muni) %>%
        pull(ri)
      paste0("Impostos, Líquidos de Subsídios, Sobre Produtos a Preços Correntes (Mil Reais) dos municípios, Região de Integração ",
             ri, " - ", input$pib7ano)
    })
    # 8 - Evolução do Produto Interno Bruto per Capita----
    ## Gráfico - Evolução do Produto Interno Bruto per Capita----
    t81 <- reactive({
      req(input$pib8municomp)
      if (input$pib8municomp == "Selecione um município") {
        paste0("Evolução do Produto Interno Bruto per Capita, ",
               input$pib8muni, " - ", min(pib8$ano), " a ", max(pib8$ano))
      } else {
        paste0("Evolução do Produto Interno Bruto per Capita, ",
               input$pib8muni, " x ", input$pib8municomp, " - ", min(pib8$ano), " a ", max(pib8$ano))
      }
    })
    ## Tabela - Produto Interno Bruto per Capita----
    t82 <- reactive({
      ri <- pib8 %>%
        filter(ano == input$pib8ano, localidade == input$pib8muni) %>%
        pull(ri)
      paste0("Produto Interno Bruto per Capita dos municípios, Região de Integração ",
             ri, " - ", input$pib8ano)
    })
    #VISUALIZAÇÃO----
    # 1 - Produto Interno Bruto a Preços Correntes (Mil Reais)----
    ## Gráfico de linha- Produto Interno Bruto a Preços Correntes (Mil Reais)----
    # Atualização da entrada
    pib1comp <- reactive({
      input$pib1muni
    })
    observeEvent(pib1comp(), {
      x <- pib1 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != pib1comp())
      choices <- x$localidade %>% unique()
      updatePickerInput(
        inputId = "pib1municomp", 
        choices = c("Selecione um município", choices), 
        session)
    })
    output$pib1txt1 <- renderText({
      t11()
    })

    output$pib1graf <- renderEcharts4r({
      req(input$pib1municomp)
      if (input$pib1municomp == "Selecione um município") {
        a <- pib1 %>% filter(localidade == input$pib1muni)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f2c94e",
            name = "Valor(em Mil Reais)",
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
            name = "Valor(em Mil Reais)",
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
        a <- pib1 %>% filter(localidade == input$pib1muni)
        b <- pib1 %>% filter(localidade == input$pib1municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$pib1muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$pib1municomp,
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
            name = "Valor(em Mil Reais)",
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
    ## Tabela - Produto Interno Bruto a Preços Correntes (Mil Reais) da mesma Região de Integração----
    output$pib1txt2 <- renderText({
      t12()
    })
    output$pib1tab2 <- renderReactable({
      ris <- pib1 %>%
        filter(ano == input$pib1ano, localidade == input$pib1muni) %>%
        pull(ri)
      x <- pib1 %>% filter(ano == input$pib1ano, localidade != "Pará")
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
          ri = colDef(name = "Região de integração"),
          localidade = colDef(name = "Municípios"),
          Ano = colDef(align = "center"),
          valor = colDef(
            name = "Valor(em Mil Reais)",
            format = colFormat(
              digits = 4,
              separators = T,
              locales = "pt-BR"
            ),
            cell = data_bars(x,
              text_position = "above",
              box_shadow = TRUE,
              align_bars = "right",
              number_fmt = scales::number_format(
                big.mark = ".",
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
    # 2 - Valor Adicionado Bruto a Preços Correntes Total (Mil Reais)----
    ## Gráfico - Valor Adicionado Bruto a Preços Correntes Total (Mil Reais)----
    # Atualização da entrada
    pib2comp <- reactive({
      input$pib2muni
    })
    observeEvent(pib2comp(), {
      x <- pib2 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != pib2comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "pib2municomp", choices = c("Selecione um município", choices), session)
    })

    output$pib2txt1 <- renderText({
      t21()
    })

    output$pib2graf <- renderEcharts4r({
      req(input$pib2municomp)
      if (input$pib2municomp == "Selecione um município") {
        a <- pib2 %>% filter(localidade == input$pib2muni)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f2c94e",
            name = "Valor(em Mil Reais)",
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
            name = "Valor(em Mil Reais)",
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
        a <- pib2 %>% filter(localidade == input$pib2muni)
        b <- pib2 %>% filter(localidade == input$pib2municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$pib2muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$pib2municomp,
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
            name = "Valor(em Mil Reais)",
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
    ## Tabela - Valor Adicionado Bruto a Preços Correntes Total (Mil Reais) da mesma Região de Integração----
    output$pib2txt2 <- renderText({
      t22()
    })
    output$pib2tab2 <- renderReactable({
      ris <- pib2 %>%
        filter(ano == input$pib2ano, localidade == input$pib2muni) %>%
        pull(ri)
      x <- pib2 %>% filter(ano == input$pib2ano, localidade != "Pará")
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
          ri = colDef(name = "Região de integração"),
          localidade = colDef(name = "Municípios"),
          Ano = colDef(align = "center"),
          valor = colDef(
            name = "Valor(em Mil Reais)",
            format = colFormat(
              digits = 4,
              separators = T,
              locales = "pt-BR"
            ),
            cell = data_bars(x,
              text_position = "above",
              box_shadow = TRUE,
              align_bars = "right",
              number_fmt = scales::number_format(
                big.mark = ".",
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

    # 3 - Valor Adicionado Bruto a Preços Correntes da Agropecuária (Mil Reais)----
    ## Gráfico - Valor Adicionado Bruto a Preços Correntes da Agropecuária (Mil Reais)----
    # Atualização da entrada
    pib3comp <- reactive({
      input$pib3muni
    })
    observeEvent(pib3comp(), {
      x <- pib3 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != pib3comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "pib3municomp", choices = c("Selecione um município", choices), session)
    })

    output$pib3txt1 <- renderText({
      t31()
    })

    output$pib3graf <- renderEcharts4r({
      req(input$pib3municomp)
      if (input$pib3municomp == "Selecione um município") {
        a <- pib3 %>% filter(localidade == input$pib3muni)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f2c94e",
            name = "Valor(em Mil Reais)",
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
            name = "Valor(em Mil Reais)",
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
        a <- pib3 %>% filter(localidade == input$pib3muni)
        b <- pib3 %>% filter(localidade == input$pib3municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$pib3muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$pib3municomp,
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
            name = "Valor(em Mil Reais)",
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
    ## Tabela - Valor Adicionado Bruto a Preços Correntes da Agropecuária (Mil Reais) da mesma Região de Integração----
    output$pib3txt2 <- renderText({
      t32()
    })
    output$pib3tab2 <- renderReactable({
      ris <- pib3 %>%
        filter(ano == input$pib3ano, localidade == input$pib3muni) %>%
        pull(ri)
      x <- pib3 %>% filter(ano == input$pib3ano, localidade != "Pará")
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
          ri = colDef(name = "Região de integração"),
          localidade = colDef(name = "Municípios"),
          Ano = colDef(align = "center"),
          valor = colDef(
            name = "Valor(em Mil Reais)",
            format = colFormat(
              digits = 4,
              separators = T,
              locales = "pt-BR"
            ),
            cell = data_bars(x,
              text_position = "above",
              box_shadow = TRUE,
              align_bars = "right",
              number_fmt = scales::number_format(
                big.mark = ".",
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

    # 4 - Valor adicionado bruto a preços correntes da indústria (Mil Reais)----
    ## Gráfico - Valor adicionado bruto a preços correntes da indústria (Mil Reais)----
    # Atualização da entrada
    pib4comp <- reactive({
      input$pib4muni
    })
    observeEvent(pib4comp(), {
      x <- pib4 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != pib4comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "pib4municomp", choices = c("Selecione um município", choices), session)
    })

    output$pib4txt1 <- renderText({
      t41()
    })

    output$pib4graf <- renderEcharts4r({
      req(input$pib4municomp)
      if (input$pib4municomp == "Selecione um município") {
        a <- pib4 %>% filter(localidade == input$pib4muni)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f2c94e",
            name = "Valor(em Mil Reais)",
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
            name = "Valor(em Mil Reais)",
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
        a <- pib4 %>% filter(localidade == input$pib4muni)
        b <- pib4 %>% filter(localidade == input$pib4municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$pib4muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$pib4municomp,
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
            name = "Valor(em Mil Reais)",
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
    ## Tabela - Valor adicionado bruto a preços correntes da indústria (Mil Reais) da mesma Região de Integração----
    output$pib4txt2 <- renderText({
      t42()
    })
    output$pib4tab2 <- renderReactable({
      ris <- pib4 %>%
        filter(ano == input$pib4ano, localidade == input$pib4muni) %>%
        pull(ri)
      x <- pib4 %>% filter(ano == input$pib4ano, localidade != "Pará")
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
          ri = colDef(name = "Região de integração"),
          localidade = colDef(name = "Municípios"),
          Ano = colDef(align = "center"),
          valor = colDef(
            name = "Valor(em Mil Reais)",
            format = colFormat(
              digits = 4,
              separators = T,
              locales = "pt-BR"
            ),
            cell = data_bars(x,
              text_position = "above",
              box_shadow = TRUE,
              align_bars = "right",
              number_fmt = scales::number_format(
                big.mark = ".",
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

    # 5 - Valor Adicionado Bruto a Preços Correntes dos Serviços, Exclusive Administração, Saúde e Educação Públicas e Seguridade Social (Mil Reais)----
    ## Gráfico - Valor Adicionado Bruto a Preços Correntes dos Serviços, Exclusive Administração, Saúde e Educação Públicas e Seguridade Social (Mil Reais)----
    # Atualização da entrada
    pib5comp <- reactive({
      input$pib5muni
    })
    observeEvent(pib5comp(), {
      x <- pib5 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != pib5comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "pib5municomp", choices = c("Selecione um município", choices), session)
    })

    output$pib5txt1 <- renderText({
      t51()
    })

    output$pib5graf <- renderEcharts4r({
      req(input$pib5municomp)
      if (input$pib5municomp == "Selecione um município") {
        a <- pib5 %>% filter(localidade == input$pib5muni)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f2c94e",
            name = "Valor(em Mil Reais)",
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
            name = "Valor(em Mil Reais)",
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
        a <- pib5 %>% filter(localidade == input$pib5muni)
        b <- pib5 %>% filter(localidade == input$pib5municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$pib5muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$pib5municomp,
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
            name = "Valor(em Mil Reais)",
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
    ## Tabela - Valor Adicionado Bruto a Preços Correntes dos Serviços, Exclusive Administração, Saúde e Educação Públicas e Seguridade Social (Mil Reais) da mesma Região de Integração----
    output$pib5txt2 <- renderText({
      t52()
    })
    output$pib5tab2 <- renderReactable({
      ris <- pib5 %>%
        filter(ano == input$pib5ano, localidade == input$pib5muni) %>%
        pull(ri)
      x <- pib5 %>% filter(ano == input$pib5ano, localidade != "Pará")
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
          ri = colDef(name = "Região de integração"),
          localidade = colDef(name = "Municípios"),
          Ano = colDef(align = "center"),
          valor = colDef(
            name = "Valor(em Mil Reais)",
            format = colFormat(
              digits = 4,
              separators = T,
              locales = "pt-BR"
            ),
            cell = data_bars(x,
              text_position = "above",
              box_shadow = TRUE,
              align_bars = "right",
              number_fmt = scales::number_format(
                big.mark = ".",
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

    # 6 - Valor Adicionado Bruto a Preços Correntes da Administração, Saúde e Educação Públicas e Seguridade Social (Mil Reais)----
    ## Gráfico - Valor Adicionado Bruto a Preços Correntes dos Serviços, Exclusive Administração, Saúde e Educação Públicas e Seguridade Social (Mil Reais)----
    # Atualização da entrada
    pib6comp <- reactive({
      input$pib6muni
    })
    observeEvent(pib6comp(), {
      x <- pib6 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != pib6comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "pib6municomp", choices = c("Selecione um município", choices), session)
    })

    output$pib6txt1 <- renderText({
      t61()
    })

    output$pib6graf <- renderEcharts4r({
      req(input$pib6municomp)
      if (input$pib6municomp == "Selecione um município") {
        a <- pib6 %>% filter(localidade == input$pib6muni)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f2c94e",
            name = "Valor(em Mil Reais)",
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
            name = "Valor(em Mil Reais)",
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
        a <- pib6 %>% filter(localidade == input$pib6muni)
        b <- pib6 %>% filter(localidade == input$pib6municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$pib6muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$pib6municomp,
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
            name = "Valor(em Mil Reais)",
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
    ## Tabela - Valor Adicionado Bruto a Preços Correntes dos Serviços, Exclusive Administração, Saúde e Educação Públicas e Seguridade Social (Mil Reais) da mesma Região de Integração----
    output$pib6txt2 <- renderText({
      t62()
    })
    output$pib6tab2 <- renderReactable({
      ris <- pib6 %>%
        filter(ano == input$pib6ano, localidade == input$pib6muni) %>%
        pull(ri)
      x <- pib6 %>% filter(ano == input$pib6ano, localidade != "Pará")
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
          ri = colDef(name = "Região de integração"),
          localidade = colDef(name = "Municípios"),
          Ano = colDef(align = "center"),
          valor = colDef(
            name = "Valor(em Mil Reais)",
            format = colFormat(
              digits = 4,
              separators = T,
              locales = "pt-BR"
            ),
            cell = data_bars(x,
              text_position = "above",
              box_shadow = TRUE,
              align_bars = "right",
              number_fmt = scales::number_format(
                big.mark = ".",
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

    # 7 - Impostos, Líquidos de Subsídios, Sobre Produtos a Preços Correntes (Mil Reais)----
    ## Gráfico - Impostos, Líquidos de Subsídios, Sobre Produtos a Preços Correntes (Mil Reais)----
    # Atualização da entrada
    pib7comp <- reactive({
      input$pib7muni
    })
    observeEvent(pib7comp(), {
      x <- pib7 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != pib7comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "pib7municomp", choices = c("Selecione um município", choices), session)
    })

    output$pib7txt1 <- renderText({
      t71()
    })

    output$pib7graf <- renderEcharts4r({
      req(input$pib7municomp)
      if (input$pib7municomp == "Selecione um município") {
        a <- pib7 %>% filter(localidade == input$pib7muni)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f2c94e",
            name = "Valor(em Mil Reais)",
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
            name = "Valor(em Mil Reais)",
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
        a <- pib7 %>% filter(localidade == input$pib7muni)
        b <- pib7 %>% filter(localidade == input$pib7municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$pib7muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$pib7municomp,
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
            name = "Valor(em Mil Reais)",
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
    ## Tabela - Impostos, Líquidos de Subsídios, Sobre Produtos a Preços Correntes (Mil Reais) da mesma Região de Integração----
    output$pib7txt2 <- renderText({
      t72()
    })
    output$pib7tab2 <- renderReactable({
      ris <- pib7 %>%
        filter(ano == input$pib7ano, localidade == input$pib7muni) %>%
        pull(ri)
      x <- pib7 %>% filter(ano == input$pib7ano, localidade != "Pará")
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
          ri = colDef(name = "Região de integração"),
          localidade = colDef(name = "Municípios"),
          Ano = colDef(align = "center"),
          valor = colDef(
            name = "Valor(em Mil Reais)",
            format = colFormat(
              digits = 4,
              separators = T,
              locales = "pt-BR"
            ),
            cell = data_bars(x,
              text_position = "above",
              box_shadow = TRUE,
              align_bars = "right",
              number_fmt = scales::number_format(
                big.mark = ".",
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

    # 8 - Evolução do Produto Interno Bruto per Capita----
    ## Gráfico - Evolução do Produto Interno Bruto per Capita----
    # Atualização da entrada
    pib8comp <- reactive({
      input$pib8muni
    })
    observeEvent(pib8comp(), {
      x <- pib8 %>% filter(localidade != "Pará")
      x <- x %>% filter(localidade != pib8comp())

      choices <- x$localidade %>% unique()
      updatePickerInput(inputId = "pib8municomp", choices = c("Selecione um município", choices), session)
    })

    output$pib8txt1 <- renderText({
      t81()
    })
    output$pib8graf <- renderEcharts4r({
      req(input$pib8municomp)
      if (input$pib8municomp == "Selecione um município") {
        a <- pib8 %>% filter(localidade == input$pib8muni)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#f2c94e",
            name = "Valor(em Mil Reais)",
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
            name = "Valor(em Mil Reais)",
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
        a <- pib8 %>% filter(localidade == input$pib8muni)
        b <- pib8 %>% filter(localidade == input$pib8municomp)
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$pib8muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$pib8municomp,
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
            name = "Valor(em Mil Reais)",
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
    ## Tabela - Produto Interno Bruto per Capita----
    output$pib8txt2 <- renderText({
      t82()
    })
    output$pib8tab2 <- renderReactable({
      ris <- pib8 %>%
        filter(ano == input$pib8ano, localidade == input$pib8muni) %>%
        pull(ri)
      x <- pib8 %>% filter(ano == input$pib8ano, localidade != "Pará")
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
          ri = colDef(name = "Região de integração"),
          localidade = colDef(name = "Municípios"),
          Ano = colDef(align = "center"),
          valor = colDef(
            name = "Valor(em Mil Reais)",
            format = colFormat(
              digits = 4,
              separators = T,
              locales = "pt-BR"
            ),
            cell = data_bars(x,
              text_position = "above",
              box_shadow = TRUE,
              align_bars = "right",
              number_fmt = scales::number_format(
                big.mark = ".",
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
    #DOWNLOADS----
    # 1 - Produto Interno Bruto a Preços Correntes (Mil Reais)----
    ## - Gráfico - Produto Interno Bruto a Preços Correntes (Mil Reais) no município----
    # Filtra os dados
    pib1_1 <- reactive({
      req(input$pib1municomp)
      if (input$pib1municomp == "Selecione um município") {
        a <- pib1 %>% filter(localidade == input$pib1muni)
        } else {
        a <- pib1 %>% filter(localidade == input$pib1muni)
        b <- pib1 %>% filter(localidade == input$pib1municomp)
        df <- rbind(a,b)}
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pib1_1(), {
      t11()
      downset_Server("pib1_1", pib1_1(), t11())
    })
    ## - Tabela - Produto Interno Bruto a Preços Correntes (Mil Reais) nos municípios----
    # Filtra os dados
    pib1_2 <- reactive({
      ris <- pib1 %>%
        filter(ano == input$pib1ano, localidade == input$pib1muni) %>%
        pull(ri)
      x <- pib1 %>% 
        filter(ano == input$pib1ano, 
               localidade != "Pará",
               ri == ris
               ) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pib1_2(), {
      t12()
      downset_Server("pib1_2", pib1_2(), t12())
    })

    # 2 - Valor Adicionado Bruto a Preços Correntes Total (Mil Reais)----
    ## - Gráfico - Valor Adicionado Bruto a Preços Correntes Total (Mil Reais) no município----
    # Filtra os dados
    pib2_1 <- reactive({
      req(input$pib2municomp)
      if (input$pib2municomp == "Selecione um município") {
        a <- pib2 %>% filter(localidade == input$pib2muni)
        } else {
        a <- pib2 %>% filter(localidade == input$pib2muni)
        b <- pib2 %>% filter(localidade == input$pib2municomp)
        df <- rbind(a,b)}
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pib2_1(), {
      t21()
      downset_Server("pib2_1", pib2_1(), t21())
    })
    ## - Tabela - Valor Adicionado Bruto a Preços Correntes Total (Mil Reais) nos municípios----
    # Filtra os dados
    pib2_2 <- reactive({
      ris <- pib2 %>%
        filter(ano == input$pib2ano, localidade == input$pib2muni) %>%
        pull(ri)
      x <- pib2 %>% 
        filter(ano == input$pib2ano, 
               localidade != "Pará",
               ri == ris
               ) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pib2_2(), {
     t22()
      downset_Server("pib2_2", pib2_2(), t22())
    })

    # 3 - Valor Adicionado Bruto a Preços Correntes da Agropecuária (Mil Reais)----
    ## - Gráfico - Valor Adicionado Bruto a Preços Correntes da Agropecuária (Mil Reais) no município----
    # Filtra os dados
    pib3_1 <- reactive({
      req(input$pib3municomp)
      if (input$pib3municomp == "Selecione um município") {
        a <- pib3 %>% filter(localidade == input$pib3muni)
        } else {
        a <- pib3 %>% filter(localidade == input$pib3muni)
        b <- pib3 %>% filter(localidade == input$pib3municomp)
        df <- rbind(a,b)}
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pib3_1(), {
      t31()
      downset_Server("pib3_1", pib3_1(), t31())
    })
    ## - Tabela - Valor Adicionado Bruto a Preços Correntes da Agropecuária (Mil Reais) nos municípios----
    # Filtra os dados
    pib3_2 <- reactive({
      ris <- pib3 %>%
        filter(ano == input$pib3ano, localidade == input$pib3muni) %>%
        pull(ri)
      x <- pib3 %>% 
        filter(ano == input$pib3ano, 
               localidade != "Pará",
               ri == ris
               ) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pib3_2(), {
      t32()
      downset_Server("pib3_2", pib3_2(), t32())
    })

    # 4 - Valor adicionado bruto a preços correntes da indústria (Mil Reais)----
    ## - Gráfico - Valor adicionado bruto a preços correntes da indústria (Mil Reais) no município----
    # Filtra os dados
    pib4_1 <- reactive({
      req(input$pib4municomp)
      if (input$pib4municomp == "Selecione um município") {
        a <- pib4 %>% filter(localidade == input$pib4muni)
        } else {
        a <- pib4 %>% filter(localidade == input$pib4muni)
        b <- pib4 %>% filter(localidade == input$pib4municomp)
        df <- rbind(a,b)}
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pib4_1(), {
      t41()
      downset_Server("pib4_1", pib4_1(), t41())
    })
    ## - Tabela - Valor adicionado bruto a preços correntes da indústria (Mil Reais) nos municípios----
    # Filtra os dados
    pib4_2 <- reactive({
      ris <- pib4 %>%
        filter(ano == input$pib4ano, localidade == input$pib4muni) %>%
        pull(ri)
      x <- pib4 %>% 
        filter(ano == input$pib4ano, 
               localidade != "Pará",
               ri == ris
               ) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pib4_2(), {
     t42()
      downset_Server("pib4_2", pib4_2(), t42())
    })

    # 5 - Valor Adicionado Bruto a Preços Correntes dos Serviços, Exclusive Administração, Saúde e Educação Públicas e Seguridade Social (Mil Reais)----
    ## - Gráfico - Valor Adicionado Bruto a Preços Correntes dos Serviços, Exclusive Administração, Saúde e Educação Públicas e Seguridade Social (Mil Reais)----
    # Filtra os dados
    pib5_1 <- reactive({
      req(input$pib5municomp)
      if (input$pib5municomp == "Selecione um município") {
        a <- pib5 %>% filter(localidade == input$pib5muni)
        } else {
        a <- pib5 %>% filter(localidade == input$pib5muni)
        b <- pib5 %>% filter(localidade == input$pib5municomp)
        df <- rbind(a,b)}
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pib5_1(), {
      t51()
      downset_Server("pib5_1", pib5_1(), t51())
    })
    ## - Tabela - Valor Adicionado Bruto a Preços Correntes dos Serviços, Exclusive Administração, Saúde e Educação Públicas e Seguridade Social (Mil Reais)----
    # Filtra os dados
    pib5_2 <- reactive({
      ris <- pib5 %>%
        filter(ano == input$pib5ano, localidade == input$pib5muni) %>%
        pull(ri)
      x <- pib5 %>% 
        filter(ano == input$pib5ano,
               localidade != "Pará",
               ri == ris
               ) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pib5_2(), {
      t52()
      downset_Server("pib5_2", pib5_2(), t52())
    })

    # 6 - Valor Adicionado Bruto a Preços Correntes da Administração, Saúde e Educação Públicas e Seguridade Social (Mil Reais)----
    ## - Gráfico - Valor Adicionado Bruto a Preços Correntes dos Serviços, Exclusive Administração, Saúde e Educação Públicas e Seguridade Social----
    # Filtra os dados
    pib6_1 <- reactive({
      req(input$pib6municomp)
      if (input$pib6municomp == "Selecione um município") {
        a <- pib6 %>% filter(localidade == input$pib6muni)
        } else {
        a <- pib6 %>% filter(localidade == input$pib6muni)
        b <- pib6 %>% filter(localidade == input$pib6municomp)
        df <- rbind(a,b)}
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pib6_1(), {
     t61()
      downset_Server("pib6_1", pib6_1(), tt61())
    })
    ## - Tabela - Valor Adicionado Bruto a Preços Correntes dos Serviços, Exclusive Administração, Saúde e Educação Públicas e Seguridade Social----
    # Filtra os dados
    pib6_2 <- reactive({
      ris <- pib6 %>%
        filter(ano == input$pib6ano, localidade == input$pib6muni) %>%
        pull(ri)
      x <- pib6 %>% 
        filter(ano == input$pib6ano,
               localidade != "Pará",
               ri == ris
               ) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pib6_2(), {
     t62()
      downset_Server("pib6_2", pib6_2(), t62())
    })

    # 7 - Impostos, Líquidos de Subsídios, Sobre Produtos a Preços Correntes (Mil Reais)----
    ## - Gráfico - Impostos, Líquidos de Subsídios, Sobre Produtos a Preços Correntes (Mil Reais) no município----
    # Filtra os dados
    pib7_1 <- reactive({
      req(input$pib7municomp)
      if (input$pib7municomp == "Selecione um município") {
        a <- pib7 %>% filter(localidade == input$pib7muni)
        } else {
        a <- pib7 %>% filter(localidade == input$pib7muni)
        b <- pib7 %>% filter(localidade == input$pib7municomp)
        df <- rbind(a,b)}
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pib7_1(), {
      t71()
      downset_Server("pib7_1", pib7_1(), t71())
    })
    ## - Tabela - Impostos, Líquidos de Subsídios, Sobre Produtos a Preços Correntes (Mil Reais) nos municípios----
    # Filtra os dados
    pib7_2 <- reactive({
      ris <- pib7 %>%
        filter(ano == input$pib7ano, localidade == input$pib7muni) %>%
        pull(ri)
      x <- pib7 %>% 
        filter(ano == input$pib7ano,
               localidade != "Pará",
               ri == ris
               ) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pib7_2(), {
      t72()
      downset_Server("pib7_2", pib7_2(), t72())
    })

    # 8 - Evolução do Produto Interno Bruto per Capita----
    ## - Gráfico - Evolução do Produto Interno Bruto per Capita no município----
    # Filtra os dados
    pib8_1 <- reactive({
      req(input$pib8municomp)
      if (input$pib8municomp == "Selecione um município") {
        a <- pib8 %>% filter(localidade == input$pib8muni)
        } else {
        a <- pib8 %>% filter(localidade == input$pib8muni)
        b <- pib8 %>% filter(localidade == input$pib8municomp)
        df <- rbind(a,b)}
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pib8_1(), {
      t81()
      downset_Server("pib8_1", pib8_1(), t81())
    })
    ## - Tabela - Produto Interno Bruto per Capita nos municípios----
    # Filtra os dados
    pib8_2 <- reactive({
      ris <- pib8 %>%
        filter(ano == input$pib8ano, localidade == input$pib8muni) %>%
        pull(ri)
      x <- pib8 %>% 
        filter(ano == input$pib8ano,
               localidade != "Pará",
               ri == ris
               ) %>%
        arrange(desc(valor))
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(pib8_2(), {
      t82()
      downset_Server("pib8_2", pib8_2(), t82())
    })
  })
}

# Play do Módulo
# ui <- dashboardPage(
#   header = dashboardHeader(),
#   sidebar = dashboardSidebar(),
#   body = dashboardBody(fluidPage(economia_pib_mp_ui("economia_pib_mp")))
# )
# 
# 
# server <- function(input, output) {
#   economia_pib_mp_Server("economia_pib_mp")
# }
# 
# shinyApp(ui, server)
