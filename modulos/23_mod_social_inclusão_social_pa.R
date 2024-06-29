# Funções de módulo de Social - Inclusão Social - Estadual
# Função de UI
social_inclusao_social_pa_ui <- function(id) {
  fluidPage(
    # CAMINHO DO ARQUIVO CSS----
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "estilo.css")
    ),
    div(
      class = "navbar_social",
      navbarPage(
        tags$b("Inclusão Social - Pará"),
        navbarMenu(
          "Indicadores",
          # 1 - Famílias Atendidas e Valor Total Empregado no Programa Bolsa Família----
          tabPanel(
            "Famílias Atendidas e Valor Total Empregado no Programa Bolsa Família",
            panel(
              ## Controle----
              heading =
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Famílias Atendidas e Valor Total Empregado no Programa Bolsa Família"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "inc1ano"),
                  label = "Ano",
                  choices = sort(unique(inc1[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "inc1ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(inc1[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Famílias Atendidas no Programa Bolsa Família----
              box(
                title = textOutput(NS(id, "inc1txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "inc1map1"), height = "600px"),
                  type = 8,
                  color = "#f07701",
                  size = 0.5
                ),
                footer = list(
                  tags$h6(tags$b("Fonte:"), "SAGI/MDS"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                )
              ),
              ## Mapa - Valor Total Empregado no Programa Bolsa Família----
              box(
                title = textOutput(NS(id, "inc1txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "inc1map2"), height = "600px"),
                  type = 8,
                  color = "#f07701",
                  size = 0.5
                ),
                footer = list(
                  tags$h6(tags$b("Fonte:"), "SAGI/MDS"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                )
              )
            ),
            fluidRow(
              ## Tabela - Famílias Atendidas no Programa Bolsa Família----
              box(
                title = textOutput(NS(id, "inc1txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "inc1tab"), height = "400px"),
                  type = 8,
                  color = "#f07701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "SAGI/MDS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "inc1_3"))
                )
              ),
              ## Gráfico - Valor Total Empregado no Programa Bolsa Família----
              box(
                title = textOutput(NS(id, "inc1txt4")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "inc1graf")),
                  type = 8,
                  color = "#f07701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "SAGI/MDS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "inc1_4"))
                )
              )
            )
          ),
          # 2 - Famílias Atendidas e Valor Total Empregado no Programa Auxílio Brasil----
          tabPanel(
            "Famílias Atendidas e Valor Total Empregado no Programa Auxílio Brasil",
            panel(
              ## Controle----
              heading =
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Famílias Atendidas e Valor Total Empregado no Programa Auxílio Brasil"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "inc2ano"),
                  label = "Ano",
                  choices = sort(unique(inc2[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "inc2ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(inc2[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Famílias Atendidas no Programa Auxílio Brasil----
              box(
                title = textOutput(NS(id, "inc2txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "inc2map1"), height = "600px"),
                  type = 8,
                  color = "#f07701",
                  size = 0.5
                ),
                footer = list(
                  tags$h6(tags$b("Fonte:"), "SAGI/MDS"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                )
              ),
              ## Mapa - Valor Total Empregado no Programa Auxílio Brasil----
              box(
                title = textOutput(NS(id, "inc2txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "inc2map2"), height = "600px"),
                  type = 8,
                  color = "#f07701",
                  size = 0.5
                ),
                footer = list(
                  tags$h6(tags$b("Fonte:"), "SAGI/MDS"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                )
              )
            ),
            fluidRow(
              ## Tabela - Famílias Atendidas no Programa Auxílio Brasil----
              box(
                title = textOutput(NS(id, "inc2txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "inc2tab"), height = "400px"),
                  type = 8,
                  color = "#f07701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "SAGI/MDS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "inc2_3"))
                )
              ),
              ## Gráfico - Valor Total Empregado no Programa Auxílio Brasil----
              box(
                title = textOutput(NS(id, "inc2txt4")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "inc2graf")),
                  type = 8,
                  color = "#f07701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "SAGI/MDS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "inc2_4"))
                )
              )
            )
          ),
          # 3 - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico)----
          tabPanel(
            "Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico)",
            panel(
              ## Controle----
              heading =
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico)"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "inc3ano"),
                  label = "Ano",
                  choices = sort(unique(inc3[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "inc3ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(inc3[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico)----
              box(
                title = textOutput(NS(id, "inc3txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "inc3map"), height = "600px"),
                  type = 8,
                  color = "#f07701",
                  size = 0.5
                ),
                footer = list(
                  tags$h6(tags$b("Fonte:"), "SAGI/MDS"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                )
              ),
              ## Tabela - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico)----
              box(
                title = textOutput(NS(id, "inc3txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "inc3tab"), height = "400px"),
                  type = 8,
                  color = "#f07701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "SAGI/MDS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "inc3_2"))
                )
              )
            ),
            fluidRow(
              ## Gráfico de Linha - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico)----
              box(
                title = textOutput(NS(id, "inc3txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "inc3graf")),
                  type = 8,
                  color = "#f07701",
                  size = 1
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "SAGI/MDS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "inc3_3"))
                )
              )
            )
          ),
          # 4 - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico) com rendimento familiar per capita de até 1/2 salário mínimo----
          tabPanel(
            "Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico) com rendimento familiar per capita de até 1/2 salário mínimo",
            panel(
              ## Controle----
              heading =
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico) com rendimento familiar per capita de até 1/2 salário mínimo"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "inc4ano"),
                  label = "Ano",
                  choices = sort(unique(inc4[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2",
                pickerInput(
                  inputId = NS(id, "inc4ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(inc4[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ## Mapa - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico) com rendimento familiar per capita de até 1/2 salário mínimo----
              box(
                title = textOutput(NS(id, "inc4txt1")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  leafletOutput(NS(id, "inc4map"), height = "600px"),
                  type = 8,
                  color = "#f07701",
                  size = 0.5
                ),
                footer = list(
                  tags$h6(tags$b("Fonte:"), "SAGI/MDS"),
                  tags$h6(tags$b("Elaboração:"), "FAPESPA")
                )
              ),
              ## Tabela - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico) com rendimento familiar per capita de até 1/2 salário mínimo----
              box(
                title = textOutput(NS(id, "inc4txt2")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  reactableOutput(NS(id, "inc4tab"), height = "400px"),
                  type = 8,
                  color = "#f07701",
                  size = 0.5
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "SAGI/MDS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "inc4_2"))
                )
              )
            ),
            fluidRow(
              ## Gráfico de Linha - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico) com rendimento familiar per capita de até 1/2 salário mínimo----
              box(
                title = textOutput(NS(id, "inc4txt3")),
                collapsible = T,
                collapsed = F,
                headerBorder = T,
                width = 12,
                withSpinner(
                  echarts4rOutput(NS(id, "inc4graf")),
                  type = 8,
                  color = "#f07701",
                  size = 1
                ),
                footer = list(
                  column(
                    11,
                    tags$h6(tags$b("Fonte:"), "SAGI/MDS"),
                    tags$h6(tags$b("Elaboração:"), "FAPESPA")
                  ),
                  downset_ui(NS(id, "inc4_3"))
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
social_inclusao_social_pa_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    #TÍTULO----
    # 1 - Famílias Atendidas e Valor Total Empregado no Programa Bolsa Família----
    ## Mapa - Famílias Atendidas e no Programa Bolsa Família----
    t11 <- reactive({
      if (input$inc1ri == "Pará") {
        paste0(
          "Famílias Atendidas no Programa Bolsa Família, Pará - ",
          input$inc1ano
        )
      } else{
        paste0(
          "Famílias Atendidas no Programa Bolsa Família, Região de Integração ",
          input$inc1ri,
          " - ",
          input$inc1ano
        )
      }
    })
    
    ## Mapa - Valor Total Empregado no Programa Bolsa Família----
    t12 <- reactive({
      if (input$inc1ri == "Pará") {
        paste0(
          "Valor Total Empregado no Programa Bolsa Família, Pará - ",
          input$inc1ano
        )
      } else{
        paste0(
          "Valor Total Empregado no Programa Bolsa Família, Região de Integração ",
          input$inc1ri,
          " - ",
          input$inc1ano
        )
      }
    })
    
    ## Tabela - Famílias Atendidas e no Programa Bolsa Família----
    t13 <- reactive({
      if (input$inc1ri == "Pará") {
        paste0(
          "Famílias Atendidas e Valor Total Empregado no Programa Bolsa Família por Município, Pará - ",
          input$inc1ano
        )
      } else{
        paste0(
          "Famílias Atendidas e Valor Total Empregado no Programa Bolsa Família por Município, Região de Integração ",
          input$inc1ri,
          " - ",
          input$inc1ano
        )
      }
    })
    
    ## Gráfico de linha - Famílias Atendidas e no Programa Bolsa Família----
    t14 <- reactive({
      paste0(" Famílias Atendidas e Valor Total Empregado no Programa Bolsa Família, Pará - ",
             min(inc1$ano), " a ", max(inc1$ano))
    })
    # 2 - Famílias Atendidas e Valor Total Empregado no Programa Auxílio Brasil----
    ## Mapa - Famílias Atendidas e Valor Total Empregado no Programa Auxílio Brasil----
    t21 <- reactive({
      if (input$inc2ri == "Pará") {
        paste0(
          "Famílias Atendidas no Programa Auxílio Brasil, Pará - ",
          input$inc2ano
        )
      } else{
        paste0(
          "Famílias Atendidas no Programa Auxílio Brasil, Região de Integração ",
          input$inc2ri,
          " - ",
          input$inc2ano
        )
      }
    })
    
    ## Mapa - Valor Total Empregado no Programa Auxílio Brasil----
    t22 <- reactive({
      if (input$inc2ri == "Pará") {
        paste0(
          "Valor Total Empregado no Programa Auxílio Brasil, Pará - ",
          input$inc2ano
        )
      } else{
        paste0(
          "Valor Total Empregado no Programa Auxílio Brasil, Região de Integração ",
          input$inc2ri,
          " - ",
          input$inc2ano
        )
      }
    })
    
    ## Tabela - Famílias Atendidas e Valor Total Empregado no Programa Auxílio Brasil----
    t23 <- reactive({
      if (input$inc2ri == "Pará") {
        paste0(
          "Famílias Atendidas e Valor Total Empregado no Programa Auxílio Brasil por Município, Pará - ",
          input$inc2ano
        )
      } else{
        paste0(
          "Famílias Atendidas e Valor Total Empregado no Programa Auxílio Brasil por Município, Região de Integração ",
          input$inc2ri,
          " - ",
          input$inc2ano
        )
      }
    })
    
    ## Gráfico de linha - Famílias Atendidas e Valor Total Empregado no Programa Auxílio Brasil----
    t24 <- reactive({
      paste0(" Famílias Atendidas e Valor Total Empregado no Programa Auxílio Brasil, Pará - ",
             min(inc2$ano), " a ", max(inc2$ano))
    })
    # 3 - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico)----
    ## Mapa - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico)----
    t31 <- reactive({
      if (input$inc3ri == "Pará") {
        paste0(
          "Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico), Pará - ",
          input$inc3ano
        )
      } else{
        paste0(
          "Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico), Região de Integração ",
          input$inc3ri,
          " - ",
          input$inc3ano
        )
      }
    })
    
    ## Tabela - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico)----
    t32 <- reactive({
      if (input$inc3ri == "Pará") {
        paste0(
          "Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico) por Município, Pará - ",
          input$inc3ano
        )
      } else{
        paste0(
          "Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico) por Município, Região de Integração ",
          input$inc3ri,
          " - ",
          input$inc3ano
        )
      }
    })
    
    ## Gráfico de Linha - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico)----
    t33 <- reactive({
      paste0("Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico), Pará - ",
             min(inc3$ano), " a ", max(inc3$ano))
    })
    # 4 - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico) com rendimento familiar per capita de até 1/2 salário mínimo----
    ## Mapa - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico) com rendimento familiar per capita de até 1/2 salário mínimo----
    t41 <- reactive({
      if (input$inc4ri == "Pará") {
        paste0(
          "Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico) com rendimento familiar per capita de até 1/2 salário mínimo, Pará - ",
          input$inc4ano
        )
      } else{
        paste0(
          "Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico) com rendimento familiar per capita de até 1/2 salário mínimo, Região de Integração ",
          input$inc4ri,
          " - ",
          input$inc4ano
        )
      }
    })
    
    ## Tabela - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico) com rendimento familiar per capita de até 1/2 salário mínimo----
    t42 <- reactive({
      if (input$inc4ri == "Pará") {
        paste0(
          "Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico) com rendimento familiar per capita de até 1/2 salário mínimo por Município, Pará - ",
          input$inc4ano
        )
      } else{
        paste0(
          "Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico) com rendimento familiar per capita de até 1/2 salário mínimo por Município, Região de Integração ",
          input$inc4ri,
          " - ",
          input$inc4ano
        )
      }    })
    
    ## Gráfico de Linha - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico) com rendimento familiar per capita de até 1/2 salário mínimo----
    t43 <- reactive({
      paste0("Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico) com rendimento familiar per capita de até 1/2 salário mínimo, Pará - ",
             min(inc4$ano), " a ", max(inc4$ano))
    })

    #VISUALIZAÇÃO----
    # 1 - Famílias Atendidas e Valor Total Empregado no Programa Bolsa Família----
    ## Mapa - Famílias Atendidas e no Programa Bolsa Família----
    output$inc1txt1 <- renderText({
      t11()
    })

    output$inc1map1 <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$inc1ri == "Pará") {
        df <- inc1 %>%
          filter(localidade != "Pará", ano == input$inc1ano, categoria == "Famílias") %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x$valor[x$valor == 0] <- NA
      } else {
        df <- inc1 %>%
          filter(localidade != "Pará", ano == input$inc1ano, categoria == "Famílias") %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$inc1ri)
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
          "<strong>%s</strong><br/> <b>Nº de Famílias:</b> %s",
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
          title = "Nº de Famílias",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 2)
        )
    })
    ## Mapa - Valor Total Empregado no Programa Bolsa Família----
    output$inc1txt2 <- renderText({
      t12()
    })

    output$inc1map2 <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$inc1ri == "Pará") {
        df <- inc1 %>%
          filter(localidade != "Pará", ano == input$inc1ano, categoria == "Valor Total") %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x$valor[x$valor == 0] <- NA
      } else {
        df <- inc1 %>%
          filter(localidade != "Pará", ano == input$inc1ano, categoria == "Valor Total") %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$inc1ri)
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
          "<strong>%s</strong><br/> <b>Valor Total:</b> %s",
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
          title = "Valor Total",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 2)
        )
    })
    ## Tabela - Famílias Atendidas e no Programa Bolsa Família----
    output$inc1txt3 <- renderText({
      t13()
    })

    output$inc1tab <- renderReactable({
      if (input$inc1ri == "Pará") {
        x <- inc1 %>%
          filter(localidade != "Pará", ano == input$inc1ano) %>%
          select(ri, localidade, categoria, valor) %>%
          pivot_wider(names_from = categoria, values_from = valor)
      } else {
        x <- inc1 %>%
          filter(localidade != "Pará", ano == input$inc1ano) %>%
          select(ri, localidade, categoria, valor) %>%
          pivot_wider(names_from = categoria, values_from = valor)
        x <- x %>% filter(ri == input$inc1ri)
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
          ri = colDef(name = "Região de Integração"),
          localidade = colDef(name = "Municípios"),
          Famílias = colDef(format = colFormat(separators = T, locales = "PT")),
          `Valor Total` = colDef(name = "Valor Total(R$)", format = colFormat(separators = T, locales = "PT"))
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

    ## Gráfico de linha - Famílias Atendidas e no Programa Bolsa Família----
    output$inc1txt4 <- renderText({
      t14()
    })
    output$inc1graf <- renderEcharts4r({
      inc1 %>%
        filter(localidade == "Pará") %>%
        pivot_wider(names_from = categoria, values_from = valor) %>%
        e_chart(x = ano) %>%
        e_line(
          serie = Famílias,
          name = "Nº Famílias",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
          itemStyle = list(barBorderRadius = 5)
        ) %>%
        e_line(
          y_index = 1,
          serie = `Valor Total`,
          name = "Valor Total",
          legend = T,
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
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = 14,
            padding = c(0, 0, 0, 20),
            verticalAlign = "top",
            lineHeight = 70
          )
        ) %>%
        e_y_axis(
          name = "Nº de Famílias",
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
        e_y_axis(
          name = "Valor Total(R$)",
          index = 1,
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
    # 2 - Famílias Atendidas e Valor Total Empregado no Programa Auxílio Brasil----
    ## Mapa - Famílias Atendidas e Valor Total Empregado no Programa Auxílio Brasil----
    output$inc2txt1 <- renderText({
      t21()
    })
    
    output$inc2map1 <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$inc2ri == "Pará") {
        df <- inc2 %>%
          filter(localidade != "Pará", ano == input$inc2ano, categoria == "Famílias") %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x$valor[x$valor == 0] <- NA
      } else {
        df <- inc2 %>%
          filter(localidade != "Pará", ano == input$inc2ano, categoria == "Famílias") %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$inc2ri)
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
          "<strong>%s</strong><br/> <b>Nº de Famílias:</b> %s",
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
          title = "Nº de Famílias",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 2)
        )
    })
    ## Mapa - Valor Total Empregado no Programa Auxílio Brasil----
    output$inc2txt2 <- renderText({
      t22()
    })
    
    output$inc2map2 <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$inc2ri == "Pará") {
        df <- inc2 %>%
          filter(localidade != "Pará", ano == input$inc2ano, categoria == "Valor Total") %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x$valor[x$valor == 0] <- NA
      } else {
        df <- inc2 %>%
          filter(localidade != "Pará", ano == input$inc2ano, categoria == "Valor Total") %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$inc2ri)
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
          "<strong>%s</strong><br/> <b>Valor Total:</b> %s",
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
          title = "Valor Total",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 2)
        )
    })
    ## Tabela - Famílias Atendidas e Valor Total Empregado no Programa Auxílio Brasil----
    output$inc2txt3 <- renderText({
      t23()
    })
    
    output$inc2tab <- renderReactable({
      if (input$inc2ri == "Pará") {
        x <- inc2 %>%
          filter(localidade != "Pará", ano == input$inc2ano) %>%
          select(ri, localidade, categoria, valor) %>%
          pivot_wider(names_from = categoria, values_from = valor)
      } else {
        x <- inc2 %>%
          filter(localidade != "Pará", ano == input$inc2ano) %>%
          select(ri, localidade, categoria, valor) %>%
          pivot_wider(names_from = categoria, values_from = valor)
        x <- x %>% filter(ri == input$inc2ri)
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
          ri = colDef(name = "Região de Integração"),
          localidade = colDef(name = "Municípios"),
          Famílias = colDef(format = colFormat(separators = T, locales = "PT")),
          `Valor Total` = colDef(name = "Valor Total(R$)", format = colFormat(separators = T, locales = "PT"))
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
    
    ## Gráfico de linha - Famílias Atendidas e Valor Total Empregado no Programa Auxílio Brasil----
    output$inc2txt4 <- renderText({
      t24()
    })
    output$inc2graf <- renderEcharts4r({
      inc2 %>%
        filter(localidade == "Pará") %>%
        pivot_wider(names_from = categoria, values_from = valor) %>%
        e_chart(x = ano) %>%
        e_line(
          serie = Famílias,
          name = "Nº Famílias",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
          itemStyle = list(barBorderRadius = 5)
        ) %>%
        e_line(
          y_index = 1,
          serie = `Valor Total`,
          name = "Valor Total",
          legend = T,
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
          nameTextStyle = list(
            fontWeight = "bold",
            fontSize = 14,
            padding = c(0, 0, 0, 20),
            verticalAlign = "top",
            lineHeight = 70
          )
        ) %>%
        e_y_axis(
          name = "Nº de Famílias",
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
        e_y_axis(
          name = "Valor Total(R$)",
          index = 1,
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
    # 3 - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico)----
    ## Mapa - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico)----
    output$inc3txt1 <- renderText({
      t31()
    })

    output$inc3map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$inc3ri == "Pará") {
        df <- inc3 %>%
          filter(localidade != "Pará", ano == input$inc3ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x$valor[x$valor == 0] <- NA
      } else {
        df <- inc3 %>%
          filter(localidade != "Pará", ano == input$inc3ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$inc3ri)
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
          "<strong>%s</strong><br/> <b>Nº de Famílias:</b> %s",
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
          title = "Nº de Famílias",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 2)
        )
    })
    ## Tabela - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico)----
    output$inc3txt2 <- renderText({
      t32()
    })

    output$inc3tab <- renderReactable({
      if (input$inc3ri == "Pará") {
        x <- inc3 %>%
          filter(localidade != "Pará", ano == input$inc3ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- inc3 %>%
          filter(localidade != "Pará", ano == input$inc3ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$inc3ri)
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
          ri = colDef(name = "Região de Integração"),
          localidade = colDef(name = "Municípios"),
          valor = colDef(
            name = "Total",
            format = colFormat(
              separators = T,
              locales = "pt-BR"
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

    ## Gráfico de Linha - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico)----
    output$inc3txt3 <- renderText({
      t33()
    })
    output$inc3graf <- renderEcharts4r({
      inc3 %>%
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
    # 4 - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico) com rendimento familiar per capita de até 1/2 salário mínimo----
    ## Mapa - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico) com rendimento familiar per capita de até 1/2 salário mínimo----
    output$inc4txt1 <- renderText({
      t41()
    })

    output$inc4map <- renderLeaflet({
      # Tratamento da informação
      ## Filtrando informação
      if (input$inc4ri == "Pará") {
        df <- inc4 %>%
          filter(localidade != "Pará", ano == input$inc4ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x$valor[x$valor == 0] <- NA
      } else {
        df <- inc4 %>%
          filter(localidade != "Pará", ano == input$inc4ano) %>%
          select(ri, localidade, ano, valor)
        x <- cbind(geopa, df)
        x <- x %>% filter(ri == input$inc4ri)
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
          "<strong>%s</strong><br/> <b>Nº de Famílias:</b> %s",
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
          title = "Nº de Famílias",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".", decimal.mark = ",", digits = 2)
        )
    })
    ## Tabela - otal de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico) com rendimento familiar per capita de até 1/2 salário mínimo----
    output$inc4txt2 <- renderText({
      t42()
    })

    output$inc4tab <- renderReactable({
      if (input$inc4ri == "Pará") {
        x <- inc4 %>%
          filter(localidade != "Pará", ano == input$inc4ano) %>%
          select(ri, localidade, valor)
      } else {
        x <- inc4 %>%
          filter(localidade != "Pará", ano == input$inc4ano) %>%
          select(ri, localidade, valor)
        x <- x %>% filter(ri == input$inc4ri)
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
          ri = colDef(name = "Região de Integração"),
          localidade = colDef(name = "Municípios"),
          valor = colDef(
            name = "Total",
            format = colFormat(
              separators = T,
              locales = "pt-BR"
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

    ## Gráfico de Linha - otal de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico) com rendimento familiar per capita de até 1/2 salário mínimo----
    output$inc4txt3 <- renderText({
      t43()
    })
    output$inc4graf <- renderEcharts4r({
      inc4 %>%
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
    #DOWNLOADS----
    # 1 - Famílias Atendidas e Valor Total Empregado no Programa Bolsa Família----
    ## Tabela - Famílias Atendidas no Programa Bolsa Família----
    # Filtra os dados
    inc1_3 <- reactive({
       if (input$inc1ri == "Pará") {
        x <- inc1 %>%
          filter(localidade != "Pará", 
                 ano == input$inc1ano) 
      } else {
        x <- inc1 %>%
          filter(localidade != "Pará", 
                 ano == input$inc1ano,
                 ri == input$inc1ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inc1_3(), {
      downset_Server("inc1_3", inc1_3(), t13())
    })
    ## Gráfico - Valor Total Empregado no Programa Bolsa Família----
    # Filtra os dados
    inc1_4 <- reactive({
      inc1 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inc1_4(), {
      downset_Server("inc1_4", inc1_4(), t14())
    })
    # 2 - Famílias Atendidas e Valor Total Empregado no Programa Auxílio Brasil----
    ## Tabela - Famílias Atendidas no Programa Auxílio Brasil----
    # Filtra os dados
    inc2_3 <- reactive({
      if (input$inc2ri == "Pará") {
        x <- inc2 %>%
          filter(localidade != "Pará", 
                 ano == input$inc2ano) 
      } else {
        x <- inc2 %>%
          filter(localidade != "Pará", 
                 ano == input$inc2ano,
                 ri == input$inc2ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inc2_3(), {
      downset_Server("inc2_3", inc2_3(), t23())
    })
    ## Gráfico - Valor Total Empregado no Programa Auxílio Brasil----
    # Filtra os dados
    inc2_4 <- reactive({
      inc2 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inc2_4(), {
      downset_Server("inc2_4", inc2_4(), t24())
    })
    # 3 - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico)----
    ## Tabela - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico)----
    # Filtra os dados
    inc3_2 <- reactive({
      if (input$inc3ri == "Pará") {
        x <- inc3 %>%
          filter(localidade != "Pará", ano == input$inc3ano)
      } else {
        x <- inc3 %>%
          filter(localidade != "Pará",
                 ri == input$inc3ri,
                 ano == input$inc3ano)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inc3_2(), {
      downset_Server("inc3_2", inc3_2(), t32())
    })
    ## Gráfico de Linha - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico)----
    # Filtra os dados
    inc3_3 <- reactive({
      inc3 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inc3_3(), {
      downset_Server("inc3_3", inc3_3(), t33())
    })

    # 4 - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico) com rendimento familiar per capita de até 1/2 salário mínimo----
    ## Tabela - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico) com rendimento familiar per capita de até 1/2 salário mínimo----
    # Filtra os dados
    inc4_2 <- reactive({
       if (input$inc4ri == "Pará") {
        x <- inc4 %>%
          filter(localidade != "Pará", ano == input$inc4ano)
      } else {
        x <- inc4 %>%
          filter(localidade != "Pará", 
                 ano == input$inc4ano,
                 ri == input$inc4ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inc4_2(), {
      downset_Server("inc4_2", inc4_2(), t42())
    })
    ## Gráfico de Linha - Total de famílias inscritas no Cadastro Único para Programas Sociais (CadÚnico) com rendimento familiar per capita de até 1/2 salário mínimo----
    # Filtra os dados
    inc4_3 <- reactive({
      inc4 %>%
        filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inc4_3(), {
      downset_Server("inc4_3", inc4_3(), t43())
    })
  })
}

# Play do Módulo
# ui <- dashboardPage(
#   header = dashboardHeader(),
#   sidebar = dashboardSidebar(),
#   body = dashboardBody(fluidPage(
#     social_inclusao_social_pa_ui("social_inclusao_social_pa")
#   ))
# )
# 
# 
# server <- function(input, output) {
#   social_inclusao_social_pa_Server("social_inclusao_social_pa")
# }
# 
# shinyApp(ui, server)
