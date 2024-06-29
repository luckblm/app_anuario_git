#Funções de módulo de Social - Educação - Estadual
#Função de UI
social_educacao_pa_ui <- function(id) {
  fluidPage(tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "estilo.css")
  ),
  #Lista de Navegação lateral----
  div(class = "navbar_social",
      navbarPage(
        tags$b("Eduação - Pará"),
        navbarMenu(
          "Indicadores",
          #1 - Taxa de Aprovação no Ensino Fundamental por Esfera Administrativa----
          tabPanel(
            "Taxa de Aprovação no Ensino Fundamental",
            panel(
              ##Controle----
              heading =
              h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Taxa de Aprovação no Ensino Fundamental"),
              tags$div(
                class = "seletor1",
                pickerInput(
                  inputId = NS(id, "edu1ano"),
                  label = "Ano",
                  choices = sort(unique(edu1[["ano"]]), decreasing = T),
                  width = "100px"
                )
              ),
              tags$div(
                class = "seletor2", style = "width: auto;",
                pickerInput(
                  inputId = NS(id, "edu1ri"),
                  label = "Pará/Região de Integração",
                  choices = unique(edu1[["ri"]]),
                  width = "200px"
                )
              )
            ),
            fluidRow(
              ##Mapa - Taxa de Aprovação no Ensino Fundamental----
              box(
                title = textOutput(NS(id,"edu1txt1")),
                collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                leafletOutput(NS(id, "edu1map"), height = "600px"),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")
              )
            ),
            ##Tabela - Taxa de Aprovação no Ensino Fundamental----
            box(
              title = textOutput(NS(id,"edu1txt2")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                reactableOutput(NS(id, "edu1tab"), height = "600px"),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                column(11,
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                         downset_ui(NS(id, "edu1_1"))
              )
            )
            
          ),
          fluidRow(
            ##Gráfico - Taxa de Aprovação no Ensino Fundamental----
            box(
              title = textOutput(NS(id,"edu1txt3")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                echarts4rOutput(NS(id, "edu1graf")),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                column(11,
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                         downset_ui(NS(id, "edu1_2"))
              )
            )
            
          )
        ),
          #2 - Taxa de Aprovação no Ensino Médio por Esfera Administrativa----
        tabPanel(
          "Taxa de Aprovação no Ensino Médio",
          panel(
            ##Controle----
            heading =
            h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Taxa de Aprovação no Ensino Médio"),
            tags$div(
              class = "seletor1",
              pickerInput(
                inputId = NS(id, "edu2ano"),
                label = "Ano",
                choices = sort(unique(edu2[["ano"]]), decreasing = T),
                width = "100px"
              )
            ),
            tags$div(
              class = "seletor2", style = "width: auto;",
              pickerInput(
                inputId = NS(id, "edu2ri"),
                label = "Pará/Região de Integração",
                choices = unique(edu2[["ri"]]),
                width = "200px"
              )
            )
          ),
          fluidRow(
            ##Mapa - Taxa de Aprovação no Ensino Médio----
            box(
              title = textOutput(NS(id,"edu2txt1")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                leafletOutput(NS(id, "edu2map"), height = "600px"),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")
              )
            ),
            ##Tabela - Taxa de Aprovação no Ensino Médio----
            box(
              title = textOutput(NS(id,"edu2txt2")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                reactableOutput(NS(id, "edu2tab"), height = "600px"),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                column(11,
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                         downset_ui(NS(id, "edu2_1"))
              )
            )
          ),
          fluidRow(
            ##Gráfico - Gráfico - Taxa de Aprovação no Ensino Médio por Esfera Administrativa----
            box(
              title = textOutput(NS(id,"edu2txt3")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                echarts4rOutput(NS(id, "edu2graf")),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                column(11,
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                         downset_ui(NS(id, "edu2_2"))
              )
            )
            
          )
        ),
        
          #3 - Taxa de Reprovação no Ensino Fundamental por Esfera Administrativa----
        tabPanel(
          "Taxa de Reprovação no Ensino Fundamental",
          panel(
            ##Controle----
            heading =
            h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Taxa de Reprovação no Ensino Fundamental"),
            tags$div(
              class = "seletor1",
              pickerInput(
                inputId = NS(id, "edu3ano"),
                label = "Ano",
                choices = sort(unique(edu3[["ano"]]), decreasing = T),
                width = "100px"
              )
            ),
            tags$div(
              class = "seletor2", style = "width: auto;",
              pickerInput(
                inputId = NS(id, "edu3ri"),
                label = "Pará/Região de Integração",
                choices = unique(edu3[["ri"]]),
                width = "200px"
              )
            )
          ),
          fluidRow(
            ##Mapa - Taxa de Reprovação no Ensino Fundamental----
            box(
              title = textOutput(NS(id,"edu3txt1")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                leafletOutput(NS(id, "edu3map"), height = "600px"),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")
              )
            ),
            ##Tabela - Taxa de Reprovação no Ensino Fundamental----
            box(
              title = textOutput(NS(id,"edu3txt2")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                reactableOutput(NS(id, "edu3tab"), height = "600px"),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                column(11,
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                         downset_ui(NS(id, "edu3_1"))
              )
            )
          ),
          fluidRow(
            ##Gráfico - Taxa de Reprovação no Ensino Fundamental----
            box(
              title = textOutput(NS(id,"edu3txt3")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                echarts4rOutput(NS(id, "edu3graf")),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                column(11,
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                         downset_ui(NS(id, "edu3_2"))
              )
            )
            
          )
        ),
          #4 - Taxa de Reprovação no Ensino Médio por Esfera Administrativa----
        tabPanel(
          "Taxa de Reprovação no Ensino Médio",
          panel(
            ##Controle----
            heading =
            h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Taxa de Reprovação no Ensino Médio"),
            tags$div(
              class = "seletor1",
              pickerInput(
                inputId = NS(id, "edu4ano"),
                label = "Ano",
                choices = sort(unique(edu4[["ano"]]), decreasing = T),
                width = "100px"
              )
            ),
            tags$div(
              class = "seletor2", style = "width: auto;",
              pickerInput(
                inputId = NS(id, "edu4ri"),
                label = "Pará/Região de Integração",
                choices = unique(edu4[["ri"]]),
                width = "200px"
              )
            )
          ),
          fluidRow(
            ##Mapa - Taxa de Reprovação no Ensino Médio----
            box(
              title = textOutput(NS(id,"edu4txt1")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                leafletOutput(NS(id, "edu4map"), height = "600px"),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")
              )
            ),
            ##Tabela - Taxa de Reprovação no Ensino Médio----
            box(
              title = textOutput(NS(id,"edu4txt2")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                reactableOutput(NS(id, "edu4tab"), height = "600px"),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                column(11,
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                         downset_ui(NS(id, "edu4_1"))
              )
            )
            
          ),
          fluidRow(
            ##Gráfico - Taxa de Reprovação no Ensino Médio----
            box(
              title = textOutput(NS(id,"edu4txt3")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                echarts4rOutput(NS(id, "edu4graf")),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                column(11,
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                         downset_ui(NS(id, "edu4_2"))
              )
            )
          )
        ),
        
          #5 - Taxa de Abandono no Ensino Fundamental por Esfera Administrativa----
        tabPanel(
          "Taxa de Abandono no Ensino Fundamental",
          panel(
            ##Controle----
            heading =
            h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Taxa de Abandono no Ensino Fundamental"),
            tags$div(
              class = "seletor1",
              pickerInput(
                inputId = NS(id, "edu5ano"),
                label = "Ano",
                choices = sort(unique(edu5[["ano"]]), decreasing = T),
                width = "100px"
              )
            ),
            tags$div(
              class = "seletor2", style = "width: auto;",
              pickerInput(
                inputId = NS(id, "edu5ri"),
                label = "Pará/Região de Integração",
                choices = unique(edu5[["ri"]]),
                width = "200px"
              )
            )
          ),
          fluidRow(
            ##Mapa - Taxa de Abandono no Ensino Fundamental----
            box(
              title = textOutput(NS(id,"edu5txt1")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                leafletOutput(NS(id, "edu5map"), height = "600px"),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")
              )
            ),
            ##Tabela - Taxa de Abandono no Ensino Fundamental----
            box(
              title = textOutput(NS(id,"edu5txt2")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                reactableOutput(NS(id, "edu5tab"), height = "600px"),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                column(11,
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                         downset_ui(NS(id, "edu5_1"))
              )
            )
            
          ),
          fluidRow(
            ##Gráfico - Taxa de Abandono no Ensino Fundamental----
            box(
              title = textOutput(NS(id,"edu5txt3")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                echarts4rOutput(NS(id, "edu5graf")),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                column(11,
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                         downset_ui(NS(id, "edu5_2"))
              )
            )
          )
        ),
        
          #6 - Taxa de Abandono no Ensino Médio por Esfera Administrativa----
        tabPanel(
          "Taxa de Abandono no Ensino Médio",
          panel(
            ##Controle----
            heading =
            h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Taxa de Abandono no Ensino Médio"),
            tags$div(
              class = "seletor1",
              pickerInput(
                inputId = NS(id, "edu6ano"),
                label = "Ano",
                choices = sort(unique(edu6[["ano"]]), decreasing = T),
                width = "100px"
              )
            ),
            tags$div(
              class = "seletor2", style = "width: auto;",
              pickerInput(
                inputId = NS(id, "edu6ri"),
                label = "Pará/Região de Integração",
                choices = unique(edu6[["ri"]]),
                width = "200px"
              )
            )
          ),
          fluidRow(
            ##Mapa - Taxa de Abandono no Ensino Médio----
            box(
              title = textOutput(NS(id,"edu6txt1")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                leafletOutput(NS(id, "edu6map"), height = "600px"),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")
              )
            ),
            ##Tabela - Taxa de Abandono no Ensino Médio----
            box(
              title = textOutput(NS(id,"edu6txt2")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                reactableOutput(NS(id, "edu6tab"), height = "600px"),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                column(11,
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                         downset_ui(NS(id, "edu6_1"))
              )
            )
          ),
          fluidRow(
            ##Gráfico - Taxa de Abandono no Ensino Médio----
            box(
              title = textOutput(NS(id,"edu6txt3")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                echarts4rOutput(NS(id, "edu6graf")),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                column(11,
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                         downset_ui(NS(id, "edu6_2"))
              )
            )
          )
        ),
          #7 - Distorção Idade-Série Total por Nível de Ensino----
        tabPanel(
          "Distorção Idade-Série Total por Nível de Ensino",
          panel(
            ##Controle----
            heading =
            h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Distorção Idade-Série Total por Nível de Ensino"),
            tags$div(
              class = "seletor1",
              pickerInput(
                inputId = NS(id, "edu7ano"),
                label = "Ano",
                choices = sort(unique(edu7[["ano"]]), decreasing = T),
                width = "100px"
              )
            ),
            tags$div(
              class = "seletor2", style = "width: auto;",
              pickerInput(
                inputId = NS(id, "edu7ri"),
                label = "Pará/Região de Integração",
                choices = unique(edu7[["ri"]]),
                width = "200px"
              )
            ),
            tags$div(
              class = "seletor2", style = "width: auto;",
              pickerInput(
                inputId = NS(id, "edu7cat"),
                label = "Nível de Ensino",
                choices = unique(edu7[["categoria"]]),
                width = "200px"
              )
            )
          ),
          fluidRow(
            ##Mapa - Distorção Idade-Série Total por Nível de Ensino----
            box(
              title = textOutput(NS(id,"edu7txt1")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                leafletOutput(NS(id, "edu7map"), height = "600px"),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")
              )
            ),
            ##Tabela - Distorção Idade-Série Total por Nível de Ensino----
            box(
              title = textOutput(NS(id,"edu7txt2")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                reactableOutput(NS(id, "edu7tab"), height = "600px"),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                column(11,
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                         downset_ui(NS(id, "edu7_1"))
              )
            )
          ),
          fluidRow(
            ##Gráfico - Distorção Idade-Série Total por Nível de Ensino----
            box(
              title = textOutput(NS(id,"edu7txt3")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                echarts4rOutput(NS(id, "edu7graf")),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                column(11,
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                         downset_ui(NS(id, "edu7_2"))
              )
            )
            
          )
        ),
          #8 -  IDEB - Escola pública - 5ª ano (séries iniciais)----
        tabPanel(
          "IDEB - Escola pública - 5ª ano (séries iniciais)",
          panel(
            ##Controle----
            heading =
            h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "IDEB - Escola pública - 5ª ano (séries iniciais)"),
            tags$div(
              class = "seletor1",
              pickerInput(
                inputId = NS(id, "edu8ano"),
                label = "Ano",
                choices = sort(unique(edu8[["ano"]]), decreasing = T),
                width = "100px"
              )
            ),
            tags$div(
              class = "seletor2", style = "width: auto;",
              pickerInput(
                inputId = NS(id, "edu8ri"),
                label = "Pará/Região de Integração",
                choices = unique(edu8[["ri"]]),
                width = "200px"
              )
            )
          ),
          fluidRow(
            ##Mapa - IDEB - Escola pública - 5ª ano (séries iniciais)----
            box(
              title = textOutput(NS(id,"edu8txt1")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                leafletOutput(NS(id, "edu8map"), height = "600px"),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")
              )
            ),
            ##Tabela - IDEB - Escola pública - 5ª ano (séries iniciais)----
            box(
              title = textOutput(NS(id,"edu8txt2")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                reactableOutput(NS(id, "edu8tab"), height = "600px"),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                column(11,
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                         downset_ui(NS(id, "edu8_1"))
              )
            )
          ),
          fluidRow(
            ##Gráfico - IDEB - Escola pública - 5ª ano (séries iniciais)----
            box(
              title = textOutput(NS(id,"edu8txt3")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                echarts4rOutput(NS(id, "edu8graf")),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                column(11,
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                         downset_ui(NS(id, "edu8_2"))
              )
            )
            
          )
        ),
          #9 -  IDEB - Escola pública - 9ª ano (séries finais)----
        tabPanel(
          "IDEB - Escola pública - 9ª ano (séries finais)",
          panel(
            ##Controle----
            heading =
            h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "IDEB - Escola pública - 9ª ano (séries finais)"),
            tags$div(
              class = "seletor1",
              pickerInput(
                inputId = NS(id, "edu9ano"),
                label = "Ano",
                choices = sort(unique(edu9[["ano"]]), decreasing = T),
                width = "100px"
              )
            ),
            tags$div(
              class = "seletor2", style = "width: auto;",
              pickerInput(
                inputId = NS(id, "edu9ri"),
                label = "Pará/Região de Integração",
                choices = unique(edu9[["ri"]]),
                width = "200px"
              )
            )
          ),
          fluidRow(
            ##Mapa - IDEB - Escola pública - 9ª ano (séries finais)----
            box(
              title = textOutput(NS(id,"edu9txt1")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                leafletOutput(NS(id, "edu9map"), height = "600px"),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")
              )
            ),
            ##Tabela - IDEB - Escola pública - 9ª ano (séries finais)----
            box(
              title = textOutput(NS(id,"edu9txt2")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                reactableOutput(NS(id, "edu9tab"), height = "600px"),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                column(11,
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                         downset_ui(NS(id, "edu9_1"))
              )
            )
          ),
          fluidRow(
            ##Gráfico - IDEB - Escola pública - 9ª ano (séries finais)----
            box(
              title = textOutput(NS(id,"edu9txt3")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                echarts4rOutput(NS(id, "edu9graf")),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                column(11,
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                         downset_ui(NS(id, "edu9_2"))
              )
            )
            
          )
        ),
        
          #10 - Número de Matrículas no Ensino Pré-Escolar por Esfera Administrativa----
        tabPanel(
          "Número de Matrículas no Ensino Pré-Escolar",
          panel(
            ##Controle----
            heading =
            h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Número de Matrículas no Ensino Pré-Escolar"),
            tags$div(
              class = "seletor1",
              pickerInput(
                inputId = NS(id, "edu10ano"),
                label = "Ano",
                choices = sort(unique(edu10[["ano"]]), decreasing = T),
                width = "100px"
              )
            ),
            tags$div(
              class = "seletor2", style = "width: auto;",
              pickerInput(
                inputId = NS(id, "edu10ri"),
                label = "Pará/Região de Integração",
                choices = unique(edu10[["ri"]]),
                width = "200px"
              )
            )
          ),
          fluidRow(
            ##Mapa - Número de Matrículas Total no Ensino Pré-Escolar----
            box(
              title = textOutput(NS(id,"edu10txt1")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                leafletOutput(NS(id, "edu10map"), height = "600px"),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")
              )
            ),
            ##Tabela - Número de Matrículas no Ensino Pré-Escolar----
            box(
              title = textOutput(NS(id,"edu10txt2")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                reactableOutput(NS(id, "edu10tab"), height = "600px"),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                column(11,
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                         downset_ui(NS(id, "edu10_1"))
              )
            )
            
          ),
          fluidRow(
            ##Gráfico - Número de Matrículas no Ensino Pré-Escolar----
            box(
              title = textOutput(NS(id,"edu10txt3")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                echarts4rOutput(NS(id, "edu10graf")),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                column(11,
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                         downset_ui(NS(id, "edu10_2"))
              )
            )
            
          )
        ),
          #11 - Número de Matrículas no Ensino Fundamental por Esfera Administrativa----
        tabPanel(
          "Número de Matrículas no Ensino Fundamental",
          panel(
            ##Controle----
            heading =
            h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Número de Matrículas no Ensino Fundamental"),
            tags$div(
              class = "seletor1",
              pickerInput(
                inputId = NS(id, "edu11ano"),
                label = "Ano",
                choices = sort(unique(edu11[["ano"]]), decreasing = T),
                width = "100px"
              )
            ),
            tags$div(
              class = "seletor2", style = "width: auto;",
              pickerInput(
                inputId = NS(id, "edu11ri"),
                label = "Pará/Região de Integração",
                choices = unique(edu11[["ri"]]),
                width = "200px"
              )
            )
          ),
          fluidRow(
            ##Mapa - Número de Matrículas Total no Ensino Fundamental----
            box(
              title = textOutput(NS(id,"edu11txt1")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                leafletOutput(NS(id, "edu11map"), height = "600px"),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")
              )
            ),
            ##Tabela - Número de Matrículas no Ensino Fundamental----
            box(
              title = textOutput(NS(id,"edu11txt2")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                reactableOutput(NS(id, "edu11tab"), height = "600px"),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                column(11,
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                         downset_ui(NS(id, "edu11_1"))
              )
            )
          ),
          fluidRow(
            ##Gráfico - Número de Matrículas no Ensino Fundamental----
            box(
              title = textOutput(NS(id,"edu11txt3")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                echarts4rOutput(NS(id, "edu11graf")),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                column(11,
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                         downset_ui(NS(id, "edu11_2"))
              )
            )
          )
        ),
          #12 - Número de Matrículas no Ensino Médio por Esfera Administrativa----
        tabPanel(
          "Número de Matrículas no Ensino Médio",
          panel(
            ##Controle----
            heading =
            h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Número de Matrículas no Ensino Médio"),
            tags$div(
              class = "seletor1",
              pickerInput(
                inputId = NS(id, "edu12ano"),
                label = "Ano",
                choices = sort(unique(edu12[["ano"]]), decreasing = T),
                width = "100px"
              )
            ),
            tags$div(
              class = "seletor2", style = "width: auto;",
              pickerInput(
                inputId = NS(id, "edu12ri"),
                label = "Pará/Região de Integração",
                choices = unique(edu12[["ri"]]),
                width = "200px"
              )
            )
          ),
          fluidRow(
            ##Mapa - Número de Matrículas Total no Ensino Médio----
            box(
              title = textOutput(NS(id,"edu12txt1")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                leafletOutput(NS(id, "edu12map"), height = "600px"),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")
              )
            ),
            ##Tabela - Número de Matrículas no Ensino Médio----
            box(
              title = textOutput(NS(id,"edu12txt2")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                reactableOutput(NS(id, "edu12tab"), height = "600px"),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                column(11,
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                         downset_ui(NS(id, "edu12_1"))
              )
            )
          ),
          fluidRow(
            ##Gráfico - Número de Matrículas no Ensino Médio----
            box(
              title = textOutput(NS(id,"edu12txt3")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                echarts4rOutput(NS(id, "edu12graf")),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                column(11,
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                         downset_ui(NS(id, "edu12_2"))
              )
            )
          )
        ),
          #13 - Média de Alunos por Turma por Nível de Ensino----
          tabPanel(
          "Média de Alunos por Turma por Nível de Ensino",
          panel(
            ##Controle----
            heading =
            h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Média de Alunos por Turma por Nível de Ensino"),
            tags$div(
              class = "seletor1",
              pickerInput(
                inputId = NS(id, "edu13ano"),
                label = "Ano",
                choices = sort(unique(edu13[["ano"]]), decreasing = T),
                width = "100px"
              )
            ),
            tags$div(
              class = "seletor2", style = "width: auto;",
              pickerInput(
                inputId = NS(id, "edu13ri"),
                label = "Pará/Região de Integração",
                choices = unique(edu13[["ri"]]),
                width = "200px"
              )
            ),
            tags$div(
              class = "seletor2", style = "width: auto;",
              pickerInput(
                inputId = NS(id, "edu13cat"),
                label = "Nível de Ensino",
                choices = unique(edu13[["categoria"]]),
                width = "200px"
              )
            )
          ),
          fluidRow(
            ##Mapa - Média de Alunos por Turma por Nível de Ensino----
            box(
              title = textOutput(NS(id,"edu13txt1")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                leafletOutput(NS(id, "edu13map"), height = "600px"),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")
              )
            ),
            ##Tabela - Média de Alunos por Turma por Nível de Ensino----
            box(
              title = textOutput(NS(id,"edu13txt2")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                reactableOutput(NS(id, "edu13tab"), height = "600px"),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                column(11,
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                         downset_ui(NS(id, "edu13_1"))
              )
            )
          ),
          fluidRow(
            ##Gráfico - Média de Alunos por Turma por Nível de Ensino----
            box(
              title = textOutput(NS(id,"edu13txt3")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                echarts4rOutput(NS(id, "edu13graf")),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                column(11,
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                         downset_ui(NS(id, "edu13_2"))
              )
            )
          )
        ),
          #14 - Número de Docentes no Ensino Pré-escolar por Esfera Administrativa----
        tabPanel(
          "Número de Docentes no Ensino Pré-escolar",
          panel(
            ##Controle----
            heading =
            h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Número de Docentes no Ensino Pré-escolar"),
            tags$div(
              class = "seletor1",
              pickerInput(
                inputId = NS(id, "edu14ano"),
                label = "Ano",
                choices = sort(unique(edu14[["ano"]]), decreasing = T),
                width = "100px"
              )
            ),
            tags$div(
              class = "seletor2", style = "width: auto;",
              pickerInput(
                inputId = NS(id, "edu14ri"),
                label = "Pará/Região de Integração",
                choices = unique(edu14[["ri"]]),
                width = "200px"
              )
            )
          ),
          fluidRow(
            ##Mapa - Número de Docentes Total no Ensino Pré-escolar----
            box(
              title = textOutput(NS(id,"edu14txt1")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                leafletOutput(NS(id, "edu14map"), height = "600px"),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")
              )
            ),
            ##Tabela - Número de Docentes no Ensino Pré-escolar----
            box(
              title = textOutput(NS(id,"edu14txt2")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                reactableOutput(NS(id, "edu14tab"), height = "600px"),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                column(11,
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                         downset_ui(NS(id, "edu14_1"))
              )
            )
          ),
          fluidRow(
            ##Gráfico - Número de Docentes no Ensino Pré-escolar----
            box(
              title = textOutput(NS(id,"edu14txt3")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                echarts4rOutput(NS(id, "edu14graf")),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                column(11,
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                         downset_ui(NS(id, "edu14_2"))
              )
            )
          )
        ),
          #15 - Número de Docentes no Ensino Fundamental por Esfera Administrativa----
        tabPanel(
          "Número de Docentes no Ensino Fundamental",
          panel(
            ##Controle----
            heading =
            h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Número de Docentes no Ensino Fundamental"),
            tags$div(
              class = "seletor1",
              pickerInput(
                inputId = NS(id, "edu15ano"),
                label = "Ano",
                choices = sort(unique(edu15[["ano"]]), decreasing = T),
                width = "100px"
              )
            ),
            tags$div(
              class = "seletor2", style = "width: auto;",
              pickerInput(
                inputId = NS(id, "edu15ri"),
                label = "Pará/Região de Integração",
                choices = unique(edu15[["ri"]]),
                width = "200px"
              )
            )
          ),
          fluidRow(
            ##Mapa - Número de Docentes Total no Ensino Fundamental----
            box(
              title = textOutput(NS(id,"edu15txt1")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                leafletOutput(NS(id, "edu15map"), height = "600px"),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")
              )
            ),
            ##Tabela - Número de Docentes no Ensino Fundamental----
            box(
              title = textOutput(NS(id,"edu15txt2")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                reactableOutput(NS(id, "edu15tab"), height = "600px"),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                column(11,
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                         downset_ui(NS(id, "edu15_1"))
              )
            )
          ),
          fluidRow(
            ##Gráfico - Número de Docentes no Ensino Fundamental----
            box(
              title = textOutput(NS(id,"edu15txt3")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                echarts4rOutput(NS(id, "edu15graf")),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                column(11,
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                         downset_ui(NS(id, "edu15_2"))
              )
            )
          )
        ),
          #16 - Número de Docentes no Ensino Médio por Esfera Administrativa----
        tabPanel(
          "Número de Docentes no Ensino Médio",
          panel(
            ##Controle----
            heading =
            h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Número de Docentes no Ensino Médio"),
            tags$div(
              class = "seletor1",
              pickerInput(
                inputId = NS(id, "edu16ano"),
                label = "Ano",
                choices = sort(unique(edu16[["ano"]]), decreasing = T),
                width = "100px"
              )
            ),
            tags$div(
              class = "seletor2", style = "width: auto;",
              pickerInput(
                inputId = NS(id, "edu16ri"),
                label = "Pará/Região de Integração",
                choices = unique(edu16[["ri"]]),
                width = "200px"
              )
            )
          ),
          fluidRow(
            ##Mapa - Número de Docentes Total no Ensino Médio----
            box(
              title = textOutput(NS(id,"edu16txt1")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                leafletOutput(NS(id, "edu16map"), height = "600px"),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")
              )
            ),
            ##Tabela - Número de Docentes no Ensino Médio----
            box(
              title = textOutput(NS(id,"edu16txt2")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                reactableOutput(NS(id, "edu16tab"), height = "600px"),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                column(11,
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                         downset_ui(NS(id, "edu16_1"))
              )
            )
          ),
          fluidRow(
            ##Gráfico - Número de Docentes no Ensino Médio----
            box(
              title = textOutput(NS(id,"edu16txt3")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                echarts4rOutput(NS(id, "edu16graf")),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                column(11,
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                         downset_ui(NS(id, "edu16_2"))
              )
            )
          )
        ),
          #17 - Estabelecimentos de Pré-Escola por Esfera Administrativa----
        tabPanel(
          "Estabelecimentos de Pré-Escola",
          panel(
            ##Controle----
            heading =
            h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Estabelecimentos de Pré-Escola"),
            tags$div(
              class = "seletor1",
              pickerInput(
                inputId = NS(id, "edu17ano"),
                label = "Ano",
                choices = sort(unique(edu17[["ano"]]), decreasing = T),
                width = "100px"
              )
            ),
            tags$div(
              class = "seletor2", style = "width: auto;",
              pickerInput(
                inputId = NS(id, "edu17ri"),
                label = "Pará/Região de Integração",
                choices = unique(edu17[["ri"]]),
                width = "200px"
              )
            )
          ),
          fluidRow(
            ##Mapa - Estabelecimentos de Pré-Escola por Esfera Administrativa----
            box(
              title = textOutput(NS(id,"edu17txt1")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                leafletOutput(NS(id, "edu17map"), height = "600px"),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")
              )
            ),
            ##Tabela - Estabelecimentos de Pré-Escola por Esfera Administrativa----
            box(
              title = textOutput(NS(id,"edu17txt2")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                reactableOutput(NS(id, "edu17tab"), height = "600px"),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                column(11,
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                         downset_ui(NS(id, "edu17_1"))
              )
            )
          ),
          fluidRow(
            ##Gráfico - Estabelecimentos de Pré-Escola por Esfera Administrativa----
            box(
              title = textOutput(NS(id,"edu17txt3")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                echarts4rOutput(NS(id, "edu17graf")),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                column(11,
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                         downset_ui(NS(id, "edu17_2"))
              )
            )
          )
        ),
          #18 - Estabelecimentos de Ensino Fundamental por Esfera Administrativa----
        tabPanel(
          "Estabelecimentos de Ensino Fundamental",
          panel(
            ##Controle----
            heading =
            h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Estabelecimentos de Ensino Fundamental"),
            tags$div(
              class = "seletor1",
              pickerInput(
                inputId = NS(id, "edu18ano"),
                label = "Ano",
                choices = sort(unique(edu18[["ano"]]), decreasing = T),
                width = "100px"
              )
            ),
            tags$div(
              class = "seletor2", style = "width: auto;",
              pickerInput(
                inputId = NS(id, "edu18ri"),
                label = "Pará/Região de Integração",
                choices = unique(edu18[["ri"]]),
                width = "200px"
              )
            )
          ),
          fluidRow(
            ##Mapa - Estabelecimentos de Ensino Fundamental por Esfera Administrativa----
            box(
              title = textOutput(NS(id,"edu18txt1")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                leafletOutput(NS(id, "edu18map"), height = "600px"),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")
              )
            ),
            ##Tabela - Estabelecimentos de Ensino Fundamental por Esfera Administrativa----
            box(
              title = textOutput(NS(id,"edu18txt2")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                reactableOutput(NS(id, "edu18tab"), height = "600px"),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                column(11,
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                         downset_ui(NS(id, "edu18_1"))
              )
            )
          ),
          fluidRow(
            ##Gráfico - Estabelecimentos de Ensino Fundamental por Esfera Administrativa----
            box(
              title = textOutput(NS(id,"edu18txt3")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                echarts4rOutput(NS(id, "edu18graf")),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                column(11,
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                         downset_ui(NS(id, "edu18_2"))
              )
            )
          )
        ),
          #19 - Estabelecimentos de Ensino Médio por Esfera Administrativa----
        tabPanel(
          "Estabelecimentos de Ensino Médio",
          panel(
            ##Controle----
            heading =
            h4(style = "margin-bottom: 2px;margin-top: 2px;
                font-weight: bold;color: #6D6D6D;",
                "Estabelecimentos de Ensino Médio"),
            tags$div(
              class = "seletor1",
              pickerInput(
                inputId = NS(id, "edu19ano"),
                label = "Ano",
                choices = sort(unique(edu19[["ano"]]), decreasing = T),
                width = "100px"
              )
            ),
            tags$div(
              class = "seletor2", style = "width: auto;",
              pickerInput(
                inputId = NS(id, "edu19ri"),
                label = "Pará/Região de Integração",
                choices = unique(edu19[["ri"]]),
                width = "200px"
              )
            )
          ),
          fluidRow(
            ##Mapa - Estabelecimentos de Ensino Médio por Esfera Administrativa----
            box(
              title = textOutput(NS(id,"edu19txt1")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                leafletOutput(NS(id, "edu19map"), height = "600px"),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")
              )
            ),
            ##Tabela - Estabelecimentos de Ensino Médio por Esfera Administrativa----
            box(
              title = textOutput(NS(id,"edu19txt2")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                reactableOutput(NS(id, "edu19tab"), height = "600px"),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                column(11,
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                         downset_ui(NS(id, "edu19_1"))
              )
            )
          ),
          fluidRow(
            ##Gráfico - Estabelecimentos de Ensino Médio por Esfera Administrativa----
            box(
              title = textOutput(NS(id,"edu19txt3")),
              collapsible = T,
              collapsed = F,
              headerBorder = T,
              width = 12,
              withSpinner(
                echarts4rOutput(NS(id, "edu19graf")),
                type = 8,
                color = "#f17701",
                size = 0.5
              ),
              footer = list(
                column(11,
                tags$h6(tags$b("Fonte:"), "INEP"),
                tags$h6(tags$b("Elaboração:"), "FAPESPA")),
                         downset_ui(NS(id, "edu19_2"))
              )
            )
          )
        )
      )
  )))
  
}

#Função do modulo servidor
social_educacao_pa_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    #TÍTULOS----
    #1 - Taxa de Aprovação no Ensino Fundamental por Esfera Administrativa----
    ##Mapa - Taxa de Aprovação no Ensino Fundamental por Esfera Administrativa----
    t11 <- reactive({
      if (input$edu1ri == "Pará") {
        paste0(
          "Taxa de Aprovação Total no Ensino Fundamental, Pará - ",
          input$edu1ano
        )
      } else{
        paste0(
          "Taxa de Aprovação Total no Ensino Fundamental, Região de Integração ",
          input$edu1ri,
          " - ",
          input$edu1ano
        )
      }
    })
    ##Tabela - Taxa de Aprovação no Ensino Fundamental por Esfera Administrativa----
    t12 <- reactive({
      if (input$edu1ri == "Pará") {
        paste0(
          "Taxa de Aprovação no Ensino Fundamental por Esfera Administrativa e Município, Pará - ",
          input$edu1ano
        )
      } else{
        paste0(
          "Taxa de Aprovação no Ensino Fundamental por Esfera Administrativa e Município, Região de Integração ",
          input$edu1ri,
          " - ",
          input$edu1ano
        )
      }
    })
    ##Gráfico - Taxa de Aprovação no Ensino Fundamental por Esfera Administrativa----
    t13 <- reactive({
      paste0("Taxa de Aprovação no Ensino Fundamental por Esfera Administrativa, Pará - ",min(edu1$ano)," a ",max(edu1$ano))
    })
    #2 - Taxa de Aprovação no Ensino Médio por Esfera Administrativa----
    ##Mapa - Taxa de Aprovação no Ensino Médio por Esfera Administrativa----
    t21 <- reactive({
      if (input$edu2ri == "Pará") {
        paste0(
          "Taxa de Aprovação Total no Ensino Médio, Pará - ",
          input$edu2ano
        )
      } else{
        paste0(
          "Taxa de Aprovação Total no Ensino Médio, Região de Integração ",
          input$edu2ri,
          " - ",
          input$edu2ano
        )
      }
    })
    ##Tabela - Taxa de Aprovação no Ensino Médio por Esfera Administrativa----
    t22 <- reactive({
      if (input$edu2ri == "Pará") {
        paste0(
          "Taxa de Aprovação no Ensino Médio por Esfera Administrativa e Município, Pará - ",
          input$edu2ano
        )
      } else{
        paste0(
          "Taxa de Aprovação no Ensino Médio por Esfera Administrativa e Município, Região de Integração ",
          input$edu2ri,
          " - ",
          input$edu2ano
        )
      }
    })
    ##Gráfico - Taxa de Aprovação no Ensino Médio por Esfera Administrativa----
    t23 <- reactive({
      paste0("Taxa de Aprovação no Ensino Médio por Esfera Administrativa, Pará - ",min(edu2$ano)," a ",max(edu2$ano))
    })
    #3 - Taxa de Reprovação no Ensino Fundamental por Esfera Administrativa----
    ##Mapa - Taxa de Reprovação no Ensino Fundamental por Esfera Administrativa----
    t31 <- reactive({
      if (input$edu3ri == "Pará") {
        paste0(
          "Taxa de Reprovação Total no Ensino Fundamental, Pará - ",
          input$edu3ano
        )
      } else{
        paste0(
          "Taxa de Reprovação Total no Ensino Fundamental, Região de Integração ",
          input$edu3ri,
          " - ",
          input$edu3ano
        )
      }
    })
    ##Tabela - Taxa de Reprovação no Ensino Fundamental por Esfera Administrativa----
    t32 <- reactive({
      if (input$edu3ri == "Pará") {
        paste0(
          "Taxa de Reprovação no Ensino Fundamental por Esfera Administrativa por Município, Pará - ",
          input$edu3ano
        )
      } else{
        paste0(
          "Taxa de Reprovação no Ensino Fundamental por Esfera Administrativa por Município, Região de Integração ",
          input$edu3ri,
          " - ",
          input$edu3ano
        )
      }
    })
    ##Gráfico - Taxa de Reprovação no Ensino Fundamental por Esfera Administrativa----
    t33 <- reactive({
      paste0("Taxa de Reprovação no Ensino Fundamental por Esfera Administrativa, Pará - ",min(edu3$ano)," a ",max(edu3$ano))
    })
    #4 - Taxa de Reprovação no Ensino Médio por Esfera Administrativa----
    ##Mapa - Taxa de Reprovação no Ensino Médio por Esfera Administrativa----
    t41 <- reactive({
      if (input$edu4ri == "Pará") {
        paste0(
          "Taxa de Reprovação Total no Ensino Médio, Pará - ",
          input$edu4ano
        )
      } else{
        paste0(
          "Taxa de Reprovação Total no Ensino Médio, Região de Integração ",
          input$edu4ri,
          " - ",
          input$edu4ano
        )
      }
    })
    ##Tabela - Taxa de Reprovação no Ensino Médio por Esfera Administrativa----
    t42 <- reactive({
        if (input$edu4ri == "Pará") {
          paste0(
            "Taxa de Reprovação no Ensino Médio por Esfera Administrativa e Município, Pará - ",
            input$edu4ano
          )
        } else{
          paste0(
            "Taxa de Reprovação no Ensino Médio por Esfera Administrativa e Município, Região de Integração ",
            input$edu4ri,
            " - ",
            input$edu4ano
          )
        }
    })
    ##Gráfico - Taxa de Reprovação no Ensino Médio por Esfera Administrativa----
    t43 <- reactive({
      paste0("Taxa de Reprovação no Ensino Médio por Esfera Administrativa, Pará - ",min(edu4$ano)," a ",max(edu4$ano))
    })
    #5 - Taxa de Abandono no Ensino Fundamental por Esfera Administrativa----
    ##Mapa - Taxa de Abandono no Ensino Fundamental por Esfera Administrativa----
    t51 <- reactive({
      if (input$edu5ri == "Pará") {
        paste0(
          "Taxa de Abandono Total no Ensino Fundamental, Pará - ",
          input$edu5ano
        )
      } else{
        paste0(
          "Taxa de Abandono Total no Ensino Fundamental, Região de Integração ",
          input$edu5ri,
          " - ",
          input$edu5ano
        )
      }
    })
    ##Tabela - Taxa de Abandono no Ensino Fundamental por Esfera Administrativa----
    t52 <- reactive({
      if (input$edu5ri == "Pará") {
        paste0(
          "Taxa de Abandono no Ensino Fundamental por Esfera Administrativa e Município, Pará - ",
          input$edu5ano
        )
      } else{
        paste0(
          "Taxa de Abandono no Ensino Fundamental por Esfera Administrativa e Município, Região de Integração ",
          input$edu5ri,
          " - ",
          input$edu5ano
        )
      }
    })
    ##Gráfico - Taxa de Abandono no Ensino Fundamental por Esfera Administrativa----
    t53 <- reactive({
      paste0("Taxa de Abandono no Ensino Fundamental por Esfera Administrativa, Pará - ",min(edu5$ano)," a ",max(edu5$ano))
    })
    #6 - Taxa de Abandono no Ensino Médio por Esfera Administrativa----
    ##Mapa - Taxa de Abandono no Ensino Médio por Esfera Administrativa----
    t61 <- reactive({
      if (input$edu6ri == "Pará") {
        paste0(
          "Taxa de Abandono Total no Ensino Médio, Pará - ",
          input$edu6ano
        )
      } else{
        paste0(
          "Taxa de Abandono Total no Ensino Médio, Região de Integração ",
          input$edu6ri,
          " - ",
          input$edu6ano
        )
      }
    })
    ##Tabela - Taxa de Abandono no Ensino Médio por Esfera Administrativa----
    t62 <- reactive({
      if (input$edu6ri == "Pará") {
        paste0(
          "Taxa de Abandono no Ensino Médio por Esfera Administrativa e Município, Pará - ",
          input$edu6ano
        )
      } else{
        paste0(
          "Taxa de Abandono no Ensino Médio por Esfera Administrativa e Município, Região de Integração ",
          input$edu6ri,
          " - ",
          input$edu6ano
        )
      }
    })
    ##Gráfico - Taxa de Abandono no Ensino Médio por Esfera Administrativa----
    t63 <- reactive({
      paste0("Taxa de Abandono no Ensino Médio por Esfera Administrativa, Pará - ",min(edu6$ano)," a ",max(edu6$ano))
    })
    #7 - Distorção Idade-Série Total por Nível de Ensino----
    ##Mapa - Distorção Idade-Série Total por Nível de Ensino----
    t71 <- reactive({
      if (input$edu7ri == "Pará") {
        paste0(
          "Distorção Idade-Série Total por Nível de Ensino - " ,
          input$edu7cat,
          ", Pará - ",
          input$edu7ano
        )
      } else{
        paste0(
          "Distorção Idade-Série Total por Nível de Ensino - ",
          input$edu7cat,
          ", Região de Integração ",
          input$edu7ri,
          " - ",
          input$edu7ano
        )
      }
    })
    ##Tabela - Distorção Idade-Série Total por Nível de Ensino----
    t72 <- reactive({
      if (input$edu7ri == "Pará") {
        paste0(
          "Distorção Idade-Série Total por Nível de Ensino, Pará - " ,
          input$edu7ano
        )
      } else{
        paste0(
          "Distorção Idade-Série Total por Nível de Ensino, Região de Integração - ",
          input$edu7ri,
          " - ",
          input$edu7ano
        )
      }
    })
    ##Gráfico - Distorção Idade-Série Total por Nível de Ensino----
    t73 <- reactive({
      paste0("Distorção Idade-Série Total por Nível de Ensino, Pará - ",min(edu7$ano)," a ",max(edu7$ano))
    })
    #8 -  IDEB - Escola pública - 5ª ano (séries iniciais)----
    ##Mapa - DDEB - Escola pública - 5ª ano (séries iniciais)----
    t81 <- reactive({
      if (input$edu8ri == "Pará") {
        paste0(
          "Índice de desenvonvimento da educação básica - IDEB - Escola pública - 5ª ano (séries iniciais), Pará - ",
          input$edu8ano
        )
      } else{
        paste0(
          "Índice de desenvonvimento da educação básica - IDEB - Escola pública - 5ª ano (séries iniciais), Região de Integração ",
          input$edu8ri,
          " - ",
          input$edu8ano
        )
      }
    })
    ##Tabela - DDEB - Escola pública - 5ª ano (séries iniciais)----
    t82 <- reactive({
      if (input$edu8ri == "Pará") {
        paste0(
          "Índice de desenvonvimento da educação básica - IDEB - Escola pública - 5ª ano (séries iniciais) por Município, Pará - ",
          input$edu8ano
        )
      } else{
        paste0(
          "Índice de desenvonvimento da educação básica - IDEB - Escola pública - 5ª ano (séries iniciais) por Município, Região de Integração ",
          input$edu8ri,
          " - ",
          input$edu8ano
        )
      }
    })
    ##Gráfico - DEB - Escola pública - 5ª ano (séries iniciais)----
    t83 <- reactive({
      paste0("DEB - Escola pública - 5ª ano (séries iniciais), Pará - ",min(edu8$ano)," a ",max(edu8$ano))
    })
    #9 -  IDEB - Escola pública - 9ª ano (séries finais)----
    ##Mapa - DDEB - Escola pública - 9ª ano (séries finais)----
    t91 <- reactive({
      if (input$edu9ri == "Pará") {
        paste0(
          "Índice de desenvonvimento da educação básica - IDEB - Escola pública - 9ª ano (séries finais) por Município, Pará - ",
          input$edu9ano
        )
      } else{
        paste0(
          "Índice de desenvonvimento da educação básica - IDEB - Escola pública - 9ª ano (séries finais) por Município, Região de Integração ",
          input$edu9ri,
          " - ",
          input$edu9ano
        )
      }
    })
    ##Tabela - DDEB - Escola pública - 9ª ano (séries finais)----
    t92 <- reactive({
      if (input$edu9ri == "Pará") {
        paste0(
          "Índice de desenvonvimento da educação básica - IDEB - Escola pública - 9ª ano (séries finais) por Município, Pará - ",
          input$edu9ano
        )
      } else{
        paste0(
          "Índice de desenvonvimento da educação básica - IDEB - Escola pública - 9ª ano (séries finais) por Município, Região de Integração ",
          input$edu9ri,
          " - ",
          input$edu9ano
        )
      }
    })
    ##Gráfico - DEB - Escola pública - 9ª ano (séries finais)----
    t93 <- reactive({
      paste0("DEB - Escola pública - 9ª ano (séries finais), Pará - ",min(edu9$ano)," a ",max(edu9$ano))
    })
    #10 - Número de Matrículas no Ensino Pré-Escolar por Esfera Administrativa----
    ##Mapa - Número de Matrículas no Ensino Pré-Escolar por Esfera Administrativa----
    t101 <- reactive({
      if (input$edu10ri == "Pará") {
        paste0(
          "Número Total de Matrículas no Ensino Pré-Escolar, Pará - ",
          input$edu10ano
        )
      } else{
        paste0(
          "Número Total de Matrículas no Ensino Pré-Escolar, Região de Integração ",
          input$edu10ri,
          " - ",
          input$edu10ano
        )
      }
    })
    ##Tabela - Número de Matrículas no Ensino Pré-Escolar por Esfera Administrativa----
    t102 <- reactive({
      if (input$edu10ri == "Pará") {
        paste0(
          "Número Total de Matrículas no Ensino Pré-Escolar por Esfera Administrativa e Município, Pará - ",
          input$edu10ano
        )
      } else{
        paste0(
          "Número Total de Matrículas no Ensino Pré-Escolar por Esfera Administrativa e Município, Região de Integração ",
          input$edu10ri,
          " - ",
          input$edu10ano
        )
      }
    })
    ##Gráfico - Número de Matrículas no Ensino Pré-Escolar por Esfera Administrativa----
    t103 <- reactive({
      paste0("Número de Matrículas no Ensino Pré-Escolar por Esfera Administrativa, Pará - ",min(edu10$ano)," a ",max(edu10$ano))
    })
    #11 - Número de Matrículas no Ensino Fundamental por Esfera Administrativa----
    ##Mapa - Número de Matrículas no Ensino Fundamental por Esfera Administrativa----
    t111 <- reactive({
      if (input$edu11ri == "Pará") {
        paste0(
          "Número Total de Matrículas no Ensino Fundamental, Pará - ",
          input$edu11ano
        )
      } else{
        paste0(
          "Número Total de Matrículas no Ensino Fundamental, Região de Integração ",
          input$edu11ri,
          " - ",
          input$edu11ano
        )
      }
    })
    ##Tabela - Número de Matrículas no Ensino Fundamental por Esfera Administrativa----
    t112 <- reactive({
      if (input$edu11ri == "Pará") {
        paste0(
          "Número Total de Matrículas no Ensino Fundamental por Esfera Administrativa e Município, Pará - ",
          input$edu11ano
        )
      } else{
        paste0(
          "Número Total de Matrículas no Ensino Fundamental por Esfera Administrativa e Município, Região de Integração ",
          input$edu11ri,
          " - ",
          input$edu11ano
        )
      }
    })
    ##Gráfico - Número de Matrículas no Ensino Fundamental por Esfera Administrativa----
    t113 <- reactive({
      paste0("Número de Matrículas no Ensino Fundamental por Esfera Administrativa, Pará - ",min(edu11$ano)," a ",max(edu11$ano))
    })
    #12 - Número de Matrículas no Ensino Médio por Esfera Administrativa----
    ##Mapa - Número de Matrículas no Ensino Médio por Esfera Administrativa----
    t121 <- reactive({
      if (input$edu12ri == "Pará") {
        paste0(
          "Número Total de Matrículas no Ensino Médio, Pará - ",
          input$edu12ano
        )
      } else{
        paste0(
          "Número Total de Matrículas no Ensino Médio, Região de Integração ",
          input$edu12ri,
          " - ",
          input$edu12ano
        )
      }
    })
    ##Tabela - Número de Matrículas no Ensino Médio por Esfera Administrativa----
    t122 <- reactive({
      if (input$edu12ri == "Pará") {
        paste0(
          "Número Total de Matrículas no Ensino Médio por Esfera Administrativa e Município, Pará - ",
          input$edu12ano
        )
      } else{
        paste0(
          "Número Total de Matrículas no Ensino Médio por Esfera Administrativa e Município, Região de Integração ",
          input$edu12ri,
          " - ",
          input$edu12ano
        )
      }
    })
    ##Gráfico - Número de Matrículas no Ensino Médio por Esfera Administrativa----
    t123 <- reactive({
      paste0("Número de Matrículas no Ensino Médio por Esfera Administrativa, Pará - ",min(edu12$ano)," a ",max(edu12$ano))
    })
    #13 - Média de Alunos por Turma por Nível de Ensino----
    ##Mapa - Média de Alunos por Turma por Nível de Ensino----
    t131 <- reactive({
      if (input$edu13ri == "Pará") {
        paste0(
          "Média de Alunos por Turma por Nível de Ensino - ",
          input$edu13cat,
          ", Pará - ",
          input$edu13ano
        )
      } else{
        paste0(
          "Média de Alunos por Turma por Nível de Ensino - ",
          input$edu13cat,
          ", Região de Integração ",
          input$edu13ri,
          " - ",
          input$edu13ano
        )
      }
    })
    ##Tabela - Média de Alunos por Turma por Nível de Ensino----
    t132 <- reactive({
      if (input$edu13ri == "Pará") {
        paste0(
          "Média de Alunos por Turma por Nível de Ensino, Pará - ",
          input$edu13ano
        )
      } else{
        paste0(
          "Média de Alunos por Turma por Nível de Ensino, Região de Integração - ",
          input$edu13ri,
          " - ",
          input$edu13ano
        )
      }
    })
    ##Gráfico - Média de Alunos por Turma por Nível de Ensino----
    t133 <- reactive({
      paste0("Média de Alunos por Turma por Nível de Ensino, Pará - ",min(edu13$ano)," a ",max(edu13$ano))
    })
    #14 - Número de Docentes no Ensino Pré-escolar por Esfera Administrativa----
    ##Mapa - Número de Docentes no Ensino Pré-escolar por Esfera Administrativa----
    t141 <- reactive({
      if (input$edu14ri == "Pará") {
        paste0(
          "Número Total de Docentes no Ensino Pré-escolar, Pará - ",
          input$edu14ano
        )
      } else{
        paste0(
          "Número Total de Docentes no Ensino Pré-escolar, Região de Integração ",
          input$edu14ri,
          " - ",
          input$edu14ano
        )
      }
    })
    ##Tabela - Número de Docentes no Ensino Pré-escolar por Esfera Administrativa----
    t142 <- reactive({
      if (input$edu14ri == "Pará") {
        paste0(
          "Número Total de Docentes no Ensino Pré-escolar por Esfera Administrativa e Município, Pará - ",
          input$edu14ano
        )
      } else{
        paste0(
          "Número Total de Docentes no Ensino Pré-escolar por Esfera Administrativa e Município, Região de Integração ",
          input$edu14ri,
          " - ",
          input$edu14ano
        )
      }
    })
    ##Gráfico - Número de Docentes no Ensino Pré-escolar por Esfera Administrativa----
    t143 <- reactive({
      paste0("Número de Docentes no Ensino Pré-escolar por Esfera Administrativa, Pará - ",min(edu14$ano)," a ",max(edu14$ano))
    })
    #15 - Número de Docentes no Ensino Fundamental por Esfera Administrativa----
    ##Mapa - Número de Docentes no Ensino Fundamental por Esfera Administrativa----
    t151 <- reactive({
      if (input$edu15ri == "Pará") {
        paste0(
          "Número Total de Docentes no Ensino Fundamental, Pará - ",
          input$edu15ano
        )
      } else{
        paste0(
          "Número Total de Docentes no Ensino Fundamental, Região de Integração ",
          input$edu15ri,
          " - ",
          input$edu15ano
        )
      }
    })
    ##Tabela - Número de Docentes no Ensino Fundamental por Esfera Administrativa----
    t152 <- reactive({
      if (input$edu15ri == "Pará") {
        paste0(
          "Número Total de Docentes no Ensino Fundamental por Esfera Administrativa e Município, Pará - ",
          input$edu15ano
        )
      } else{
        paste0(
          "Número Total de Docentes no Ensino Fundamental por Esfera Administrativa e Município, Região de Integração ",
          input$edu15ri,
          " - ",
          input$edu15ano
        )
      }
    })
    ##Gráfico - Número de Docentes no Ensino Fundamental por Esfera Administrativa----
    t153 <- reactive({
      paste0("Número de Docentes no Ensino Fundamental por Esfera Administrativa, Pará - ",min(edu15$ano)," a ",max(edu15$ano))
    })
    #16 - Número de Docentes no Ensino Médio por Esfera Administrativa----
    ##Mapa - Número de Docentes no Ensino Médio por Esfera Administrativa----
    t161 <- reactive({
      if (input$edu16ri == "Pará") {
        paste0(
          "Número Total de Docentes no Ensino Médio, Pará - ",
          input$edu16ano
        )
      } else{
        paste0(
          "Número Total de Docentes no Ensino Médio, Região de Integração ",
          input$edu16ri,
          " - ",
          input$edu16ano
        )
      }
    })
    ##Tabela - Número de Docentes no Ensino Médio por Esfera Administrativa----
    t162 <- reactive({
      if (input$edu16ri == "Pará") {
        paste0(
          "Número Total de Docentes no Ensino Médio por Esfera Administrativa e Município, Pará - ",
          input$edu16ano
        )
      } else{
        paste0(
          "Número Total de Docentes no Ensino Médio por Esfera Administrativa e Município, Região de Integração ",
          input$edu16ri,
          " - ",
          input$edu16ano
        )
      }
    })
    ##Gráfico - Número de Docentes no Ensino Médio por Esfera Administrativa----
    t163 <- reactive({
      paste0("Número de Docentes no Ensino Médio por Esfera Administrativa, Pará - ",min(edu16$ano)," a ",max(edu16$ano))
    })
    #17 - Estabelecimentos de Pré-Escola por Dependência Administrativa----
    ##Mapa - Estabelecimentos de Pré-Escola por Dependência Administrativa----
    t171 <- reactive({
      if (input$edu17ri == "Pará") {
        paste0(
          "Total de Estabelecimentos de Pré-Escola, Pará - ",
          input$edu17ano
        )
      } else{
        paste0(
          "Total de Estabelecimentos de Pré-Escola, Região de Integração ",
          input$edu17ri,
          " - ",
          input$edu17ano
        )
      }
    })
    ##Tabela - Estabelecimentos de Pré-Escola por Dependência Administrativa----
    t172 <- reactive({
      if (input$edu17ri == "Pará") {
        paste0(
          "Total de Estabelecimentos de Pré-Escola por Dependência Administrativa e Município, Pará - ",
          input$edu17ano
        )
      } else{
        paste0(
          "Total de Estabelecimentos de Pré-Escola por Dependência Administrativa e Município, Região de Integração ",
          input$edu17ri,
          " - ",
          input$edu17ano
        )
      }
    })
    ##Gráfico - Estabelecimentos de Pré-Escola por Dependência Administrativa----
    t173 <- reactive({
      paste0("Estabelecimentos de Pré-Escola por Dependência Administrativa, Pará - ",min(edu17$ano)," a ",max(edu17$ano))
    })
    #18 - Estabelecimentos de Ensino Fundamental por Dependência Administrativa----
    ##Mapa - Estabelecimentos de Ensino Fundamental por Dependência Administrativa----
    t181 <- reactive({
      if (input$edu18ri == "Pará") {
        paste0(
          "Total de Estabelecimentos de Ensino Fundamental, Pará - ",
          input$edu18ano
        )
      } else{
        paste0(
          "Total de Estabelecimentos de Ensino Fundamental, Região de Integração ",
          input$edu18ri,
          " - ",
          input$edu18ano
        )
      }
    })
    ##Tabela - Estabelecimentos de Ensino Fundamental por Dependência Administrativa----
    t182 <- reactive({
      if (input$edu18ri == "Pará") {
        paste0(
          "Total de Estabelecimentos de Ensino Fundamental por Dependência Administrativa e Município, Pará - ",
          input$edu18ano
        )
      } else{
        paste0(
          "Total de Estabelecimentos de Ensino Fundamental por Dependência Administrativa e Município, Região de Integração ",
          input$edu18ri,
          " - ",
          input$edu18ano
        )
      }
    })
    ##Gráfico - Estabelecimentos de Ensino Fundamental por Dependência Administrativa----
    t183 <- reactive({
      paste0("Estabelecimentos de Ensino Fundamental por Dependência Administrativa, Pará - ",min(edu18$ano)," a ",max(edu18$ano))
    })
    #19 - Estabelecimentos de Ensino Médio por Dependência Administrativa----
    ##Mapa - Estabelecimentos de Ensino Médio por Dependência Administrativa----
    t191 <- reactive({
      if (input$edu19ri == "Pará") {
        paste0(
          "Total de Estabelecimentos de Ensino Médio, Pará - ",
          input$edu19ano
        )
      } else{
        paste0(
          "Total de Estabelecimentos de Ensino Médio, Região de Integração ",
          input$edu19ri,
          " - ",
          input$edu19ano
        )
      }
    })
    ##Tabela - Estabelecimentos de Ensino Médio por Dependência Administrativa----
    t192 <- reactive({
      if (input$edu19ri == "Pará") {
        paste0(
          "Total de Estabelecimentos de Ensino Médio por Dependência Administrativa e Município, Pará - ",
          input$edu19ano
        )
      } else{
        paste0(
          "Total de Estabelecimentos de Ensino Médio por Dependência Administrativa e Município, Região de Integração ",
          input$edu19ri,
          " - ",
          input$edu19ano
        )
      }
    })
    ##Gráfico - Estabelecimentos de Ensino Médio por Dependência Administrativa----
    t193 <- reactive({
      paste0("Estabelecimentos de Ensino Médio por Dependência Administrativa, Pará - ",min(edu19$ano)," a ",max(edu19$ano))
    })
    #VISUALIZAÇÃO----
    #1 - Taxa de Aprovação no Ensino Fundamental por Dependência Administrativa----
    ##Mapa - Taxa de Aprovação no Ensino Fundamental por Dependência Administrativa----
    output$edu1txt1 <- renderText({
      t11()
    })
    
    output$edu1map <- renderLeaflet({
      #Tratamento da informação
      ##Filtrando informação

      if (input$edu1ri == "Pará") {
        df <- edu1 %>% filter(localidade !="Pará",ano == input$edu1ano, categoria == "Total") %>%
          select(ri,localidade,ano,valor)
        x <- cbind(geopa,df)
        x$valor[x$valor == 0] <- NA
      }      else{
        df <- edu1 %>% filter(localidade !="Pará",ano == input$edu1ano, categoria == "Total") %>%
          select(ri,localidade,ano,valor)
        x <- cbind(geopa,df)
        x <- x %>% filter(ri == input$edu1ri)
        x$valor[x$valor == 0] <- NA
      }
      
      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }
      
      pal <- colorBin(c("#FFEBCD","#ffcd97","#ffaf66","#fa9236","#f17701"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Taxa:</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
            format(x$valor, big.mark = ".", decimal.mark = ",")
          )
        ) %>% lapply(htmltools::HTML)
      
      #Mapas com leafleft
      leaflet(x,options = leafletOptions(minZoom = 0,maxZoom = 15)) %>%
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
          labFormat = labelFormat_decimal(big.mark = ".",decimal.mark = ",",digits = 2)
        )
    })
    ##Tabela - Taxa de Aprovação no Ensino Fundamental por Esfera Administrativa----
    output$edu1txt2 <- renderText({
      t12()
    })
    output$edu1tab <- renderReactable({
      if (input$edu1ri == "Pará") {
        x <- edu1 %>% filter(localidade !="Pará",ano == input$edu1ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
      }      else{
        x <- edu1 %>% filter(localidade !="Pará",ano == input$edu1ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
        x <- x %>% filter(ri == input$edu1ri)
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
            ri = colDef(name = "Região de Integração",width = 170),
            localidade = colDef(name = "Municípios",width = 150),
            Federal = colDef(format = colFormat(separators = T,locales = "pt-BR")),
            Estadual = colDef(format = colFormat(separators = T,locales = "pt-BR")),
            Municipal = colDef(format = colFormat(separators = T,locales = "pt-BR")),
            Privada = colDef(format = colFormat(separators = T,locales = "pt-BR")),
            `Aprovação Total` = colDef(name = "Total",format = colFormat(separators = T,locales = "pt-BR"))
          ),
          defaultColDef = colDef(footerStyle = list(fontWeight = "bold"),
                                 headerStyle = list(background = "#f7f7f8")),
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
    ##Gráfico - Taxa de Aprovação no Ensino Fundamental por Esfera Administrativa----
    output$edu1txt3 <- renderText({
      t13()
    })
    output$edu1graf <- renderEcharts4r({
      edu1 %>% filter(localidade == "Pará") %>% 
        pivot_wider(names_from = categoria, values_from = valor) %>% 
        e_chart(x = ano) %>%
        e_line(
          serie = Federal,
          name = "Federal",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Estadual,
          name = "Estadual",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Municipal,
          name = "Municipal",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Privado,
          name = "Privada",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = `Total`,
          name = "Total",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_tooltip(
          trigger = "axis",
          formatter = e_tooltip_pointer_formatter("decimal",digits = 1,locale = "pt-BR"),
          axisPointer =list(type = 'shadow')
        ) %>% 
         e_x_axis(
            axisLabel = list(show = T,fontSize = 11),
            name = "Ano",
            splitLine = list(show = T),
            nameTextStyle = list(fontWeight = "bold", fontSize = 14)
          ) %>%
        e_y_axis(
          name = "Taxa",
          nameTextStyle = list(fontWeight = "bold",fontSize = 14),
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
          e_datazoom(toolbox = F,  fillerColor = "#E5F5F9") %>%
          e_grid(show = T)
    })
    #2 - Taxa de Aprovação no Ensino Médio por Esfera Administrativa----
    ##Mapa - Taxa de Aprovação no Ensino Médio por Esfera Administrativa----
    output$edu2txt1 <- renderText({
      t21()
    })
    
    output$edu2map <- renderLeaflet({
      #Tratamento da informação
      ##Filtrando informação
      if (input$edu2ri == "Pará") {
        df <- edu2 %>% filter(localidade !="Pará",ano == input$edu2ano, categoria == "Aprovação Total") %>% 
          select(ri,localidade,ano,valor)
        x <- cbind(geopa,df)
        x$valor[x$valor == 0] <- NA
      }      else{
        df <- edu2 %>% filter(localidade !="Pará",ano == input$edu2ano, categoria == "Aprovação Total") %>% 
          select(ri,localidade,ano,valor)
        x <- cbind(geopa,df)
        x <- x %>% filter(ri == input$edu2ri)
        x$valor[x$valor == 0] <- NA
      }
      
      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }
      
      pal <- colorBin(c("#FFEBCD","#ffcd97","#ffaf66","#fa9236","#f17701"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Taxa:</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
            format(x$valor, big.mark = ".", decimal.mark = ",")
          )
        ) %>% lapply(htmltools::HTML)
      #Mapas com leafleft
      leaflet(x,options = leafletOptions(minZoom = 0,maxZoom = 15)) %>%
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
          labFormat = labelFormat_decimal(big.mark = ".",decimal.mark = ",",digits = 2)
        )
    })
    ##Tabela - Taxa de Aprovação no Ensino Médio por Esfera Administrativa----
    output$edu2txt2 <- renderText({
      t22()
    })
    output$edu2tab <- renderReactable({
      if (input$edu2ri == "Pará") {
        x <- edu2 %>% filter(localidade !="Pará",ano == input$edu2ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
      }      else{
        x <- edu2 %>% filter(localidade !="Pará",ano == input$edu2ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
        x <- x %>% filter(ri == input$edu2ri)
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
          ri = colDef(name = "Região de Integração",width = 170),
          localidade = colDef(name = "Municípios",width = 150),
          Federal = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          Estadual = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          Municipal = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          Privada = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          `Aprovação Total` = colDef(name = "Total",format = colFormat(separators = T,locales = "pt-BR"))
        ),
        defaultColDef = colDef(footerStyle = list(fontWeight = "bold"),
                               headerStyle = list(background = "#f7f7f8")),
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
    ##Gráfico - Taxa de Aprovação no Ensino Médio por Esfera Administrativa----
    output$edu2txt3 <- renderText({
      t23()
    })
    output$edu2graf <- renderEcharts4r({
      edu2 %>% filter(localidade == "Pará") %>% 
        pivot_wider(names_from = categoria, values_from = valor) %>% 
        e_chart(x = ano) %>%
        e_line(
          serie = Federal,
          name = "Federal",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Estadual,
          name = "Estadual",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Municipal,
          name = "Municipal",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Privada,
          name = "Privada",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = `Aprovação Total`,
          name = "Total",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
         e_tooltip(
          trigger = "axis",
          formatter = e_tooltip_pointer_formatter("decimal",digits = 1,locale = "pt-BR"),
          axisPointer =list(type = 'shadow')
        ) %>% 
         e_x_axis(
            axisLabel = list(show = T,fontSize = 11),
            name = "Ano",
            splitLine = list(show = T),
            nameTextStyle = list(fontWeight = "bold", fontSize = 14)
          ) %>%
        e_y_axis(
          name = "Taxa",
          nameTextStyle = list(fontWeight = "bold",fontSize = 14),
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
          e_datazoom(toolbox = F,  fillerColor = "#E5F5F9") %>%
          e_grid(show = T)
    })
    #3 - Taxa de Reprovação no Ensino Fundamental por Esfera Administrativa----
    ##Mapa - Taxa de Reprovação no Ensino Fundamental por Esfera Administrativa----
    output$edu3txt1 <- renderText({
      t31()
    })
    output$edu3map <- renderLeaflet({
      #Tratamento da informação
      ##Filtrando informação
      if (input$edu3ri == "Pará") {
        df <- edu3 %>% filter(localidade !="Pará",ano == input$edu3ano, categoria == "Reprovação Total") %>% 
          select(ri,localidade,ano,valor)
        x <- cbind(geopa,df)
      }      else{
        df <- edu3 %>% filter(localidade !="Pará",ano == input$edu3ano, categoria == "Reprovação Total") %>% 
          select(ri,localidade,ano,valor)
        x <- cbind(geopa,df)
        x <- x %>% filter(ri == input$edu3ri)
      }
      
      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }
      
      pal <- colorBin(c("#FFEBCD","#ffcd97","#ffaf66","#fa9236","#f17701"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Taxa:</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
            format(x$valor, big.mark = ".", decimal.mark = ",")
          )
        ) %>% lapply(htmltools::HTML)
      
      #Mapas com leafleft
      leaflet(x,options = leafletOptions(minZoom = 0,maxZoom = 15)) %>%
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
          labFormat = labelFormat_decimal(big.mark = ".",decimal.mark = ",",digits = 2)
        )
    })
    ##Tabela - Taxa de Reprovação no Ensino Fundamental por Esfera Administrativa----
    output$edu3txt2 <- renderText({
      t32()
    })
    output$edu3tab <- renderReactable({
      if (input$edu3ri == "Pará") {
        x <- edu3 %>% filter(localidade !="Pará",ano == input$edu3ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
      }      else{
        x <- edu3 %>% filter(localidade !="Pará",ano == input$edu3ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
        x <- x %>% filter(ri == input$edu3ri)
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
          ri = colDef(name = "Região de Integração",width = 170),
          localidade = colDef(name = "Municípios",width = 150),
          Federal = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          Estadual = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          Municipal = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          Privada = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          `Reprovação Total` = colDef(name = "Total",format = colFormat(separators = T,locales = "pt-BR"))
        ),
        defaultColDef = colDef(footerStyle = list(fontWeight = "bold"),
                               headerStyle = list(background = "#f7f7f8")),
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
    ##Gráfico - Taxa de Reprovação no Ensino Fundamental por Esfera Administrativa----
    output$edu3txt3 <- renderText({
      t33()
    })
    output$edu3graf <- renderEcharts4r({
      edu3 %>% filter(localidade == "Pará") %>% 
        pivot_wider(names_from = categoria, values_from = valor) %>% 
        e_chart(x = ano) %>%
        e_line(
          serie = Federal,
          name = "Federal",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Estadual,
          name = "Estadual",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Municipal,
          name = "Municipal",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Privada,
          name = "Privada",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = `Reprovação Total`,
          name = "Total",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_tooltip(
          trigger = "axis",
          formatter = e_tooltip_pointer_formatter("decimal",digits = 1,locale = "pt-BR"),
          axisPointer =list(type = 'shadow')
        ) %>% 
         e_x_axis(
            axisLabel = list(show = T,fontSize = 11),
            name = "Ano",
            splitLine = list(show = T),
            nameTextStyle = list(fontWeight = "bold", fontSize = 14)
          ) %>%
        e_y_axis(
          name = "Taxa",
          nameTextStyle = list(fontWeight = "bold",fontSize = 14),
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
          e_datazoom(toolbox = F,  fillerColor = "#E5F5F9") %>%
          e_grid(show = T)
    })
    #4 - Taxa de Reprovação no Ensino Médio por Esfera Administrativa----
    ##Mapa - Taxa de Reprovação no Ensino Médio por Esfera Administrativa----
    output$edu4txt1 <- renderText({
      t41()
    })
    output$edu4map <- renderLeaflet({
      #Tratamento da informação
      ##Filtrando informação
      if (input$edu4ri == "Pará") {
        df <- edu4 %>% filter(localidade !="Pará",ano == input$edu4ano, categoria == "Reprovação Total") %>% 
          select(ri,localidade,ano,valor)
        x <- cbind(geopa,df)
      }      else{
        df <- edu4 %>% filter(localidade !="Pará",ano == input$edu4ano, categoria == "Reprovação Total") %>% 
          select(ri,localidade,ano,valor)
        x <- cbind(geopa,df)
        x <- x %>% filter(ri == input$edu4ri)
      }
      
      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }
      
      pal <- colorBin(c("#FFEBCD","#ffcd97","#ffaf66","#fa9236","#f17701"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Taxa:</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
            format(x$valor, big.mark = ".", decimal.mark = ",")
          )
        ) %>% lapply(htmltools::HTML)
      
      #Mapas com leafleft
      leaflet(x,options = leafletOptions(minZoom = 0,maxZoom = 15)) %>%
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
          labFormat = labelFormat_decimal(big.mark = ".",decimal.mark = ",",digits = 2)
        )
    })
    ##Tabela - Taxa de Reprovação no Ensino Médio por Esfera Administrativa----
    output$edu4txt2 <- renderText({
      t42()
    })
    output$edu4tab <- renderReactable({
      if (input$edu4ri == "Pará") {
        x <- edu4 %>% filter(localidade !="Pará",ano == input$edu4ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
      }      else{
        x <- edu4 %>% filter(localidade !="Pará",ano == input$edu4ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
        x <- x %>% filter(ri == input$edu4ri)
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
          ri = colDef(name = "Região de Integração",width = 170),
          localidade = colDef(name = "Municípios"),
          Federal = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          Estadual = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          Municipal = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          Privada = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          `Reprovação Total` = colDef(name = "Total",format = colFormat(separators = T,locales = "pt-BR"))
        ),
        defaultColDef = colDef(footerStyle = list(fontWeight = "bold"),
                               headerStyle = list(background = "#f7f7f8")),
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
    ##Gráfico - Taxa de Reprovação no Ensino Médio por Esfera Administrativa----
    output$edu4txt3 <- renderText({
      t43()
    })
    output$edu4graf <- renderEcharts4r({
      edu4 %>% filter(localidade == "Pará") %>% 
        pivot_wider(names_from = categoria, values_from = valor) %>% 
        e_chart(x = ano) %>%
        e_line(
          serie = Federal,
          name = "Federal",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Estadual,
          name = "Estadual",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Municipal,
          name = "Municipal",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Privada,
          name = "Privada",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = `Reprovação Total`,
          name = "Total",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_tooltip(
          trigger = "axis",
          formatter = e_tooltip_pointer_formatter("decimal",digits = 1,locale = "pt-BR"),
          axisPointer =list(type = 'shadow')
        ) %>% 
         e_x_axis(
            axisLabel = list(show = T,fontSize = 11),
            name = "Ano",
            splitLine = list(show = T),
            nameTextStyle = list(fontWeight = "bold", fontSize = 14)
          ) %>%
        e_y_axis(
          name = "Taxa",
          nameTextStyle = list(fontWeight = "bold",fontSize = 14),
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
          e_datazoom(toolbox = F,  fillerColor = "#E5F5F9") %>%
          e_grid(show = T)
    })
    #5 - Taxa de Abandono no Ensino Fundamental por Esfera Administrativa----
    ##Mapa - Taxa de Abandono no Ensino Fundamental por Esfera Administrativa----
    output$edu5txt1 <- renderText({
      t51()
    })
    output$edu5map <- renderLeaflet({
      #Tratamento da informação
      ##Filtrando informação
      if (input$edu5ri == "Pará") {
        df <- edu5 %>% filter(localidade !="Pará",ano == input$edu5ano, categoria == "Evasão Total") %>% 
          select(ri,localidade,ano,valor)
        x <- cbind(geopa,df)
      }      else{
        df <- edu5 %>% filter(localidade !="Pará",ano == input$edu5ano, categoria == "Evasão Total") %>% 
          select(ri,localidade,ano,valor)
        x <- cbind(geopa,df)
        x <- x %>% filter(ri == input$edu5ri)
      }
      
      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }
      
      pal <- colorBin(c("#FFEBCD","#ffcd97","#ffaf66","#fa9236","#f17701"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Taxa:</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
            format(x$valor, big.mark = ".", decimal.mark = ",")
          )
        ) %>% lapply(htmltools::HTML)
      
      #Mapas com leafleft
      leaflet(x,options = leafletOptions(minZoom = 0,maxZoom = 15)) %>%
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
          labFormat = labelFormat_decimal(big.mark = ".",decimal.mark = ",",digits = 2)
        )
    })
    ##Tabela - Taxa de Abandono no Ensino Fundamental por Esfera Administrativa----
    output$edu5txt2 <- renderText({
      t52()
    })
    output$edu5tab <- renderReactable({
      if (input$edu5ri == "Pará") {
        x <- edu5 %>% filter(localidade !="Pará",ano == input$edu5ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
      }      else{
        x <- edu5 %>% filter(localidade !="Pará",ano == input$edu5ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
        x <- x %>% filter(ri == input$edu5ri)
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
          ri = colDef(name = "Região de Integração",width = 170),
          localidade = colDef(name = "Municípios"),
          Federal = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          Estadual = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          Municipal = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          Privada = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          `Evasão Total` = colDef(name = "Total",format = colFormat(separators = T,locales = "pt-BR"))
        ),
        defaultColDef = colDef(footerStyle = list(fontWeight = "bold"),
                               headerStyle = list(background = "#f7f7f8")),
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
    ##Gráfico - Taxa de Abandono no Ensino Fundamental por Esfera Administrativa----
    output$edu5txt3 <- renderText({
      t53()
    })
    output$edu5graf <- renderEcharts4r({
      edu5 %>% filter(localidade == "Pará") %>% 
        pivot_wider(names_from = categoria, values_from = valor) %>% 
        e_chart(x = ano) %>%
        e_line(
          serie = Federal,
          name = "Federal",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Estadual,
          name = "Estadual",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Municipal,
          name = "Municipal",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Privada,
          name = "Privada",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = `Evasão Total`,
          name = "Total",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_tooltip(
          trigger = "axis",
          formatter = e_tooltip_pointer_formatter("decimal",digits = 1,locale = "pt-BR"),
          axisPointer =list(type = 'shadow')
        ) %>% 
         e_x_axis(
            axisLabel = list(show = T,fontSize = 11),
            name = "Ano",
            splitLine = list(show = T),
            nameTextStyle = list(fontWeight = "bold", fontSize = 14)
          ) %>%
        e_y_axis(
          name = "Taxa",
          nameTextStyle = list(fontWeight = "bold",fontSize = 14),
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
          e_datazoom(toolbox = F,  fillerColor = "#E5F5F9") %>%
          e_grid(show = T)
    })
    #6 - Taxa de Abandono no Ensino Médio por Esfera Administrativa----
    ##Mapa - Taxa de Abandono no Ensino Médio por Esfera Administrativa----
    output$edu6txt1 <- renderText({
      t61()
    })
    output$edu6map <- renderLeaflet({
      #Tratamento da informação
      ##Filtrando informação
      if (input$edu6ri == "Pará") {
        df <- edu6 %>% filter(localidade !="Pará",ano == input$edu6ano, categoria == "Evasão Total") %>% 
          select(ri,localidade,ano,valor)
        x <- cbind(geopa,df)
      }      else{
        df <- edu6 %>% filter(localidade !="Pará",ano == input$edu6ano, categoria == "Evasão Total") %>% 
          select(ri,localidade,ano,valor)
        x <- cbind(geopa,df)
        x <- x %>% filter(ri == input$edu6ri)
      }
      
      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }
      
      pal <- colorBin(c("#FFEBCD","#ffcd97","#ffaf66","#fa9236","#f17701"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Taxa:</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
            format(x$valor, big.mark = ".", decimal.mark = ",")
          )
        ) %>% lapply(htmltools::HTML)
      
      #Mapas com leafleft
      leaflet(x,options = leafletOptions(minZoom = 0,maxZoom = 15)) %>%
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
          labFormat = labelFormat_decimal(big.mark = ".",decimal.mark = ",",digits = 2)
        )
    })
    ##Tabela - Taxa de Abandono no Ensino Médio por Esfera Administrativa----
    output$edu6txt2 <- renderText({
      t62()
    })
    output$edu6tab <- renderReactable({
      if (input$edu6ri == "Pará") {
        x <- edu6 %>% filter(localidade !="Pará",ano == input$edu6ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
      }      else{
        x <- edu6 %>% filter(localidade !="Pará",ano == input$edu6ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
        x <- x %>% filter(ri == input$edu6ri)
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
          ri = colDef(name = "Região de Integração",width = 170),
          localidade = colDef(name = "Municípios"),
          Federal = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          Estadual = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          Municipal = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          Privada = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          `Evasão Total` = colDef(name = "Total",format = colFormat(separators = T,locales = "pt-BR"))
        ),
        defaultColDef = colDef(footerStyle = list(fontWeight = "bold"),
                               headerStyle = list(background = "#f7f7f8")),
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
    ##Gráfico - Taxa de Abandono no Ensino Médio por Esfera Administrativa----
    output$edu6txt3 <- renderText({
      t63()
    })
    output$edu6graf <- renderEcharts4r({
      edu6 %>% filter(localidade == "Pará") %>% 
        pivot_wider(names_from = categoria, values_from = valor) %>% 
        e_chart(x = ano) %>%
        e_line(
          serie = Federal,
          name = "Federal",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Estadual,
          name = "Estadual",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Municipal,
          name = "Municipal",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Privada,
          name = "Privada",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = `Evasão Total`,
          name = "Total",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_tooltip(
          trigger = "axis",
          formatter = e_tooltip_pointer_formatter("decimal",digits = 1,locale = "pt-BR"),
          axisPointer =list(type = 'shadow')
        ) %>% 
         e_x_axis(
            axisLabel = list(show = T,fontSize = 11),
            name = "Ano",
            splitLine = list(show = T),
            nameTextStyle = list(fontWeight = "bold", fontSize = 14)
          ) %>%
        e_y_axis(
          name = "Taxa",
          nameTextStyle = list(fontWeight = "bold",fontSize = 14),
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
          e_datazoom(toolbox = F,  fillerColor = "#E5F5F9") %>%
          e_grid(show = T)
    })
    #7 - Distorção Idade-Série Total por Nível de Ensino----
    ##Mapa - Distorção Idade-Série Total por Nível de Ensino----
    output$edu7txt1 <- renderText({
      t71()
    })
    output$edu7map <- renderLeaflet({
      #Tratamento da informação
      ##Filtrando informação
      if (input$edu7ri == "Pará") {
        df <- edu7 %>% filter(localidade !="Pará",ano == input$edu7ano, categoria == input$edu7cat) %>% 
          select(ri,localidade,ano,valor)
        x <- cbind(geopa,df)
      }      else{
        df <- edu7 %>% filter(localidade !="Pará",ano == input$edu7ano, categoria == input$edu7cat) %>% 
          select(ri,localidade,ano,valor)
        x <- cbind(geopa,df)
        x <- x %>% filter(ri == input$edu7ri)
      }
      
      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }
      
      pal <- colorBin(c("#FFEBCD","#ffcd97","#ffaf66","#fa9236","#f17701"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Distorção:</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
            format(x$valor, big.mark = ".", decimal.mark = ",")
          )
        ) %>% lapply(htmltools::HTML)
      
      #Mapas com leafleft
      leaflet(x,options = leafletOptions(minZoom = 0,maxZoom = 15)) %>%
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
          title = "Distorção",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".",decimal.mark = ",",digits = 2)
        )
    })
    ##Tabela - Distorção Idade-Série Total por Nível de Ensino----
    output$edu7txt2 <- renderText({
      t72()
    })
    output$edu7tab <- renderReactable({
      if (input$edu7ri == "Pará") {
        x <- edu7 %>% filter(localidade !="Pará",ano == input$edu7ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
      }      else{
        x <- edu7 %>% filter(localidade !="Pará",ano == input$edu7ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
        x <- x %>% filter(ri == input$edu7ri)
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
          ri = colDef(name = "Região de Integração",width = 170),
          `Ensino Fundamental` = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          `Ensino Médio` = colDef(format = colFormat(separators = T,locales = "pt-BR"))
        ),
        defaultColDef = colDef(footerStyle = list(fontWeight = "bold"),
                               headerStyle = list(background = "#f7f7f8")),
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
    ##Gráfico - Distorção Idade-Série Total por Nível de Ensino----
    output$edu7txt3 <- renderText({
      t73()
    })
    output$edu7graf <- renderEcharts4r({
      edu7 %>% filter(localidade == "Pará") %>% 
        pivot_wider(names_from = categoria, values_from = valor) %>% 
        e_chart(x = ano) %>%
        e_line(
          serie = `Ensino Fundamental`,
          name = "Ensino Fundamental",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = `Ensino Médio`,
          name = "Ensino Médio",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_tooltip(
          trigger = "axis",
          formatter = e_tooltip_pointer_formatter("decimal",digits = 1,locale = "pt-BR"),
          axisPointer =list(type = 'shadow')
        ) %>% 
         e_x_axis(
            axisLabel = list(show = T,fontSize = 11),
            name = "Ano",
            splitLine = list(show = T),
            nameTextStyle = list(fontWeight = "bold", fontSize = 14)
          ) %>%
        e_y_axis(
          name = "Distorção",
          nameTextStyle = list(fontWeight = "bold",fontSize = 14),
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
          e_datazoom(toolbox = F,  fillerColor = "#E5F5F9") %>%
          e_grid(show = T)
    })
    #8 -  IDEB - Escola pública - 5ª ano (séries iniciais)----
    ##Mapa - DDEB - Escola pública - 5ª ano (séries iniciais)----
    output$edu8txt1 <- renderText({
      t81()
    })
    output$edu8map <- renderLeaflet({
      #Tratamento da informação
      ##Filtrando informação
      if (input$edu8ri == "Pará") {
        df <- edu8 %>% filter(localidade !="Pará",ano == input$edu8ano) %>% 
          select(ri,localidade,ano,valor)
        x <- cbind(geopa,df)
      }      else{
        df <- edu8 %>% filter(localidade !="Pará",ano == input$edu8ano) %>% 
          select(ri,localidade,ano,valor)
        x <- cbind(geopa,df)
        x <- x %>% filter(ri == input$edu8ri)
      }
      
      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }
      
      pal <- colorBin(c("#FFEBCD","#ffcd97","#ffaf66","#fa9236","#f17701"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Índice:</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
            format(x$valor, big.mark = ".", decimal.mark = ",")
          )
        ) %>% lapply(htmltools::HTML)
      
      #Mapas com leafleft
      leaflet(x,options = leafletOptions(minZoom = 0,maxZoom = 15)) %>%
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
          labFormat = labelFormat_decimal(big.mark = ".",decimal.mark = ",",digits = 2)
        )
    })
    ##Tabela - DDEB - Escola pública - 5ª ano (séries iniciais)----
    output$edu8txt2 <- renderText({
      t82()
    })
    output$edu8tab <- renderReactable({
      if (input$edu8ri == "Pará") {
        x <- edu8 %>% filter(localidade !="Pará",ano == input$edu8ano) %>% 
          select(ri,localidade,valor) 
      }      else{
        x <- edu8 %>% filter(localidade !="Pará",ano == input$edu8ano) %>% 
          select(ri,localidade,valor) 
        x <- x %>% filter(ri == input$edu8ri)
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
          ri = colDef(name = "Região de Integração",width = 170),
          localidade = colDef(name = "Municípios"),
          valor = colDef(name = "Índice",format = colFormat(separators = T,locales = "pt-BR"))
        ),
        defaultColDef = colDef(footerStyle = list(fontWeight = "bold"),
                               headerStyle = list(background = "#f7f7f8")),
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
    ##Gráfico - DEB - Escola pública - 5ª ano (séries iniciais)----
    output$edu8txt3 <- renderText({
      t83()
    })
    output$edu8graf <- renderEcharts4r({
      edu8 %>% filter(localidade == "Pará") %>% 
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          name = "Índice",
          legend = F,
          color = "#f17701",
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_tooltip(
          trigger = "axis",
          formatter = e_tooltip_pointer_formatter("decimal",digits = 1,locale = "pt-BR"),
          axisPointer =list(type = 'shadow')
        ) %>% 
         e_x_axis(
            axisLabel = list(show = T,fontSize = 11),
            name = "Ano",
            splitLine = list(show = T),
            nameTextStyle = list(fontWeight = "bold", fontSize = 14)
          ) %>%
        e_y_axis(
          name = "Índice",
          nameTextStyle = list(fontWeight = "bold",fontSize = 14),
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
          e_datazoom(toolbox = F,  fillerColor = "#E5F5F9") %>%
          e_grid(show = T)
    })
    #9 -  IDEB - Escola pública - 9ª ano (séries finais)----
    ##Mapa - DDEB - Escola pública - 9ª ano (séries finais)----
    output$edu9txt1 <- renderText({
      t91()
    })
    output$edu9map <- renderLeaflet({
      #Tratamento da informação
      ##Filtrando informação
      if (input$edu9ri == "Pará") {
        df <- edu9 %>% filter(localidade !="Pará",ano == input$edu9ano) %>% 
          select(ri,localidade,ano,valor)
        x <- cbind(geopa,df)
      }      else{
        df <- edu9 %>% filter(localidade !="Pará",ano == input$edu9ano) %>% 
          select(ri,localidade,ano,valor)
        x <- cbind(geopa,df)
        x <- x %>% filter(ri == input$edu9ri)
      }
      
      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }
      
      pal <- colorBin(c("#FFEBCD","#ffcd97","#ffaf66","#fa9236","#f17701"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Índice:</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
            format(x$valor, big.mark = ".", decimal.mark = ",")
          )
        ) %>% lapply(htmltools::HTML)
      
      #Mapas com leafleft
      leaflet(x,options = leafletOptions(minZoom = 0,maxZoom = 15)) %>%
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
          labFormat = labelFormat_decimal(big.mark = ".",decimal.mark = ",",digits = 2)
        )
    })
    ##Tabela - DDEB - Escola pública - 9ª ano (séries finais)----
    output$edu9txt2 <- renderText({
      t92()
    })
    output$edu9tab <- renderReactable({
      if (input$edu9ri == "Pará") {
        x <- edu9 %>% filter(localidade !="Pará",ano == input$edu9ano) %>% 
          select(ri,localidade,valor) 
      }      else{
        x <- edu9 %>% filter(localidade !="Pará",ano == input$edu9ano) %>% 
          select(ri,localidade,valor) 
        x <- x %>% filter(ri == input$edu9ri)
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
          ri = colDef(name = "Região de Integração",width = 170),
          localidade = colDef(name = "Municípios"),
          valor = colDef(name = "Índice",format = colFormat(separators = T,locales = "pt-BR"))
        ),
        defaultColDef = colDef(footerStyle = list(fontWeight = "bold"),
                               headerStyle = list(background = "#f7f7f8")),
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
    ##Gráfico - DEB - Escola pública - 9ª ano (séries finais)----
    output$edu9txt3 <- renderText({
      t93()
    })
    output$edu9graf <- renderEcharts4r({
      edu9 %>% filter(localidade == "Pará") %>% 
        e_chart(x = ano) %>%
        e_line(
          serie = valor,
          name = "Índice",
          color = "#f17701",
          legend = F,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_tooltip(
          trigger = "axis",
          formatter = e_tooltip_pointer_formatter("decimal",digits = 1,locale = "pt-BR"),
          axisPointer =list(type = 'shadow')
        ) %>% 
         e_x_axis(
            axisLabel = list(show = T,fontSize = 11),
            name = "Ano",
            splitLine = list(show = T),
            nameTextStyle = list(fontWeight = "bold", fontSize = 14)
          ) %>%
        e_y_axis(
          name = "Índice",
          nameTextStyle = list(fontWeight = "bold",fontSize = 14),
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
          e_datazoom(toolbox = F,  fillerColor = "#E5F5F9") %>%
          e_grid(show = T)
    })
    #10 - Número de Matrículas no Ensino Pré-Escolar por Esfera Administrativa----
    ##Mapa - Número de Matrículas no Ensino Pré-Escolar por Esfera Administrativa----
    output$edu10txt1 <- renderText({
      t101()
    })
    output$edu10map <- renderLeaflet({
      #Tratamento da informação
      ##Filtrando informação
      if (input$edu10ri == "Pará") {
        df <- edu10 %>% filter(localidade !="Pará",ano == input$edu10ano, categoria == "Total") %>% 
          select(ri,localidade,ano,valor)
        x <- cbind(geopa,df)
        x$valor[x$valor == 0] <- NA
      }      else{
        df <- edu10 %>% filter(localidade !="Pará",ano == input$edu10ano, categoria == "Total") %>% 
          select(ri,localidade,ano,valor)
        x <- cbind(geopa,df)
        x <- x %>% filter(ri == input$edu10ri)
        x$valor[x$valor == 0] <- NA
      }
      
      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }
      
      pal <- colorBin(c("#FFEBCD","#ffcd97","#ffaf66","#fa9236","#f17701"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Número de Matrículas:</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
            format(x$valor, big.mark = ".", decimal.mark = ",")
          )
        ) %>% lapply(htmltools::HTML)
      
      #Mapas com leafleft
      leaflet(x,options = leafletOptions(minZoom = 0,maxZoom = 15)) %>%
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
          title = "Número de Matrículas",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".",decimal.mark = ",",digits = 2)
        )
    })
    ##Tabela - Número de Matrículas no Ensino Pré-Escolar por Esfera Administrativa----
    output$edu10txt2 <- renderText({
      t102()
    })
    output$edu10tab <- renderReactable({
      if (input$edu10ri == "Pará") {
        x <- edu10 %>% filter(localidade !="Pará",ano == input$edu10ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
      }      else{
        x <- edu10 %>% filter(localidade !="Pará",ano == input$edu10ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
        x <- x %>% filter(ri == input$edu10ri)
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
          ri = colDef(name = "Região de Integração",width = 170),
          localidade = colDef(name = "Municípios",width = 150),
          Federal = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          Estadual = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          Municipal = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          Privada = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          Total = colDef(name = "Total",format = colFormat(separators = T,locales = "pt-BR"))
        ),
        defaultColDef = colDef(footerStyle = list(fontWeight = "bold"),
                               headerStyle = list(background = "#f7f7f8")),
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
    ##Gráfico - Número de Matrículas no Ensino Pré-Escolar por Esfera Administrativa----
    output$edu10txt3 <- renderText({
      t103()
    })
    output$edu10graf <- renderEcharts4r({
      edu10 %>% filter(localidade == "Pará") %>% 
        pivot_wider(names_from = categoria, values_from = valor) %>% 
        e_chart(x = ano) %>%
        e_line(
          serie = Federal,
          name = "Federal",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Estadual,
          name = "Estadual",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Municipal,
          name = "Municipal",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Privado,
          name = "Privada",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Total,
          name = "Total",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_tooltip(
          trigger = "axis",
          formatter = e_tooltip_pointer_formatter("decimal",digits = 0,locale = "pt-BR"),
          axisPointer =list(type = 'shadow')
        ) %>% 
         e_x_axis(
            axisLabel = list(show = T,fontSize = 11),
            name = "Ano",
            splitLine = list(show = T),
            nameTextStyle = list(fontWeight = "bold", fontSize = 14)
          ) %>%
        e_y_axis(
          name = "Número",
          nameTextStyle = list(fontWeight = "bold",fontSize = 14),
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
          e_datazoom(toolbox = F,  fillerColor = "#E5F5F9") %>%
          e_grid(show = T)
    })
    #11 - Número de Matrículas no Ensino Fundamental por Esfera Administrativa----
    ##Mapa - Número de Matrículas no Ensino Fundamental por Esfera Administrativa----
    output$edu11txt1 <- renderText({
      t111()
    })
    output$edu11map <- renderLeaflet({
      #Tratamento da informação
      ##Filtrando informação
      if (input$edu11ri == "Pará") {
        df <- edu11 %>% filter(localidade !="Pará",ano == input$edu11ano, categoria == "Total") %>% 
          select(ri,localidade,ano,valor)
        x <- cbind(geopa,df)
        x$valor[x$valor == 0] <- NA
      }      else{
        df <- edu11 %>% filter(localidade !="Pará",ano == input$edu11ano, categoria == "Total") %>% 
          select(ri,localidade,ano,valor)
        x <- cbind(geopa,df)
        x <- x %>% filter(ri == input$edu11ri)
        x$valor[x$valor == 0] <- NA
      }
      
      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }
      
      pal <- colorBin(c("#FFEBCD","#ffcd97","#ffaf66","#fa9236","#f17701"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Número de Matrículas:</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
            format(x$valor, big.mark = ".", decimal.mark = ",")
          )
        ) %>% lapply(htmltools::HTML)
      
      #Mapas com leafleft
      leaflet(x,options = leafletOptions(minZoom = 0,maxZoom = 15)) %>%
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
          title = "Número de Matrículas",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".",decimal.mark = ",",digits = 2)
        )
    })
    ##Tabela - Número de Matrículas no Ensino Fundamental por Esfera Administrativa----
    output$edu11txt2 <- renderText({
      t112()
    })
    output$edu11tab <- renderReactable({
      if (input$edu11ri == "Pará") {
        x <- edu11 %>% filter(localidade !="Pará",ano == input$edu11ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
      }      else{
        x <- edu11 %>% filter(localidade !="Pará",ano == input$edu11ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
        x <- x %>% filter(ri == input$edu11ri)
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
          ri = colDef(name = "Região de Integração",width = 170),
          localidade = colDef(name = "Municípios",width = 150),
          Federal = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          Estadual = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          Municipal = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          Privada = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          Total = colDef(name = "Total",format = colFormat(separators = T,locales = "pt-BR"))
        ),
        defaultColDef = colDef(footerStyle = list(fontWeight = "bold"),
                               headerStyle = list(background = "#f7f7f8")),
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
    ##Gráfico - Número de Matrículas no Ensino Fundamental por Esfera Administrativa----
    output$edu11txt3 <- renderText({
      t113()
    })
    output$edu11graf <- renderEcharts4r({
      edu11 %>% filter(localidade == "Pará") %>% 
        pivot_wider(names_from = categoria, values_from = valor) %>% 
        e_chart(x = ano) %>%
        e_line(
          serie = Federal,
          name = "Federal",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Estadual,
          name = "Estadual",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Municipal,
          name = "Municipal",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Privado,
          name = "Privada",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Total,
          name = "Total",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_tooltip(
          trigger = "axis",
          formatter = e_tooltip_pointer_formatter("decimal",digits = 0,locale = "pt-BR"),
          axisPointer =list(type = 'shadow')
        ) %>% 
         e_x_axis(
            axisLabel = list(show = T,fontSize = 11),
            name = "Ano",
            splitLine = list(show = T),
            nameTextStyle = list(fontWeight = "bold", fontSize = 14)
          ) %>%
        e_y_axis(
          name = "Número",
          nameTextStyle = list(fontWeight = "bold",fontSize = 14),
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
          e_datazoom(toolbox = F,  fillerColor = "#E5F5F9") %>%
          e_grid(show = T)
    })
    #12 - Número de Matrículas no Ensino Médio por Esfera Administrativa----
    ##Mapa - Número de Matrículas no Ensino Médio por Esfera Administrativa----
    output$edu12txt1 <- renderText({
      t121()
    })
    output$edu12map <- renderLeaflet({
      #Tratamento da informação
      ##Filtrando informação
      if (input$edu12ri == "Pará") {
        df <- edu12 %>% filter(localidade !="Pará",ano == input$edu12ano, categoria == "Total Médio") %>% 
          select(ri,localidade,ano,valor)
        x <- cbind(geopa,df)
        x$valor[x$valor == 0] <- NA
      }      else{
        df <- edu12 %>% filter(localidade !="Pará",ano == input$edu12ano, categoria == "Total Médio") %>% 
          select(ri,localidade,ano,valor)
        x <- cbind(geopa,df)
        x <- x %>% filter(ri == input$edu12ri)
        x$valor[x$valor == 0] <- NA
      }
      
      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }
      
      pal <- colorBin(c("#FFEBCD","#ffcd97","#ffaf66","#fa9236","#f17701"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Número de Matrículas:</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
            format(x$valor, big.mark = ".", decimal.mark = ",")
          )
        ) %>% lapply(htmltools::HTML)
      
      #Mapas com leafleft
      leaflet(x,options = leafletOptions(minZoom = 0,maxZoom = 15)) %>%
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
          title = "Número de Matrículas",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".",decimal.mark = ",",digits = 2)
        )
    })
    ##Tabela - Número de Matrículas no Ensino Médio por Esfera Administrativa----
    output$edu12txt2 <- renderText({
      t122()
    })
    output$edu12tab <- renderReactable({
      if (input$edu12ri == "Pará") {
        x <- edu12 %>% filter(localidade !="Pará",ano == input$edu12ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
      }      else{
        x <- edu12 %>% filter(localidade !="Pará",ano == input$edu12ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
        x <- x %>% filter(ri == input$edu12ri)
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
          ri = colDef(name = "Região de Integração",width = 170),
          localidade = colDef(name = "Municípios",width = 150),
          Federal = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          Estadual = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          Municipal = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          Privada = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          `Total Médio` = colDef(name = "Total",format = colFormat(separators = T,locales = "pt-BR"))
        ),
        defaultColDef = colDef(footerStyle = list(fontWeight = "bold"),
                               headerStyle = list(background = "#f7f7f8")),
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
    ##Gráfico - Número de Matrículas no Ensino Médio por Esfera Administrativa----
    output$edu12txt3 <- renderText({
      t123()
    })
    output$edu12graf <- renderEcharts4r({
      edu12 %>% filter(localidade == "Pará") %>% 
        pivot_wider(names_from = categoria, values_from = valor) %>% 
        e_chart(x = ano) %>%
        e_line(
          serie = Federal,
          name = "Federal",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Estadual,
          name = "Estadual",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Municipal,
          name = "Municipal",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Privado,
          name = "Privada",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = `Total Médio`,
          name = "Total",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_tooltip(
          trigger = "axis",
          formatter = e_tooltip_pointer_formatter("decimal",digits = 0,locale = "pt-BR"),
          axisPointer =list(type = 'shadow')
        ) %>% 
         e_x_axis(
            axisLabel = list(show = T,fontSize = 11),
            name = "Ano",
            splitLine = list(show = T),
            nameTextStyle = list(fontWeight = "bold", fontSize = 14)
          ) %>%
        e_y_axis(
          name = "Número",
          nameTextStyle = list(fontWeight = "bold",fontSize = 14),
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
          e_datazoom(toolbox = F,  fillerColor = "#E5F5F9") %>%
          e_grid(show = T)
    })
    #13 - Média de Alunos por Turma por Nível de Ensino----
    ##Mapa - Média de Alunos por Turma por Nível de Ensino----
    output$edu13txt1 <- renderText({
      t131()
    })
    output$edu13map <- renderLeaflet({
      #Tratamento da informação
      ##Filtrando informação
      if (input$edu13ri == "Pará") {
        df <- edu13 %>% filter(localidade !="Pará",ano == input$edu13ano, categoria == input$edu13cat) %>% 
          select(ri,localidade,ano,valor)
        x <- cbind(geopa,df)
      }      else{
        df <- edu13 %>% filter(localidade !="Pará",ano == input$edu13ano, categoria == input$edu13cat) %>% 
          select(ri,localidade,ano,valor)
        x <- cbind(geopa,df)
        x <- x %>% filter(ri == input$edu13ri)
      }
      
      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }
      
      pal <- colorBin(c("#FFEBCD","#ffcd97","#ffaf66","#fa9236","#f17701"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Média:</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
            format(x$valor, big.mark = ".", decimal.mark = ",")
          )
        ) %>% lapply(htmltools::HTML)
      
      #Mapas com leafleft
      leaflet(x,options = leafletOptions(minZoom = 0,maxZoom = 15)) %>%
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
          title = "Média",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".",decimal.mark = ",",digits = 2)
        )
    })
    ##Tabela - Média de Alunos por Turma por Nível de Ensino----
    output$edu13txt2 <- renderText({
      t132()
    })
    output$edu13tab <- renderReactable({
      if (input$edu13ri == "Pará") {
        x <- edu13 %>% filter(localidade !="Pará",ano == input$edu13ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
      }      else{
        x <- edu13 %>% filter(localidade !="Pará",ano == input$edu13ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
        x <- x %>% filter(ri == input$edu13ri)
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
          ri = colDef(name = "Região de Integração",width = 170),
          `Pré-Escola` = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          `Ensino Fundamental` = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          `Ensino Médio` = colDef(format = colFormat(separators = T,locales = "pt-BR"))
        ),
        defaultColDef = colDef(footerStyle = list(fontWeight = "bold"),
                               headerStyle = list(background = "#f7f7f8")),
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
    ##Gráfico - Média de Alunos por Turma por Nível de Ensino----
    output$edu13txt3 <- renderText({
      t133()
    })
    output$edu13graf <- renderEcharts4r({
      edu13 %>% filter(localidade == "Pará") %>% 
        pivot_wider(names_from = categoria, values_from = valor) %>% 
        e_chart(x = ano) %>%
        e_line(
          serie = `Ensino Fundamental`,
          name = "Ensino Fundamental",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = `Ensino Médio`,
          name = "Ensino Médio",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = `Pré-Escola`,
          name = "Pré-Escola",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_tooltip(
          trigger = "axis",
          formatter = e_tooltip_pointer_formatter("decimal",digits = 1,locale = "pt-BR"),
          axisPointer =list(type = 'shadow')
        ) %>% 
         e_x_axis(
            axisLabel = list(show = T,fontSize = 11),
            name = "Ano",
            splitLine = list(show = T),
            nameTextStyle = list(fontWeight = "bold", fontSize = 14)
          ) %>%
        e_y_axis(
          name = "Média",
          nameTextStyle = list(fontWeight = "bold",fontSize = 14),
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
          e_datazoom(toolbox = F,  fillerColor = "#E5F5F9") %>%
          e_grid(show = T)
    })
    #14 - Número de Docentes no Ensino Pré-escolar por Esfera Administrativa----
    ##Mapa - Número de Docentes no Ensino Pré-escolar por Esfera Administrativa----
    output$edu14txt1 <- renderText({
      t141()
    })
    output$edu14map <- renderLeaflet({
      #Tratamento da informação
      ##Filtrando informação
      if (input$edu14ri == "Pará") {
        df <- edu14 %>% filter(localidade !="Pará",ano == input$edu14ano, categoria == "Total") %>% 
          select(ri,localidade,ano,valor)
        x <- cbind(geopa,df)
        x$valor[x$valor == 0] <- NA
      }      else{
        df <- edu14 %>% filter(localidade !="Pará",ano == input$edu14ano, categoria == "Total") %>% 
          select(ri,localidade,ano,valor)
        x <- cbind(geopa,df)
        x <- x %>% filter(ri == input$edu14ri)
        x$valor[x$valor == 0] <- NA
      }
      
      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }
      
      pal <- colorBin(c("#FFEBCD","#ffcd97","#ffaf66","#fa9236","#f17701"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Número de Docentes:</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
            format(x$valor, big.mark = ".", decimal.mark = ",")
          )
        ) %>% lapply(htmltools::HTML)
      
      #Mapas com leafleft
      leaflet(x,options = leafletOptions(minZoom = 0,maxZoom = 15)) %>%
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
          title = "Número de Docentes",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".",decimal.mark = ",",digits = 2)
        )
    })
    ##Tabela - Número de Docentes no Ensino Pré-escolar por Esfera Administrativa----
    output$edu14txt2 <- renderText({
      t142()
    })
    output$edu14tab <- renderReactable({
      if (input$edu14ri == "Pará") {
        x <- edu14 %>% filter(localidade !="Pará",ano == input$edu14ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
      }      else{
        x <- edu14 %>% filter(localidade !="Pará",ano == input$edu14ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
        x <- x %>% filter(ri == input$edu14ri)
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
          ri = colDef(name = "Região de Integração",width = 170),
          localidade = colDef(name = "Municípios",width = 150),
          Federal = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          Estadual = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          Municipal = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          Privada = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          Total = colDef(name = "Total",format = colFormat(separators = T,locales = "pt-BR"))
        ),
        defaultColDef = colDef(footerStyle = list(fontWeight = "bold"),
                               headerStyle = list(background = "#f7f7f8")),
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
    ##Gráfico - Número de Docentes no Ensino Pré-escolar por Esfera Administrativa----
    output$edu14txt3 <- renderText({
      t143()
    })
    output$edu14graf <- renderEcharts4r({
      edu14 %>% filter(localidade == "Pará") %>% 
        pivot_wider(names_from = categoria, values_from = valor) %>% 
        e_chart(x = ano) %>%
        e_line(
          serie = Federal,
          name = "Federal",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Estadual,
          name = "Estadual",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Municipal,
          name = "Municipal",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Privado,
          name = "Privada",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Total,
          name = "Total",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_tooltip(
          trigger = "axis",
          formatter = e_tooltip_pointer_formatter("decimal",digits = 0,locale = "pt-BR"),
          axisPointer =list(type = 'shadow')
        ) %>% 
         e_x_axis(
            axisLabel = list(show = T,fontSize = 11),
            name = "Ano",
            splitLine = list(show = T),
            nameTextStyle = list(fontWeight = "bold", fontSize = 14)
          ) %>%
        e_y_axis(
          name = "Número",
         nameTextStyle = list(fontWeight = "bold",fontSize = 14),
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
          e_datazoom(toolbox = F,  fillerColor = "#E5F5F9") %>%
          e_grid(show = T)
    })
    #15 - Número de Docentes no Ensino Fundamental por Esfera Administrativa----
    ##Mapa - Número de Docentes no Ensino Fundamental por Esfera Administrativa----
    output$edu15txt1 <- renderText({
      t151()
    })
    output$edu15map <- renderLeaflet({
      #Tratamento da informação
      ##Filtrando informação
      if (input$edu15ri == "Pará") {
        df <- edu15 %>% filter(localidade !="Pará",ano == input$edu15ano, categoria == "Total") %>% 
          select(ri,localidade,ano,valor)
        x <- cbind(geopa,df)
        x$valor[x$valor == 0] <- NA
      }      else{
        df <- edu15 %>% filter(localidade !="Pará",ano == input$edu15ano, categoria == "Total") %>% 
          select(ri,localidade,ano,valor)
        x <- cbind(geopa,df)
        x <- x %>% filter(ri == input$edu15ri)
        x$valor[x$valor == 0] <- NA
      }
      
      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }
      
      pal <- colorBin(c("#FFEBCD","#ffcd97","#ffaf66","#fa9236","#f17701"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Número de Docentes:</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
            format(x$valor, big.mark = ".", decimal.mark = ",")
          )
        ) %>% lapply(htmltools::HTML)
      
      #Mapas com leafleft
      leaflet(x,options = leafletOptions(minZoom = 0,maxZoom = 15)) %>%
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
          title = "Número de Docentes",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".",decimal.mark = ",",digits = 2)
        )
    })
    ##Tabela - Número de Docentes no Ensino Fundamental por Esfera Administrativa----
    output$edu15txt2 <- renderText({
      t152()
    })
    output$edu15tab <- renderReactable({
      if (input$edu15ri == "Pará") {
        x <- edu15 %>% filter(localidade !="Pará",ano == input$edu15ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
      }      else{
        x <- edu15 %>% filter(localidade !="Pará",ano == input$edu15ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
        x <- x %>% filter(ri == input$edu15ri)
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
          ri = colDef(name = "Região de Integração",width = 170),
          localidade = colDef(name = "Municípios",width = 150),
          Federal = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          Estadual = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          Municipal = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          Privada = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          Total = colDef(name = "Total",format = colFormat(separators = T,locales = "pt-BR"))
        ),
        defaultColDef = colDef(footerStyle = list(fontWeight = "bold"),
                               headerStyle = list(background = "#f7f7f8")),
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
    ##Gráfico - Número de Docentes no Ensino Fundamental por Esfera Administrativa----
    output$edu15txt3 <- renderText({
      t153()
    })
    output$edu15graf <- renderEcharts4r({
      edu15 %>% filter(localidade == "Pará") %>% 
        pivot_wider(names_from = categoria, values_from = valor) %>% 
        e_chart(x = ano) %>%
        e_line(
          serie = Federal,
          name = "Federal",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Estadual,
          name = "Estadual",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Municipal,
          name = "Municipal",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Privado,
          name = "Privada",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Total,
          name = "Total",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_tooltip(
          trigger = "axis",
          formatter = e_tooltip_pointer_formatter("decimal",digits = 1,locale = "pt-BR"),
          axisPointer =list(type = 'shadow')
        ) %>% 
         e_x_axis(
            axisLabel = list(show = T,fontSize = 11),
            name = "Ano",
            splitLine = list(show = T),
            nameTextStyle = list(fontWeight = "bold", fontSize = 14)
          ) %>%
        e_y_axis(
          name = "Número",
          nameTextStyle = list(fontWeight = "bold",fontSize = 14),
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
          e_datazoom(toolbox = F,  fillerColor = "#E5F5F9") %>%
          e_grid(show = T)
    })
    #16 - Número de Docentes no Ensino Médio por Esfera Administrativa----
    ##Mapa - Número de Docentes no Ensino Médio por Esfera Administrativa----
    output$edu16txt1 <- renderText({
      t161()
    })
    output$edu16map <- renderLeaflet({
      #Tratamento da informação
      ##Filtrando informação
      if (input$edu16ri == "Pará") {
        df <- edu16 %>% filter(localidade !="Pará",ano == input$edu16ano, categoria == "Total") %>% 
          select(ri,localidade,ano,valor)
        x <- cbind(geopa,df)
        x$valor[x$valor == 0] <- NA
      }      else{
        df <- edu16 %>% filter(localidade !="Pará",ano == input$edu16ano, categoria == "Total") %>% 
          select(ri,localidade,ano,valor)
        x <- cbind(geopa,df)
        x <- x %>% filter(ri == input$edu16ri)
        x$valor[x$valor == 0] <- NA
      }
      
      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }
      
      pal <- colorBin(c("#FFEBCD","#ffcd97","#ffaf66","#fa9236","#f17701"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Número de Docentes:</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
            format(x$valor, big.mark = ".", decimal.mark = ",")
          )
        ) %>% lapply(htmltools::HTML)
      
      #Mapas com leafleft
      leaflet(x,options = leafletOptions(minZoom = 0,maxZoom = 15)) %>%
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
          title = "Número de Docentes",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".",decimal.mark = ",",digits = 2)
        )
    })
    ##Tabela - Número de Docentes no Ensino Médio por Esfera Administrativa----
    output$edu16txt2 <- renderText({
      t162()
    })
    output$edu16tab <- renderReactable({
      if (input$edu16ri == "Pará") {
        x <- edu16 %>% filter(localidade !="Pará",ano == input$edu16ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
      }      else{
        x <- edu16 %>% filter(localidade !="Pará",ano == input$edu16ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
        x <- x %>% filter(ri == input$edu16ri)
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
          ri = colDef(name = "Região de Integração",width = 170),
          localidade = colDef(name = "Municípios",width = 150),
          Federal = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          Estadual = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          Municipal = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          Privada = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          Total = colDef(name = "Total",format = colFormat(separators = T,locales = "pt-BR"))
        ),
        defaultColDef = colDef(footerStyle = list(fontWeight = "bold"),
                               headerStyle = list(background = "#f7f7f8")),
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
    ##Gráfico - Número de Docentes no Ensino Médio por Esfera Administrativa----
    output$edu16txt3 <- renderText({
      t163()
    })
    output$edu16graf <- renderEcharts4r({
      edu16 %>% filter(localidade == "Pará") %>% 
        pivot_wider(names_from = categoria, values_from = valor) %>% 
        e_chart(x = ano) %>%
        e_line(
          serie = Federal,
          name = "Federal",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Estadual,
          name = "Estadual",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Municipal,
          name = "Municipal",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Privado,
          name = "Privada",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Total,
          name = "Total",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_tooltip(
          trigger = "axis",
          formatter = e_tooltip_pointer_formatter("decimal",digits = 1,locale = "pt-BR"),
          axisPointer =list(type = 'shadow')
        ) %>% 
         e_x_axis(
            axisLabel = list(show = T,fontSize = 11),
            name = "Ano",
            splitLine = list(show = T),
            nameTextStyle = list(fontWeight = "bold", fontSize = 14)
          ) %>%
        e_y_axis(
          name = "Número",
          nameTextStyle = list(fontWeight = "bold",fontSize = 14),
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
          e_datazoom(toolbox = F,  fillerColor = "#E5F5F9") %>%
          e_grid(show = T)
    })
    #17 - Estabelecimentos de Pré-Escola por Esfera Administrativa----
    ##Mapa - Estabelecimentos de Pré-Escola por Esfera Administrativa----
    output$edu17txt1 <- renderText({
      t171()
    })
    output$edu17map <- renderLeaflet({
      #Tratamento da informação
      ##Filtrando informação
      if (input$edu17ri == "Pará") {
        df <- edu17 %>% filter(localidade !="Pará",ano == input$edu17ano, categoria == "Total") %>% 
          select(ri,localidade,ano,valor)
        x <- cbind(geopa,df)
        x$valor[x$valor == 0] <- NA
      }      else{
        df <- edu17 %>% filter(localidade !="Pará",ano == input$edu17ano, categoria == "Total") %>% 
          select(ri,localidade,ano,valor)
        x <- cbind(geopa,df)
        x <- x %>% filter(ri == input$edu17ri)
        x$valor[x$valor == 0] <- NA
      }
      
      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }
      
      pal <- colorBin(c("#FFEBCD","#ffcd97","#ffaf66","#fa9236","#f17701"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Número de Docentes:</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
            format(x$valor, big.mark = ".", decimal.mark = ",")
          )
        ) %>% lapply(htmltools::HTML)
      
      #Mapas com leafleft
      leaflet(x,options = leafletOptions(minZoom = 0,maxZoom = 15)) %>%
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
          title = "Número de Docentes",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".",decimal.mark = ",",digits = 2)
        )
    })
    ##Tabela - Estabelecimentos de Pré-Escola por Esfera Administrativa----
    output$edu17txt2 <- renderText({
      t172()
    })
    output$edu17tab <- renderReactable({
      if (input$edu17ri == "Pará") {
        x <- edu17 %>% filter(localidade !="Pará",ano == input$edu17ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
      }      else{
        x <- edu17 %>% filter(localidade !="Pará",ano == input$edu17ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
        x <- x %>% filter(ri == input$edu17ri)
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
          ri = colDef(name = "Região de Integração",width = 170),
          localidade = colDef(name = "Municípios",width = 150),
          Federal = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          Estadual = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          Municipal = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          Privada = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          Total = colDef(name = "Total",format = colFormat(separators = T,locales = "pt-BR"))
        ),
        defaultColDef = colDef(footerStyle = list(fontWeight = "bold"),
                               headerStyle = list(background = "#f7f7f8")),
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
    ##Gráfico - Estabelecimentos de Pré-Escola por Esfera Administrativa----
    output$edu17txt3 <- renderText({
      t173()
    })
    output$edu17graf <- renderEcharts4r({
      edu17 %>% filter(localidade == "Pará") %>% 
        pivot_wider(names_from = categoria, values_from = valor) %>% 
        e_chart(x = ano) %>%
        e_line(
          serie = Federal,
          name = "Federal",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Estadual,
          name = "Estadual",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Municipal,
          name = "Municipal",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Privado,
          name = "Privada",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Total,
          name = "Total",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_tooltip(
          trigger = "axis",
          formatter = e_tooltip_pointer_formatter("decimal",digits = 1,locale = "pt-BR"),
          axisPointer =list(type = 'shadow')
        ) %>% 
         e_x_axis(
            axisLabel = list(show = T,fontSize = 11),
            name = "Ano",
            splitLine = list(show = T),
            nameTextStyle = list(fontWeight = "bold", fontSize = 14)
          ) %>%
        e_y_axis(
          name = "Quantidade",
         nameTextStyle = list(fontWeight = "bold",fontSize = 14),
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
          e_datazoom(toolbox = F,  fillerColor = "#E5F5F9") %>%
          e_grid(show = T)
    })
    #18 - Estabelecimentos de Ensino Fundamental por Esfera Administrativa----
    ##Mapa - Estabelecimentos de Ensino Fundamental por Esfera Administrativa----
    output$edu18txt1 <- renderText({
      t181()
    })
    output$edu18map <- renderLeaflet({
      #Tratamento da informação
      ##Filtrando informação
      if (input$edu18ri == "Pará") {
        df <- edu18 %>% filter(localidade !="Pará",ano == input$edu18ano, categoria == "Total") %>% 
          select(ri,localidade,ano,valor)
        x <- cbind(geopa,df)
        x$valor[x$valor == 0] <- NA
      }      else{
        df <- edu18 %>% filter(localidade !="Pará",ano == input$edu18ano, categoria == "Total") %>% 
          select(ri,localidade,ano,valor)
        x <- cbind(geopa,df)
        x <- x %>% filter(ri == input$edu18ri)
        x$valor[x$valor == 0] <- NA
      }
      
      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }
      
      pal <- colorBin(c("#FFEBCD","#ffcd97","#ffaf66","#fa9236","#f17701"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Número de Docentes:</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
            format(x$valor, big.mark = ".", decimal.mark = ",")
          )
        ) %>% lapply(htmltools::HTML)
      
      #Mapas com leafleft
      leaflet(x,options = leafletOptions(minZoom = 0,maxZoom = 15)) %>%
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
          title = "Número de Docentes",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".",decimal.mark = ",",digits = 2)
        )
    })
    ##Tabela - Estabelecimentos de Ensino Fundamental por Esfera Administrativa----
    output$edu18txt2 <- renderText({
      t182()
    })
    output$edu18tab <- renderReactable({
      if (input$edu18ri == "Pará") {
        x <- edu18 %>% filter(localidade !="Pará",ano == input$edu18ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
      }      else{
        x <- edu18 %>% filter(localidade !="Pará",ano == input$edu18ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
        x <- x %>% filter(ri == input$edu18ri)
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
          ri = colDef(name = "Região de Integração",width = 170),
          localidade = colDef(name = "Municípios",width = 150),
          Federal = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          Estadual = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          Municipal = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          Privada = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          Total = colDef(name = "Total",format = colFormat(separators = T,locales = "pt-BR"))
        ),
        defaultColDef = colDef(footerStyle = list(fontWeight = "bold"),
                               headerStyle = list(background = "#f7f7f8")),
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
    ##Gráfico - Estabelecimentos de Ensino Fundamental por Esfera Administrativa----
    output$edu18txt3 <- renderText({
      t183()
    })
    output$edu18graf <- renderEcharts4r({
      edu18 %>% filter(localidade == "Pará") %>% 
        pivot_wider(names_from = categoria, values_from = valor) %>% 
        e_chart(x = ano) %>%
        e_line(
          serie = Federal,
          name = "Federal",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Estadual,
          name = "Estadual",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Municipal,
          name = "Municipal",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Privada,
          name = "Privada",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Total,
          name = "Total",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_tooltip(
          trigger = "axis",
          formatter = e_tooltip_pointer_formatter("decimal",digits = 0,locale = "pt-BR"),
          axisPointer =list(type = 'shadow')
        ) %>% 
         e_x_axis(
            axisLabel = list(show = T,fontSize = 11),
            name = "Ano",
            splitLine = list(show = T),
            nameTextStyle = list(fontWeight = "bold", fontSize = 14)
          ) %>%
        e_y_axis(
          name = "Quantidade",
         nameTextStyle = list(fontWeight = "bold",fontSize = 14),
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
          e_datazoom(toolbox = F,  fillerColor = "#E5F5F9") %>%
          e_grid(show = T)
    })
    #19 - Estabelecimentos de Ensino Médio por Esfera Administrativa----
    ##Mapa - Estabelecimentos de Ensino Médio por Esfera Administrativa----
    output$edu19txt1 <- renderText({
      t191()
    })
    output$edu19map <- renderLeaflet({
      #Tratamento da informação
      ##Filtrando informação
      if (input$edu19ri == "Pará") {
        df <- edu19 %>% filter(localidade !="Pará",ano == input$edu19ano, categoria == "Total") %>% 
          select(ri,localidade,ano,valor)
        x <- cbind(geopa,df)
        x$valor[x$valor == 0] <- NA
      }      else{
        df <- edu19 %>% filter(localidade !="Pará",ano == input$edu19ano, categoria == "Total") %>% 
          select(ri,localidade,ano,valor)
        x <- cbind(geopa,df)
        x <- x %>% filter(ri == input$edu19ri)
        x$valor[x$valor == 0] <- NA
      }
      
      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }
      
      pal <- colorBin(c("#FFEBCD","#ffcd97","#ffaf66","#fa9236","#f17701"), domain = x$valor, bins = bins)
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>Número de Docentes:</b> %s",
          x$name_muni,
          ifelse(
            is.na(x$valor),
            "Não disponível",
            format(x$valor, big.mark = ".", decimal.mark = ",")
          )
        ) %>% lapply(htmltools::HTML)
      
      #Mapas com leafleft
      leaflet(x,options = leafletOptions(minZoom = 0,maxZoom = 15)) %>%
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
          title = "Número de Docentes",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(big.mark = ".",decimal.mark = ",",digits = 2)
        )
    })
    ##Tabela - Estabelecimentos de Ensino Médio por Esfera Administrativa----
    output$edu19txt2 <- renderText({
      t192()
    })
    output$edu19tab <- renderReactable({
      if (input$edu19ri == "Pará") {
        x <- edu19 %>% filter(localidade !="Pará",ano == input$edu19ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
      }      else{
        x <- edu19 %>% filter(localidade !="Pará",ano == input$edu19ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
        x <- x %>% filter(ri == input$edu19ri)
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
          ri = colDef(name = "Região de Integração",width = 170),
          localidade = colDef(name = "Municípios",width = 150),
          Federal = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          Estadual = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          Municipal = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          Privada = colDef(format = colFormat(separators = T,locales = "pt-BR")),
          Total = colDef(name = "Total",format = colFormat(separators = T,locales = "pt-BR"))
        ),
        defaultColDef = colDef(footerStyle = list(fontWeight = "bold"),
                               na = "-",
                               headerStyle = list(background = "#f7f7f8")),
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
    ##Gráfico - Estabelecimentos de Ensino Médio por Esfera Administrativa----
    output$edu19txt3 <- renderText({
      t193()
    })
    output$edu19graf <- renderEcharts4r({
      edu19 %>% filter(localidade == "Pará") %>% 
        pivot_wider(names_from = categoria, values_from = valor) %>% 
        e_chart(x = ano) %>%
        e_line(
          serie = Federal,
          name = "Federal",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Estadual,
          name = "Estadual",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Municipal,
          name = "Municipal",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Privada,
          name = "Privada",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_line(
          serie = Total,
          name = "Total",
          legend = T,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = T,
        ) %>%
        e_tooltip(
          trigger = "axis",
          formatter = e_tooltip_pointer_formatter("decimal",digits = 0,locale = "pt-BR"),
          axisPointer =list(type = 'shadow')
        ) %>% 
         e_x_axis(
            axisLabel = list(show = T,fontSize = 11),
            name = "Ano",
            splitLine = list(show = T),
            nameTextStyle = list(fontWeight = "bold", fontSize = 14)
          ) %>%
        e_y_axis(
          name = "Quantidade",
          nameTextStyle = list(fontWeight = "bold",fontSize = 14),
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
          e_datazoom(toolbox = F,  fillerColor = "#E5F5F9") %>%
          e_grid(show = T)
    })
    #DOWNLOADS----
    #1 - Taxa de Aprovação no Ensino Fundamental por Esfera Administrativa----
    ## - Tabela - Taxa de Aprovação no Ensino Fundamental----
    # Filtra os dados
    edu1_1 <- reactive({
      if (input$edu1ri == "Pará") {
        x <- edu1 %>% filter(localidade !="Pará",ano == input$edu1ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
      }      else{
        x <- edu1 %>% filter(localidade !="Pará",ano == input$edu1ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
        x <- x %>% filter(ri == input$edu1ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu1_1(), {
      downset_Server("edu1_1", edu1_1(), t12())
    })
    ## - Gráfico - Taxa de Aprovação no Ensino Fundamental----
    # Filtra os dados
    edu1_2 <- reactive({
      edu1 %>% filter(localidade == "Pará") %>% 
        pivot_wider(names_from = categoria, values_from = valor)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu1_2(), {
      downset_Server("edu1_2", edu1_2(), t13())
    })

    #2 - Taxa de Aprovação no Ensino Médio por Esfera Administrativa----
    ## - Tabela - Taxa de Aprovação no Ensino Médio----
    # Filtra os dados
    edu2_1 <- reactive({
      if (input$edu2ri == "Pará") {
        x <- edu2 %>% filter(localidade !="Pará",ano == input$edu2ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
      }      else{
        x <- edu2 %>% filter(localidade !="Pará",ano == input$edu2ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
        x <- x %>% filter(ri == input$edu2ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu2_1(), {
      downset_Server("edu2_1", edu2_1(), t22())
    })
    ## - Gráfico - Taxa de Aprovação no Ensino Médio----
    # Filtra os dados
    edu2_2 <- reactive({
      edu2 %>% filter(localidade == "Pará") %>% 
        pivot_wider(names_from = categoria, values_from = valor)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu2_2(), {
      downset_Server("edu2_2", edu2_2(), t23())
    })

    #3 - Taxa de Reprovação no Ensino Fundamental por Esfera Administrativa----
    ## - Tabela - Taxa de Reprovação no Ensino Fundamental----
    # Filtra os dados
    edu3_1 <- reactive({
      if (input$edu3ri == "Pará") {
        x <- edu3 %>% filter(localidade !="Pará",ano == input$edu3ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
      }      else{
        x <- edu3 %>% filter(localidade !="Pará",ano == input$edu3ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
        x <- x %>% filter(ri == input$edu3ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu3_1(), {
      downset_Server("edu3_1", edu3_1(), t32())
    })
    ## - Gráfico - Taxa de Reprovação no Ensino Fundamental----
    # Filtra os dados
    edu3_2 <- reactive({
      edu3 %>% filter(localidade == "Pará") %>% 
        pivot_wider(names_from = categoria, values_from = valor)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu3_2(), {
      downset_Server("edu3_2", edu3_2(), t33())
    })

    #4 - Taxa de Reprovação no Ensino Médio por Esfera Administrativa----
    ## - Tabela - Taxa de Reprovação no Ensino Médio----
    # Filtra os dados
    edu4_1 <- reactive({
       if (input$edu4ri == "Pará") {
        x <- edu4 %>% filter(localidade !="Pará",ano == input$edu4ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
      }      else{
        x <- edu4 %>% filter(localidade !="Pará",ano == input$edu4ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
        x <- x %>% filter(ri == input$edu4ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu4_1(), {
      downset_Server("edu4_1", edu4_1(), t42())
    })
    ## - Gráfico - Taxa de Reprovação no Ensino Médio----
    # Filtra os dados
    edu4_2 <- reactive({
       edu4 %>% filter(localidade == "Pará") %>% 
        pivot_wider(names_from = categoria, values_from = valor)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu4_2(), {
      downset_Server("edu4_2", edu4_2(), t43())
    })

    #5 - Taxa de Abandono no Ensino Fundamental por Esfera Administrativa----
    ## - Tabela - Taxa de Abandono no Ensino Fundamental----
    # Filtra os dados
    edu5_1 <- reactive({
       if (input$edu5ri == "Pará") {
        x <- edu5 %>% filter(localidade !="Pará",ano == input$edu5ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
      }      else{
        x <- edu5 %>% filter(localidade !="Pará",ano == input$edu5ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
        x <- x %>% filter(ri == input$edu5ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu5_1(), {
      downset_Server("edu5_1", edu5_1(), t52())
    })
    ## - Gráfico - Taxa de Abandono no Ensino Fundamental----
    # Filtra os dados
    edu5_2 <- reactive({
      edu5 %>% filter(localidade == "Pará") %>% 
        pivot_wider(names_from = categoria, values_from = valor)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu5_2(), {
      downset_Server("edu5_2", edu5_2(), t53())
    })

    #6 - Taxa de Abandono no Ensino Médio por Esfera Administrativa----
    ## - Tabela - Taxa de Abandono no Ensino Médio----
    # Filtra os dados
    edu6_1 <- reactive({
      if (input$edu6ri == "Pará") {
        x <- edu6 %>% filter(localidade !="Pará",ano == input$edu6ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
      }      else{
        x <- edu6 %>% filter(localidade !="Pará",ano == input$edu6ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
        x <- x %>% filter(ri == input$edu6ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu6_1(), {
      downset_Server("edu6_1", edu6_1(), t62())
    })
    ## - Gráfico - Taxa de Abandono no Ensino Médio----
    # Filtra os dados
    edu6_2 <- reactive({
      edu6 %>% filter(localidade == "Pará") %>% 
        pivot_wider(names_from = categoria, values_from = valor)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu6_2(), {
      downset_Server("edu6_2", edu6_2(), t63())
    })

    #7 - Distorção Idade-Série Total por Nível de Ensino----
    ## - Tabela - Distorção Idade-Série Total por Nível de Ensino----
    # Filtra os dados
    edu7_1 <- reactive({
       if (input$edu7ri == "Pará") {
        x <- edu7 %>% filter(localidade !="Pará",ano == input$edu7ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
      }      else{
        x <- edu7 %>% filter(localidade !="Pará",ano == input$edu7ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
        x <- x %>% filter(ri == input$edu7ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu7_1(), {
      downset_Server("edu7_1", edu7_1(), t72())
    })
    ## - Gráfico - Distorção Idade-Série Total por Nível de Ensino----
    # Filtra os dados
    edu7_2 <- reactive({
       edu7 %>% filter(localidade == "Pará") %>% 
        pivot_wider(names_from = categoria, values_from = valor)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu7_2(), {
      downset_Server("edu7_2", edu7_2(), t73())
    })

    #8 -  IDEB - Escola pública - 5ª ano (séries iniciais)----
    ## - Tabela - IDEB - Escola pública - 5ª ano (séries iniciais)----
    # Filtra os dados
    edu8_1 <- reactive({
      if (input$edu8ri == "Pará") {
        x <- edu8 %>% filter(localidade !="Pará",ano == input$edu8ano) %>% 
          select(ri,localidade,valor) 
      }      else{
        x <- edu8 %>% filter(localidade !="Pará",ano == input$edu8ano) %>% 
          select(ri,localidade,valor) 
        x <- x %>% filter(ri == input$edu8ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu8_1(), {
      downset_Server("edu8_1", edu8_1(), t82())
    })
    ## - Gráfico - IDEB - Escola pública - 5ª ano (séries iniciais)----
    # Filtra os dados
    edu8_2 <- reactive({
      edu8 %>% filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu8_2(), {
      downset_Server("edu8_2", edu8_2(), t83())
    })

    #9 -  IDEB - Escola pública - 9ª ano (séries finais)----
    ## - Tabela - IDEB - Escola pública - 9ª ano (séries finais)----
    # Filtra os dados
    edu9_1 <- reactive({
      if (input$edu9ri == "Pará") {
        x <- edu9 %>% filter(localidade !="Pará",ano == input$edu9ano) %>% 
          select(ri,localidade,valor) 
      }      else{
        x <- edu9 %>% filter(localidade !="Pará",ano == input$edu9ano) %>% 
          select(ri,localidade,valor) 
        x <- x %>% filter(ri == input$edu9ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu9_1(), {
      downset_Server("edu9_1", edu9_1(), t92())
    })
    ## - Gráfico - IDEB - Escola pública - 9ª ano (séries finais)----
    # Filtra os dados
    edu9_2 <- reactive({
      edu9 %>% filter(localidade == "Pará")
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu9_2(), {
      downset_Server("edu9_2", edu9_2(), t93())
    })

    #10 - Número de Matrículas no Ensino Pré-Escolar por Esfera Administrativa----
    ## - Tabela - Número de Matrículas no Ensino Pré-Escolar----
    # Filtra os dados
    edu10_1 <- reactive({
      if (input$edu10ri == "Pará") {
        x <- edu10 %>% filter(localidade !="Pará",ano == input$edu10ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
      }      else{
        x <- edu10 %>% filter(localidade !="Pará",ano == input$edu10ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
        x <- x %>% filter(ri == input$edu10ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu10_1(), {
      downset_Server("edu10_1", edu10_1(), t102())
    })
    ## - Gráfico - Número de Matrículas no Ensino Pré-Escolar----
    # Filtra os dados
    edu10_2 <- reactive({
       edu10 %>% filter(localidade == "Pará") %>% 
        pivot_wider(names_from = categoria, values_from = valor)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu10_2(), {
      downset_Server("edu10_2", edu10_2(), t103())
    })

    #11 - Número de Matrículas no Ensino Fundamental por Esfera Administrativa----
    ## - Tabela - Número de Matrículas no Ensino Fundamental----
    # Filtra os dados
    edu11_1 <- reactive({
        if (input$edu11ri == "Pará") {
        x <- edu11 %>% filter(localidade !="Pará",ano == input$edu11ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
      }      else{
        x <- edu11 %>% filter(localidade !="Pará",ano == input$edu11ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
        x <- x %>% filter(ri == input$edu11ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu11_1(), {
      downset_Server("edu11_1", edu11_1(), t112())
    })
    ## - Gráfico - Número de Matrículas no Ensino Fundamental----
    # Filtra os dados
    edu11_2 <- reactive({
       edu11 %>% filter(localidade == "Pará") %>% 
        pivot_wider(names_from = categoria, values_from = valor)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu11_2(), {
      downset_Server("edu11_2", edu11_2(), t113())
    })

    #12 - Número de Matrículas no Ensino Médio por Esfera Administrativa----
    ##Tabela - Número de Matrículas no Ensino Médio----
    # Filtra os dados
    edu12_1 <- reactive({
      if (input$edu12ri == "Pará") {
        x <- edu12 %>% filter(localidade !="Pará",ano == input$edu12ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
      }      else{
        x <- edu12 %>% filter(localidade !="Pará",ano == input$edu12ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
        x <- x %>% filter(ri == input$edu12ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu12_1(), {
      downset_Server("edu12_1", edu12_1(), t122())
    })
    ##Gráfico - Número de Matrículas no Ensino Médio----
    # Filtra os dados
    edu12_2 <- reactive({
      edu12 %>% filter(localidade == "Pará") %>% 
        pivot_wider(names_from = categoria, values_from = valor)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu12_2(), {
      downset_Server("edu12_2", edu12_2(), t123())
    })

    #13 - Média de Alunos por Turma por Nível de Ensino----
    ##Tabela - Média de Alunos por Turma por Nível de Ensino----
    # Filtra os dados
    edu13_1 <- reactive({
      if (input$edu13ri == "Pará") {
        x <- edu13 %>% filter(localidade !="Pará",ano == input$edu13ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
      }      else{
        x <- edu13 %>% filter(localidade !="Pará",ano == input$edu13ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
        x <- x %>% filter(ri == input$edu13ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu13_1(), {
      downset_Server("edu13_1", edu13_1(), t132())
    })
    ##Gráfico - Média de Alunos por Turma por Nível de Ensino----
    # Filtra os dados
    edu13_2 <- reactive({
       edu13 %>% filter(localidade == "Pará") %>% 
        pivot_wider(names_from = categoria, values_from = valor) 
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu13_2(), {
      downset_Server("edu13_2", edu13_2(), t133())
    })

    #14 - Número de Docentes no Ensino Pré-escolar por Esfera Administrativa----
    ##Tabela - Número de Docentes no Ensino Pré-escolar----
    # Filtra os dados
    edu14_1 <- reactive({
       if (input$edu14ri == "Pará") {
        x <- edu14 %>% filter(localidade !="Pará",ano == input$edu14ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
      }      else{
        x <- edu14 %>% filter(localidade !="Pará",ano == input$edu14ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
        x <- x %>% filter(ri == input$edu14ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu14_1(), {
      downset_Server("edu14_1", edu14_1(), t142())
    })
    ##Gráfico - Número de Docentes no Ensino Pré-escolar----
    # Filtra os dados
    edu14_2 <- reactive({
       edu14 %>% filter(localidade == "Pará") %>% 
        pivot_wider(names_from = categoria, values_from = valor)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu14_2(), {
      downset_Server("edu14_2", edu14_2(), t143())
    })

    #15 - Número de Docentes no Ensino Fundamental por Esfera Administrativa----
    ##Tabela - Número de Docentes no Ensino Fundamental----
    # Filtra os dados
    edu15_1 <- reactive({
       if (input$edu15ri == "Pará") {
        x <- edu15 %>% filter(localidade !="Pará",ano == input$edu15ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
      }      else{
        x <- edu15 %>% filter(localidade !="Pará",ano == input$edu15ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
        x <- x %>% filter(ri == input$edu15ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu15_1(), {
      downset_Server("edu15_1", edu15_1(), t152())
    })
    ##Gráfico - Número de Docentes no Ensino Fundamental----
    # Filtra os dados
    edu15_2 <- reactive({
       edu15 %>% filter(localidade == "Pará") %>% 
        pivot_wider(names_from = categoria, values_from = valor)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu15_2(), {
      downset_Server("edu15_2", edu15_2(), t153())
    })

    #16 - Número de Docentes no Ensino Médio por Esfera Administrativa----
    ##Tabela - Número de Docentes no Ensino Médio----
    # Filtra os dados
    edu16_1 <- reactive({
       if (input$edu16ri == "Pará") {
        x <- edu16 %>% filter(localidade !="Pará",ano == input$edu16ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
      }      else{
        x <- edu16 %>% filter(localidade !="Pará",ano == input$edu16ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
        x <- x %>% filter(ri == input$edu16ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu16_1(), {
      downset_Server("edu16_1", edu16_1(), t162())
    })
    ##Gráfico - Número de Docentes no Ensino Médio----
    # Filtra os dados
    edu16_2 <- reactive({
      edu16 %>% filter(localidade == "Pará") %>% 
        pivot_wider(names_from = categoria, values_from = valor)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu16_2(), {
      downset_Server("edu16_2", edu16_2(), t163())
    })

    #17 - Estabelecimentos de Pré-Escola por Esfera Administrativa----
    ##Tabela - Estabelecimentos de Pré-Escola por Esfera Administrativa----
    # Filtra os dados
    edu17_1 <- reactive({
      if (input$edu17ri == "Pará") {
        x <- edu17 %>% filter(localidade !="Pará",ano == input$edu17ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
      }      else{
        x <- edu17 %>% filter(localidade !="Pará",ano == input$edu17ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
        x <- x %>% filter(ri == input$edu17ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu17_1(), {
      downset_Server("edu17_1", edu17_1(), t172())
    })
    ##Gráfico - Estabelecimentos de Pré-Escola por Esfera Administrativa----
    # Filtra os dados
    edu17_2 <- reactive({
      edu17 %>% filter(localidade == "Pará") %>% 
        pivot_wider(names_from = categoria, values_from = valor)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu17_2(), {
      downset_Server("edu17_2", edu17_2(), t173())
    })

    #18 - Estabelecimentos de Ensino Fundamental por Esfera Administrativa----
    ##Tabela - Estabelecimentos de Ensino Fundamental por Esfera Administrativa----
    # Filtra os dados
    edu18_1 <- reactive({
      if (input$edu18ri == "Pará") {
        x <- edu18 %>% filter(localidade !="Pará",ano == input$edu18ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
      }      else{
        x <- edu18 %>% filter(localidade !="Pará",ano == input$edu18ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
        x <- x %>% filter(ri == input$edu18ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu18_1(), {
      downset_Server("edu18_1", edu18_1(), t182())
    })
    ##Gráfico - Estabelecimentos de Ensino Fundamental por Esfera Administrativa----
    # Filtra os dados
    edu18_2 <- reactive({
      edu18 %>% filter(localidade == "Pará") %>% 
        pivot_wider(names_from = categoria, values_from = valor)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu18_2(), {
      downset_Server("edu18_2", edu18_2(), t183())
    })

    #19 - Estabelecimentos de Ensino Médio por Esfera Administrativa----
    ##Tabela - Estabelecimentos de Ensino Médio por Esfera Administrativa----
    # Filtra os dados
    edu19_1 <- reactive({
      if (input$edu19ri == "Pará") {
        x <- edu19 %>% filter(localidade !="Pará",ano == input$edu19ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
      }      else{
        x <- edu19 %>% filter(localidade !="Pará",ano == input$edu19ano) %>% 
          select(ri,localidade,categoria,valor) %>% 
          pivot_wider(names_from = categoria,values_from = valor)
        x <- x %>% filter(ri == input$edu19ri)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu19_1(), {
      downset_Server("edu19_1", edu19_1(), t192())
    })
    ##Gráfico - Estabelecimentos de Ensino Médio por Esfera Administrativa----
    # Filtra os dados
    edu19_2 <- reactive({
      edu19 %>% filter(localidade == "Pará") %>% 
        pivot_wider(names_from = categoria, values_from = valor)
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(edu19_2(), {
      t <- "edu19_2"
      downset_Server("edu19_2", edu19_2(), t193())
    })
  })
}

#Play do Módulo
# ui <- dashboardPage(
#   header = dashboardHeader(disable = ),
#   sidebar = dashboardSidebar(),
#   body = dashboardBody(fluidPage(
#     social_educacao_pa_ui("social_educacao_pa")
#   ))
# )
# 
# 
# server <- function(input, output) {
#   social_educacao_pa_Server("social_educacao_pa")
# }
# 
# shinyApp(ui, server)
