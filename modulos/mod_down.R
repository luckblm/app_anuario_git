#Módulo - down
down_ui <- function(id) {
  fluidPage(useShinyjs(), introjsUI(), fluidRow(
    #Controles----
    ##Filtro Tema----
    box(
      width = 12,
      collapsible = T,
      solidHeader = T,
      status = "black",
      title = tags$b("Filtro"),
      icon = fa_i(name = "database", verify_fa = FALSE),
      tags$div(
        style = 'float:left;position: relative;padding-right: 15px;',
        introBox(
          pickerInput(
            NS(id, "tema_select"),
            label = "Temática",
            choices = unique(base_dados[["tematica"]]),
            width = "250px",
            multiple = T,
            options = list(
              `actions-box` = TRUE,
              `deselect-all-text` = "Limpar",
              `select-all-text` = "Selecionar Tudo",
              `none-selected-text` = "--Opções--",
              `live-search` = TRUE
            )
          ),
          data.step = 1,
          data.intro = "Esolha uma ou mais Temática para começar",
          data.position = "right"
          
        )
      ),
      ##Filtro Sub Temática----
      tags$div(
        style = 'float:left;position: relative;padding-right: 15px;',
        introBox(
          pickerInput(
            NS(id, "subtema_select"),
            label = "Sub temática",
            choices = NULL,
            width = "250px",
            multiple = T,
            options = list(
              `actions-box` = TRUE,
              `deselect-all-text` = "Limpar",
              `select-all-text` = "Selecionar Tudo",
              `none-selected-text` = "--Opções--",
              `live-search` = TRUE
            )
          ),
          data.step = 2,
          data.position = "right",
          data.intro = HTML(
            "<p><b>Selecione uma subtemática</b></p>
            <p>No caso de Demografia, Infraestrutura e Meio Ambiente, selecione o ( - )
            para acessar as opções de indicadores. Para as temáticas de Economia e Social,
            escolha um ou mais subtemas.</p>"
          )
        )
      ),
      ##Filtro Indicador----
      tags$div(
        style = 'float:left;position: relative;padding-right: 15px;',
        introBox(
          pickerInput(
            NS(id, "indicador_select"),
            label = "Indicador",
            choices = NULL,
            width = "250px",
            multiple = T,
            options = list(
              `actions-box` = TRUE,
              `deselect-all-text` = "Limpar",
              `select-all-text` = "Selecionar Tudo",
              `none-selected-text` = "--Opções--",
              `live-search` = TRUE
            )
          ),
          data.step = 3,
          data.intro = HTML("Escolha um ou mais indicadores"),
          data.position = "right"
        )
      ),
      ##Filtro RI----
      tags$div(
        style = 'float:left;position: relative;padding-right: 15px;',
        introBox(
          pickerInput(
            NS(id, "ri_select"),
            label = "Pará/Região de Integração",
            choices = NULL,
            width = "200px",
            multiple = T,
            options = list(
              `actions-box` = TRUE,
              `deselect-all-text` = "Limpar",
              `select-all-text` = "Selecionar Tudo",
              `none-selected-text` = "--Opções--",
              `live-search` = TRUE
            )
          ),
          data.step = 4,
          data.position = "right",
          data.intro = HTML(
            "<p>Filtre as dados em nível Estadual ou por Região de Integração</p>"
          )
        )
      ),
      ##Filtro Município----
      tags$div(
        style = 'float:left;position: relative;padding-right: 15px;',
        introBox(
          data.step = 5,
          data.position = "right",
          data.intro = HTML(
            "<p>Filtre as dados por um ou mais Municípios ou em nivel Estadual</p>"
          ),
          pickerInput(
            NS(id, "muni_select"),
            label = "Estado ou Municipio(s)",
            choices = NULL,
            width = "250px",
            multiple = T,
            options = list(
              `actions-box` = TRUE,
              `deselect-all-text` = "Limpar",
              `select-all-text` = "Selecionar Tudo",
              `none-selected-text` = "--Opções--",
              `live-search` = TRUE
            )
          )
        )
      ),
      ##Filtro Ano----
      tags$div(
        style = 'float:left;position: relative;padding-right: 15px;',
        introBox(
          data.step = 6,
          data.position = "left",
          data.intro = HTML("<p>Filtre as dados por um ou mais Anos</p>"),
          pickerInput(
            NS(id, "ano_select"),
            label = "Ano",
            choices = NULL,
            width = "220px",
            multiple = T,
            options = list(
              `actions-box` = TRUE,
              `deselect-all-text` = "Limpar",
              `select-all-text` = "Selecionar Tudo",
              `none-selected-text` = "--Opções--",
              `live-search` = TRUE
            )
          )
        )
      )
    )
  ), #Tabela de exibição----
  fluidRow(
    box(
      width = 12,
      collapsible = T,
      solidHeader = T,
      status = "black",
      title = tags$b("Base de Dados"),
      icon = fa_i(name = "server", verify_fa = FALSE),
      reactableOutput(NS(id, "db")),
      textOutput(NS(id, "teste")),
      introBox(
        data.step = 7,
        data.position = "top",
        data.intro = HTML(
          "<p>Para baixar a sua seleção, clique no <b>Botão Download</b>,
                                   caso queira baixar toda a base de dados, clique em <b>Baixar tudo.</b></p>"
        ),
        actionButton(NS(id, "modal_show"), tags$b("Download")),
        actionButton(NS(id, "modal_show2"), tags$b("Baixar tudo")),
        actionButton(NS(id, "limpar"), tags$b("Limpar Seleção"))
      ),
      div(style = "position:absolute; bottom:10px; right:10px;", actionButton(NS(id, "ajuda"), tags$b("Tutorial")))
    )
  ))
}

#Função do modulo servidor
down_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    #Filtro Subtema----
    subtema_ob <- reactive({
      base_dados %>% filter(tematica %in% input$tema_select)
    })
    #Sub temática
    observeEvent(subtema_ob(), {
      choices <- unique(subtema_ob()[["subtema"]])
      updatePickerInput(inputId = "subtema_select", choices = choices, session)
    })
    
    indicador_ob <- reactive({
      req(input$tema_select)
      subtema_ob() %>% filter(subtema %in% input$subtema_select)
    })
    #Indicador
    observeEvent(indicador_ob(), {
      choices <- unique(indicador_ob()[["indicador"]])
      updatePickerInput(inputId = "indicador_select", choices = choices, session)
    })
    #Região de Integração
    observeEvent(subtema_ob(), {
      choices <- unique(subtema_ob()[["ri"]])
      updatePickerInput(inputId = "ri_select", choices = choices, session)
    })
    #Município
    observeEvent(subtema_ob(), {
      choices <- unique(subtema_ob()[["localidade"]])
      updatePickerInput(inputId = "muni_select", choices = choices, session)
    })
    #Ano
    observeEvent(subtema_ob(), {
      choices <- unique(subtema_ob()[["ano"]])
      updatePickerInput(inputId = "ano_select", choices = choices, session)
    })
    #Filtro inteligente----
    basedados_ob <- reactive({
      req(input$tema_select)
      a <- if (length(input$tema_select) > 0) {
        base_dados %>% filter(tematica %in% input$tema_select)
      }
      
      b <- if (length(input$subtema_select) > 0) {
        a %>% filter(subtema %in% input$subtema_select)
      } else {
        a
      }
      
      c <- if (length(input$indicador_select) > 0) {
        b %>% filter(indicador %in% input$indicador_select)
      } else {
        b
      }
      
      d <- if (length(input$ri_select) > 0) {
        c %>% filter(ri %in% input$ri_select)
      } else {
        c
      }
      
      e <- if (length(input$muni_select) > 0) {
        c %>% filter(localidade %in% input$muni_select)
      } else {
        d
      }
      
      f <- if (length(input$ano_select) > 0) {
        e %>% filter(ano %in% input$ano_select)
      } else {
        e
      }
    })
    
    #Saída dos dados filtrados----
    output$db <- renderReactable({
      basedados_ob() %>%
        reactable(
          defaultPageSize = 15,
          striped = FALSE,
          highlight = TRUE,
          bordered = TRUE,
          outlined = TRUE,
          resizable = TRUE,
          showSortable = TRUE,
          pagination = TRUE,height = 600,
          columns = list(
            tematica = colDef(name = "Temática",minWidth = 70),
            subtema = colDef(name = "Sub Temática",minWidth = 90),
            indicador = colDef(name = "Indicador",minWidth = 300),
            ri = colDef(name = "Região de Integração"),
            localidade = colDef(name = "Municípios",minWidth = 70),
            categoria = colDef(name = "Categoria"),
            ano = colDef(name = "Ano",minWidth = 50),
            valor = colDef( name = "Valores", minWidth = 80,format = colFormat(separators = T, locales = "pt-BR"))
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
    
    # output$db <- renderDataTable({
    #   basedados_ob() %>%
    #     datatable(
    #       rownames = F,
    #       style = "bootstrap",
    #       options = list(
    #         dom = 'lfrtip',
    #         language = list(url = '//cdn.datatables.net/plug-ins/1.11.5/i18n/pt-BR.json'),
    #         lengthMenu = list(c(10, 25, 50, -1), c(10, 25, 50, "Todos")),
    #         paging = T,
    #         bFilter = F
    #       )
    #     )
    # })
    
    dados_filtrados <- reactive({
      basedados_ob()
    })
    
    #Modal  com opções de extenção de Download da base de dados----
    observeEvent(input$modal_show, {
      if (length(input$tema_select >= 1)) {
        showModal(
          modalDialog(
            title = h4("Escolha a extenção de arquivo desejada para Download!"),
            downloadButton(
              NS(id, "download1"),
              label = "Excel",
              icon = icon("file-excel"),
              class = "btn btn-link"
            ),
            downloadButton(
              NS(id, "download2"),
              label = "CSV",
              icon = icon("file-csv"),
              class = "btn btn-link"
            ),
            downloadButton(
              NS(id, "download3"),
              label = "RData",
              icon = icon("database"),
              class = "btn btn-link"
            ),
            easyClose = F,
            footer = tagList (modalButton(h4("Fechar"))),
            fade = T,
            size = 'm'
          )
        )
      } else{
        sendSweetAlert(
          session = session,
          title = "Aviso !!!",
          text = "É necessário selecionar dados!",
          type = "warning"
        )
      }
    })
    #Funções de Download pós filtro----
    ##Download em Excel----
    output$download1 <- downloadHandler(
      filename = function() {
        paste0("Dados.xlsx")
      },
      content = function(file) {
        write.xlsx(dados_filtrados(), file)
      }
    )
    ##Download em CSV----
    output$download2 <- downloadHandler(
      filename = function() {
        paste0("Dados.csv")
      },
      content = function(file) {
        write.csv(dados_filtrados(), file)
      }
    )
    ##Download em RDS----
    output$download3 <- downloadHandler(
      filename = function() {
        paste0("Dados.rds")
      },
      content = function(file) {
        write_rds(dados_filtrados(), file)
      }
    )
    
    #Fim Modal Donwload
    # Botão para baixar toda a base de dados com opção em vários formatos----
    observeEvent(input$modal_show2, {
      showModal(
        modalDialog(
          title = h4("Escolha a extenção de arquivo desejada para Download!"),
          downloadButton(
            NS(id, "download1_todo"),
            label = "Excel",
            icon = icon("file-excel"),
            class = "btn btn-link"
          ),
          downloadButton(
            NS(id, "download2_todo"),
            label = "CSV",
            icon = icon("file-csv"),
            class = "btn btn-link"
          ),
          downloadButton(
            NS(id, "download3_todo"),
            label = "RData",
            icon = icon("database"),
            class = "btn btn-link"
          ),
          easyClose = F,
          footer = tagList (modalButton(h4("Fechar"))),
          fade = T,
          size = 'm'
        )
      )
      
    })
    ##Download em Excel----
    output$download1_todo <- downloadHandler(
      filename = function() {
        paste0("Dados.xlsx")
      },
      content = function(file) {
        write.xlsx(base_dados, file)
      }
    )
    ##Download em CSV----
    output$download2_todo <- downloadHandler(
      filename = function() {
        paste0("Dados.csv")
      },
      content = function(file) {
        write.csv(base_dados, file)
      }
    )
    ##Download em RDS----
    output$download3_todo <- downloadHandler(
      filename = function() {
        paste0("Dados.rds")
      },
      content = function(file) {
        write_rds(base_dados, file)
      }
    )
    
    #Fim Modal Donwload
    #Limpa a seleção em todas opções, por existir hierarquia basta limpar o sub tem por estar no topo----
    observeEvent (input$limpar, {
      reset ("tema_select")
    })
    
    
    observeEvent(input$ajuda, {
      showModal(
        modalDialog(
          title = "Tutorial",
          tags$p(
            style = "text-align:justify;",
            "Este é um tutorial sobre como usar a seção de download. Primeiro, você precisa selecionar os dados que deseja baixar usando os filtros disponíveis. Depois de selecionar os dados, clique no botão 'Download' para baixar os dados selecionados. Se você quiser baixar todos os dados, clique no botão 'Baixar tudo'."
          ),
          easyClose = TRUE,
          footer = tagList(actionButton(
            NS(id, "iniciar"), tags$b("Iniciar")
          ), modalButton("Fechar")),
          style = "border-radius: 10px;"
        )
      )
    })
    #Remove o Modal ao Clicar em iniciar
    observeEvent(input$iniciar, {
      removeModal()
    })
    observeEvent(input$iniciar, introjs(
      session,
      options = list(
        "nextLabel" = "Continue",
        "prevLabel" = "Voltar",
        "doneLabel" = "Sair do Tutorial"
      )
    ))
  })
}

# Play do Módulo
# ui <- dashboardPage(header = dashboardHeader(),
#                     sidebar = dashboardSidebar(),
#                     body = dashboardBody(fluidPage(down_ui("down"))))
# server <- function(input, output) {
#   down_Server("down")
# }
# 
# shinyApp(ui, server)