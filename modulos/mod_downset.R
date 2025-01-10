#Função de modulo de UI Donwload
downset_ui <- function(id) {
  fluidPage(
    
    tags$head(
      tags$style(HTML("
    .btn-custom {
      background-color: #4CAF50; /* Cor de fundo (verde) */
      color: white; /* Cor do texto e ícone */
      border-color: #4CAF50; /* Cor da borda */
    }
    .btn-custom:hover {
      background-color: #45a049; /* Cor ao passar o mouse */
      color: white; /* Certifica-se de manter o texto e ícone brancos */
    }
    
    .btn-excel {
      background-color: #217346; /* Verde para Excel */
      color: white; 
      border-color: #217346;
    }
    .btn-excel:hover {
      background-color: #1a5b37; /* Cor ao passar o mouse */
    }
    .btn-csv {
      background-color: #ff9900; /* Laranja para CSV */
      color: white; 
      border-color: #ff9900;
    }
    .btn-csv:hover {
      background-color: #cc7a00; /* Cor ao passar o mouse */
    }
    .btn-rdata {
      background-color: #0073e6; /* Azul para RData */
      color: white; 
      border-color: #0073e6;
    }
    .btn-rdata:hover {
      background-color: #005bb5; /* Cor ao passar o mouse */
    }
  "
  ))
    ),
    
    
    dropMenu(
      padding = "20px",
      placement = "left",
      actionButton(NS(id, "demo1down"),
                   tags$b("Download"),
                   icon = icon("download"),
                   class = "btn-custom"),
      tags$p("Escolha o formato de arquivo para download:"),
      tags$p("Excel (XLSX), CSV ou RData. Obrigado!"),
      tags$hr(),
      downloadButton(
        NS(id, "xlsx"),
        label = "Excel",
        icon = icon("file-excel"),
        class = "btn btn-excel"
      ),
      downloadButton(
        NS(id, "csv"),
        label = "CSV",
        icon = icon("file-csv"),
        class = "btn btn-csv"
      ),
      downloadButton(
        NS(id, "rds"),
        label = "RData",
        icon = icon("database"),
        class = "btn btn-rdata"
      )
    )
  )
}

#Função de modulo de servidor
downset_Server <- function(id,df,nome) {
  moduleServer(id, function(input, output, session) {
    
    output$xlsx <- downloadHandler(
      filename = function() {
        paste0(nome,".xlsx")
      },
      content = function(file) {
        write.xlsx(df, file)
      }
    )
    
    output$csv <-   downloadHandler(
      filename = function() {
        paste0(nome,".csv")
      },
      content = function(file) {
        write.csv(df, file)
      }
    )
    

    output$rds <-  downloadHandler(
      filename = function() {
        paste0(nome,".rds")
      },
      content = function(file) {
        write_rds(df, file)
      }
    )
  }
)
}
    

# #Função de modulo de APP
# download_App <- function() {
#   ui <- fluidPage(download_ui("download"))
#   server <- function(input, output, session) {
#     download_Server("donwload",df)
#   }
#   shinyApp(ui, server)
# }



# #Play do Módulo
# ui <- dashboardPage(
#   header = dashboardHeader(),
#   sidebar = dashboardSidebar(),
#   body = dashboardBody(fluidPage(downset_ui("download")))
# 
# )
# server <- function(input, output) {
#   df <- reactive({
#     demo1
#   })
#   downset_Server("download",df())
# }
# 
# shinyApp(ui, server)

