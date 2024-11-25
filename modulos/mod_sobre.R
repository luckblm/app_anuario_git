#Módulo - sobre
sobre_ui <- function(id) {
  fluidPage(
    fluidRow(
      box(
        title = tags$b("Sobre o Dashboard"), 
        width = 12,
        column(12,
               tags$div(
                 HTML("
          <h4>O Anuário Estatístico do Estado do Pará</h4>
          
          <p style='text-align: justify;font-size: 16px;'>
          O Anuário Estatístico do Estado do Pará é uma valiosa fonte de informações que oferece uma visão abrangente e detalhada sobre as estatísticas e dados do Pará. Este anuário é uma ferramenta essencial para entender a dinâmica, o crescimento e os desafios enfrentados por esse vibrante estado brasileiro.
          <br><br>
          Com uma ampla gama de informações demográficas, econômicas, sociais e ambientais, o Anuário Estatístico do Pará é uma referência confiável para pesquisadores, acadêmicos, tomadores de decisões e qualquer pessoa interessada em explorar e analisar os dados do estado. Ele oferece uma visão panorâmica, permitindo que você mergulhe em aspectos como a distribuição da população, indicadores de saúde, educação, economia, agricultura, meio ambiente e muito mais.
          <br><br>
          Além disso, o anuário frequentemente fornece análises aprofundadas e interpretações dos dados, ajudando a contextualizar as tendências e os desenvolvimentos ao longo do tempo. Isso torna o Anuário Estatístico do Estado do Pará uma ferramenta valiosa para pesquisas acadêmicas, planejamento estratégico e políticas públicas.
          <br><br>
          Seja você um profissional que busca informações para embasar suas decisões, um estudante que deseja aprofundar seu conhecimento sobre o Pará ou alguém curioso sobre os números e estatísticas que moldam o estado, o Anuário Estatístico do Pará é uma fonte confiável e essencial para explorar e compreender essa região diversificada e em constante evolução.
           <br><br>
          
          <h4>Explorando o Futuro com o Dashboard do Anuário Estatístico do Estado do Pará</h4>
          
          O Dashboard do Anuário Estatístico do Estado do Pará é uma poderosa ferramenta de visualização de dados construída com tecnologia de ponta, que permite explorar e analisar as estatísticas do estado de forma interativa e eficaz.

          Para criar este Dashboard, foram utilizadas as seguintes tecnologias:

          <ul style='text-align: justify;font-size: 16px;'>
          <li><strong>Linguagem R:</strong> A base do Dashboard é construída na linguagem de programação R, amplamente reconhecida por suas capacidades estatísticas e gráficas. O R é uma escolha sólida para manipular e analisar dados de forma eficaz.</li>
      
          <li><strong>Pacote Shiny:</strong> O Shiny é um pacote em R que permite criar aplicativos web interativos de maneira simples e eficiente. Ele desempenha um papel fundamental na construção deste Dashboard, tornando possível a interatividade e a exploração dos dados de forma dinâmica.</li>
      
          <li><strong>Shiny DashboardPlus:</strong> O Shiny DashboardPlus é uma extensão do Shiny Dashboard que oferece funcionalidades avançadas de personalização e aprimoramento do layout. Ele permite criar painéis de controle atraentes e funcionais para apresentar os dados de forma eficaz.</li>
          </ul>
           </p>
           <p style='text-align: justify;font-size: 16px;'>
           Com a combinação dessas tecnologias, o Dashboard do Anuário Estatístico do Estado do Pará oferece uma experiência de visualização de dados intuitiva e rica, permitindo aos usuários explorar as estatísticas do estado de maneira detalhada e significativa.
           Este Dashboard representa um exemplo notável de como a tecnologia pode ser aproveitada para tornar os dados mais acessíveis e compreensíveis, contribuindo para uma melhor compreensão e tomada de decisões informadas no contexto do Pará.
          </p>
          "),
                 tags$hr(),
                 tags$div(
                   style = "text-align: center;",
                   tags$img(src = "Rlogo.png", width = 100, style = "margin-right: 10px;") %>% animateAppend("pulse",speed = "slow"),
                   tags$img(src = "Shinyrlogo.png", width = 50, style = "margin-left: 10px;") %>% animateAppend("pulse",speed = "slow")
                 )
               )
        ),
        column(12,
               fluidRow(
                 column(6,
                        tags$div(
                          tags$a(href = "https://www.fapespa.pa.gov.br/",
                                 tags$img(src = "Logo2022_FAPESPA_2.jpg", width = 230, style = "margin: 0 auto;"),
                                 style = "text-align: center;"
                          )
                        )
                 ),
                 
                 column(6,
                        tags$div(
                          tags$div(
                            tags$p("Fundação Amazônia de Amparo a Estudos e Pesquisas"),
                            tags$p("Avenida Presidente Vargas, nº 670. Belém - PA"),
                            tags$p("Atendimento: de 2ª a 6ª feira, das 08:00 às 14:00"),
                            style = "text-align: right; padding: 10px;"
                          )
                        )
                 )
               )
        )
      )
    )
  )
}

#Função do modulo servidor
sobre_Server <- function(id) {}

# sobre_App <- function() {
#   ui <-
#     fluidPage(sobre_ui("sobre"))
#   server <- function(input, output, session) {
#     sobre_Server("sobre")
#   }
#   shinyApp(ui, server)
# }
#Play do Módulo
ui <- dashboardPage(
  header = dashboardHeader(),
  sidebar = dashboardSidebar(),
  body = dashboardBody(fluidPage(sobre_ui("sobre")))

)
server <- function(input, output) {
  sobre_Server("sobre")
}

shinyApp(ui, server)