# Interface do usuário---------------------------------------
#Carregando----
source('global.R')
shinyApp(
  ui = dashboardPage(
    header = dashboardHeader(
      title = span(tags$b("Anuário Estatístico")),
      
      #Icone de redes sociais----
      tags$li(class = "dropdown",
              a(href = "https://www.facebook.com/FapespaPA/",
                class = "fa fa-facebook",
                target = "_blank"
              )),
      
      tags$li(class = "dropdown",
              a(href = "https://www.instagram.com/fapespa/",
                class = "fa fa-instagram",
                target = "_blank"
              )
      )
    ), 
    # dbHeader,
    skin = "black",
    scrollToTop = T,
    options = list(sidebarExpandOnHover = T),
    sidebar = dashboardSidebar(
      minified = F,
      collapsed = F,
      #Imagem
      tags$img(src = "Logo2022_FAPESPA_2.jpg", width = 230),
      sidebarMenu(
        id = "tabs",
        menuItem("Demografia",tabName = "demografia",icon = shiny::icon("user"),
                 menuSubItem("Pará", tabName = "pa_demo"),
                 menuSubItem("Municípios", tabName = "muni_demo")
        ),
        menuItem("Economia",tabName = "economia", icon = icon("hand-holding-usd"),
                 menuItem("Lavoura Permanente",tabName = "lavoura_permanente",
                          menuSubItem("Pará", tabName = "pa_lp"),
                          menuSubItem("Município", tabName = "muni_lp")
                 ),
                 menuItem("Lavoura Temporária", tabName = "lavoura_temporaria",
                          menuSubItem("Pará", tabName = "pa_lt"),
                          menuSubItem("Município", tabName = "muni_lt")       
                 ),
                 menuItem("Pecuária",tabName = "pecuaria",
                          menuSubItem("Pará", tabName = "pa_pec"),
                          menuSubItem("Município", tabName = "muni_pec")
                 ),
                 menuItem("Extração Vegetal",tabName = "exv",
                          menuSubItem("Pará", tabName = "pa_exv"),
                          menuSubItem("Município", tabName = "muni_exv")
                 ),
                 menuItem("PIB",tabName = "pib",
                          menuSubItem("Pará", tabName = "pa_pib"),
                          menuSubItem("Município", tabName = "muni_pib")
                 ),
                 menuItem("Balança Comercial",tabName = "bc",
                          menuSubItem("Pará", tabName = "pa_bc"),
                          menuSubItem("Município", tabName = "muni_bc")
                 ),
                 menuItem("Finanças Públicas",tabName = "fp",
                          menuSubItem("Pará", tabName = "pa_fp"),
                          menuSubItem("Município", tabName = "muni_fp")
                 )
        ),
        menuItem("Infraestrutura", tabName = "infraestrutura", icon = icon("industry"),
                 menuSubItem("Pará", tabName = "pa_inf"),
                 menuSubItem("Município", tabName = "muni_inf")
        ),
        menuItem("Meio Ambiente", tabName = "meioambiente", icon = icon("leaf"),
                 menuSubItem("Pará", tabName = "pa_mab"),
                 menuSubItem("Município", tabName = "muni_mab")
        ),
        menuItem("Social", tabName = "social", icon = icon("users"),
                 menuItem("Educação",tabName = "educacao",
                          menuSubItem("Pará", tabName = "pa_edu"),
                          menuSubItem("Município", tabName = "muni_edu")
                 ),
                 menuItem("Inclusão Social",tabName = "inclusao_social",
                          menuSubItem("Pará", tabName = "pa_inc"),
                          menuSubItem("Município", tabName = "muni_inc")
                 ),
                 menuItem("Mercado de Trabalho",tabName = "mercado_trabalho",
                          menuSubItem("Pará", tabName = "pa_merc"),
                          menuSubItem("Município", tabName = "muni_merc")
                 ),
                 menuItem("Previdência Social",tabName = "previdencia_social",
                          menuSubItem("Pará", tabName = "pa_prev"),
                          menuSubItem("Município", tabName = "muni_prev")
                 ),
                 menuItem("Saúde",tabName = "saude",
                          menuSubItem("Pará", tabName = "pa_sd"),
                          menuSubItem("Município", tabName = "muni_sd")
                 ),
                 menuItem("Segurança",tabName = "seguranca",
                          menuSubItem("Pará", tabName = "pa_seg"),
                          menuSubItem("Município", tabName = "muni_seg")
                 )
        ),
        menuSubItem("Donwloads", tabName = "download", icon = icon("download")),
        
        menuItem("Sobre", tabName = "sobre",icon = icon("book"))
      )
    ),
    body = dashboardBody(
      fluidPage(
        tags$head(
          tags$link(rel = "shortcut icon", href = "icons8-favicon-94.png", type = "image/x-icon"),
          
          tags$link(
            rel = "stylesheet",
            href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css"
          )
          
          
        )
      ),
      tabItems(
        #Demografia----
        tabItem(tabName = "pa_demo", fluidPage(demografia_pa_ui("demografia_pa"))),
        tabItem(tabName = "muni_demo",fluidPage(demografia_mp_ui("demografia_mp"))),
        #Economia----
        tabItem(tabName = "pa_lp", fluidPage(economia_lp_pa_ui("economia_lp_pa"))),
        tabItem(tabName = "muni_lp",fluidPage(economia_lp_mp_ui("economia_lp_mp"))),
        tabItem(tabName = "pa_lt",fluidPage(economia_lt_pa_ui("economia_lt_pa"))),
        tabItem(tabName = "muni_lt",fluidPage(economia_lt_mp_ui("economia_lt_mp"))),
        tabItem(tabName = "pa_pec",fluidPage(economia_pec_pa_ui("economia_pec_pa"))),
        tabItem(tabName = "muni_pec",fluidPage(economia_pec_mp_ui("economia_pec_mp"))),
        tabItem(tabName = "pa_exv",fluidPage(economia_exv_pa_ui("economia_exv_pa"))),
        tabItem(tabName = "muni_exv",fluidPage(economia_exv_mp_ui("economia_exv_mp"))),
        tabItem(tabName = "pa_pib",fluidPage(economia_pib_pa_ui("economia_pib_pa"))),
        tabItem(tabName = "muni_pib",fluidPage(economia_pib_mp_ui("economia_pib_mp"))),
        tabItem(tabName = "pa_bc",fluidPage(economia_bc_pa_ui("economia_bc_pa"))),
        tabItem(tabName = "muni_bc",fluidPage(economia_bc_mp_ui("economia_bc_mp"))),
        tabItem(tabName = "pa_fp",fluidPage(economia_fp_pa_ui("economia_fp_pa"))),
        tabItem(tabName = "muni_fp",fluidPage(economia_fp_mp_ui("economia_fp_mp"))),
        #Infraestrutura----
        tabItem(tabName = "pa_inf",fluidPage(infraestrutura_pa_ui("infraestrutura_pa"))),
        tabItem(tabName = "muni_inf",fluidPage(infraestrutura_mp_ui("infraestrutura_mp"))),
        #Meio Ambiente----
        tabItem(tabName = "pa_mab",fluidPage(meio_ambiente_pa_ui("meio_ambiente_pa"))),
        tabItem(tabName = "muni_mab",fluidPage(meio_ambiente_mp_ui("meio_ambiente_mp"))),
        #Social
        tabItem(tabName = "pa_edu",fluidPage(social_educacao_pa_ui("social_educacao_pa"))),
        tabItem(tabName = "muni_edu",fluidPage(social_educacao_mp_ui("social_educacao_mp"))),
        tabItem(tabName = "pa_inc",fluidPage(social_inclusao_social_pa_ui("social_inclusao_social_pa"))),
        tabItem(tabName = "muni_inc",fluidPage(social_inclusao_social_mp_ui("social_inclusao_social_mp"))),
        tabItem(tabName = "pa_merc",fluidPage(social_merc_tab_pa_ui("social_merc_tab_pa"))),
        tabItem(tabName = "muni_merc",fluidPage(social_merc_tab_mp_ui("social_merc_tab_mp"))),
        tabItem(tabName = "pa_prev",fluidPage(social_previdencia_pa_ui("social_previdencia_pa"))),
        tabItem(tabName = "muni_prev",fluidPage(social_previdencia_mp_ui("social_previdencia_mp"))),
        tabItem(tabName = "pa_sd",fluidPage(social_saude_pa_ui("social_saude_pa"))),
        tabItem(tabName = "muni_sd",fluidPage(social_saude_mp_ui("social_saude_mp"))),
        tabItem(tabName = "pa_seg",fluidPage(social_seguranca_pa_ui("social_seguranca_pa"))),
        tabItem(tabName = "muni_seg",fluidPage(social_seguranca_mp_ui("social_seguranca_mp"))),
        #Donwload Avançado
        tabItem(tabName = "download",fluidPage(down_ui("down"))),
        tabItem(tabName = "sobre",fluidPage(sobre_ui("sobre"))),
        #Abas Radar de indicadores----
        tabItem(tabName = "radar")
      )
    ),
    footer = dashboardFooter(
      left = tags$b("Fapespa"), 
      right = "Belém-PA, 2022 v.1"
    ),
    title = "Dashboard Fapespa"
  ),
  server = function(input, output,session) {
    # #Módulos _ Estadual----
    demografia_pa_Server("demografia_pa")
    economia_lp_pa_Server("economia_lp_pa")
    economia_lt_pa_Server("economia_lt_pa")
    economia_pec_pa_Server("economia_pec_pa")
    economia_exv_pa_Server("economia_exv_pa")
    economia_pib_pa_Server("economia_pib_pa")
    economia_bc_pa_Server("economia_bc_pa")
    economia_fp_pa_Server("economia_fp_pa")
    infraestrutura_pa_Server("infraestrutura_pa")
    meio_ambiente_pa_Server("meio_ambiente_pa")
    social_educacao_pa_Server("social_educacao_pa")
    social_inclusao_social_pa_Server("social_inclusao_social_pa")
    social_merc_tab_pa_Server("social_merc_tab_pa")
    social_previdencia_pa_Server("social_previdencia_pa")
    social_saude_pa_Server("social_saude_pa")
    social_seguranca_pa_Server("social_seguranca_pa")
    #Módulos _ Municipal----
    demografia_mp_Server("demografia_mp")
    economia_lp_mp_Server("economia_lp_mp")
    economia_lt_mp_Server("economia_lt_mp")
    economia_pec_mp_Server("economia_pec_mp")
    economia_exv_mp_Server("economia_exv_mp")
    economia_pib_mp_Server("economia_pib_mp")
    economia_bc_mp_Server("economia_bc_mp")
    economia_fp_mp_Server("economia_fp_mp")
    infraestrutura_mp_Server("infraestrutura_mp")
    meio_ambiente_mp_Server("meio_ambiente_mp")
    social_educacao_mp_Server("social_educacao_mp")
    social_inclusao_social_mp_Server("social_inclusao_social_mp")
    social_merc_tab_mp_Server("social_merc_tab_mp")
    social_previdencia_mp_Server("social_previdencia_mp")
    social_saude_mp_Server("social_saude_mp")
    social_seguranca_mp_Server("social_seguranca_mp")
    #Downloads----
    down_Server("down")
    #Sobre----
    sobre_Server("sobre")
  }
)



