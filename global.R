#Carregamento de Pacotes ----
source("load_pacotes.R")

#Carregando Módulos
#Módulos Pará
#Demografia
source("modulos/01_mod_demografia_pa.R")
source("modulos/02_mod_demografia_mp.R")
#Economia
source("modulos/03_mod_economia_lp_pa.R")
source("modulos/04_mod_economia_lp_mp.R")
source("modulos/05_mod_economia_lt_pa.R")
source("modulos/06_mod_economia_lt_mp.R")
source("modulos/07_mod_economia_pecuaria_pa.R")
source("modulos/08_mod_economia_pecuaria_mp.R")
source("modulos/09_mod_economia_exv_pa.R")
source("modulos/10_mod_economia_exv_mp.R")
source("modulos/11_mod_economia_pib_pa.R")
source("modulos/12_mod_economia_pib_mp.R")
source("modulos/13_mod_economia_bc_pa.R")
source("modulos/14_mod_economia_bc_mp.R")
source("modulos/15_mod_economia_fp_pa.R")
source("modulos/16_mod_economia_fp_mp.R")
#Infraestrutura
source("modulos/17_mod_infraestrutura_pa.R")
source("modulos/18_mod_infraestrutura_mp.R")
#Meio Ambiente
source("modulos/19_mod_meio_ambiente_pa.R")
source("modulos/20_mod_meio_ambiente_mp.R")
#Social
source("modulos/21_mod_social_educacao_pa.R")
source("modulos/22_mod_social_educacao_mp.R")
source("modulos/23_mod_social_inclusão_social_pa.R")
source("modulos/24_mod_social_inclusão_social_mp.R")
source("modulos/25_mod_social_merc_trabalho_pa.R")
source("modulos/26_mod_social_merc_trabalho_mp.R")
source("modulos/27_mod_social_previdencia_pa.R")
source("modulos/28_mod_social_previdencia_mp.R")
source("modulos/29_mod_social_saude_pa.R")
source("modulos/30_mod_social_saude_mp.R")
source("modulos/31_mod_social_seguranca_pa.R")
source("modulos/32_mod_social_seguranca_mp.R")

#Sidebar itens
source("modulos/mod_down.R")
#Função download
source("modulos/mod_downset.R")
#Sobre----
source("modulos/mod_sobre.R")

#Carregamento de funções
#Função em java Script para Separado de milhar e decimal no echart4R
formatar_numero_br <- function(serie) {
  htmlwidgets::JS(
    glue::glue(
      "function(params) {return Intl.NumberFormat('pt-BR', { style: 'decimal'}).format(params.value[{{serie}}]);}",
      .open = "{{",
      .close = "}}"
    )
  )
}

# #Função para formatar lengenda em leaflet
labelFormat_decimal <- function (prefix = "", suffix = "", between = " &ndash; ", digits = 3,
                                 big.mark = ",", transform = identity, decimal.mark = "."){
  formatNum <- function(x) {
    format(round(transform(x), digits), trim = TRUE, scientific = FALSE,
           big.mark = big.mark, decimal.mark = decimal.mark)
  }
  function(type, ...) {
    switch(type, numeric = (function(cuts) {
      paste0(prefix, formatNum(cuts), suffix)
    })(...), bin = (function(cuts) {
      n <- length(cuts)
      paste0(prefix, formatNum(cuts[-n]), between, formatNum(cuts[-1]),
             suffix)
    })(...), quantile = (function(cuts, p) {
      n <- length(cuts)
      p <- paste0(round(p * 100), "%")
      cuts <- paste0(formatNum(cuts[-n]), between, formatNum(cuts[-1]))
      paste0("<span title=\"", cuts, "\">", prefix, p[-n],
             between, p[-1], suffix, "</span>")
    })(...), factor = (function(cuts) {
      paste0(prefix, as.character(transform(cuts)), suffix)
    })(...))
  }
}

load(file = "anuario_2024-06-07.RData") 




