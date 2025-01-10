rm(df,x)
if (input$demo2ri == "Pará") {
  df <- demo2 %>% filter(localidade != "Pará", ano == "2022") %>%
    pivot_wider(names_from = "categoria", values_from = "valor")
  
  x <- cbind(geopa, df)
}else{
  df <- demo2 %>% filter(localidade != "Pará", ano == "2022") %>%
    pivot_wider(names_from = "categoria", values_from = "valor")
  
  x <- cbind(geopa, df)
  x <- x %>% filter(ri == input$demo2ri)
}

# Criar uma coluna que define a cor com base na faixa etária predominante

x <- x %>% 
  mutate(
    valor = pmax(X0.a.4.anos, X5.a.9.anos, X10.a.14.anos, X15.a.19.anos, X20.a.29.anos,  
                 X30.a.39.anos, X40.a.49.anos, X50.a.59.anos, X60.a.69.anos, X70.a.79.anos, X80.anos.e.mais, na.rm = TRUE),
    faixa_predominante = case_when(
      valor == 0 ~ "Nenhum",
      `0 a 4 anos` == valor ~ "0 a 4 anos",
      `5 a 9 anos` == valor ~ "5 a 9 anos",
      `10 a 14 anos` == valor ~ "10 a 14 anos",
      `15 a 19 anos` == valor ~ "15 a 19 anos",
      `20 a 29 anos` == valor ~ "20 a 29 anos",
      `30 a 39 anos` == valor ~ "30 a 39 anos",
      `40 a 49 anos` == valor ~ "40 a 49 anos",
      `50 a 59 anos` == valor ~ "50 a 59 anos",
      `60 a 69 anos` == valor ~ "60 a 69 anos",
      `70 a 79 anos` == valor ~ "70 a 79 anos",
      `80 anos e mais` == valor ~ "80 anos e mais",
      TRUE ~ "Indefinido"
    ),
    cor = case_when(
      faixa_predominante == "Nenhum" ~ "gray",
      faixa_predominante == "0 a 4 anos" ~ "#fdae61",
      faixa_predominante == "5 a 9 anos" ~ "#fee08b",
      faixa_predominante == "10 a 14 anos" ~ "#d73027",
      faixa_predominante == "15 a 19 anos" ~ "#91bfdb",
      faixa_predominante == "20 a 29 anos" ~ "#4575b4",
      faixa_predominante == "30 a 39 anos" ~ "#313695",
      faixa_predominante == "40 a 49 anos" ~ "#542788",
      faixa_predominante == "50 a 59 anos" ~ "#542788",
      faixa_predominante == "60 a 69 anos" ~ "#542788",
      faixa_predominante == "70 a 79 anos" ~ "#542788",
      faixa_predominante == "80 anos e mais" ~ "#542788",
      TRUE ~ "gray"
    )
  )

conteudo <- sprintf(
  "<strong>Município de %s</strong><br/>
       <b>0 a 4 anos:</b> %s<br/>
       <b>5 a 9 anos:</b> %s<br/>
       <b>10 a 14 anos:</b> %s<br/>
       <b>15 a 19 anos:</b> %s<br/>
       <b>20 a 29 anos:</b> %s<br/>
       <b>30 a 39 anos:</b> %s<br/>
       <b>40 a 49 anos:</b> %s<br/>
       <b>50 a 59 anos:</b> %s<br/>
       <b>60 a 69 anos:</b> %s<br/>
       <b>70 a 79 anos:</b> %s<br/>
       <b>80 anos e mais:</b> %s<br/>
       <b>Predominância:</b> %s",
  dados$name_muni,
  format(dados$`0 a 4 anos`, big.mark = ".", decimal.mark = ","),
  format(dados$`5 a 9 anos`, big.mark = ".", decimal.mark = ","),
  format(dados$`10 a 14 anos`, big.mark = ".", decimal.mark = ","),
  format(dados$`15 a 19 anos`, big.mark = ".", decimal.mark = ","),
  format(dados$`20 a 29 anos`, big.mark = ".", decimal.mark = ","),
  format(dados$`30 a 39 anos`, big.mark = ".", decimal.mark = ","),
  format(dados$`40 a 49 anos`, big.mark = ".", decimal.mark = ","),
  format(dados$`50 a 59 anos`, big.mark = ".", decimal.mark = ","),
  format(dados$`60 a 69 anos`, big.mark = ".", decimal.mark = ","),
  format(dados$`70 a 79 anos`, big.mark = ".", decimal.mark = ","),
  format(dados$`80 anos e mais`, big.mark = ".", decimal.mark = ","),
  dados$faixa_predominante
) %>% lapply(htmltools::HTML)

leaflet(dados, options = leafletOptions(minZoom = 0, maxZoom = 15, zoomControl = FALSE)) %>%
  addTiles() %>%
  htmlwidgets::onRender(
    "function(el, x) {
            L.control.zoom({ position: 'topright' }).addTo(this);
            }"
  ) %>%
  addPolygons(
    weight = 2,
    opacity = 1,
    color = "black",
    fillColor = ~cor,
    fillOpacity = 1,
    dashArray = 1,
    smoothFactor = 1.5,
    highlightOptions = highlightOptions(
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
    colors = c("#fdae61", "#fee08b", "#d73027", "#91bfdb", "#4575b4", "#313695", "#542788", "gray"),
    labels = c("0 a 4 anos", "5 a 9 anos", "10 a 14 anos", "15 a 19 anos", "20 a 29 anos", "30 a 39 anos", "40 a 49 anos", "50 anos mais", "80 anos mais"),
    opacity = 0.7,
    title = "Predominância por Faixa Etária",
    position = "bottomright"
  )