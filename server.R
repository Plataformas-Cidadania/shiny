## Carrega pacotes -----------------------------------------

if (!require("shinythemes")) install.packages("shinythemes")
if (!require("lubridate")) install.packages("lubridate")
if (!require("scales")) install.packages("scales")
if (!require("stringr")) install.packages("stringr")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("janitor")) install.packages("janitor")
if (!require("markdown")) install.packages("markdown")
if (!require("leaflet")) install.packages("leaflet")
if (!require("DT")) install.packages("DT")
if (!require("plotly")) install.packages("plotly")
if (!require("devtools")) install.packages("devtools")
if (!require("rCharts")) install.packages("rCharts")
if (!require("RPostgreSQL")) install.packages("RPostgreSQL")
if (!require("data.tree")) install.packages("data.tree")
if (!require("d3treeR")) install.packages("d3treeR")
if (!require("treemap")) install.packages("treemap")
if (!require("shiny")) install.packages("shiny")
if (!require("feather")) install.packages("feather")
if (!require("fst")) install.packages("fst")
if (!require("shinyjs")) install.packages("shinyjs")
if (!require("shinycssloaders")) install.packages("shinycssloaders")
if (!require("shinyalert")) install.packages("shinyalert")
if (!require("highcharter")) install.packages("highcharter")
if (!require("V8")) install.packages("V8")
if (!require("pool")) install.packages("pool")

library(shinythemes, warn.conflicts = F)
library(lubridate, warn.conflicts = F)
library(scales, warn.conflicts = F)
library(stringr, warn.conflicts = F)
library(rlang)
library(tidyverse, warn.conflicts = F, quietly = T)
library(janitor, warn.conflicts = F)
library(markdown, warn.conflicts = F)
library(leaflet, warn.conflicts = F)
library(DT, warn.conflicts = F)
library(plotly, warn.conflicts = F)
library(devtools, warn.conflicts = F)
library(RPostgreSQL, warn.conflicts = F)
library(data.tree, warn.conflicts = F)
library(shiny, warn.conflicts = F)
library(shinycssloaders)
library(shinyalert)
library(shinyjs)
library(pool)
library(dbplyr)
library(highcharter)
library(V8)

## Carrega labels -----------------------------------------------------
load("label.RData")

dc_ods <- data.frame(ods = c(paste("Objetivo", 1:17)), cd_ods = 1:17)
dc_conferencia$tx_nome_conferencia <- iconv(dc_conferencia$tx_nome_conferencia, from = "UTF-8", to = "Windows-1252")
dc_conselho$tx_nome_conselho <- iconv(dc_conselho$tx_nome_conselho, from = "UTF-8", to = "Windows-1252")

## Conecta ao banco ----------------------------------------------------

source("conecta-banco.R")

## Server --------------------------------------------------------

shinyServer(function(input, output, session) { 
      
      vl_projeto_maximo <- as.integer(dbGetQuery(db, paste0("SELECT MAX(nr_valor_total_projeto) AS MAXVAL FROM graph.vw_shiny LIMIT 1"))$maxval)+1
      
      vl_projeto_minimo <- as.integer(dbGetQuery(db, paste0("SELECT MIN(nr_valor_total_projeto) AS MINVAL FROM graph.vw_shiny LIMIT 1"))$minval)
      ano_min_parceria <- as.numeric(dbGetQuery(db, paste0("SELECT MIN(ano_parceria::integer) AS MAXVAL FROM graph.vw_shiny LIMIT 1"))$maxval)
      ano_max_parceria <- as.numeric(dbGetQuery(db, paste0("SELECT MAX(ano_parceria::integer) AS MAXVAL FROM graph.vw_shiny LIMIT 1"))$maxval)
      ano_min_fundacao <- as.numeric(dbGetQuery(db, paste0("SELECT MIN(ano_fundacao::integer) AS MAXVAL FROM graph.vw_shiny LIMIT 1"))$maxval)
      ano_max_fundacao <- as.numeric(dbGetQuery(db, paste0("SELECT MAX(ano_fundacao::integer) AS MAXVAL FROM graph.vw_shiny LIMIT 1"))$maxval)
      
      output$fundacao_slider <- renderUI({
            sliderInput(inputId = "ano_fundacao", 
                        label   = "Ano de fundação", 
                        min     = ano_min_fundacao, 
                        max     = ano_max_fundacao, 
                        step    = 1,
                        value   = c(ano_min_fundacao, ano_max_fundacao),
                        ticks   = T,
                        sep     = "")
      })
      
      output$parceria_slider <- renderUI({
            sliderInput(inputId = "ano_parceria", 
                        label   = "Ano de Parceria", 
                        min     = ano_min_parceria, 
                        max     = ano_max_parceria, 
                        step    = 1,
                        value   = c(ano_min_parceria, ano_max_parceria),
                        ticks   = T,
                        sep     = "")
      })
      
      output$valor_projeto_slider <- renderUI({
            sliderInput(inputId = "valor_total", 
                        label   = "Valor total do projeto", 
                        min     = vl_projeto_minimo, 
                        max     = vl_projeto_maximo, 
                        step    = 1,
                        value   = c(vl_projeto_minimo, vl_projeto_maximo),
                        ticks   = T,
                        sep     = "")
      })
      
      output$subfinalidade <- renderUI({
            
            if(length(input$finalidade) > 0){
                  
                  cd_finalidade <- dc_area_atuacao %>% filter(tx_nome_area_atuacao %in% input$finalidade) %>% pull(cd_area_atuacao)
                  
                  dc_sub_area_atuacao <- dc_sub_area_atuacao %>% 
                        filter(cd_area_atuacao %in% cd_finalidade) %>% 
                        dplyr::select(tx_nome_subarea_atuacao) %>% 
                        filter(!duplicated(tx_nome_subarea_atuacao))
                  
            }
            
            selectizeInput(inputId = "subfinalidade",
                           "Sub-finalidade de atuação",
                           choices = unique(dc_sub_area_atuacao$tx_nome_subarea_atuacao),
                           multiple = TRUE,
                           width = "100%",
                           selected = NULL,
                           options = list(placeholder = "Digite ou clique na finalidade")
                           )
                          
            })
      
      
      output$uf <- renderUI({
            
            if(length(input$regiao) > 0){
                  
                  uf <- uf %>% 
                        filter(regiao %in% input$regiao) %>% 
                        dplyr::select(uf) %>% 
                        filter(!duplicated(uf))
                  
            }
            
            selectizeInput(inputId = "uf",
                                 "Unidade da Federação",
                                 choices = uf$uf,
                                 multiple = TRUE,
                                 width = "100%",
                                 selected = NULL,
                                 options = list(placeholder = "Digite ou clique na UF"))
                  })
      
      output$municipio <- renderUI({
            
            if(length(input$uf) > 0){
                  
                  dc_municipio <- dc_municipio %>% 
                        filter(estado %in% input$uf) %>% 
                        dplyr::select(tx_nome_municipio) %>% 
                        filter(!duplicated(tx_nome_municipio))
                  
                  
            }
            
           selectizeInput(inputId = "municipio",
                           "Município",
                           choices = sort(dc_municipio$tx_nome_municipio),
                           multiple = TRUE,
                           width = "100%",
                           selected = NULL,
                           options = list(placeholder = "Digite ou clique no município"))
      })
      
      output$porte <- renderUI({
            
            if(input$porte_all == TRUE) {
                  
                  selectizeInput(inputId = "porte",
                                 "Pessoal ocupado",
                                 choices = c(porte_c),
                                 multiple = TRUE,
                                 width = "100%",
                                 selected = porte_c,
                                 options = list(placeholder = "Digite ou clique no porte"))
                  
            }
            })
      
      # select input de finalidade
      output$finalidade <- renderUI({
            
            if(input$finalidade_all == TRUE) {
                  
                  selectizeInput(inputId = "finalidade",
                                 "Finalidade de atuação",
                                 choices = c("Não informado", unique(dc_area_atuacao$tx_nome_area_atuacao)),
                                 multiple = TRUE,
                                 width = "100%",
                                 selected = NULL,
                                 options = list(placeholder = "Digite ou clique na finalidade"))
                  
            } 
            })
      
      # select input de certificado
      output$ods <- renderUI({
            
            if(input$ods_all == TRUE) {
                  
                  selectizeInput(inputId = "ods",
                                 "ODS",
                                 choices = c(paste("Objetivo", 1:17)),
                                 multiple = TRUE,
                                 width = "100%",
                                 selected = c(paste("Objetivo", 1:17)),
                                 options = list(placeholder = "Digite ou clique no objetivo"))
                  
            } 
            
      })

      # "observe" referente ao reset button
      
      observeEvent(input$reset_all, {
            reset("form")
      })
      
      # Filtros da consulta
      
      where_fundacao <- reactive ({ 
            
            if(isTRUE(input$in_fundacao))  {" ano_fundacao::integer IS NULL"}
            
            else  {
                  
                  sprintf(" ano_fundacao::integer >= '%i' AND ano_fundacao::integer <= '%i'", 
                          input$ano_fundacao[1],
                          input$ano_fundacao[2])
                  
            }
            
      })
      
      where_parceria <- reactive ({ 
            
            if(isTRUE(input$in_parceria))  {sprintf(" AND ano_parceria::integer >= '%i' AND ano_parceria::integer <= '%i'", 
                                                    input$ano_parceria[1],
                                                    input$ano_parceria[2])}
            
            else  {sprintf(" AND ((ano_parceria IS NULL) OR (ano_parceria::integer >= '%i' AND ano_parceria::integer <= '%i'))",
                           input$ano_parceria[1],
                           input$ano_parceria[2])}
            
      })
      
      where_valor_projeto <- reactive ({ 
            
            if(isTRUE(input$in_parceria))  {sprintf(" AND nr_valor_total_projeto::integer >= %i AND nr_valor_total_projeto::integer <= %i", 
                                                    input$valor_total[1],
                                                    input$valor_total[2])}
            
            else  {
                  
                  sprintf(" AND ((nr_valor_total_projeto::integer IS NULL) OR (nr_valor_total_projeto::integer >= %i AND nr_valor_total_projeto::integer <= %i))", 
                          input$valor_total[1],
                          input$valor_total[2])
                  
            }
            
      })
      
      where_regiao <- reactive ({ 
            
            if(is.null(input$regiao))  {""}
            
            else  {
                  
                  regiao <- dc_regiao %>% dplyr::filter(regiao %in% input$regiao) %>% pull(cd_regiao)            
                  
                  paste0(" AND edre_cd_regiao::integer IN (", 
                         noquote(paste0("'", noquote(regiao), collapse=",", "'")), ")" )
                  
                  }
            
      })
      
      where_uf <- reactive ({ 
           
            if(is.null(input$uf))  {""}
            
            else  {
                  
                  uf <- dc_uf %>% filter(tx_nome_uf %in% input$uf) %>% pull(cd_uf)
                  
                  paste0(" AND eduf_cd_uf::integer IN (", 
                         noquote(paste0("'", noquote(uf), collapse=",", "'")), ")" )
                  }
            
      })
      
      where_municipio <- reactive ({ 
            
            if(is.null(input$municipio))  {""}
            
            else  {
                  
                  municipio <- dc_municipio %>% filter(tx_nome_municipio %in% input$municipio) %>% pull(cd_municipio)
                  
                  paste0(" AND edmu_cd_municipio::integer IN (", 
                         noquote(paste0("'", noquote(municipio), collapse=",", "'")), ")" )
            }
            
      })
      
      where_porte <- reactive ({ 
            
            if(is.null(input$porte))  {""}
            
            else  {
                  
                  porte <- dc_pessoal_ocupado %>% filter(pessoal_ocupado %in% input$porte) %>% pull(cd_pessoal_ocupado)
                  
                  paste0(" AND cd_pessoal_ocupado::integer IN (", 
                         noquote(paste0("'", noquote(porte), collapse=",", "'")), ")" )
            }
            
      })
      
      where_juridico <- reactive ({ 
            
            if(is.null(input$nat_jur))  {""}
            
            else  {
                  
                  nat_jur <- dc_juridico %>% filter(tx_nome_natureza_juridica_osc %in% input$nat_jur) %>% pull(cd_natureza_juridica_osc)
                  
                  if(is.na(nat_jur)) {
                  
                        paste0(" AND cd_natureza_juridica_osc::integer IS NULL" )      
                        
                  }
                  
                  else{
                  
                  paste0(" AND cd_natureza_juridica_osc::integer IN (", 
                         noquote(paste0("'", noquote(nat_jur), collapse=",", "'")), ")" )
                        
                  }
            }
            
      })
      
      where_finalidade <- reactive ({ 
            
            if(is.null(input$finalidade))  {""}
            
            else  {
                  
                  if(input$finalidade == "Não informado") {
                        
                        paste0(" AND cd_area_atuacao::integer IS NULL" )      
                        
                  }
                  
                  else{
                  
                  area_atuacao <- dc_area_atuacao %>% filter(tx_nome_area_atuacao %in% input$finalidade) %>% pull(cd_area_atuacao)
                  
                  paste0(" AND cd_area_atuacao::integer IN (", 
                         noquote(paste0("'", noquote(area_atuacao), collapse=",", "'")), ")" )
                  }
            }
            
      })
      
      where_subfinalidade <- reactive ({ 
            
            if(is.null(input$subfinalidade))  {""}
            
            else  {
                  
                  subarea_atuacao <- dc_sub_area_atuacao %>% 
                        filter(tx_nome_subarea_atuacao %in% input$subfinalidade) %>% 
                        pull(cd_subarea_atuacao)
                  
                  paste0(" AND cd_subarea_atuacao::integer IN (", 
                         noquote(paste0("'", noquote(subarea_atuacao), collapse=",", "'")), ")" )
            }
            
      })
      
      where_ods <- reactive ({ 
            
            if(is.null(input$ods))  {""}
            
            else  {
                  
                  ods <- dc_ods %>% filter(ods %in% input$ods) %>% pull(cd_ods)
                  
                  paste0(" AND id_objetivo_osc::integer IN (", 
                         noquote(paste0("'", noquote(ods), collapse=",", "'")), ")" )
            }
            
      })
      
      where_ods_parceria <- reactive ({

            if(is.null(input$ods_parceria))  {""}

            else  {

                  ods <- dc_ods %>% filter(ods %in% input$ods_parceria) %>% pull(cd_ods)

                  paste0(" AND tx_codigo_objetivo_projeto::integer IN (",
                         noquote(paste0("'", noquote(ods), collapse=",", "'")), ")" )
            }

      })
      
      where_conselho <- reactive({

            if(isTRUE(input$in_conselho)) { return(" AND cd_conselho::integer > 0") }

            else  {""}

            if(!is.null(input$nome_conselho)) {
            
                  conselho <- dc_conselho %>%
                        filter(tx_nome_conselho %in% c(input$nome_conselho)) %>%
                        pull(cd_conselho)

                  paste0(" AND cd_conselho::integer IN (",
                         noquote(paste0("'", noquote(conselho), collapse=",", "'")), ")" )
            }

      })
      
      where_conferencia <- reactive({
            
            if(isTRUE(input$in_conferencia)) { return(" AND cd_conferencia::integer > 0") }
            
            else{""}
                  
            if(!is.null(input$nome_conferencia)) {
           
                  conferencia <- dc_conferencia %>% 
                        dplyr::filter(tx_nome_conferencia %in% c(input$nome_conferencia)) %>% 
                        pull(cd_conferencia)

                  paste0(" AND cd_conferencia::integer IN (",
                         noquote(paste0("'", noquote(conferencia), collapse=",", "'")), ")" )
            }

      })
      
   where_fonte_recursos <- reactive ({ 
            
            if(is.null(input$fonte_recursos))  {""}
            
            else  {
                  
                  fonte_recursos <- dc_fonte_recursos_projeto %>% filter(tx_nome_origem_fonte_recursos_projeto %in% input$fonte_recursos) %>% pull(cd_origem_fonte_recursos_projeto)
                  
                  paste0(" AND cd_fonte_recursos_osc::integer IN (", 
                         noquote(paste0("'", noquote(fonte_recursos), collapse=",", "'")), ")" )
            }
            
      })
      
      where_certificado <- reactive ({ 
            
            if(is.null(input$certificado))  {""}
            
            else  {
                  
                  certificado <- dc_certificado %>% filter(tx_nome_certificado %in% input$certificado) %>% pull(cd_certificado)
                  
                  if(certificado == 9) {
                        
                        paste0(" AND cd_certificado::integer IS NULL" )      
                        
                  }
                  
                  else{
                  
                  paste0(" AND cd_certificado::integer IN (", 
                         noquote(paste0("'", noquote(certificado), collapse=",", "'")), ")" )
                  }
            }
            
      })
      
      query <- eventReactive(input$ver_dados, {
            
            return(paste0("SELECT * FROM graph.vw_shiny WHERE",
                          where_fundacao(),
                          where_parceria(),
                          where_regiao(),
                         where_uf(), 
                          where_municipio(),
                          where_porte(),
                          where_juridico(),
                          where_finalidade(),
                          where_subfinalidade(),
                          where_ods(),
                          where_certificado(),
                          where_ods_parceria(),
                          where_fonte_recursos(),
                          where_valor_projeto(),
                          where_conselho(),
                          where_conferencia(),
                          " LIMIT 50"))
      })
      
      output$query <- renderPrint({
            query()
      })
      
      # disable tab2 on page load
      js$disableTab("graficos")
      js$disableTab("dados")
      
      observeEvent(input$ver_dados, {
            
            withBusyIndicatorServer("ver_dados", {
            
            if (nrow(extracao()) > 0) {
            
                  # enable tab2 when clicking the button
                  js$enableTab("graficos")
                  
                  updateTabItems(session, "filtro",
                                    selected = "graficos")
            }
                  
            })
                  
            })
      
      observeEvent(input$ver_dados, {
            
            if (nrow(extracao()) > 0) {
                  
                  # enable dados when clicking the button
                  js$enableTab("dados")
                  
                  
            }
            
            
      })
      
      extracao <- eventReactive(input$ver_dados, {
            
            dados <- dbGetQuery(db, paste0("SELECT * FROM graph.vw_shiny WHERE",
                                           where_fundacao(),
                                           where_parceria(),
                                           where_regiao(),
                                           where_uf(), 
                                           where_municipio(),
                                           where_porte(),
                                           where_juridico(),
                                           where_finalidade(),
                                           where_subfinalidade(),
                                           where_ods(),
                                           where_certificado(),
                                           where_ods_parceria(),
                                           where_fonte_recursos(),
                                           where_valor_projeto(),
                                           where_conselho(),
                                           where_conferencia()))
            
            dados <- mutate_if(dados, is.character, funs(iconv(., from = "utf-8", to = "latin1")))
            
            return(dados)
            }
            )
      
      observeEvent(input$ver_dados, {
      
            if (nrow(extracao())==0) {
            
            shinyalert(
                  title = "Consulta não encontrada.",
                  text = "Não dá dados disponíveis para os filtros selecionados.",
                  closeOnEsc = TRUE,
                  closeOnClickOutside = FALSE,
                  html = FALSE,
                  type = "warning",
                  showConfirmButton = TRUE,
                  showCancelButton = FALSE,
                  confirmButtonText = "OK",
                  confirmButtonCol = "#66C2ED",
                  timer = 0,
                  imageUrl = "",
                  animation = TRUE
            )
            
            }
            })
      
      output$tab_extracao <- DT::renderDataTable({
            
            validate(
                  need(input$ver_dados, "Por favor, clique em 'Ver dados' na página inicial para realizar a consulta.")
            )
            
            validate(
                  need(nrow(extracao()) > 0, "Não dá dados disponíveis para os filtros selecionados."))
            
            extracao() %>% 
                  dplyr::select(., -tx_descricao_projeto)
            
                  },
      rownames = F,
      filter = 'top',
      server = TRUE,
      options = list(
            pageLength = 5,
            ordering = FALSE,
            scrollX = TRUE
            )
      )
      
      output$evolucao_fundacao <- renderHighchart({
            
           tab1 <- extracao() %>%
                 dplyr::filter(!duplicated(id_osc)) %>% 
                  group_by(ano_fundacao) %>% 
                  dplyr::summarize(n = n())
            
            return(
                  hchart(tab1, 
                         type = "line", hcaes(y = n, x = ano_fundacao)) %>% 
                        hc_yAxis(title = list(text = "")) %>%
                        hc_xAxis(title = list(text = ""), 
                                 categories = tab1$ano_fundacao) %>% 
                        hc_plotOptions(
                              series = list(
                                    marker = list(enabled = TRUE),
                                    name = "Total de OSCs")) %>%
                        hc_credits(enabled = TRUE, 
                                   text = "Fonte: Elaboração do Ipea a partir da SRF (2016) e representantes de OSCs.",
                                   position = list(y = 0))
            )
            
            })
      
      output$evolucao_recursos <- renderHighchart({
            
            tab2 <- extracao() %>%
                  dplyr::filter(!duplicated(id_osc)) %>% 
                  group_by(ano_parceria) %>% 
                  dplyr::summarize(vl = sum(nr_valor_total_projeto, na.rm = T)) %>% 
                  filter(!is.na(ano_parceria)) %>% 
                  data.frame
            
            return(
                  hchart(filter(tab2, !is.na(ano_parceria)) %>% data.frame, 
                         type = "line", hcaes(y = vl, x = ano_parceria)) %>% 
                        hc_yAxis(title = list(text = "")) %>%
                        hc_xAxis(title = list(text = ""), categories = tab2$ano_parceria) %>% 
                        hc_plotOptions(
                              series = list(
                                    marker = list(enabled = TRUE),
                                    name = "Valor total em R$")) %>%
                        hc_tooltip(headerFormat = "",
                                   useHTML = T,
                                   formatter = JS("function () { return this.x + ':' + '<b>' + 'R$' + Highcharts.numberFormat(this.point.y, -1) + '</b>';}")
                                   ) %>%
                        hc_credits(enabled = TRUE, 
                                   text = "Fonte: Elaboração do Ipea a partir da SRF (2016), Senado Federal (2018) e representantes de OSCs.",
                                   position = list(y = 0))
            )
            
      })
      
      output$total_uf <- renderHighchart({
            
            tab3 <- extracao() %>%
                  dplyr::filter(!duplicated(id_osc)) %>% 
                  tabyl(eduf_sg_uf) %>% 
                  dplyr::rename(uf = eduf_sg_uf) %>% 
                  dplyr::mutate(percent = round(percent*100, 0)) %>% 
                  filter(!is.na(uf)) %>% 
                  data.frame
                  
            return(
                  
                  hchart(tab3, 
                         type = "column", hcaes(y = percent, x = uf, color = uf)) %>% 
                        hc_yAxis(title = list(text = "")) %>%
                        hc_xAxis(type = "category", title = list(text = ""), categories = dplyr::select(tab3, uf) %>% pull %>% as.list) %>% 
                        hc_plotOptions(
                              series = list(name = "% de OSCs")) %>%
                        hc_tooltip(headerFormat = "",
                                   useHTML = T,
                                   formatter = JS("function () { return this.x + ':' + '<b>' + Highcharts.numberFormat(this.point.y, -1) + '%' + '</b>';}")
                        ) %>%
                        hc_credits(enabled = TRUE, 
                                   text = "Fonte: Elaboração do Ipea a partir da SRF (2016) e representantes de OSCs.",
                                   position = list(y = 0))
                  )
            })
      
      output$total_finalidade <- renderHighchart({
            
            tab4 <- extracao() %>%
                  dplyr::filter(!duplicated(id_osc)) %>% 
                  tabyl(tx_nome_area_atuacao) %>% 
                  dplyr::rename(finalidade = tx_nome_area_atuacao) %>% 
                  dplyr::mutate(percent = round(percent*100, 0)) %>% 
                  filter(!is.na(finalidade)) %>% 
                  data.frame
            
            return(
                  
                  hchart(tab4, 
                         type = "bar", hcaes(y = percent, x = finalidade, color = finalidade)) %>% 
                        hc_yAxis(title = list(text = "")) %>%
                        hc_xAxis(title = list(text = ""), 
                                 categories = tab4$finalidade) %>% 
                        hc_plotOptions(
                              series = list(name = "% de OSCs")) %>%
                        hc_tooltip(headerFormat = "",
                                   useHTML = T,
                                   formatter = JS("function () { return this.x + ':' + '<b>' + Highcharts.numberFormat(this.point.y, -1) + '%' + '</b>';}")
                        ) %>%
                        hc_credits(enabled = TRUE, 
                                   text = "Fonte: Elaboração do Ipea a partir da SRF (2016) e representantes de OSCs.",
                                   position = list(y = 0))
            )
            })
      
      output$download_data <- downloadHandler(
            filename = function() {
                  paste0('tb-osc-', Sys.Date(), ".csv")
            },
            content = function(file) {
                  write.csv(extracao(), file, row.names = FALSE, sep = ";")
            }
      )
      
      hide(id = "loading-content", anim = TRUE, animType = "fade")    
      
})