library(shinyjs)
library(shinycssloaders)
library(shinydashboard)
library(tidyverse, warn.conflicts = F, quietly = T)
library(shinyalert)
library(highcharter)
library(V8)

source(file.path("helpers", "helpers.R"))

jscode <- "
shinyjs.disableTab = function(name) {
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.bind('click.tab', function(e) {
    e.preventDefault();
    return false;
  });
  tab.addClass('disabled');
}

shinyjs.enableTab = function(name) {
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.unbind('click.tab');
  tab.removeClass('disabled');
}
"

load("label.RData")
dc_conferencia$tx_nome_conferencia <- iconv(dc_conferencia$tx_nome_conferencia, from = "latin1", to = "Windows-1252")
dc_conselho$tx_nome_conselho <- iconv(dc_conselho$tx_nome_conselho, from = "latin1", to = "Windows-1252")
juridico_c <- iconv(juridico_c, from = 'latin1', to='UTF-8')
dc_sub_area_atuacao$tx_nome_subarea_atuacao <- iconv(dc_sub_area_atuacao$tx_nome_subarea_atuacao, from = 'latin1', to='UTF-8')

shinyUI(
      dashboardPage(
            dashboardHeader(disable = T),
            dashboardSidebar(disable = T),
            dashboardBody(
                  includeCSS("./www/styles.css"),
                  useShinyjs(),
                  div(id = "loading-content", "Por favor, aguarde...", img(src = "ajax-loader-bar.gif")),
                  useShinyalert(),
                  extendShinyjs(text = jscode),
                  fluidRow(
                  tabBox(id = "filtro",
                         width = 12,
                         tabPanel(tagList(shiny::icon("filter"), "Filtros"), value = "filtros",
                                  div(id = "form",
                                      tagList(
                                            fluidRow(class = "myRow1", 
                                                     column(3, 
                                                            br(), h4("1. Dados gerais"), br(), 
                                                            uiOutput("fundacao_slider"),
                                                            checkboxInput("in_fundacao",
                                                                          label = "Selecionar OSCs com ano de fundação não informado.", FALSE 
                                                            ),
                                                            selectizeInput(inputId = "porte",
                                                                           "Pessoal ocupado",
                                                                           choices = c(porte_c),
                                                                           multiple = TRUE,
                                                                           width = "100%",
                                                                           selected = NULL,
                                                                           options = list(placeholder = "Digite ou clique no porte")),
                                                            tags$div(class = "ods",
                                                                     selectizeInput(inputId = "ods",
                                                                                    "ODS da organização",
                                                                                    choices = c(paste("Objetivo", 1:17)),
                                                                                    multiple = TRUE,
                                                                                    width = "100%",
                                                                                    selected = NULL,
                                                                                    options = list(placeholder = "Digite ou clique no objetivo"))
                                                            )
                                                            ),
                                                     column(3, 
                                                            br(), 
                                                            br(), 
                                                            br(), 
                                                            br(), 
                                                            selectizeInput(inputId = "nat_jur",
                                                                           "Natureza jurídica",
                                                                           choices = c(juridico_c),
                                                                           multiple = TRUE,
                                                                           width = "100%",
                                                                           selected = NULL,
                                                                           options = list(placeholder = "Digite ou clique na nat. jurídica")),
                                                            br(), 
                                                            tags$div(class = "finalidade",
                                                            selectizeInput(inputId = "finalidade",
                                                                           "Finalidade de atuação",
                                                                           choices = c("Não informado", unique(dc_area_atuacao$tx_nome_area_atuacao)),
                                                                           multiple = TRUE,
                                                                           width = "100%",
                                                                           selected = NULL,
                                                                           options = list(placeholder = "Digite ou clique na finalidade"))
                                                            
                                                            )),
                                                     column(3,
                                                            br(), 
                                                            br(), 
                                                            br(), 
                                                            br(), 
                                                            uiOutput("certificado"),
                                                            selectizeInput(inputId = "certificado",
                                                                           "Certificado",
                                                                           choices = c(unique(dc_certificado$tx_nome_certificado)),
                                                                           multiple = TRUE,
                                                                           width = "100%",
                                                                           selected = NULL,
                                                                           options = list(placeholder = "Digite ou clique no certificado")),
                                                            tags$div(class = "subfinalidade",
                                                                     uiOutput("subfinalidade")
                                                            )
                                                            )
                                                     ),
                                            fluidRow(class = "myRow2", 
                                                     column(3, 
                                                            br(), h4("2. Localização"), br(), 
                                                            selectizeInput(inputId = "regiao",
                                                                           "Região",
                                                                           choices = c(pull(distinct(dc_regiao, as.character(regiao)))),
                                                                           multiple = TRUE,
                                                                           width = "100%",
                                                                           selected = NULL,
                                                                           options = list(placeholder = "Digite ou clique na região"))
                                                            ),
                                                     br(), 
                                                     br(), 
                                                     br(), 
                                                     br(), 
                                                     column(3, 
                                                            uiOutput("uf")
                                                       ),
                                                     column(3, 
                                                            uiOutput("municipio")
                                                            )
                                                     ),
                                            fluidRow(class = "myRow3", 
                                                     column(3, 
                                                            br(), h4("3. Parcerias"),  br(),
                                                            uiOutput("parceria_slider"),
                                                            checkboxInput("in_parceria",
                                                                          label = "Selecionar apenas OSCs com parcerias.", FALSE 
                                                            ),
                                                            selectizeInput(inputId = "ods_parceria",
                                                                           "ODS do projeto",
                                                                           choices = c(paste("Objetivo", 1:17)),
                                                                           multiple = TRUE,
                                                                           width = "100%",
                                                                           selected = NULL,
                                                                           options = list(placeholder = "Digite ou clique no objetivo"))
                                                            ),
                                                     br(),
                                                     br(),
                                                     br(),
                                                     br(),
                                                     column(3, 
                                                            uiOutput("valor_projeto_slider")
                                                            ),
                                                     column(3, 
                                                            selectizeInput(inputId = "fonte_recursos",
                                                                           "Fonte de Recursos",
                                                                           choices = unique(dc_fonte_recursos_projeto$tx_nome_origem_fonte_recursos_projeto),
                                                                           multiple = TRUE,
                                                                           width = "100%",
                                                                           selected = NULL,
                                                                           options = list(placeholder = "Digite ou clique na fonte"))
                                                            )
                                                     ),
                                      fluidRow(class = "myRow4", 
                                               column(6, 
                                                      br(), h4("4. Participação"), br(),
                                                      
                                                      selectizeInput(inputId = "nome_conselho",
                                                                     "Nome do conselho",
                                                                     choices = sort(unique(dc_conselho$tx_nome_conselho)),
                                                                     multiple = TRUE,
                                                                     width = "100%",
                                                                     selected = NULL,
                                                                     options = list(placeholder = "Digite ou no conselho")),
                                                      
                                                      checkboxInput("in_conselho", 
                                                                    label = "Selecionar apenas OSCs em conselhos.", FALSE
                                                      ),
                                                      
                                                      selectizeInput(inputId = "nome_conferencia",
                                                                     "Nome da conferência",
                                                                     choices = sort(unique(dc_conferencia$tx_nome_conferencia)),
                                                                     multiple = TRUE,
                                                                     width = "100%",
                                                                     selected = NULL,
                                                                     options = list(placeholder = "Digite ou clique na conferencia")),
                                                      checkboxInput("in_conferencia", 
                                                                    label = "Selecionar apenas OSCs em conferências.", FALSE
                                                      )
                                                      
                                                      )
                                               ),
                                      fluidRow(class = "myRow5", 
                                               column(3, 
                                                      br(),
                                                      withBusyIndicator(
                                                            actionButton(
                                                                  "ver_dados", 
                                                                  tagList(shiny::icon("external-link"), "Ver dados"), 
                                                                  width = "80%",
                                                                  class = "btn-primary btn-lg"
                                                                  )
                                                      )
                                               ),
                                               column(3, 
                                                      tags$div(class = "download",
                                                               withBusyIndicator(
                                                               downloadButton('download_data', 'Download', width = "80%",
                                                                              class = "btn-primary btn-lg")
                                                               )
                                                      )
                                               ),
                                               column(3, 
                                                      tags$div(class = "reset",
                                                               withBusyIndicator(
                                                      actionButton(
                                                            inputId = "reset_all", 
                                                            label = "Resetar campos", 
                                                            icon = icon("refresh"), width = "80%",
                                                            class = "btn-primary btn-lg")
                                                      )
                                                      )
                                               )
                                      )
                                      ),
                                      tags$style(type = 'text/css', "#data_dl0 { width:80%}"),
                                      br(),
                                      br()
                                  )
                  ),
                  
                  tabPanel(tagList(shiny::icon("line-chart"), "Gráficos"), 
                           value = "graficos",
                           width = "1000px",
                           tagList(
                                 fluidRow(class = "myRow6", 
                                 tags$div(class = "evolucao_funda",
                                 column(width = 6,
                                        box(
                                              title = "Evolução do total de OSCs por ano de fundação, Brasil - 2018",
                                              status = "primary",
                                              width = 12,
                                              solidHeader = FALSE,
                                              collapsible = TRUE,
                                              highchartOutput("evolucao_fundacao", height = "350px")
                                        )
                                        )
                                 ),
                           tagList(
                                 tags$div(class = "evolucao_funda",
                                          column(width = 6,
                                                 box(
                                                       title = "Evolução do valor dos projetos de OSCs, Brasil - 2018",
                                                       status = "primary",
                                                       width = 12,
                                                       solidHeader = FALSE,
                                                       collapsible = TRUE,
                                                       highchartOutput("evolucao_recursos", height = "350px")
                                                 )
                                          )
                                 )
                                 )
                           )),
                           tagList(
                                 fluidRow(class = "myRow7", 
                                          tags$div(class = "total_uf",
                                                   column(width = 6,
                                                          box(
                                                                title = "% de OSCs por Unidade da Federação, Brasil - 2016",
                                                                status = "primary",
                                                                width = 12,
                                                                solidHeader = FALSE,
                                                                collapsible = TRUE,
                                                                highchartOutput("total_uf", height = "350px")
                                                          )
                                                   )
                                                ),
                                          tagList(
                                                tags$div(class = "total_finalidade",
                                                         column(width = 6,
                                                                box(
                                                                      title = "% de OSCs por finalidade de atuação, Brasil - 2016",
                                                                      status = "primary",
                                                                      width = 12,
                                                                      solidHeader = FALSE,
                                                                      collapsible = TRUE,
                                                                      highchartOutput("total_finalidade", height = "350px")
                                                                )
                                                         )
                                                )
                                                )
                                 ))
                           
                           
                           
                  ),
                  
                  tabPanel(tagList(shiny::icon("table"), "Dados"), value = "dados",
                           tagList(
                                 column(width = 12, br(), textOutput("nao_encontrado")),
                                 column(width = 12, br(), withSpinner(DT::dataTableOutput(outputId = "tab_extracao")))
                                 )
                           )
                  
            )
            )
      )
      )
)
