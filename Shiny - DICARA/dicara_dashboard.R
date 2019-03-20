library(shiny)
library(shinydashboard)
library(ggplot2)
library(magrittr)
library(RCurl)
library(dplyr)
library(ggsci)
library(tidyr)

header <- dashboardHeader(title = "DICARA")



sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem('Ver base', tabName = 'bases', icon = icon('database')),
    menuItem('Descrição Geral', tabName = 'sumgeneral', icon = icon('table'),
             menuSubItem('Formulário Familiar', tabName = 'itembase1'),
             menuSubItem('Grupo Familiar', tabName = 'itembase2'))
  ),
  
  selectInput(inputId = 'municipio',
              label = 'Município',
              c('Carauari' = 'Carauari',
                "Eirunepé" = "Eirunepé",
                'Iranduba' = 'Iranbuda',
                "Itapiranga" = "Itapiranga",
                'Maraã' = 'Maraã',
                'Novo Aripuanã' = 'Novo Aripuanã',
                "Tefé" = "Tefé",
                'Todos' = 'Todos'
                ), multiple = TRUE)
)

body <- dashboardBody(
    tabItems(
      tabItem('bases',
              fluidPage(tabsetPanel(
                  id = 'dataset',
                  tabPanel('Formulário de Aplicação Familiar', DT::dataTableOutput('base1')),
                  tabPanel('Grupo Familiar', DT::dataTableOutput('base2'))
                  )
              )
      ),

      # aplicação familiar
      tabItem('itembase1',
              # primeira linha
              fluidRow( 
                valueBoxOutput(outputId = 'entBox')
              ),
              # segunda linha
              fluidRow(
                box(title = 'Municípios', plotOutput('plot_municipio'), solidHeader = TRUE, collapsible = TRUE, status = 'primary'),
                box(title = 'UC', plotOutput('plot_uc'), solidHeader = TRUE, collapsible = TRUE, status = 'primary')
              ),
              # terceira linha
              fluidRow(
                box(title = 'Local de Aplicação', plotOutput('plot_locap'), solidHeader = TRUE, collapsible = TRUE, status = 'primary'),
                box(title = 'Regional', plotOutput('plot_regional'), solidHeader = TRUE, collapsible = TRUE, status = 'primary')
              ),
              # quarta linha
              fluidRow(
                box(title = 'Nº de moradores na casa', plotOutput('plot_nmoradores'), solidHeader = TRUE, collapsible = TRUE, status = 'primary', width = 12)
              )
      ),
      
      
      # grupo familiar
      tabItem('itembase2',
              fluidRow( box(plotOutput('plot2')) )
              )
    )

)


ui <- dashboardPage(header, 
                    sidebar, 
                    body, 
                    skin = 'green')


server <- function(input, output) {
  base1 <- read.table(text = getURL('https://raw.githubusercontent.com/caiolif/datasets/master/dados_aplicacaofamiliar.txt', .encoding = 'ISO-8859-1'),
                      header = TRUE, blank.lines.skip = FALSE, sep = "\t")
  
  base2 <- read.table(text = getURL('https://raw.githubusercontent.com/caiolif/datasets/master/dados_grupofamiliar.txt', .encoding = 'Latin-1'),
                      header = TRUE, blank.lines.skip = FALSE, sep = "\t", quote = "")
  
  output$base1 <- DT::renderDataTable(base1, options = list(scrollX = TRUE))
  output$base2 <- DT::renderDataTable(base2, options = list(scrollX = TRUE))
  
  
  output$entBox <- renderInfoBox(
    valueBox(nrow(base1), 'Entrevistados', icon = icon('users'))
  )
  
  output$plot_municipio <- renderPlot(
    base1 %>% 
      as_data_frame() %>% 
      select('Município') %>% 
      group_by(Município) %>% 
      count() %>%
      ungroup() %>% 
      mutate(prop = round(n*100/sum(n), 0), y.breaks = cumsum(prop) - prop/2) %>% 
      ggplot(aes(x = Município, y = prop)) +
      geom_bar(stat = 'identity', fill = 'orange') + 
      geom_text(aes(label = paste0(n)),
                position = position_stack(vjust = 0.5)) +
      labs(x = '', y = 'Nº de entrevistados (%)') +
      theme_minimal(base_size = 14) 
  )
  
  output$plot_regional <- renderPlot(
    base1 %>% 
      as_data_frame() %>% 
      select('Município', 'Regional') %>%
      group_by(Município) %>% 
      count(Regional) %>%
      ungroup() %>% 
      mutate(prop = n*100/sum(n)) %>% 
      ggplot(aes(x = Município, y = prop, fill = Regional, group = Regional)) +
      geom_bar(stat = 'identity') +
      scale_fill_d3(name = 'Região:') + 
      geom_text(aes(label = paste0(n)),
                position = position_stack(vjust = 0.5), size = 4.5) +
      labs(x = '', y = 'Região (%)') + 
      theme_minimal(base_size = 14) + 
      theme(legend.position = 'top')
  )
  
  output$plot_uc <- renderPlot(
    base1 %>% 
      as_data_frame() %>% 
      select('UC') %>% 
      group_by(UC) %>% 
      count() %>%
      ungroup() %>%
      mutate(prop = round(n*100/sum(n), 0), y.breaks = cumsum(prop) - prop/2) %>% 
      ggplot(aes(x = UC, y = prop)) +
      geom_bar(fill = 'orange', stat = 'identity') + 
      geom_text(aes(label = paste0(n)),
                position = position_stack(vjust = 0.5), size = 4.5) +
      labs(x = "", y = "UC (%)") + 
      theme_minimal(base_size = 14) +
      coord_flip()
  )
  
  output$plot_locap <- renderPlot(
    base1 %>% as_data_frame() %>% 
      select('Município', 'Local.de.aplicação') %>% 
      group_by(Município) %>% 
      count(Local.de.aplicação) %>%
      mutate(prop = n*100/sum(n)) %>% 
      ungroup() %>% 
      ggplot(aes(x = Município, y = prop, fill = Local.de.aplicação, group = Local.de.aplicação)) +
      geom_bar(stat = 'identity') + 
      geom_text(aes(label = paste0( n)),
                position = position_stack(vjust = 0.5), size = 4.5) +
      labs(y = 'Local de Aplicação (%)') +
      scale_fill_d3(name = 'Local de Aplicação:') +
      theme_minimal(base_size = 14) + theme(legend.position = 'top')
  )
  
  output$plot_nmoradores <- renderPlot(
    base1 %>% as_data_frame() %>% 
      select('Município', 'No..de.moradores.na.casa..mesma.família.') %>% 
      filter(No..de.moradores.na.casa..mesma.família. != 196) %>% 
      group_by(Município) %>% 
      count(No..de.moradores.na.casa..mesma.família.) %>% 
      ggplot(aes(x = No..de.moradores.na.casa..mesma.família., y = n, color = Município)) +
      geom_line(size = 1.1) + geom_point(size = 2) + 
      scale_color_d3(name = 'Município:') +
      scale_x_discrete(limits = seq(0, 30, by = 3)) +
      labs(y = 'n', x = 'Nº de moradores na casa') +
      theme_minimal(base_size = 14) + 
      theme(legend.position = 'top')
  )
  
  
}


shinyApp(ui, server)
