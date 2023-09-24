# Bibliotecas Shiny
library(shinydashboard)
library(leaflet)
library(shiny)
library(dplyr)
library(here)
library(fresh)
library(shinythemes)
library(magrittr)
library(rvest)
library(readxl)
library(maps)
library(ggplot2)
library(reshape2)
library(ggiraph)
library(RColorBrewer)
library(plotly)
library(geojsonio)
library(shinyWidgets)
library(stringr)

# Bibliotecas operacionais
library(dplyr)
library(fpp3)
library(tsibble)
library(zoo)


# Leitura dos dados
dados <- read.csv("dados2011_2022.csv", sep=",", header = TRUE)
dados$Date = as.Date(dados$Date)


estacoes <- unique(dados$Station_code)
cidades <- read.csv("CatalogoEstaçõesAutomáticas.csv", sep=";", header = TRUE)

# Criando uma coluna com o nome da cidade e estado
cidades_mod <- data.frame(coluna1 = cidades$DC_NOME,coluna2 = cidades$SG_ESTADO)
cidades_mod$coluna1 <- str_to_title(tolower(cidades_mod$coluna1))
cidades_mod$Cidade_Estado <- paste(cidades_mod$coluna1, cidades_mod$coluna2, sep = "-")

# Variáveis
var <- names(dados)

# Função que calcula a média de cada variável por estação
calcular_media_por_estacao <- function(variavel, data_inicio, data_fim, dados) {
  # Filtrar os dados de acordo com a data de início e fim
  mascara <- (data_inicio <= dados$Date) & (dados$Date <= data_fim)
  dados_corte <- dados[mascara,]
  
  # Definir uma função para calcular a média ignorando NA
  mean_na <- function(x) mean(x, na.rm = TRUE)
  
  # Calcular a média da variável para cada estação
  media_por_estacao <- aggregate(x = dados_corte[, variavel], 
                                 by = list(dados_corte$Station), 
                                 FUN = mean_na)
  
  # Adicionar as colunas de latitude e longitude
  media_por_estacao$Latitude <- c("-15.78944","-3.10719","-30.05","-30.05361","-13.01667","-23.48333")
  media_por_estacao$Longitude <- c("-47.92583","-60.01639","-51.16667","-51.17472","-38.51667","-46.61667")
  
  # Renomear as colunas para ficar mais informativo
  colnames(media_por_estacao) <- c("Estacao", paste("Media", variavel, sep = "_"), "Latitude", "Longitude")
  
  return(media_por_estacao)
}


ui <- fluidPage(
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
    HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">Meteorologia INMET</a>'), id="nav",
    windowTitle = "Meteorologia INMET",

    tabPanel("Análise Temporal",
      navlistPanel(widths=c(2, 10),
        tabPanel("Média móvel",
          sidebarLayout(
            sidebarPanel(
              numericInput("k", "Período (dias):", value = 30, min = 0),
              selectInput("var", "Selecione a variável:", var, selected = var[2]),
              selectInput("est", "Selecione a estação:", estacoes),
              dateInput("data_i", "Data de início", "2015-01-01"),
              dateInput("data_f", "Data de fim", "2016-01-01")
            ),
            mainPanel(
                plotOutput("graph_media_movel")
            )
          )
        ),
        tabPanel("Sazonaliade")
      )
    ),
    tabPanel("Modelagem Preditiva"),
    tabPanel("Relações entre as regiões"),
    tabPanel("Dados"),
    tabPanel("Sobre o site",
      tags$div(
        tags$h4("Introdução"),
        "No contexto de um programa de aprendizado ao longo de um semestre acadêmico, empreenderemos uma jornada composta por quatro fases cruciais,
        culminando na elaboração de uma plataforma digital interativa voltada para a análise e exploração de dados meteorológicos oriundos da base 
        de dados do INMET, abrangendo o intervalo temporal de 2020 a 2021. Através deste percurso, conduziremos a transformação do conhecimento 
        teórico em aplicações práticas.",

        tags$br(),tags$br(),tags$h4("Exploração e Compreensão dos Dados Meteorológicos: Desvelando a Complexidade dos Dados"),
        "Na fase inaugural, imergiremos em um amplo acervo de dados meteorológicos públicos disponibilizados pelo INMET. 
        Nesta imersão, decifraremos a infraestrutura que alberga tais dados, solidificando uma compreensão aprofundada das metodologias 
        subjacentes à coleta e disseminação. A exploração deste ecossistema não somente ampliará o horizonte de compreensão dos dados, mas também
        fornecerá perspicácia sobre o contexto subjacente e as limitações inerentes.",
        
        tags$br(),tags$br(),tags$h4("Da Curiosidade à Formulação de Questões Relevantes: Navegando pelas Interrogações"),
        "Em posse dos questionamentos, procederemos à exploração das teorias matemáticas, estatísticas e computacionais que fundamentam a análise dos dados
        meteorológicos. Esta etapa nos capacitará a conduzir investigações mais abrangentes, utilizando técnicas estatísticas robustas e métodos 
        avançados de análise computacional. Ao fazê-lo, serão revelados padrões, tendências e correlações latentes nos dados, reforçando sua habilidade
        para sustentar decisões informadas.",
        
        tags$br(),tags$br(),tags$h4("Alicerces Teóricos para Análise Profunda: Explorando a Essência da Investigação"),
        "Em posse dos questionamentos, procederemos à exploração das teorias matemáticas, estatísticas e computacionais que fundamentam a análise
        dos dados meteorológicos. Esta etapa nos capacitará a conduzir investigações mais abrangentes, utilizando técnicas estatísticas robustas
        e métodos avançados de análise computacional. Ao fazê-lo, serão revelados padrões, tendências e correlações latentes nos dados, 
        reforçando sua habilidade para sustentar decisões informadas.",
        
        tags$br(),tags$br(),tags$h4("Desenvolvimento de uma Plataforma Web Interativa: Unindo Teoria e Prática"),
        "O ápice deste programa educacional será materializado na construção de uma plataforma digital interativa.
        Essa plataforma se converterá em um portal, viabilizando a análise de dados meteorológicos a um público de diversas disciplinas e 
        níveis de expertise. Por meio da adoção de uma abordagem intuitiva e customizada, a plataforma habilitará os usuários a explorar
        discernimentos de forma eficaz. Dessa maneira, a teoria será concretizada por meio de uma ferramenta funcional, oferecendo uma experiência
        envolvente tanto para os pesquisadores quanto para os interessados em compreender os padrões climáticos característicos do período 
        entre 2020 e 2021.",
        
        tags$br(),tags$br(),tags$h4("Referências"),
        tags$b("Pacote ‘BrazilMet’: "), tags$a(href="https://github.com/nytimes/covid-19-data", "Package ‘BrazilMet’"),tags$br(),
        tags$b("GitHub dos autores da biblioteca ‘BrazilMet’: "), tags$a(href="https://github.com/FilgueirasR/BrazilMet", "GitHub - FilgueirasR / BrazilMet"),tags$br(),
        tags$b("Portal do INMET: "), tags$a(href="https://portal.inmet.gov.br/", "Instituto Nacional de Meteorologia (INMET)"),tags$br(),
        
        tags$br(),tags$br(),tags$h4("Autores"),
        "Aime Gomes da Nobrega",tags$br(),
        "Alice Guimarães Perez",tags$br(),
        "André Dylan Andrade",tags$br(),
        "Carolina Spera Braga",tags$br(),
        "Daniel Gregório Chagas",tags$br(),
        "Matheus Vinicius Barreto de Farias",tags$br(),
        "Thaís Parron Alves",tags$br(),
        
        # tags$br(),tags$br(),tags$h4("Contact"),
        # "https://www.icmc.usp.br/",tags$br(),tags$br(),
        tags$img(src = "usp-university-of-sao-paulo7715.jpg", width = "120px", height = "65px"), tags$img(src = "logo-icmc.png", width = "120px", height = "65px")
      )
    )
  )
)


# Server logic
server <- function(input, output) {
  output$graph_media_movel <- renderPlot({

    mascara = (dados$Date >= as.Date(input$data_i)) &
        (dados$Date <= as.Date(input$data_f)) &
        (dados$Station_code == input$est)

    d <- dados[mascara,]
    d <- data.frame(x=d$Date, y=d[,input$var])

    ggplot(d, aes(x = x, y = y)) +
      geom_line() +
      geom_line(aes(y = rollmean(y, k = input$k, na.pad=TRUE), color="#FF0000")) +
      labs(x = "Data", y = input$var, color="Média móvel") +
      theme(legend.position = "none")
  })
}

shinyApp(ui = ui, server = server)