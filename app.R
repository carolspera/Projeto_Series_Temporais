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
df <- read.csv("A001.csv", header = TRUE)
var <- colnames(df)

dados <- read.csv("dados2011_2022.csv", sep=",", header = TRUE)
dados$Date = as.Date(dados$Date)


estacoes <- unique(dados$Station_code)
cidades <- read.csv("CatalogoEstaçõesAutomáticas.csv", sep=";", header = TRUE)

# Criando uma coluna com o nome da cidade e estado
cidades_mod <- data.frame(coluna1 = cidades$DC_NOME,coluna2 = cidades$SG_ESTADO)
cidades_mod$coluna1 <- str_to_title(tolower(cidades_mod$coluna1))
cidades_mod$Cidade_Estado <- paste(cidades_mod$coluna1, cidades_mod$coluna2, sep = "-")

# Variáveis
variaveis <- c("Temperatura do ar média (°C)","Temperatura do ar mínima (°C)","Temperatura do ar máxima (°C)","Temperatura do ponto de orvalho média (°C)",
               "Temperatura do ponto de orvalho mínima (°C)","Temperatura do ponto de orvalho máxima (°C)","Temperatura de bulbo seco (°C)",
               "Precipitação total (mm)","Umidade relativa média (%)","Umidade relativa mínima (%)","Umidade relativa máxima (%)",
               "Velocidade do vento a 10 metros de altura (m/s)","Velocidade do vento a 2 metros de altura (m/s)","Lufada - Rajada de vento (Wind gust) (m/s)",
               "Radiação solar (MJ/m²)")
ano <- c("2022","2021","2020","2019","2018","2017","2016","2015","2014","2013","2012","2011","2010","2009","2008","2007","2006","2005","2004","2003","2002")

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
              selectInput("var", "Selecione a variável:", var),
              selectInput("est", "Selecione a estação:", estacoes),
              dateInput("data_i", "Data de início", "2015-01-01"),
              dateInput("data_f", "Data de fim", "2016-01-01")
            ),
            mainPanel(
                plotOutput("graph_analise_temporal")
            )
          )
        )
      )
    )
  )
)


# Server logic
server <- function(input, output) {
  output$graph_analise_temporal <- renderPlot({

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
      # labs(title = paste("Series:", "teste"), x = "Data", y = input$var)
  })
}

shinyApp(ui = ui, server = server)