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

# Traduções das variáveis
titulos <- list(
  "Tair_mean..c." = "Temperatura média do ar, em dias sucessivos",
  "Tair_min..c." = "Temperatura mínima do ar, em dias sucessivos",      
  "Tair_max..c." = "Temperatura máxima do ar, em dias sucessivos",    
  "Dew_tmean..c." = "Temperatura do ponto de orvalho média, em dias sucessivos",   
  "Dew_tmin..c." = "Temperatura do ponto de orvalho mínima, em dias sucessivos",
  "Dew_tmax..c." = "Temperatura do ponto de orvalho máxima, em dias sucessivos",    
  "Dry_bulb_t..c." = "Temperatura de bulbo seco, em dias sucessivos",  
  "Rainfall..mm." = "Precipitação total, em dias sucessivos",      
  "Patm..mB." = "Pressão atmosférica, em dias sucessivos",
  "Rh_mean..porc." = "Umidade relativa do ar média, em dias sucessivos",    
  "Rh_max..porc." = "Umidade relativa do ar máxima, em dias sucessivos",      
  "Rh_min..porc." = "Umidade relativa do ar mínima, em dias sucessivos",  
  "Ws_10..m.s.1." = "Velocidade do vento a 10 metros de altura, em dias sucessivos",     
  "Ws_2..m.s.1." = "Velocidade do vento a 2 metros de altura, em dias sucessivos",       
  "Ws_gust..m.s.1." = "Rajada de vento, em dias sucessivos",    
  "Wd..degrees." = "Direção do vento, em dias sucessivos",      
  "Sr..Mj.m.2.day.1." = "Radiação solar, em dias sucessivos",
  "Ra..Mj.m.2.day.1." = "Radiação extraterrestre por períodos diários, em dias sucessivos"
)

legendaS <- list(
  "Tair_mean..c." = 'Temperatura (°C)',
  "Tair_min..c." = 'Temperatura (°C)',      
  "Tair_max..c." = 'Temperatura (°C)',    
  "Dew_tmean..c." = 'Temperatura (°C)',   
  "Dew_tmin..c." = 'Temperatura (°C)',
  "Dew_tmax..c." = 'Temperatura (°C)',    
  "Dry_bulb_t..c." = 'Temperatura (°C)',  
  "Rainfall..mm." = 'Precipitação (mm)',     
  "Patm..mB." = 'Pressão atmosférica (mB)',       
  "Rh_mean..porc." = 'Umidade (%)',    
  "Rh_max..porc." = 'Umidade (%)',     
  "Rh_min..porc." = 'Umidade (%)',  
  "Ws_10..m.s.1." = 'Velocidade do vento (m/s)',    
  "Ws_2..m.s.1." = 'Velocidade do vento (m/s)',       
  "Ws_gust..m.s.1." = 'Velocidade do vento (m/s)',    
  "Wd..degrees." = 'Direção do vento (°)',      
  "Sr..Mj.m.2.day.1." = expression(paste("Radiação solar (MJ/",m^2,")")),
  "Ra..Mj.m.2.day.1." = expression(paste("Radiação solar (MJ/",m^2,")"))
)


# Variáveis
var <- names(dados)

ui <- fluidPage(
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
    HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">Meteorologia INMET</a>'), id="nav",
    windowTitle = "Meteorologia INMET",

    tabPanel("Análise Temporal",
      navlistPanel(widths=c(2, 10),
        tabPanel("Média móvel",
          sidebarLayout(
            sidebarPanel(
              numericInput("mediamovel_k", "Período (dias):", value = 30, min = 0),
              selectInput("mediamovel_var", "Selecione a variável:", var, selected = var[2]),
              selectInput("mediamovel_est", "Selecione a estação:", estacoes),
              dateInput("mediamovel_data_i", "Data de início", "2015-01-01"),
              dateInput("mediamovel_data_f", "Data de fim", "2016-01-01")
            ),
            mainPanel(
                plotOutput("graph_media_movel")
            )
          )
        ),
        tabPanel("Gráfico sazonal",
          sidebarLayout(
            sidebarPanel(
              selectInput("sazonalidade_var", "Selecione a variável:", var, selected = var[2]),
              selectInput("sazonalidade_est", "Selecione a estação:", estacoes),
              dateInput("sazonalidade_data_i", "Data de início", "2013-01-01"),
              dateInput("sazonalidade_data_f", "Data de fim", "2020-01-01")
            ),
            mainPanel(
                plotOutput("graph_sazonalidade")
            )
          )
        )
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
    data_i = input$mediamovel_data_i
    data_f = input$mediamovel_data_f
    estacao = input$mediamovel_est
    k = input$mediamovel_k
    variavel = input$mediamovel_var

    mascara = (dados$Date >= as.Date(data_i)) & (dados$Date <= as.Date(data_f)) & (dados$Station_code == estacao)

    d <- dados[mascara,]
    d <- data.frame(x=d$Date, y=d[,variavel])

    ggplot(d, aes(x = x, y = y)) +
      geom_line() +
      geom_line(aes(y = rollmean(y, k = k, na.pad=TRUE), color="#FF0000")) +
      labs(x = "Data", y = variavel, color="Média móvel") +
      theme(legend.position = "none")
  })

  output$graph_sazonalidade <- renderPlot({
    base = dados
    estacao = input$sazonalidade_est
    variavel = input$sazonalidade_var
    Data_ini = input$sazonalidade_data_i
    Data_fim = input$sazonalidade_data_f

    filtro <- filter(base, Station_code == toString(estacao) & Date >= toString(Data_ini) & Date <= toString(Data_fim) )
    # View(filtro)
    dados = tsibble(
      data = ymd(filtro$Date),
      y = filtro[[variavel]],
      index = data
    )
    G2 =
      dados %>%
      fill_gaps(data,y = mean(y),.full=TRUE) %>%
      gg_season(y, labels = 'both',period='year') +
      #gg_season(fill_gaps(dados,y = mean(y),.full=TRUE), labels = 'both') +
      labs(
        y = legendaS[[variavel]],
        x = 'Tempo (em meses)',
        title = titulos[[variavel]]
      );
    G2
  })
}

shinyApp(ui = ui, server = server)