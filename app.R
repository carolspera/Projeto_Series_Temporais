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

# Função que calcula as médias das variáveis por ano e armazena em um dataframe
calcular_media_por_ano <- function(variaveis, dados) {
    anos <- format(as.Date(dados$Date), "%Y")

    # Adicionando os anos como uma nova coluna nos dados
    dados$Ano <- as.integer(anos)

    # Inicializando um data frame para armazenar as médias
    medias_por_ano <- data.frame(Ano = integer(0), Station = character(0),
                                 Latitude = numeric(0), Longitude = numeric(0))

    # Loop sobre os anos e estações para calcular a média de cada variável por ano
    for (ano in unique(dados$Ano)) {
        for (estacao in unique(dados$Station)) {
            dados_filtrados <- subset(dados, Ano == ano & Station == estacao)

            if (nrow(dados_filtrados) > 0) {
                media_ano_estacao <- c(Ano = as.numeric(ano),
                                       Station = estacao,
                                       Latitude = as.numeric(unique(dados_filtrados$Latitude..degrees.)),
                                       Longitude = as.numeric(unique(dados_filtrados$Longitude..degrees.)))

                for (variavel in variaveis) {
                    media_ano_estacao[paste("Media", variavel, sep = "_")] <- mean(dados_filtrados[[variavel]], na.rm = TRUE)
                }

                medias_por_ano <- rbind(medias_por_ano, media_ano_estacao)
                colnames(medias_por_ano) <- c("Ano", "Station", "Latitude..degrees.", "Longitude..degrees.",
                                              "Tair_mean..c.", "Tair_min..c.", "Tair_max..c.",
                                              "Dew_tmean..c.", "Dew_tmin..c.", "Dew_tmax..c.", "Dry_bulb_t..c.",
                                              "Rainfall..mm.", "Rh_mean..porc.", "Rh_max..porc.",
                                              "Rh_min..porc.", "Ws_10..m.s.1.", "Ws_2..m.s.1.", "Ws_gust..m.s.1.",
                                              "Wd..degrees.", "Sr..Mj.m.2.day.1.") #"Ra..Mj.m.2.day.1." e "Patm..mB."

            }
        }
    }

    return(medias_por_ano)
}

variaveis <- c("Tair_mean..c.", "Tair_min..c.", "Tair_max..c.","Dew_tmean..c.", "Dew_tmin..c.", "Dew_tmax..c.", "Dry_bulb_t..c.", 
               "Rainfall..mm.", "Rh_mean..porc.", "Rh_max..porc.", "Rh_min..porc.", "Ws_10..m.s.1.", "Ws_2..m.s.1.", "Ws_gust..m.s.1.", 
               "Wd..degrees.", "Sr..Mj.m.2.day.1.") #"Ra..Mj.m.2.day.1." e "Patm..mB."
medias_por_ano <- calcular_media_por_ano(variaveis, dados)
medias_por_ano <- replace(medias_por_ano, is.na(medias_por_ano),  -3.10719)

# Variáveis
var <- names(dados)


# UI
ui <- fluidPage(
	navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
	    HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">Meteorologia INMET</a>'), id="nav",
	    windowTitle = "Meteorologia INMET",

        ## Análise geográfica
        tabPanel("Análise geográfica",
            div(class="outer",
                tags$head(includeCSS("styles.css")),
                leafletOutput("map", width="100%", height="100%"),
                
                absolutePanel(id = "controls", class = "panel panel-default",
                            top = 140, left = 55, width = 400, fixed=TRUE,
                            draggable = F, height = "auto", h3("Análise geográfica"),
                            
                    selectInput("var","Var:",label = h4("Selecione a variável:"),choices = variaveis),
                    sliderInput("ano", label = h4("Selecione o ano"), min = 2011, max = 2022, value = 2018), hr(),
                    fluidRow(column(4, verbatimTextOutput("value"))),
                    tags$div(id="cite", h6('Dados retirados do portal INMET.'))))
        ),

        ## Análise temporal
        tabPanel("Análise Temporal",
            navlistPanel(widths=c(2, 10),
            
                tabPanel("Média móvel",
                    sidebarLayout(
                        sidebarPanel(
                            numericInput("mediamovel_k", h5("Período (dias):"), value = 30, min = 0),
                            selectInput("mediamovel_var", h5("Selecione a variável:"), var, selected = var[2]),
                            selectInput("mediamovel_est", h5("Selecione a estação:"), estacoes),
                            dateInput("mediamovel_data_i", h5("Data de início"), "2015-01-01"),
                            dateInput("mediamovel_data_f", h5("Data de fim"), "2016-01-01"),
                            tags$div(id = "cite", h6('Dados retirados do portal INMET.'))),
                        mainPanel(plotOutput("graph_media_movel"))
                    )
                ),
                tabPanel("Gráfico sazonal",
                    sidebarLayout(
                        sidebarPanel(
                            selectInput("sazonalidade_var", h5("Selecione a variável:"), var, selected = var[2]),
                            selectInput("sazonalidade_est", h5("Selecione a estação:"), estacoes),
                            dateInput("sazonalidade_data_i", h5("Data de início"), "2013-01-01"),
                            dateInput("sazonalidade_data_f", h5("Data de fim"), "2020-01-01"),
                            tags$div(id = "cite", h6('Dados retirados do portal INMET.'))),
                        mainPanel(plotOutput("graph_sazonalidade"))
                    )
                )
            )
        ),
        
        ## Modelagem preditiva
        tabPanel("Modelagem preditiva"),

        ## Sobre o site
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


# Server 
server <- function(input, output){
    
    ## Análise temporal 
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

    ## Análise geográfica 
    output$map <- renderLeaflet({
        
        #Mapa para Tair_mean..c.
        if (input$var == "Tair_mean..c."){
            media_temp_ar <- as.numeric(medias_por_ano$"Tair_mean..c.")
            ano <- input$ano
            bins <- seq(16, 30, by = 2)
            data_filtered <- subset(medias_por_ano, Ano == ano)
            pal <- colorBin("YlOrRd", domain = data_filtered$media_temp_ar, bins = bins)
            labels <- sprintf("<strong>%s</strong><br/>%g anos<sup></sup>",
                                                data_filtered$Station, data_filtered$media_temp_ar) %>% lapply(htmltools::HTML)
            mapa <- leaflet(data = data_filtered) %>% addTiles() %>%
                addCircles(lng = ~as.numeric(Longitude..degrees.), lat = ~as.numeric(Latitude..degrees.), weight = 15, 
                                    popup = ~Station, radius = 30000, color = ~pal(as.numeric(Tair_mean..c.)), fillOpacity = 1) %>%
                addLegend("bottomright", pal = pal, values = ~as.numeric(Tair_mean..c.), title = "Temperatura do ar média (°C)", opacity = 1)
            
            mapa
            
            #Mapa para Média_Tair_min..c.   
        }else if (input$var == "Tair_min..c."){
            
            media_temp_ar_min <- as.numeric(medias_por_ano$"Tair_min..c.")
            ano <- as.numeric(medias_por_ano$Ano)
            ano <- input$ano
            bins <- seq(14, 30, by = 2)
            data_filtered <- subset(medias_por_ano, Ano == ano)
            pal <- colorBin("YlOrRd", domain = data_filtered$media_temp_ar_min, bins = bins)
            labels <- sprintf("<strong>%s</strong><br/>%g anos<sup></sup>",
                                                data_filtered$Station, data_filtered$media_temp_ar_min) %>% lapply(htmltools::HTML)
            mapa <- leaflet(data = data_filtered) %>% addTiles() %>%
                addCircles(lng = ~as.numeric(Longitude..degrees.), lat = ~as.numeric(Latitude..degrees.), weight = 15, 
                                    popup = ~Station, radius = 30000, color = ~pal(as.numeric(Tair_min..c.)), fillOpacity = 1) %>%
                addLegend("bottomright", pal = pal, values = ~as.numeric(Tair_min..c.), title = "Temperatura do ar mínima (°C)", opacity = 1)
            
            mapa
            
            #Mapa para Tair_max..c.        
        }else if (input$var == "Tair_max..c."){
            media_temp_ar_max <- as.numeric(medias_por_ano$"Tair_max..c.")
            ano <- input$ano
            bins <- seq(16, 34, by = 4)
            data_filtered <- subset(medias_por_ano, Ano == ano)
            pal <- colorBin("YlOrRd", domain = data_filtered$media_temp_ar_max, bins = bins)
            labels <- sprintf("<strong>%s</strong><br/>%g anos<sup></sup>",
                                                data_filtered$Station, data_filtered$media_temp_ar_max) %>% lapply(htmltools::HTML)
            mapa <- leaflet(data = data_filtered) %>% addTiles() %>%
                addCircles(lng = ~as.numeric(Longitude..degrees.), lat = ~as.numeric(Latitude..degrees.), weight = 15, 
                                    popup = ~Station, radius = 30000, color = ~pal(as.numeric(Tair_max..c.)), fillOpacity = 1) %>%
                addLegend("bottomright", pal = pal, values = ~as.numeric(Tair_max..c.), title = "Temperatura do ar máxima (°C)", opacity = 1)
            
            mapa
            
            #Mapa para Dew_tmean..c.        
        }else if (input$var == "Dew_tmean..c."){
            media_dew_m <- as.numeric(medias_por_ano$"Dew_tmean..c.")
            ano <- input$ano
            bins <- seq(10, 24, by = 2)
            data_filtered <- subset(medias_por_ano, Ano == ano)
            pal <- colorBin("YlOrRd", domain = data_filtered$media_dew_m, bins = bins)
            labels <- sprintf("<strong>%s</strong><br/>%g anos<sup></sup>",
                                                data_filtered$Station, data_filtered$media_dew_m) %>% lapply(htmltools::HTML)
            mapa <- leaflet(data = data_filtered) %>% addTiles() %>%
                addCircles(lng = ~as.numeric(Longitude..degrees.), lat = ~as.numeric(Latitude..degrees.), weight = 15, 
                                    popup = ~Station, radius = 30000, color = ~pal(as.numeric(Dew_tmean..c.)), fillOpacity = 1) %>%
                addLegend("bottomright", pal = pal, values = ~as.numeric(Dew_tmean..c.), title = "Temperatura do ponto de orvalho média (°C)", opacity = 1)
            
            mapa
            
            #Mapa para Dew_tmin..c.        
        }else if (input$var == "Dew_tmin..c."){
            media_dew_min <- as.numeric(medias_por_ano$"Dew_tmin..c.")
            ano <- input$ano
            bins <- seq(8, 22, by = 2)
            data_filtered <- subset(medias_por_ano, Ano == ano)
            pal <- colorBin("YlOrRd", domain = data_filtered$media_dew_min, bins = bins)
            labels <- sprintf("<strong>%s</strong><br/>%g anos<sup></sup>",
                            data_filtered$Station, data_filtered$media_dew_min) %>% lapply(htmltools::HTML)
            mapa <- leaflet(data = data_filtered) %>% addTiles() %>%
                addCircles(lng = ~as.numeric(Longitude..degrees.), lat = ~as.numeric(Latitude..degrees.), weight = 15, 
                                    popup = ~Station, radius = 30000, color = ~pal(as.numeric(Dew_tmin..c.)), fillOpacity = 1) %>%
                addLegend("bottomright", pal = pal, values = ~as.numeric(Dew_tmin..c.), title = "Temperatura do ponto de orvalho mínima (°C)", opacity = 1)
            
            mapa
            
            #Mapa para Dew_tmax..c.        
        }else if (input$var == "Dew_tmax..c."){
            media_dew_max <- as.numeric(medias_por_ano$"Dew_tmax..c.")
            ano <- input$ano
            bins <- seq(12, 26, by = 2)
            data_filtered <- subset(medias_por_ano, Ano == ano)
            pal <- colorBin("YlOrRd", domain = data_filtered$media_dew_max, bins = bins)
            labels <- sprintf("<strong>%s</strong><br/>%g anos<sup></sup>",
                                                data_filtered$Station, data_filtered$media_dew_max) %>% lapply(htmltools::HTML)
            mapa <- leaflet(data = data_filtered) %>% addTiles() %>%
                addCircles(lng = ~as.numeric(Longitude..degrees.), lat = ~as.numeric(Latitude..degrees.), weight = 15, 
                                    popup = ~Station, radius = 30000, color = ~pal(as.numeric(Dew_tmax..c.)), fillOpacity = 1) %>%
                addLegend("bottomright", pal = pal, values = ~as.numeric(Dew_tmax..c.), title = "Temperatura do ponto de orvalho máxima (°C)", opacity = 1)
            
            mapa
            
            #Mapa para Dry_bulb_t..c.        
        }else if (input$var == "Dry_bulb_t..c."){
            media_dry_bulb <- as.numeric(medias_por_ano$"Dry_bulb_t..c.")
            ano <- input$ano
            bins <- seq(16, 30, by = 2)
            data_filtered <- subset(medias_por_ano, Ano == ano)
            pal <- colorBin("YlOrRd", domain = data_filtered$media_dry_bulb, bins = bins)
            labels <- sprintf("<strong>%s</strong><br/>%g anos<sup></sup>",
                                                data_filtered$Station, data_filtered$media_dry_bulb) %>% lapply(htmltools::HTML)
            mapa <- leaflet(data = data_filtered) %>% addTiles() %>%
                addCircles(lng = ~as.numeric(Longitude..degrees.), lat = ~as.numeric(Latitude..degrees.), weight = 15, 
                                    popup = ~Station, radius = 30000, color = ~pal(as.numeric(Dry_bulb_t..c.)), fillOpacity = 1) %>%
                addLegend("bottomright", pal = pal, values = ~as.numeric(Dry_bulb_t..c.), title = "Temperatura de bulbo seco (°C)", opacity = 1)
            
            mapa
            
            #Mapa para Rainfall..mm.        
        }else if (input$var == "Rainfall..mm."){
            media_rainfall <- as.numeric(medias_por_ano$"Rainfall..mm.")
            ano <- input$ano
            bins <- seq(0, 10, by = 2)
            data_filtered <- subset(medias_por_ano, Ano == ano)
            pal <- colorBin("YlOrRd", domain = data_filtered$media_rainfall, bins = bins)
            labels <- sprintf("<strong>%s</strong><br/>%g anos<sup></sup>",
                                                data_filtered$Station, data_filtered$media_rainfall) %>% lapply(htmltools::HTML)
            mapa <- leaflet(data = data_filtered) %>% addTiles() %>%
                addCircles(lng = ~as.numeric(Longitude..degrees.), lat = ~as.numeric(Latitude..degrees.), weight = 15, 
                                    popup = ~Station, radius = 30000, color = ~pal(as.numeric(Rainfall..mm.)), fillOpacity = 1) %>%
                addLegend("bottomright", pal = pal, values = ~as.numeric(Rainfall..mm.), title = "Precipitação total (mm)", opacity = 1)
            
            mapa
            
            #Mapa para Rh_mean..porc.        
        }else if (input$var == "Rh_mean..porc."){
            media_urm <- as.numeric(medias_por_ano$"Rh_mean..porc.")
            ano <- input$ano
            bins <- seq(60, 82, by = 4)
            data_filtered <- subset(medias_por_ano, Ano == ano)
            pal <- colorBin("YlOrRd", domain = data_filtered$media_urm, bins = bins)
            labels <- sprintf("<strong>%s</strong><br/>%g anos<sup></sup>",
                                                data_filtered$Station, data_filtered$media_urm) %>% lapply(htmltools::HTML)
            mapa <- leaflet(data = data_filtered) %>% addTiles() %>%
                addCircles(lng = ~as.numeric(Longitude..degrees.), lat = ~as.numeric(Latitude..degrees.), weight = 15, 
                                    popup = ~Station, radius = 30000, color = ~pal(as.numeric(Rh_mean..porc.)), fillOpacity = 1) %>%
                addLegend("bottomright", pal = pal, values = ~as.numeric(Rh_mean..porc.), title = "Umidade relativa média (%)", opacity = 1)
            
            mapa
            
            #Mapa para Rh_max..porc.        
        }else if (input$var == "Rh_max..porc."){
            media_urma <- as.numeric(medias_por_ano$"Rh_max..porc.")
            ano <- input$ano
            bins <- seq(80, 98, by = 2)
            data_filtered <- subset(medias_por_ano, Ano == ano)
            pal <- colorBin("YlOrRd", domain = data_filtered$media_urma, bins = bins)
            labels <- sprintf("<strong>%s</strong><br/>%g anos<sup></sup>",
                                                data_filtered$Station, data_filtered$media_urma) %>% lapply(htmltools::HTML)
            mapa <- leaflet(data = data_filtered) %>% addTiles() %>%
                addCircles(lng = ~as.numeric(Longitude..degrees.), lat = ~as.numeric(Latitude..degrees.), weight = 15, 
                                    popup = ~Station, radius = 30000, color = ~pal(as.numeric(Rh_max..porc.)), fillOpacity = 1) %>%
                addLegend("bottomright", pal = pal, values = ~as.numeric(Rh_max..porc.), title = "Umidade relativa máxima (%)", opacity = 1)
            
            mapa
            
            #Mapa para Rh_min..porc.        
        }else if (input$var == "Rh_min..porc."){
            media_urmi <- as.numeric(medias_por_ano$"Rh_min..porc.")
            ano <- input$ano
            bins <- seq(36, 66, by = 4)
            data_filtered <- subset(medias_por_ano, Ano == ano)
            pal <- colorBin("YlOrRd", domain = data_filtered$media_urmi, bins = bins)
            labels <- sprintf("<strong>%s</strong><br/>%g anos<sup></sup>",
                                                data_filtered$Station, data_filtered$media_urmi) %>% lapply(htmltools::HTML)
            mapa <- leaflet(data = data_filtered) %>% addTiles() %>%
                addCircles(lng = ~as.numeric(Longitude..degrees.), lat = ~as.numeric(Latitude..degrees.), weight = 15, 
                                    popup = ~Station, radius = 30000, color = ~pal(as.numeric(Rh_min..porc.)), fillOpacity = 1) %>%
                addLegend("bottomright", pal = pal, values = ~as.numeric(Rh_min..porc.), title = "Umidade relativa mínima (%)", opacity = 1)
            
            mapa
            
            #Mapa para Ws_10..m.s.1.        
        }else if (input$var == "Ws_10..m.s.1."){
            media_ws10 <- as.numeric(medias_por_ano$"Ws_10..m.s.1.")
            ano <- input$ano
            bins <- seq(0.8, 2.6, by = 0.4)
            data_filtered <- subset(medias_por_ano, Ano == ano)
            pal <- colorBin("YlOrRd", domain = data_filtered$media_ws10, bins = bins)
            labels <- sprintf("<strong>%s</strong><br/>%g anos<sup></sup>",
                                                data_filtered$Station, data_filtered$media_ws10) %>% lapply(htmltools::HTML)
            mapa <- leaflet(data = data_filtered) %>% addTiles() %>%
                addCircles(lng = ~as.numeric(Longitude..degrees.), lat = ~as.numeric(Latitude..degrees.), weight = 15, 
                                    popup = ~Station, radius = 30000, color = ~pal(as.numeric(Ws_10..m.s.1.)), fillOpacity = 1) %>%
                addLegend("bottomright", pal = pal, values = ~as.numeric(Ws_10..m.s.1.), title = "Velocidade do vento a 10 metros de altura (m/s)", opacity = 1)
            
            mapa
            
            #Mapa para Ws_2..m.s.1.        
        }else if (input$var == "Ws_2..m.s.1."){
            media_ws2 <- as.numeric(medias_por_ano$"Ws_2..m.s.1.")
            ano <- input$ano
            bins <- seq(0.8, 2, by = 0.2)
            data_filtered <- subset(medias_por_ano, Ano == ano)
            pal <- colorBin("YlOrRd", domain = data_filtered$media_ws2, bins = bins)
            labels <- sprintf("<strong>%s</strong><br/>%g anos<sup></sup>",
                                                data_filtered$Station, data_filtered$media_ws2) %>% lapply(htmltools::HTML)
            mapa <- leaflet(data = data_filtered) %>% addTiles() %>%
                addCircles(lng = ~as.numeric(Longitude..degrees.), lat = ~as.numeric(Latitude..degrees.), weight = 15, 
                                    popup = ~Station, radius = 30000, color = ~pal(as.numeric(Ws_2..m.s.1.)), fillOpacity = 1) %>%
                addLegend("bottomright", pal = pal, values = ~as.numeric(Ws_2..m.s.1.), title = "Velocidade do vento a 2 metros de altura (m/s)", opacity = 1)
            
            mapa
            
            #Mapa para Ws_gust..m.s.1.        
        }else if (input$var == "Ws_gust..m.s.1."){
            media_l <- as.numeric(medias_por_ano$"Ws_gust..m.s.1.")
            ano <- input$ano
            bins <- seq(6.4, 9.6, by = 0.4)
            data_filtered <- subset(medias_por_ano, Ano == ano)
            pal <- colorBin("YlOrRd", domain = data_filtered$media_l, bins = bins)
            labels <- sprintf("<strong>%s</strong><br/>%g anos<sup></sup>",
                                                data_filtered$Station, data_filtered$media_l) %>% lapply(htmltools::HTML)
            mapa <- leaflet(data = data_filtered) %>% addTiles() %>%
                addCircles(lng = ~as.numeric(Longitude..degrees.), lat = ~as.numeric(Latitude..degrees.), weight = 15, 
                                    popup = ~Station, radius = 30000, color = ~pal(as.numeric(Ws_gust..m.s.1.)), fillOpacity = 1) %>%
                addLegend("bottomright", pal = pal, values = ~as.numeric(Ws_gust..m.s.1.), title = "Lufada - Rajada de vento (m/s)", opacity = 1)
            
            mapa
            
            #Mapa para Wd..degrees.        
        }else if (input$var == "Wd..degrees."){
            media_l <- as.numeric(medias_por_ano$"Wd..degrees.")
            ano <- input$ano
            bins <- seq(106, 166, by = 10)
            data_filtered <- subset(medias_por_ano, Ano == ano)
            pal <- colorBin("YlOrRd", domain = data_filtered$media_l, bins = bins)
            labels <- sprintf("<strong>%s</strong><br/>%g anos<sup></sup>",
                                                data_filtered$Station, data_filtered$media_l) %>% lapply(htmltools::HTML)
            mapa <- leaflet(data = data_filtered) %>% addTiles() %>%
                addCircles(lng = ~as.numeric(Longitude..degrees.), lat = ~as.numeric(Latitude..degrees.), weight = 15, 
                        popup = ~Station, radius = 30000, color = ~pal(as.numeric(Wd..degrees.)), fillOpacity = 1) %>%
                addLegend("bottomright", pal = pal, values = ~as.numeric(Wd..degrees.), title = "Direção do vento (°C)", opacity = 1)
            
            mapa
            
            #Mapa para Sr..Mj.m.2.day.1.        
        }else if (input$var == "Sr..Mj.m.2.day.1."){
            media_sr <- as.numeric(medias_por_ano$"Sr..Mj.m.2.day.1.")
            ano <- input$ano
            bins <- seq(12, 20, by = 2)
            data_filtered <- subset(medias_por_ano, Ano == ano)
            pal <- colorBin("YlOrRd", domain = data_filtered$media_sr, bins = bins)
            labels <- sprintf("<strong>%s</strong><br/>%g anos<sup></sup>",
                                                data_filtered$Station, data_filtered$media_sr) %>% lapply(htmltools::HTML)
            mapa <- leaflet(data = data_filtered) %>% addTiles() %>%
                addCircles(lng = ~as.numeric(Longitude..degrees.), lat = ~as.numeric(Latitude..degrees.), weight = 15, 
                                    popup = ~Station, radius = 30000, color = ~pal(as.numeric(Sr..Mj.m.2.day.1.)), fillOpacity = 1) %>%
                addLegend("bottomright", pal = pal, values = ~as.numeric(Sr..Mj.m.2.day.1.), title = "Radiação solar (MJ/m^2)", opacity = 1)
            
            mapa
            
        #   #Mapa para Ra..Mj.m.2.day.1.        
        # }else if (input$var == "Ra..Mj.m.2.day.1."){
        #   media_ra <- as.numeric(medias_por_ano$"Ra..Mj.m.2.day.1.")
        #   ano <- input$ano
        #   bins <- seq(12, 20, by = 2)
        #   data_filtered <- subset(medias_por_ano, Ano == ano)
        #   pal <- colorBin("YlOrRd", domain = data_filtered$media_ra, bins = bins)
        #   labels <- sprintf("<strong>%s</strong><br/>%g anos<sup></sup>",
        #                     data_filtered$Station, data_filtered$media_ra) %>% lapply(htmltools::HTML)
        #   mapa <- leaflet(data = data_filtered) %>% addTiles() %>%
        #     addCircles(lng = ~as.numeric(Longitude..degrees.), lat = ~as.numeric(Latitude..degrees.), weight = 15, 
        #                popup = ~Station, radius = 30000, color = ~pal(as.numeric(Ra..Mj.m.2.day.1.)), fillOpacity = 1) %>%
        #     addLegend("bottomright", pal = pal, values = ~as.numeric(Ra..Mj.m.2.day.1.), title = expression(paste("Radiação solar (MJ/",m^2,")")), opacity = 1)
        #   
        #   mapa
            
        }
    })
}

shinyApp(ui = ui, server = server)
