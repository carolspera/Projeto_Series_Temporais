
setwd("C:/Users/Usuario/Desktop/Estações 2")

library(BrazilMet)
stations <- see_stations_info()
nStations <- length(stations$OMM)

nome_estacoes <- rep(NA,nStations)
UFs <- rep(NA,nStations)
latitudes <- rep(NA,nStations)
longitudes <- rep(NA,nStations)
altitudes <- rep(NA,nStations)
datas_fundacao <- rep(NA,nStations)

erros <- file.create("C:/Users/Usuario/Downloads/Problemas Catálogo.txt", open = "w")
con <- file("C:/Users/Usuario/Downloads/Problemas Catálogo.txt", open = "a")

caminho = "C:/Users/Usuario/Desktop/Estações 2/"
for(i in 1:nStations){
  estacao <- read.csv(paste0(caminho,stations$OMM[i],".csv",sep = ""))
  
  nome_estacao <- unique(estacao$Station)
  if(length(nome_estacao) == 1){
    nome_estacoes[i] <- nome_estacao
    erro_nome_estacao <- FALSE
  } else {
    nome_estacoes[i] <- NA
    erro_nome_estacao <- TRUE
  }
  
  UF <- unique(estacao$UF)
  if(length(UF) == 1){
    UFs[i] <- UF
    erro_UF <- FALSE
  } else {
    UFs[i] <- NA
    erro_UF <- TRUE
  }

  latitude <- unique(estacao$Latitude..degrees.)
  if(length(latitude) == 1){
    latitudes[i] <- round(latitude, 3)
    erro_latitude <- FALSE
  } else if (abs(max(latitude) - min(latitude)) < 0.5) {
    latitudes[i] <- round(mean(latitude), 3)
    erro_latitude <- FALSE
  } else {
    latitudes[i] <- NA
    erro_latitude <- TRUE
  }
  
  longitude <- unique(estacao$Longitude..degrees.)
  if(length(longitude) == 1){
    longitudes[i] <- round(longitude, 3)
    erro_longitude <- FALSE
  } else if (abs(max(longitude) - min(longitude)) < 0.5) {
    longitudes[i] <- round(mean(longitude), 3)
    erro_longitude <- FALSE
  } else {
    longitudes[i] <- NA
    erro_longitude <- TRUE
  }
  
  altitude <- unique(estacao$Altitude..m.)
  if(length(altitude) == 1){
    altitudes[i] <- round(altitude, 3)
    erro_altitude <- FALSE
  } else if (abs(max(altitude) - min(altitude)) <= 20) {
    altitudes[i] <- round(mean(altitude), 3)
    erro_altitude <- FALSE
  } else {
    altitudes[i] <- NA
    erro_altitude <- TRUE
  }
  
  datas_fundacao[i] <- min(as.Date(estacao$Date, format = "%Y-%m-%d")[!is.na(estacao$Date)])
  
  if(sum(c(erro_nome_estacao,erro_UF,erro_latitude,erro_longitude,erro_altitude)) != 0){
    writeLines(" ", con)
    writeLines(paste0("################ ",stations$OMM[i]," ",i," ################", sep = ""), con)
    writeLines(" ", con)
    if(erro_nome_estacao == 1){
      writeLines("Nome da estação:", con)
      writeLines(paste(nome_estacao), con)
    }
    if(erro_UF == 1){
      writeLines("UF:", con)
      writeLines(paste(UF), con)
    }
    if(erro_latitude == 1){
      writeLines("Latitude:", con)
      writeLines(paste(latitude), con)
    }
    if(erro_longitude == 1){
      writeLines("Longitude:", con)
      writeLines(paste(longitude), con)
    }
    if(erro_altitude == 1){
      writeLines("Altitude:", con)
      writeLines(paste(altitude), con)
    }
  }
}
close(con)

catalogo <- data.frame(
  Código = stations$OMM,
  Nome = nome_estacoes,
  UF = UFs,
  Latitude = latitudes,
  Longitude = longitudes,
  Altitude = altitudes,
  Fundação = datas_fundacao
)

which(is.na(catalogo$Nome))
catalogo$Nome[101] <- "BRASNORTE (NOVO MUNDO)" # OU BRASNORTE (MUNDO NOVO)
catalogo$Nome[252] <- "FORMOSA DO RIO PRETO" # OU FORMOSO DO RIO PRETO
catalogo$Nome[374] <- "PORTO ALEGRE - JARDIM BOTANICO" # OU PORTO ALEGRE
catalogo$Nome[448] <- "PRESIDENTE KENNEDY" # OU PRES. KENNEDY
catalogo$Nome[472] <- "BELO HORIZONTE (PAMPULHA)" # OU PAMPULHA
catalogo$Nome[512] <- "NOVA PORTEIRINHA (JANAUBA)" # OU JANAUBA
catalogo$Nome[522] <- "SEROPEDICA-ECOLOGIA AGRICOLA" # OU ECOLOGIA AGRICOLA
catalogo$Nome[523] <- "RIO DE JANEIRO-MARAMBAIA" # OU MARAMBAIA
catalogo$Nome[524] <- "DUQUE DE CAXIAS - XEREM" # OU XEREM
catalogo$Nome[527] <- "CAMPOS DOS GOYTACAZES" # OU CAMPOS
catalogo$Nome[532] <- "TERESOPOLIS-PARQUE NACIONAL" # OU TERESOPOLIS
catalogo$Nome[533] <- "PARATY" # OU PARATI
catalogo$Nome[534] <- "CAMPOS DOS GOYTACAZES - SAO TOME" # OU SAO TOME
catalogo$Nome[535] <- "RIO DE JANEIRO - VILA MILITAR" # OU VILA MILITAR
catalogo$Nome[536] <- "NOVA FRIBURGO - SALINAS" # OU NOVA FRIBURGO
catalogo$Nome[545] <- "RIO DE JANEIRO - FORTE DE COPACABANA" # OU FORTE DE COPACABANA
catalogo$Nome[547] <- "SAQUAREMA - SAMPAIO CORREIA" # OU SAQUAREMA
catalogo$Nome[572] <- "SAO LUIZ DO PARAITINGA" # OU SAO LUIS DO PARAITINGA
catalogo$Nome[582] <- "BEBEDOURO" # OU BEBDOURO


for(var in names(catalogo)){
  cat("\n",sum(is.na(catalogo[[var]])))
}

caminho_arquivo <- ("~/Arquivos/Novo Principal/USP - São Carlos/8º Semestre/SME0808 - Séries Temporais e Aprendizado Dinâmico/Trabalho/Catálogo.csv")
write.csv(catalogo, file = caminho_arquivo, row.names = FALSE)



