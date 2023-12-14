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
datas_finais <- rep(NA,nStations)

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
  latitudes[i] <- round(latitude[length(latitude)], 4)
  
  longitude <- unique(estacao$Longitude..degrees.)
  longitudes[i] <- round(longitude[length(longitude)], 4)
  
  altitude <- unique(estacao$Altitude..m.)
  altitudes[i] <- round(altitude[length(altitude)], 2)
  
  datas_validas <- which(!is.na(estacao$Date))
  datas_fundacao[i] <- as.character(min(as.Date(estacao$Date, format = "%Y-%m-%d")[datas_validas])) 
  datas_finais[i] <- as.character(max(as.Date(estacao$Date, format = "%Y-%m-%d")[datas_validas]))
    
  if( sum(c(erro_nome_estacao,erro_UF)) != 0 ){
    writeLines(" ", con)
    writeLines(paste0("################ ",stations$OMM[i]," ",i," ################", sep = ""), con)
    writeLines(" ", con)
    writeLines("Nome da estação:", con)
    writeLines(paste(nome_estacao,"-",UF), con)
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
  Fundação = datas_fundacao,
  DataFinal = datas_finais
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
