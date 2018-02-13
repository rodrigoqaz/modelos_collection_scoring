##################
#Starting (from the bottom now we are here)#
##################
require(data.table)
diretorio="C:/Users/Trabalho/Documents/Lucas/Artigos/2018/Rbras 2018 - 2"
setwd(diretorio)
dados_modelo=fread("dados_modelo.TXT",sep=";",colClasses = "character",dec = ".",data.table = FALSE,drop=c(1,2,6,13,17,19,21,22,23))
dados_validacao=fread("dados_validacao.TXT",sep=";",colClasses = "character",dec = ".",data.table = FALSE,drop=c(1,2,6,13,17,19,21,22,23))
names(dados_modelo)[1]="CPF"
names(dados_validacao)[1]="CPF"

#importar as funçoes
diretorio2="C:/Users/Trabalho/Documents/Lucas/R/R - Pessoal/RLogistica"
setwd(diretorio2)
source("script_reglogistica.R")
source("script_dummies.R")
setwd(diretorio)

##################
#Data preparation#
##################
dados=dados_modelo
#########
dados=subset(dados,SEXO!="")
dados=subset(dados,ESTADO_CIVIL!="")
dados=subset(dados,IDADE!="")
dados=subset(dados,UF!="")
dados=subset(dados,IMOVEL_PROPRIO!="")
dados=subset(dados,VALOR_DIV!="")
dados=subset(dados,RESULTADO!="")
dados$CEP=substr(dados$CEP,1,1)
dados$VLR_NEG=ifelse(dados$VLR_NEG=="",0,1)
dados$DT_INICIO_ATRASO=as.Date(paste(substr(dados$DT_INICIO_ATRASO,1,2),
                                     substr(dados$DT_INICIO_ATRASO,3,4),
                                     substr(dados$DT_INICIO_ATRASO,5,8),sep="-"),format="%d-%m-%Y")
dados=data.frame(dados,ATRASO=as.Date(dados$DATA_ARQUIVO)-as.Date(dados$DT_INICIO_ATRASO))

dados=dados[,-c(2,16)]
dados$RESULTADO=as.factor(dados$RESULTADO)
dados=dummify(dados,"VALOR_DIV",class="numeric",validation=FALSE,save=TRUE)
dados=dummify(dados,"SEXO",class="factor",validation=FALSE,save=TRUE)
dados=dummify(dados,"ESTADO_CIVIL",class="factor",validation=FALSE,save=TRUE)
dados=dummify(dados,"IDADE",class="numeric",validation=FALSE,save=TRUE)
dados=dummify(dados,"UF",class="factor",validation=FALSE,save=TRUE)
dados=dummify(dados,"CEP",class="factor",validation=FALSE,save=TRUE)
dados=dummify(dados,"IMOVEL_PROPRIO",class="factor",validation=FALSE,save=TRUE)
dados=dummify(dados,"QTD_RECADO",class="numeric",validation=FALSE,save=TRUE)
dados=dummify(dados,"QTD_DIRETO",class="numeric",validation=FALSE,save=TRUE)
dados=dummify(dados,"VLR_NEG",class="factor",validation=FALSE,save=TRUE)
dados=dummify(dados,"QTD_SMS",class="numeric",validation=FALSE,save=TRUE)
dados=dummify(dados,"QTD_CARTA",class="numeric",validation=FALSE,save=TRUE)
dados=dummify(dados,"ATRASO",class="numeric",validation=FALSE,save=TRUE)
#Ordenando as colunas
pos=grep("D_",substr(names(dados),1,2)) #Todas as dummies
dados=cbind(dados[,-pos],dados[,pos])
dados_modelo=dados
rm(dados)
#########

#########
dados=dados_validacao
#########
dados=subset(dados,SEXO!="")
dados=subset(dados,ESTADO_CIVIL!="")
dados=subset(dados,IDADE!="")
dados=subset(dados,UF!="")
dados=subset(dados,IMOVEL_PROPRIO!="")
dados=subset(dados,VALOR_DIV!="")
dados=subset(dados,RESULTADO!="")
dados$CEP=substr(dados$CEP,1,1)
dados$VLR_NEG=ifelse(dados$VLR_NEG=="",0,1)
dados$DT_INICIO_ATRASO=as.Date(paste(substr(dados$DT_INICIO_ATRASO,1,2),
                                     substr(dados$DT_INICIO_ATRASO,3,4),
                                     substr(dados$DT_INICIO_ATRASO,5,8),sep="-"),format="%d-%m-%Y")
dados=data.frame(dados,ATRASO=as.Date(dados$DATA_ARQUIVO)-as.Date(dados$DT_INICIO_ATRASO))

dados=dados[,-c(2,16)]
dados$RESULTADO=as.factor(dados$RESULTADO)
dados=dummify(dados,"VALOR_DIV",class="numeric",validation=TRUE,save=FALSE)
dados=dummify(dados,"SEXO",class="factor",validation=TRUE,save=FALSE)
dados=dummify(dados,"ESTADO_CIVIL",class="factor",validation=TRUE,save=FALSE)
dados=dummify(dados,"IDADE",class="numeric",validation=TRUE,save=FALSE)
dados=dummify(dados,"UF",class="factor",validation=TRUE,save=FALSE)
dados=dummify(dados,"CEP",class="factor",validation=TRUE,save=FALSE)
dados=dummify(dados,"IMOVEL_PROPRIO",class="factor",validation=TRUE,save=FALSE)
dados=dummify(dados,"QTD_RECADO",class="numeric",validation=TRUE,save=FALSE)
dados=dummify(dados,"QTD_DIRETO",class="numeric",validation=TRUE,save=FALSE)
dados=dummify(dados,"VLR_NEG",class="factor",validation=TRUE,save=FALSE)
dados=dummify(dados,"QTD_SMS",class="numeric",validation=TRUE,save=FALSE)
dados=dummify(dados,"QTD_CARTA",class="numeric",validation=TRUE,save=FALSE)
dados=dummify(dados,"ATRASO",class="numeric",validation=TRUE,save=FALSE)
#Ordenando as colunas
pos=grep("D_",substr(names(dados),1,2)) #Todas as dummies
dados=cbind(dados[,-pos],dados[,pos])
dados_validacao=dados
rm(dados)
#########

#conferindo se os dados tem as mesmas dummies
names(dados_modelo)==names(dados_validacao)

# dados=dados_modelo[,-c(32:48)] #31,41, 48
# names(dados)

#Modelando e Escorando e seguindo a canção
modelo=scoring(dados_modelo,modelagem = T,graph = T,update = F)
validacao=scoring(dados_validacao,modelagem = F,graph = T,update = F)

write.table(modelo,file="dados_escorados_modelo.csv", sep=";",row.names = FALSE, quote = FALSE)
write.table(validacao,file="dados_escorados_validacao.csv", sep=";",row.names = FALSE, quote = FALSE)
