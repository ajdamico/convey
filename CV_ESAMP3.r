diretorio<-"C:\\Users\\djalma\\Desktop"
setwd(diretorio)

### Pessoas ###
dados.pes<-read.fwf("pessoas.dat", widths=c(4,4,-2,-2,-4,-4,15,4,-4,-4,-4,-4,-4,10))
colnames(dados.pes)<-c("tabela","ano","munic","var","CV")
dados.pes[,"CV"]<-dados.pes[,"CV"]/10
dados.pes[,"var"]<-paste(dados.pes[,"tabela"],".",dados.pes[,"var"],sep="")
dados.pes<-subset(dados.pes,select=-tabela)

                                ######################
                               ###      Legenda     ###
                              ###    Variável: var   ###                            
                             ############################
                            ###  762.2 - 0% até 10%    ### 
                            ###  762.3 - 10% até 20%   ###
                            ###  762.4 - 20% até 30%   ###
                            ###  762.5 - 30% até 40%   ###
                            ###  762.6 - 40% até 50%   ###
                            ###  762.7 - 50% até 60%   ###
                            ###  762.8 - 60% até 70%   ###
                            ###  762.9 - 70 até 80%    ###
                            ###  762.10 - 80 até 90%   ###
                            ###  762.11 - 90 até 100%  ###
                            ###  762.12 - 95 até 100%  ###
                            ###  762.13 - 99 até 100%  ###
                            ###  763.1 - Até 10        ###
                            ###  763.2 - Até 20        ###
                            ###  763.3 - Até 30        ###
                            ###  763.4 - Até 40        ###
                            ###  763.5 - Até 50        ###
                            ###  763.6 - Até 60        ###
                            ###  763.7 - Até 70%       ###
                            ###  763.8 - Até 80%       ###
                            ###  763.9 - Até 90%       ###
                            ###  763.10 - Até 100%     ###
                             ############################

### Domicílios ###
dados.dom<-read.fwf("domicilios.dat", widths=c(4,4,-2,-2,-4,-4,15,4,-4,-4,-4,-4,-4,10))
colnames(dados.dom)<-c("tabela","ano","munic","var","CV")
dados.dom[,"CV"]<-dados.dom[,"CV"]/10
dados.dom[,"var"]<-paste(dados.dom[,"tabela"],".",dados.dom[,"var"],sep="")
dados.dom<-subset(dados.dom,select=-tabela)

                                ########################
                               ###       Legenda      ###
                              ###     Variável: var    ###                            
                             ##############################
                            ###   3073.2 - 0% até 10%    ### 
                            ###   3073.3 - 10% até 20%   ###
                            ###   3073.4 - 20% até 30%   ###
                            ###   3073.5 - 30% até 40%   ###
                            ###   3073.6 - 40% até 50%   ###
                            ###   3073.7 - 50% até 60%   ###
                            ###   3073.8 - 60% até 70%   ###
                            ###   3073.9 - 70 até 80%    ###
                            ###   3073.10 - 80 até 90%   ###
                            ###   3073.11 - 90 até 100%  ###
                            ###   3073.12 - 95 até 100%  ###
                            ###   3073.13 - 99 até 100%  ###
                            ###   3074.1 - Até 10        ###
                            ###   3074.2 - Até 20        ###
                            ###   3074.3 - Até 30        ###
                            ###   3074.4 - Até 40        ###
                            ###   3074.5 - Até 50        ###
                            ###   3074.6 - Até 60        ###
                            ###   3074.7 - Até 70%       ###
                            ###   3074.8 - Até 80%       ###
                            ###   3074.9 - Até 90%       ###
                            ###   3074.10 - Até 100%     ###
                             ##############################
                            
### Gini ###
# FUNÇÃO
gini <- function(diretorio, arquivo, tabelaS, tabelaA, variavel, nivel,initS,initA){


  # Carregar pacote(s)
  library(survey)
  
  # Opções da survey
  options(survey.ultimate.cluster = TRUE)
  options(survey.lonely.psu = "adjust")
  options(survey.adjust.domain.lonely = TRUE)
  
  # Especificar pasta de trabalho
  setwd(diretorio)
  
  # Função que calculo coef. Gini e lineariza a renda para estimar o CV
  LIN_GINI<-function(inc, wght){
    inc<-inc[order(inc)]
    wght<-wght[order(inc)]
    # tamanho da população
    N = sum(wght)
    # tamanho da amostra
    n<-length(inc)
    # Renda total
    T<-sum(inc*wght)
    # peso acumulado
    r<-cumsum(wght)
    
    Num<-sum((2*r-1)*inc*wght)
    Den<-N*T
    
    # Gini coeficient
    Gini<-Num/Den-1
    
    # Função de distribuição de renda acumulada
    F<-cumsum(wght/N)
    
    #Função parcial ponderada
    G<-cumsum(inc*wght)
    
    #Variável linearizada do Coeficiente de Gini
    lin<-(2*(T-G+inc*wght+N*(inc*F))-inc-(Gini+1)*(T+N*inc))/(N*T)
    list(Gini,lin)
  }
  
  # Ler arquivo
  t1 <- Sys.time()
  tudo <- read.table(paste(diretorio, "\\", arquivo, sep = ""), header = F, dec = ",", as.is = TRUE)
  names(tudo) <- c("ANO","UFMUNIC","STRAT","PSU","PESO","POSEST","PESORIG","DOMINIO","V1")
  t2 <- Sys.time()
  cat("Leitura do arquivo:", round(t2-t1, 2), units(t2-t1),"\n")
  
  # Anos existentes no arquivo
  anos <- unique(tudo$ANO)
    
  # Municípios existentes no arquivo
  MUNIC <- unique(tudo$UFMUNIC)
  #freq.munic<-sort(table(tudo$UFMUNIC))
  #MUNIC<-dimnames(freq.munic)[[1]][c(1:3,29:31,56:58)]
  k<-1
  result<-matrix(0,length(anos)*length(MUNIC),4)
      
  # Rodar para cada ano
  for(ano in anos){
  
    # Tempo inicial
    t1 <- Sys.time()
    
    # Printar o ano na tela
    print(ano)
    
    # Filtrar apenas um ano
    dados <- subset(tudo, ANO == ano )
    
    # Rodar para cada município
    for(munic in MUNIC){
    
      # Filtar por domínio e município
      dados.1<-subset(dados, UFMUNIC==munic)
      
      # Criar objeto de desenho
      pnad.des <- svydesign(data = dados.1, ids = ~PSU, strata = ~STRAT, weights = ~PESORIG, nest = TRUE)
    
      # Pós-estratificação (desenho)
      pop.types <- data.frame(POSEST = as.character(unique(dados.1$POSEST)), Freq = as.numeric(as.character(unique(dados.1$POSEST))))
      pnad.post <- postStratify(design = pnad.des, strata = ~POSEST, population = pop.types)
      pnad.post.1<-subset(pnad.post,DOMINIO==1)
      
      # índice domínio = 1
      ind.dom<-which(dados.1$DOMINIO==1) 
            
      # Cálculo do CV
      GINI<-LIN_GINI(dados.1$V1[ind.dom],dados.1$PESO[ind.dom])
      z.0<-rep(0,nrow(dados.1))
      z.0[ind.dom]<-GINI[[2]]
      SE_GINI<-SE(svytotal(x=z.0,design=pnad.post.1))
      CV<-100*(SE_GINI/GINI[[1]])      
      
      # Organizar saída
      matriz<- as.data.frame(matrix(0, 1, 14))
      
      # Informações nas matrizes
      matriz[,1] <- tabelaS
      matriz[,2] <- ano
      matriz[,5] <- variavel
      matriz[,6] <- nivel
      matriz[,7] <- munic        
      matriz[,8] <- 0
      matriz[,14] <- round(10*CV, digits = 0)
      result[k,1]<- ano
      result[k,2]<- munic
      result[k,3]<- GINI[[1]]
     result[k,4]<- CV 
      k<-k+1
      # Especificar tamanho das variáveis e preencher lacunas com zeros  
      formatos <- c(4,4,2,2,4,4,15,4,4,4,4,4,4,10)
      for(i in 1:length(formatos)){
        matriz[,i] <- formatC(as.numeric(as.character(matriz[,i])), width = formatos[i], flag = "0", format = "d")   
      }
      
      # Exportar resultados           :
      if(ano == anos[1]&munic==MUNIC[1]){
        write.table(matriz, paste("gini",".dat", sep=""), dec = ".", sep = "", quote = F, row.names = FALSE, col.names = F, na = "")
      }else{
        write.table(matriz, paste("gini",".dat", sep=""), append = TRUE, dec = ".", sep = "", quote = F, row.names = FALSE, col.names = F, na = "")   
      }  
      
      # Tempo de execução acumulado por município
      t2 <- Sys.time()
      cat(ano," - (",munic,"): ", round(t2-t1, 2), " ", units(t2-t1), sep = "", "\n")
    
    # Fim do for de municípios
    }
  
  # Fim do for de anos
  }

result
# Fim da função
}

# EXECUTAR A FUNÇÃO
dados.gini<-gini(diretorio, 't7_1_2.txt', '762', '763', '0702','0006' , 2, 1)
dados.gini<-as.data.frame(dados.gini)
colnames(dados.gini)<-c("ano","munic","gini","CV")

#######################################################################
dados.gini<-transform(dados.gini,CV.cat=cut(CV,c(0,.5,1,2.5,5,7.5,10,15,25,35,50,200),labels=c("A","B","C","D","E","F","G","H","I","J","K")))
(table(dados.gini$CV.cat)/sum(table(dados.gini$CV.cat)))*100
nomes.munic<-read.table("Munics_Selecionados.txt")
names(nomes.munic)<- c("munic","nomes")
ftable(dados.gini$CV.cat~dados.gini$munic+dados.gini$ano)




tapply(dados.gini1$CV, dados.gini1$nomes,max)
tudo <- read.table(paste(diretorio, "\\", 't7_1_2.txt', sep = ""), header = F, dec = ",", as.is = TRUE)
  names(tudo) <- c("ANO","UFMUNIC","STRAT","PSU","PESO","POSEST","PESORIG","DOMINIO","V1")

by(tudo,tudo$ANO,function(t)table(t$UFMUNIC))  

table(tudo$UFMUNIC)

lixo<-cbind(nomes.munic,tab.vec)
dados.gini1<-merge(dados.gini,lixo)
dados.gini1<-dados.gini1[order(dados.gini1$tab.vec),]

lixo2<-lixo[order(lixo$tab.vec)[c(1:3,29:31,56:58)],]
result<-data.frame(nomes=as.vector(lixo2[,"nomes"]))

for (ano in 2001:2009){
  for (i in 1:9){
    result[i,paste(ano)]<-dados.gini1[dados.gini1[,"ano"]==ano & dados.gini1[,"nomes"]==as.vector(lixo2[,         "nomes"])[i],"CV"]
  }
}
result[,2:10]<-round(result[,2:10],2)

par(mfrow=c(3,1))

plot(2001:2009,result[1,2:10],ylim=c(0,25),xlim=c(2001,2009),ylab="CV(%)",xlab="Ano",type="l")
lines(2001:2009,result[2,2:10], col="red")
lines(2001:2009,result[3,2:10], col="green")
legend(2001.5,8,legend=as.character(result[1:3,1]),lty=c(1,1,1),col=c("black","red","green"))

plot(2001:2009,result[4,2:10],ylim=c(0,25),xlim=c(2001,2009),ylab="CV(%)",xlab="Ano",type="l")
lines(2001:2009,result[5,2:10], col="red")
lines(2001:2009,result[6,2:10], col="green")
legend(2001.5,8,legend=as.character(result[4:6,1]),lty=c(1,1,1),col=c("black","red","green"))

plot(2001:2009,result[7,2:10],ylim=c(0,25),xlim=c(2001,2009),ylab="CV(%)",xlab="Ano",type="l")
lines(2001:2009,result[8,2:10], col="red")
lines(2001:2009,result[9,2:10], col="green")
legend(2001.5,25,legend=as.character(result[7:9,1]),lty=c(1,1,1),col=c("black","red","green"))

############################
ano=2001
lixo<-dados.pes[dados.pes[,"ano"]==ano & dados.pes[,"munic"]%in%c(3529401,3548500,3513801,2211001,3304904,3518800,5300108,3550308,2927408) & dados.pes[,"var"]%in%c("763.1","763.2","763.3","763.4","763.5","763.6","763.7","763.8","763.9","763.10"),]

lixo[,"classe"]<-rep(seq(10,100,10),9)
lixo1<-merge(lixo,nomes.munic)
lixo1[,"nomes"]<-as.character(lixo1[,"nomes"])

par(mfrow=c(3,1))

plot(seq(10,100,10),lixo1[lixo1$nomes=="Mauá","CV"],ylim=c(0,40),xlim=c(10,100),ylab="CV(%)",xlab="% Acumulado",type="l")
lines(seq(10,100,10),lixo1[lixo1$nomes=="Santos","CV"], col="red")
lines(seq(10,100,10),lixo1[lixo1$nomes=="Diadema","CV"], col="green")
legend(85,40,legend=as.character(result[1:3,1]),lty=c(1,1,1),col=c("black","red","green"))

plot(seq(10,100,10),lixo1[lixo1$nomes=="Teresina","CV"],ylim=c(0,40),xlim=c(10,100),ylab="CV(%)",xlab="% Acumulado",type="l")
lines(seq(10,100,10),lixo1[lixo1$nomes=="São Gonçalo","CV"], col="red")
lines(seq(10,100,10),lixo1[lixo1$nomes=="Guarulhos","CV"], col="green")
legend(85,40,legend=as.character(result[4:6,1]),lty=c(1,1,1),col=c("black","red","green"))

plot(seq(10,100,10),lixo1[lixo1$nomes=="Brasília","CV"],ylim=c(0,40),xlim=c(10,100),ylab="CV(%)",xlab="% Acumulado",type="l")
lines(seq(10,100,10),lixo1[lixo1$nomes=="São Paulo","CV"], col="red")
lines(seq(10,100,10),lixo1[lixo1$nomes=="Salvador","CV"], col="green")
legend(85,40,legend=as.character(result[7:9,1]),lty=c(1,1,1),col=c("black","red","green"))





#######################

dados.pes1<-read.fwf("estim_esamp3_pes.dat", widths=c(4,4,-2,-2,-4,-4,15,4,-4,-4,-4,-4,-4,10))
colnames(dados.pes1)<-c("tabela","ano","munic","var","estim")
dados.pes1[,"estim"]<-dados.pes1[,"estim"]/10
dados.pes1[,"var"]<-paste(dados.pes1[,"tabela"],".",dados.pes1[,"var"],sep="")
dados.pes1<-subset(dados.pes1,select=-tabela)
lixo.pes<-merge(dados.pes,dados.pes1,all.x=F,all.y=T)
lixo.pes<-merge(lixo.pes,nomes.munic)
lixo.pes.sub<-subset(lixo.pes,ano%in%c(2001,2005,2009))


#######################

dados.dom1<-read.fwf("estim_esamp3_dom.dat", widths=c(4,4,-2,-2,-4,-4,15,4,-4,-4,-4,-4,-4,10))
colnames(dados.dom1)<-c("tabela","ano","munic","var","estim")
dados.dom1[,"estim"]<-dados.dom1[,"estim"]/10
dados.dom1[,"var"]<-paste(dados.dom1[,"tabela"],".",dados.dom1[,"var"],sep="")
dados.dom1<-subset(dados.dom1,select=-tabela)
lixo.dom<-merge(dados.dom,dados.dom1,all.x=F,all.y=T)
lixo.dom<-merge(lixo.dom,nomes.munic)
lixo.dom.sub<-subset(lixo.dom,ano%in%c(2001,2005,2009))