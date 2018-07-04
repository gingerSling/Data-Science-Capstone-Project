library(shiny)
library(data.table)
library(NLP)
library(tm)

tablauni<-read.csv("unidefR.csv")[-1]
tablabi<-read.csv("dfbtR.csv")[-1]
tablatri<-read.csv("dfttR.csv")[-1]
tablauni$names<-as.character(tablauni$names)
tablauni$total<-as.numeric(tablauni$total)
tablabi$names<-as.character(tablabi$names)
tablabi$total<-as.numeric(tablabi$total)
tablatri$names<-as.character(tablatri$names)
tablatri$total<-as.numeric(tablatri$total)
profanity<-readLines("profanity.txt")

tokenizeMe<-function(texto){
    aux<-gsub("[^ -~]","",texto)
    cleanText <- tolower(aux)
    cleanText <- removePunctuation(cleanText)
    cleanText <- removeNumbers(cleanText)
    cleanText<-removeWords(cleanText,profanity)
    tokens<-unlist(strsplit(cleanText,"[^a-z]+"))
    tokens<-tokens[tokens!=""]
    tokens
}
alfaB<-function(biPre,tb,gamma2){
    startUni<-tb[grep(paste("^",biPre$names,sep=""),tb$names),]
    alfa<-1-sum((startUni$total-gamma2)/biPre$total)
    alfa
}
alfaT<-function(triPre,tt,gamma3){
    startBi<-tt[grep(paste("^",triPre$names,sep=""),tt$names),]
    alfa<-1-sum((startBi$total-gamma3)/triPre$total)
    alfa
}

prediction2<-function(biG,tu=tablauni,tb=tablabi,tt=tablatri,gamma2=0.5,gamma3=0.5){
    token<-tokenizeMe(biG)
    #print(length(token))
    #token<-strsplit(biG,split=" ")[[1]]
    #print("token total")
    print(token)
    #(is.na(token))
    #(token)
    if(length(token)<2){
        return("We need you to type more words or different words.")
    }
    if(length(token)>2){
        token<-c(token[length(token)-1],token[length(token)])
    }
    uniCheck<-tu[tu$names==token[2],]
    if(dim(uniCheck)[1]<1){
        return("We need you to type more words or different words.")
    }
    else{
        uniCheck2<-tu[tu$names==token[1],]
        if(dim(uniCheck2)[1]==1){
            biPre<-tu[tu$names==token[2],]
            triPre<-tb[tb$names==paste(token[1],token[2]),]
            alfaBW<-alfaB(biPre,tb,0.5)
            alfaTW<-alfaT(triPre,tt,0.5)
            denOB<-tu[tu$names==token[2],]$total
            denOT<-tb[tb$names==paste(token[1],token[2]),]$total
            denUB<-sum(tu[!(paste(token[2],tu$names) %in% tb$names),]$total)
            unTT<-tu[!(paste(token[1],token[2],tu$names) %in% tt$names),]
            denUT<-sum(alfaBW*(tu[!(paste(token[2],tu$names) %in% tb$names),]$total/denUB))
            if(dim(tb[tb$names==paste(token[2],unTT[paste(token[2],unTT$names) %in% tb$names,]$names),])[1]>0){
                denUT<-denUT+sum((tb[tb$names==paste(token[2],unTT[paste(token[2],unTT$names) %in% tb$names,]$names),]$total-0.5)/tu[tu$names==token[2],]$total)
            }
            wp<-data.frame(names=as.character(),total=as.numeric())
            biPos<-paste(token[2],tu$names)
            triPos<-paste(token[1],token[2],tu$names)
            tabla<-data.frame(names=triPos,total=rep(0,length(triPos)))
            TOT<-ifelse(tt$names %in% triPos,((tt$total-gamma3)/denOT),0)
            TON<-ifelse(tt$names %in% triPos,tt$names,"qweasdzxcasd")
            TOBT<-ifelse(tb$names %in% biPos,(alfaTW*((tb$total-gamma2)/denOB)/denUT),0)
            TOBN<-ifelse(tb$names %in% biPos,tb$names,"qweasdzxcasd")
            TOBN<-paste(token[1],TOBN)
            aux<-unlist(strsplit(biPos[!(biPos %in% tb$names)],split=" "))
            TUB<-tu[tu$names %in% aux[seq(2,length(aux),2)],]
            TUB$total<-(alfaTW*(alfaBW*TUB$total/denUB)/denUT)
            TUB$names<-paste(token[1],token[2],TUB$names)
            TUBT<-ifelse(tb$names %in% biPos,0,(alfaTW*(alfaBW*tu[tu$names==strsplit(tb$names,split=" ")[[1]][2],]$total/denUB)/denUT))
            TUBN<-ifelse(tb$names %in% biPos,0,tu[tu$names==strsplit(tb$names,split=" ")[[1]][2],]$names)
            ###alternativa mergeando tablas###
            print("Estoy en la alternativa")
            TON<-as.character(TON)
            TOBdf<-data.frame(names=TOBN,total=TOBT)
            TOTdf<-data.frame(names=TON,total=TOT)
            ######################QUITAME!!!!!!!!!!!!!!!!!!!!!!#########################
            TOBdf<<-TOBdf
            TOTdf<<-TOTdf
            res<-TUB[!(TUB$names %in% TOBdf$names),]
            res<-rbind(res,TOBdf)
            res<-res[!(res$names %in% TOTdf$names),]
            res<-rbind(res,TOTdf)
            res<-res[-(grep("qweasdzxcasd",res$names)),]
            return(res[order(res$total,decreasing=TRUE),])
            
            ####################################
            
            
            return(tabla[order(tabla$total,decreasing = TRUE),])
        }
        
        else{
            return("We need you to type more words or different words.")
        }
    }
    
    
}

#27 1419 por un lado hay bigrams compuestos por unigrams que no existen (tm elimina stopwords) y por otro lado la asignacion que hago a la tabla final de las tablas auxiliares esta mal