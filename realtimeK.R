#notice:
#stock:http://hq.sinajs.cn/list=sh601006,sz000001
#index:http://hq.sinajs.cn/list=s_sh000001,s_sz399001
#futures Index:http://hq.sinajs.cn/list=CFF_RE_IF1506
#futures:http://hq.sinajs.cn/list=TA0,M0,RB1309,M1309
#Parameters：STOCKID,list of stock or index(vector).type:stock,index,futuresIndex,futures
#sh900922
#id<-"sz000875"
realtimeK<-function(STOCKID,TYPE="stock"){
    if(class(STOCKID)=="character"){id<-STOCKID}else{
          id<-as.vector(unlist(STOCKID))}
    if(TYPE=="stock"){
          #K on sina
          Klist<-unlist(lapply(id,function(id){
                readLines(paste("http://hq.sinajs.cn/list=",id,sep="")
                          )}))
          Klist<-substr(Klist,22,unlist(lapply(Klist,nchar))-2)
          Kdt<-data.frame(matrix(unlist(strsplit(Klist,",")),byrow=T,ncol=33))
          Kdt<-data.frame(id,Kdt)
          names(Kdt)<-c("id","stockname","todayOpen","yestClose",
                      "nowPrice","todayHigh",
                      "todayLow","buyPrice","sellPrice",
                      "volume","turnOver",
                      "buyOneAmount","buyOnePrice",
                      "buyTwoAmount","buyTwoPrice",
                      "buyThreeAmount","buyThreePrice",
                      "buyFourAmount","buyFourPrice",
                      "buyFiveAmount","buyFivePrice",
                      "sellOneAmount","sellOnePrice",
                      "sellTwoAmount","sellTwoPrice",
                      "sellThreeAmount","sellThreePrice",
                      "sellFourAmount","sellFourPrice",
                      "sellFiveAmount","sellFivePrice",
                      "Date","Time","mark")
          Kdt<-transform(Kdt,
                         nowPrice=as.numeric(as.character(nowPrice)),
                         yestClose=as.numeric(as.character(yestClose)))
          Kdt$nowPrice<-ifelse((Kdt$mark=="03") & (Kdt$nowPrice==0),
                               as.numeric(as.character(Kdt$yestClose)),
                               as.numeric(as.character(Kdt$nowPrice)))
          
          #market capitalization
          Klist1<-unlist(lapply(id,function(id){
                readLines(paste("http://qt.gtimg.cn/r=0.8409869808238q=",
                              id,sep="")
                          )}))
          Klist1<-substr(Klist1,16,unlist(lapply(Klist1,nchar))-2)
          Kdt1<-data.frame(matrix(unlist(strsplit(Klist1,"~")),
                                  byrow=T,ncol=48))
          Kdt1<-Kdt1[,c(2,44,45)]
          Kdt1<-data.frame(id,Kdt1)
          names(Kdt1)<-c("id","stockid","MarketValue","TotalMarketValue")
          
          #merge
          Kdt<-merge(Kdt,Kdt1,by="id",all.x=T)
    
    }
    if(TYPE=="index"){
          Klist<-unlist(lapply(id,function(id){
                readLines(paste("http://hq.sinajs.cn/list=s_",id,sep=""),
                          encoding ="UTF-8")}))
          Klist<-substr(Klist,23,unlist(lapply(Klist,nchar))-2)
          Kdt<-data.frame(matrix(unlist(strsplit(Klist,",")),byrow=T,ncol=6))
          Kdt<-data.frame(id,Kdt)
          names(Kdt)<-c("id","indexname",
                        "currentPoints","currentPrice",
                        "changeRate",
                        "volume_hand","turnOver_million")
    }
    if(TYPE=="futuresIndex"){
          Klist<-unlist(lapply(id,function(id){
                readLines(paste("http://hq.sinajs.cn/list=CFF_RE_",id,sep=""),
                          encoding ="UTF-8")}))
          Klist<-substr(Klist,19,unlist(lapply(Klist,nchar))-2)
          
          Kdt<-data.frame(matrix(unlist(strsplit(Klist,"[,]|[=\"]")),
                                 byrow=T,ncol=51))
          Kdt<-Kdt[c(1,3:10,16:17)]
          names(Kdt)<-c("id","open","high","low",
                        "currentPice","Amount","turnover",
                        "holdings","clearing",
                        "yesterdayClose","yesterdayClearing")
    }
    if(TYPE=="futures"){
          Klist<-unlist(lapply(id,function(id){
                readLines(paste("http://hq.sinajs.cn/list=",id,sep=""))}))
          Klist<-substr(Klist,12,unlist(lapply(Klist,nchar))-2)
          
          Kdt<-data.frame(matrix(unlist(strsplit(Klist,"[,]|[=\"]",perl =T)),
                                 byrow=T,ncol=30))
          Kdt<-Kdt[c(1,3,5:8,11:13,16:20)]
          names(Kdt)<-c("id","name",
                        "open","high","low","yesterdayClose",
                        "currentPice","clearing","yesterdayClearing",
                        "holdings","amount",
                        "exchange","nameabbr","date")
    }
    Kdt
}
