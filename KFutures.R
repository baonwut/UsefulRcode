#about futures:http://blog.sina.com.cn/s/blog_53988e5a0101kx19.html
#Physical Futures:http://stock2.finance.sina.com.cn/futures/api/json.php/IndexService.getInnerFuturesDailyKLine?symbol=AU1506
#Physical Futures:though can use like AU0 as Gold Futures,But there something different from actual world
#Stock Index Futures:http://stock2.finance.sina.com.cn/futures/api/json.php/CffexFuturesService.getCffexFuturesDailyKLine?symbol=IF1504

#eg:id<-data.frame(c("IF1506" ,"IF1505"));type<-"Index"
#call:ReadAllFuture(id,type)
#write.table(ReadAllFuture(id,type),file="StockIndexFutures.txt",
 #           sep=",",row.names=F,quote =F,fileEncoding="UTF-8")
URLFun<-function(ID,TYPE=c("Physical","Index")){
      if(TYPE=="Physical") paste("http://stock2.finance.sina.com.cn/futures/api/json.php/IndexService.getInnerFuturesDailyKLine?symbol=",
                                 ID,sep="")
      if(TYPE=="Index") paste("http://stock2.finance.sina.com.cn/futures/api/json.php/CffexFuturesService.getCffexFuturesDailyKLine?symbol=",
                                 ID,sep="")
      
}

ReadOneFuture<-function(ID,TYPE){
      #read url
      con<-url(URLFun(ID,TYPE))
      data<-readLines(con,warn =F)
      close(con)
      #clean the data
      data<-gsub("\"|]|[[]","",data,perl=TRUE)
      data<-strsplit(data,",")
      #make the data frame
      data.frame(ID,matrix(unlist(data),ncol=6,byrow=T))
      
}

ReadAllFuture<-function(IDLIST,TYPE){
      #make the IDLIST as a vector
      if(class(IDLIST)=="data.frame") IDLIST<-as.vector(as.matrix(IDLIST))
      #merge future data into one dataframe
      dt<-data.frame()
      for(i in IDLIST){
            tmp<-ReadOneFuture(i,TYPE)
            dt<-rbind(dt,tmp)
      }
      dt<-dt[!dt[,2]=="null",]
      names(dt)<-c("ID","date","open","high","low","close","change")
      dt
}

