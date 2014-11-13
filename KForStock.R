######sina，针对中国股市#########
get.quotes<-function(ticker,
                     from=(Sys.Date()-365),
                     to=(Sys.Date())){
      #ticker="sh600018";from<-as.Date("2014-11-11");to<-as.Date("2014-11-11")
      begin.base<-"http://biz.finance.sina.com.cn/stock/flash_hq/kline_data.php?&rand=random(10000)&";
      symbol<-paste("symbol=",ticker,sep="")
      #from and end date
      from.base<-"&begin_date="
      from.date<-paste(format(from,"%Y"),format(from,"%m"),format(from,"%d"),
                       sep="")
      to.base<-"&end_date="
      to.date<-paste(format(to,"%Y"),format(to,"%m"),format(to,"%d"),
                     sep="")
      #the end of url
      end.base<-"&type=plain"
      #bind all url
      url<-paste(begin.base,symbol,from.base,from.date,to.base,to.date,
                 end.base,sep="")
      
      #read the date, and insert the stock id
      tmp<-readLines(url)
      tmp<-strsplit(tmp,",")
      i<-0
      repeat {
            if(!is.null(unlist(tmp)) & i==0){
            tmp<-as.data.frame(matrix(unlist(tmp),ncol=6,byrow=TRUE))
            tmp<-cbind(symbol=ticker,tmp)
            i<-i+1}
            if(is.null(unlist(tmp)) | i!=0) break
      }
      #stockId,date,open,high,close,low,change
      tmp
}

get.multiple.quotes<-function(tkrs,
                              from=(Sys.Date()-365),
                              to=(Sys.Date())){
      tmp<-NULL
      for(tkr in tkrs){
            if(is.null(tmp))
                  tmp<-get.quotes(tkr,from,to)
            else tmp<-rbind(tmp,get.quotes(tkr,from,to))
      }
      tmp
}

dow.tickers<-as.vector(t(read.table("allStokeId.txt",head=FALSE)))
#tkrs<-dow.tickers;ticker<-tkrs[[2]]
write.table(get.multiple.quotes(dow.tickers,
                                from=as.Date("2014-07-01"),
                                to=as.Date("2014-10-31")),
            file="股票K值合并sina.txt",
            sep=",",row.names = FALSE,quote = FALSE,
            col.names=c("stockId","date","open","high","close","low","change"))

######yahoo,对于中国股市，会出现异常##################################
get.quotes<-function(ticker,
                     from=(Sys.Date()-134),
                     to=(Sys.Date()),
                     interval="d"){
      base<-"http://ichart.finance.yahoo.com/table.csv?";
      symbol<-paste("s=",ticker,sep="");
      
      from.month<-paste("&a=",
                        formatC(as.integer(format(from,"%m"))-1,width=2,flag="0"),
                        sep="");
      from.day<-paste("&b=",format(from,"%d"),sep="");
      from.year<-paste("&c=",format(from,"%Y"),sep="");
      to.month<-paste("&d=",
                      formatC(as.integer(format(to,"%m"))-1,width=2,flag="0"),
                      sep="");
      to.day<-paste("&e=",format(to,"%d"),sep="");
      to.year<-paste("&f=",format(to,"%Y"),sep="");
      inter<-paste("&g=",interval,sep="");
      last<-"&ignore=.csv";
      
      url<-paste(base,symbol,from.month,from.day,from.year,
                 to.month,to.day,to.year,inter,last,sep="");
      
      
      tmp<-read.csv(url);
      
      cbind(symbol=ticker,tmp);
      
}

get.multiple.quotes<-function(tkrs,
                              from=(Sys.Date()-134),
                              to=(Sys.Date()),
                              interval="d"){
      tmp<-NULL
      for(tkr in tkrs){
            if(is.null(tmp))
                  tmp<-get.quotes(tkr,from,to,interval)
            else tmp<-rbind(tmp,get.quotes(tkr,from,to,interval))
      }
      tmp
}

dow.tickers<-as.vector(t(read.table("yahooStokeId.txt",head=FALSE)))
tkrs<-dow.tickers
write.table(tmp,
            file="股票K值合并yahoo1.txt",
            sep=",",row.names = FALSE,quote = FALSE)


write.table(get.multiple.quotes(dow.tickers,
                                from=as.Date("2014-9-17","%Y-%m-%d"),
                                to=as.Date("2014-9-17","%Y-%m-%d")),
            file="股票K值合并yahoo.txt",
            sep=",",row.names = FALSE,quote = FALSE)
