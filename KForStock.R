######sina#########
get.quotes<-function(ticker,
                     from=(Sys.Date()-365),
                     to=(Sys.Date())){
      #eg:ticker="sh600018";from<-as.Date("2014-11-11");to<-as.Date("2014-11-11")
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
      #If one stock is not presence,the loop must be break for next one.
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

#all stocke id in this file.The type must be vectors.
#For shanghai,the vector must bulid like "sh600018"
#For shenzhen,the vector must bulid like "sz002241"
dow.tickers<-as.vector(t(read.table("etfID.txt",header=FALSE)))
dow.tickers<-c("sh000001","sz399106")
dow.tickers<-c("sh510180")
#write out
write.table(get.multiple.quotes(dow.tickers,
                                from=as.Date("2005-04-28"),
                                to=as.Date("2015-05-13")),
            file="sh000001.txt",
            sep=",",row.names = FALSE,quote = FALSE,
            col.names=c("stockId","date","open","high","close","low","change"))

write.table(get.multiple.quotes(dow.tickers,
                                from=as.Date("2015-04-28"),
                                to=as.Date("2015-05-13")),
            file="sh000001.txt",
            sep=",",row.names = FALSE,quote = FALSE)

########If there is missing data in dataframe ####
#insert function
insertFun<-function(stockname){
      partdt<-df1[df1$stockId==stockname,]
      minNA<-partdt[is.na(partdt$open),"date"]
      n<-length(minNA)
      for(i in 1:n){
            positionNA<-grep(minNA[i],partdt$date)
            #IF there has missing data, using the fowlling data insert to df
            if(positionNA !=1){
                  partdt[positionNA,3:7]<-partdt[positionNA-1,3:7]
            }
            else partdt[i,3:7]<-NA
      }
      partdt
}

#result
resultData<-function(df1){
      stockn<-unique(df1[is.na(df1$open),"stockId"])
      newdf<-data.frame()
      for(stockname in stockn){
            newdf<-rbind(newdf,insertFun(stockname))
      }
      newdf<-newdf[!is.na(newdf$open),]
      newdf<-rbind(newdf,df1[!(df1$stockId %in% stockn),])
      newdf<-newdf[c("stockId","date","open","high","close","low","change")]
      newdf
}

#read the data and out put the result
#read table,and sort
df<-read.table("etfData_more.txt",head=T,sep=",",
               colClasses=c("character","Date","numeric","numeric","numeric",
                            "numeric","numeric"))
df<-df[order(df$stockId,df$date),]
#build a time series
seriestime<-unique(df[order(df$date),"date"])
n<-length(seriestime)
#bind the data and time series
allstockname<-unique(df$stockId)
seriestime<-data.frame(date=as.Date(seriestime),
                       stockId=rep(allstockname,each=n))
df1<-merge(df,seriestime,by=c("date","stockId"),all.y=T)

#out put the result
write.table(resultData(df1),file="ETFData1.txt",quote =FALSE,sep = ",",
            row.names = FALSE)

######yahoo,face internal stock,not suit for china############
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


#all stocke id in this file.The type must be vectors.
#For stock in China,the vector must bulid like "600018.SS"or "002241.SZ"
dow.tickers<-as.vector(t(read.table("yahooStokeId.txt",head=FALSE)))
#write out
write.table(get.multiple.quotes(dow.tickers,
                                from=as.Date("2014-9-17","%Y-%m-%d"),
                                to=as.Date("2014-9-20","%Y-%m-%d")),
            file="YahooStock.txt",
            sep=",",row.names = FALSE,quote = FALSE)
