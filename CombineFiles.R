##可以同时处理csv和txt两种格式
##eg：a<-comfile(file="D:/test/",header=TRUE,sep="|","txt")
##eg：a<-comfile(file="D:/test/",header=TRUE,sep="|","csv")

comfile<-function(file,header,sep,format){
        data<-data.frame()        
        filename<-list.files(path=file,full.names=FALSE)
        if(format=="txt") fun<-function(file,header,sep) 
                read.table(file,header,sep)
        else fun<-function(file,header,sep) 
                read.csv(file,header,sep)
        i<-1
        for(i in i:length(filename)){
                d<-fun(file=paste(file,filename[i],sep=""),
                                 header=header,sep=sep)
                data<-rbind(d,data)
        }
        write.table(data,file=paste(file,"all_data.txt",sep=""),sep=sep,
                    quote=FALSE,row.names =FALSE)
}
