##eg:a<-comfile(file="D:/test",header=TRUE,sep="|","txt")
##eg:a<-comfile(file="D:/test",header=TRUE,sep="|","csv")


comfile<-function(file,header,sep,format){
      #path
      filename<-dir(path=file)
      
      #read txt or csv function
      if(format=="txt") fun<-function(file,header,sep) 
            read.table(file,header,sep)
      else fun<-function(file,header,sep) 
            read.csv(file,header,sep)
      
      #read file and combine
      dt<-lapply(paste(file,filename,sep="/"),fun,header=header,sep=sep)
      dt<-do.call(rbind,dt)
      
      #write
      write.table(dt,file=paste(file,"/all_data.txt",sep=""),sep=sep,
                  quote=FALSE,row.names =FALSE)
}

