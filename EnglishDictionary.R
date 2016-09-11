dictionary <- function(fname="word",path="/Users/takuma_m/DESKTOP",state="verb"){
  #internal function
  enendic <- function(kword){  
    library("rjson")
    
    # use rjson
    data <- fromJSON(readLines(paste0("http://api.pearson.com/v2/dictionaries/ldoce5/entries?headword=",kword, collapse="")))
    # get a data from Google Elevation API
    # convert JSON data into R object using fromJSON()
    # toJSON(): convert R object into JSON data
    i <- 1
    kakunin2 <- data$results[[i]]$part_of_speech[[1]]
    if((kakunin2 != state) == TRUE){
      while((kakunin2 != state) == TRUE){
        i <- i + 1
        kakunin2 <- data$results[[i]]$part_of_speech
      }
    } else {
      kakunin <- data$results[[i]]$senses[[1]]$definition
      enendic <- kakunin
    }
    
    kakunin3 <- data$results[[i]]$headword[[1]]
    if((kakunin3 != kword) == TRUE){
      while((kakunin3 != kword) == TRUE){
        i <- i + 1
        kakunin3 <- data$results[[i]]$headword
      }
    } else {
      kakunin <- data$results[[i]]$senses[[1]]$definition
      enendic <- kakunin
    }
    
    #i don't use this algorithm
    kakunin <- data$results[[i]]$senses[[1]]$definition
    if(is.null(kakunin) == TRUE){
      while(is.null(kakunin) == TRUE){
        i <- i + 1
        kakunin <- data$results[[i]]$senses[[1]]$definition
      } 
    } else {
      enendic <- kakunin
    }
  }
  setwd(path)
  data <- read.csv(paste0(fname,".csv", collapse = ""),header=FALSE)
  kekka <- matrix(rep(0,nrow(data)),byrow=TRUE)
  data <- cbind(data,kekka)
  for (i in 1:nrow(data)){
    val <- try(enendic(data[i,1]),silent=TRUE)
    if(class(val) != "try-error"){
      print(data[i,1])
      data[i,2] <- print(val)
      val <- ""
    }
  }
  fdate <- format(Sys.time(), "%Y%b%d%H%M")
  write.table(data, file=paste0(fname,gsub(" ","0",fdate),".xls", collapse = ""), sep="\t",row.names=FALSE,col.names=FALSE,fileEncoding="SHIFT-JIS") 
}
dictionary("word")