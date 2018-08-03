path<-"D:/Pratik Das/Machine Learning/Mumbai Rental Data"
setwd(path)

dataset<-read.csv("16-Jul-99acres-mumbai-rental-data.csv")
dataset[,'negotiable']<-1*grepl('Negotiable|negotiable|Neg|neg',dataset$description)

security_deposit<-function(x=dataset,col="description"){
  library(stringr)
  for(i in seq(nrow(x))){
    ext_val_n<-unlist(str_extract_all(x[i,col],pattern = "[:digit:]{2,7}"))
    ext_val_k<-unlist(str_extract_all(x[i,col],pattern = "[:digit:]+[.]*[:digit:]*k"))
    ext_val_k<-as.numeric(unlist(str_extract_all(ext_val_k,pattern = "[:digit:]+[.]*[:digit:]*")))*1000
    ext_val_l<-unlist(str_extract_all(x[i,col],pattern = "[:digit:]+[.]*[:digit:]*[:space:]*lakh|[:digit:]+[.]*[:digit:]*[:space:]*lac"))
    ext_val_l<-as.numeric(unlist(str_extract_all(ext_val_l,pattern = "[:digit:]+[.]*[:digit:]*")))*100000
    ext_val_c<-unlist(str_extract_all(x[i,col],pattern = "[:digit:]+[,]+[:digit:]*[,]*[:digit:]*"))
    ext_val_c<-as.numeric(gsub(pattern = ",",replacement = "",ext_val_c))
    ext_val<-as.numeric(c(ext_val_n,ext_val_k,ext_val_l,ext_val_c))
    ext_val<-ext_val[!(ext_val==x[i,"monthly.rent"]*10|ext_val%%10!=0)]
    if(length(ext_val)==0){
      x[i,"security.deposit"]<-'No info'
      next
    }
    x[i,"security.deposit"]<-ifelse(max(ext_val)<=x[i,"monthly.rent"]|is.na(max(ext_val)),'No info',max(ext_val))
  }
  return(x)
}

dataset<-security_deposit()
write.csv(dataset,file = "numeric_data_extracted.csv",row.names = F)
