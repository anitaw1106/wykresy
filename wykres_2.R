getNBPRates <- function(year=2015){
  
  if(year>=2013){
    dane <- try({  
      
      lines <- readLines(paste0("https://www.nbp.pl/kursy/Archiwum/archiwum_tab_a_",year,".csv"))
      lines <- lines[-2]
      lines <- lines[-c((length(lines)-3):length(lines))]
      columnNames <- strsplit(lines[1],";",useBytes=T)[[1]]
      columnNames <- columnNames[-c((length(columnNames)-1):length(columnNames))]
      lines <- do.call("rbind",
                   lapply(strsplit(lines[-1],";"),
                          function(x){
                            matrix(as.numeric(gsub(",",".",x[-c((length(x)-1):length(x))])),nrow=1)
                          })
      )
      colnames(lines) <- columnNames
      df_rates <- as.data.frame(lines)
      df_rates <- df_rates[,grep("data|USD|EUR",colnames(df_rates))]
      df_rates$data <- as.Date(as.character(df_rates$data),format="%Y%m%d")
      colnames(df_rates) <- c('date','usd','eur')
    },silent=T)
    
    if(inherits(dane,"try-error")){
      cat(paste("Try again, an error!!!\n")) 
    }
    
    
  }
  
  return(df_rates)
  
}

a <- getNBPRates(year = 2015)
head(as.data.frame(a))
year_range <- 2014:2020
dane <- getNBPRates(2013)
for (i in year_range){
  tmp <- getNBPRates(i)
  dane <- rbind(dane, tmp)
  
}

library(ggplot2)

wykres <- ggplot(data=dane, aes(x=date, y=usd)) +
  geom_line(data=dane, aes(x=date, y=eur), colour = "red")+
  geom_line(colour = "blue") + 
  xlab("date")+
  ylab("usd")+
  ggtitle("Wykres kursów œrednich NBP dla EUR i USD")+
  theme_bw()

print(wykres)

