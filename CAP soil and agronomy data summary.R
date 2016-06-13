cscap <- read.csv("https://raw.githubusercontent.com/giorgichi/CSCAP/master/CSCAP_all_data_2016-6-9.csv")

cap <- as.data.frame(cscap)
#get names of sites you want to remove 
cap_sites_to_remove <- levels(cap$site)[grep("NAEW|ONFARM",levels(cap$site))]
#removing rows with unwanted sites
cap <- cap[! cap$site %in% cap_sites_to_remove,]
#remove column SOIL6 (soil texture) 
cap[,"SOIL6"] <- NULL


#runing a loop to convert all AGR and SOIL data from Factor to Numeric
for(i in which(names(cap)=="AGR1"):ncol(cap)) {
  cap[,i] <- as.numeric(levels(cap[,i]))[cap[,i]]
}
rm(i)


#run summary for measurements
capsum <- as.data.frame(summary(cap[,15:dim(cap)[2]]))[,-1]

#splitting column 
a <- data.frame(do.call('rbind',strsplit(as.character(capsum$Freq), ':', fixed = TRUE)))
capsum$Freq <-a[,1]
capsum$values <- a[,2]
rm(a)
names(capsum) <- c("code", "stat", "value")

#install.packages("stringr")
library(stringr)
capsum$code <- str_trim(capsum$code, side = "left")
capsum$stat <- str_trim(capsum$stat, side = "both")
capsum$value <- as.numeric(as.character(capsum$value))

#show codes that have not data (all data is NAs) 
no_meas_code <- capsum[capsum$stat=="NA's"& capsum$value==dim(cap)[1],1]

#write into text file tab-delimited data that does not contain measurements (codes) with no data
write.table(capsum[!capsum$code %in% no_meas_code,], 
            choose.files(), 
            sep='\t', quote = FALSE, row.names = FALSE)

library(tidyr)
#wide format for exporting data that does not contain measurements (codes) with no data
capsum_wide <- spread(capsum[! capsum$code %in% no_meas_code,],stat,value = value)

write.table(capsum_wide, choose.files(), 
            sep='\t', quote = FALSE, row.names = FALSE)