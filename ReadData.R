urlfolder<- 'https://raw.githubusercontent.com/NuoroForestrySchool/Data/master/TaperData/2018Poplar_TLSPuletti/'
file <- c("expData.csv")
urlfile <- paste0(urlfolder, file)
dsin<-read.csv(urlfile)
