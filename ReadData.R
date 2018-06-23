right = function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
}
stems_basic_measurements <- 
  readxl::read_xlsx("Pioppi_Viadana_CaratteriDendrometriciBase.xlsx") %>%
  mutate(TreeId = paste0("s", right(as.character(1000+TreeId),2))) %>%
  mutate_at(c("Tesi", "TreeId"), as.factor)


urlfolder<- 'https://raw.githubusercontent.com/NuoroForestrySchool/Data/master/TaperData/2018Poplar_TLSPuletti/'
file <- c("expData.csv")
urlfile <- paste0(urlfolder, file)
#TLSstem_vertical_crosssection<-read.csv(urlfile)
TLSderivedDiam <- read.csv("expData2.csv")
