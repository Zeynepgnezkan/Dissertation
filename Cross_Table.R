# Zeynep Gunes Ozkan 2022, Contingency table
# Bournemouth University

# This script is a separate script written for the contingency table of the multi-response question purpose of use.
# To run this Script, first dataMix.Rda must be preprocessed and ready to use.
# If you haven't preprocessed dataMix.Rda, you can do it from Preproc.R script.

library(stringr)
library(gmodels)

dataCross = NULL
for(i in 1:nrow(dataMix)){
  if(grepl('[1]',dataMix$Purpose[i]) == TRUE){
    dataMix$purpose1[i] <- 1
  }else{
    dataMix$purpose1[i] <- 0
  }
  if(grepl('[2]',dataMix$Purpose[i]) == TRUE){
    dataMix$purpose2[i] <- 1
  }else{
    dataMix$purpose2[i] <- 0
  }
  if(grepl('[3]',dataMix$Purpose[i]) == TRUE){
    dataMix$purpose3[i] <- 1
  }else{
    dataMix$purpose3[i] <- 0
  }
  if(grepl('[4]',dataMix$Purpose[i]) == TRUE){
    dataMix$purpose4[i] <- 1
  }else{
    dataMix$purpose4[i] <- 0
  }
  if(grepl('[5]',dataMix$Purpose[i]) == TRUE){
    dataMix$purpose5[i] <- 1
  }else{
    dataMix$purpose5[i] <- 0
  }
  if(grepl('[6]',dataMix$Purpose[i]) == TRUE){
    dataMix$purpose6[i] <- 1
  }else{
    dataMix$purpose6[i] <- 0
  }
  if(grepl('[7]',dataMix$Purpose[i]) == TRUE){
    dataMix$purpose7[i] <- 1
  }else{
    dataMix$purpose7[i] <- 0
  }
  if(grepl('[8]',dataMix$Purpose[i]) == TRUE){
    dataMix$purpose8[i] <- 1
  }else{
    dataMix$purpose8[i] <- 0
  }
  if(grepl('[9]',dataMix$Purpose[i]) == TRUE){
    dataMix$purpose9[i] <- 1
  }else{
    dataMix$purpose9[i] <- 0
  }
}


cross=NULL
dataMix$subj <- seq(nrow(dataMix))
cross <- dataMix[,c("subj","Nationality","purpose1","purpose2","purpose3","purpose4","purpose5","purpose6","purpose7","purpose8","purpose9")]


multfreqtable = function(data, question.prefix) {
  z = length(question.prefix)
  temp = vector("list", z)
  
  for (i in 1:z) {
    a = grep(question.prefix[i], names(data))
    b = sum(data[, a] != 0)
    d = colSums(data[, a] )
    e = sum(rowSums(data[,a]) !=0)
    f = as.numeric(c(d, b))
    temp[[i]] = data.frame(question = c(sub(question.prefix[i], 
                                            "", names(d)), "Total"),
                           freq = f,
                           percent_response = (f/b)*100,
                           percent_cases = round((f/e)*100, 2))
    names(temp)[i] = question.prefix[i]
  }
  temp
}

multfreqtable(cross, "purpose")
# TR
dataCrosstr = NULL
for(i in 1:nrow(dataTR)){
  if(grepl('[1]',dataTR$Purpose[i]) == TRUE){
    dataTR$purpose1[i] <- 1
  }else{
    dataTR$purpose1[i] <- 0
  }
  if(grepl('[2]',dataTR$Purpose[i]) == TRUE){
    dataTR$purpose2[i] <- 1
  }else{
    dataTR$purpose2[i] <- 0
  }
  if(grepl('[3]',dataTR$Purpose[i]) == TRUE){
    dataTR$purpose3[i] <- 1
  }else{
    dataTR$purpose3[i] <- 0
  }
  if(grepl('[4]',dataTR$Purpose[i]) == TRUE){
    dataTR$purpose4[i] <- 1
  }else{
    dataTR$purpose4[i] <- 0
  }
  if(grepl('[5]',dataTR$Purpose[i]) == TRUE){
    dataTR$purpose5[i] <- 1
  }else{
    dataTR$purpose5[i] <- 0
  }
  if(grepl('[6]',dataTR$Purpose[i]) == TRUE){
    dataTR$purpose6[i] <- 1
  }else{
    dataTR$purpose6[i] <- 0
  }
  if(grepl('[7]',dataTR$Purpose[i]) == TRUE){
    dataTR$purpose7[i] <- 1
  }else{
    dataTR$purpose7[i] <- 0
  }
  if(grepl('[8]',dataTR$Purpose[i]) == TRUE){
    dataTR$purpose8[i] <- 1
  }else{
    dataTR$purpose8[i] <- 0
  }
  if(grepl('[9]',dataTR$Purpose[i]) == TRUE){
    dataTR$purpose9[i] <- 1
  }else{
    dataTR$purpose9[i] <- 0
  }
}


crosstr=NULL
dataTR$subj <- seq(nrow(dataTR))
crosstr <- dataTR[,c("subj","Nationality","purpose1","purpose2","purpose3","purpose4","purpose5","purpose6","purpose7","purpose8","purpose9")]

multfreqtable(crosstr, "purpose")

# UK
dataCrossuk = NULL
for(i in 1:nrow(dataUK)){
  if(grepl('[1]',dataUK$Purpose[i]) == TRUE){
    dataUK$purpose1[i] <- 1
  }else{
    dataUK$purpose1[i] <- 0
  }
  if(grepl('[2]',dataUK$Purpose[i]) == TRUE){
    dataUK$purpose2[i] <- 1
  }else{
    dataUK$purpose2[i] <- 0
  }
  if(grepl('[3]',dataUK$Purpose[i]) == TRUE){
    dataUK$purpose3[i] <- 1
  }else{
    dataUK$purpose3[i] <- 0
  }
  if(grepl('[4]',dataUK$Purpose[i]) == TRUE){
    dataUK$purpose4[i] <- 1
  }else{
    dataUK$purpose4[i] <- 0
  }
  if(grepl('[5]',dataUK$Purpose[i]) == TRUE){
    dataUK$purpose5[i] <- 1
  }else{
    dataUK$purpose5[i] <- 0
  }
  if(grepl('[6]',dataUK$Purpose[i]) == TRUE){
    dataUK$purpose6[i] <- 1
  }else{
    dataUK$purpose6[i] <- 0
  }
  if(grepl('[7]',dataUK$Purpose[i]) == TRUE){
    dataUK$purpose7[i] <- 1
  }else{
    dataUK$purpose7[i] <- 0
  }
  if(grepl('[8]',dataUK$Purpose[i]) == TRUE){
    dataUK$purpose8[i] <- 1
  }else{
    dataUK$purpose8[i] <- 0
  }
  if(grepl('[9]',dataUK$Purpose[i]) == TRUE){
    dataUK$purpose9[i] <- 1
  }else{
    dataUK$purpose9[i] <- 0
  }
}


crossuk=NULL
dataUK$subj <- seq(nrow(dataUK))
crossuk <- dataUK[,c("subj","Nationality","purpose1","purpose2","purpose3","purpose4","purpose5","purpose6","purpose7","purpose8","purpose9")]

multfreqtable(crossuk, "purpose")


