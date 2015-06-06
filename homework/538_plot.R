# Where To Find The Libertarians
# Americans' views on gay marriage 
# and income redistribution
# 2010-2012
# General Source Survey (eqwith and marhomo)

setwd("C:/Users/Emily/Desktop/Documents/_Spring 2015/cs183/my_plot")
library("vcd")
libData <- read.csv("m_gss.csv", header = TRUE, stringsAsFactors = FALSE)

########## Clean eqwith and set income favorable variable ##############
#change text to NAs, 1, or 7
for (i in 1:nrow(libData)){
  if (libData$eqwith[i] == "Not applicable"){
    libData$eqwith[i] = NA
  }
  else if (libData$eqwith[i] == "No answer"){
    libData$eqwith[i] = NA
  }
  else if (libData$eqwith[i] == "Don't know"){
    libData$eqwith[i] = NA
  }
  else if (libData$eqwith[i] == "Govt reduce diff"){
    libData$eqwith[i] = 1
  }
  else if (libData$eqwith[i] == "No govt action"){
    libData$eqwith[i] = 7
  }
}

# #set NAs to mean
# libData$eqwith = as.numeric(libData$eqwith)
# eq_mean = round(mean(libData$eqwith, na.rm=TRUE))
# for (i in 1:nrow(libData)){
#   if (is.na(libData$eqwith[i])){
#     libData$eqwith[i] = eq_mean
#   }  
# }

# if less than 4, in favor, not in favor otherwise
# in favor = 1, na/neither = 0, opposed = -1
for (i in 1:nrow(libData)){
  if (is.na(libData$eqwith[i])){
    libData$favincome[i] = 0
  }
  else if (libData$eqwith[i] == 4){
    libData$favincome[i] = 0
  }
  else if (libData$eqwith[i] < 4){
    libData$favincome[i] = 1
  }
  else{
    libData$favincome[i] = -1
  }
}

############### Clean marhomo and set marriage favorable variable ######

# change all to NA
for (i in 1:nrow(libData)){
  if (libData$marhomo[i] == "Not applicable"){
    libData$marhomo[i] = NA
  }
  else if (libData$marhomo[i] == "No answer"){
    libData$marhomo[i] = NA
  }
  else if (libData$marhomo[i] == "Cant choose"){
    libData$marhomo[i] = NA
  }
}

#in favor = 1, na/neither = 0, opposed = -1
for (i in 1:nrow(libData)){
  if (is.na(libData$marhomo[i])){
    libData$favmar[i] = 0
  }
  else if (libData$marhomo[i] == "Neither agree nor disagree"){
    libData$favmar[i] = 0
  }
  else if (libData$marhomo[i] == "Disagree" || libData$marhomo[i] == "Strongly disagree"){
    libData$favmar[i] = -1
  }
  else{
    libData$favmar[i] = 1
  }
}

######## Labels ############
for (i in 1:nrow(libData)){
  if (libData$favincome[i] == 1 && libData$favmar[i] == 1){
    libData$label[i] = "Liberal"
  }
  else if (libData$favincome[i] == -1 && libData$favmar[i] == 1){
    libData$label[i] = "Libertarian"
  }
  else if (libData$favincome[i] == 1 && libData$favmar[i] == -1){
    libData$label[i] = "Hardhat"
  }
  else if (libData$favincome[i] == -1 && libData$favmar[i] == -1){
    libData$label[i] = "Conservative"
  }
  else{
    libData$label[i] = "Other"
  }
}


mosaic(favincome ~ favmar, data=libData, gp=shading_binary, shade=TRUE)
