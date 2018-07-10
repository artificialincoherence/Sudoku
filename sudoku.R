install.packages("readtext")
library(readtext)
setwd("C:/Users/Reizinger Kristóf/Desktop/Data Science/Sudoku")
txt <- readtext(paste0(Sample_tests.txt), text_field = "texts")
# ha ezt ide írom azt commiteli
data<-readtext("Sample_tests.txt",text_field = "text", dvsep=" ")
data
