rm(list = ls())
library("tidyverse")
library("tabulizer")
library("stringr")
library("tictoc")
library("rlist")
library("xlsx")


cat("\014")
setwd("Z:/Reid/Fact-Checking tools/EETableCheckFiles/EETableCheckRFiles")
FunctionLoc = "SimpleTableReaderFunctions.R"
source(FunctionLoc)


# Put in the header labels for the PDF table you want to read.
HeaderValues = c("CNP 2018", "CNP 2019", "CLF 2018", "CLF 2019", "EMP 2018", "EMP 2019", "UNEMP 2018", "UNEMP 2019", "URATE 2018", "URATE 2019", "ERROR", "RANGE", "RATE")

# Put the file location for the PDF table you want to read.
FileLocation = "//filer6/dfsms/FSMSSRV0/LAUSOA/Regional_and_State_News_Release/2019/Ann_Avg_2019/review_folders_2019aa/Susan/table1_2019aa.pdf"

# Adjust the PagesPerSection and PagesToRead as needed to fit the table in question. Consider modifying the function to eliminate the YearCheck element.
AA = PDF_Table_Extract(FileLocation, Headers = HeaderValues, PagesPerSection = 1, PagesToRead = 1, YearCheck = 2019)


# This creates an Excel file containing the read-in table. Adjust the file destination path as desired.
A0 = createWorkbook()
A1 = createSheet(A0, sheetName = "Table1PDF")
addDataFrame(as.data.frame(AA[[1]]), A1, row.names = TRUE)
saveWorkbook(A0, file = "//filer6/dfsms/FSMSSRV0/LAUSOA/Regional_and_State_News_Release/2019/Ann_Avg_2019/review_folders_2019aa/Reid/Table1PDF.xlsx")
