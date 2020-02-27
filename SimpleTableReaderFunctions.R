
PDF_Table_Extract = function(FileName, Headers = NULL,SpannerExists = FALSE, PagesToRead = NULL, PagesPerSection = 1, YearCheck){
  # This is the initiating function to extract table data from a PDF file. The process is as follows:
  # 1. PDF_Table_Extract reads in the PDF pages as a character string. It does some initial formatting, and then splits the pages into sections.
  # 2. For each section, it calls PDF_Section_Format. This function arranges each section. 
  # 3. For each page in a section, PDF_Section_Format calls a third function, PDF_Page_Split, which converts the character string into a matrix. 
  # 4. PDF_Page_Split then returns these matrices to the flow of PDF_Section_Format. 
  # 5. From here PDF_Section_Format takes each page's matrix and combines them into one by binding the matrices horizontally, adding more columns.
  # 6. PDF_Section_Format then returns this combined matrix to the flow of PDF_Table_Extract; it also returns vectors contianing the row names.
  # 7. PDF_Table_Extract then stacks the matrices from each section vertically, making one matrix by adding rows.
  # 8. Lastly, it supplies the row names (taken from the first part of each individual line on each page), and if provided them, the headers.
  # 9. It then exports this to either R's global environment or the function that called it, whichever one was the source.
  
  
  if (is.null(PagesToRead)) {
    PagesToRead = c(1:get_n_pages(FileName))
  }
  
  
  # Initial Extraction of data into a character vector. Each element of the vector is a string that has a single page.
  TableList = extract_text(FileName, pages = PagesToRead, encoding = "UTF-8")
  
  # Turns the vector into a list and reorganizes each page. Each element of the list is a character vector containing a page. Each vector element is a single line on the page it is part of.
  
  # This breaks each page into each individual line.
  TableList = str_split(TableList,"\r\n")
  # This splits the line into individual columns.
  TableList = lapply(TableList, function(x) lapply(x, Col_Separator))
  # This trims the extra whitespace from individual cells.
  TableList = lapply(TableList, function(x) lapply(x, function(y) str_trim(y, side = "left")))
  # The Page Check function determines the appropriate number of columns for each page of the table by checking the modal number of columns for the page's lines. This includes eliminating pages that have only one column, as they are usually notes at the beginning/end of the table.
  PageColumns = Page_Check(TableList)
  # This eliminates the pages where their was only one column.
  TableList = TableList[which(!is.na(PageColumns))]
  # This gives the number of each column on the remaining pages.
  PageColumns = PageColumns[which(!is.na(PageColumns))]
  # This isolates a single section (each section contains all the pages with the same rows, if those rows extend onto multiple pages).
  PageColumns = PageColumns[1:PagesPerSection]
  
  
  # Splits the table into sections each of which becomes its own list. TableStart takes the first section, so that some extra data can be recovered (namely the column lengths). The rest of the sections are combined into a list of each section list, for easy use of lapply later.
  if (length(TableList) %% PagesPerSection != 0){
    return("Error: Table does not divide evenly by section.")
  }
  TableList = split(TableList,c(PagesPerSection:(length(TableList)+PagesPerSection-1))%/%PagesPerSection)
  TableStart = TableList[[1]]
  TableList = TableList[-1]
  
  # Calls the Section format function on the first section. This will reformat that section and find the number of columns on each page of this and all future sections.
  TableStart = PDF_Section_Format(TableStart, DataColNum = PageColumns, SpannerCheck = SpannerExists, YearCheck)
  
  # Grabs the number of columns of data from the first page, so it doesn't need to be found again.
  
  
  # Compiles the rest of the list 
  TableList = lapply(TableList, function(x) PDF_Section_Format(x, DataColNum = PageColumns, SpannerCheck = SpannerExists, YearCheck))
  TableRowNames = lapply(TableList, function(x) x[[2]])
  TableRowNames = list.prepend(TableRowNames, TableStart[[2]])
  
  
  
  
  TableList = lapply(TableList, function(x) x[[1]])
  TableList = list.prepend(TableList, TableStart[[1]])
  TableList = list.rbind(TableList)
  TableList = gsub(",","",TableList)
  class(TableList) = "numeric"
  if(length(Headers) == ncol(TableList)) {
    colnames(TableList) = Headers
  }
  
  if(SpannerExists == FALSE){
    TableRowNames = unlist(TableRowNames)
    TableRowNames = str_trim(TableRowNames, side = "right")
    rownames(TableList) = TableRowNames
  } else {
    TableRowNames = list.rbind(TableRowNames)
    TableRowNames[,1] = str_trim(TableRowNames[,1], side = "both")
    TableRowsCombined = paste(TableRowNames[,1],TableRowNames[,2], sep = "|")
    TableRowsCombined = str_trim(TableRowsCombined, side = "right")
    rownames(TableList) = TableRowsCombined
  }
  
  
  TableOut = list(TableList,TableRowNames,Headers)  
  
  return(TableOut)
}


# This function formats a section (a collection of 1 or more pages of a table, who all have the same spanner and rownames). It is used when a single row runs on to two or more pages. (See CPS Unpublished Basic Table 23 for an example.)
PDF_Section_Format = function(TableGroup, DataColNum, SpannerCheck = FALSE, YearCheck){
  # Runs the page-by-page formatting function.
  A = 1
  TableGroup = lapply(seq_along(TableGroup), function(x) PDF_Page_Split(TableGroup[[x]],DataColNumber = DataColNum[x], SpannerChecker = SpannerCheck, YearCheck))
  
  
  # Binds each page of the section together accross the rows, so that one matrix has all of the data for one row.
  if(SpannerCheck == FALSE) {
    TableRows = TableGroup[[1]][,1]
    TableGroup = lapply(seq_along(TableGroup), function(x) TableGroup[[x]][,2:DataColNum[x]])
  } else {
    TableRows = TableGroup[[1]][,1:2]
    TableGroup = lapply(seq_along(TableGroup), function(x) TableGroup[[x]][,3:(DataColNum[x]+1)])
  }
  TableRows = gsub("\\.","",TableRows)
  TableGroup = list.cbind(TableGroup)  
  # Groups all the pieces into a list for outputting.
  TableGroup = list(TableGroup,TableRows)
  
  # Returns List.
  return(TableGroup)
}


# This function splits an individual page of the table, removes the header info, and converts it into a matrix.
PDF_Page_Split = function(TableToSplit, DataColNumber, SpannerChecker = FALSE, YearCheck) {
  
  YearCheckVal = as.character(YearCheck)
  TableToSplit = rev(TableToSplit)
  T0 = lengths(TableToSplit)
  # This is the original variable for checking for multi-level row names
  T0Low = sapply(TableToSplit, function(x) str_detect(str_trim(x[1], side = "left"), "^[:lower:]"))
  # These are additional variables for checking for multi-level row names
  T0Dash = sapply(TableToSplit,function(x) str_detect(x[1], "-"))
  T0Span = sapply(TableToSplit, function(x) str_detect(x[1], "\\s[:lower:]"))
  T0Check = T0Dash&!T0Span
  T1 = rep(TRUE, length(TableToSplit))
  TableLength = length(TableToSplit) 
  k = 1
  while(k <= TableLength) {
    if ((T0[k] == DataColNumber)&(T0[k+1] == 1)) {
      if (T0Low[k]) {
        if (!T0Low[k+1]) {
          TableToSplit[[k]][1] = str_c(TableToSplit[k+1],TableToSplit[[k]][1], sep = " ")
          T1[k+1] = FALSE
          k = k + 1
        } else if ((T0[k+2] == 1)&(!T0Low[k+2])) {
            TableToSplit[[k]][1] = str_c(TableToSplit[[k+2]],TableToSplit[k+1],TableToSplit[[k]][1], sep = " ")
            T1[k+1] = FALSE
            T1[k+2] = FALSE
            k =  k + 2  
        } else {
            TableToSplit[k] = TableToSplit[k]
        }
      } else if (T0Check[k+1]) {
          TableToSplit[[k]][1] = str_c(TableToSplit[k+1],TableToSplit[[k]][1], sep = " ")
          T1[k+1] = FALSE
          k = k + 1
      } else if ((T0[k+2] == DataColNumber)|(TableToSplit[[k+2]][1] == YearCheckVal)|(TableToSplit[[k+2]][1] == str_c(YearCheckVal, "p"))) { 
          EmptyDataIn = rep("", DataColNumber - 1)
          TableToSplit[[k+1]] = c(TableToSplit[[k+1]][1], EmptyDataIn)
          
          T0 = c(T0[1:(k+1)], DataColNumber, T0[(k+2):TableLength])
          T0Check = c(T0Check[1:(k+1)], FALSE, T0Check[(k+2):TableLength])
          T0Dash = c(T0Dash[1:(k+1)], FALSE, T0Dash[(k+2):TableLength])
          T0Low = c(T0Low[1:(k+1)], FALSE, T0Low[(k+2):TableLength])
          T0Span = c(T0Span[1:(k+1)], FALSE, T0Span[(k+2):TableLength])
          T1 = c(T1[1:(k+1)], TRUE, T1[(k+2):TableLength])
          
          TableToSplit = list.insert(TableToSplit, (k+2), rep("", DataColNumber))
          
          k = k + 3
          TableLength = TableLength + 1
          
      } else {
          TableToSplit[k] = TableToSplit[k]
      }
    } else {
        TableToSplit[k] = TableToSplit[k]
    }
    k = k + 1
  }    

  
  
  
  
  TableToSplit = rev(TableToSplit[which(T1)])
  #Strips Header info, which must be supplied manually. The If statement breaks execution into two options, one if a spanner exists, and the other if it doesn't. Then it splits table into rownames and data, and formats them appropriately. Spanner is included with row names if it exists.
  if (SpannerChecker == FALSE) {
  
    TableToSplit = TableToSplit[-which(lengths(TableToSplit)!=DataColNumber)]
    #Converts the list of character vectors into a character matrix.
    TableToSplit = matrix(unlist(TableToSplit), ncol = DataColNumber, byrow = TRUE)
  
  } else {
    ExcessLines = which(lengths(TableToSplit)!=DataColNumber)
    TableToSplit = TableToSplit[-ExcessLines[-(match(DataColNumber,lengths(TableToSplit))-1)]]
    
    #Isolates the top row, containing the spanner info.
    TopRow = rep_len(TableToSplit[[1]],length(TableToSplit[-1]))
    
    #Converts the list of character vectors into a character matrix.
    TableToSplit = cbind(TopRow,matrix(unlist(TableToSplit[-1]), ncol = DataColNumber, byrow = TRUE))
  }
  
  
  
  #Returns the list.
  return(TableToSplit)
}


# Col_Separator, Page_Check and Mode_Finder have formatting roles in PDF_Table_Extract.


# Col_Separator splits the string containing a single line of the pdf into the separate columns of the table. If modifying this code for a table that is not formatted like our unpublished PDFs, watch this portion carefully. This relies heavily on regular expressions; for more info see the following:                http://stringr.tidyverse.org/articles/regular-expressions.html
Col_Separator = function(StringIn) {
  # The first command splits the line at what should be the end of the row name. It does this by making a split only after the last letter in the line. It is designed this way so that it won't split a row name with numbers in it (e.g. 18-19 year olds). The second command then splits the remainder of the line. It looks for a single space in-between two sets of digits (or a couple of versions of dashes).
  Y = unlist(str_split(StringIn, "(?<=[:alpha:]|\\.)\\s(?=(\\d|\u2013|\u2d)[^:alpha:]+$)"))
  if (length(Y)==2) {Z = c(Y[1], unlist(str_split(Y[2],"(?<=\\d|\u2013|\u2d)\\s+(?=\\d|\u2013|\u2d)")))} else {Z = Y}
  # Note, I added a plus to capture multiple white spaces within the str_split function inside the conditional. This may cause problems on the files this was originally used for. Examine carefullly. 
  return(Z)
}


# Page_Check determines the modal number of columns for each page. It eliminates all rows with only one column (as these are likely extraneous text) and then finds the modal number of columns for the remaining lines. It returns a NA if there are no lines with more than one column. PDF_Table_Extract will drop these pages - this was designed to automatically remove the introductory note pages that appear on some of the tables. See O&I05 in 2011 for an example of this intro page.
Page_Check = function(TableIn) {
  T0 = lapply(TableIn, lengths)
  T1 = lapply(T0, function(x) x[which(x>2)])
  T2 = sapply(T1, ModeFind)
  
  return(T2)
}


# This simple function finds the mode of a set of numbers. I use it in the Page_check Function.
ModeFind <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux)) 
  
  return(ux[tab == max(tab)])
  
}



LAUS_EETable_Headers = function(CurrentMonthValue, CurrentYearValue) {
  # MetroCheckValue==True means that it is one of the two metro end tables. MetroCheckValue==FALSE means it is one of the state end tables.
  # TableNameValue is an integer with either a 1 or a 2, depending on the table requested. 
  # Current Month is an integer from 1-12
  # Current year is a 4 digit integer representing the year.
  Header = matrix(nrow = 1, ncol = 13)
  
  for (j in 0:12) {
    if(CurrentMonthValue - j > 0) {
      YY = CurrentYearValue
    } else if(CurrentMonthValue - j <= 0) {
      YY = CurrentYearValue - 1
    }
    MM = (CurrentMonthValue - j + 12)%%12
    if(MM == 9) {
      Header[1,13-j] = str_c(month.abb[MM],"t.\n", YY)  
    } else if (any(MM == c(5,6,7))) {
      Header[1,13-j] = str_c(month.name[MM],"\n", YY)
    } else if (MM == 0) {
      Header[1,13-j] = str_c(month.abb[12],".\n", YY)
    } else {
      Header[1,13-j] = str_c(month.abb[MM],".\n", YY)
    }
  }
  
  return(Header)
  
}