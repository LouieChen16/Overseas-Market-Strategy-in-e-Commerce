##Read File
#install.packages("readr")
library(readr)
data_file <- read.csv("D:/Tableau (Github)/Overseas-Market-Strategy-in-e-Commerce/Datasets/data.csv") #notice it should be /, not \

df <- data.frame(data_file)
##check type of each columns
col_name <- c()
type <- c()

for (coln in c(1:ncol(df))){
  #print(colnames(df[coln]))
  #print(typeof(df[,coln]))  ##Check type
  #cat(colnames(df[coln]), typeof(df[,coln]))
  col_name <- append(col_name, colnames(df[coln]))
  type <- append(type, typeof(df[,coln]))
}
print(rbind(col_name, type)) ##print a matrix

##Set correct data type and format
df$CustomerID <- as.character(df$CustomerID) ##Set type of CustomerID to character
#typeof(df$CustomerID)

##Split InvoiceDate into Date and Time
split_list <- strsplit(df[1:nrow(df),5], " ")  ##split InvoiceDate with " "
#strsplit(df[1:16,5], " ")[[1]][2] ## first list, second string

part1    <- unlist(split_list)[2*(1:nrow(df))-1] ##Turn list into vector and only take odd element
part2    <- unlist(split_list)[2*(1:nrow(df))  ] ##Turn list into vector and only take even element

colnames(df)[5] <- "InvoiceDate&Time" ##Rename col[5]

df <- cbind(df, InvoiceDate = part1, InvoiceTime = part2) ##Add InvoiceDate and InvoiceTime to df
#colnames(df2)

df$InvoiceDate <- as.Date(df$InvoiceDate,  '%m/%d/%Y') ##Set type to date
df$InvoiceTime <- format(as.POSIXct(df$InvoiceTime,format="%H:%M"), "%H:%M") ##Set type to time
#colnames(df)

##Create Price Column
Total_Price <- df[4]*df[6]
colnames(Total_Price) <- "Sales"
df <- cbind(df, Total_Price)

##omit null, negative numbers
dim(df)
df[df < 0] <- NA  ##Change every negative number to NA (If condition in [] is true, then replace them with NA)
nona <- na.omit(df)  ##omit NA
dim(nona)
write.csv(nona, "D:/Tableau (Github)/Overseas-Market-Strategy-in-e-Commerce/Datasets/Preprocessed Data.csv")