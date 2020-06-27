#install.packages("plyr")
library(plyr)
library(readr)
data_file <- read.csv("D:/Tableau (Github)/Overseas-Market-Strategy-in-e-Commerce/Datasets/Preprocessed Data.csv") #notice it should be /, not \

df <- data.frame(data_file)
df <- df[order(df$CustomerID),]  ##Sorted by CustomerID

Cus_ID_Frq <- count(df, vars = "CustomerID")

#Calculate Total Sales
Total_Sal <- c()
Cus_row <- as.integer(1)
sum <- as.integer(0)
for(Mas_row in c(1:nrow(df))){
  sum <- df[Mas_row, 12] + sum  
  if (Mas_row == sum(Cus_ID_Frq[1:Cus_row,2])){
    Total_Sal <- append(Total_Sal, sum)
    cat("Start Total_Sal:", length(Total_Sal) + 1,"\n", sep='')
    Cus_row = Cus_row + 1
    sum <- as.integer(0)
  }
}
Total_Sal

##Calculate Quantity
Total_Quan <- c()
Ct <- c()
Cus_row2 <- as.integer(1)
sum2 <- as.integer(0)
for(Mas_row2 in c(1:nrow(df))){
  sum2 <- df[Mas_row2, 5] + sum2  
  if (Mas_row2 == sum(Cus_ID_Frq[1:Cus_row2,2])){
    Total_Quan <- append(Total_Quan, sum2)
    cat("Start Total_Quan:", length(Total_Quan) + 1,"\n", sep='')
    Cus_row2 = Cus_row2 + 1
    sum2 <- as.integer(0)
    Ct <- append(Ct, df[Mas_row2,9])
  }
}
Total_Quan

df2 <- cbind(Cus_ID_Frq, Total_Sales = Total_Sal, Total_Quantity = Total_Quan, Country = Ct)

df2 <- df2[order(-df2$Total_Sales),]  ##Sorted by Total_Sales (descending)
##Classify into 4 groups
Clf <- c()
for (df2_row in c(1:nrow(df2))){
  if (df2_row <= 0.01 * nrow(df2)){
    Clf <- append(Clf, "Platinum")
  }else if (df2_row > 0.01 * nrow(df2) & df2_row <= 0.05 * nrow(df2)){
    Clf <- append(Clf, "Gold")
  }else if (df2_row > 0.05 * nrow(df2) & df2_row <= 0.2 * nrow(df2)){
    Clf <- append(Clf, "Iron")
  }else{
    Clf <- append(Clf, "Lead")
  }
}
df2 <- cbind(df2, Classification = Clf)
#View(df2)
write.csv(df2, "D:/Tableau (Github)/Overseas-Market-Strategy-in-e-Commerce/Datasets/Customer Classification.csv")