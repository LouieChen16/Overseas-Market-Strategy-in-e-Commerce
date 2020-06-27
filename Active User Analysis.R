library(readr) ##To manage files
library(lubridate)   ##To manage time
data_file <- read.csv("D:/Tableau (Github)/Overseas-Market-Strategy-in-e-Commerce/Active User Analysis (FR).csv") #notice it should be /, not \

df <- data.frame(data_file)
##Transpose df
df_T <- as.data.frame(t(as.matrix(df)))

rownames(df_T) <- NULL
df_T[1,1] <- ""
rownames(df_T)[1] <- "Customer ID"
colnames(df_T) <- ""
colnames(df_T)[1] <- "Date"

df_T$Date <- as.Date(df_T$Date, '%Y年%m月%d日')

##Complete the whole Date Sequence
ro <- 3
repeat{
  if (is.na(df_T[ro, 1])){
    break
  }
  if (df_T$Date[ro] != df_T$Date[ro - 1] %m+% days(1)){
    new_row <- data.frame(matrix(nrow = 1, ncol = ncol(df_T)))  ##Set an empty 1 row df
    new_row[1] <- df_T$Date[ro - 1] %m+% days(1)  ##Set first element in df
    names(new_row) <- names(df_T)   ##Let names identical to combine 2 dataframes
    df_T <- rbind(df_T[1:ro-1,], new_row, df_T[ro:nrow(df_T),])
  }
  ro <- ro + 1
}
#View(df_T)
#df_T$Date[2] %m+% months(1) ##Add 1 month by library(lubridate)

##Set the last row date as 2011-12-31
repeat{
  if (df_T[nrow(df_T), 1] != ymd("2011-12-31")){
    new_row <- data.frame(matrix(nrow = 1, ncol = ncol(df_T))) ##Set an empty 1 row df
    new_row[1] <- df_T$Date[nrow(df_T)] %m+% days(1)  ##Set first element in df
    names(new_row) <- names(df_T)   ##Let names identical to combine 2 dataframes
    df_T <- rbind(df_T, new_row)
  }else{
    break
  }
}
##Replace NA with ""
df_T[2:ncol(df_T)][is.na(df_T[2:ncol(df_T)])] <- ""

##Setting New User, Returning User, Lost User
for (co in c(2:ncol(df_T))){
  vec <- c()
  vec <- apply(df_T[co], 2, function(x) which(x != ""))
  vec <- vec[-1]
  if(length(vec) >= 1){
    df_T[vec[1], co] <- "New User"
  }
  if(length(vec) >= 2){
  df_T[vec[2:length(vec)], co] <- "Returning User"
  }
  if(df_T[vec[length(vec)], 1] %m+% months(1) <= ymd("2011-12-31")){
    correct_row <- apply(df_T[1], 2, function(x) which(x == df_T[vec[length(vec)], 1] %m+% months(1)))
    correct_row
    df_T[correct_row, co] <- "Lost User"
  }
}

vec <- c()
vec <- apply(df_T[2], 2, function(x) which(x != ""))
vec
if(df_T[vec[length(vec)], 1] %m+% months(1) <= ymd("2011-12-31")){
  correct_row <- apply(df_T[1], 2, function(x) which(x == df_T[vec[length(vec)], 1] %m+% months(1)))
  correct_row
  df_T[correct_row, 2] <- "Lost User"
}

##Count New Users, Returning Users, Lost Users
New_Vec <- c("")
Re_Vec <- c("")
Lo_Vec <- c("")
for (ro in c(2:nrow(df_T))){
  New_Vec <- append(New_Vec, length(grep("New User", df_T[ro,])))
  Re_Vec <- append(Re_Vec, length(grep("Returning User", df_T[ro,])))
  Lo_Vec <- append(Lo_Vec, length(grep("Lost User", df_T[ro,])))
}
df_T <- cbind(df_T, Count_of_New_Customer = New_Vec, Count_of_Returning_Customer = Re_Vec, Count_of_Lost_Customer = Lo_Vec)

write.csv(df_T, "D:/Tableau (Github)/Overseas-Market-Strategy-in-e-Commerce/Active User Analysis processed (FR).csv")