# Import Data or read data.
# Alternately one could create a matrix , turn into a dataframe or create a dataframe 
# Table or Data Assumption. There is same number of fish type in both the tables and the sequence in which they are stored is also identical.

#TempPrice <- read_csv("~/Anurooba/UCSB/BREN/3 Spring 2017/262/Assignment/Assignment 4/TempPrice.csv")

#TempLocation <- read_csv("~/Anurooba/UCSB/BREN/3 Spring 2017/262/Assignment/Assignment 4/TempLocation.csv")

#View(TempLocation)
#View(TempPrice)


#Example : Create a function that takes as input a table that jas proces for different fish
#FishData <- function (df1){
  #View(df1)
#}
# Function call ---> FishData(TempPrice) , where TempPrice is the dataframe given as input to the FishData function



# FishyBusiness() Function takes two dataframes as inputs
# For out reference to Assignment :
#df1 - fish price table
#df2 - fish location table

FishyBusiness<- function(df1,df2)
     {
     
#Whereever this function us called, the ggplot2 package needs to installed. The fucntion uses ggplot2 lirbary
    
     library(ggplot2)
     
# Variable Initialization

     Table<- data.frame()
     rowPriceLocTotalAll <- 0
     
# Following variables will be used to store the row and column numbers in each of the table/dataframe loaded. These will act as count for the loops used later in the funciton.     
     numColumndf1 <- ncol(df1)
     numRowdf1 <- nrow(df1)
     numColumndf2 <- ncol(df2)
     numColumndf2max <- ncol(df2)
     numRowdf2 <- nrow(df2)

# The following lines of code is a loop function to caluclate max value present in a column in the data frame. 
# Considering Fish Location Table -> The loop parses throw each row within a column to find the maximum value. Loop omits the first column since it contains the reference variable, Fish Name
# 
     while (numColumndf2max > 1)
          {
          x <- max(df2[numColumndf2max])
          cat("Maximum catch for port at",names(df2[numColumndf2max]), "is..", x,"\n")
          numColumndf2max <- numColumndf2max - 1
          }
     
# The following lines of code is a loop funciton to calculate
#    1) Total Revenue for each port or Location
#    2) Grand Total Revenuw of all ports or Location
# The outer loop considers one column at a time of the Fish Location Data (omiting hte first column). One column gives the number of fishes caught in that particular location per fish type
# The inner loop considers the price for each fish from the Fish Price Data. And multiplies the price of each fish with the fish caught as per the location column under consideration from the Fish Location Data
     
     for(y in 2:numColumndf2)
     {
          # Variable Initialization for the outer loop
          rowPriceLoc <-0
          rowPriceLocTot <- 0
          
          for(x in 1:numRowdf1)
          {
               rowPrice <- df1[x,2]
               colCatch <- df2[x,y]
               rowPriceLoc <- rowPrice * colCatch
               rowPriceLocTot <- rowPriceLocTot + rowPriceLoc
          }
          
          # The following data frame holds the valuew of Location Name and Total Revenue per location . This table will be later used to plot the graph
          Table <- rbind(Table, data.frame(as.character(colnames(df2[y])), as.character(rowPriceLocTot)))
          
          # Print on console the result  
          cat(" Total Revenue for", colnames(df2[y]), ":", as.character(rowPriceLocTot),"\n")
          # For each pass add up the total revenue for location. This is building towards the grand total.
          rowPriceLocTotalAll <- rowPriceLocTotalAll + rowPriceLocTot
          } 
     

     cat(" Grand Total Revenue of the fishes from all the location:", as.character(rowPriceLocTotalAll),"\n")

     # Change the column name of the dataframe Table 
     colnames(Table) <- c("Location","TotalRev") 
     # Basic plot of the Location and Total Revenuw in each location
     ggplot(Table, aes(x=Location,y = TotalRev)) +geom_bar(stat = "identity")

}
