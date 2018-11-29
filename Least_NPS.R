#Installing Packages
getwd()
install.packages('data.table')
library(data.table)
install.packages("arules")
library("arules")

#Reading the dataset
test11<-fread("group.csv")

head(test1)

#Cleaning the dataset
test11 <- test11[,-c(48,38,14)]
test11<- test11[,-c(41,42)]
test11[test11==""] <- NA
test1 <- na.omit(test11)

#Performing Data manipulation
View(test1)
test11 <- test11[test11$State_PL == "New York",]
table(test11$Brand_PL)
table(test12$)
View(test12)
table(test12$Brand_PL)
table(test12$City_PL)

#Isolating the cities in New York and forming datasets for each cities
test_N <- test12[test12$City_PL=="New York",]
test_H <- test12[test12$City_PL=="Hauppauge",]
test_G <- test12[test12$City_PL=="Garden City",]
test_R <- test12[test12$City_PL=="Riverhead",]
test_W <- test12[test12$City_PL=="White Plains",]

#Writing a function to calculate NPS
#For some reason the values of variables inside the function are not getting updated in the environment, going to do the analyses inside the function itself
CalculateNPS <- function(data)
{ 
  count_p <- 0
  count_d <- 0
  count_n <- 0
  for(i in 1:nrow(data))
  {
    if(data[i,]$Likelihood_Recommend_H==9 |data[i,]$Likelihood_Recommend_H==10)
    {
      count_p <- count_p + 1
      
    }
    else if(data[i,]$Likelihood_Recommend_H==7 |data[i,]$Likelihood_Recommend_H==8)
    {
      count_n <- count_n +1
      
    }
    else
    {
      count_d <- count_d + 1
    }
    
  }
  print(paste("The number of neutral respondents is", count_n))
  print(paste("The number of positive respondents is", count_p))
  print(paste("The number of negative respondents is", count_d))
  total <- count_n + count_p + count_d
  print(paste("Total number of respondents is", total))
  NPS <- ((count_p - count_d)/total)*100
  print(paste("NPS for this particular hotel is", NPS))
}

#Calculating NPS for hotels in New York City in New York state
table(test_N$Brand_PL)
#There are three hotels in New York City 
#Calculating NPS for all three
#First for Andaz brand
#Calculating NPS for hotels in New York
test_N_A <- test_N[test_N$Brand_PL=="Andaz"]
test_N_G <- test_N[test_N$Brand_PL=="Grand Hyatt"]
test_N_H <- test_N[test_N$Brand_PL=="Hyatt"]
CalculateNPS(test_N_A)
CalculateNPS(test_N_G)
CalculateNPS(test_N_H)

#Calculating NPS for hotels in Hauppauge
table(test_H$Brand_PL)
test_H_H <- test_H[test_H$Brand_PL=="Hyatt Regency"]
CalculateNPS(test_H_H)

#Calculating NPS for hotels in Garden City
table(test_G$Brand_PL)
test_G_H <- test_G[test_G$Brand_PL=="Hyatt Place"]
CalculateNPS(test_G_H)
#Calculating NPS for hotels in Riverhead
table(test_R$Brand_PL)
test_R_H <- test_R[test_R$Brand_PL=="Hyatt Place"]
CalculateNPS(test_R_H)

#Calculating NPS for hotels in White Plains
table(test_W$Brand_PL)
test_W_H <- test_W[test_W$Brand_PL=="Hyatt House"]
CalculateNPS(test_W_H)


