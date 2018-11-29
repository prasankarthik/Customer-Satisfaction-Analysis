#Reading the data
data_C <- fread("capital_city.csv")
View(data_C)
dim(data_C)

#Forming association rules for Capital city hotels
#Removing the not so important variables from the dataset
data_C <-  data_C[,-(4:11)]
data_C <- data_C[,-c(13)]
data_C <- data_C[,-c(5)]
colnames(data_C)

#Doing explanatory analysis by creating plots
hist(table(data_C$City_PL))
plot(data_C$City_PL,las=2)
counts <- table((data_C$City_PL))
barplot(counts, main="The capital cities in US", 
        xlab="City name", ylab = "No.of.customer responses",las=2)

#Converting the values in the dataframe into factors
data_C <- data.frame(lapply(data_C,as.factor))

#Forming rules
#Identifiying common factors with customer satisfaction score of 9 and 10
rules_C_1 <- apriori(data_C, parameter = list(support = 0.20, confidence = 0.40,  target = "rules"),appearance = list(rhs=c("Likelihood_Recommend_H=10","Likelihood_Recommend_H=9"),default="lhs"))
#Sorting the factos with high lift scores
top.lift_1 <- sort(rules_C_1,decreasing = TRUE,na.last=NA, by = "lift")
inspect(head(top.lift_1,100))