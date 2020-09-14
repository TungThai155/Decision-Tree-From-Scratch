# Question 3 - Homework 2
# Created by Tung Thai
setwd("C:/Users/tungt/OneDrive/Undergraduate/2019-2 SM/Machine Learning/Homework/Homework 2")
library(readr)
insects <- read_csv("insects_data.csv")
# Create a name variable to store the name in insects data
name1 <- character()
name1 <- colnames(insects)
n <- ncol(insects)
nr <- nrow(insects)
gini_results <- numeric()
ant <- insects$Antenna_Length
# Initialize a test sequence with length test_num
# The length (test_num) can be changed depend on how detail you want to test the program
test_num <- 10
split_point <- seq(min(insects$Antenna_Length),max(insects$Antenna_Length),length = test_num)
ns <- length(split_point)
for (i in 1:ns) {
  for (k in 1:nr) {
  if (insects$Antenna_Length[k] < split_point[i]) {
    ant[k] = 1
  }
  else
  {
    ant[k] = 2
  }
# Gini mannually uses which()
  n1 <- length(which(ant==1))
  n2 <- length(which(ant==2))
  if (n1==0) {n1=1}
  if (n2==0) {n2=1}
  #n1 yes = 1
  n1y <- length(which(ant==1 & insects[,1]==1))
  #n1 no = 2
  n1n <- n1-n1y
  #n2 yes = 1
  n2y <- length(which(ant==2 & insects[,1]==1))
  #n2 no = 2
  n2n <- n2-n2y
  gini_1 = 1 - ((n1y/n1)^2 + (n1n/n1)^2)
  gini_2 = 1 - ((n2y/n2)^2 + (n2n/n2)^2)
  # index will be deduct one because of the class column
  gini_results[i] = (n1/nr)*gini_1 + (n2/nr)*gini_2
  }
}
nmin <- which.min(gini_results) # index will be increase one because of the class column
point <- split_point[nmin]
print(paste(c("The smallest optimal split point is ", point)))
print(paste(c("With the Gini Index equals to ",gini_results[nmin])))
name_graph = "Gini_Index_Optimal"
num = 1
ext = ".pdf"
name_save = paste(name_graph, num, ext, sep = '')
# plot and save the pdf file
pdf(name_save)
plot(x = 1:length(gini_results),gini_results, main = "Optimal Break Point by Gini Index", xlab = "Test Number", ylab = "Gini Index")
dev.off()