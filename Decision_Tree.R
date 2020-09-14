setwd("...")
library(rpart)
library(readr)
wine_train <- read_csv("winedata.csv")
wine_test <- read_csv("winedata_test.csv")
fit_train <- rpart(wine_train$c~. ,method="class", data=wine_train)
name1 = "decisionTree_wine_train"
num = 1
ext = ".pdf"
name2 = paste(name1, num, ext, sep = '')
# plot and save the pdf file
pdf(name2)
plot(fit_train, uniform = T,main = "Decision Tree for Wine Data Train")
text(fit_train, use.n = T, all = T, cex = 0.6)
dev.off()
#predict
pred <- predict(fit_train,wine_test[,-1], type = "class")
# table
t <- table(wine_test$c,pred)
results <- data.frame(wine_test$c,pred)
colnames(results) <- c("actual","predict")
# Calculate Accuracy
n4<-length(results$actual)
#class_1_pred_class_1_act
c1p_c1a <- 0
#class_1_pred_class_2_act
c1p_c2a <- 0
#class_1_pred_class_3_act
c1p_c3a <- 0
#class_2_pred_class_2_act
c2p_c2a <- 0
#class_2_pred_class_2_act
c2p_c1a <- 0
#class_2_pred_class_3_act
c2p_c3a <- 0
#class_3_pred_class_3_act
c3p_c3a <- 0
#class_3_pred_class_1_act
c3p_c1a <- 0
#class_3_pred_class_2_act
c3p_c2a <- 0
for (i in 1:n4){
  c1 <- results$actual
  c2 <- results$predict
  # acutal 1
  if (c1[i] == 1){
    #predict 1
    if (c2[i] == 1){
      c1p_c1a = c1p_c1a + 1
      #predict 2
    } else if (c2[i] == 2){
      c2p_c1a = c2p_c1a + 1
      #predict 3
    } else{
      c3p_c1a = c3p_c1a + 1
    }
    # actual 2
  } else if(c1[i] == 2){
    #predict 1
    if(c2[i] == 1){
      c1p_c2a = c1p_c2a + 1
      #predict 2
    }else if (c2[i] == 2){
      c2p_c2a = c2p_c2a + 1
      #predict 3
    }else{
      c3p_c2a = c3p_c2a + 1
    }
    # actual 3
  } else{
    #predict 1
    if(c2[i] == 1){
      c1p_c3a = c1p_c3a + 1
      #predict 2
    }else if (c2[i] == 2){
      c2p_c3a = c2p_c3a + 1
      #predict 3
    }else{
      c3p_c3a = c3p_c3a + 1
    }
  }
}
# True Positive
TP <- c1p_c1a + c2p_c2a + c3p_c3a
# Class 1 actual
c1a <- c1p_c1a+c2p_c1a+c3p_c1a
# Class 2 actual
c2a <- c1p_c2a+c2p_c2a+c3p_c2a
# Class 3 actual
c3a <- c1p_c3a+c2p_c3a+c3p_c3a
#calculate overall accuracy
OA <- TP/n4
#calculate acc. of class 1
if (c1a==0){A1=0}
A1 <- c1p_c1a/c1a
#calculate acc. of class 2
if (c2a==0){A2=0}
A2 <- c2p_c2a/c2a
#calculate acc. of class 3
if (c3a==0){AP=0}
A3 <- c3p_c3a/c3a
#Calculate error
#Error
OE <- 1-OA
#Error c1
E1 <- 1-A1
#Error c2
E2 <- 1-A2
#Error c3
E3 <- 1-A3
accuracy <- data.frame(OA, A1, A2, A3)
error.c <- data.frame(OE, E1, E2, E3)
colnames(accuracy) <- c ("Overall", "Class 1", "Class 2", "Class 3")
colnames(error.c) <- c ("Overall", "Class 1", "Class 2", "Class 3")
write.table(accuracy, file = "accuracy.cvs", append = F,quote = F, sep = ",",eol = "\n", row.names = F, col.names = T)
write.table(error.c, file = "error.cvs", append = F,quote = F, sep = ",",eol = "\n", row.names = F, col.names = T)
