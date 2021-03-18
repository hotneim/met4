
# Dataene får vi tak i via pakken ISLR
library(ISLR)       
head(Default)       

# Trenger denne pakken for KNN:
# install.packages("caret")
library(caret)

# Hvis vi vil sette k selv
model1 <- train(default ~ balance,
                data = Default,
                method = "knn",
                tuneGrid = data.frame(k = 50))


# R-kode dersom vi vil velge k automatisk med kryssvalidering
trControl <- trainControl(method  = "cv", # 5-fold kryssvalidering
                          number  = 5)

# Tilpasser modellen
model2 <- train(default ~ balance,
                data = Default,
                method = "knn",
                trControl  = trControl,
                metric     = "Accuracy")

# Hvilken k valgte kryssvalideringen?
k <- model2$finalModel$k
k

# prediksjon av to kunder 
to_kunder <- data.frame(balance = c(1500,500))
predict(model2, newdata = to_kunder)

