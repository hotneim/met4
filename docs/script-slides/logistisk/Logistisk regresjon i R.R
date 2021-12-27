
# Data
library(ISLR)       

# Se på dataene
head(Default)  

# Ingen omkoding trengs:
str(Default$default)

# trening og testsett
nrow(Default)
train <- Default[1:7000, ]
test <- Default[7001:1000, ]

# modell
model1 <- glm(default ~ balance, data = train,
              family = "binomial")
summary(model1)
exp(coef(model1))
1.055^50

# prediksjon
to_personer <- data.frame(balance = c(1000, 2000))
pred <- predict(model1, newdata = to_personer, type = "response")
klass <- ifelse(pred > 0.5, "Yes", "No")

#--------- test av klassifiseringsevne

sann <- test$default                                              # Den sanne verdien i testdataene
pred_test <- predict(model1, newdata = test, type = "response")   # Predikert sannsynlighet
klassifisering <- ifelse(pred_test > 0.5, "Yes", "No")            # Klassifisering av kundene
table(sann, klassifisering)                                       # Kontigenstabell


