library(readr)
library(caTools)
library(ggplot2)
library(scales)
library(e1071)
library(caret)
library(rpart)
library(class)

adult1<-read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data", header = FALSE)
adult2<-read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.test",header = FALSE, skip=1)
adult<-rbind(adult1,adult2)


names(adult)<-c("Age","WorkClass","Fnlwgt","Education","EducationNum","MaritalStatus","Ocupation","Relationship","Race","Sex","CapitalGain","CapitalLoss","HoursPerWeek","NativeCountry","Income")


adult$Income[adult$Income==' <=50K.']<-' <=50K'
adult$Income[adult$Income==' >50K.']<-' >50K'
adult$Income[adult$Income==' <=50K']<-0
adult$Income[adult$Income==' >50K']<-1
adult$Income=as.numeric(as.character(adult$Income))

adult$Sex[adult$Sex ==' Male'] <- 0
adult$Sex[adult$Sex ==' Female'] <- 1
adult$Sex=as.numeric(as.character(adult$Sex))



Americadelnorte<-c(" Canada", " Cuba", " Dominican-Republic", " El-Salvador", " Guatemala"," Haiti", " Honduras", " Jamaica", " Mexico", " Nicaragua"," Outlying-US(Guam-USVI-etc)", " Puerto-Rico", " Trinadad&Tobago"," United-States")
Otro<-c(" Cambodia", " China", " Hong", " India", " Iran", " Japan", " Laos"," Philippines", " Taiwan", " Thailand", " Vietnam"," Columbia", " Ecuador", " Peru"," England", " France", " Germany", " Greece", " Holand-Netherlands"," Hungary", " Ireland", " Italy", " Poland", " Portugal", " Scotland"," Yugoslavia"," South"," ?")
adult$NativeCountry[adult$NativeCountry %in% Americadelnorte]<-"America del norte"
adult$NativeCountry[adult$NativeCountry %in% Otro]<-" ?"

adult[adult == ' ?'] <- NA
adult <- na.omit(adult)

##FUNCION MATRICES DE CONFUSIÓN

ggplotConfusionMatrix <- function(m){
  mytitle <- paste("Precisión", percent_format()(m$overall[1]),
                   "Kappa", percent_format()(m$overall[2]))
  dat <- as.data.frame(m$table)
  dat$lab <- ifelse(dat$Freq == 0, '', dat$Freq)
  p <-
    ggplot(data = dat ,
           aes(x = actual, y = predicted)) +
    geom_tile(aes(fill = log(Freq)), colour = "white") +
    scale_fill_gradient(low = "white", high = "steelblue") +
    geom_text(aes(x = actual, y = predicted, label = lab)) +
    theme(legend.position = "none") +
    ggtitle(mytitle)
  return(p)
}


#NAIVE BAYES 
set.seed(1)
data<-adult
random<-sample(1:nrow(data), 0.9*nrow(data))
train<-data[random,]
test<-data[-random,]
modelo_nb<-naiveBayes(train$Income~., data=train)
pred_nb<-predict(modelo_nb,test)
cm_nb<-confusionMatrix(pred_nb, as.factor(test$Income), dnn=c("predicted","actual"))

ggplotConfusionMatrix(cm_nb)

##ARBOL DE DECISIÓN (usando los train y test de antes)


modelo_ad<-rpart(as.factor(train$Income) ~., data=train)
pred_ad<-predict(modelo_ad, test, type="class")
cm_ad<- confusionMatrix(pred_ad, as.factor(test$Income), dnn=c("predicted","actual"))
ggplotConfusionMatrix(cm_ad)

##KNN (usando los train y test de antes, pero solo variables numéricas)

modelo_knn<-knn ( train = cbind(train$Age,train$Sex,train$CapitalGain) , test = cbind (test$Age,test$Sex,test$CapitalGain) ,cl = train$Income ,k =7)
cm_knn<-confusionMatrix(modelo_knn,as.factor(test$Income), dnn=c("predicted","actual"))
ggplotConfusionMatrix(cm_knn)



##REGRESION
amodel<-train(adult$Income ~ adult$Age+ adult$Sex + adult$CapitalGain, data=train, method="glm", family="binomial")

amodel<-train(train$Income ~ train$Age+ train$Sex + train$CapitalGain, data=train, method="glm", familia's="binomial")
modelo_rl<-glm( train$Income ~ train$Age+train$Sex+train$CapitalGain, family=binomial(link="logit"))
pred_rl<-predict(modelo_reg, newdata=data.frame(train$Age,train$Sex,train$CapitalGain), type="response")
cm_rl<-confusionMatrix(modelo_rl,as.factor(test$Income), dnn=c("predicted","actual"))
#CURVA ROC

labels <- adult$Income
scores <- predict(modelo,x)
plot(roc(labels, scores, direction="<"),
     col="yellow", lwd=3, main="Curva ROC")


