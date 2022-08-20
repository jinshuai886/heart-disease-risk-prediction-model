
###### heart disease risk prediction model ######

library(data.table)
data <- read.table(file = "processed.cleveland.data", sep = ",")
str(data)

names(data)<-c("age","sex","cp","trestbps","chol",
               "fbs","restecg","thalach","exang","oldpeak",
               "slope","ca","thal","num")
str(data)

data$sex <- as.factor(data$sex)
data$cp <- as.factor(data$cp)
data$fbs <- as.factor(data$fbs)
data$restecg <- as.factor(data$restecg)
data$exang <- as.factor(data$exang)
data$slope <- as.factor(data$slope)
data$thal <- as.factor(data$thal)
data$ca <- as.numeric(data$ca)

table(data$thal)
table(data$num)

data$thal <- ifelse(data$thal=="?", 2.0, data$thal)
table(data$thal)

data$num <- ifelse(data$num==0, "no", "yes")
table(data$num)

sum(is.na(data$ca))
data$ca[is.na(data$ca)] <- 0
sum(is.na(data$ca))

library(VIM)
aggr_plot <- aggr(data, col=c('navyblue','red'), numbers=TRUE, 
                  labels=names(data), cex.axis=.7, gap=3, 
                  ylab=c("Histogram  of missing data","Pattern"))



library(caret)
fitControl <- trainControl(method = "none", classProbs = TRUE)

set.seed(123456)
RF_model <- train(num~age+sex+cp+trestbps+chol+fbs+
                      restecg+thalach+exang+oldpeak+
                      slope+ca+thal, 
                  data=data,
                  method = "parRF", 
                  trControl = fitControl, 
                  verbose = FALSE, 
                  tuneGrid = data.frame(.mtry=3),
                  metric = "ROC")

SVM_model <- train(num~age+sex+cp+trestbps+chol+fbs+
                     restecg+thalach+exang+oldpeak+
                     slope+ca+thal, 
                   data=data,
                   method = "svmRadial", 
                   trControl = fitControl, 
                   verbose = FALSE, 
                   tuneGrid = data.frame(sigma = 0.05,C= 0.5),
                   metric = "ROC")

library("randomForest")
library("DALEX")
explain_rf <- DALEX::explain(model = RF_model,  
                             data = data[, -14],
                             y = data$num== "yes", 
                             label = "Random Forest")

explain_svm <- DALEX::explain(model = SVM_model,  
                             data = data[, -14],
                             y = data$num== "yes", 
                             label = "Support Vector Machine")

predict(explain_rf, data[1,-14])
predict(explain_svm, data[1,-14])

shap_1_rf <- predict_parts(explainer = explain_rf, 
                            new_observation = data[1,-14], 
                            type = "shap",
                            B = 25)

shap_1_svm <- predict_parts(explainer = explain_svm, 
                           new_observation = data[1,-14], 
                           type = "shap",
                           B = 25)

shap_1_rf

shap_1_svm

A <- plot(shap_1_rf)
B <- plot(shap_1_svm)

library(ggpubr)
ggarrange(A,
          B,
          ncol = 2, nrow = 1)
ggsave("SHAP explanation.jpeg", 
       width = 24, height = 12, 
       dpi = 800, units = "cm", device='jpeg')



set.seed(1)
library("DALEXtra")
library("lime")

model_type.dalex_explainer <- DALEXtra::model_type.dalex_explainer
predict_model.dalex_explainer <- DALEXtra::predict_model.dalex_explainer

lime_1_rf <- predict_surrogate(explainer = explain_rf, 
                                 new_observation = data[1,-14], 
                                 n_features = 13, 
                                 n_permutations = 1000,
                                 type = "lime")

lime_1_svm <- predict_surrogate(explainer = explain_svm, 
                               new_observation = data[1,-14], 
                               n_features = 13, 
                               n_permutations = 1000,
                               type = "lime")

C <- plot(lime_1_rf)
D <- plot(lime_1_svm)

library(ggpubr)
ggarrange(C,
          D,
          ncol = 2, nrow = 1)
ggsave("LIME explanation.jpeg", 
       width = 24, height = 12, 
       dpi = 800, units = "cm", device='jpeg')


vim_rf <- model_parts(explainer = explain_rf)
vim_svm <- model_parts(explainer = explain_svm)

E <- plot(vim_rf)
F <- plot(vim_svm)

library(ggpubr)
ggarrange(E,
          F,
          ncol = 2, nrow = 1)
ggsave("VIM explanation.jpeg", 
       width = 24, height = 12, 
       dpi = 800, units = "cm", device='jpeg')


