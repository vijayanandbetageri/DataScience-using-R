View(Zoo)
# drop the id feature
Zoo <- Zoo[-1]
View(Zoo)

# table of diagnosis
table(Zoo$type)

str(Zoo)
# recode diagnosis as a factor
Zoo$type <- factor(Zoo$type, levels = c("1", "2","3","4","5","6","7"),
                         labels = c("Type1", "Type2", "Type3", "Type4", "Type5", "Type6", "Type7"))
View(Zoo)
str(Zoo)

# table or proportions with more informative labels
round(prop.table(table(Zoo$type)) * 100, digits = 1)

# summarize any three numeric features
summary(Zoo[c("animal.name", "type")])

# create normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# test normalization function - result should be identical
#normalize(c(0.01, 0.02, 0.03, 0.04, 0.05))
#normalize(c(10, 20, 30, 40, 50))

# normalize the wbcd data
Zoo_n <- as.data.frame(lapply(Zoo[2:17], normalize))
View(Zoo_n)

# confirm that normalization worked
summary(Zoo_n$Type)

# create training and test data
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]

# create labels for training and test data

wbcd_train_labels <- wbcd[1:469, 1]
wbcd_train_labels <- wbcd_train_labels[["diagnosis"]] 

wbcd_test_labels <- wbcd[470:569, 1]
wbcd_test_labels <- wbcd_test_labels[["diagnosis"]]
#---- Training a model on the data ----

# load the "class" library
install.packages("class")
library(class)
?knn

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k=21)
wbcd_test_pred



##--------Evaluating model performance ----

# load the "gmodels" library
library(gmodels)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq=FALSE)











## Improving model performance ----

# use the scale() function to z-score standardize a data frame
wbcd_z <- as.data.frame(scale(wbcd[-1]))
View(wbcd_z)

# confirm that the transformation was applied correctly
summary(wbcd_z$area_mean)

# create training and test datasets
wbcd_train <- wbcd_z[1:469, ]
wbcd_test <- wbcd_z[470:569, ]


# re-classify test cases
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k=10)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq=FALSE)

# try several different values of k
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=1)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=5)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=11)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=15)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=27)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)
