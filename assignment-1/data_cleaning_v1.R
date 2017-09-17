install.packages('mice')
library(mice)
install.packages('Hmisc')
library(Hmisc)
install.packages('xlsx')
library(xlsx)

#Cleaning the environment and garbage collection
rm(list = ls())
gc()

#Importing the dataset
dataset = read.csv('ToyotaCorolla_new.csv', header = TRUE, sep = ',', na.strings = c("", "NA"))

#Viewing and understanding the structure of the dataset
View(dataset)
str(dataset)
summary(dataset)

#Check for missing values
colSums(is.na(dataset))
md.pattern(dataset)

#Handling missing values

#Deleting columns with too many missing values
column_id = which(colnames(dataset) %in% c('KM', 'Fuel_Type', 'Radio_cassette'))
newdataset = dataset[,-column_id]
dim(newdataset)
View(newdataset)
colSums(is.na(newdataset))

#Handling missing values for column weight
newdataset$Weight[is.na(newdataset$Weight)] <- mean(newdataset$Weight, na.rm = TRUE)

#Handling missing values for column cylinder
newdataset$Cylinders[is.na(newdataset$Cylinders)] <- 4
summary(newdataset)
str(newdataset)

#Changing column to factor which are labelled as integer
newdataset$Met_Color <- as.factor(newdataset$Met_Color)
newdataset$Color <- as.factor(newdataset$Color)
newdataset$Automatic <- as.factor(newdataset$Automatic)
newdataset$Mfr_Guarantee <- as.factor(newdataset$Mfr_Guarantee)
newdataset$BOVAG_Guarantee <- as.factor(newdataset$BOVAG_Guarantee)
col_id <- (16:32)
newdataset[col_id] <- lapply(newdataset[col_id], factor)
str(newdataset)

#Checking rows with NAs
row_id_na <- which(!complete.cases(newdataset))
row_id_na
#Removing rows with missing values
dataset_no_na <- newdataset[-row_id_na,]

#Viewing the summary, strucutre of the new dataset with no NA values
View(dataset_no_na)
summary(dataset_no_na)
str(dataset_no_na)

#Creating dummy variables for categorical variable
table(dataset_no_na$Color)
dataset_no_na$black = dataset_no_na$blue = dataset_no_na$grey = dataset_no_na$red = dataset_no_na$othercolor = 0

dataset_no_na$black[which(dataset_no_na$Color == 'Black')] = 1
dataset_no_na$blue[which(dataset_no_na$Color == 'Blue')] = 1
dataset_no_na$grey[which(dataset_no_na$Color == 'Grey')] = 1
dataset_no_na$red[which(dataset_no_na$Color == 'Red')] = 1
dataset_no_na$othercolor[which(dataset_no_na$Color %in% c('Beige', 'Silver', 'Violet', 'White', 'Yellow'))] = 1

table(dataset_no_na$black)
table(dataset_no_na$red)
table(dataset_no_na$othercolor)

#Checking continuous variables
boxplot(dataset_no_na$Price)
hist(dataset_no_na$Price)
boxplot(dataset_no_na$CC)
hist(dataset_no_na$CC)

#Handling Outlier in column CC

summary(dataset_no_na$CC)
dataset_no_na[which(dataset_no_na$CC == '16000'), ] #Finds the row where the outlier is present
boxplot(dataset_no_na$CC)
hist(dataset_no_na$CC)
final_data <- subset(dataset_no_na, CC != 16000)
boxplot(final_data$CC)
hist(final_data$CC)

#Viewing and summarizing the final updated data after removing missing values, categorization and removing outlier from CC column
View(final_data)
str(final_data)
summary(final_data)

#Exporting the data to Excel File

write.xlsx(final_data, "ToyotaCorolla_dataclean.xlsx")
