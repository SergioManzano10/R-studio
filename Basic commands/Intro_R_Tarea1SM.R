
# PART 1 ------------------------------------------------------------------

# 1. Code to execute a script called “myscript.R”

source("myscript.R")

# 2. Code to assign the value A to a variable x

x<-"A"

# 3. Code to generate a sequence from 7 to 30 with increment 3

seq(from=7, to=30, by=3)

# 4. Code to obtain information about function glm

help("glm")

# 5. Code to list all the objects in the current environment

ls()

# 6. Code to remove all objects

rm(list = ls())

# 7. Code to specify the following path to the working directory: C:

setwd(C:/)

# 8. Create a vector x containing the numbers 1, 2, 1, 1, 1, 2
x<-c(1, 2, 1, 1, 1, 2)

# 9. Create a vector y containing the words yes, no, no, yes, no
y<-c("yes", "no", "no", "yes", "no")


# 10. Compute the number of elements in vector y
length(y)

# 11. Code to obtain the sequence of integer numbers from 10 to 25
seq(10,25)

# 12. Use the function rep() to generate the sequence 1, 2, 1, 2, 1, 2

rep(c("1","2"),3)

# 13. Code to generate the sequence 1, 1, 1, 2, 2, 2

rep(c("1","2"), each=3)

# 14. Code to generate a sequence containing 7 yes and 5 no
vec1<-seq(from=10, to=120, by=10)
vec1>50

# 15. Code to obtain the sequence 40, 35, 30, 25, 20, 15, 10

vec2<-seq(from=10,to=40, by=5) 
sort(vec2, decreasing=T)



# PART 2 ------------------------------------------------------------------

# 1. Input the text file using read.table, assigning the input to a variable pdata.

pData<-read.table("C:/Users/smanz/OneDrive/Escritorio/OMIC_DATA_ANALYSIS/BIOINFROMATICS/TEMA_1/Tareas/Tarea1/pData.txt", sep=" ", header=T)
pData
View(pData)

# 2.Find the class of the variables pheno and sex. Convert them into factors using as.factor.

is.numeric(pData$pheno)
is.character(pData$sex)
class(pData$pheno)
class(pData$sex)


phenoData<-pData$pheno
factorpheno<-as.factor(phenoData)
is.factor(factorpheno)

sexData<-pData$sex
factorsex<-as.factor(sexData)
is.factor(factorsex)

# 3.Show the 10 first values of the variable “age”

pData[(1:10),"age"]
pData[(1:10),"3"]

# 4.Repeat the previous values, each 3 times.

repetition <- pData[(1:10),"age"]
repeated<-rep(repetition, each=3)
repeated

# 5.Create a new data.frame “pdata_subset” containing the first 20 rows.
newrows<-pData[1:20,]
View(newrows)
pdata_subset<-data.frame(newrows)

# 6.Add in the previous dataset a new column of random values “Values”, that goes from 0.05 to 0.95.
View(pdata_subset)
littleval<-seq(from=0.05, to=0.95, by=0.025)
littleval

randomvalue<-sample(littleval, size=20)

pdata_subset$Values<-randomvalue
pdata_subset
View(pdata_subset)

# 7.Create a new matrix “Data_matrix” containing the information of the variables “X1”, “X3” and “X5”, uniquely for the last 10 observations of the original “pdata” dataset.

View(pData)

pData10<-tail(pData, n=10) 
View(pData10)


Data_matrix<-matrix(c(pData10$X1,pData10$X3, pData10$X5), nrow=10, ncol=3)
View(Data_matrix)

colnames(Data_matrix)<-c("X1", "X3", "X5")
rownames(Data_matrix)<-c(991:1000)

# 8.Print a sentence indicating the dimensions (rows and columns) of the matrix. Use the function print, paste, nrow and ncol to do so. The sentence should be “The matrix has XXXX rows and XXX columns”.

rows1<-nrow(Data_matrix)
cols1<-ncol(Data_matrix)

phrase<-paste("The matrix has", rows1, "rows and", cols1, "columns", sep=" ")

print(phrase)



# Comentarios de la profesora ---------------------------------------------

#In part 1 exercise 7, to indicate a path to a working directory you have to use "". So setwd("C:/XX") instead of setwd(C:/).

#In exercises number 12 and 13 values 1 and 2 can be integers (numeric values), there is no need to define them as character by using "".

#Number 14 I see the idea but I was expecting a vector c("yes","no") to be repeated a specific amount of times (example: c(rep("yes",7),rep("no",5)))

#In part 2 exercise 2, when you want to change the factor of a variable you should do it replacing the existing variable, not creating a new one. 

#So pdata$pheno<- as.factor(pdata$pheno) to directly modifiy it in the dataset.

#In exercise 3 pData[(1:10),"age"] is correct, pData[(1:10),"3"] isn't. If you want to use the position just indicate the number 3 without "".
