
# Part I ------------------------------------------------------------------

#Considering the following vector:
  
vec<-c("x^4", "y^5", "J**2", "3.5", "4200234")
vec
  
#a.	Write a code to replace all ^ with **

vec_rpl<-gsub("\\^","**",vec) #\\^ encuentra todos los elementos que tienen "^"; hay que ponerlo porque es un símbolo especial
vec_rpl

#b.	Write a code to replace any upper case to the lower case (cambiar de mayúsculas a minusculas)

vec_tl<-tolower(vec)
vec_tl


#c.	Write a code to count the number of digits in each element of the vector ‘vec’. Hint: To make it easier to count, start by either selecting all the digits or removing any non-digit characters.

    ##Entendiendo dígitos como números

digits<-gsub("[A-z]", "", vec) 
digits2<-gsub("\\*", "", digits)
digits3<-gsub("\\.", "", digits2)

counted<-nchar(digits3)
counted

#d.	Write a code to extract only the numeric values from the vector ‘vec’. Transform them to a numeric vector.

  ##Entendiendo como valores numéricos unicamente aquellos que son todo números (sin letras)

vec_extracted<-as.numeric(gsub("^[0-9]$", "", vec)) #Con esto indico que quiero que empiece (^) y acabe ($) por un dígito numérico
vec_numeric<-vec_extracted[!is.na(vec_extracted)]
vec_numeric2<-as.numeric(vec_numeric)


#e.	Write a code to create a logical vector that checks if each element of ‘vec’ is numeric (i.e., only contains digits). The result should be a vector of TRUE and FALSE values.

log_vec<-grepl("[0-9]+.*[0-9]", vec) 
log_vec

#f.	Write a code to create a new vector that contains only the elements of ‘vec’ that include the ** symbol.


help("grep") #If TRUE, pattern is a string to be matched as is.

vec_astherics<-grep("**", vec, value=T, fixed=T) #La opción "fixed=T" sirve para que los caracteres especiales los considere como texto literalmente y no como caracteres especiales
                                                 #Si intento seleccionarlos con "+" me da error
vec_astherics



#g.	Write a code to calculate the mean of the numeric elements in the vector ‘vec’. Ignore non-numeric elements.

vec

numeric_elements_ch<-grep("[0-9]+.*[0-9]",vec, value=T) #Con esto indico que quiero un número ([0-9]) seguido obligatoriamente (+) de un punto (.) o (*) otro número ([0-9])
numeric_elements_num<-as.numeric(numeric_elements_ch)

mean(numeric_elements_num)


#h.	Write a code to create a logical vector that checks if each element of ‘vec’ contains the letter “y”. The result should be a vector of TRUE and FALSE values.
  
log_vec<-grepl("y", vec) #grepl busca directamente ese patrón, sin poner * ya que al poner una letra y * reconoce todos los elementos
log_vec





# Part II -----------------------------------------------------------------


#Step 1. Install and load the R package readxl from CRAN.

install.packages("openxl")
install.packages("readxl")
library(readxl)

getwd()

#Step 2. Import the tables WAI_GeneExpression.xlsx and WAI_Phenotypic.xlsx into R.

gene_expression<-read_xlsx("WAI_GeneExpression.xlsx", col_names=T) #La forma de leer un excel es diferente de la forma de leer un archivo de texto
phenotipic<-read_xlsx("WAI_Phenotypic.xlsx", col_names=T)


#Step 3. Check these datasets and rename the individuals’ column to “ID” in both datasets.

View(gene_expression)
View(phenotipic)


colnames(gene_expression)[1]<-"ID"
View(gene_expression)

colnames(phenotipic)[1]<-"ID"
View(phenotipic)



#Step 4. Determine how many individuals and genes are included in the WAI_GeneExpression.xlsx dataset. Hint: use of the grep() function to make your code more efficient and concise.
help(grep)

count1<-grep("Individual", gene_expression$ID, value=F)
length(count1)

colnames_ge<-colnames(gene_expression) #Estos comandos no responden a la pregunta, es para ordenar la tabla
colnames_ge #Estos comandos no responden a la pregunta, es para ordenar la tabla

count2<-grep("Gene", colnames_ge, value=F)
length(count2)

#Step 5. Modify the column names for genes, changing the format from “Gene.X” to “Gene_X”. Hint: Use of gsub() function.
help(gsub)

gene_expression_modified<-gsub("Gene.", "Gene_", colnames(gene_expression))
table(gene_expression_modified)


colnames(gene_expression)<-gene_expression_modified
View(gene_expression)




#Step 6. Merge the two tables based on common ID field and store the result in a new object.

data_merged<-merge(phenotipic,gene_expression, by="ID", all=T) #Se desordenan por el orden del segundo número dentro de cada individuo
View(data_merged)



#Step 7. Create a new dataset named “dis” containing all individuals with values greater than 8 in ‘Gene_101’ column and ‘Disease’ in ‘DiseaseStatus’ column. Determine how many individuals remain in this new dataset.

dis<-data_merged[which(data_merged$Gene_101 >8 & data_merged$DiseaseStatus == "Disease"),]
View(dis)

length(dis$ID) #40 remaining individuals



#Step 8. Calculate the mean and standard deviation of ‘Gene_500’ column

Gene_500_column<-data_merged$Gene_500


mean(Gene_500_column)
sd(Gene_500_column)

#Step 9. Generate a boxplot to compare the expression of ‘Gene_500’ by sex within the “dis” dataset. Customize your plot to your preference and save it as a PDF file.

View(dis)

expression500<-dis$Gene_500
sex500<-dis$Sex

boxplot(expression500 ~ sex500, xlab= "Sex", ylab="Gene Expression", main="Gene 500", col=c("red", "green")) #El símbolo "~" me permite clasificar la expresión del gen en función del sexo


      ################Para guardar archivos en pdf hay que usar la función que me crea un sitio para guardarlo, el gráfico que quiero que me guarde dentro y lafunción que cierra e pdf################
pdf("Gráfico Step 9")
boxplot(expression500 ~ sex500, xlab= "Sex", ylab="Gene Expression", main="Gene 500", col=c("red", "green")) #El símbolo "~" me permite clasificar la expresión del gen en función del sexo
dev.off()



#Step 10. Write the ‘dis’ data frame into both a tab-delimited text file and Excel file.

View(dis)

write.table(dis, "dis_data.txt", sep = "\t", row.names = FALSE) #Tab-delimited #row.names=F sirve para que no me ponga los números de fila que pone R por defecto

install.packages("openxlsx") #Sin instalar y cargar la librería no funciona el comando write.xlsx
library(openxlsx)
write.xlsx(dis, "dis_data.xlsx", rowNames = FALSE) #Excel-delimited




#Step 11. Write all commands from this exercise into an R script named “exerciseRbasics.R”. Execute the script with the source function as follows:
#This will run all commands of this exercise and generate the corresponding output files in the current working directory.

source("C:/Users/smanz/OneDrive/Escritorio/OMIC_DATA_ANALYSIS/BIOINFROMATICS/TEMA_2/Tareas/ExerciseRbasics_Tarea2_SergioManzano.R")



# Part III ----------------------------------------------------------------

#Create the following plots with R using data from “SNPdataset.txt”:

SNPdataset<-read.table("SNPdataset.txt", header = T)
View(SNPdataset)

#1. Histogram

hist(SNPdataset$GENE_EXPRESSION, ncol=12, col=c("red", "yellow", "limegreen","blue","magenta"), main = expression(italic("Colored histogram")), ylab="", xlab="Gene expression")

#2. Second graph

type_CC<-SNPdataset[which(SNPdataset$SNP1=="CC"),]
type_CT<-SNPdataset[which(SNPdataset$SNP1=="CT"),]
type_TT<-SNPdataset[which(SNPdataset$SNP1=="TT"),]

type_CC$gender_CC_if<-ifelse(type_CC$GENDER=="Male", "1", "2")
type_CT$gender_CT_if<-ifelse(type_CT$GENDER=="Male", "1", "2")
type_TT$gender_TT_if<-ifelse(type_TT$GENDER=="Male", "1", "2")

gender_CC_numeric<-as.numeric(type_CC$gender_CC_if)
sum(type_CC$gender_CC_if==1)
sum(type_CC$gender_CC_if==2)

gender_CT_numeric<-as.numeric(type_CT$gender_CT_if)
sum(type_CT$gender_CT_if==1)
sum(type_CT$gender_CT_if==2)

gender_TT_numeric<-as.numeric(type_TT$gender_TT_if)
sum(type_TT$gender_TT_if==1)
sum(type_TT$gender_TT_if==2)


matrix_data<-matrix(c(8, 4, 27, 26, 40, 52), nrow = 2, ncol=3)
row.names(matrix_data)<-c("Male", "Female")
colnames(matrix_data)<-c("CC", "CT","TT")
matrix_data

barplot(matrix_data, col=c("#00CD00", "#00B2EE"), ylim=c(0,100), 
        names.arg = c("CC", "CT", "TT"), xlab="SNP1", ylab="GENDER")

legend(x=0.3, y=100, legend = c("Female", "Male"), 
       pch = c(22,22), col = c("#00CD00", "#00B2EE"),
       title = "", bg = "white", bty = "y")





#2. Third graph

gene_expression<-SNPdataset$GENE_EXPRESSION
snpdata_Gender<-SNPdataset$GENDER

boxplot(gene_expression~snpdata_Gender, ylim=c(0,125000), col=c("mediumblue", "cyan"), xlab="snpdata$GENDER", ylab="x")

#4. Fourth graph

type_CC<-SNPdataset[which(SNPdataset$SNP1=="CC"),]
expression_CC<-type_CC$GENE_EXPRESSION
age_CC<-type_CC$AGE

type_CT<-SNPdataset[which(SNPdataset$SNP1=="CT"),]
expression_CT<-type_CT$GENE_EXPRESSION
age_CT<-type_CT$AGE

type_TT<-SNPdataset[which(SNPdataset$SNP1=="TT"),]
expression_TT<-type_TT$GENE_EXPRESSION
age_TT<-type_TT$AGE



plot(c(expression_CC,expression_CT,expression_TT), c(age_CC, age_CT,age_TT), col=c("black","#8B2323","#66CD00"),
     ylim=c(0,80), ylab="Age", xlab="Gene expression", xlim=c(0,130000), abline(lm(SNPdataset$AGE~SNPdataset$GENE_EXPRESSION))) #el comando abline es para añadir la recta de regresión

legend(x=90000, y=80, legend = c("SNP1=CC", "SNP1=CT", "SNP1=TT"), 
       pch = c(1,1,1), col = c("black", "#8B2323", "#66CD00"),
       title = "", bg = "white", bty = "n")




# Comentarios de la profesora ---------------------------------------------

#Great job, and it's excellent that you've added comments to clarify how you interpret the questions. In general, I consider multiple interpretations valid, but I appreciate your clarification. Keep up the good work! Here are some tips for generating the plots (I know we didn't cover this in class, but it will be useful for you in the future):

#- For the barplot, you don't need to create subsets for each genotype, distinguishing between men and women (although it's correct). To simplify, you can pass a table with the two character vectors (genotypes and gender) to "barplot," and it will automatically count them:

#>barplot(table(SNPdataset$GENDER,SNPdataset$SNP1), col=c(3,4), xlab="SNP1", ylab="GENDER")

#>legend(0.2,90,c("Female","Male"),col=c(3,4),pch=c(0,0), cex=0.7)

#- For the scatter plot, you don't need to create subsets for each gene expression associated with each genotype and age for each individual. You can create a graph with two coordinates, x and y, where 'x = Gene Expression' and 'y = Age', and then color each point based on the genotypes stored in the "SNP1" column:
                                                                                                               
                                                                                                               #>plot(x= SNPdataset$GENE_EXPRESSION,y = SNPdataset$AGE, xlab="Gene expression", ylim=c(20,75), ylab="Age", col=as.factor(snp$SNP1))
                                                                                                             
                                                                                                             #>abline(lm(SNPdataset$AGE~SNPdataset$GENE_EXPRESSION))
                                                                                                             
                                                                                                             #>legend(90000,78,c("SNP1=CC","SNP1=CT","SNP1=TT"),col=c(1,2,3),pch=c(1,1), cex=0.7,  bty = "n")

