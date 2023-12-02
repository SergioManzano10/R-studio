#1 Exercises Basic Functions and Programming

# Exercises I -------------------------------------------------------------


#31.1 Exercises I
#Write a function, named function1, with three parameters (p,alpha and x) that computes the following expression:
  
#Q<-x*(1−p)^(−1/α−1)

  function1<- function (x,p,a) {
    Q <- x*(1-p)^((-1)/(a-1))
    return(Q)
  }
  
  function1(3,2,2)

#Write a function, named function2, with an additional parameter (lower.tail). When lower.tail=TRUE, function2=function1; 
#but if lower.tail=FALSE, function2 should first obtain the complement of p (that is p <- 1-p) and then compute Q(p,a,x). 
#In the parameters definition of function2 set by default lower.tail=TRUE.
  
function2<- function(p, a, x, lower.tail = TRUE){ #Aquí tengo que poner lower.tail=TRUE por la frase del enunciado que dice: In the parameters definition of function2 set by default lower.tail=TRUE.
  if(lower.tail==TRUE){
    Q <- x*(1-p)^((-1)/(a-1))
    return(Q)} else{
      p <- 1-p
      Q <- x*(1-p)^((-1)/(a-1))
      return(Q)}
  }


print(function2(3,2,2))


  
#Write a function, named function3, 
#equivalent to function2 that calls function1 for performing the computation of Q.

function3<-function(p, a, x, lower.tail = TRUE){ #Aquí tengo que poner lower.tail=TRUE por la frase del enunciado que dice: In the parameters definition of function2 set by default lower.tail=TRUE.
  if(lower.tail==TRUE){
    resp<-function1(x,p,a)
    return(resp)}
  else{
  p <- 1-p
  resp<-function1(x,p,a)
  return(resp)}
}


print(function3(3,2,2))


#Write a function, named function4, equivalent to function3 but with the specification of the parameters range 
#(p between 0 and 1, alpha > 1, x0 > 0). Use R function stopifnot

function4<-function(p, a, x, lower.tail = TRUE){ #Aquí tengo que poner lower.tail=TRUE por la frase del enunciado que dice: In the parameters definition of function2 set by default lower.tail=TRUE.
  stopifnot(p >= 0 & p <= 1, a > 1, x > 0)
   if(lower.tail==TRUE){
    resp<-function1(x,p,a)
    return(resp)}
  else{
  p <- 1-p
  resp<-function1(x,p,a)
  return(resp)}
}

print(function4(0.8,2,0.1)) #Si no se cumple alguna de las condiciones especificadas en stopifnot, me sale el error "Error in function4(1, 0.5, 0.5) : a > 1 is not TRUE" dependiendo del valor que no se cumpla


#Write a function, named function5, with one parameter n (plus other necessary parameters), 
#that calls function4 n times using a for loop, each time taking p as a random value from a uniform distribution 
#between 0 and 1. Function5 should return a vector of size n of values of Q(p,alpha,x0).
set.seed(1234)

function5 <- function(n, a, x, lower.tail=TRUE) { #aquí no incluyo p ya que si no tendría que definirlo cada vez y no sería aleatorio #La expresión 1:n se utiliza en el bucle for para generar una secuencia de números desde 1 hasta n. Esta secuencia se utiliza para controlar cuántas veces se ejecuta el bucle
  vec <- numeric(n) #Creamos un vector llamado vec que almacenará los valores de Q(p) que generaremos (tras las distintas repeticiones "n")
  for (i in 1:n) { #para cada valor aleatorio i generado en la función runif cuando especificamos las diferentes epeticiones #si p fuera un vector con valores definidos pondria for (i in p)
    p <- runif(1,0,1)
    vec[i] <- function4(p, a, x, lower.tail) #El resultado se almacena en la posición i del vector vec.
  }
  return(vec)
}

function5(3,2,2) #dependiedo del número que le ponga a la "n", me devolverá ese número de valores


#Write a function, named function6, equivalent to function5 that avoids the for loop.

function6 <- function(n, a, x, lower.tail = TRUE) {
  set.seed(1234)
  p <- runif(n, 0, 1)  #al poner la n aquí, la coge luego del vector donde pone print y tenemos el mismo resultado ya que indicamos el número de veces que lo hacemos
  new_func <- function4(p, a, x, lower.tail)
  return(new_func)
}

print(function6(3,2,2))

#Write a function, named function7, equivalent to function6 that instead of returning a vector of values of Q(p,alpha,x0) returns the mean and the standard deviation of the Q values.

function7 <- function(n, a, x, lower.tail = TRUE) {
  set.seed(1234)
  p <- runif(n, 0, 1)
  vec_sol <- function4(p, a, x, lower.tail)
  
  mean_val <- mean(vec_sol)        
  sd_val <- sd(vec_sol)            
  
  return= c(mean=mean_val , sd=sd_val)
}

print(function7(3,2,2))

#Write a function, named function8, equivalent to function6 that instead of returning a vector of values of Q(p,alpha,x0) returns the quantile of the Q values for a given probability.

function8 <- function(n, a, x, prob = 0.25, lower.tail = TRUE) {
  set.seed(1234)
  p <- runif(n, 0, 1)
  vec <- function4(p, a, x, lower.tail)
  quantile_val <- quantile(vec, probs = prob)
  return(quantile_val)
}

print(function8(3,2,2))


#Write a function, named function9, equivalent to function6 that instead of returning a vector of values of Q(p,alpha,x0) returns a histogram of the Q values. Let the user to decide the color of the histogram.

function9 <- function(n, a, x, lower.tail = TRUE) {
  set.seed(1234)
  p <- runif(n,0,1)
  vec <- function6(n, a, x, lower.tail)
  histogram <- hist(vec, col = "limegreen", xlab = "Q Values", main = "Histogram of Q Values")
  return(histogram)
}

function9(100,200,120)



# Exercises II ------------------------------------------------------------


#1.2 Exercises II
#These exercises are designed to be solved using functions from the apply() family, unless stated otherwise.

#Import the ‘WA2_OmicsDataExercisesII.txt’ into R

table<-read.table("WA2_OmicsDataExercisesII.txt", header = T)
View(table)

#Step 1: Find the length of dataset1 columns.

apply(table,2,length)

#Step 2: Write a function which computes the mean value in each column of a given matrix or data frame. 
#Find the mean expression value, Fold Change and p-value of ‘WA2_OmicsDataExercisesII.txt’ using this function.

library(dplyr) #Necesaria para usar la función select_if

all_means <- function(table){ #que lo que hay entre paréntesis sea table permite aplicar la función a un conjunto de datos con valores establecidos (no tenemos que dárlos nosotros al final como en otras ocasiones)
  numeric_values <- select_if(table, is.numeric) #la función seleccionará todas las columnas de table que contengan valores numéricos
  means <- apply(numeric_values, 2, mean)
  return(means)
  
}

res <- all_means(table)  #Para aplicar la funcion all_means creada al dataset "table"
res


#apply(table[c("Fold_Change","Expression_Value","p_Value")],2,mean) #es otra forma de hacrlo sin función


#Step 3: Create a matrix containing the the minimum and maximum expression value, Fold Change and p-value for Sample 87.

Sample87<-table[which(table$Sample_ID=="Sample87"),]

min_val<-apply(Sample87[3:5],2,min)
max_val<-apply(Sample87[3:5],2,max)

matriz1<-as.matrix(min_val)
matriz2<-as.matrix(max_val)

matriz_unida <- cbind(matriz1, matriz2)
colnames(matriz_unida)<-c("Min","Max")

matriz_unida

#Step 4: Find the mean expression value for each gene. Hint: explore the use of other functions from the apply() family

?apply
help(tapply)


tapply(table$Expression_Value, table$Gene_Name, mean) #tapply function calculates the mean expression value for each unique gene name in your dataset.



#Step 5: Use a for loop to create a list of gene names corresponding to each unique gene name. Hint: The final list of gene names should have length equal 7.

gene_names_list<-list(table$Gene_Name) #Lo primero es crear una lista con todos los nombres de los genes para luego ir seleccionandoen ella
gene_names<-unlist(gene_names_list, use.names = TRUE) #Para usar esta lista tenemos que usar el comando "unlist" obteniendo así cada valor


unique_gene_names <- list() #Creo una lista vacía llamada unique_gene_names donde se guarden las x con las condiciones establecidas en el "loop for"

for (x in gene_names) { #esto indica que el loop se aplica para todas las x(elementos diferentes) que haya en la lista gene_names
  if (!(x %in% unique_gene_names)) { #Este comando indica que si el nombre encontrado en gene_names no pertenece ya a unique_gene_names, lo añada
    unique_gene_names <- append(unique_gene_names, list(x)) #append sirve para agregar elementos (x de gene names) a un objeto existente  (unique_gene_names), que cumplan la condición establecida en la línea anterior #meto x dentro de lista ya que append solo espera objetos en forma de lista
    print(unique_gene_names)                                #en la línea de arriba, el unique_gene_names izquierdo es el nuevo vector que creo cda vez que añado al unique_gene_names derecho (antiguo) un nombre de la lista
  }
}

length(unique_gene_names)


#Step 6: Repeat the previous step but avoiding the for loop. The result should be a list of 7 elements. Hint: Use functions from the apply() family wisely to avoid nested lists.

?lapply
help("unique")
help("identity")


unique_gene_names_2<-lapply(unique(table$Gene_Name), identity) #lapply returns a list of the same length as X pero con el unique especifico que no los coja repetidos
unique_gene_names_2                                             #con identity se imprime el valor del objeto que se le pasa como argumento
length(unique_gene_names_2)                                   



#Step 7: Write a function with the name avg_gt with two formal arguments: 
#a vector x and a value gt.The functions computes the average of the values greater than gt in x. 
#Write a version with a loop and if and one version without loops and if statements. 
#Calculate the average of the expression values for genes having a expression value 
#greater than 0.25 using the two functions. Which one of them is faster?

######Método 1

avg_gt <- function(x, gt) {
  valid_values <- x[which(x > gt)] #solo usa aquellos valores de x que son mayores que el gt
  if (length(valid_values) == 0) { #esta línea es se utiliza para verificar que no hay valores en el vector valid_values después de aplicar el filtro para seleccionar aquellos que son mayores que el umbral gt.
    return(NA)  # Return NA if there are no valid values
  } else {
    return(mean(valid_values))
  }
}

gene_expression <- table$Expression_Value # Valores de expresión génica

gt <- 0.25 # Threshold/Umbral que tienen que pasar los valores de expresión para que sean contados al hacer la media

result <- avg_gt(gene_expression, gt) # Función para calcular la media para valores que superan un umbral #gene_expression susituye a la x de la función
result

print(paste("Average of values greater than", gt, "is:", result)) #Opcional

system.time(avg_gt(gene_expression, gt)) #Para saber cuato tarda en ejecutar esta función

######Método 2

avg_gt_no_if <- function(x, gt) {
  valid_values <- x[x > gt]  # Usar subconjuntos de vectores para filtrar valores válidos
  return(mean(valid_values))
}

gene_expression <- table$Expression_Value # Valores de expresión génica

gt <- 0.25 # Threshold/Umbral que tienen que pasar los valores de expresión para que sean contados al hacer la media

result <- avg_gt_no_if(gene_expression, gt) # Función para calcular la media para valores que superan un umbral
result

print(paste("Average of values greater than", gt, "is:", result)) #Opcional

system.time(avg_gt_no_if(gene_expression, gt)) #Para saber cuato tarda en ejecutar esto

#Sin el loop es más rápio



# Comentarios de la profesora ---------------------------------------------

#Be careful when creating nested functions and changing the order of the arguments when you define them ...
  