#Author : Larbi Bedrani
#Date : April 13, 2018
#version: 0.6


###########################################################################
#Building the string of the function as string that will be evaluated after
###########################################################################

build_equation_string = function(dataset, formula_, function_name, additional_options=NULL){
  #Get dataset's name as a string
  data_name = deparse(substitute(dataset))
  #Check if the function has other arguments in addition to the formula, and build the equation as a string
  if(is.null(additional_options)){
    equation = paste(function_name, "(", 
                     paste(formula_, paste0(" data=", data_name), sep =",")
                     ,")", sep="")
  }else{
    equation = paste(function_name, "(", 
                     paste(formula_, paste0(" data=", data_name), paste(additional_options, collapse = ","),
                           sep =",")
                     ,")", sep="")
  }
  return(equation)
} 

#####################################################################
#Perform pairwise comparisons Using the combinat package as a starter
#####################################################################
Pairwise_comparisons = function(dataset, outcome_labels, function_name, 
                                formula_, additional_options=NULL){
  #Load the combn library if available or install it otherwise
  if("combinat" %in% rownames(installed.packages())){
    library(combinat)
  } else{
    install.packages("combinat")
    library(combinat)
  }
  #Generate a matrix of all possible combinations of 2 factors
  dataset <- dataset[order(dataset[,outcome_labels]),]
  all_combin <- as.matrix(combn(unique(dataset[,outcome_labels]), 2))
  
  #Create a list to store the results as results objects
  final_results <- list()
  combinations <- apply(all_combin, 2, function(x,dataset, outcome_labels, function_name, 
                                                formula_, additional_options){
    #Subset the dataset depending of the combination of 2 factors
    sub_table <- dataset[dataset[,outcome_labels]%in%x,]
    #Order the sub_dataset to get the righ order of factors
    sub_table <- sub_table[order(sub_table[outcome_labels]),]
    #Build the string of the function
    equation <- build_equation_string(dataset=sub_table, formula_=formula_, 
                                      function_name=function_name, additional_options=additional_options)
    message(equation)
    #Evaluate the function and return the results
    final_results[[paste(unique(sub_table[,outcome_labels]), collapse = "_vs_")]] <<- eval(parse(text=equation), envir = sub_table)
    return(NULL)
  }, dataset, outcome_labels, function_name, formula_, additional_options)
  
  return(final_results)
}


##################################
#Detail of the function arguments
##################################
#dataset: Name of the dataset containing the categorical variable, the values and additional columns (i.e. if multiple regression)
#outcome_labels: name of the column to be put in the left side of the formula
#function_name: name of the function to be called on the formula (i.e: lm, glm, ...etc) 
#formula_ : the formula to be used in strig format (i.e: "Values~factor(labels)")
#additional_options: Additional options to be added to the function definition (default value: NULL)


##############
#Examples
##############

dataset = data.frame("labels" = sample(c("A", "B", "C"), 100, replace = T), 
                     "Values" = rnorm(100,12,0.5)*rnorm(100,0.5,2)
                     )
                    
#Try different functions
res = Pairwise_comparisons( dataset=dataset, outcome_labels="labels", function_name="glm", 
                      formula_ ="factor(labels)~Values", 
                      additional_options=c("family = 'binomial'", "model = TRUE"))

res = Pairwise_comparisons( dataset=dataset, outcome_labels="labels", function_name="t.test", 
                            formula_ ="Values~factor(labels)" 
                            )
res = Pairwise_comparisons( dataset=dataset, outcome_labels="labels", 
                            function_name="kruskal.test", 
                            formula_ ="Values~factor(labels)" 
)






