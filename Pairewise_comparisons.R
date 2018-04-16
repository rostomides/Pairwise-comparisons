#Author : Larbi Bedrani
#Date : April 13, 2018
#version: 1.0

###########################################################################
#Building the string of the function as string that will be evaluated after
###########################################################################

build_equation_string = function(dataset, formula_, additional_options=NULL){
  #Get dataset's name as a string
  data_name = deparse(substitute(dataset)
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

######################################################################
#Perform pairwise comparisons using no library as a starter
######################################################################
Pairwise_comparisons = function(dataset, outcome_labels, function_name, formula_, additional_options=NULL){
  #Order the dataset to get the right order of the factors
  dataset <- dataset[order(dataset[,outcome_labels]),]
  #Get the unique factors available in the outcome_labels column
  groups <- as.character(unique(dataset[,outcome_labels]))
  #Create a nother vector groups without the first element
  groups_m <-groups[-1]
  #Create a list to store the results as results objects
  final_results <- list()
  #Perform the pairwise comparisons
  combinations <- sapply(groups[-length(groups)], function(x,dataset){    
    sub_table <- dataset[dataset[,outcome_labels]%in%c(x,groups_m[1]),]
    sub_table <- sub_table[order(sub_table[outcome_labels]),]
    equation <- build_equation_string(sub_table, formula_, additional_options)
    final_results[[paste(unique(sub_table[,outcome_labels]), collapse = "_vs_")]] <<- eval(parse(text=equation), envir = sub_table)
    groups_m <<-groups_m[-1]
    return(NULL)
  }, dataset)
  
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

#####################
#Test the function
#####################


dataset = data.frame("labels" = sample(c("A", "B", "C","D", "E", "F", "G", "H", "I"), 10000, replace = T), 
                     "Values" = rnorm(10000,12,0.5)
                     )
                    
res = Pairwise_comparisons( dataset=dataset, outcome_labels="labels", function_name="glm", 
                      formula_ ="factor(labels)~Values", additional_options=c("family = 'binomial'", "model = TRUE"))

res = Pairwise_comparisons( dataset=dataset, outcome_labels="labels", function_name="t.test", 
                            formula_ ="Values~factor(labels)" 
                            )
res = Pairwise_comparisons( dataset=dataset, outcome_labels="labels", function_name="kruskal.test", 
                            formula_ ="Values~factor(labels)" 
)

#Every element of the res is a result abject of the function that has been used







