`%>%` <- dplyr::`%>%`

#' Simulates data based on the structural equations provided by the user
#'
#' This function simulates causal data from a string given by the user. There are a few important things that must be true of the string. The format should be the name of the variable ~ the equation of the variable (binomial, normal, or an equation like x+5) and then the type of output in parenthesis (quant, binary, linear). Each variable should be comma separated, and the variables should be in order with the furthest ancestor first and the furthest descendant last.
#'
#' @param N the number of observations for the data
#' @param string a string containing the structural equation or type of distribution for each variable, as well as the type of data they want in parentheses, comma separated for each variable.
#' @return A data frame with data generated as specified by the structural equations provided.
#' @examples
#' # example code
#' sim_data <- sim_from_str(100, "z ~ normal (quant), x ~ binomial (bin), y ~ x + z (linear)")
#' sim_from_str(1000, "a ~ binomial (bin), b ~ 5a + 30 (bin), c~ a+b+20 (quant), d ~ a + b + c (lin)")
#' @export
sim_from_str <- function(N, string) {
  fulldata <- data.frame(0, 1:N) #create data frame
  fulldata <- fulldata[-1] #empty data frame
  list_of_parts <- strsplit(string, ",") #split the string into sections based on commas
  columnNames <- c() #creates empty list of column names for the final data frame
  for (i in list_of_parts[[1]]) { #loop by each string section
    variableName <- tolower(gsub("[[:space:]]", "", gsub("~.*", "", i))) #extract the variable name, the first part of each string section
    variableFormula <- tolower(gsub("[[:space:]]", "",(sub("\\([[:alpha:]]*?\\).*", "", sub(".*~", "", i))))) #extract the formula, the second part of each string section
    formulaType <- tolower(gsub("[[:space:]]", "",gsub("[\\(\\)]", "", regmatches(i, gregexpr("\\([[:alpha:]]*?\\)", i))[[1]]))) #extract the last part, the type of formula from the parentheses
    columnNames <- append(columnNames, variableName) #updates column name list to include new variable
    if (formulaType=="quant" || formulaType=="quantatative") {  #checks if they specified quantitative
      if (variableFormula=="normal" || variableFormula == "norm") { #checks if they specified just normal
        dataOutput <- rnorm(N, mean = 30, sd = 3) #save normal distribution samples to a data output
        assign(variableName, dataOutput) #assign that data to the variable name
        fulldata <- cbind(fulldata, dataOutput) #adds normal data for that variable to the data frame
      }
      else if (variableFormula=="uniform" || variableFormula == "unif") { #checks if they specified just uniform
        dataOutput <- runif(N, min=0, max =1) #save uniform distribution samples to a data output
        assign(variableName, dataOutput) #assign that data to the variable name
        fulldata <- cbind(fulldata, dataOutput) #adds uniform data for the variable to the data frame
      }
      else if (grepl("[\\+\\-\\/\\*]", variableFormula) == TRUE || grepl("[0-9]", variableFormula) == TRUE || tolower(gsub("[[:space:]]", "", variableFormula)) %in% columnNames == T) { #checks if they have any numbers or operators or previous variables
        place <- variableFormula #stores the formula so it can be changed to be evaluated
        correct1 <- regmatches(variableFormula, gregexpr("[0-9][a-z]", variableFormula)) #extracts number letter combos to list/string
        if (length(correct1[[1]]) > 0) { #checks if that list/string element is empty
          for (j in 1:length(correct1[[1]])) { #loops over that list/string element
            place <- gsub(correct1[[1]][[j]], paste0(substr(correct1[[1]][[j]],1,1),"*",substr(correct1[[1]][[j]],2,2)), place) #places an asterisk between numbers and letters
          }
        }
        correct2 <- regmatches(variableFormula, gregexpr("[a-z][0-9]", variableFormula)) #extracts letter number combos to list/string
        if (length(correct2[[1]]) > 0) { #checks if that list/string is empty
          for (j in 1:length(correct2[[1]])) { #loops over that list/string
            place <- gsub(correct2[[1]][[j]], paste0(substr(correct2[[1]][[j]],1,1),"*",substr(correct2[[1]][[j]],2,2)), place) #places an asterisk between letters and numbers
          }
        }
        formula_output <- eval(parse(text=place)) #evaluate and parses the cleaned string
        dataOutput <- rnorm(N, mean = formula_output, sd = 3) #generates normal data with the parsed and evaluated string as the mean
        assign(variableName, dataOutput) #assign the generated data to the variable name
        fulldata <- cbind(fulldata, dataOutput) #adds the normal data to the data frame
      }
      else {
        warning(paste("The function did not recognize your input for ", variableName, " so the data generated for it is just ~Normal(30, 3)")) #post a warning if the code isn't recognized
        dataOutput <- rnorm(N, mean = 30, sd = 3) #generates normal data instead of just generating nothing
        assign(variableName, dataOutput) #assigns that normal data to the variable name
        fulldata <- cbind(fulldata, dataOutput) #adds the normal data to the data frame
      }
    }
    else if (formulaType=="bin" || formulaType=="binary") { #checks if the formula type is binary
      if (variableFormula=="binomial" || variableFormula == "bin") { #checks if they just want typical binomial data
        dataOutput <- rbinom(N, size = 1, prob = 0.5) #generate binomial data, size 1, probability 0.5
        assign(variableName, dataOutput) #assign that binomial data to the variable
        fulldata <- cbind(fulldata, dataOutput) #adds the binomial data to the data frame
      }
      else if (grepl("[\\+\\-\\/\\*]", variableFormula) == TRUE || grepl("[0-9]", variableFormula) == TRUE || tolower(gsub("[[:space:]]", "", variableFormula)) %in% columnNames == T) { #checks if they have any numbers or operators
        place <- variableFormula #stores the formula so it can be changed to be evaluated
        correct1 <- regmatches(variableFormula, gregexpr("[0-9][a-z]", variableFormula))  #extracts number letter combos to list/string
        if (length(correct1[[1]]) > 0) { #checks if list/string is empty
          for (j in 1:length(correct1[[1]])) { #loops over each element of list/string
            place <- gsub(correct1[[1]][[j]], paste0(substr(correct1[[1]][[j]],1,1),"*",substr(correct1[[1]][[j]],2,2)), place) #puts asterisks between each letter and number combo
          }
        }
        correct2 <- regmatches(variableFormula, gregexpr("[a-z][0-9]", variableFormula)) #extracts letter number combos to list/string
        if (length(correct2[[1]]) > 0) { #checks if the list/string is empty
          for (j in 1:length(correct2[[1]])) { #loops over the list/string
            place <- gsub(correct2[[1]][[j]], paste0(substr(correct2[[1]][[j]],1,1),"*",substr(correct2[[1]][[j]],2,2)), place) #puts asterisks between each letter and number combo
          }
        }
        formula_output <- eval(parse(text=place)) #evaluates the parsed string
        odds_out <- exp(formula_output) #exponentiates the log odds
        p_out <- odds_out/(1+odds_out) #converts odds to probability
        dataOutput <- rbinom(N, size= 1, prob = p_out) #generates binomial data using the probability
        assign(variableName, dataOutput) #assigns the binomial data to the variable
        fulldata <- cbind(fulldata, dataOutput) #adds the binomial data to the data frame
      }

      else {
        warning(paste("The function did not recognize your input for ", variableName, " so the data generated for it is just ~binom(1, 0.5)")) #gives user a warning that the code didn't recognize your input
        dataOutput <- rbinom(N, size = 1, prob = 0.5) #produces binomial data instead of not producing anything
        assign(variableName, dataOutput) #assigns this binomial data to the variable
        fulldata <- cbind(fulldata, dataOutput) #adds this binomial data to the data frame
      }}
    else if (formulaType=="lin" || formulaType=="linear") { #checks if the formula type is linear (as in linear combination)
      place <- variableFormula #stores the formula so it can be changed to be evaluated
      correct1 <- regmatches(variableFormula, gregexpr("[0-9][a-z]", variableFormula))  #extracts number letter combos to list/string
      if (length(correct1[[1]]) > 0) { #checks if list/string is empty
        for (j in 1:length(correct1[[1]])) { #loops over each element of list/string
          place <- gsub(correct1[[1]][[j]], paste0(substr(correct1[[1]][[j]],1,1),"*",substr(correct1[[1]][[j]],2,2)), place) #puts asterisks between each letter and number combo
        }
      }
      correct2 <- regmatches(variableFormula, gregexpr("[a-z][0-9]", variableFormula)) #extracts letter number combos to list/string
      if (length(correct2[[1]]) > 0) { #checks if the list/string is empty
        for (j in 1:length(correct2[[1]])) { #loops over the list/string
          place <- gsub(correct2[[1]][[j]], paste0(substr(correct2[[1]][[j]],1,1),"*",substr(correct2[[1]][[j]],2,2)), place) #puts asterisks between each letter and number combo
        }
      }
      formula_output <- eval(parse(text=place)) #evaluates the parsed string
      dataOutput <- formula_output + rnorm(N, mean = 0, sd = 1) #adds white noise to the linear combination
      assign(variableName, dataOutput) #assigns the linear combination data to the variable
      fulldata <- cbind(fulldata, dataOutput) #adds the linear combination data to the data frame
    }
  }
  fulldata <- fulldata[-1] #removes the first column (id numbers essentially)
  colnames(fulldata) <- columnNames #assigns the column names to the data frame
  return(fulldata) #returns the data frame :)
}

#' Changing odds to a probability
#'
#' @param odds A number in odds form
#' @examples
#' prob = odds_to_prob(log_odds_A)
#' odds_to_prob(-0.6)
#' @export
odds_to_prob <- function(odds) {
  prob <- exp(odds)/(1+exp(odds))
  return(prob)
}

#' Creating the weights used for inverse probability weighting or propensity score weighting
#'
#' @param data the data used to create the weights
#' @param model the model which we want to apply the weights to
#' @param output what we are trying to model (for IPW it should be the treatment {A}; for propensity scoring it should be our outcome variable {Y})
#' @examples
#' weighting(sim_data, ps_mod_simple, A)
#' weighting(tenure, ps_mod, tenure$gncs)
#' @export
weighting <- function(data, model, output) {
  data %>% dplyr::mutate(
    ps = predict(model, newdata = data, type = "response"),
    ipw = dplyr::case_when(
      output==1 ~ 1/ps,
      output==0 ~ 1/(1-ps)
    )
  )
}

#' Creating bar charts to check if the IPW weighting actually removed the relationship between treatment and the intended variable
#'
#' @param data the data used in the IPW analysis
#' @param x the x axis for the boxplot (the variable we want to no longer have a relationship with treatment)
#' @param fill the fill for the boxplot (typically the treatment)
#' @param weights optional parameter to include the weights
#' @examples
#' ipw_viz_bar(sim_data, Z, A)
#' ipw_viz_bar(sim_data, Z, A, ipw_complex)
#' @export
ipw_viz_bar <- function(data, x, fill, weights) {
  if(missing(weights)){
    data %>%
      ggplot2::ggplot(aes({{x}}, fill = {{fill}})) +
      ggplot2::geom_bar(position = "fill")
  } else {
    data %>%
      ggplot2::ggplot(aes({{x}}, fill = {{fill}}, weight = {{weights}})) +
      ggplot2::geom_bar(position = "fill")
  }
}

#' Creating scatterplots to determine model specification for propensity scoring (linear or non-linear) - the blue smooth reflects observed data trends, and the red smooth shows the predictions from a logistic regression model with a specific model formula
#'
#' @param data the data used for the propensity scoring
#' @param x the x axis for the scatterplot (the variable whose specification you're exploring)
#' @param y the y axis for the scatterplot (the outcome variable you're interested in)
#' @examples
#' ipw_viz_point(tenure, revenue, gncs)
#' ipw_viz_point(tenure, full_ratio, gncs)
#' @export
ipw_viz_point <- function(data, x, y){
  p1 <- ggplot2::ggplot(data, aes({{x}}, {{y}})) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(se = F, color = "blue", method = "loess") +
    ggplot2::geom_smooth(formula = y ~ x, method = "glm",
                method.args = list(family = "binomial"),
                se = F, color = 'red') +
    ggplot2::labs(title = "Assuming Linearity")

  p2 <- ggplot2::ggplot(data, aes({{x}}, {{y}})) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(se = F, color = "blue", method = "loess") +
    ggplot2::geom_smooth(formula = y ~ ns(x,3), method = "glm",
                method.args = list(family = "binomial"),
                se = F, color = 'red') +
    ggplot2::labs(title = "Assuming Nonlinearity",
         subtitle = "natural spline with 3 df")
  gridExtra::grid.arrange(p1, p2, nrow = 1, ncol = 2)
}

#' Creating a boxplot to compare effectiveness of propensity score weighting
#'
#'@param data the data used for the propensity scoring analysis
#'@param x the x axis for the boxplot (usually the outcome variable) - must be a factor
#'@param y the y axis for the boxplot (usually the variable we want to no longer have a relationship with outcome)
#'@param weights optional parameter to include the weights
#'@examples
#'ipw_viz_box(tenure, factor(gncs), full_ratio)
#'ipw_viz_box(tenure, factor(gncs), full_ratio, ipw)
#'@export
ipw_viz_box <- function(data, x, y, weights) {
  if(missing(weights)){
    data %>%
      ggplot2::ggplot(aes({{x}}, {{y}})) +
      ggplot2::geom_boxplot()
  } else {
    data %>%
      ggplot2::ggplot(aes({{x}}, {{y}}, weight = {{weights}})) +
      ggplot2::geom_boxplot()
  }
}

#' Provides information on the marginal/conditional (in)dependence of the three key causal structures: chain, fork, and collider
#'
#' This function takes a string, either 'chain', 'fork', or 'collider' and returns a paragraph on how the three variables in each structure are marginally/conditionally (in)dependent from one another
#'
#' @param structure A string input, either 'chain', 'fork', or 'collider'
#' @return A paragraph on how the three variables in each structure are marginally/conditionally (in)dependent from one another
#' @export
check_key_structure_independence <- function(structure) {
  if (tolower(structure) == 'chain') {
    return("In a chain X → Y → Z, marginal dependence holds for every pair of variables in a chain causal graph without controlling for other variables. Specifically: (1) X and Y are marginally dependent. (2) Y and Z are marginally dependent. (3) X and Z are marginally dependent. However, when keeping the intermediate variable constant (i.e. Y in the relationship X → Y → Z), then X and Z are conditionally independent due to no changes in Y.")
  } else if (tolower(structure) == 'fork') {
    return("In a fork X ← Y → Z, marginal dependence holds for every pair of variables in a chain causal graph without controlling for other variables. Specifically: (1) X and Y are marginally dependent. (2) Y and Z are marginally dependent. (3) X and Z are marginally dependent. However, when keeping the common parent constant (i.e. Y in the relationship X ← Y → Z), then X and Z are conditionally independent due to no changes in Y.")
  } else if (tolower(structure) == 'collider') {
    return("In a collider X → Y ← Z, marginal independence holds for the parentless variables X and Z, while the common child variable Y is marginally dependent on each of the parent variable X and Z. Specifically: (1) X and Y are marginally dependent. (2) Y and Z are marginally dependent. (3) X and Z are marginally independent. When the child variable Y is kept constant, X and Z are conditionally dependent.")
  } else if (tolower(structure) == 'spork') {
    return("You mean fork????!!!! In a fork X ← Y → Z, marginal dependence holds for every pair of variables in a chain causal graph without controlling for other variables. Specifically: (1) X and Y are marginally dependent. (2) Y and Z are marginally dependent. (3) X and Z are marginally dependent. However, when keeping the common parent constant (i.e. Y in the relationship X ← Y → Z), then X and Z are conditionally independent due to no changes in Y.")
  } else {
    return("Incorrect input. Please input a string specifying one in three options: 'chain', 'fork', or 'collider'")
  }
}


#' Simulates Z values that are dependent on Y values in a chain. This function is used within the set_y_and_z_in_chain(n, y_numeric, z_numeric, X) function.
#'
#' @param n The number of observations of Z for a single simulation
#' @param z_numeric TRUE for numeric Z and FALSE for binary Z
#' @param Y The values of Y in the big function to generate Z values that are dependent on Y values
#' @return Simulated Z values
#' @export
set_z_in_chain <- function(n, z_numeric, Y) {
  if (z_numeric) {
    # Simulate numeric z
    Z <- rnorm(n, mean = 1 + 5*Y, sd = 1)
  } else {
    # Simulate binary z
    coeff <- ifelse(length(unique(Y)) > 2, 0.05, 1)
    log_odds_Z <- -1 + coeff*Y
    odds_Z <- exp(log_odds_Z)
    p_Z <- odds_Z/(1+odds_Z)
    Z <- rbinom(n, size = 1, prob = p_Z)
    Z <- factor(Z)
  }
  return(Z)
}

#' Simulates Y and Z values where Y values are dependent on X values for a chain. This function is used within the simple_chain_simulation(n, x_numeric, y_numeric, z_numeric).
#'
#' @param n The number of observations of Z for a single simulation
#' @param y_numeric TRUE for numeric Y and FALSE for binary Y
#' @param z_numeric TRUE for numeric Z and FALSE for binary Z
#' @param X The values of X in the big function to generate Y values that are dependent on X values
#' @return A list containing simulated Y and Z values
#' @export
set_y_and_z_in_chain <- function(n, y_numeric, z_numeric, X) {
  if (y_numeric) {
    # Simulate numeric y
    Y <- rnorm(n, mean = 1 + 5*X, sd = 1)
    # Set z
    Z <- set_z_in_chain(n, z_numeric, Y)
  } else {
    # Simulate binary y
    coeff <- ifelse(length(unique(X)) > 2, 0.05, 1)
    log_odds_Y <- -1 + coeff*X
    odds_Y <- exp(log_odds_Y)
    p_Y <- odds_Y/(1+odds_Y)
    Y <- rbinom(n, size = 1, prob = p_Y)
    # Set z
    Z <- set_z_in_chain(n, z_numeric, Y)
    Y <- factor(Y)
  }
  result_list = list(Y_values=Y, Z_values=Z)
  return(result_list)
}


#' Simulates the dependency structure of a basic chain, with 3 variables X, Y, Z, where X → Y → Z
#'
#' This function takes the number of observations of X, Y, Z for a single simulation. It also requires the user to specify whether they want X, Y, or Z to be numeric or binary
#'
#' @param n The number of observations of X, Y, Z for a single simulation
#' @param x_numeric TRUE for numeric X and FALSE for binary X
#' @param y_numeric TRUE for numeric Y and FALSE for binary Y
#' @param z_numeric TRUE for numeric Z and FALSE for binary Z
#' @return A data frame of the 4 tests performed (Check if X and Y are marginally dependent, Check if Z and Y are marginally dependent, Check if X and Z are marginally dependent, Check if X and Z are conditionally independent given Y), the model p-values corresponding to the tests, and the interpretation of the result.
#' @examples
#' # example code
#' # Results in a chain simulation with 10000 simulated observations of X, Y, Z, with X and Y being numeric and Z being binary.
#' simple_chain_simulation(10000, x_numeric = TRUE, y_numeric = TRUE, z_numeric = FALSE)
#' @export
simple_chain_simulation <- function(n, x_numeric, y_numeric, z_numeric) {
  # Variable configuration
  if (x_numeric) {
    # Simulate numeric x
    X <- rnorm(n, mean = 30, sd = 3)
    Y_and_Z <- set_y_and_z_in_chain(n, y_numeric, z_numeric, X)
    Y <- Y_and_Z$Y
    Z <- Y_and_Z$Z
  } else {
    # Simulate binary x
    X <- rbinom(n, size = 1, prob = 0.6)
    Y_and_Z <- set_y_and_z_in_chain(n, y_numeric, z_numeric, X)
    Y <- Y_and_Z$Y_values
    Z <- Y_and_Z$Z_values
    X <- factor(X)
  }
  sim_data <- data.frame(X, Y, Z)
  # View(sim_data)
  # return(sim_data)
  if (y_numeric) {
    if (z_numeric) {
      mod1 <- lm(Y ~ X, data = sim_data) # Check if X and Y are marginally dependent
      mod2 <- lm(Y ~ Z, data = sim_data) # Check if Z and Y are marginally dependent
      mod3 <- lm(Z ~ X, data = sim_data) # Check if X and Z are marginally dependent
      mod4 <- lm(Z ~ X+Y, data = sim_data) # Check if X and Z are conditionally independent given Y
    } else {
      mod1 <- lm(Y ~ X, data = sim_data) # Check if X and Y are marginally dependent
      mod2 <- lm(Y ~ Z, data = sim_data) # Check if Z and Y are marginally dependent
      mod3 <- glm(Z ~ X, data = sim_data, family = "binomial") # Check if X and Z are marginally dependent
      mod4 <- glm(Z ~ X+Y, data = sim_data, family = "binomial") # Check if X and Z are conditionally independent given Y
    }
  } else {
    if (z_numeric) {
      mod1 <- glm(Y ~ X, data = sim_data, family = "binomial") # Check if X and Y are marginally dependent
      mod2 <- glm(Y ~ Z, data = sim_data, family = "binomial") # Check if Z and Y are marginally dependent
      mod3 <- lm(Z ~ X, data = sim_data) # Check if X and Z are marginally dependent
      mod4 <- lm(Z ~ X+Y, data = sim_data) # Check if X and Z are conditionally independent given Y
    } else {
      mod1 <- glm(Y ~ X, data = sim_data, family = "binomial") # Check if X and Y are marginally dependent
      mod2 <- glm(Y ~ Z, data = sim_data, family = "binomial") # Check if Z and Y are marginally dependent
      mod3 <- glm(Z ~ X, data = sim_data, family = "binomial") # Check if X and Z are marginally dependent
      mod4 <- glm(Z ~ X+Y, data = sim_data, family = "binomial") # Check if X and Z are conditionally independent given Y
    }
  }

  # Extract p-values
  pval1 <- broom::tidy(mod1)$p.value[2] # The [2] extracts the 2nd p-value in the output table
  pval2 <- broom::tidy(mod2)$p.value[2]
  pval3 <- broom::tidy(mod3)$p.value[2]
  pval4 <- broom::tidy(mod4)$p.value[2]

  # Put into a data frame
  result_df = data.frame(
    description = c("Check if X and Y are marginally dependent", "Check if Z and Y are marginally dependent", "Check if X and Z are marginally dependent", "Check if X and Z are conditionally independent given Y"),
    model_pval = c(pval1, pval2, pval3, pval4),
    pval_interpretation = c(ifelse(pval1 <= 0.05, 'The two variables are dependent', 'The two variables are independent'), ifelse(pval2 <= 0.05,  'The two variables are dependent', 'The two variables are independent'), ifelse(pval3 <= 0.05,  'The two variables are dependent', 'The two variables are independent'), ifelse(pval4 <= 0.05, 'The two variables are dependent', 'The two variables are independent'))
  )
  return(result_df)

}

#' Simulates Z values that are dependent on Y values in a fork. This function is used within the simple_fork_simulation(n, x_numeric, y_numeric, z_numeric) function.
#'
#' @param n The number of observations of Z for a single simulation
#' @param z_numeric TRUE for numeric Z and FALSE for binary Z
#' @param Y The values of Y in the big function to generate Z values that are dependent on Y values
#' @return Simulated Z values
#' @export
set_z_in_fork <- function(n, z_numeric, Y) {
  if (z_numeric) {
    # Simulate numeric z
    Z <- rnorm(n, mean = 5 + Y, sd = 3)
  } else {
    # Simulate binary z
    coeff <- ifelse(length(unique(Y)) > 2, 0.05, 1.5)
    log_odds_Z <- -1 + coeff*Y
    odds_Z <- exp(log_odds_Z)
    p_Z <- odds_Z/(1+odds_Z)
    Z <- rbinom(n, size = 1, prob = p_Z)
    Z <- factor(Z)
  }
  return(Z)
}

#' Simulates X values that are dependent on Y values in a fork. This function is used within the simple_fork_simulation(n, x_numeric, y_numeric, z_numeric) function.
#'
#' @param n The number of observations of X for a single simulation
#' @param x_numeric TRUE for numeric X and FALSE for binary X
#' @param Y The values of Y in the big function to generate X values that are dependent on Y values
#' @return Simulated X values
#' @export
set_x_in_fork <- function(n, x_numeric, Y) {
  if (x_numeric) {
    # Simulate numeric z
    X <- rnorm(n, mean = 8 + Y, sd = 3)
  } else {
    #Simulate binary X, X depends on Y
    coeff <- ifelse(length(unique(Y)) > 2, 0.05, 2)
    log_odds_X <- -1 + coeff*Y
    odds_X <- exp(log_odds_X)
    p_X <- odds_X/(1+odds_X)
    X <- rbinom(n, size = 1, prob = p_X)
    X <- factor(X)
  }
  return(X)
}

#' Simulates the dependency structure of a basic fork, with 3 variables X, Y, Z, where X ← Y → Z
#'
#' This function takes the number of observations of X, Y, Z for a single simulation. It also requires the user to specify whether they want X, Y, or Z to be numeric or binary
#'
#' @param n The number of observations of X, Y, Z for a single simulation
#' @param x_numeric TRUE for numeric X and FALSE for binary X
#' @param y_numeric TRUE for numeric Y and FALSE for binary Y
#' @param z_numeric TRUE for numeric Z and FALSE for binary Z
#' @return A data frame of the 4 tests performed (Check if X and Y are marginally dependent, Check if Z and Y are marginally dependent, Check if X and Z are marginally dependent, Check if X and Z are conditionally independent given Y), the model p-values corresponding to the tests, and the interpretation of the result.
#' @examples
#' # example code
#' # Results in a fork simulation with 10000 simulated observations of X, Y, Z, with X and Y being numeric and Z being binary.
#' set.seed(451)
#' simple_fork_simulation(10000, x_numeric = TRUE, y_numeric = TRUE, z_numeric = FALSE)
#' @export
simple_fork_simulation <- function(n, x_numeric, y_numeric, z_numeric) {
  # Variable configuration
  if (y_numeric) {
    # Simulate numeric y
    Y <- rnorm(n, mean = 15, sd = 3)
    X <- set_x_in_fork(n, x_numeric, Y)
    Z <- set_z_in_fork(n, z_numeric, Y)
  } else {
    # Simulate binary y
    Y <- rbinom(n, size = 1, prob = 0.6)
    X <- set_x_in_fork(n, x_numeric, Y)
    Z <- set_z_in_fork(n, z_numeric, Y)
    Y <- factor(Y)
  }
  sim_data <- data.frame(X, Y, Z)
  if (y_numeric) {
    if (z_numeric) {
      mod1 <- lm(Y ~ X, data = sim_data) # Check if X and Y are marginally dependent
      mod2 <- lm(Y ~ Z, data = sim_data) # Check if Z and Y are marginally dependent
      mod3 <- lm(Z ~ X, data = sim_data) # Check if X and Z are marginally dependent
      mod4 <- lm(Z ~ X+Y, data = sim_data) # Check if X and Z are conditionally independent given Y
    } else {
      mod1 <- lm(Y ~ X, data = sim_data) # Check if X and Y are marginally dependent
      mod2 <- lm(Y ~ Z, data = sim_data) # Check if Z and Y are marginally dependent
      mod3 <- glm(Z ~ X, data = sim_data, family = "binomial") # Check if X and Z are marginally dependent
      mod4 <- glm(Z ~ X+Y, data = sim_data, family = "binomial") # Check if X and Z are conditionally independent given Y
    }
  } else {
    if (z_numeric) {
      mod1 <- glm(Y ~ X, data = sim_data, family = "binomial") # Check if X and Y are marginally dependent
      mod2 <- glm(Y ~ Z, data = sim_data, family = "binomial") # Check if Z and Y are marginally dependent
      mod3 <- lm(Z ~ X, data = sim_data) # Check if X and Z are marginally dependent
      mod4 <- lm(Z ~ X+Y, data = sim_data) # Check if X and Z are conditionally independent given Y
    } else {
      mod1 <- glm(Y ~ X, data = sim_data, family = "binomial") # Check if X and Y are marginally dependent
      mod2 <- glm(Y ~ Z, data = sim_data, family = "binomial") # Check if Z and Y are marginally dependent
      mod3 <- glm(Z ~ X, data = sim_data, family = "binomial") # Check if X and Z are marginally dependent
      mod4 <- glm(Z ~ X+Y, data = sim_data, family = "binomial") # Check if X and Z are conditionally independent given Y
    }
  }

  # Extract p-values
  pval1 <- broom::tidy(mod1)$p.value[2] # The [2] extracts the 2nd p-value in the output table
  pval2 <- broom::tidy(mod2)$p.value[2]
  pval3 <- broom::tidy(mod3)$p.value[2]
  pval4 <- broom::tidy(mod4)$p.value[2]

  # Put into a data frame
  result_df = data.frame(
    description = c("Check if X and Y are marginally dependent", "Check if Z and Y are marginally dependent", "Check if X and Z are marginally dependent", "Check if X and Z are conditionally independent given Y"),
    model_pval = c(pval1, pval2, pval3, pval4),
    pval_interpretation = c(ifelse(pval1 <= 0.05, 'The two variables are dependent', 'The two variables are independent'), ifelse(pval2 <= 0.05,  'The two variables are dependent', 'The two variables are independent'), ifelse(pval3 <= 0.05,  'The two variables are dependent', 'The two variables are independent'), ifelse(pval4 <= 0.05, 'The two variables are dependent', 'The two variables are independent'))
  )
  return(result_df)

}



#' Simulates X values in a collider. This function is used within the simple_collider_simulation(n, x_numeric, y_numeric, z_numeric) function.
#'
#' @param n The number of observations of X for a single simulation
#' @param x_numeric TRUE for numeric X and FALSE for binary X
#' @return Simulated X values
#' @export
set_x_in_collider <- function(n, x_numeric) {
  if (x_numeric) {
    # Simulate numeric z
    X <- rnorm(n, mean = 10, sd = 5)
  } else {
    #Simulate binary X, X depends on Y
    X <- rbinom(n, size = 1, prob = 0.6)
  }
  return(X)
}

#' Simulates Z values in a collider. This function is used within the simple_collider_simulation(n, x_numeric, y_numeric, z_numeric) function.
#'
#' @param n The number of observations of Z for a single simulation
#' @param z_numeric TRUE for numeric Z and FALSE for binary Z
#' @return Simulated Z values
#' @export
set_z_in_collider <- function(n, z_numeric) {
  if (z_numeric) {
    # Simulate numeric z
    Z <- rnorm(n, mean = 10, sd = 5)
  } else {
    # Simulate binary z
    Z <- rbinom(n, size = 1, prob = 0.6)
  }
  return(Z)
}

#' Simulates the dependency structure of a basic collider, with 3 variables X, Y, Z, where X → Y ← Z
#'
#' This function takes the number of observations of X, Y, Z for a single simulation. It also requires the user to specify whether they want X, Y, or Z to be numeric or binary
#'
#' @param n The number of observations of X, Y, Z for a single simulation
#' @param x_numeric TRUE for numeric X and FALSE for binary X
#' @param y_numeric TRUE for numeric Y and FALSE for binary Y
#' @param z_numeric TRUE for numeric Z and FALSE for binary Z
#' @return A data frame of the 4 tests performed (Check if X and Y are marginally dependent, Check if Z and Y are marginally dependent, Check if X and Z are marginally dependent, Check if X and Z are conditionally independent given Y), the model p-values corresponding to the tests, and the interpretation of the result.
#' @examples
#' # example code
#' # Results in a collider simulation with 10000 simulated observations of X, Y, Z, with X and Y being numeric and Z being binary.
#' set.seed(451)
#' simple_collider_simulation(10000, x_numeric = TRUE, y_numeric = TRUE, z_numeric = FALSE)
#' @export
simple_collider_simulation <- function(n, x_numeric, y_numeric, z_numeric) {
  # Variable configuration
  if (y_numeric) {
    X <- set_x_in_collider(n, x_numeric)
    Z <- set_x_in_collider(n, z_numeric)
    Y <- rnorm(n, mean = 5 + X + Z, sd = 3) # Y depends on X and Z
  } else {
    X <- set_x_in_collider(n, x_numeric)
    Z <- set_x_in_collider(n, z_numeric)
    #Simulate binary y
    coeff_X <- ifelse(length(unique(X)) > 2, 0.08, 4)
    coeff_Z <- ifelse(length(unique(Z)) > 2, 0.08, 5)
    log_odds_Y <- -1 + coeff_X*X + coeff_Z*Z
    odds_Y <- exp(log_odds_Y)
    p_Y <- odds_Y/(1+odds_Y)
    Y <- rbinom(n, size = 1, prob = p_Y)
    Y <- factor(Y)
  }
  if (x_numeric == FALSE) {
    X <- factor(X)
  }
  if (z_numeric == FALSE) {
    Z <- factor(Z)
  }
  sim_data <- data.frame(X, Y, Z)
  if (y_numeric) {
    if (z_numeric) {
      mod1 <- lm(Y ~ X, data = sim_data) # Check if X and Y are marginally dependent
      mod2 <- lm(Y ~ Z, data = sim_data) # Check if Z and Y are marginally dependent
      mod3 <- lm(Z ~ X, data = sim_data) # Check if X and Z are marginally dependent
      mod4 <- lm(Z ~ X+Y, data = sim_data) # Check if X and Z are conditionally independent given Y
    } else {
      mod1 <- lm(Y ~ X, data = sim_data) # Check if X and Y are marginally dependent
      mod2 <- lm(Y ~ Z, data = sim_data) # Check if Z and Y are marginally dependent
      mod3 <- glm(Z ~ X, data = sim_data, family = "binomial") # Check if X and Z are marginally dependent
      mod4 <- glm(Z ~ X+Y, data = sim_data, family = "binomial") # Check if X and Z are conditionally independent given Y
    }
  } else {
    if (z_numeric) {
      mod1 <- glm(Y ~ X, data = sim_data, family = "binomial") # Check if X and Y are marginally dependent
      mod2 <- glm(Y ~ Z, data = sim_data, family = "binomial") # Check if Z and Y are marginally dependent
      mod3 <- lm(Z ~ X, data = sim_data) # Check if X and Z are marginally dependent
      mod4 <- lm(Z ~ X+Y, data = sim_data) # Check if X and Z are conditionally independent given Y
    } else {
      mod1 <- glm(Y ~ X, data = sim_data, family = "binomial") # Check if X and Y are marginally dependent
      mod2 <- glm(Y ~ Z, data = sim_data, family = "binomial") # Check if Z and Y are marginally dependent
      mod3 <- glm(Z ~ X, data = sim_data, family = "binomial") # Check if X and Z are marginally dependent
      mod4 <- glm(Z ~ X+Y, data = sim_data, family = "binomial") # Check if X and Z are conditionally independent given Y
    }
  }

  # Extract p-values
  pval1 <- broom::tidy(mod1)$p.value[2] # The [2] extracts the 2nd p-value in the output table
  pval2 <- broom::tidy(mod2)$p.value[2]
  pval3 <- broom::tidy(mod3)$p.value[2]
  pval4 <- broom::tidy(mod4)$p.value[2]

  # Put into a data frame
  result_df = data.frame(
    description = c("Check if X and Y are marginally dependent", "Check if Z and Y are marginally dependent", "Check if X and Z are marginally independent", "Check if X and Z are conditionally dependent given Y"),
    model_pval = c(pval1, pval2, pval3, pval4),
    pval_interpretation = c(ifelse(pval1 <= 0.05, 'The two variables are dependent', 'The two variables are independent'), ifelse(pval2 <= 0.05,  'The two variables are dependent', 'The two variables are independent'), ifelse(pval3 <= 0.05,  'The two variables are dependent', 'The two variables are independent'), ifelse(pval4 <= 0.05, 'The two variables are dependent', 'The two variables are independent'))
  )
  return(result_df)
}

#' Replicates n number of times a chosen function out of 3 functions: simple_chain_simulation(n, x_numeric, y_numeric, z_numeric), simple_fork_simulation(n, x_numeric, y_numeric, z_numeric), simple_collider_simulation(n, x_numeric, y_numeric, z_numeric).
#'
#' @param function A function out of the 3 configured functions: simple_chain_simulation(n, x_numeric, y_numeric, z_numeric), simple_fork_simulation(n, x_numeric, y_numeric, z_numeric), simple_collider_simulation(n, x_numeric, y_numeric, z_numeric)
#' @param n_rep_time The number of times a function is replicated
#' @return A data frame of the 4 tests performed (Check if X and Y are marginally dependent, Check if Z and Y are marginally dependent, Check if X and Z are marginally dependent, Check if X and Z are conditionally independent given Y), the percentage of significant p-values corresponding to the tests, and the interpretation of the result.
#' @examples
#' # example code
#' # A replication of the simple_fork_simulation(10000, x_numeric = TRUE, y_numeric = FALSE, z_numeric = TRUE) for 100 times
#' set.seed(451)
#' simulate_many(simple_fork_simulation(10000, x_numeric = TRUE, y_numeric = FALSE, z_numeric = TRUE), n_rep_time = 100)
#' @export
simulate_many <- function(func, n_rep_time) {
  sim_results <- replicate(n_rep_time, func, simplify = FALSE)
  sim_results <- dplyr::bind_rows(sim_results)

  sim_results %>%
    dplyr::group_by(description) %>%
    dplyr::summarize(percentage_of_significant_p_values = mean(model_pval < 0.05),
              interpretation = dplyr::case_when(percentage_of_significant_p_values == 0 ~ 'The two variables are INDEPENDENT in all of the simulations',
                                                percentage_of_significant_p_values == 1 ~ 'The two variables are DEPENDENT in all of the simulations',
                                                percentage_of_significant_p_values > 0 & percentage_of_significant_p_values < 1 ~ paste0('The two variables are DEPENDENT in ', percentage_of_significant_p_values*100, '% of the ', n_rep_time, ' simulations'))
    )
}
