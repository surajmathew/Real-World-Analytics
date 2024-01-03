###############################################################
# title: "Suraj-Code.R"
# output: R Script
# Student Name: Suraj Mathew Thomas
# Student ID: S223509398
# Subject: SIG 718 - Real World Analytics
# Assessment: End Term Assessment
###############################################################

###############################################################

## Question 2 b) Solve the model using R/R Studio. Find the optimal profit and optimal values of the decision variables.

###############################################################

library(lpSolveAPI) #Calling the lpSolveAPI library

# Initialising the model with 9  constraints and 9 decision variables
FactoryModel = make.lp(9, 9)


# Specifying that we want to maximize our objective function
lp.control(FactoryModel, sense = "max")


# Setting up our objective
set.objfn(FactoryModel, c(15,10,25,11,6,21,15,10,25))

# Constraints 

# Demand side Constraints
set.row(FactoryModel, 1, c(1,1,1), indices = c(1,2,3)) #Bloom
set.row(FactoryModel, 2, c(1,1,1), indices = c(4,5,6)) #Amber
set.row(FactoryModel, 3, c(1,1,1), indices = c(7,8,9)) #Leaf

# Proportion Constraint of Cotton
set.row(FactoryModel, 4, c(0.5,-0.5,-0.5), indices = c(1,2,3)) #Bloom
set.row(FactoryModel, 5, c(0.4,-0.6,-0.6), indices = c(4,5,6)) #Amber
set.row(FactoryModel, 6, c(0.5,-0.5,-0.5), indices = c(7,8,9)) #Leaf

# Proportion Constraint of Wool
set.row(FactoryModel, 7, c(-0.4,0.6,-0.4), indices = c(1,2,3)) #Bloom
set.row(FactoryModel, 8, c(-0.4,0.6,-0.4), indices = c(4,5,6)) #Amber
set.row(FactoryModel, 9, c(-0.3,0.7,-0.3), indices = c(7,8,9)) #Leaf


set.rhs(FactoryModel, c(4200,3200,3500,0,0,0,0,0,0)) #Setting the RHS of the equation in the order of the constraints given
set.type(FactoryModel, c(1:9),"real") #making sure we are dealing with real numbers
set.bounds(FactoryModel, lower = rep(0,9), upper = rep(Inf,9)) #setting the model bounds

set.constr.type(FactoryModel, c("<=","<=","<=",">=",">=",">=",">=",">=",">=")) #Setting the equation constructors

solve(FactoryModel)

get.objective(FactoryModel) #141850

get.variables(FactoryModel) #2100, 1680,  420, 1920, 1280, 0,  1750, 1050,  700



###############################################################

## Question 3 (b) Considering all possible combinations of bids, formulate the payoff matrix for the game.

###############################################################

#Defining Payoff Matrix

PayoffMatrix <- matrix(c(-1,-1,-1,-1,1,1,-1,-1,-1,1,1,1,-1,-1,1,1,1,1,-1,1,1,1,1,1,-1), nrow = 5, byrow = TRUE)

#PLAYER 1

#Function to calculate the Maximum Secured Payoff for Player 1 [ in this case SKY]
calc_max_secured_payoff <- function(PayoffMatrix) {
  min_secured_payoff <- apply(PayoffMatrix, 1, min)
  return(max(min_secured_payoff))
}

#Calculation and printing of the Maximum Secured Payoff for Player 1
max_secured_payoff <- calc_max_secured_payoff(PayoffMatrix)
cat("The Maximum Secured Payoff for Player 1:", max_secured_payoff, "\n")

###############################################################

#PLAYER 2

#Function to calculate the Maximum Secured Payoff for Player 1 [ in this case GIANT]

calc_max_secured_payoff2 <- function(PayoffMatrix) {
  max_secured_payoff2 <- apply(PayoffMatrix, 2, max)
  return(min(max_secured_payoff2))
}


#Calculation and printing of the Maximum Secured Payoff for Player 2
max_secured_payoff2 <- calc_max_secured_payoff2(PayoffMatrix)
cat("The Maximum Secured Payoff for Player 2:", max_secured_payoff2, "\n")


###############################################################


###############################################################

# Question 3 (d) Construct a linear programming model for Company Sky in this game
# Question 3 (e) Produce an appropriate code to solve the linear programming model in part (d).
# Question 3 (f) Solve the game for Sky using the linear programming model and the code you constructed in parts (d) and (e). Interpret your solution.

###############################################################


library(lpSolveAPI)

SkyModel <- make.lp(0,6) #For now declaring only the objective function variables (5 inputs and 1 output = 6)

lp.control(SkyModel, sense ="max") #We need to maximize SKY's profit

#Setting the objective function

set.objfn(SkyModel, c(0,0,0,0,0,1)) #Setting the Objective Function.


#Setting the Constraints

add.constraint(SkyModel, c(1,-1,-1,-1,-1,1),"<=", 0) # GIANT Chooses Strategy 1 this constraint is the expected payoff for SKY

add.constraint(SkyModel, c(1,1,-1,-1,-1,1),"<=", 0) # GIANT Chooses Strategy 2 this constraint is the expected payoff for SKY

add.constraint(SkyModel, c(1,1,1,-1,-1,1),"<=", 0) # GIANT Chooses Strategy 3 this constraint is the expected payoff for SKY

add.constraint(SkyModel, c(1,1,1,1,-1,1),"<=", 0) #GIANT Chooses Strategy 4 this constraint is the expected payoff for SKY

add.constraint(SkyModel, c(-1,-1,-1,-1,1,1),"<=", 0) #GIANT Chooses Strategy 5 this constraint is the expected payoff for SKY

add.constraint(SkyModel, c(1,1,1,1,1,0), "=", 1) #Sum of probabilites constraints, always equal to 1

# Setting the Boundaries

set.bounds(SkyModel, lower = c(0,0,0,0,0,-Inf)) #Boundaries of the Model 

# Defining Labels for the LP model 
# Row Names

RowNames <- c("Row1","Row2", "Row3", "Row4", "Row5", "Row6") #Defining the Row Names

#Column Names 

ColNames <- c("X1","X2","X3","X4","X5","V") #Defining the Column Names

#Adding the Row and Columns Names to the model

dimnames(SkyModel) <- list(RowNames,ColNames)

SkyModel

solve(SkyModel) #The game has arrived at an optimal solution 

get.objective(SkyModel) #0

get.variables(SkyModel) # 0.5 0.0 0.0 0.0 0.5 0.0



