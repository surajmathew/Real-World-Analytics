###############################################################
# title: "Suraj-Code.R"
# output: R Script
# Student Name: Suraj Mathew Thomas
# Student ID: S223509398
# Subject: SIG 718 - Real World Analytics
# Assessment: Mid Term Assessment
###############################################################


###############################################################

#------ T1. Understand the data ------#
#------ (i) Download the txt file (ENB_2023.txt) from CloudDeakin and save it to your R working directory.------#

###############################################################

#Office Computer Path - To work in Office Laptop
#setwd("C:/Users/tsuraj/OneDrive/DEAKIN_UNIVERSITY/SIG718_Real_World_Analytics/Mid_Term_Assessment/mid_term_zip_file")

#Personal Computer Path - To work in Personal Computer
setwd("/Users/surajmathewthomas/Library/CloudStorage/OneDrive-Personal/DEAKIN_UNIVERSITY/SIG718_Real_World_Analytics/Mid_Term_Assessment/mid_term_zip_file")

# ENB_2023.txt has been downloaded and saved in the working directory


###############################################################

#------ (ii) Assign the data to a matrix ------#

###############################################################

# Reading the given input txt file, transforming the structure into a matrix, 
# and assigning it to a variable


the.data <- as.matrix(read.table("ENB_2023.txt")) 

###############################################################

#------ (iii) The variable of interest is Y (Appliances). To investigate Y, generate a subset of 340 with numerical data ------#

###############################################################

set.seed(48)
my.data_df <- data.frame(the.data[sample(1:671,340),c(1:6)])
colnames(my.data_df) <- c("X1","X2","X3","X4","X5","Y") #assigning custom column names
head(my.data_df)

# Here my.data_df is a subset object that is sampling the the.data bringing it down to 340 rows of data (observations) 
# while retaining all the 6 columns
#The output is a dataframe


set.seed(48)
my.data_mat <- the.data[sample(1:671,340),c(1:6)]
colnames(my.data_mat) <- c("X1","X2","X3","X4","X5","Y") #assigning custom column names
head(my.data_mat)

#Taking a backup

my.data_mat2 <- my.data_mat
View(my.data_mat2)

# The output is Without a dataframe and retaining it as a matrix structure

###############################################################

#------ (iv) Use scatter plots and histograms to understand the relationship between each of the variables X1, X2, X3, X4, X5, and your variable of interest Y. (You should build 5 scatter plots and 6 histograms).------#

###############################################################

#------ Individual HISTOGRAMS using the base hist function ------#

# X1 - Temperature in Kitchen Area in Celsius

hist(my.data_mat[,1], 
     main = "Histogram of X1",
     labels = TRUE,
     xlab = "Temperature in Kitchen Area in Celsius", 
     ylab ="Frequency",
     ylim = c(0, 80),
     col = "brown", border = "black")

# X2 - Temperature in Kitchen Area in Celsius

hist(my.data_mat[,2], 
     main = "Histogram of X2",
     labels = TRUE,
     xlab = "Humidity in kitchen area, given as a percentage", 
     ylab ="Frequency",
     ylim = c(0, 80),
     col = "brown", border = "black")


# X3 - Temperature in Kitchen Area in Celsius

hist(my.data_mat[,3], 
     main = "Histogram of X3",
     labels = TRUE,
     xlab = "Temperature outside (from weather station), in Celsius", 
     ylab ="Frequency",
     ylim = c(0, 150),
     col = "brown", border = "black")


# X4 - Humidity outside (from weather station), given as a percentage

hist(my.data_mat[,4], 
     main = "Histogram of X4",
     labels = TRUE,
     xlab = "Humidity outside (from weather station), given as a percentage", 
     ylab ="Frequency",
     ylim = c(0, 100),
     col = "brown", border = "white")


# X5 - Humidity outside (from weather station), given as a percentage

hist(my.data_mat[,5], 
     main = "Histogram of X5",
     labels = TRUE,
     xlab = "Visibility (from weather station), in km", 
     ylab ="Frequency",
     ylim = c(0, 150),
     col = "brown", border = "black")


# Y - Appliances, energy use, in Wh

hist(my.data_mat[,6], 
     main = "Histogram of Y - Target Variable",
     labels = TRUE,
     xlab = "Appliances, energy use, in Wh", 
     ylab ="Frequency",
     ylim = c(0, 160),
     col = "brown", border = "black")


#------ SCATTER PLOTS USING PLOTLY ------#

install.packages("plotly")
library(plotly)
attach(my.data_df)

fig1 <- plot_ly(data=my.data_df, type = "scatter", x=~Y, y=~X1, mode="markers")%>%
  layout(title = 'Scatter Plot b/w Temperature in Kitchen & Appliances Energy Usage', plot_bgcolor = "#e5ecf6", xaxis =   list(title = 'Appliances, energy use, in Wh'), yaxis = list(title = 'Temperature in kitchen area, in Celsius'))
fig1

fig2 <- plot_ly(data=my.data_df, type = "scatter", x=~Y, y=~X2, mode="markers")%>%
  layout(title = 'Scatter Plot b/w Humidity in kitchen area & Appliances Energy Usage', plot_bgcolor = "#e5ecf6", xaxis =   list(title = 'Appliances, energy use, in Wh'), yaxis = list(title = 'Humidity in kitchen area, given as a percentage'))
fig2

fig3 <- plot_ly(data=my.data_df, type = "scatter", x=~Y, y=~X3, mode="markers")%>%
  layout(title = 'Scatter Plot b/w Temperature outside (from weather station) & Appliances Energy Usage', plot_bgcolor = "#e5ecf6", xaxis =   list(title = 'Appliances, energy use, in Wh'), yaxis = list(title = 'Temperature outside (from weather station), in Celsius'))
fig3

fig4 <- plot_ly(data=my.data_df, type = "scatter", x=~Y, y=~X4, mode="markers")%>%
  layout(title = 'Scatter Plot b/w Humidity outside (from weather station) & Appliances Energy Usage', plot_bgcolor = "#e5ecf6", xaxis =   list(title = 'Appliances, energy use, in Wh'), yaxis = list(title = 'Humidity outside (from weather station), given as a percentage'))
fig4

fig5 <-plot_ly(data=my.data_df, type = "scatter", x=~Y, y=~X5, mode="markers")%>%
  layout(title = 'Scatter Plot b/w Visibility (from weather station) & Appliances Energy Usage', plot_bgcolor = "#e5ecf6", xaxis =   list(title = 'Appliances, energy use, in Wh'), yaxis = list(title = 'Visibility (from weather station), in km'))
fig5

#------ SCATTER PLOTS USING BASE PLOT FUNCTION ------#

# Create a scatter plot of X1 against Y
plot(my.data_mat[, "X1"], my.data_mat[, "Y"], 
     xlab = "Kitchen Temperature in. Celsius", ylab = "Energy Usage", 
     main = "Kitchen Temperature Vs Energy Usage", col = "magenta")

# Create a scatter plot of X2 against Y
plot(my.data_mat[, "X2"], my.data_mat[, "Y"], 
     xlab = "Kitchen Humidity in. %", ylab = "Energy Usage",
     main = "Kitchen Humidity Vs Energy Usage", col = "black")

# Create a scatter plot of X3 against Y
plot(my.data_mat[, "X3"], my.data_mat[, "Y"], 
     xlab = "Outside Temperature in Celsius", ylab = "Energy Usage",
     main = "Outside Temperature Vs Energy Usage", col = "brown")

# Create a scatter plot of X4 against Y
plot(my.data_mat[, "X4"], my.data_mat[, "Y"], 
     xlab = "Outside Humidity in %", ylab = "Energy Usage",
     main = "Outside Humidity Vs Energy Usage", col = "purple")

# Create a scatter plot of X5 against Y
plot(my.data_mat[, "X5"], my.data_mat[, "Y"], 
     xlab = "Visibility from Weather station", ylab = "Energy Usage",
     main = "Visibility from Weather station  Vs Energy Usage", col = "orange")


#------ SCATTER PLOTS USING GGPLOT2 ------#

install.packages("ggplot2")
library(ggplot2)
attach(my.data_df)

ggplot(my.data_df, aes(x = X1, y = Y)) +
  geom_point() + 
  stat_smooth(method=lm)

ggplot(my.data_df, aes(x = X2, y = Y)) +
  geom_point() + 
  stat_smooth(method=lm)


ggplot(my.data_df, aes(x = X3, y = Y)) +
  geom_point() + 
  stat_smooth(method=lm)

ggplot(my.data_df, aes(x = X4, y = Y)) +
  geom_point() + 
  stat_smooth(method=lm)


ggplot(my.data_df, aes(x = X5, y = Y)) +
  geom_point() + 
  stat_smooth(method=lm)


#------ HISTOGRAMS IN ONE VIEW FOR REPORTING PURPOSE ------#

install.packages("rcompanion")
library(rcompanion)

par(mfrow=c(2,3))

plotNormalHistogram(my.data_mat[,1], main = "Temp in kitchen area",
                    xlab= "Temperature Kitchen (in Celsius)", breaks=5, las=1, xlim=c(14,26), col="orange")

plotNormalHistogram(my.data_mat[,2], main = "Humidity in kitchen area",
                    xlab= "Humidity Kitchen (in Percentage)", breaks=5, las=1, xlim=c(25,55), col="orange")

plotNormalHistogram(my.data_mat[,3], main = "Temp outside",
                    xlab= "Temperature Outside (in Celsius)", breaks=5, las=1, xlim=c(0,7),col="orange")

plotNormalHistogram(my.data_mat[,4], main = "Humidity outside",
                    xlab= "Humidity Outside (in Percentage)", breaks=5, las=1, xlim=c(60,100),col="orange")

plotNormalHistogram(my.data_mat[,5], main = "Visibility",
                    xlab= "Visibility from Station (in km)", breaks=5, las=1, xlim=c(10,70), col="orange")

plotNormalHistogram(my.data_mat[,6], main = "Energy Use",
                    xlab= "Energy Used (in Wh)", breaks=5, las=1,xlim=c(0,200),col="green")



#------ SCATTER PLOTS IN ONE VIEW FOR REPORTING PURPOSE ------#

par(mfrow=c(2,3))


plot(my.data_mat[,6], my.data_mat[,1], main="Scatter plot b/w Energy use and
     Temperature in kitchen area", xlab="Energy Use (in Wh)", 
     ylab="Temperature (in Celsius)", col="maroon")


plot(my.data_mat[,6], my.data_mat[,2], main="Scatter plot b/w Energy use and
     Humidity in kitchen area", xlab="Energy Use (in Wh)", 
     ylab="Humidity (in Percentage)", col="maroon")

plot(my.data_mat[,6], my.data_mat[,3], main="Scatter plot b/w Energy use and
     Temperature outside", xlab="Energy Use (in Wh)", 
     ylab="Temperature (in Celsius)", col="maroon")

plot(my.data_mat[,6], my.data_mat[,4], main="Scatter plot b/w Energy use and
     Humidity outisde", xlab="Energy Use (in Wh)", 
     ylab="Humidity (in Percentage)", col="maroon")

plot(my.data_mat[,6], my.data_mat[,5], main="Scatter plot b/w Energy use and
     Visibility", xlab="Energy Use (in Wh)", 
     ylab="Visibility (in km)", col="maroon")


###############################################################

#------ T2. Transform the data ------ #

#------ Choose any FOUR variables from the five variables X1, X2, X3, X4, X5.------ #

#------ Make appropriate transformations so that the values can be aggregated in order to predict. ------ #


###############################################################

# Performing the Normality Test to analyze the data distribution column wise

for( x in 1:6) {
  pvalues<-ks.test(jitter(my.data_mat[,x]), "pnorm", mean=mean(my.data_mat[,x]), sd=sd(my.data_mat[,x]))
  print(pvalues)
}

# One Sample Kolmogorov-Smirnov Test

#X1 pvalue = 0.2185
#X2 pvalue = 0.2698
#X3 pvalue = 0.01134
#X4 pvalue = 0.09223
#X5 pvalue = 8.807e-05
#Y pvalue = 1.554e-15

#X3, X5 and Y are less than 0.05.
#X1, X2, X4 are greater than 0.05

#Therefore, since X1, X2 and X4 are greater than 0.05, there is no significant difference between normal distribution and the distribution of X1, X2 and X4. Since these are almost normally distributed, (we can cross verify this using the histogram that we generated above) we need not perform any polynomial or log transformation.


# X3, X5 and Y looks a little skewed in the histogram and also the KS test says that they are not  normally distributed. Hence let us test the skewness to see what is the transformation we need to apply.

install.packages("moments")
library(moments)

v_skew1<-skewness(my.data_mat[,1])
v_skew2<-skewness(my.data_mat[,2])
v_skew4<-skewness(my.data_mat[,4])


v_skew3<-skewness(my.data_mat[,3])
v_skew5<-skewness(my.data_mat[,5])
v_skew6<-skewness(my.data_mat[,6])

#For Skewness let's look at how we can analyse

#Skewness = 0, then it is perfect normal distribution
#Skewness is between -0.5 and 0.5, then we approximate it to a normal distribution
#Skewness > 0.5 then positively skewed
#Skewness < 0.5 then negatively skewed

print(v_skew1) #0.3572175 #can be approx to normal distribution
print(v_skew2) #0.4270809 #can be approx to normal distribution
print(v_skew4) #-0.4451346 #can be approx to normal distribution
print(v_skew3) #0.09757824 #can be approx to normal distribution 
print(v_skew5) #0.7172966 #positively skewed - We need to apply log transformation
print(v_skew6) #1.798201 #positively skewed - We need to apply log transformation

#Standard Deviation 

sd_X1 <- sd(my.data_df$X1)
print(paste("The standard deviation of X1 [Temperature in kitchen area] is:", sd_X1))

sd_X2 <- sd(my.data_df$X2)
print(paste("The standard deviation of X2 [Humidity in kitchen area] is:", sd_X2))

sd_X3 <- sd(my.data_df$X3)
print(paste("The standard deviation of X3 [Temperature Outside] is:", sd_X3))

sd_X4 <- sd(my.data_df$X4)
print(paste("The standard deviation of X4 [Humidity Outside] is:", sd_X4))

sd_X5 <- sd(my.data_df$X5)
print(paste("The standard deviation of X5 [Visibiity] is:", sd_X5))

# Boxplot

par(mfrow=c(2,5))
for (i in 1:length(my.data_df)) {
  boxplot(my.data_df[,i], main=names(my.data_df[i]), type="l")
}

# Checking for what transformation needs to be done | Finding the Pearson Correlation


#Pearson

pearson_output <- array(0,5)

for(y in 1:5){
  pearson_output[y] <- cor(my.data_mat[,6], my.data_mat[,y], method = "pearson")
}

pearson_output # 0.3579697 0.1694812 0.4965041 0.1075600 0.3152205



pearson_max_result <- which(pearson_output == max(pearson_output))

pearson_max_result # 0.4965041

cat("The variable X",pearson_max_result, "which is Temperature outside (from weather station) in Celsius has 
    strongest relationship with variable of interest, Y (energy consumption
    based on pearson coefficient)")



# Checking for what transformation needs to be done | Finding the Spearman Correlation

#Spearman

spearman_output <- array(0,5)

for(z in 1:5){
  spearman_output[z] <- cor(my.data_mat[,6], my.data_mat[,z], method ="spearman")
}

spearman_output #0.32754308 0.11100195 0.42428935 0.07760502 0.35049866

spearman_max_result <- which(spearman_output == max(spearman_output))

spearman_max_result #0.42428935

cat("The variable X",spearman_max_result, "which is Temperature outside (from weather station) in Celsius has 
    strongest relationship with variable of interest, Y i.e. energy consumption
    based on spearman coefficient")


# We can see that all the correlations are positive in both the spearman and pearson methods. 
# Therefore, we need not do the negation transformation.



#Choosing  FOUR variables from the FIVE variables X1, X2, X3, X4, X5.
#In both the pearson and spearman method, X4 variable has the weakest relationship with the variable of interest Y.
#Therefore, the four variables chosen are X1, X2, X3, X5.
#For the variables X1, X2 and X3, we have seen above that it can be approximated to normal distribution as skewness is between -0.5 and 0.5.
#Therefore, we will apply min-max transformation to X1, X2, X3.
#For the variable X5 and Y, we will first apply log transformation and then apply the min-max transformation as it is positively skewed.


#Applying log transformation on X5 and Y variables

log_trans_func <- function(x){
  v_result = log10(x)
  return(v_result)
}

#---- X5 ----#

my.data_mat[,5] <- log_trans_func(my.data_mat[,5])
log_transform_X5 <- my.data_mat[,5]
print(log_transform_X5)


#---- Y ----#

my.data_mat[,6] <- log_trans_func(my.data_mat[,6])
log_transform_Y <- my.data_mat[,6]
print(log_transform_Y)


#Applying min-max scaling on the log transformed variables (X5, Y) and X1, X2, X3

minmax_transform <- function(x){
  v_result_transform = (x-min(x))/(max(x)-min(x))
  return(v_result_transform)
}

my.data_mat[,1] <- minmax_transform(my.data_mat[,1])
my.data_mat[,2] <- minmax_transform(my.data_mat[,2])
my.data_mat[,3] <- minmax_transform(my.data_mat[,3])
my.data_mat[,5] <- minmax_transform(my.data_mat[,5])
my.data_mat[,6] <- minmax_transform(my.data_mat[,6])

#Plotting the Histogram after Transformation

par(mfrow=c(2,3)) 

plotNormalHistogram(my.data_mat[,1], main = "Histogram of Temperature in kitchen after Min-Max transformation",
                    xlab= "Temperature (in Celsius)",las=1,col="violet")

plotNormalHistogram(my.data_mat[,2], main = "Histogram of Humidity in kitchen area after Min-Max transformation",
                    xlab= "Humidity (in Percentage)", las=1,col="violet")

plotNormalHistogram(my.data_mat[,3], main = "Histogram of Temperature outside after Min-Max transformation",
                    xlab= "Temperature (in Celsius)", las=1, col="violet")

plotNormalHistogram(my.data_mat[,5], main = "Histogram of Visibility after Log and Min-Max transformation",
                    xlab= "Visibility (in km)", las=1,col="violet")

plotNormalHistogram(my.data_mat[,6], main = "Histogram of Energy use after Log and Min-Max Transformation",
                    xlab= "Energy (in Wh)", las=1, col="violet")

# We can now see in the plot that all the selected variables are almost normally distributed

View(my.data_mat)

# We can see that X1, X2, X3, X5 and Y are transformed. X4 still remains in the matrix. Let us go ahead and remove that

my.data_mat <- my.data_mat[,-4]
View(my.data_mat)

# Let us next output this transformed dataset into a txt file

write.table(my.data_mat, "SurajMathew-transformed.txt")

###############################################################

#------ The following tasks are based on the saved transformed data. ------#

#------ T3. Build models and investigate the importance of each variable. ------#

#------ (i) Download the AggWaFit.R file (from CloudDeakin) to your working directory and load into the R workspace using, source("AggWaFit718.R") ------#

###############################################################


# Loading the Custom Package 

source("AggWaFit718.R")

install.packages("lpSolve")
library(lpSolve)


df_transformed <- as.matrix(read.table("SurajMathew-transformed.txt"))
View(df_transformed)

###############################################################

#------ (ii) Use the fitting functions to learn the parameters for ------#

# a. A weighted arithmetic mean (WAM),
# b. Weighted power means (WPM) with p = 0.5,
# c. Weighted power means (WPM) with p = 2,
# d. An ordered weighted averaging function (OWA).
# e. The Choquet integral


###############################################################

# a. A weighted arithmetic mean (WAM)

fit.QAM(df_transformed, "WAMoutput.txt","WAMstats.txt")

# b. Weighted power means (WPM) with p = 0.5

fit.QAM(df_transformed, "WPM_P0.5_output.txt", "WPM_P0.5_stats.txt", g=PM05, g.inv =invPM05)

# c. Weighted power means (WPM) with p = 2

PM2 <- function(x) {x^2}
invPM2 <-function(x) {x^(1/2)}

fit.QAM(df_transformed, "WPM_P2_output.txt", "WPM_P2_stats.txt", g=PM2, g.inv =invPM2)

# d. An ordered weighted averaging function (OWA)

fit.OWA(df_transformed, "OWAoutput.txt", "OWAstats.txt")

# e. The Choquet integral

fit.choquet(df_transformed, "Choquetoutput.txt", "Choquetstats.txt")

###############################################################

#------T4. Use your model for prediction.------#
#------Using your best fitting model from T3, predict Y (the area) for the following input------#
#------X1=22; X2=38; X3=4; X4=88.2, X5=34.------#
#------You should use the same preprocessing as in Task 2.------#
#------Compare your prediction with the measured value of Y, Y=100.------#

###############################################################


#Comparing all the models that were fitted in T3, the choquet model is the best model because
# 1) It has the lowest RMSE 0.154012557951176
# 2) It has the lowest Average Absolute Error 0.118444291784058
# 3) Highest Pearson Correlation Coefficient 0.640774620517393
# 4) Highest Spearman Correlation Coefficient 0.585900862326587


#We will now fit the new data into the Choquet Model.

new_data <- c(22,38,4,34)

# Applying the same transformation to the columns that was done in T2

transform_new_data <- new_data

#Min Max Scaling
transform_new_data[1] <- (new_data[1]-min(my.data_mat2[,1]))/(max(my.data_mat2[,1])-min(my.data_mat2[,1]))
transform_new_data[2] <- (new_data[2]-min(my.data_mat2[,2]))/(max(my.data_mat2[,2])-min(my.data_mat2[,2]))
transform_new_data[3] <- (new_data[3]-min(my.data_mat2[,3]))/(max(my.data_mat2[,3])-min(my.data_mat2[,3]))

#Log Transformation
transform_new_data[4] <- log_trans_func(new_data[4])

#Min Max Scaling
transform_new_data[4] <- (transform_new_data[4]-min(log_transform_X5))/(max(log_transform_X5)-min(log_transform_X5))


View(transform_new_data)
head(transform_new_data) #0.7000000 0.3248195 0.5333333 0.4736989

# The above transformed data row will be applied to the selected Choquet Integral Model.
# The Choquet model is our selected model because of the above mentioned evalution criteria

Choquet_weights <- c(0.507007038664365,
                     0,
                     0.650754540090088,
                     0.768673598432682,
                     0.999999999999814,
                     0.768673598432682,
                     1.00000000000007,
                     0.422296984735332,
                     0.507007038664366,
                     0.503401599359623,
                     0.6507545400899,
                     0.900168816161214,
                     0.999999999998843,
                     0.900168816161137,
                     0.999999999998566)


choq_model_out <- choquet(transform_new_data, Choquet_weights)

choq_model_out #0.6178345


choq_model_transformed_out <- choq_model_out*(max(log_transform_Y)-min(log_transform_Y))+min(log_transform_Y)


choq_model_transformed_out #1.918865


choq_model_predicted_out <- 10^(choq_model_transformed_out)

choq_model_predicted_out #82.95919

#Measured value of Y is 100 - given in the question
#The predicted value of Y is 82.95919 - which is a pretty good prediction


###############################################################################

#We will now check the application of linear regression model on df_transformed (the dataset we are working on)

View(df_transformed) #this is the transformed selected dataset (X1,X2,X3,X5,Y)

#Let us now translate this into a dataframe. The original df_transformed is a matrix

df_transformed_frame <- as.data.frame(df_transformed)

View(df_transformed_frame)

#Performing the lm fitment. Here [,5] is the Y element

lm_fit <- lm(df_transformed_frame[,5]~df_transformed_frame[,1]+df_transformed_frame[,2]+df_transformed_frame[,3]+df_transformed_frame[,4],
              data = df_transformed_frame)

summary(lm_fit)

#Next we will visualize the predicted Y values Vs the original Y values

View(lm_fit)

lm_model_data <- as.matrix(lm_fit[["model"]])

View(lm_model_data)

#Original/Given/True Values

lm_model_true_values <- lm_model_data[,1]

View(lm_model_true_values)

#Predicted Values

lm_model_predicted_values <- lm_fit[["fitted.values"]]

View(lm_model_predicted_values)


par(mfrow=c(1,1))

plot(lm_model_true_values, lm_model_predicted_values, main="Scatter plot b/w original values and
     predicted values for linear model", xlab="Original Values", 
     ylab="Predicted Values", col="black" )

abline(lm(lm_model_true_values~lm_model_predicted_values), 
       col="red", lty=1) 


#Let us now fit the Choquet Model
# Reading from the previously generated file for Choquet Integral Model above

choquet_model_output_data <- as.matrix(read.table("Choquetoutput.txt"))
View(choquet_model_output_data)

ch_model_true_values <- choquet_model_output_data[,5]
ch_model_predicted_values <- choquet_model_output_data[,6]
 
par(mfrow=c(1,1))

plot(ch_model_true_values, ch_model_predicted_values , main="Scatter plot b/w original values and
     predicted values for best fitting model (Choquet Integral Model)", xlab="Original Values", 
     ylab="Predicted Values", col="blue" )

abline(lm(ch_model_true_values~ch_model_predicted_values), 
       col="pink", lty=1) 


#We will now visualize both the original values and predicted values for the two models together

par(mfrow=c(1,2))

plot(lm_model_true_values, lm_model_predicted_values, main="Scatter plot b/w original values and
     predicted values for linear model", xlab="Original Values", 
     ylab="Predicted Values", col="maroon" )

abline(lm(lm_model_true_values~lm_model_predicted_values), 
       col="black", lty=1)


plot(ch_model_true_values, ch_model_predicted_values , main="Scatter plot b/w original values and
     predicted values for best fitting model (Choquet Integral)", xlab="Original Values", 
     ylab="Predicted Values", col="navy" )

abline(lm(ch_model_true_values~ch_model_predicted_values), 
       col="red", lty=1)


#Predicted Value Comparison b/w the two models (linear and choquet)

par(mfrow=c(1,1))


plot(ch_model_predicted_values, lm_model_predicted_values , 
     main="Scatter plot between predicted values for both models (Choquet Integral and Linear Model)", xlab="Choquet Predicted Values", 
     ylab="Linear Predicted Values", col="navy" )

abline(lm(ch_model_predicted_values~lm_model_predicted_values), 
       col="red", lty=1)




