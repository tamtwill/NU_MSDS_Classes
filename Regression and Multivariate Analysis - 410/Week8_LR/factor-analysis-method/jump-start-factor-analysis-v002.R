# From Principal Components to Factor Analysis (R)

# programmed by Thomas W. Miller, July 2017

# obtain data from public-domain source at Stanford University
# 240 student participants' self-ratings on 32 personality characteristics
# a review of these data suggests that student survey participants were
# given an adjective check-list with instructions to self-rate such as:
# "Rate the extent to which each adjective describes you. Use a 
# 1-to-9 scale, where 1 means 'very much unlike me' and 
# 9 means 'very much like me.' " 

# source: http://www.stanford.edu/class/psych253/data/personality0.txt
# create data frame from the text file 
student_data <- read.table("student_data.txt")

# show names in original data file, abbreviated English names
print(names(student_data))

# assign English variable names to make reports easier to comprehend
colnames(student_data) <- c("distant", "talkative", "careless", "hardworking", 
"anxious", "agreeable", "tense", "kind", "opposing", "relaxed",
"disorganized", "outgoing", "approving", "shy", "disciplined", 
"harsh", "persevering", "friendly", "worrying", "responsive",
"contrary", "sociable", "lazy", "cooperative", "quiet",   
"organized", "critical", "lax", "laidback", "withdrawn",
"givingup", "easygoing")

# for fun, consider adding your own data to the student_data
# data frame by self-rating the 32 adjectives on a 1-to-9 scale ...
# this would provide 241 observations
# for example...
# my_data <- 
#     data.frame(distant = 1, talkative = 5, careless = 1, 
#     hardworking = 8, anxious = 2, agreeable = 6, tense = 1, 
#     kind = 7, opposing = 3, relaxed = 5, disorganized = 4, 
#     outgoing = 5, approving = 3, shy = 1, disciplined = 5, 
#     harsh = 1, persevering = 9, friendly = 7, worrying = 3, 
#     responsive = 6, contrary = 2, sociable = 6, lazy = 1, 
#     cooperative = 8, quiet = 3, organized = 6, critical = 5, 
#     lax = 2, laidback = 5, withdrawn = 1, givingup = 1, easygoing = 6)
# student_data <- rbind(student_data, my_data)
# show the structure of 241-row data frame
# print(str(student_data))
  
# write data to comma-delimited text file for use with other programs
write.table(student_data, file = "student_data.csv", sep = ",",
            eol = "\r\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE)
            
cat('\n----- Summary of Input Data -----\n\n') 
           
# show the structure of the data frame
print(str(student_data)) 
print(summary(student_data))

# explore relationships between pairs of variables
# with R corrplot visualization of correlation matrix
# ensure that corrplot is installed prior to using library command
library(corrplot)
corrplot(cor(student_data), order = "hclust", tl.col='black', tl.cex=.75)  

cat('\n----- Principal Components Analysis -----\n\n')

# principal components analysis across all 32 variables
pca <- princomp(student_data, cor = T) 

# show structure of the pca object... a list
print(str(pca))

# show summary of pca solution
print(summary(pca))

# show variable loadings on principal components
print(loadings(pca))

# show the scree plot for the pricipal component analysis
plot(pca)

cat('\n----- Factor Analysis (Unrotated) -----\n\n')

# there is much psychological research about what are called
# the big five factors of perosnality:
# extraversion, agreeableness, conscientiousness, neuroticism, openness
#
# some personality researchers have focused on only two factors:
# extraversion/introversion and neuroticism

# suppose we think five factors will be sufficient 
# we begin with an unrotated orthogonal solution
# unrotated factor analysis with five factors 
# maximum likelihood estimation is employed
fa_unrotated <- factanal(x = student_data, factors = 5, rotation = "none")

# show structure of the unrotated solution, the list object
print(str(fa_unrotated))

# uniqueness represents the proportion of variable variance 
# that is unique to the variable, not shared with other variables
cat('Variable uniquenesses: ', round(fa_unrotated$uniquenesses, digits = 3))

# communality represents the proportion of variable variance
# that is common to the factor analytic solution
# 1 - uniqueness 
cat('Variable communalities: ', round(1 - fa_unrotated$uniquenesses, digits = 3)) 

# show the loadings of the variables on the factors
# for the unrotated maximum likelihood solution
print(loadings(fa_unrotated)) 

# note that unrotated solutions are often difficult to interpret
# so we employ an orthogonal rotation called varimax
# that is, we rotate factor axes while maintaining their
# orthogonality (factor scores remain uncorrelated),
# and we do it in a way that maximizes the sum of the
# variances of the factor loadings....
# this has the effect of moving individual loadings
# in the direction of plus/minus one or zero, so a variable
# is either strongly associated with a factor or not....
# when loadings of variables on factors are either
# plus/minus one or zero, it is easier to interpret
# the factor analytic solution

cat('\n----- Factor Analysis (Varimax-Rotated) -----\n\n')

# unrotated factor analysis with five factors 
# maximum likelihood estimation is employed
# here we also estimate factor scores with regression
fa_varimax <- factanal(x = student_data, factors = 5, 
    rotation = "varimax", scores = "regression")    

# show structure of the rotated solution, the list object
print(str(fa_varimax))

# uniqueness represents the proportion of variable variance 
# unaffected by rotation
# that is unique to the variable, not shared with other variables
print(round(fa_varimax$uniquenesses, digits = 3))

# communality represents the proportion of variable variance
# unaffected by rotation
# that is common to the factor analytic solution
# 1 - uniqueness 
print(round(1 - fa_varimax$uniquenesses, digits = 3)) 

# show the loadings of the variables on the rotated factors
# for the rotated maximum likelihood solution
# rotation makes a big difference here
print(loadings(fa_varimax)) 

# try interpreting the varimax-rotated solution...
# to what extent does it match up with the big five
# personality factors...
# if the factors do not match up with the big five,
# try naming the identified factors yourself

# naming of factors is as much art as science...
# refer to the matrix of factor loadings and
# note the variables that have the highest
# positive and negative loadings on each factor...
# then come up with a words that describe these variables

# if you had added your own data to the survey responses,
# consider plotting your personality profile,
# describing yourself in terms of the five factors
# identifed through the varimax rotation

# add factor scores to the data frame, resulting in five new 
# variables or columns for analysis
student_data$varimax1 <- fa_varimax$scores[,'Factor1']
student_data$varimax2 <- fa_varimax$scores[,'Factor2']
student_data$varimax3 <- fa_varimax$scores[,'Factor3']
student_data$varimax4 <- fa_varimax$scores[,'Factor4']
student_data$varimax5 <- fa_varimax$scores[,'Factor5']

# demonstrate that estimated factor scores from the
# varimax-rotated factor analytic solution  
# are themselves uncorrelated (within rounding error)
varimax_factor_cormat <- cor(student_data[, c("varimax1", "varimax2", 
     "varimax3", "varimax4", "varimax5")])
print(round(varimax_factor_cormat, digits = 3))

# note that scores on factors are not fully determined by 
# a factor analytic solution... factor scores are "indeterminate"  
# this means that there is an infinite number of ways of assigning 
# factor scores for any given set of factor loadings...
# this is yet another reason for statisticians to decry 
# factor analytic methods, choosing to stick with unrotated 
# principal components analysis.  



     