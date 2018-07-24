Boston Housing-Regression
================
Pedro Alarcon
7/18/2018

Table of Content
================

1.  [Introduction](#introduction)
2.  [Dara Exploration](#dataexp)
3.  [Feature Engineering](#feateng)
4.  [Machince Learning](#machlearn)
5.  [Best Subset Selection](#bestsub)
6.  [Ridge Regression](#ridgereg)
7.  [LASSO](#lasso)
8.  [Regression Trees](#regtrees)
    1.  [Bagging](#bag)
    2.  [Random Forest](#rf)
    3.  [Boosting](#boost)

<a name="introduction"></a>

``` r
library(ISLR)
library(ggplot2)
library(plotly)
library(tidyr)
library(reshape2)
library(cowplot)
library(ggExtra)
library(ggcorrplot)
library(DataExplorer)
library(scales) 
library(gtable)
library(gridExtra)
library(grid)
library(rockchalk) 
library(knitr)
library(kableExtra)
library(randomForest)
library(e1071)
library(data.table)
library(MASS)
library(nlme)
library(mgcv)
library(tree) 
library(randomForest)
library(gbm)
library(leaps)
attach(Boston)
library(glmnet)
```

<a name="dataexp"></a> We begin by performing variable identification

``` r
dim(Boston); str(Boston); 
```

    ## [1] 506  14

    ## 'data.frame':    506 obs. of  14 variables:
    ##  $ crim   : num  0.00632 0.02731 0.02729 0.03237 0.06905 ...
    ##  $ zn     : num  18 0 0 0 0 0 12.5 12.5 12.5 12.5 ...
    ##  $ indus  : num  2.31 7.07 7.07 2.18 2.18 2.18 7.87 7.87 7.87 7.87 ...
    ##  $ chas   : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ nox    : num  0.538 0.469 0.469 0.458 0.458 0.458 0.524 0.524 0.524 0.524 ...
    ##  $ rm     : num  6.58 6.42 7.18 7 7.15 ...
    ##  $ age    : num  65.2 78.9 61.1 45.8 54.2 58.7 66.6 96.1 100 85.9 ...
    ##  $ dis    : num  4.09 4.97 4.97 6.06 6.06 ...
    ##  $ rad    : int  1 2 2 3 3 3 5 5 5 5 ...
    ##  $ tax    : num  296 242 242 222 222 222 311 311 311 311 ...
    ##  $ ptratio: num  15.3 17.8 17.8 18.7 18.7 18.7 15.2 15.2 15.2 15.2 ...
    ##  $ black  : num  397 397 393 395 397 ...
    ##  $ lstat  : num  4.98 9.14 4.03 2.94 5.33 ...
    ##  $ medv   : num  24 21.6 34.7 33.4 36.2 28.7 22.9 27.1 16.5 18.9 ...

``` r
kable((Boston)[1:6,]) %>%
  kable_styling()
```

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:right;">
crim
</th>
<th style="text-align:right;">
zn
</th>
<th style="text-align:right;">
indus
</th>
<th style="text-align:right;">
chas
</th>
<th style="text-align:right;">
nox
</th>
<th style="text-align:right;">
rm
</th>
<th style="text-align:right;">
age
</th>
<th style="text-align:right;">
dis
</th>
<th style="text-align:right;">
rad
</th>
<th style="text-align:right;">
tax
</th>
<th style="text-align:right;">
ptratio
</th>
<th style="text-align:right;">
black
</th>
<th style="text-align:right;">
lstat
</th>
<th style="text-align:right;">
medv
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
0.00632
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
2.31
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.538
</td>
<td style="text-align:right;">
6.575
</td>
<td style="text-align:right;">
65.2
</td>
<td style="text-align:right;">
4.0900
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
296
</td>
<td style="text-align:right;">
15.3
</td>
<td style="text-align:right;">
396.90
</td>
<td style="text-align:right;">
4.98
</td>
<td style="text-align:right;">
24.0
</td>
</tr>
<tr>
<td style="text-align:right;">
0.02731
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
7.07
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.469
</td>
<td style="text-align:right;">
6.421
</td>
<td style="text-align:right;">
78.9
</td>
<td style="text-align:right;">
4.9671
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
242
</td>
<td style="text-align:right;">
17.8
</td>
<td style="text-align:right;">
396.90
</td>
<td style="text-align:right;">
9.14
</td>
<td style="text-align:right;">
21.6
</td>
</tr>
<tr>
<td style="text-align:right;">
0.02729
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
7.07
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.469
</td>
<td style="text-align:right;">
7.185
</td>
<td style="text-align:right;">
61.1
</td>
<td style="text-align:right;">
4.9671
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
242
</td>
<td style="text-align:right;">
17.8
</td>
<td style="text-align:right;">
392.83
</td>
<td style="text-align:right;">
4.03
</td>
<td style="text-align:right;">
34.7
</td>
</tr>
<tr>
<td style="text-align:right;">
0.03237
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2.18
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.458
</td>
<td style="text-align:right;">
6.998
</td>
<td style="text-align:right;">
45.8
</td>
<td style="text-align:right;">
6.0622
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
222
</td>
<td style="text-align:right;">
18.7
</td>
<td style="text-align:right;">
394.63
</td>
<td style="text-align:right;">
2.94
</td>
<td style="text-align:right;">
33.4
</td>
</tr>
<tr>
<td style="text-align:right;">
0.06905
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2.18
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.458
</td>
<td style="text-align:right;">
7.147
</td>
<td style="text-align:right;">
54.2
</td>
<td style="text-align:right;">
6.0622
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
222
</td>
<td style="text-align:right;">
18.7
</td>
<td style="text-align:right;">
396.90
</td>
<td style="text-align:right;">
5.33
</td>
<td style="text-align:right;">
36.2
</td>
</tr>
<tr>
<td style="text-align:right;">
0.02985
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
2.18
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0.458
</td>
<td style="text-align:right;">
6.430
</td>
<td style="text-align:right;">
58.7
</td>
<td style="text-align:right;">
6.0622
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
222
</td>
<td style="text-align:right;">
18.7
</td>
<td style="text-align:right;">
394.12
</td>
<td style="text-align:right;">
5.21
</td>
<td style="text-align:right;">
28.7
</td>
</tr>
</tbody>
</table>
Our set contains 506 rows and 14 columns. There are 13 predictos, 12 of them are continuos and one is categorical. Lets subset categorical and continuos variables to perform univariate analysis.

``` r
df = Boston
categorical = Boston[,4, drop = F]
continuos = Boston[,c(1:3,5:14)]

continuos %>% gather() %>% head()
```

    ##    key   value
    ## 1 crim 0.00632
    ## 2 crim 0.02731
    ## 3 crim 0.02729
    ## 4 crim 0.03237
    ## 5 crim 0.06905
    ## 6 crim 0.02985

``` r
ggplot(gather(continuos), aes(value)) + geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')
```

![](Boston_Housing_Regression_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
ggplot(categorical, aes(chas)) + geom_bar(fill = "lightblue",stat = "count" ,aes(y = (..count..)/sum(..count..))) + scale_y_continuous(labels=scales::percent) + ylab("rel frequencies") + theme(axis.text.x = element_text(angle=25, vjust=0.6)) 
```

![](Boston_Housing_Regression_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
corr = round(cor(Boston), 2)

# Plot
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of Boston Housing Data Set", 
           ggtheme=theme_bw)
```

![](Boston_Housing_Regression_files/figure-markdown_github/unnamed-chunk-5-1.png) variables rm, ptratio, and lstat are a correlation score &gt;.5 with our response variable, which makes them good candidates for predictors. In addition, tax and rad are highly correlated with each other. Meanwhile we note that Chas and dis have the lowest correlation score with our response variable.

Bi-variate Analysis
===================

Bi-variate analysis between continuos variable and predictor

``` r
# sp1 = ggplot(newdata, aes(x=chas, y=medv)) + geom_point(shape=1) +  geom_smooth(method=lm)
# l1= ggMarginal(sp1, type = "boxplot", margins = "x" ,fill="transparent")
# 
# sp2 = ggplot(newdata, aes(x=rm, y=medv)) + geom_point(shape=1) +  geom_smooth(method=lm) 
# l2 = ggMarginal(sp2, type = "boxplot", margins = "x" ,fill="transparent")
# 
# sp3 = ggplot(newdata, aes(x=tax, y=medv)) + geom_point(shape=1) +  geom_smooth(method=lm)
# l3=ggMarginal(sp3, type = "boxplot", margins = "x" ,fill="transparent")
# 
# sp4 = ggplot(newdata, aes(x=ptratio, y=medv)) + geom_point(shape=1) +  geom_smooth(method=lm)
# l4=ggMarginal(sp4, type = "boxplot", margins = "x" ,fill="transparent")
# 
# sp5 = ggplot(newdata, aes(x=black, y=medv)) + geom_point(shape=1) +  geom_smooth(method=lm) 
# l5=ggMarginal(sp5, type = "boxplot", margins = "x" ,fill="transparent")
# 
# sp6 = ggplot(newdata, aes(x=lstat, y=medv)) + geom_point(shape=1) +  geom_smooth(method=lm) 
# l6= ggMarginal(sp6, type = "boxplot", margins = "x" ,fill="transparent")
# 
# plot_grid(l1, l2, l3,l4,l5,l6,labels = "AUTO")
```

Lets check for any mising values

``` r
sum(is.na(Boston)) 
```

    ## [1] 0

<a name="feateng"></a> Looks like there are no missing values

Machine Learning
================

<a name="machlearn"></a>

Split data into 25% test and 75% training creates a 1-D vector of size that equals to 75% of the total rows in Boston data set elements in vector can vary from 1-506, the amount of rows

``` r
set.seed(1)
 
train = sample(nrow(Boston), size = nrow(Boston)*.75)
#creates a subset matrix whose rows are specifiec by the train vector
boston.train = Boston[train,] 
#creates a subset matrix whose rows are not contained in the train vector
boston.test = Boston[-train,] 
```

BEST SUBSET SELECTION
---------------------

<a name="bestsub"></a>

this models identifies a subset of p predictors that are most related to our response and fit a model using least squares regression on the reduced predictors.

``` r
regfit.best = regsubsets(medv~., data = boston.train, nvmax = 13 )
test.matrix = model.matrix(medv~., data = boston.test) #creates matrix from test data

#Since there is no predict() method for regsubsets(), we must manually
#calculate predicted values to obtain test MSE for each subset
val.errors = rep(NA, 13)
for(i in 1:13){
  coefi = coef(regfit.best, id = i)
  pred = test.matrix[,names(coefi)]%*%coefi #matric multiplication
  val.errors[i] = mean((boston.test$medv-pred)^2)
}
which.min(val.errors)
```

    ## [1] 11

``` r
bestsubset.MSE= val.errors[which.min(val.errors)]
coef(regfit.best, 11)
```

    ##   (Intercept)          crim            zn          chas           nox 
    ##  38.173889691  -0.097824363   0.046491976   2.777263394 -18.340334800 
    ##            rm           dis           rad           tax       ptratio 
    ##   3.799912979  -1.558987415   0.314514910  -0.010535051  -1.007004373 
    ##         black         lstat 
    ##   0.007760261  -0.537306307

``` r
bestsubset.MSE
```

    ## [1] 29.59027

``` r
#We can see that predictors indus and age were eliminated, however
#the test MSE is quite high, we better try other modeling techniques
```

RIDGE REGRESSION
----------------

<a name="ridgereg"></a>

``` r
x = model.matrix(medv~., Boston)[,-1]
y = Boston$medv
y.test = y[-train]

#perform cross-validation to choose tuning parameter lambda
cv.out = cv.glmnet(x[train,], y[train], alpha = 0)
plot(cv.out)
```

![](Boston_Housing_Regression_files/figure-markdown_github/unnamed-chunk-12-1.png)

``` r
bestlambda = cv.out$lambda.min #lambda that results in lowest cross validation error

#fit model to train data
grid = 10^seq(10,-2,length = 100) #from 10^10 to 10^-2 
ridge.mod = glmnet(x[train,],y[train],alpha= 0, lambda = grid, thresh = 1e-12)

#make predictions for lambda = bestlambda 
ridge.pred = predict(ridge.mod, s=bestlambda, newx = x[-train,])
ridge.MSE = mean((ridge.pred - y.test)^2)
#ridge regression did not perform better than 
```

LASSO
-----

<a name="lasso"></a>

``` r
lasso.mod = glmnet(x[train,], y[train], alpha = 1, lambda= grid)
plot(lasso.mod)
```

![](Boston_Housing_Regression_files/figure-markdown_github/unnamed-chunk-13-1.png)

``` r
cv.lasso = cv.glmnet(x[train,], y[train], alpha = 1)
bestlambda = cv.lasso$lambda.min
lasso.pred = predict(lasso.mod, s=bestlambda, newx = x[-train,])
lasso.MSE = mean((lasso.pred-y.test)^2)
```

So far, all three linear models have a test MSE around 27-28 with very little improvement. It is evident that the data is not well described by a linear relationship, hence, we now try non-linear models. \#\#GENERALIZED ADDITIVE MODEL (GAM)

``` r
#We will fit a spline on only continuos predictors
sapply(Boston, class) #displays objects data types
```

    ##      crim        zn     indus      chas       nox        rm       age 
    ## "numeric" "numeric" "numeric" "integer" "numeric" "numeric" "numeric" 
    ##       dis       rad       tax   ptratio     black     lstat      medv 
    ## "numeric" "integer" "numeric" "numeric" "numeric" "numeric" "numeric"

``` r
gam.formula = as.formula(paste("medv~  s(lstat) + s(crim) + s(zn) + s(indus)+ s(nox)+ s(rm)+ s(age)+ s(dis)+ s(ptratio)+ s(tax)+ s(black) +", paste(colnames(boston.train)[c(4,9)], collapse = "+")))
fit.gam = gam(formula = gam.formula, data = boston.train)
summary(fit.gam)
```

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## medv ~ s(lstat) + s(crim) + s(zn) + s(indus) + s(nox) + s(rm) + 
    ##     s(age) + s(dis) + s(ptratio) + s(tax) + s(black) + chas + 
    ##     rad
    ## 
    ## Parametric coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  20.1535     1.3308  15.144   <2e-16 ***
    ## chas          1.8055     0.7091   2.546   0.0114 *  
    ## rad           0.2267     0.1354   1.675   0.0950 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##              edf Ref.df      F  p-value    
    ## s(lstat)   7.443  8.404 19.040  < 2e-16 ***
    ## s(crim)    2.651  3.238 12.604 3.15e-08 ***
    ## s(zn)      7.125  8.115  1.673  0.10338    
    ## s(indus)   7.037  7.960  2.601  0.00945 ** 
    ## s(nox)     8.933  8.993  9.846 2.35e-13 ***
    ## s(rm)      3.672  4.638 34.063  < 2e-16 ***
    ## s(age)     1.000  1.000  1.056  0.30482    
    ## s(dis)     8.756  8.977  8.074 5.08e-11 ***
    ## s(ptratio) 1.000  1.000 20.542 8.16e-06 ***
    ## s(tax)     3.912  4.672  6.034 7.45e-05 ***
    ## s(black)   5.048  6.080  2.187  0.04408 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =   0.89   Deviance explained = 90.7%
    ## GCV = 10.493  Scale est. = 8.8434    n = 379

``` r
gam.pred = predict(fit.gam, newdata=boston.test) 
MSE.train.gam = mean((boston.train$medv - predict(fit.gam))^2)
MSE.test.gam = mean((boston.test$medv - gam.pred)^2)
```

REGRESSION TREE
---------------

<a name="regtrees"></a>

``` r
tree.boston = tree(medv~., data=Boston, subset=train)
summary(tree.boston) #we see that the tree only uses four variables
```

    ## 
    ## Regression tree:
    ## tree(formula = medv ~ ., data = Boston, subset = train)
    ## Variables actually used in tree construction:
    ## [1] "rm"    "lstat" "age"   "crim" 
    ## Number of terminal nodes:  10 
    ## Residual mean deviance:  12.76 = 4707 / 369 
    ## Distribution of residuals:
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## -13.260  -2.171  -0.274   0.000   2.072  14.730

``` r
plot(tree.boston)
text(tree.boston, pretty=0)
```

![](Boston_Housing_Regression_files/figure-markdown_github/unnamed-chunk-15-1.png)

``` r
#makes prediction on the test set and computes test MSE
tree.pred.test = predict(tree.boston, boston.test)
tree.test.MSE = mean((tree.pred.test - boston.test$medv)^2)

#PRUNING TREE
cv.boston = cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type = 'b') 
```

![](Boston_Housing_Regression_files/figure-markdown_github/unnamed-chunk-15-2.png)

``` r
#seems like prunning will not since cross-validation chooses the most
#complex tree
```

### BAGGING

<a name="bag"></a>

``` r
?randomForest
bag.boston = randomForest(medv~., data = Boston, subset = train, mtry=13,importance=TRUE )
bag.boston
```

    ## 
    ## Call:
    ##  randomForest(formula = medv ~ ., data = Boston, mtry = 13, importance = TRUE,      subset = train) 
    ##                Type of random forest: regression
    ##                      Number of trees: 500
    ## No. of variables tried at each split: 13
    ## 
    ##           Mean of squared residuals: 9.906612
    ##                     % Var explained: 87.68

``` r
yhat.bag = predict(bag.boston, boston.test)
bag.MSE = mean((yhat.bag-boston.test$medv)^2)
```

### RANDOM FOREST

<a name="rf"></a>

We create a for loop that will determine the number of predictor to use such that it minimizes the test MSE

``` r
predictor = 1:12 
rf.test.MSE = rep(NA, length(predictor) )
rf.train.MSE = rep(NA, length(predictor) )
for(i in predictor){
  rf.boston = randomForest(medv~., data=Boston, subset=train, mtry=i, importance = T )
  rf.train.pred = predict(rf.boston, boston.train)
  rf.test.pred = predict(rf.boston, boston.test)
  rf.train.MSE[i]= mean((boston.train$medv - rf.train.pred)^2)
  rf.test.MSE[i] = mean((boston.test$medv - rf.test.pred)^2)
}
plot(predictor, rf.test.MSE, xlab="Number of Predictor", ylab= "Random Forest Test MSE")
```

![](Boston_Housing_Regression_files/figure-markdown_github/unnamed-chunk-17-1.png)

``` r
min(rf.test.MSE) #7.446
```

    ## [1] 12.7571

``` r
predictor[which.min(rf.test.MSE)] #12 predictors
```

    ## [1] 4

``` r
rf.boston = randomForest(medv~., data=Boston, subset=train, mtry=12, importance = T )
plot(rf.boston)
```

![](Boston_Housing_Regression_files/figure-markdown_github/unnamed-chunk-17-2.png)

``` r
rf.boston
```

    ## 
    ## Call:
    ##  randomForest(formula = medv ~ ., data = Boston, mtry = 12, importance = T,      subset = train) 
    ##                Type of random forest: regression
    ##                      Number of trees: 500
    ## No. of variables tried at each split: 12
    ## 
    ##           Mean of squared residuals: 10.04493
    ##                     % Var explained: 87.51

BOOSTING
========

<a name="boost"></a>

``` r
# boost.boston = gbm(medv~., data = boston.train, distribution = "gaussian", n.trees = 5000, interaction.depth = 4)
# summary(boost.boston)
# 
# yhat.boost = predict(boost.boston, boston.test, n.trees = 5000)
# boost.MSE = mean((yhat.boost-boston.test$medv)^2)


#Find lowest boost MSE as a function of shrinking parameter
exponents = seq(from = -10, to= -0.2, by= 0.1) 
shrinking.param = 10^exponents
b.train.err = rep(NA,length(shrinking.param)) #empty vector of same size as shrinking param
b.test.err = rep(NA,length(shrinking.param))
for(i in 1:length(shrinking.param)){
  boost.boston = gbm(medv~., data = boston.train, distribution = "gaussian",
                     n.trees = 1000, interaction.depth = 4, shrinkage = shrinking.param[i])
  train.pred = predict(boost.boston, boston.train, n.trees=1000)
  test.pred = predict(boost.boston, boston.test, n.trees=1000)
  b.train.err[i] = mean((boston.train$medv - train.pred )^2)
  b.test.err[i]= mean((boston.test$medv - test.pred)^2)
}
plot(shrinking.param, b.train.err, xlab = "Shrinking Parameter", ylab = "Train Set MSE")
```

![](Boston_Housing_Regression_files/figure-markdown_github/unnamed-chunk-18-1.png)

``` r
plot(shrinking.param, b.test.err, xlab = "Shrinking Parameter", ylab = "Test Set MSE")
```

![](Boston_Housing_Regression_files/figure-markdown_github/unnamed-chunk-18-2.png)

``` r
min(b.test.err) #10.793
```

    ## [1] 10.69305

``` r
shrinking.param[which.min(b.test.err)]
```

    ## [1] 0.03981072
