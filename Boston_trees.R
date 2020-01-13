# Regression Trees #
# Fitting fit a regression tree to the Boston data set #
 library (MASS)
 library (tree)
 set.seed (1)
 train = sample (1: nrow(Boston ), nrow(Boston )/2)
 tree.boston =tree(medv~.,Boston ,subset =train)
# the deviance is simply the sum of squared errors for the tree
 summary (tree.boston )


 plot(tree.boston )
 text(tree.boston ,pretty =0)
 
 # The tree predicts a median house price  of $46,400 for larger homes
 # in suburbs in which residents have high socioeconomic
 # status (rm>=7.437 and lstat<9.715).

 # cv.tree() function used to see whether pruning the tree will improve
 # performance.

 cv.boston =cv.tree(tree.boston )
 plot(cv.boston$size ,cv.boston$dev)
 plot(cv.boston$size ,cv.boston$dev ,type="b")

# Optional , try with different best
 prune.boston =prune.tree(tree.boston ,best =7)
 plot(prune.boston )
 text(prune.boston ,pretty =0)


 yhat=predict (prune.boston ,newdata =Boston [-train ,])
 boston.test=Boston [-train ,"medv"]
 plot(yhat ,boston.test)
 abline (0,1)
 # Squaring the residuals, averaging the squares, 
 # and taking the square root gives us the r.m.s error. 
 #You then use the r.m.s. error as a measure of the spread of 
 # the y values about the predicted y value.
 
 mean((yhat -boston.test)^2)
 sqrt(mean((yhat -boston.test)^2))
 #  RMS error is measured on the same scale, with the same units as y
 #
 # The square root of the MSE is therefore around 5.005, 
 # indicating  that this model leads to test predictions that 
 # are within around $5,005 of  the true median home value 
 # for the suburb.
 
 