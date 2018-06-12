# TitanicKaggle
My attempts to solve the titanic problem on kaggle.


Files Description:
train.csv, test.csv -> The problem input files. 

script.R -> No ML algorithm applied in this attempt, results obtained only through observing the data statistically.
Accuracy: 0.78468

Imputed the missing ages in training data using the mean value of the same (Pclass, Sex and Survived) value people with known age values. Plotting and observing the survival with Sex, Age, fare and Pclass observed that almost all females survived and most males died with a few exceptional categories observed:
1. females with 20+$ Fare and 3rd Pclass did not survive mostly (only about 0.1 survival rate comapred to around 7.5 for females generally.
2. Males under 18 age belonging to the 1st and 2nd class mostly survived.
For the output, set the survival of all females in test set to 1 and all males to 0 and according to the exceptional categories stated above, set the corresponding categoy females and males survival to 0 and 1 respectively.

script2.R -> Applied CART Decision tree model for prediction.
Accuracy: 0.78468

Imputed missing ages using same D-tree algo, performed feature engineering to extract the titles from passenger names and
their familyID (whole family might have sticked together during the disaster so correlated) and then applied rpart model to predict the test results.

script3.R -> Conditional Forest Implementation
Accuracy: 0.77033

sript4.R -> XGBoost Implementation.
Accuracy: 0.77511

script5.R -> Logistic Regression
Accuracy: 0.77511

script6.R -> Ensembled the previous best 5 results using the mean value of their output results for this result.
Accuracy: 0.75119

script7.R -> Conditional Forest applied again, with different parameters.
Accuracy: 0.78947
After statistical observation, removed some redundant features from the prediction model.

script8.R -> Random Forest algorithm applied on the features selected in previous model (after removing redundant ones) and with the Passenger Class feature treated as a factor instead of a numeric.
Accuracy: 0.79904

script9.R -> Slight changes to previous script, considering some more features. Random Forest again.
Accuracy: 0.79425

script10.R -> Tried tuning the random forest parameters in script9.R to improve accuracy.
Accuracy: 0.79904 (Improved compared to script9 but similar to script8 which considered less features).

