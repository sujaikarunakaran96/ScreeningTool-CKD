# ScreeningTool-CKD

Introduction: 

The data set was split into train (6000 observations) and validation (2819 observations).

All the null values in the data set was removed with 4145 observations in train data and 1912 observation is validation set.

Looking at the t-statistic score and chi-squared score for the variables weight, BMI, Total Chol, Female, Obese, Dyslipidemia, and Fam Diabetes was removed as they were not statistically significant


Risk Factors: 

By running logistic regression for the previously mentioned variables, we understand that Age, Race grp, Unmarried, SBP, HDL, PVD, Hypertension, Diabetes and Anemia were found to be the risk factors which significantly increases the odd of getting CKD

Therefore, using this subset of variables a new model was used to predict CKD in the validation set


Fitting the Model: 

Using the ROC curve, since recall is more more important in the case, we try to find a threshold where we get a good recall and precision. 
We can find the threshold that will yield the best Recall and Precision to be 0.06200222.


Prediction: 

Now, using the validation set, we predict the likelihood of the Chronic Kidney Disease. 
We predict that out of 1912 people, 524 people are predicted to get chronic kidney disease due to the risk factors.
For example, A Hispanic person with HDL of 74, Age of 84, with PVD and hypertension, and without diabetes has a probability of 0.511711 against a person without all hypertension, lesser age, etc.
