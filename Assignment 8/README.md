Attached  are three files: One M2021train.csv with  original Professor Moody grading data and another, the M2021test_students.csv  data with missing GRADE column.  Finally M2021test_submission is the file you will submit to Kaggle of course after filling up the GRADE column.  

GRADE is just Pass/Fail. So you have to predict this binary variable. 

As you will see  again, score is not everything! Other variables play a role.


 Your job is to predict the grades in the testing file, adding to it GRADE attribute  with predicted grades.  This submission will be handled though kaggle not sakai (instructions coming). Kaggle will automatically calculate your prediction error.  In this case, of Professor Moody data, it will be a fraction of  Pass/Fail grades which you have predicted incorrectly. 

FOR THIS PREDICTION CHALLENGE:  Clean or free style prediction only, Do not use any R library ML (machine learning)  functions.  Just follow example from  last lecture slides with  myprediction vector or http://data101.cs.rutgers.edu/laboratory/pages/cleanprediction0.  

Use your own rules implmented for example like I did through the decision vector. But beware of overfitting since it may not work on testing data (remember we are predicting the future...)

WHAT TO SUBMIT:  Kaggle submission is a MUST, cannot be late - per instruction follow the attached M2018_sample_submission . You can only submit it ONCE. But you can refine your predictor on R studio as many times as you want before submitting the final csv file with predicted grades to Kaggle. ALSO - Kaggle will only be open on the last day of submision - next Friday

Submit to sakai: power points explaining what you did.  Critical - EXPLAIN HOW YOU ARRIVED AT YOUR PREDICTION MODEL
Plus  your R code.  

Submit to Kaggle the submission file with grades filled in according to your prediction model

 We will check the top solutions  for the consistency of code - i.e. did you really get such a small error using this code?

KAGGLE submission instructions and instructions how to create Kaggle account  are posted in recitation materials this week - but first build your predictor using R studio

- kaggle is the last thing you do - since it just involves submitting a simple csv file with two columns - student id and  the grade (pass/fail) which your predictor generated.
