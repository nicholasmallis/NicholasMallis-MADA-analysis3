The R script for this week's assessment can be found in the analysis_code folder and it's called Exercise11.Rmd. If you knit that rmd, you will see my results. The contents of this exercise include code, output, and figures for fitting the null models and Decision Tree. I unfortunately could not get my LASSO or Random Forest models to work. Everytime I try to run tune_grid(), R crashes. I tried multiple ways and seeked help, but nothing would work.I'm not sure if it's my computer. I have a seperate R script called LASSO and RF.R which includes include code, output, and figures for fitting the null models, LASSO, and Random Forests. While these do not work on my computer, maybe they will on yours?

These folders contain code on processing, exploration, and analysis  

The processing.Rmd file loads in and cleans the data.

The processed data contains 730 observations and 32 variables.

Here is the list of variables. Our main outcomes and main predictor are labled as such. With the exception of BodyTemp, all variables are categorical, each indicating the prescence of a certain symptom. In some variables, there are multiple levels of severity for these categorical variables. The Body Temp variable is continuous.

SwollenLymphNodes 
ChestCongestion   
ChillsSweats      
NasalCongestion   
CoughYN           
Sneeze            
Fatigue         
SubjectiveFever   
Headache         
Weakness          
WeaknessYN       
CoughIntensity    
CoughYN2          
Myalgia           
MyalgiaYN        
RunnyNose (main predictor)       
AbPain            
ChestPain        
Diarrhea          
EyePn             
Insomnia          
ItchyEye          
Nausea (main outcome)           
EarPn             
Hearing          
Pharyngitis      
Breathless        
ToothPn           
Vision            
Vomit             
Wheeze            
BodyTemp (main outcome)


The exploration.Rmd file explores the data with plots and tables that show descriptive statistics

The Analysis.Rmd file runs linear and logistic models and explores model fits.

The Model Evaluation.R script runs evaluations for both the simple and complex models for the categorical outcome. It also runs evaluations for both the simple and complex models for the continuous outcome.

