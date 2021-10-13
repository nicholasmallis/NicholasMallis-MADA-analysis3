

The code folders contains code on processing, exploration, and analysis.  

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

