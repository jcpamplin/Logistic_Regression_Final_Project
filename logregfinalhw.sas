/*--------------------------------*/
/*  MSA 2019: Logistic Regression */
/*           Final HW             */
/*                                */
/*         Homework Team 9        */
/*--------------------------------*/

libname logistic 'C:\Users\14lmu\OneDrive\Documents\LogisticRegressionData\MSA2019LogisticData\data';
run;

proc means data=logistic.construction nmiss n mean stddev min max; 
run; 

data logistic.construction;
set logistic.construction;
markup = ((Bid_Price__Millions_ - Estimated_Cost__Millions_)/(Estimated_Cost__Millions_))*100;
run; 

%let Competitors = Competitor_A Competitor_B Competitor_C Competitor_D Competitor_E Competitor_F Competitor_G Competitor_H Competitor_I Competitor_J;
%let AllVar = markup Estimated_Years_To_Complete Number_Of_Competitor_Bids; *Sector Region_of_Country;

data logistic.construction;
set logistic.construction;
if win_bid = "Yes" then resp = 1;
else resp = 0;
run; 

proc reg data=logistic.construction;
	model resp = &AllVar &Competitors;
run;
quit;

*Probably some multicollinearity going on, so let's explore; 
proc reg data=logistic.construction;
	model resp = &AllVar &Competitors / tol vif;
run;
quit;
*huge multicollinearity with Winning_Bid_Price__Millions_ Bid_Price__Millions_ and Estimated_Cost__Millions_;

*Create training and testing data sets;
data training testing;
	set logistic.construction;
	if ranuni(4) >= .3 then output training;
    else output testing;
run;

*Fit a model with all variables from assignment;  
proc logistic data=training;
	class Region_of_Country Sector;
	model Win_Bid(event='Yes') = &AllVar &Competitors/ plcl plrl;
run; 
*AIC: 128.457; 

***Traditional Selection Methods***;
*Backward selection; 
proc logistic data=training;
	class Region_of_Country Sector;
	model Win_Bid(event='Yes') = &AllVar &Competitors/ selection = backwards;
run;
*AIC: 121.851;

*Forward selection; 
proc logistic data=training;
	class Region_of_Country Sector;
	model Win_Bid(event='Yes') = &AllVar &Competitors/ selection = forward;
run;
*AIC: 122.412, same model;  

*Stepwise; 
proc logistic data=training;
	class Region_of_Country Sector;
	model Win_Bid(event='Yes') = &AllVar &Competitors/ selection = stepwise;
run;
*AIC: 122.412;

*Using scores ;
proc logistic data=logistic.construction;
	model Win_Bid(event='Yes') = markup Estimated_Years_To_Complete Number_Of_Competitor_Bids &Competitors/ selection=score best=1 ;
run;
*don't use this;

*Manual reduction with full model? 

*OKAY - STARTING WITH OUR MODEL FROM BACKWARDS ELIMINATION;

*check for interactions;

*check assumptions - linearity (higher order) and additivity; 

*check influential points - deal with at beginning?;

