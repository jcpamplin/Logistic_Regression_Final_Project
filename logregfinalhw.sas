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
markup = (Winning_Bid_Price__Millions_ - Estimated_Cost__Millions_)/(Estimated_Cost__Millions_);
run; 

%let Competitors = Competitor_A Competitor_B Competitor_C Competitor_D Competitor_E Competitor_F Competitor_G Competitor_H Competitor_I Competitor_J;
%let AllVar = markup Estimated_Years_To_Complete  Number_Of_Competitor_Bids Sector Region_of_Country;

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
*AIC: 175.749; 

***Traditional Selection Methods***;
*Backward selection; 
proc logistic data=training;
	class Region_of_Country Sector;
	model Win_Bid(event='Yes') = &AllVar &Competitors/ selection = backwards;
run;
*AIC: 167.884;

*Forward selection; 
proc logistic data=training;
	class Region_of_Country Sector;
	model Win_Bid(event='Yes') = &AllVar &Competitors/ selection = forward;
run;
*AIC: 167.884, same model;  

*Stepwise; 
proc logistic data=training;
	class Region_of_Country Sector;
	model Win_Bid(event='Yes') = &AllVar &Competitors/ selection = stepwise;
run;
*AIC: 182.787;

*Using scores;
proc logistic data=logistic.construction_new;
	model resp(event='1') = Estimated_Cost__Millions_ Estimated_Years_To_Complete Bid_Price__Millions_ Number_Of_Competitor_Bids Winning_Bid_Price__Millions_ &Competitors/ selection=score best=1;
run;
*Run each of these and look at the AIC - note that region and sector not included - is there a way to include these?; 
*can we tell which one of these is best based on chi square scores? 

*Manual reduction from full model and forward stage could result in better models, so let's explore that;

*Full model reduction; 
%let CompetitorsN = Competitor_A Competitor_F Competitor_J;
%let AllVarN = Estimated_Cost__Millions_  Bid_Price__Millions_ Sector Region_of_Country Number_Of_Competitor_Bids Winning_Bid_Price__Millions_;
proc logistic data=logistic.construction_new;
	class Region_of_Country Sector;
	model resp(event='1') = &AllVarN &CompetitorsN/ plcl plrl;
run;

*Forward selection model reduction; 

*taking care of multicollinearity;

*any that need transformations?;

*check for interactions;

*check for additivity;

*check assumptions; 

*check influential points - deal with at beginning?;

