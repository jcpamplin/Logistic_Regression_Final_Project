/*--------------------------------*/
/*  MSA 2019: Logistic Regression */
/*           Final HW             */
/*                                */
/*         Homework Team 9        */
/*--------------------------------*/

libname logistic 'C:\Users\14lmu\OneDrive\Documents\LogisticRegressionData\MSA2019LogisticData\data';
run;

*first change winbid to binary variable that records as a 0 or 1; 

data logistic.construction_new;
set logistic.construction;
if win_bid = "Yes" then resp = 1;
else resp = 0;
run; 

*Fit a model with all variables from assignment;  
%let Competitors = Competitor_A Competitor_B Competitor_C Competitor_D Competitor_E Competitor_F Competitor_G Competitor_H Competitor_I Competitor_J;
%let AllVar = Estimated_Cost__Millions_ Estimated_Years_To_Complete Bid_Price__Millions_ Sector Region_of_Country Number_Of_Competitor_Bids Winning_Bid_Price__Millions_;
*Region_of_Country;
proc logistic data=logistic.construction_new;
	class Region_of_Country Sector;
	model resp(event='1') = &AllVar &Competitors/ plcl plrl;
run;
title; 
*Almost ALL insignificant here; 
*AIC: 69.391; 

*Probably some multicollinearity going on, so let's explore; 
proc reg data=logistic.construction_new;
	model resp = &AllVar &Competitors / tol vif;
run;
quit;
*huge multicollinearity with Winning_Bid_Price__Millions_ Bid_Price__Millions_ and Estimated_Cost__Millions_;
*This is obvious, but these seem like they should be really important factors; 
*If the winning bid and our bid match, then that means we won. 
*Could we take the difference between these and see how off we are on losing bids?;

***Traditional Selection Methods***;
*Backward selection; 
proc logistic data=logistic.construction_new;
	class Region_of_Country Sector;
	model resp(event='1') = &AllVar &Competitors/ selection = backwards;
run;
*AIC: 50.762, only two variables included;

*Forward selection; 
proc logistic data=logistic.construction_new;
	class Region_of_Country Sector;
	model resp(event='1') = &AllVar &Competitors/ selection = forward;
run;
*AIC: 230.556, lots of variables included;

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
*Intial AIC 69.391: 
*Removed Competitors C,G,H,I: AIC: 61.243;
*Removed Competitors D and E: AIC: 57.163; 
*Removed Sector: AIC: 59.704 - added back in, is there a way to just remove some sectors?;
*Remove Estimated_Years_To_Complete: AIC: 55.418;
*Remove Competitor B: AIC: 54.116;  
*Try removing sector again: AIC: 56.814 add back in; 
*Try removing region: AIC: 59.549 add back in;
*This could be improved further by removing some sections of sector and region;  

*check for interactions;

*check for additivity;

*check assumptions; 

*check influential points

