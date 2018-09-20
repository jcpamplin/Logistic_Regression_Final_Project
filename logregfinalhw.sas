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
%let AllVar = markup Estimated_Years_To_Complete Number_Of_Competitor_Bids Sector Region_of_Country;

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

*MODEL - backwards elimination;   
proc logistic data=training;
	class Region_of_Country Sector;
	model Win_Bid(event='Yes') = &AllVar &Competitors/ selection = backwards;
run;

*MODEL - backwards elimination;
%let backvar = markup Number_Of_Competitor_Bids Sector Region_of_Country Competitor_B Competitor_F Competitor_H Competitor_J;
proc logistic data=training;
	class Region_of_Country Sector;
	model Win_Bid(event='Yes') = &backvar;
run;

*Check for interactions - additivity;   
*Checked all combinations of two variables one by one; 
*Significant ones(alone): 
markup*Competitor_J (AIC: 118.454)
Number_Of_Competitor_Bids*Competitor_B (AIC: 117.781) 
Competitor_B*Competitor_H (AIC: 119.699);  

*Try them together next (4 combinations) 
markup*Competitor_J Number_Of_Competitor_Bids*Competitor_B (AIC:112.231)
Number_Of_Competitor_Bids*Competitor_B Competitor_B*Competitor_H (AIC: 115.268)
markup*Competitor_J Competitor_B*Competitor_H (AIC 115.348)
markup*Competitor_J Number_Of_Competitor_Bids*Competitor_B Competitor_B*Competitor_H (third not significant);
 
*Model with interactions that lower the AIC;
%let interact = markup*Competitor_J Number_Of_Competitor_Bids*Competitor_B;
%let final = markup Number_Of_Competitor_Bids Sector Region_of_Country Competitor_B Competitor_F Competitor_H Competitor_J markup*Competitor_J Number_Of_Competitor_Bids*Competitor_B;
proc logistic data=training;
	class Region_of_Country Sector;
	model Win_Bid(event='Yes') = &backvar &interact;
	output out=predicted reschi=respearson pred=phat predprobs=x;
run;

*Check linearity assumption;  
*Continuous variables: markup and Number_Of_Competitor_Bids (do I check the interaction?);

*Check partial residuals; 
/* coefficients improved than before --> modified_savbal = 0.1476*/
data predicted;
set predicted;
 working = (resp - phat)/(phat*(1 - phat));
 respart_markup = -2.5777*markup + working;
 respart_num = -1.8089*Number_Of_Competitor_Bids + working;
 run;

 /*this will take a lot of time*/
*ODS GRAPHICS on/LOESSMAXOBS=10000;/* for getting CI for data points > 5000*/
 proc sgplot data=predicted;
 scatter x=markup y= respart_markup;
 loess x=markup y=respart_markup / clm;
 reg x=markup y=respart_markup / nomarkers;
 run;

proc sgplot data=predicted;
 scatter x=Number_Of_Competitor_Bids y= respart_num;
 loess x=Number_Of_Competitor_Bids y= respart_num / clm;
 reg x=Number_Of_Competitor_Bids y= respart_num/ nomarkers;
 run;  

 *these both look great!;

*Fit the additive model;  
proc gam data=training plots=components(clm additive commonaxes);
	class Region_of_Country Sector;
	model Win_Bid(event='Yes') = param(&final) spline(markup, df=2)/ dist=binomial link=logit;
run; 

proc gam data=training plots=components(clm additive commonaxes);
	class Region_of_Country Sector;
	model Win_Bid(event='Yes') = param(&final) spline(Number_Of_Competitor_Bids, df=2)/ dist=binomial link=logit;
run; 

*Calibration curve;
proc sgplot data=predicted;
loess x=phat y=resp / smooth=0.75 interpolation=cubic clm;
lineparm x=0 y=0 slope=1 / lineattrs=(color=grey pattern=dash);
run;


*check influential points - deal with at beginning?;

proc sort data=construction;
by resp;
run;

proc logistic data=construction plots(MAXPOINTS=NONE only label)=influence;
	model resp(event='1') = &final;
run;

proc logistic data=logistic.insurance_modified plots(MAXPOINTS=NONE only label)=dpc;
	model resp(event='1') = &final;
run;

proc logistic data=logistic.construction(MAXPOINTS=NONE only label)=dfbetas;
	model INS(event='1') = &final;
run;


/* fitting */
/* roc curve brier score c stat */
proc logistic data=training plots(only)=ROC(id = prob);
	model resp(event='1') = &final / rocci; 
	score data=test out=Valpred outroc=vroc fitstat;
	roc; roccontrast;
run;


/* distribution of predicted probabilities */
proc logistic data=training noprint;
	model resp(event='1') = &final; 
	/* output predicted probabilities */
	output out=predprobs p=phat;
run;


proc logistic data=test;
	model resp(event='1') = &final;
	/* output predicted probabilities */
	output out=predprobs p=phat;
run;

/* graphics -- sorting by low to get mean(1s) - mean(0s) in the next step.
that's the coefficient of discrimination */
proc sort data=predprobs;
by descending resp;
run;

/* proc ttest will give the coefficient of discrimination.
also gives a nice histogram (with density overlaid)
and boxplots for each group */

proc ttest data=Valpred order=data;
ods select statistics summarypanel;
class resp;
var P_1;
title 'coefficient of discrimination and plots';
run;

/* classification table */
/* NOTE that SAS does leave-one-out when computing predicted probabilities,
so the table results (and youden index) are different than what I have in R */
proc logistic data=test;
	class resp;
	model resp(event='1') = &final / ctable pprob = 0 to 0.98 by 0.02;
	/* output table */
	score data=test out=Valpred outroc=vroc fitstat;
	ods output classification=classtable;
	title 'classification table';
	/* in this table:
	true positives = "number of correct events"
	true negatives = "number of correct nonevents"
	false positive = "number of incorrect events"
	false negative = "number of incorrect nonevents" */
run;



/* Youden's J statistic */
data classtable;
set classtable;
/* using 100 because sas gives these in percentages */
youden = sensitivity + specificity - 100;
/* weighted youden would be 2*(w*sens + (1-w)*spec) - 100, where w is between 0 and 100 */
run;
proc sort data=classtable;
by descending youden;
run;
proc print data=classtable;
run;
