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

*percent mark up;
data logistic.construction;
set logistic.construction;
markup = ((Bid_Price__Millions_ - Estimated_Cost__Millions_)/(Estimated_Cost__Millions_))*100;
run; 

data logistic.construction;
set logistic.construction;
comp = (Competitor_A + Competitor_B + Competitor_C + Competitor_D + Competitor_E + Competitor_F + Competitor_G + Competitor_H + Competitor_I + Competitor_J);
run;

%let Competitors = Competitor_A Competitor_B Competitor_C Competitor_D Competitor_E Competitor_F Competitor_G Competitor_H Competitor_I Competitor_J;
%let AllVar = markup Estimated_Years_To_Complete Sector Region_of_Country;

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
	model Win_Bid(event='Yes') = &AllVar comp/ selection = backwards;
run;
*AIC: 190;

*Forward selection; 
proc logistic data=training;
	class Region_of_Country Sector;
	model Win_Bid(event='Yes') = &AllVar comp/ selection = forward;
run;
*AIC: 190, the winner;  

*Stepwise; 
proc logistic data=training;
	class Region_of_Country Sector;
	model Win_Bid(event='Yes') = &AllVar comp/ selection = stepwise;
run;
*AIC: 190;

*MODEL - forward;
%let backvar = markup Estimated_Years_To_Complete Sector Region_of_Country comp;
proc logistic data=training;
	class Region_of_Country Sector;
	model Win_Bid(event='Yes') = &backvar;
run;

*Check for interactions - additivity;   
*Check for interactions between the biggest main effects: markup and estimated years to complete; 
*Not significant or have no logical interpretation;
%let interact = markup*Estimated_Years_To_Complete markup*comp comp*Estimated_Years_To_Complete;

*Model;
proc logistic data=training;
	class Region_of_Country Sector;
	model Win_Bid(event='Yes') = &backvar;
	output out=predicted reschi=respearson pred=phat predprobs=x;
run;

*Check linearity assumption;  
*Continuous variables: markup and Number_Of_Competitor_Bids (do I check the interaction?);

*Check partial residuals; 
/* coefficients improved than before --> modified_savbal = 0.1476*/
data predicted;
set predicted;
 working = (resp - phat)/(phat*(1 - phat));
 respart_markup = -0.5227*markup + working;
 respart_est = 0.4103*Estimated_Years_To_Complete + working;
 respart_comp = -1.0069*comp + working;
 run;
title;
 /*this will take a lot of time*/
ODS GRAPHICS on; *LOESSMAXOBS=10000 for getting CI for data points > 5000;
 proc sgplot data=predicted;
 scatter x=markup y= respart_markup;
 xaxis label="Markup"; 
 yaxis label="partial (deviance) residuals";
 loess x=markup y=respart_markup / clm;
 reg x=markup y=respart_markup / nomarkers;
 run;

 proc sgplot data=predicted;
 scatter x=comp y= respart_comp;
 xaxis label="Comp"; 
 yaxis label="partial (deviance) residuals";
 loess x=comp y=respart_comp / clm;
 reg x=comp y=respart_comp / nomarkers;
 run;

proc sgplot data=predicted;
 scatter x=Estimated_Years_To_Complete y= respart_est;
 xaxis label="Estimated Years to Complete"; 
 yaxis label="partial (deviance) residuals";
 loess x=Estimated_Years_To_Complete y= respart_est / clm;
 reg x=Estimated_Years_To_Complete y= respart_est / nomarkers;
 run;  

 *these both look okay;

*Fit the additive model;  
proc gam data=training plots=components(clm additive commonaxes);
	class Region_of_Country Sector;
	model Win_Bid(event='Yes') = param(Estimated_Years_To_Complete Sector Region_of_Country comp) spline(markup, df=4)/ dist=binomial link=logit;
run; 

proc gam data=training plots=components(clm additive commonaxes);
	class Region_of_Country Sector;
	model Win_Bid(event='Yes') = param(markup Sector Region_of_Country comp) spline(Estimated_Years_To_Complete, df=4)/ dist=binomial link=logit;
run;  

proc gam data=training plots=components(clm additive commonaxes);
	class Region_of_Country Sector;
	model Win_Bid(event='Yes') = param(markup Estimated_Years_To_Complete Sector Region_of_Country) spline(comp, df=4)/ dist=binomial link=logit;
run;  

*Calibration curve;
proc sgplot data=predicted;
yaxis label="Win_Bid";
loess x=phat y=resp / smooth=0.75 interpolation=cubic clm;
lineparm x=0 y=0 slope=1 / lineattrs=(color=grey pattern=dash);
run;

*check influential points - deal with at beginning?;
proc sort data=logistic.construction;
by resp;
run;

proc logistic data=logistic.construction plots(MAXPOINTS=NONE only label)=influence;
	class Region_of_Country Sector;
	model resp(event='1') = &backvar;
run;

proc logistic data=logistic.construction plots(MAXPOINTS=NONE only label)=dpc;
	class Region_of_Country Sector;
	model resp(event='1') = &backvar;
run;

proc logistic data=logistic.construction plots(MAXPOINTS=NONE only label)=dfbetas;
	class Region_of_Country Sector;
	model resp(event='1') = &backvar;
run;


/* fitting */
/* roc curve brier score c stat */
proc logistic data=training plots(only)=ROC(id = prob);
	class Region_of_Country Sector;
	model resp(event='1') = &backvar / rocci; 
	score data=testing out=Valpred outroc=vroc fitstat;
	roc; roccontrast;
run;


/* distribution of predicted probabilities */
proc logistic data=training noprint;
	class Region_of_Country Sector;
	model resp(event='1') = &backvar; 
	/* output predicted probabilities */
	output out=predprobs p=phat;
run;

proc logistic data=testing;
	class Region_of_Country Sector;
	model resp(event='1') = &backvar;
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
proc logistic data=training;
	class resp Region_of_Country Sector;
	model resp(event='1') = &final / ctable pprob = 0 to 0.98 by 0.02;
	/* output table */
	score data=testing out=Valpred outroc=vroc fitstat;
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
