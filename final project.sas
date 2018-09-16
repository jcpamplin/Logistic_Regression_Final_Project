/*proc contents data=logreg.construction;
run;*/

*Create training and testing data sets;
data training testing;
	set logreg.construction ;
	if ranuni(4) >= .3 then output training;
    else output testing;
run;

*Model with all variables included;
proc logistic data=training;
class region_of_country(ref='West') sector(ref='1')/param=ref;
model win_bid(event='Yes')= /*bid_price__millions_*/ competitor_a--competitor_j /*cost_after_engineering_estimate_*/ estimated_cost__millions_
								estimated_years_to_complete number_of_competitor_bids region_of_country winning_bid_price__millions_ sector;
run;

*Model with all variables and interaction terms included;
proc logistic data=training;
class region_of_country(ref='West') sector(ref='1')/param=ref;
model win_bid(event='Yes')= bid_price__millions_|competitor_a|competitor_b|competitor_c|competitor_d|competitor_e|competitor_f|competitor_g|competitor_h|
							competitor_i|competitor_j|cost_after_engineering_estimate_ |estimated_cost__millions_|
								estimated_years_to_complete |number_of_competitor_bids |region_of_country| winning_bid_price__millions_| sector @2
								/selection=stepwise slentry=.2 slstay=.2;
run;

*Checking for multicollinearity between our bid and the winning bid;
/*proc corr data=logreg.construction;
var bid_price__millions_ winning_bid_price__millions_;
run;*/

*All p-values are below .20, scores the model against testing data;
proc logistic data=training;
	class region_of_country(ref='Mid-west') sector(ref='9')/param=ref;
	model win_bid(event='Yes')= competitor_b competitor_g competitor_h competitor_j 
								estimated_cost__millions_ number_of_competitor_bids region_of_country winning_bid_price__millions_
								sector;
	score data=testing fitstat;
run;
*Error rate of .10;


*All p-values are below .20 but with interaction terms included;
/*proc logistic data=logreg.construction;
class region_of_country(ref='Mid-west') sector(ref='9')/param=ref;
model win_bid(event='Yes')= competitor_a|competitor_b |competitor_d|competitor_e|competitor_f |competitor_h |competitor_j |estimated_cost__millions_|
								estimated_years_to_complete| number_of_competitor_bids| region_of_country |winning_bid_price__millions_| sector @2/
									selection=stepwise slentry=.2 slstay=.2;
run;*/

*All p-values are below .10. .05, and .01;
proc logistic data=training outmodel=small_model;
	class region_of_country(ref='Mid-west') sector(ref='9')/param=ref;
	model win_bid(event='Yes')= competitor_b competitor_h competitor_j estimated_cost__millions_
								number_of_competitor_bids region_of_country winning_bid_price__millions_ sector;
run;

*Scores model against testing data;
proc logistic inmodel=small_model;
	score data=testing fitstat;
run;
*Error rate of .08;

*Very low p-values with interaction terms;
/*proc logistic data=logreg.construction;
class region_of_country(ref='Mid-west') sector(ref='9')/param=ref;
model win_bid(event='Yes')= competitor_b |competitor_f |competitor_h |competitor_j |estimated_cost__millions_|
								 number_of_competitor_bids| region_of_country |winning_bid_price__millions_ |sector @2 /selection=stepwise slentry=.05 slstay=.05;
run;*/

