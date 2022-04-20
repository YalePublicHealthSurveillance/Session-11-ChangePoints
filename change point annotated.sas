*IMPORT DATA, creating new dataset called DS1;
PROC IMPORT OUT= WORK.trend1 
            DATAFILE= "C:\Users\dmw63\Desktop\My documents h\TEACHING\CASE STUDY VACCINE IMPACT 2018\Day 3 Intro to evaluation methods\Brazil_acp.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
	 guessingrows=400; /* <--------prevents truncation of character variables*/
RUN;

*VISUALIZE DATA-- time series;
*Set graphical parameters;
		goptions reset=all;
				proc template;
				   define style nicestyle;
				   parent=styles.default;
				      class graphwalls / 
				            frameborder=off;
				      class graphbackground / 
				            color=white;
				   end;
				run;
				proc sort data=trend1;
					by age_group date;
					run;
					
		ods graphics on/ border=off  width=600px height=400px ;
					ODS listing  IMAGE_DPI=200 style=nicestyle;
	proc sgplot data=trend1;
		by age_group;
		series x=date y=j12_18;
		refline "01Jan2010"d/axis=x lineattrs=(pattern=dash); /*add reference line at time of vaccine intro*/
		yaxis min=0; /*extend y axis to 0*/
	run;

	/*We will use MACROS as organizational tools here to keep related pieces of code together*/
/*this first macr runs linear regression and log-linear regression on dep-var*/

	data trend2; set trend1;
		if ach_noj >0 then log_ach_noj=log(ach_noj);
			else log_ach_noj=log(0.5);

		J12_18_pre=J12_18;
			if date>="01Jan2010"d then J12_18_pre=.;

			index=intck("MONTH", "01Jan2004"d, date) +1;
	run;

/*In the first data set, we will create 3 linear splines, with knots at month 60, 80 or 100...they take a value of 0 until the knot and then 1,2,3... afterwards*/
data ds1; set trend2;
where age_group="<1" ;
	if "01Jan2010"d<=date<"01Jan2011"d then period1=0;
	else period1=1;

	if date>="01Jan2011"d then period2=0;
	else period2=1;

	/*linear spline*/
	if index>=60 then spl60=index-60;
		else spl60=0;

	if index>=48 then spl48=index-48;
		else spl48=0;

	if index>=72 then spl72=index-72;
		else spl72=0;
		if index>=84 then spl84=index-84;
		else spl84=0;

	month=month(date);

	
	year=year(date);
	if year>=2006 then year2006=1; else year2006=0;
	if year>=2008 then year2008=1; else year2008=0;

run;

/*Here, we will create a MACRO which lets us efficiently test different sets of covariates.
Within the macro, where it sees &covars, the program will subsitute in whatever you put in paraentheses in
	"%model_comparisons();"
*/
%MACRO MODEL_COMPARISONS(covars);
proc genmod data=ds1;
	class month;
		by age_group;
	model J12_18= month year2006 year2008 &covars/dist=negbin link=log offset=log_ach_noj; /*negative binomial model for cases*/
	output out=pred1 predicted=pred_cases; /*output predicted values*/
run;
/*plot the observed and predicted values*/
proc sgplot data=pred1;
	by age_group;
	scatter x=date y=J12_18;
	series x=date y=pred_cases;
run;
%MEND MODEL_COMPARISONS;
%MODEL_COMPARISONS(index );
%MODEL_COMPARISONS(index period1 period2 index*period1 index*period2);
%MODEL_COMPARISONS(index spl48);
%MODEL_COMPARISONS(index spl60);
%MODEL_COMPARISONS(index spl72);
%MODEL_COMPARISONS(index spl84);

/*Instead of just testing  random knot locations, we want to test ALL knot locations between  month 6 and 114
to do this efficiently, we are going to rplicate the original data set 108 times, and in each of those
	replicates, we will test a different knot location (given by "cp")*/

data ds2; set ds1;
/*this do-loop replicates the dataset 108 times, and each of the replicates will be numbered from 6 to 114,
	corrsponding to different possible knot locations*/
	DO cp=0, 61 to 114; /*changepoint can be between 2009 and 2014*/
	if index>=cp then spl= index-cp; /*in a particular replicate, if we are after the knot location (cp), then takes on a value of 1,2,3,4..., otehrwise it is 0*/
	else spl=0;
	if cp=0 then spl=0;

	output ;
	end;
run;
/*sort by the replicates*/
proc sort data=ds2;
	by age_group cp index;
	run;
/*run he model separately fr each possible knot location using "by cp";*/
ods listing close; /*this just turns off the output window so that we don't overload it results*/
proc genmod data=ds2;
	by age_group cp;
	class month;
	ods output parameterestimates=parm_spl modelfit=modelfit_CP; /*outputs the parameter estimates and AIC scores to new files*/
	model j12_18= month year2006 year2008 index spl /dist=negbin link=log offset=log_ach_noj;
	output out=pred_spl predicted=pred_cases;
run;
ods listing; /*this turns the output window back on*/

/*Plot predictions from all 108 model*/
proc sgplot data=pred_spl;
	by age_group;
	series x=index y=pred_cases/group=cp;
run;

/*keep the AIC score from each of the models*/
%MACRO AIC_WEIGHTS;
data aic1; set modelfit_CP;
	where criterion="AIC (smaller is better)";
	one=1; /*this just gives a convenient way to mereg the results back in after collapsing*/
run;
proc sort data=aic1;
	by age_group value;
run;
/*plots the raw AIC score by change point knotlocation...smaller=better fit*/
proc sgplot data=aic1;
	scatter x=cp y=value;
run;

/*now we need to weight the models based on AIC scores. the best model has the lowest AIC score. Models nearte best receive more weight*/
		/** ofirst obtain the AIC score for best model and output to new dataset**/
					proc means noprint data=aic1 nway noprint;
					 var value;
					 class age_group one;
					output out=minaic min=minvari;
					run;
					
			/*Merge the minimum AIC score in with all of the AIC score;
						Delta AIC gives AIC difference between model with minimum AIC and other models*/
				data aic2;
					merge aic1 minaic;
						by age_group one;
						delta_aic = value- minvari; /*difference between each model's AIC and best AIC*/
					/*Relative likelihood of model given the data*/
					 wnum=exp((-0.5 * delta_aic)); /*this gives the raw weight of the model*/
					run;
					/*we want to scale the weights so they add to one, so we sum the weights from all models,
						then divide by this value to get weights that add to 1*/
					proc means data=aic2 nway noprint;
						var wNUM;
						class age_group one;
					output out=wdenominator sum=wden;
					run;
					/*GIVES THE AIC WEIGHTS*/
				data aicw;
					merge  aic2 wdenominator;
					by age_group one;
					  w_Akaike= wnum/wden; /*this is the Scale AIC weight, which add to 1 across all models*/
					  keep cp age_group w_akaike;
					run;
					proc sort data=aicw;
						by age_group w_akaike;
						run;
				/*plot the model weights. You'll notice this is essentially an inverse of the graph of the AIC scores viewed above*/
				proc sgplot data=aicw;
					by age_group;
					scatter x=cp y=W_akaike;
					refline 73/axis=x;
				run;
		proc sort data=aicw;
			by age_group cp;
			run;
		/*next, we want to average the predicted values across all models. merge in the model weights with the predicted values*/
%MEND AIC_WEIGHTS;;
%AIC_WEIGHTS;

	/*What is probability change point is after PCV intro?*/
		proc means data=aicw nway noprint;
			class age_group;
			where cp>72;
			var w_akaike;
			output out=prob_post sum=prob_post;
		run;
		data pred_spl2;
			merge pred_spl aicw parm_spl(where=(parameter="spl"));
				by age_group cp;
			run;
		data pred_spl3;
			merge pred_spl2  prob_post;
			by age_group;
			
			pred_no_vax=pred_cases/exp(prob_post*estimate*spl);  /*<-------- counterfactual*/
		run;
		/*now take the weighted average of the predictions for each time point*/
		proc means data=pred_spl3 nway noprint;
			class age_group index;
			var j12_18 pred_cases pred_no_vax ;
			weight w_akaike; /*gives more weight to predictions from better-fitting models*/
			output out=pred_spl_ave mean=;
		run;
		/*view the averaged predictions*/
	proc sgplot data=pred_spl_ave;
		by age_group;
		scatter x=index y=j12_18;
		series x=index y=pred_cases;
		series x=index y=pred_no_vax/lineattrs=(color=red);
		refline 73;
	run;
	data pred_spl_ave2; set pred_spl_ave;
		rr=pred_cases/pred_no_vax;
	run;
	proc sgplot data=pred_spl_ave2;
		by age_group;
		scatter x=index y=rr;
		refline 73/axis=x;
	run;

	/*now let's look at the averaged  slope and 95% CI of the spl terms*/
		data parm_spl2;
			merge parm_spl(where=(parameter="spl")) aicw; /*merge in the model weights with the slope estimates for the "spl" term*/
			by age_group cp;
		run;
		data parm_spl3;
			merge parm_spl2 prob_post ;
			by age_group;
		run;
		data parm_spl4;	
			merge parm_spl3 ds2(where=(date="01Dec2013"d) keep=spl cp date age_group);
				by age_group cp;
			one=1;
		
			estimate_adj=estimate*prob_post*spl;
			sd_adj=stderr*prob_post*spl;
run;	
		/*takes the weighted average of the slope estimates*/
		proc means data=parm_spl4 nway noprint;
			var estimate_adj;
			class age_group one;
			weight W_akaike;
			output out=ave_parm mean=beta_spl_adj;
		run;

	/*confidence intervals around the average*/
					data ave_slope3;
						merge parm_spl4 ave_parm;
							by age_group one;
							ave_var_piece= (sd_adj**2+  (beta_spl_adj-estimate_adj)**2); /*the variance for each model depends on the model variance (stderr^2) as well as the between-model variation, which is given by difference between the slope estimate from a single model and the average slope estimate*/
												
					run;
					proc means data=ave_slope3 nway noprint;
						class age_group one;
						var ave_var_piece estimate_adj;
						weight w_akaike;
						output out=variance mean=variance estimate_adj; /*note there was an error here in class...should be mean, not sum*/
					run;
					data ave_slope_CIs;
						merge ave_parm variance;
						by  age_group one;
						/*the confidence intervals are the mean +/- 1.96*stderr*/
						ave_slope_LCL=beta_spl_adj-1.96*sqrt(variance);
						ave_slope_UCL=beta_spl_adj+1.96*sqrt(variance);

						/*remember, these estimates are from a POISSON model, so interpret as:
							a 1 unit increase in time is associated with an exp(beta_spl)-fold decrease in cases, if you want
								to estimate change per year (12 months) it would be exp(beta_spl*12)*/
						IRR_Last_time=exp(beta_spl_adj);
						IRR_Last_time_LCL=exp(ave_slope_LCL);
						IRR_Last_time_UCL=exp(ave_slope_UCL);
						run;
					proc print data=ave_slope_CIs;
						var age_group irr:;
						run;


		/*Main pieces to report*/
				/*1) the plot showing the weight/probability of a change at each time point*/
				proc sgplot data=aicw;
					scatter x=cp y=W_akaike;
				run;
				/*2) plot showing the averaged predictions*/
					proc sgplot data=pred_spl_ave;
						scatter x=t y=cases1;
						series x=t y=ave_pred_cases;
					run;
				/*3) the estimates for the IRR per year*/
					proc print data=ave_slope_CIs;
					var irr:;
						run;


