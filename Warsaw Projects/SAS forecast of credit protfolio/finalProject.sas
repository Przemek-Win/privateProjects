libname wyj 'C:\Users\Tom\Documents\sas statystyka\Przanowski\pd2\projekt\dane';

data start.vin;
set start.Transactions;
seniority=intck('month',input(fin_period,yymmn6.),input(period,yymmn6.));
vin3=(due_installments>=3);
output;
if status in ('B','C') and period<='200812' then do;
	n_steps=intck('month',input(period,yymmn6.),input('200812',yymmn6.));
	do i=1 to n_steps;
		period=put(intnx('month',input(period,yymmn6.),1,'end'),yymmn6.);
		seniority=intck('month',input(fin_period,yymmn6.),input(period,yymmn6.));
		output;
	end;
end;
where product='ins';
keep cid fin_period vin3 seniority;
run;


data start.prod;
set start.production;
keep cid act_age;
if act_age > 40 then do;
	act_age = 1;
end;
else
	do;
	act_age = 2;
	end;
where product = 'ins';
run;


proc sql;
create table vin_age as
select t1.*, t2.act_age as 'group'n from start.vin t1, start.prod t2 
where t1.cid = t2.cid;
run;


proc means data=start.vin_age noprint nway;
class fin_period seniority group;
var vin3;
output out=start.vintagr(drop=_freq_ _type_) n()=production mean()=vintage3;
format vintage3 nlpct12.2;
run;

proc sort data=start.vintagr out=start.vintagr_s;
by fin_period group;
run;
proc means data=start.vin_age noprint nway;
class group fin_period ;
var vin3;
output out=start.production(drop=_freq_ _type_) n()=production;
where seniority=0;
run;



 /* Define titles and footnote */                                                                                                       
title1 'Vintage3';                                                                                                        
footnote1 ' ';                                                                                                                          
                                                                                                                                        
 /* Define symbol characteristics */                                                                                                    
symbol1 color=vibg interpol=join value=dot;                                                                                             
                                                                                                                                                                                                                                                        
 
proc sort data=start.vintage out=vintagr_t;
by  group;
run;
 
 /* Generate plot of two variables */                                                                                                   
proc gplot data=start.vintagr_t;                                                                                                                 
   plot months_after_12*fin_period months_after_6*fin_period /                                                                                                                                                                         
                    cvref=black;
by group; 
run;                                                                                                                                    
quit;


libname wyj 'C:\Users\Tom\Documents\sas statystyka\Przanowski\pd2\projekt\dane';

data vin;
set wyj.Transactions;
seniority=intck('month',input(fin_period,yymmn6.),input(period,yymmn6.));
vin3=(due_installments>=3);
output;
if status in ('B','C') and period<='200812' then do;
n_steps=intck('month',input(period,yymmn6.),input('200812',yymmn6.));
do i=1 to n_steps;
period=put(intnx('month',input(period,yymmn6.),1,'end'),yymmn6.);
seniority=intck('month',input(fin_period,yymmn6.),input(period,yymmn6.));
output;
end;
end;
where product='ins';
keep fin_period vin3 seniority period;
run;
data vin2;
set wyj.Transactions;
seniority=intck('month',input(fin_period,yymmn6.),input(period,yymmn6.));
vin3=(due_installments>=3);
output;
where product='ins';
keep fin_period vin3 seniority period;
run;
proc means data=vin print nway;
class fin_period seniority;
var vin3;

output out=vintagr(drop=_freq_ _type_) n()=production mean()=vintage3;

run;
data vintagr;
set vintagr;
format vintage3 nlpct12.2;
run;
proc means data=vin print nway;
class fin_period;
var vin3;
output out=production(drop=_freq_ _type_) n()=production;
where seniority=0;
run;
proc transpose data=vintagr out=vintage prefix=months_after_;
by fin_period;
var vintage3;
id seniority;
run;
