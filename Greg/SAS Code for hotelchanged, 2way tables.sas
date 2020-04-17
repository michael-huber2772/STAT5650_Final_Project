FILENAME REFFILE '/folders/myfolders/Stat5650/Final Project/hotel.csv';

PROC IMPORT DATAFILE=REFFILE
	DBMS=CSV
	OUT=WORK.hotel;
	GETNAMES=YES;
RUN;

PROC CONTENTS DATA=WORK.hotel; RUN;

/*code for exporting hotelchanged.csv for analyzing hotel as response variable (as in nest data)*/
data hotelchanged;
	set hotel;
	if children=. then delete;
	if is_canceled=0 then hotel="Not Canceled";
	drop is_canceled country agent company reservation_status_date reservation_status;
run;

proc export data=hotelchanged outfile="/folders/myfolders/Stat5650/output/hotelchanged.csv"
	dbms=csv REPLACE;	
run;

/*TWO-WAY TABLES FOR IMPORTANT VARIABLES*/

data hotel2;  /*doesn't look very good*/
	set hotel;
	length lead $12;
	if lead_time<15 then lead = "a: 15under";
	else if 15<=lead_time<30 then lead = "b: 15-30";
	else if 30<=lead_time<45 then lead = "c: 30-45";
	else if 45<=lead_time<60 then lead = "d: 45-60";
	else if 60<=lead_time<75 then lead = "e: 60-75";
	else if 75<=lead_time<90 then lead = "f: 75-90";
	else if 90<=lead_time<105 then lead = "g: 90-105";
	else if 105<=lead_time<120 then lead = "h: 105-120";
	else if 120<=lead_time<135 then lead = "i: 120-135";
	else if 135<=lead_time<150 then lead = "j: 135-150";
	else if 150<=lead_time<165 then lead = "k: 150-165";
	else if 165<=lead_time<180 then lead = "l: 165-180";
	else if 180<=lead_time<195 then lead = "m: 180-195";
	else if 195<=lead_time<210 then lead = "n: 195-210";
	else if 210<=lead_time<225 then lead = "o: 210-225";
	else if 225<=lead_time<240 then lead = "p: 225-240";
	else if 240<=lead_time<255 then lead = "q: 240-255";
	else if 255<=lead_time<270 then lead = "r: 255-270";
	else if 270<=lead_time<285 then lead = "s: 270-285";
	else if 285<=lead_time<300 then lead = "t: 285-300";
	else if 300<=lead_time<315 then lead = "u: 300-315";
	else if 315<=lead_time<330 then lead = "v: 315-330";
	else if 330<=lead_time<345 then lead = "w: 330-345";
	else lead = "x: 345over";
	
	drop country agent company reservation_status_date reservation_status;
run;

data hotel1; /*looks better, and cutoff of 12 is based on classification tree's split of lead time at 11.5*/
	set hotel;
	length lead $15;
	if lead_time<12 then lead = "a: under 12";
	else lead = "b: 12 and over";
	
	drop country agent company reservation_status_date reservation_status;
run;

data ResortHotel;
	set hotel1;
	where hotel="Resort Hotel";
	drop hotel;
run;

data CityHotel;
	set hotel1;
	where hotel="City Hotel";
	drop hotel;
run;

ods pdf file="/folders/myfolders/Stat5650/output/impvar2waytables.pdf";

title1 "cancelation by important predictor variables";
title2 "deposit_type";
title3 "combined";
proc freq data=hotel1;
	tables deposit_type*is_canceled / nocol;
run;

title3 "resort";
proc freq data=resorthotel;
	tables deposit_type*is_canceled / nocol;
run;

title3 "City";
proc freq data=cityhotel;
	tables deposit_type*is_canceled / nocol;
run;

title2 "lead_time";
title3 "combined";

proc freq data=hotel1;
	tables lead*is_canceled / nocol;
run;

title3 "resort";
proc freq data=resorthotel;
	tables lead*is_canceled / nocol;
run;

title3 "city";
proc freq data=cityhotel;
	tables lead*is_canceled / nocol;
run;

title2 "market_segment";
title3 "combined";
proc freq data=hotel1;
	tables market_segment*is_canceled / nocol;
run;

title3 "resort";
proc freq data=resorthotel;
	tables market_segment*is_canceled / nocol;
run;

title3 "city";
proc freq data=cityhotel;
	tables market_segment*is_canceled / nocol;
run;

title2 "total_of_special_requests";
title3 "combined";

proc freq data=hotel1;
	tables total_of_special_requests*is_canceled / nocol;
run;

title3 "resort";
proc freq data=resorthotel;
	tables total_of_special_requests*is_canceled / nocol;
run;

title3 "city";
proc freq data=cityhotel;
	tables total_of_special_requests*is_canceled / nocol;
run;

ods pdf close;


/*other stuff*/ 
proc univariate data=hotel1;
	var lead_time;
run;

proc univariate data=resorthotel;
	var lead_time;
run;