/*Manitoba data 2007 to 2020
Data Analysis: Associations between Environmental Factors and Freshwater Recreational Microbial Water Quality in Manitoba, Wi
Binyam Negussie Desta
Last Updated September 29, 2023
 */
/* Creating a title and footnote */
TITLE "Association of Environmental factors with Manitoba recreational fresh water, from 2007 to 2020";
FOOTNOTE "Data Analysis for Manitoba";

/* Loading the data into SAS - using xlsx format*/
%web_drop_table(WORK.IMPORT);
FILENAME REFFILE '/home/u63568367/Manitoba/ManitobaMerged20220729.xlsx';

PROC IMPORT DATAFILE=REFFILE
	DBMS=XLSX
	OUT=WORK.IMPORT;
	GETNAMES=YES;
RUN;

PROC CONTENTS DATA=WORK.IMPORT; RUN;
%web_open_table(WORK.IMPORT);

/*Recoding variables and declaring missing values to SAS*/
DATA ManData1D;
 SET WORK.IMPORT;
 IF Beach='GIMLI BEACH' THEN Beach_C=1;
 IF Beach='GRAND BEACH EAST' THEN Beach_C=2;
 IF Beach='GRAND BEACH WEST' THEN Beach_C=3;
 IF Beach='`' THEN Beach_C=.;
 IF Beach='NA' THEN Beach_C=.;
 IF PrevGeomean='NA' THEN PrevGeomean_C=.;
ELSE PrevGeomean_C=PrevGeomean;
 IF MaxTemp='NA' THEN MaxTemp_C=.;
ELSE MaxTemp_C=MaxTemp;
 IF MinTemp='NA' THEN MinTemp_C=.;
ELSE MinTemp_C=MinTemp;
 IF MeanTemp='NA' THEN MeanTemp_C=.;
ELSE MeanTemp_C=MeanTemp;
 IF MeanTemp24='NA' THEN MeanTemp24_C=.;
ELSE MeanTemp24_C=MeanTemp24;
 IF TotalPrecip='NA' THEN TotalPrecip_C=.;
ELSE TotalPrecip_C=TotalPrecip;
 IF DirofMaxGust='NA' THEN DirofMaxGust_C=.;
ELSE DirofMaxGust_C=DirofMaxGust;
 IF SpdofMaxGust='NA' THEN SpdofMaxGust_C=.;
ELSE SpdofMaxGust_C=SpdofMaxGust;
 IF SpdMaxGust24='NA' THEN SpdMaxGust24_C=.;
ELSE SpdMaxGust24_C=SpdMaxGust24;
 IF WtrLvl24='NA' THEN WtrLvl24_C=.;
ELSE WtrLvl24_C=WtrLvl24;
 IF WaterLVL='NA' THEN WaterLVL_C=.;
ELSE WaterLVL_C=WaterLVL;
 IF AverageUV='NA' THEN AverageUV_C=.;
ELSE AverageUV_C=AverageUV;
 IF MaxUV='NA' THEN MaxUV_C=.;
ELSE MaxUV_C=MaxUV;
 IF AveUV24='NA' THEN AveUV24_C=.;
ELSE AveUV24_C=AveUV24;
 IF MaxUV24='NA' THEN MaxUV24_C=.;
ELSE MaxUV24_C=MaxUV24;
 IF Rain24='NA' THEN Rain24_C=.;
ELSE Rain24_C=Rain24;
 IF Rain48='NA' THEN Rain48_C=.;
ELSE Rain48_C=Rain48;
 IF VCAR='NA' THEN VCAR_C=.;
ELSE VCAR_C=VCAR;
 IF VCAR24='NA' THEN VCAR24_C=.;
ELSE VCAR24_C=VCAR24;
 IF VWHS='NA' THEN VWHS_C=.;
ELSE VWHS_C=VWHS;
 IF VWHS24='NA' THEN VWHS24_C=.;
ELSE VWHS24_C=VWHS24;
 IF VCMX='NA' THEN VCMX_C=.;
ELSE VCMX_C=VCMX;
 IF VTPK='NA' THEN VTPK_C=.;
ELSE VTPK_C=VTPK;
 IF VTPS='NA' THEN VTPS_C=.;
ELSE VTPS_C=VTPS;
 IF WDIR='NA' THEN WDIR_C=.;
ELSE WDIR_C=WDIR;
 IF WSPD='NA' THEN WSPD_C=.;
ELSE WSPD_C=WSPD;
 IF GSPD='NA' THEN GSPD_C=.;
ELSE GSPD_C=GSPD;
 IF GSPD24='NA' THEN GSPD24_C=.;
ELSE GSPD24_C=GSPD24;
 IF ATMS='NA' THEN ATMS_C=.;
ELSE ATMS_C=ATMS;
 IF DRYT='NA' THEN DRYT_C=.;
ELSE DRYT_C=DRYT;
 IF SSTP='NA' THEN SSTP_C=.;
ELSE SSTP_C=SSTP;
 IF DaysSinceRain='NA' THEN DaysSinceRain_C=.;
ELSE DaysSinceRain_C=DaysSinceRain;
RUN; QUIT;

/*Slicing the data set and excluding those records before 2007*/
DATA ManData1P ManData1P_missing ;
 SET Work.ManData1D;
 IF (2003 <= Year <= 2006) then OUTPUT ManData1P_missing;
 ELSE OUTPUT ManData1P;
RUN;QUIT;

PROC SURVEYFREQ DATA=ManData1P;
TABLES Beach_C Geomean5 MinTemp_C MeanTemp_C TotalPrecip_C
 DirofMaxGust_C SpdofMaxGust_C WaterLVL_C WtrLvl24_C 
AverageUV_C MaxUV_C AveUV24_C MaxUV24_C Rain24_C Rain48_C
 VCAR_C VWHS_C VCMX_C VTPK_C VTPS_C WDIR_C WSPD_C
 GSPD_C ATMS_C DRYT_C SSTP_C/ row col;
RUN;

/* Descriptive of GeoMean by Beach*/
PROC SORT DATA=ManData1P;
BY Beach_C;
RUN;
PROC MEANS DATA=ManData1P;
VAR Geomean5_C;
BY Beach_C;
RUN;
PROC UNIVARIATE DATA=ManData1P NORMAL;
VAR Geomean5_C;
HIST Geomean5_C / Normal (mu=est sigma=est);
BY Beach_C;
RUN;

/*Logtransformation of selected variables: to reduce the skeweness*/
DATA ManData1Q;
 SET ManData1P;
 LogGeomean5 = Log(Geomean5);
 LogPrevGeomean = Log(PrevGeomean_C);
RUN; QUIT;

/* Descriptive of GeoMean by Beach - after log transformation*/
PROC SORT DATA=ManData1Q;
BY Beach_C;
RUN;
PROC MEANS DATA=ManData1Q;
VAR Geomean5_C;
BY Beach_C;
RUN;
PROC UNIVARIATE DATA=ManData1Q NORMAL;
VAR LogGeomean5;
HIST LogGeomean5 / Normal (mu=est sigma=est);
BY Beach_C;
RUN;

/*Merging Grand Beach east and west*/
DATA ManData1R1;
 SET ManData1Q;
 IF Beach_C=3 THEN Beach_R=2;
ELSE Beach_R=Beach_C;
RUN; QUIT;

/*Creating a categorical variable using 200CFU/100ml threshold*/
DATA ManData1R2;
 SET ManData1R1;
 IF (0<=Geomean5<=199.9999) THEN GeomeanCat1=0;
ELSE GeomeanCat1=Geomean5;
RUN; QUIT;
DATA ManData1R;
 SET ManData1R2;
 IF (200.0000<=GeomeanCat1<=7400) THEN GeomeanCat=1;
ELSE GeomeanCat=GeomeanCat1;
RUN; QUIT;

/*Creating two datasets: Gimli and Grand beach (east and west)*/
DATA GimliDS GrandDS;
 SET Work.ManData1R;
 IF (Beach_C=1) then OUTPUT GimliDS;
 IF (Beach_C=2) then OUTPUT GrandDS;
 IF (Beach_C=3) then OUTPUT GrandDS;
RUN;QUIT;
PROC SORT DATA=GrandDS OUT=GrandDS1;
 BY Beach_C ;
RUN ;
PROC TTEST DATA = GrandDS1 ALPHA=0.05;
VAR LogGeomean5;
CLASS Beach_C;
RUN; QUIT;

/* Descriptive of GeoMean by Beach - after log transformation and merging grand east & west*/
PROC SORT DATA=ManData1R;
BY Beach_R;
RUN;
PROC MEANS DATA=ManData1R;
VAR Geomean5_C;
BY Beach_R;
RUN;
PROC UNIVARIATE DATA=ManData1R NORMAL;
VAR LogGeomean5;
HIST LogGeomean5 / Normal (mu=est sigma=est);
BY Beach_R;
RUN;

/*Creating two datasets: Gimli and Grand beach*/
DATA Gimli Grand;
 SET Work.ManData1R;
 IF (Beach_R=1) then OUTPUT Gimli;
 IF (Beach_R=2) then OUTPUT Grand;
RUN;QUIT;
/* Mean of the geomean by year - to graph yearly*/
/*Gimli*/
PROC SORT DATA=Gimli;
BY Year;
RUN;
PROC MEANS DATA=Gimli;
VAR Geomean5_C;
BY Year;
RUN;
/*Grand*/
PROC SORT DATA=Grand;
BY Year;
RUN;
PROC MEANS DATA=Grand;
VAR Geomean5_C;
BY Year;
RUN;


/*GRAPHS*/
data my_graph;
length Beach $32;
input Year Mean Beach $;
datalines;
 1 192.1 Gimli
 1 94.9 Grand
 2 61.8 Gimli
 2 23.6 Grand
 3 275.2 Gimli
 3 39.2 Grand
 4 87.2 Gimli
 4 81.2 Grand
 5 35.1 Gimli
 5 29.3 Grand
 6 50.4 Gimli
 6 29.7 Grand
 7 96.5 Gimli
 7 67.5 Grand
 8 47.3 Gimli
 8 25.7 Grand
 9 295.3 Gimli
 9 111.1 Grand
 10 369.6 Gimli
 10 70.5 Grand
 11 278.6 Gimli
 11 66.9 Grand
 12 122.7 Gimli
 12 141.6 Grand
 13 301.9 Gimli
 13 403.4 Grand
 14 143.9 Gimli
 14 32.6 Grand
 15 134.1 Gimli
 15 56.9 Grand
 ;
run;
/* Annotate some labels on the graph */
data anno_seasons;
length function x1space y1space anchor $50;
layer="front";
function="text"; textcolor="gray33"; textsize=9;
textweight='bold'; anchor='center';
width=50; widthunit='percent';
x1space='datavalue';
y1space='datavalue';
run;
/* Custom user-defined format, to make numbers print as text month name */
proc format;
value yr_fmt
1='2007'
2='2008'
3='2009'
4='2010'
5='2011'
6='2012'
7='2013'
8='2014'
9='2015'
10='2016'
11='2017'
12='2018'
13='2019'
14='2020'
15='2021';
run;
ODS LISTING STYLE = JOURNAL;
ODS PDF FILE = 'c:\MyPDFFiles\Graph.pdf';
ODS HTML gpath='c:\MyGraphs';
ods graphics /
imagemap tipmax=2500
imagefmt=png imagename="MyGraph"
width=800px height=500px border;
proc sgplot data=my_graph noborder noautolegend sganno=anno_seasons;
format Year yr_fmt.;
format Mean comma5.1;
label Beach='Beaches' Year='Year' Mean='Yearly Average of Daily Geometric Mean of E. coli (CFU/100ml)';
styleattrs datacontrastcolors=(black black);
series x=Year y=Mean / group=Beach lineattrs=(thickness=2)
curvelabel curvelabelpos=max curvelabelloc=outside 
markers markerattrs=(symbol=circle size=4pt)
tip=(Beach Year Mean);
yaxis display=All values=(0 to 450 by 50)
valuesdisplay= (" " "50" "100" "150" "200" "250" "300" "350" "400" "450")
valueattrs=(size=9pt weight=bold color=gray33)
offsetmin=0 offsetmax=0;
xaxis display=All values=(1 to 15 by 1)
valueattrs=(size=9pt weight=bold color=gray33)
offsetmin=.02 offsetmax=.005;
run; quit;

/*EXPLORATORY ANALYSIS*/
 
 /*Individual Quantitative variables */
 
 /*Gimli*/
PROC MEANS DATA=Work.Gimli MIN Q1 MEDIAN Q3 MAX RANGE;
VAR Geomean0_C Geomean5_C MaxTemp_C MinTemp_C MeanTemp_C TotalPrecip_C SpdofMaxGust_C
WaterLVL_C AverageUV_C MaxUV_C AveUV24_C MaxUV24_C Rain24_C Rain48_C VCAR_C VWHS_C
 VCMX_C VTPK_C VTPS_C WDIR_C WSPD_C GSPD_C ATMS_C DRYT_C SSTP_C DaysSinceRain_C;
RUN;
PROC UNIVARIATE DATA=Work.Gimli NORMAL;
VAR Geomean0_C Geomean5_C MaxTemp_C MinTemp_C MeanTemp_C TotalPrecip_C SpdofMaxGust_C
WaterLVL_C AverageUV_C MaxUV_C AveUV24_C MaxUV24_C Rain24_C Rain48_C VCAR_C VWHS_C
 VCMX_C VTPK_C VTPS_C WDIR_C WSPD_C GSPD_C ATMS_C DRYT_C SSTP_C DaysSinceRain_C;
HIST Geomean0_C Geomean5_C MaxTemp_C MinTemp_C MeanTemp_C TotalPrecip_C SpdofMaxGust_C
WaterLVL_C AverageUV_C MaxUV_C AveUV24_C MaxUV24_C Rain24_C Rain48_C VCAR_C VWHS_C
 VCMX_C VTPK_C VTPS_C WDIR_C WSPD_C GSPD_C ATMS_C DRYT_C SSTP_C DaysSinceRain_C/NORMAL; /* to superimpose curve over the hi
RUN;
 /*Grand*/
PROC MEANS DATA=Work.Grand MIN Q1 MEDIAN Q3 MAX RANGE;
VAR Geomean0_C Geomean5_C MaxTemp_C MinTemp_C MeanTemp_C TotalPrecip_C SpdofMaxGust_C
WaterLVL_C AverageUV_C MaxUV_C AveUV24_C MaxUV24_C Rain24_C Rain48_C VCAR_C VWHS_C
 VCMX_C VTPK_C VTPS_C WDIR_C WSPD_C GSPD_C ATMS_C DRYT_C SSTP_C DaysSinceRain_C;
RUN;
PROC UNIVARIATE DATA=Work.Grand NORMAL;
VAR Geomean0_C Geomean5_C MaxTemp_C MinTemp_C MeanTemp_C TotalPrecip_C SpdofMaxGust_C
WaterLVL_C AverageUV_C MaxUV_C AveUV24_C MaxUV24_C Rain24_C Rain48_C VCAR_C VWHS_C
 VCMX_C VTPK_C VTPS_C WDIR_C WSPD_C GSPD_C ATMS_C DRYT_C SSTP_C DaysSinceRain_C;
HIST Geomean0_C Geomean5_C MaxTemp_C MinTemp_C MeanTemp_C TotalPrecip_C SpdofMaxGust_C
WaterLVL_C AverageUV_C MaxUV_C AveUV24_C MaxUV24_C Rain24_C Rain48_C VCAR_C VWHS_C
 VCMX_C VTPK_C VTPS_C WDIR_C WSPD_C GSPD_C ATMS_C DRYT_C SSTP_C DaysSinceRain_C/NORMAL; /* to superimpose curve over the hi*/
RUN;


/*Creating two datasets: Gimli and Grand beach*/
DATA Gimli1 Grand1;
 SET Work.ManData1R;
 IF (Beach_R=1) then OUTPUT Gimli1;
 IF (Beach_R=2) then OUTPUT Grand1;
RUN;QUIT;


/*Scatter and regression plot: to check for linearity*/
/*GIMLI*/
PROC SGPLOT DATA=Gimli1;
 SCATTER X =MinTemp_C Y=LogGeomean5;
 REG X =MinTemp_C Y=LogGeomean5;
RUN; QUIT;
PROC SGPLOT DATA=Gimli1;
 SCATTER X=MaxTemp_C Y=LogGeomean5;
 REG X=MaxTemp_C Y=LogGeomean5;
RUN; QUIT;
PROC SGPLOT DATA=Gimli1;
 SCATTER X=MeanTemp_C Y=LogGeomean5;
 REG X =MeanTemp_C Y=LogGeomean5;
RUN; QUIT;
PROC SGPLOT DATA=Gimli1;
 SCATTER X=LogTotalPrecip Y=LogGeomean5;
 REG X =LogTotalPrecip  Y=LogGeomean5;
RUN; QUIT;
PROC SGPLOT DATA=Gimli1;
 SCATTER X=LogRain24 Y=LogGeomean5;
 REG X=LogRain24 Y=LogGeomean5;
RUN; QUIT;
PROC SGPLOT DATA=Gimli1;
 SCATTER X=LogRain48 Y=LogGeomean5;
 REG X=LogRain48 Y=LogGeomean5;
RUN; QUIT;
PROC SGPLOT DATA=Gimli1;
 SCATTER X=LogSpdofMaxGust Y=LogGeomean5;
 REG X=LogSpdofMaxGust Y=LogGeomean5;
RUN; QUIT;
PROC SGPLOT DATA=Gimli1;
 SCATTER X=WaterLVL_C Y=LogGeomean5;
 REG X=WaterLVL_C Y=LogGeomean5;
RUN; QUIT;
PROC SGPLOT DATA=Gimli1;
 SCATTER X=AverageUV_C Y=LogGeomean5;
 REG X=AverageUV_C Y=LogGeomean5;
RUN; QUIT;
PROC SGPLOT DATA=Gimli1;
 SCATTER X=AveUV24_C Y=LogGeomean5;
 REG X=AveUV24_C Y=LogGeomean5;
RUN; QUIT;
PROC SGPLOT DATA=Gimli1;
 SCATTER X=MaxUV_C Y=LogGeomean5;
 REG X=MaxUV_C Y=LogGeomean5;
RUN; QUIT;
PROC SGPLOT DATA=Gimli1;
 SCATTER X=MaxUV24_C Y=LogGeomean5;
 REG X=MaxUV24_C Y=LogGeomean5;
RUN; QUIT;
PROC SGPLOT DATA=Gimli1;
 SCATTER X=VCAR_C Y=LogGeomean5;
 REG X=VCAR_C Y=LogGeomean5;
PROC SGPLOT DATA=Gimli1;
 SCATTER X=LogVTPK Y=LogGeomean5;
 REG X=LogVTPK Y=LogGeomean5;
PROC SGPLOT DATA=Gimli1;
 SCATTER X=VWHS_C Y=LogGeomean5;
 REG X=VWHS_C Y=LogGeomean5;
PROC SGPLOT DATA=Gimli1;
 SCATTER X=LogVCMX Y=LogGeomean5;
 REG X=LogVCMX Y=LogGeomean5;
RUN; QUIT;
PROC SGPLOT DATA=Gimli1;

/*GRAND*/
PROC SGPLOT DATA=Grand1;
 SCATTER X=MinTemp_C Y=LogGeomean5;
 REG X=MinTemp_C Y=LogGeomean5;
RUN; QUIT;
PROC SGPLOT DATA=Grand1;
 SCATTER X=MaxTemp_C Y=LogGeomean5;
 REG X=MaxTemp_C Y=LogGeomean5;
RUN; QUIT;
PROC SGPLOT DATA=Grand1;
 SCATTER X=MeanTemp_C Y=LogGeomean5;
 REG X=MeanTemp_C Y=LogGeomean5;
RUN; QUIT;
PROC SGPLOT DATA=Grand1;
 SCATTER X=LogTotalPrecip Y=LogGeomean5;
 REG X=LogTotalPrecip Y=LogGeomean5;
RUN; QUIT;
PROC SGPLOT DATA=Grand1;
 SCATTER X=LogRain24 Y=LogGeomean5;
 REG X=LogRain24 Y=LogGeomean5;
RUN; QUIT;
PROC SGPLOT DATA=Grand1;
 SCATTER X=LogRain48 Y=LogGeomean5;
 REG X=LogRain48 Y=LogGeomean5;
RUN; QUIT;
PROC SGPLOT DATA=Grand1;
 SCATTER X=LogSpdofMaxGust Y=LogGeomean5;
 REG X=LogSpdofMaxGust Y=LogGeomean5;
RUN; QUIT;
PROC SGPLOT DATA=Grand1;
 SCATTER X=WaterLVL_C Y=LogGeomean5;
 REG X=WaterLVL_C Y=LogGeomean5;
RUN; QUIT;
PROC SGPLOT DATA=Grand1;
 SCATTER X=AverageUV_C Y=LogGeomean5;
 REG X=AverageUV_C Y=LogGeomean5;
RUN; QUIT;
PROC SGPLOT DATA=Grand1;
 SCATTER X=AveUV24_C Y=LogGeomean5;
 REG X=AveUV24_C Y=LogGeomean5;
RUN; QUIT;
PROC SGPLOT DATA=Grand1;
 SCATTER X=MaxUV_C Y=LogGeomean5;
 REG X=MaxUV_C Y=LogGeomean5;
RUN; QUIT;
PROC SGPLOT DATA=Grand1;
 SCATTER X=MaxUV24_C Y=LogGeomean5;
 REG X=MaxUV24_C Y=LogGeomean5;
RUN; QUIT;
PROC SGPLOT DATA=Grand1;
 SCATTER X=VCAR_C Y=LogGeomean5;
 REG X=VCAR_C Y=LogGeomean5;
RUN; QUIT;
PROC SGPLOT DATA=Grand1;
 SCATTER X=LogVTPK Y=LogGeomean5;
 REG X=LogVTPK Y=LogGeomean5;
RUN; QUIT;
PROC SGPLOT DATA=Grand1;
 SCATTER X=VWHS_C Y=LogGeomean5;
 REG X=VWHS_C Y=LogGeomean5;
RUN; QUIT;
PROC SGPLOT DATA=Grand1;
 SCATTER X=LogVCMX Y=LogGeomean5;
 REG X=LogVCMX Y=LogGeomean5;
RUN; QUIT;
PROC SGPLOT DATA=Grand1;
 SCATTER X=LogVTPS Y=LogGeomean5;
 REG X=LogVTPS Y=LogGeomean5;
RUN; QUIT;
PROC SGPLOT DATA=Grand1;
 SCATTER X=WDIR_C Y=LogGeomean5;
 REG X=WDIR_C Y=LogGeomean5;
RUN; QUIT;
PROC SGPLOT DATA =Grand1;
 SCATTER X=WSPD_C Y=LogGeomean5;
 REG X=WSPD_C Y=LogGeomean5;
RUN; QUIT;
PROC SGPLOT DATA=Grand1;
 SCATTER X=GSPD_C Y=LogGeomean5;
 REGX=GSPD_C Y=LogGeomean5;
RUN; QUIT;
PROC SGPLOT DATA=Grand1 ;
 SCATTER X=ATMS_C Y=LogGeomean5;
 REG X=ATMS_C Y=LogGeomean5;
RUN; QUIT;
PROC SGPLOT DATA=Grand1;
 SCATTER X=DRYT_C Y =LogGeomean5;
 REG X=DRYT_C Y=LogGeomean5;
RUN; QUIT;
PROC SGPLOT DATA=Grand1;
 SCATTER X=LogSSTP Y=LogGeomean5;
 REG X=LogSSTP Y=LogGeomean5
;RUN; QUIT;
/*UNIVARIATE Linear Regression Analysis*/
/*GIMLI*/
PROC REG DATA=Work.Gimli1;
MODEL LogGeomean5=Date_N;
 FORMAT Date_N yearfmt.;
RUN;
PROC REG DATA=Work.Gimli1;
MODEL LogGeomean5=MinTemp_C;
RUN;
PROC REG DATA=Work.Gimli1;
MODEL LogGeomean5=MaxTemp_C;
RUN;
PROC REG DATA=Work.Gimli1;
MODEL LogGeomean5=MeanTemp_C;
RUN;
PROC REG DATA=Work.Gimli1;
MODEL LogGeomean5=LogTotalPrecip;
RUN;
PROC REG DATA=Work.Gimli1;
MODEL LogGeomean5=LogRain24;
RUN;
PROC REG DATA=Work.Gimli1;
MODEL LogGeomean5=LogRain48;
RUN;
PROC REG DATA=Work.Gimli1;
MODEL LogGeomean5=LogSpdofMaxGust;
RUN;
PROC REG DATA=Work.Gimli1;
MODEL LogGeomean5=WaterLVL_C;
RUN;
PROC REG DATA=Work.Gimli1;
MODEL LogGeomean5=AverageUV_C;
RUN;
PROC REG DATA=Work.Gimli1;
MODEL LogGeomean5=AveUV24_C;
RUN;
PROC REG DATA=Work.Gimli1;
MODEL LogGeomean5=MaxUV_C;
RUN;
PROC REG DATA=Work.Gimli1;
MODEL LogGeomean5=MaxUV24_C;
RUN;
PROC REG DATA=Work.Gimli1;
MODEL LogGeomean5=VCAR_C;
RUN;
PROC REG DATA=Work.Gimli1;
MODEL LogGeomean5=LogVTPK;
RUN;
PROC REG DATA=Work.Gimli1;
MODEL LogGeomean5=VWHS_C;
RUN;
PROC REG DATA=Work.Gimli1;
MODEL LogGeomean5=LogVCMX;
RUN;
PROC REG DATA=Work.Gimli1;
MODEL LogGeomean5=LogVTPS;
RUN;
PROC REG DATA=Work.Gimli1;
MODEL LogGeomean5=WDIR_C;
RUN;
PROC REG DATA=Work.Gimli1;
MODEL LogGeomean5=WSPD_C;
RUN;
PROC REG DATA=Work.Gimli1;
MODEL LogGeomean5=GSPD_C;
RUN;
PROC REG DATA=Work.Gimli1;
MODEL LogGeomean5=ATMS_C;
RUN;
PROC REG DATA=Work.Gimli1;
MODEL LogGeomean5=DRYT_C;
RUN;
PROC REG DATA=Work.Gimli1;
MODEL LogGeomean5=LogSSTP;
RUN;
/*GRAND*/
PROC REG DATA=Work.Grand1;
MODEL LogGeomean5=Date_N;
 FORMAT Date_N yearfmt.;
RUN;
PROC REG DATA=Work.Grand1;
MODEL LogGeomean5=MinTemp_C;
RUN;
PROC REG DATA=Work.Grand1;
MODEL LogGeomean5=MaxTemp_C;
RUN;
PROC REG DATA=Work.Grand1;
MODEL LogGeomean5=MeanTemp_C;
RUN;
PROC REG DATA=Work.Grand1;
MODEL LogGeomean5=LogTotalPrecip;
RUN;
PROC REG DATA=Work.Grand1;
MODEL LogGeomean5=LogRain24;
RUN;
PROC REG DATA=Work.Grand1;
MODEL LogGeomean5=LogRain48;
RUN;
PROC REG DATA=Work.Grand1;
MODEL LogGeomean5=LogSpdofMaxGust;
RUN;
PROC REG DATA=Work.Grand1;
MODEL LogGeomean5=WaterLVL_C;
RUN;
PROC REG DATA=Work.Grand1;
MODEL LogGeomean5=AverageUV_C;
RUN;
PROC REG DATA=Work.Grand1;
MODEL LogGeomean5=AveUV24_C;
RUN;
PROC REG DATA=Work.Grand1;
MODEL LogGeomean5=MaxUV_C;
RUN;
PROC REG DATA=Work.Grand1;
MODEL LogGeomean5=MaxUV24_C;
RUN;
PROC REG DATA=Work.Grand1;
MODEL LogGeomean5=VCAR_C;
RUN;
PROC REG DATA=Work.Grand1;
MODEL LogGeomean5=LogVTPK;
RUN;
PROC REG DATA=Work.Grand1;
MODEL LogGeomean5=VWHS_C;
RUN;
PROC REG DATA=Work.Grand1;
MODEL LogGeomean5=LogVCMX;
RUN;
PROC REG DATA=Work.Grand1;
MODEL LogGeomean5=LogVTPS;
RUN;
PROC REG DATA=Work.Grand1;
MODEL LogGeomean5=WDIR_C;
RUN;
PROC REG DATA=Work.Grand1;
MODEL LogGeomean5=WSPD_C;
RUN;
PROC REG DATA=Work.Grand1;
MODEL LogGeomean5=GSPD_C;
RUN;
PROC REG DATA=Work.Grand1;
MODEL LogGeomean5=ATMS_C;
RUN;
PROC REG DATA=Work.Grand1;
MODEL LogGeomean5=DRYT_C;
RUN;
PROC REG DATA=Work.Grand1;
MODEL LogGeomean5=LogSSTP;
RUN;

/*Test of collinearity*/
PROC REG DATA=Work.Gimli1;
MODEL LogGeomean5=MinTemp_C LogRain48 LogSpdofMaxGust WaterLVL_C AveUV24_C DaysSinceRain_C/ tol vif collin;
RUN;
PROC REG DATA=Work.Grand1;
MODEL LogGeomean5=MinTemp_C LogRain48 LogSpdofMaxGust WaterLVL_C AveUV24_C DaysSinceRain_C/ tol vif collin;
RUN;

/*PATH ANALYSIS*/
/*Test of collinearity*/
PROC REG DATA=Work.Gimli1;
MODEL LogGeomean5=MeanTemp24_C Rain48_C LogPrevGeomean SpdMaxGust24_C WaterLVL_C AveUV24_C / tol vif collin;
RUN;
PROC REG DATA=Work.Grand1;
MODEL LogGeomean5=MeanTemp24_C Rain48_C LogPrevGeomean SpdMaxGust24_C WaterLVL_C AveUV24_C / tol vif collin;
RUN;
PROC REG DATA=Work.Gimli2;
MODEL LogGeomean5=MeanTemp24_C Rain48_C LogPrevGeomean SpdMaxGust24_C WaterLVL_C AveUV24_C 
VCAR_C GSPD24_C / tol vif collin;
RUN;
PROC REG DATA=Work.Grand2;
MODEL LogGeomean5=MeanTemp24_C Rain48_C LogPrevGeomean SpdMaxGust24_C WaterLVL_C AveUV24_C 
VCAR_C GSPD24_C / tol vif collin;
RUN;


/*Creating correlation/covariance matrix*/
PROC CORR DATA=ManData1S noprob outp=OutCorr COV;
VAR LogGeomean5 Rain48_C MeanTemp24_C LogPrevGeomean WaterLVL_C AveUV24_C DaysSinceRain_C;
RUN;
PROC CORR DATA=Gimli1 noprob outp=OutCorr COV;
VAR LogGeomean5 Rain48_C MeanTemp24_C LogPrevGeomean WaterLVL_C AveUV24_C DaysSinceRain_C;
RUN;
PROC CORR DATA=Grand1 noprob outp=OutCorr COV;
VAR LogGeomean5 Rain48_C MeanTemp24_C LogPrevGeomean WaterLVL_C AveUV24_C DaysSinceRain_C;
RUN;
PROC CORR DATA=ManData1T noprob outp=OutCorr COV;
VAR LogGeomean5 Rain48_C MeanTemp24_C LogPrevGeomean WaterLVL_C AveUV24_C SpdMaxGust24_C
GSPD24_C VCAR_C;
RUN;
PROC CORR DATA=Gimli2 noprob outp=OutCorr COV;
VAR LogGeomean5 Rain48_C MeanTemp24_C LogPrevGeomean WaterLVL_C AveUV24_C SpdMaxGust24_C
GSPD24_C VCAR_C;
RUN;
PROC CORR DATA=Grand2 noprob outp=OutCorr COV;
VAR LogGeomean5 Rain48_C MeanTemp24_C LogPrevGeomean WaterLVL_C AveUV24_C SpdMaxGust24_C
GSPD24_C VCAR_C;
RUN;
PROC SURVEYFREQ DATA=ManData1S;
TABLE SpdofMaxGust_C SpdMaxGust24_C;
RUN;

/*Structural model - path analysis*/
/*Overall - 2007 to 2021*/
DATA Overall1(type=corr);
INPUT _TYPE_ $ _NAME_ $4. Var1 Var2 Var3 Var4 Var5 Var6;
CARDS;
N 1625 1469 1625 1580 1591 1589
MEAN 3.35 16.56 5.41 3.36 217.99 1.40
STD 1.41 4.30 10.66 1.41 0.29 0.41
CORR Var1 1.000 0.218 0.132 0.379 0.067 -0.058
CORR Var2 0.218 1.000 -0.026 0.074 0.131 0.355
CORR Var3 0.132 -0.026 1.000 0.030 0.172 -0.251 
CORR Var4 0.379 0.074 0.030 1.000 -0.031 0.002 
CORR Var5 0.067 0.131 0.172 -0.031 1.000 -0.023
CORR Var6 -0.058 0.355 -0.251 0.002 -0.023 1.000
;
PROC PRINT DATA=Overall1;
PROC CALIS PRINT;
LINEQS
Var5 = Gi1 Var2 + Gi2 Var3 + E1,
Var1 = Gi3 Var5+ Gi4 Var2 + Gi5 Var3 + Gi6 Var4 + Gi7 Var6 + E2;
STD E1-E2 = V1-V2;
RUN;

/*Gimli - 2007 to 2021*/
DATA Gimli3(type=corr);
INPUT _TYPE_ $ _NAME_ $4. Var1 Var2 Var3 Var4 Var5 Var6 Var7;
CARDS;
N 735 735 663 720 716 721 735
MEAN 3.72 5.05 16.40 3.71 218.0 1.41 1.73
STD 1.49 9.61 4.42 1.50 0.28 0.41 2.48
CORR Var1 1.000 0.164 0.172 0.397 0.066 -0.058 -0.007
CORR Var2 0.164 1.000 -0.013 0.078 0.155 -0.216 -0.252
CORR Var3 0.172 -0.013 1.000 0.087 0.139 0.336 -0.045 
CORR Var4 0.397 0.078 0.087 1.000 -0.022 -0.006 0.078
CORR Var5 0.066 0.155 0.139 -0.022 1.000 -0.031 -0.151
CORR Var6 -0.058 -0.216 0.336 -0.006 -0.031 1.000 0.040
CORR Var7 -0.007 -0.252 -0.045 0.078 -0.151 0.040 1.000
;
PROC PRINT DATA=Gimli3;
PROC CALIS PRINT;
LINEQS
Var5 = Gi1 Var2 + Gi2 Var3 + E1,
Var1 = Gi3 Var5+ Gi4 Var2 + Gi5 Var3 + Gi6 Var4 + Gi7 Var6 + Gi8 Var7 + E2;
STD E1-E2 = V1-V2;
RUN;

/*Grand - 2007 to 2021*/
DATA Grand3(type=corr);
INPUT _TYPE_ $ _NAME_ $4. Var1 Var2 Var3 Var4 Var5 Var6 Var7;
CARDS;
N 890 890 806 860 875 868 890
MEAN 3.06 5.71 16.68 3.06 217.98 1.40 1.66
STD 1.27 11.46 4.19 1.27 0.29 0.41 2.28
CORR Var1 1.000 0.131 0.296 0.282 0.063 -0.066 -0.095
CORR Var2 0.131 1.000 -0.038 0.007 0.186 -0.277 -0.222
CORR Var3 0.296 -0.038 1.000 0.085 0.126 0.373 -0.050
CORR Var4 0.282 0.007 0.085 1.000 -0.050 0.005 -0.036
CORR Var5 0.063 0.186 0.126 -0.050 1.000 -0.016 -0.123
CORR Var6 -0.066 -0.277 0.373 0.005 -0.016 1.000 0.076
CORR Var7 -0.095 -0.222 -0.050 -0.036 -0.123 0.076 1.000
;
PROC PRINT DATA=Grand3;
PROC CALIS PRINT;
LINEQS
Var5 = Gi1 Var2 + Gi2 Var3 + E1,
Var1 = Gi3 Var5+ Gi4 Var2 + Gi5 Var3 + Gi6 Var4 + Gi7 Var6 + Gi8 Var7 + E2;
STD E1-E2 = V1-V2;
RUN;

/*Overall - 2012 to 2021*/
DATA Overall2(type=corr);
INPUT _TYPE_ $ _NAME_ $4. Var1 Var2 Var3 Var4 Var5 Var6 Var7 Var8;
CARDS;
N 718 568 718 688 684 693 541 596
MEAN 3.55 17.23 5.16 3.55 217.91 1.41 6.14 0.49
STD 1.43 3.53 11.29 1.44 0.27 0.40 2.36 0.32
CORR Var1 1.000 0.232 0.150 0.281 0.046 -0.097 0.115 0.313
CORR Var2 0.232 1.000 -0.021 0.020 0.006 0.325 -0.139 -0.054
CORR Var3 0.150 -0.021 1.000 0.025 0.224 -0.214 0.088 0.161 
CORR Var4 0.281 0.020 0.025 1.000 -0.076 -0.045 0.094 0.178 
CORR Var5 0.046 0.006 0.224 -0.076 1.000 -0.072 0.104 -0.124
CORR Var6 -0.097 0.325 -0.214 -0.045 -0.072 1.000 -0.262 -0.257
CORR Var7 0.115 -0.139 0.088 0.094 0.104 -0.262 1.000 0.303
CORR Var8 0.313 -0.054 0.161 0.178 -0.124 -0.257 0.303 1.000
;
PROC PRINT DATA=Overall2;
PROC CALIS PRINT;
LINEQS
Var5 = Gi1 Var2 + Gi2 Var3 + E1,
Var8 = Gi8 Var7 + E2,
Var1 = Gi3 Var5 + Gi4 Var2 + Gi5 Var3 + Gi6 Var4 + Gi7 Var6 + Gi9 Var8 + Gi10 Var7 + E3;
STD E1-E3 = V1-V3;
RUN;

/*Gimli - 2012 to 2021*/
DATA Gimli4(type=corr);
INPUT _TYPE_ $ _NAME_ $4. Var1 Var2 Var3 Var4 Var5 Var6 Var7 Var8;
CARDS;
N 290 221 290 280 271 282 218 245
MEAN 3.88 17.03 4.51 3.88 217.92 1.41 6.07 0.47
STD 1.51 3.66 9.74 1.51 0.25 0.40 2.39 0.31
CORR Var1 1.000 0.271 0.193 0.289 0.101 -0.028 0.048 0.331
CORR Var2 0.271 1.000 -0.008 0.114 0.057 0.317 -0.135 0.006
CORR Var3 0.193 -0.008 1.000 0.093 0.162 -0.157 0.122 0.083 
CORR Var4 0.289 0.114 0.093 1.000 -0.028 -0.009 0.102 0.219 
CORR Var5 0.101 0.057 0.162 -0.028 1.000 -0.062 0.010 -0.144
CORR Var6 -0.028 0.317 -0.157 -0.009 -0.062 1.000 -0.204 -0.186
CORR Var7 0.048 -0.135 0.122 0.102 0.010 -0.204 1.000 0.271
CORR Var8 0.331 0.006 0.083 0.219 -0.144 -0.186 0.271 1.000
;
PROC PRINT DATA=Gimli4;
PROC CALIS PRINT;
LINEQS
Var5 = Gi1 Var2 + Gi2 Var3 + E1,
Var8 = Gi8 Var7 + E2,
Var1 = Gi3 Var5 + Gi4 Var2 + Gi5 Var3 + Gi6 Var4 + Gi7 Var6 + Gi9 Var8 + Gi10 Var7 + E3;
STD E1-E3 = V1-V3;
RUN;

/*Grand - 2012 to 2021*/
DATA Grand4(type=corr);
INPUT _TYPE_ $ _NAME_ $4. Var1 Var2 Var3 Var4 Var5 Var6 Var7 Var8;
CARDS;
N 428 374 428 408 413 411 323 351
MEAN 3.33 17.36 5.59 3.33 217.90 1.42 6.19 0.50
STD 1.34 3.44 12.22 1.34 0.28 0.39 2.33 0.33
CORR Var1 1.000 0.228 0.147 0.226 0.004 -0.150 0.180 0.322
CORR Var2 0.228 1.000 -0.030 -0.037 -0.023 0.330 -0.143 -0.094
CORR Var3 0.147 -0.030 1.000 -0.001 0.255 -0.249 0.070 0.200 
CORR Var4 0.226 -0.037 -0.001 1.000 -0.118 -0.073 0.098 0.167 
CORR Var5 0.004 -0.023 0.255 -0.118 1.000 -0.078 0.162 -0.113
CORR Var6 -0.150 0.330 -0.249 -0.073 -0.078 1.000 -0.303 -0.305
CORR Var7 0.180 -0.143 0.070 0.098 0.162 -0.303 1.000 0.323
CORR Var8 0.322 -0.094 0.200 0.167 -0.113 -0.305 0.323 1.000
;
PROC PRINT DATA=Grand4;
PROC CALIS PRINT;
LINEQS
Var5 = Gi1 Var2 + Gi2 Var3 + E1,
Var8 = Gi8 Var7 + E2,
Var1 = Gi3 Var5 + Gi4 Var2 + Gi5 Var3 + Gi6 Var4 + Gi7 Var6 + Gi9 Var8 + Gi10 Var7 + E3;
STD E1-E3 = V1-V3;
RUN;

/*MULTI-lEVEL MODELLING - LINEAR */
/*First level - random intercept model*/
PROC MIXED DATA=Work.Gimli1 COVTEST NOCLPRINT METHOD= ML;
CLASS Year;
MODEL LogGeomean5=/SOLUTION DDFM = SATTERTHWAITE;
RANDOM INTERCEPT / SUBJECT=Year TYPE=VC;
PROC MIXED DATA=Work.Grand1 COVTEST NOCLPRINT METHOD= ML;
CLASS Year;
MODEL LogGeomean5=/SOLUTION DDFM = SATTERTHWAITE;
RANDOM INTERCEPT / SUBJECT=Year TYPE=VC;

/*Since the ICC value is at least 10% and significant we run the 
two-level regression analysis - year as a random effect variable*/
/*Second level - random intercept model*/
PROC MIXED DATA=Work.Gimli1 COVTEST NOCLPRINT METHOD= ML;
CLASS Year;
MODEL LogGeomean5= Rain48_C MeanTemp24_C LogPrevGeomean AveUV24_C*DaysSinceRain_C/SOLUTION DDFM = SATTERTHWAITE;
RANDOM INTERCEPT / SUBJECT=Year TYPE=VC;
PROC MIXED DATA=Work.Grand1 COVTEST NOCLPRINT METHOD= ML;
CLASS Year;
MODEL LogGeomean5= Rain48_C MeanTemp24_C LogPrevGeomean AveUV24_C*DaysSinceRain_C/SOLUTION DDFM = SATTERTHWAITE;
RANDOM INTERCEPT / SUBJECT=Year TYPE=VC;

/*Test of collinearity*/
PROC REG DATA=Work.Gimli1;
MODEL LogGeomean5=MeanTemp24_C Rain48_C LogPrevGeomean AveUV24_C SpdMaxGust24_C Year/ tol vif collin;
RUN;
PROC REG DATA=Work.Grand1;
MODEL LogGeomean5=MeanTemp24_C Rain48_C LogPrevGeomean AveUV24_C SpdMaxGust24_C Year/ tol vif collin;
RUN;

/*Multi-level modelling - LOGISITIC*/
/*First level - random intercept model*/
/*Gimli*/
PROC GLIMMIX DATA=Work.Gimli1 METHOD=LAPLACE NOCLPRINT;
CLASS Year;
MODEL GeomeanCat (EVENT='1')=/CL DIST=BINARY LINK=LOGIT SOLUTION;
RANDOM INTERCEPT / SUBJECT=Year S CL TYPE=VC;
COVTEST /WALD;
/*Grand*/
PROC GLIMMIX DATA=Work.Grand1 METHOD=LAPLACE NOCLPRINT;
CLASS Year;
MODEL GeomeanCat (EVENT='1')=/CL DIST=BINARY LINK=LOGIT SOLUTION;
RANDOM INTERCEPT / SUBJECT=Year S CL TYPE=VC;
COVTEST /WALD;

/*Since the ICC value is at least 10% and significant we run the 
two-level regression analysis - year as a random effect variable*/
/*Second level - random intercept model*/
/*Gimli*/
PROC GLIMMIX DATA=Work.Gimli1 METHOD=LAPLACE NOCLPRINT;
CLASS Year;
MODEL GeomeanCat (EVENT='1')= MeanTemp24_C Rain48_C LogPrevGeomean AveUV24_C DaysSinceRain_C/CL DIST=BINARY LINK=LOGIT 
SOLUTION CL ODDSRATIO (DIFF=FIRST LABEL);
RANDOM INTERCEPT / SUBJECT=Year S CL TYPE=VC;
COVTEST /WALD;
/*Grand*/
PROC GLIMMIX DATA=Work.Grand1 METHOD=LAPLACE NOCLPRINT;
CLASS Year;
MODEL GeomeanCat (EVENT='1')= MeanTemp24_C Rain48_C LogPrevGeomean AveUV24_C DaysSinceRain_C/CL DIST=BINARY LINK=LOGIT 
SOLUTION CL ODDSRATIO (DIFF=FIRST LABEL);
RANDOM INTERCEPT / SUBJECT=Year S CL TYPE=VC;
COVTEST /WALD;