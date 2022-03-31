****************************************************************************************************************************************
*Changes in the body mass index and blood pressure association across time: Evidence from multiple cross-sectional and cohort studies
****************************************************************************************************************************************

use "N:\Blood Pressure\BP and BMI\Datasets\HSE1994_2018_BP.dta", clear
renvars, lower

*=============================.
*HSE flowchart: 1994 to 2018.
*============================.

*Valid BP and medicine data (N=141,893)

preserve
keep if (ag16g10>=2)
tab1 hseyear
restore

*Valid BP, medicine and educ data (N=136,942)
*Educ excludes 'other'.

preserve
keep if (ag16g10>=2) & inrange(topqual5,1,4)
tab1 hseyear 
restore

*Valid BP, medicine, educ & BMI data (N=126,742)
*Educ excludes 'other'.

preserve
keep if (ag16g10>=2) & inrange(topqual5,1,4) & (bmival2>0 & bmival2!=.)
tab1 hseyear 
restore


*********************
*****Table 1.
*********************

preserve /* 1994 */
keep if hseyear==1 & (ag16g10>=2)
tab1 hseyear                                  /* valid BP & med */
tab1 topqual5
di 18+208                                     /* missing educ */
keep if inrange(topqual5,1,4)
count
count if bmival2==.
summ bmival2                                  /* missing BMI */
restore

preserve /* 2003 */
keep if hseyear==10 & (ag16g10>=2)
tab1 hseyear                                  /* valid BP & med */
tab1 topqual5
di 4+420                                    /* missing educ */
keep if inrange(topqual5,1,4)
count
count if bmival2==.
summ bmival2                                  /* missing BMI */
restore

preserve /* 2018 */
keep if hseyear==25 & (ag16g10>=2)
tab1 hseyear                                  /* valid BP & med */
tab1 topqual5
di 5+39                                    /* missing educ */
keep if inrange(topqual5,1,4)
count if bmival2==-1
summ bmival2 if bmival2>0                                 /* missing BMI */
restore


*Demographics.
gen male = sex==1
gen female = sex==2
*Age-specific (25-54; 55+).
gen age2=0
replace age2=1 if inrange(ag16g10,2,4)
replace age2=2 if inrange(ag16g10,5,7)
*Age 40-49.
gen age3 = inrange(age,40,49)

************************
*SEP (exclude 'other')
************************

keep if inrange(topqual5,1,4)
drop if flag==1
summ age  /* Age 25+ */

*******************************************************************
*BMI (all missing set to "."): but RETAINED in the dataset for now.
*******************************************************************

mvdecode bmival2,mv(-1)
gen obese = (bmival2>29.999 & bmival2!=.)
replace obese=. if bmival2==.

*Account for Tx: add 10/5 mmHg.
generate omsysval10=-2
replace omsysval10 = omsysval if (bpmedd==0)
replace omsysval10 = omsysval+10 if (bpmedd==1)
generate omdiaval5=-2
replace omdiaval5 = omdiaval if (bpmedd==0)
replace omdiaval5 = omdiaval+5 if (bpmedd==1)

*Hypertension (survey-defined).
generate htn = (highbp4==2|highbp4==3|highbp4==4)

*Medication.
generate BPmed = (highbp4==2|highbp4==3)

*Controlled.
generate BPcontrol = (highbp4==2)

*Degree.

generate degree = topqual5==1

*svyset the data
svyset [pweight=wt_nurse],psu(point1)

*******************************************************************
****** Table 1 (N=136,942): includes those with BP but missing BMI.
*******************************************************************

svy: mean omsysval omdiaval,over(hseyear)   
estat sd
svy: mean htn,over(hseyear)         
svy: mean BPmed,over(hseyear) 

svy: mean omsysval10 omdiaval5,over(hseyear)   
estat sd
svy:mean bmival2,over(hseyear)        
estat sd
svy:mean obese,over(hseyear)   
svy:mean degree,over(hseyear) 

***********************
*Supplementary Figure 3
***********************

***raw.
twoway (histogram omsysval if hseyear==1 , percent start(30) width(3) color(red%30)) ///
(histogram omsysval if hseyear==10, percent start(30) width(3) color(green%30)) ///
(histogram omsysval if hseyear==25, percent start(30) width(3) color(blue%30)) , ///
legend(rows(1) order(1 "1994" 2 "2003" 3 "2018"))  ///
xtitle("SBP mmHg (without accounting for treatment)")  ylabel(0(2)8)  

**after treatment.         
twoway (histogram omsysval10 if hseyear==1 , percent start(30) width(3) color(red%30)) ///
(histogram omsysval10 if hseyear==10, percent start(30) width(3) color(green%30)) ///
(histogram omsysval10 if hseyear==25, percent start(30) width(3) color(blue%30)) , ///
legend(rows(1) order(1 "1994" 2 "2003" 3 "2018" ))  ///
xtitle("SBP mmHg (accounting for treatment)")  ylabel(0(2)8) 

**BMI.
preserve
drop if bmival2==.
count
twoway (histogram bmival2 if hseyear==1 , percent start(10) width(0.5) color(red%30)) ///
(histogram bmival2 if hseyear==10, percent start(10) width(0.5) color(green%30)) ///
(histogram bmival2 if hseyear==25, percent start(10) width(0.5) color(blue%30)) , ///
legend(rows(1) order(1 "1994" 2 "2003" 3 "2018" )) ///
xtitle("BMI (kg/m{sup:2})") ylabel(0(2)8) 			   
restore

**************************************************
*Drop missing BMI (this is the analytical sample).
***************************************************

drop if bmival2==.

********************************************************
*Supplementary Figure 4.
*SBP (25+).
*Cohort-pooled; By year.
*(i) adjust for sex+age; (ii) +treatment; (iii) +education.
********************************************************

*Pooled across years.

svy:regress omsysval c.bmival2 i.sex i.ag16g10                         
svy:regress omsysval10 c.bmival2 i.sex i.ag16g10                       
svy:regress omsysval10 c.bmival2 i.sex i.ag16g10 ib1.topqual5         

*Year-specific.

foreach num of numlist 1/5 7/10 12/25 {
generate subpop=0
replace subpop=1 if hseyear==`num'
svy,subpop(subpop): regress omsysval c.bmival2 i.sex i.ag16g10                  
svy,subpop(subpop): regress omsysval10 c.bmival2 i.sex i.ag16g10               
svy,subpop(subpop): regress omsysval10 c.bmival2 i.sex i.ag16g10 ib1.topqual5    
drop subpop
}

********************************************************
*Figure 1: SBP: 25+.
*Adjust for age; sex; treatment; education.
********************************************************

*Pooled across years.
svy:regress omsysval10 c.bmival2 i.sex i.ag16g10 ib1.topqual5          

*Year-specific.
foreach num of numlist 1/5 7/10 12/25 {
generate subpop=0
replace subpop=1 if hseyear==`num'
svy,subpop(subpop): regress omsysval10 c.bmival2 i.sex i.ag16g10 ib1.topqual5    
drop subpop
}

********************************************************
*Figure 2A: SBP: 25-54.
*Adjust age; sex; treatment; education.
********************************************************

gen age2554 = inrange(ag16g10,2,4)

*All-years.
svy,subpop(age2554):regress omsysval10 c.bmival2 i.sex i.ag16g10 ib1.topqual5      
*Year-specific.
foreach num of numlist 1/5 7/10 12/25 {
generate subpop=0
replace subpop=1 if hseyear==`num' & age2554==1
svy,subpop(subpop): regress omsysval10 c.bmival2 i.sex i.ag16g10 ib1.topqual5
drop subpop
}

********************************************************
*Figure 2B: SBP: 55+.
*Adjust age; sex; treatment; education.
********************************************************

gen age55plus = inrange(ag16g10,5,7)
svy,subpop(age55plus):regress omsysval10 c.bmival2 i.sex i.ag16g10 ib1.topqual5       
*Year-specific.
foreach num of numlist 1/5 7/10 12/25 {
generate subpop=0
replace subpop=1 if hseyear==`num' & age55plus==1
svy,subpop(subpop): regress omsysval10 c.bmival2 i.sex i.ag16g10 ib1.topqual5
drop subpop
}

********************************************************
*Supplementary Figure S5: 40-49 yrs.
*Adjust age; sex; treatment; education.
********************************************************
svy,subpop(age3):regress omsysval10 c.bmival2 i.sex i.ag16g10 ib1.topqual5      

*Year-specific.
foreach num of numlist 1/5 7/10 12/25 {
generate subpop=0
replace subpop=1 if hseyear==`num' & age3==1
svy,subpop(subpop): regress omsysval10 c.bmival2 i.ag16g10 ib1.topqual5
drop subpop
}

*********************************************************
*Supplementary Table 1.  
*Test of change across time in the BMI-SBP association 
*(interaction terms for BMI*year)
*25+; 25-54; 55+; 40-49
***********************************************************

*Age 25+
gen yearsa = inlist(hseyear,1,2,3,4,5,6,7,8,9) 
gen yearsb = inlist(hseyear,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25) 
svy:regress omsysval10 c.bmival2 i.sex i.ag16g10 ib1.topqual5 c.hseyear c.bmival2#c.hseyear               /* Slope for year */
svy,subpop(yearsa):regress omsysval10 c.bmival2 i.sex i.ag16g10 ib1.topqual5 c.hseyear c.bmival2#c.hseyear   /* pre-2003 */
svy,subpop(yearsb):regress omsysval10 c.bmival2 i.sex i.ag16g10 ib1.topqual5 c.hseyear c.bmival2#c.hseyear   /* 2003 onwards */
drop yearsa yearsb

*Age 25-54
gen yearsa = inlist(hseyear,1,2,3,4,5,6,7,8,9) & age2554==1
gen yearsb = inlist(hseyear,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25) & age2554==1
svy,subpop(age2554):regress omsysval10 c.bmival2 i.sex i.ag16g10 ib1.topqual5 c.hseyear c.bmival2#c.hseyear      /* All years */
svy,subpop(yearsa):regress omsysval10 c.bmival2 i.sex i.ag16g10 ib1.topqual5 c.hseyear c.bmival2#c.hseyear     /* Pre 2003 */
svy,subpop(yearsb):regress omsysval10 c.bmival2 i.sex i.ag16g10 ib1.topqual5 c.hseyear c.bmival2#c.hseyear      /* 2003 onwards */
drop yearsa yearsb

*Age 55+
gen yearsa = inlist(hseyear,1,2,3,4,5,6,7,8,9) & age55plus==1
gen yearsb = inlist(hseyear,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25) & age55plus==1
svy,subpop(age55plus):regress omsysval10 c.bmival2 i.sex i.ag16g10 ib1.topqual5 c.hseyear c.bmival2#c.hseyear      /* All years */
svy,subpop(yearsa):regress omsysval10 c.bmival2 i.sex i.ag16g10 ib1.topqual5 c.hseyear c.bmival2#c.hseyear      /* Pre 2003 */
svy,subpop(yearsb):regress omsysval10 c.bmival2 i.sex i.ag16g10 ib1.topqual5 c.hseyear c.bmival2#c.hseyear      /* 2003 onwards */
drop yearsa yearsb

*age 40-49
gen yearsa = inlist(hseyear,1,2,3,4,5,6,7,8,9) & age3==1
gen yearsb = inlist(hseyear,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25) & age3==1
svy,subpop(age3):regress omsysval10 c.bmival2 i.sex i.ag16g10 ib1.topqual5 c.hseyear c.bmival2#c.hseyear               /* Slope for year */
svy,subpop(yearsa):regress omsysval10 c.bmival2 i.sex i.ag16g10 ib1.topqual5 c.hseyear c.bmival2#c.hseyear               /* pre-2003 */
svy,subpop(yearsb):regress omsysval10 c.bmival2 i.sex i.ag16g10 ib1.topqual5 c.hseyear c.bmival2#c.hseyear               /* 2003 onwards */
drop yearsa yearsb

***************************************
*Supplementary Figure 8.
*DBP: 25+.
*adjust for sex; treatment; education.
***************************************

svy:regress omdiaval c.bmival2 i.sex i.ag16g10                 /* All years */
svy:regress omdiaval5 c.bmival2 i.sex i.ag16g10                /* All years */
svy:regress omdiaval5 c.bmival2 i.sex i.ag16g10 ib1.topqual5       /* All years */

*Year-specific.
foreach num of numlist 1/5 7/10 12/25 {
generate subpop=0
replace subpop=1 if hseyear==`num'
svy,subpop(subpop): regress omdiaval c.bmival2 i.sex i.ag16g10 
svy,subpop(subpop): regress omdiaval5 c.bmival2 i.sex i.ag16g10
svy,subpop(subpop): regress omdiaval5 c.bmival2 i.sex i.ag16g10 ib1.topqual5
drop subpop
}


****************************************
*Supplementary Figure 9B.
*SBP: 25+: sex-stratified
*adjust for treatment; education.
*****************************************

*All years.
svy,subpop(male): regress omsysval c.bmival2 i.ag16g10 
svy,subpop(male): regress omsysval10 c.bmival2 i.ag16g10 
svy,subpop(male): regress omsysval10 c.bmival2 i.ag16g10 ib1.topqual5

*Year-specific.
foreach num of numlist 1/5 7/10 12/25 {
generate subpop=0
replace subpop=1 if hseyear==`num' & sex==1
svy,subpop(subpop): regress omsysval c.bmival2 i.ag16g10
svy,subpop(subpop): regress omsysval10 c.bmival2 i.ag16g10
svy,subpop(subpop): regress omsysval10 c.bmival2 i.ag16g10 ib1.topqual5
drop subpop
}

*All years.
svy,subpop(female): regress omsysval c.bmival2 i.ag16g10 
svy,subpop(female): regress omsysval10 c.bmival2 i.ag16g10 
svy,subpop(female): regress omsysval10 c.bmival2 i.ag16g10 ib1.topqual5

*Year-specific.
foreach num of numlist 1/5 7/10 12/25 {
generate subpop=0
replace subpop=1 if hseyear==`num' & sex==2
svy,subpop(subpop): regress omsysval c.bmival2 i.ag16g10
svy,subpop(subpop): regress omsysval10 c.bmival2 i.ag16g10
svy,subpop(subpop): regress omsysval10 c.bmival2 i.ag16g10 ib1.topqual5
drop subpop
}

*********************************************
*Supplementary Figure 10B.
*DBP: 25+: sex-stratified
*adjust for treatment; education.
*********************************************

*All years.
svy,subpop(male): regress omdiaval c.bmival2 i.ag16g10 
svy,subpop(male): regress omdiaval5 c.bmival2 i.ag16g10 
svy,subpop(male): regress omdiaval5 c.bmival2 i.ag16g10 ib1.topqual5

*Year-specific.
foreach num of numlist 1/5 7/10 12/25 {
generate subpop=0
replace subpop=1 if hseyear==`num' & sex==1
svy,subpop(subpop): regress omdiaval c.bmival2 i.ag16g10
svy,subpop(subpop): regress omdiaval5 c.bmival2 i.ag16g10
svy,subpop(subpop): regress omdiaval5 c.bmival2 i.ag16g10 ib1.topqual5
drop subpop
}

*All years.

svy,subpop(female): regress omdiaval c.bmival2 i.ag16g10 
svy,subpop(female): regress omdiaval5 c.bmival2 i.ag16g10 
svy,subpop(female): regress omdiaval5 c.bmival2 i.ag16g10 ib1.topqual5

*Year-specific.
foreach num of numlist 1/5 7/10 12/25 {
generate subpop=0
replace subpop=1 if hseyear==`num' & sex==2
svy,subpop(subpop): regress omdiaval c.bmival2 i.ag16g10
svy,subpop(subpop): regress omdiaval5 c.bmival2 i.ag16g10
svy,subpop(subpop): regress omdiaval5 c.bmival2 i.ag16g10 ib1.topqual5
drop subpop
}

*************************
*Figure 3.
*Quantile regression.
**************************

cumul omsysval10, gen(rank) equal
sum rank
corr rank omsysval10
sum rank if omsysval10 >=140   // Minimum = 0.71 (i.e. 29% to the right).

*x=4 is the 50th.
*x=5 is the 75th.
*4.84 as the 71st percentile (midway between 70th and 72.5th).

local quantiles 5 10 25 50 75 90 95         
local models ""                             
local xlabel ""                            
local j=1                                  

foreach q of numlist `quantiles' {

    qreg omsysval10 c.bmival2 i.sex i.ag16g10 ib1.topqual5 [pw=wt_nurse], quantile(`q') 
    estimates store me_tu`q'
    local models `"`models' me_tu`q' || "'
    local xlabel `"`xlabel' `j++' "Q{sub:`q'}""'
}

di "`models'
di `"`xlabel'"'

coefplot `models', vertical bycoefs  ///
xlab(none) xlabel(`xlabel', add) ytitle("SBP difference, mmHg") name(QregSBP, replace) ///
ylabel(0(.5)2, angle(horiz)) ///
drop(_cons 2.sex 2.ag16g10 3.ag16g10  4.ag16g10 5.ag16g10 6.ag16g10 7.ag16g10 2.topqual5 3.topqual5 4.topqual5) ///
ciopts(recast(rcap))  graphregion(color(white)) yline(0, lcolor(gs8) lpattern(dash)) title("") ///
xline(4.84, lcolor(gs8) lpattern(shortdash))  ///
text(9 5  "",size(medsmall))







