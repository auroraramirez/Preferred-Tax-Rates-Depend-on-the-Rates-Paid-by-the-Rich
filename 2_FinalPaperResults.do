***********************
*** This DO FIle shows the results of the paper
***********************

***
* Perceptions about taxes and redistribution
****

clear all
set rmsg on
set seed 12345
set more off

grstyle init
grstyle set plain, horizontal grid


global drive="C:\Users\auroraramirez\OneDrive - El Colegio de México A.C\Proyectos\Open_Society\Paper1\github"
global raw="$drive\raw"
global data="$drive\data"
global graph="$drive\graphs"
global tablas="$drive\tables"


use "RedistributionFinal.dta", replace

********************************************************************************
********************************************************************************
* Índices
********************************************************************************
********************************************************************************
  * Socioeconomic
  * Higher hh index more wealth or socioec status
pca ses_* [aw=weight]	
predict hh_index
replace hh_index=-hh_index
label var hh_index "Neighborhood quality index"

  * Wealth 
  * Higher index means better socioeconomic conditions 
pca w_* hh_index [aw=weight]	
predict wealth_index_aux
sum wealth_index_aux [aw=weight]	
gen index_wealth=(wealth_index_aux-r(mean))/r(sd)
drop wealth_index_aux
label var index_wealth "Socioeconomic status index"

xtile wealth_rank=index_wealth [aw=weight], nq(20)
label var wealth_rank "Wealth rank"

  * Beliefs
  * Higher index means higher belief each person owns its destiny
egen rowindex=rowtotal(bfs_oportunity bfs_effort bfs_circumstances bfs_work bfs_advantages)
sum rowindex [aw=weight]
gen index_poverty=(rowindex-r(mean))/r(sd)
drop rowindex
label var index_poverty "Poverty Index"

  * Inequality aversion
  * Higher index means more worried about inequality
egen rowindex=rowtotal(bfs_inequality bfs_gov bfs_equitably bfs_access)
sum rowindex [aw=weight]
gen index_ineq=(rowindex-r(mean))/r(sd)
drop rowindex
label var index_ineq "Inequality Index"

  * Preferences for rich taxation
  * Higher index means more support for inheritance/wealth tax
egen rowindex=rowtotal(bfs_inheritance bfs_tax_heritage bfs_tax_inheritance bfs_wealth_transmission)
sum rowindex [aw=weight]
gen index_taxrich=(rowindex-r(mean))/r(sd)
drop rowindex
label var index_taxrich "Taxation Index"

  * Efficiency
  * Higher index means that the economy is more affected
egen rowindex=rowtotal(eff_*)
sum rowindex [aw=weight]
gen index_efficiency=(rowindex-r(mean))/r(sd)
drop rowindex
label var index_efficiency "Efficiency Index"

  * Trust/satisfaction in government
  * Higher index higher belief in government
egen rowindex=rowtotal(trust_*)
sum rowindex [aw=weight]
gen index_trust=(rowindex-r(mean))/r(sd)
drop rowindex
label var index_trust "Trust Index"

	* Conservador/liberal social
	* Higher index means more liberal
egen rowindex=rowtotal(soc_*)
sum rowindex [aw=weight]
gen index_social=(rowindex-r(mean))/r(sd)
drop rowindex
label var index_social "Social Index"

	* Conservador/liberal económico
	* Higher index means more liberal
egen rowindex=rowtotal(econ_*)
sum rowindex [aw=weight]
gen index_econ=(rowindex-r(mean))/r(sd)
drop rowindex
label var index_econ "Economic Index"

gen cd_mxcity=entidad==9 | entidad==15
gen cd_norte=entidad==8 | entidad==19
gen cd_sur=entidad==20|entidad==31
gen cd_centro=entidad==11|entidad==14|entidad==24 

label var cd_mxcity "Mexico City"
label var cd_norte 	"North"
label var cd_sur 	"South"
label var cd_centro "Center"

gen menor40=(edad<40)
label var menor40 "Young Adult"

sum index_wealth [aw=weight], d
gen men_wealth=(index_wealth<`r(p50)')
label var men_wealth "Less wealthier than the median"

sum index_wealth [aw=weight], d
gen more_wealth=(index_wealth>`r(p50)')
label var more_wealth "Wealthier than the median"

sum index_poverty [aw=weight], d
gen men_poverty=(index_poverty<`r(p50)')

sum index_ineq [aw=weight], d
gen men_ineq=(index_ineq<`r(p50)')

sum index_taxrich [aw=weight], d
gen men_taxrich=(index_taxrich<`r(p50)')

sum index_efficiency [aw=weight], d
gen men_effi=(index_efficiency<`r(p50)')

sum index_trust [aw=weight], d
gen men_trust=(index_trust<`r(p50)')

sum index_social [aw=weight], d
gen men_social=(index_social<`r(p50)')

sum index_econ [aw=weight], d
gen men_econ=(index_econ<`r(p50)')

sum diff_perceived [aw=weight], d
gen diff_perceived_below=(diff_perceived<`r(p50)')
label var diff_perceived_below "Perceived difference below the median"

sum diff_perceived [aw=weight], d
gen diff_perceived_above=(diff_perceived>`r(p50)')
label var diff_perceived_above "Perceived difference above the median"


sum taxrate_rich [aw=weight], d
gen taxrate_rich_above=(taxrate_rich>`r(p50)')
label var taxrate_rich_above "Tax for rich above the median"


sum taxrate_super [aw=weight], d
gen taxrate_super_above=(taxrate_rich>`r(p50)')
label var taxrate_super_above "Tax for super rich above the median"

compress

save "$data\Redistribución_temp.dta", replace


************************************************************
****** Table 1 *********************************************
************************************************************
use "$data\Redistribución_temp.dta", replace

foreach var in sec3_p_41_a sec3_p_41_b sec3_p_25_1_1 sec3_p_25_2_1 sec3_p_25_3_1 sec3_p_25_4_1 sec3_p_25_5_1 {
	replace `var'=`var'*10
}
global X_sociodem="female edad married cd_mxcity cd_norte cd_centro trabajo"
global X_perception="index_poverty index_ineq index_trust index_taxrich index_efficiency sec3_p_41_a sec3_p_41_b"
global X_taxation="sec3_p_25_1_1 sec3_p_25_2_1 sec3_p_25_3_1 sec3_p_25_4_1 sec3_p_25_5_1"
putexcel set "$tablas\Tables.xlsx",  sheet("Table0", replace) modify
putexcel B2= "Variable"
putexcel C2= "N"
putexcel D2= "Mean"
putexcel E2= "Std. Dev."
putexcel F2= "Min"
putexcel G2= "Max"

putexcel B3="A. Key control variables"
local i=4
foreach var in  sec6_p_60_baseline sec6_p_60_t1 sec6_p_60_t2 sec6_p_60_t3 sec6_p_60_t4 T0 T1 T2 T3 T4 ///
				female edad married cd_mxcity cd_norte cd_centro cd_sur trabajo ///
				hh_index index_wealth ////
				index_poverty index_ineq index_trust index_taxrich index_efficiency sec3_p_41_a sec3_p_41_b ///
				sec3_p_25_1_1 sec3_p_25_2_1 sec3_p_25_3_1 sec3_p_25_4_1 sec3_p_25_5_1 {
putexcel B`i'="`var'"
quietly sum `var' [aw=weight], det
matrix A=[r(N), r(mean), r(sd), r(min), r(max)]
putexcel C`i'= matrix(A), nformat(number_d2)
local i=`i'+1
}
display " `i' "

putexcel close

*******
**Table 1: Descriptive Statistics
******
**Estadisticas Descriptivas asegurarse balance: variables de pobreza riqueza impuestos --q28-q31 y q38-q39
putexcel set "$tablas\Tables.xlsx", sheet("Table1", replace) modify 
putexcel B2= "Variable"
putexcel C2= "All"
putexcel D2= "Baseline"
putexcel E2= "Treatment 60%"
putexcel F2= "Treatment 50%"
putexcel G2= "Treatment 40%"
putexcel H2= "Treatment 20%"
putexcel I2= "p-value"
quietly sum female 
putexcel C3 = `r(N)'
quietly sum female if group==0
putexcel D3 = `r(N)'
quietly sum female if group==1
putexcel E3 = `r(N)'
quietly sum female if group==2
putexcel F3 = `r(N)'
quietly sum female if group==3
putexcel G3 = `r(N)'
quietly sum female if group==4
putexcel H3 = `r(N)'

local i=4
local j=5
foreach var in female edad married trabajo cd_mxcity cd_norte cd_centro cd_sur ///
				index_wealth ////
				index_poverty index_ineq index_trust index_taxrich index_efficiency sec3_p_41_a sec3_p_41_b ///
				sec3_p_25_1_1 sec3_p_25_2_1 sec3_p_25_3_1 sec3_p_25_4_1 sec3_p_25_5_1 {
putexcel B`i'="`var'"
quietly sum `var'  [aw=weight]
local meanx = string(`r(mean)',"%9.2f")
local sdx = string((`r(sd)'/sqrt(`r(N)')),"%9.2f")
local meansd = "[`sdx']"
putexcel C`i'="`meanx'"
putexcel C`j'="`meansd'"

foreach var2 in T0 T1 T2 T3 T4  {
quietly sum `var'  [aw=weight] if `var2'==1
local meanx = string(`r(mean)',"%9.2f")
local sdx = string((`r(sd)'/sqrt(`r(N)')),"%9.2f")
local meansd = "[`sdx']"
	if "`var2'"=="T0" {
	putexcel D`i'="`meanx'"
	putexcel D`j'="`meansd'"
	}
	else if "`var2'"=="T1" {
	putexcel E`i'="`meanx'"
	putexcel E`j'="`meansd'"
	}
	else if "`var2'"=="T2" {
	putexcel F`i'="`meanx'"
	putexcel F`j'="`meansd'"
	}
	else if "`var2'"=="T3" {
	putexcel G`i'="`meanx'"
	putexcel G`j'="`meansd'"
	}
	else if "`var2'"=="T4" {
	putexcel H`i'="`meanx'"
	putexcel H`j'="`meansd'"
	}	
}
quietly reg `var' T0 T1 T2 T3 T4 [aw=weight], robust nocons
quietly test T0=T1=T2=T3=T4
local t=string(`r(p)', "%9.3f")
putexcel I`i'="[`t']"
local i=`i'+2
local j=`j'+2
}
putexcel close

**********************************************************
****************  Figure 1  ******************************
**********************************************************
use "$data\Redistribución_temp.dta", clear
gen xn=1

collapse (mean) sec6_p_60 (sd) sd_p60=sec6_p_60 (rawsum) weight (count) xn [aw=weight], by(group)

gen orig=group
replace group=0 if orig==1
replace group=1 if orig==2
replace group=2 if orig==3
replace group=3 if orig==0
replace group=4 if orig==4

sort group

replace group=1+group
replace group = _n + 1 if _n >= 2
replace group = _n + 2 if _n >= 3
replace group = _n + 3 if _n >= 4
replace group = _n + 4 if _n >= 5
replace group =-1*(group-9)+1

label var group "Grupo"
label def group 0 " " 1 "20%" 2 " " 3 "30%" 4 " " 5 "40%" ///
				6 " " 7  "50%" 8 " " 9 "60%" 10 " "
				
label val group group

gen icp=sec6_p_60+1.96*sd_p60/sqrt(xn)
gen icn=sec6_p_60-1.96*sd_p60/sqrt(xn)


twoway (rcap icp icn group, sort msize(medium) color(black)) ///
(scatter sec6_p_60 group, sort msize(medium) mcolor(black) msymbol(square)), ////
	ylabel(0(2.5)20, grid glwidth(medthin) glpattern(dash) labsize(med) ) ////
	xlabel(0(1)10, noticks labels labsize(med) valuelabel grid glwidth(medthin) glpattern(dash) glcolor(white)) ////
	ytitle("Preferred tax rate", size(med)) xtitle("Hypothetical Tax Rate for Rich") legend(off) ////
	graphregion(fcolor(white))
graph export "$graph\Fig1_Treat_groups.emf", replace font("Times New Roman")
graph export "$graph\Fig1_Treat_groups.png", replace wid(8000)



**********************************************************
****************  Table 2  ******************************
**********************************************************
use "$data\Redistribución_temp.dta", clear

foreach var in sec3_p_41_a sec3_p_41_b sec3_p_25_1_1 sec3_p_25_2_1 sec3_p_25_3_1 sec3_p_25_4_1 sec3_p_25_5_1 {
	replace `var'=`var'*10
}
global X_sociodem="female edad married cd_mxcity cd_norte cd_centro trabajo"
global X_perception="index_poverty index_ineq index_trust index_taxrich index_efficiency sec3_p_41_a sec3_p_41_b"
global X_taxation="sec3_p_25_1_1 sec3_p_25_2_1 sec3_p_25_3_1 sec3_p_25_4_1 sec3_p_25_5_1"

sum sec6_p_60 $X_sociodem index_wealth $X_perception $X_taxation [aw=weight]
	
quietly sum sec6_p_60 [aw=weight] if T0==1
local rm=r(mean)

reg sec6_p_60 T1 T2 T3 T4 [aw=weight], rob
	test T1=T4
	local diff=_b[T1]-_b[T4]
	local p=`r(p)'
	sleep 10
		test T1=T3
	local diff2=_b[T1]-_b[T3]
	local pa2=`r(p)'
	sleep 10
		test T1=T2
	local diff3=_b[T1]-_b[T2]
	local pb3=`r(p)'
	sleep 10
		test T2=T3=T4
	local pc4=`r(p)'
	outreg2 T1 T2 T3 T4 using "$tablas\Reg1.xls", replace excel bracket dec(3) keep(T1 T2 T3 T4) ///
	ctitle(sec6_p_60) adjr2 stats(coef se ci) addtext(Basic Controls, NO, SES, NO, Perceptions, NO, Taxation, NO) ///
	adds(Mean Control, `rm', "Difference T1-T4", `diff', p-value1, `p', "Difference T1-T3", `diff2', p-value2, `pa2', "Difference T1-T2", `diff3', p-value3, `pb3', p-value-ALL, `pc4') label
	
reg sec6_p_60 T1 T2 T3 T4 $X_sociodem [aw=weight], robust
		test T1=T4
	local diff=_b[T1]-_b[T4]
	local p=`r(p)'
	sleep 10
	test T1=T3
	local diff2=_b[T1]-_b[T3]
	local pa2=`r(p)'
	sleep 10
		test T1=T2
	local diff3=_b[T1]-_b[T2]
	local pb3=`r(p)'
	sleep 10
	test T2=T3=T4
	local pc4=`r(p)'
	outreg2 T1 T2 T3 T4 using "$tablas\Reg1.xls", append excel bracket dec(3) keep(T1 T2 T3 T4) ///
	ctitle(sec6_p_60) adjr2 stats(coef se ci) addtext(Basic Controls, YES, SES, NO, Perceptions, NO, Taxation, NO) ///
	adds(Mean Control, `rm', "Difference T1-T4", `diff', p-value1, `p', "Difference T1-T3", `diff2', p-value2, `pa2', "Difference T1-T2", `diff3', p-value3, `pb3', p-value-ALL, `pc4') label

reg sec6_p_60 T1 T2 T3 T4 $X_sociodem index_wealth [aw=weight], robust
		test T1=T4
	local diff=_b[T1]-_b[T4]
	local p=`r(p)'
	sleep 10
	test T1=T3
	local diff2=_b[T1]-_b[T3]
	local pa2=`r(p)'
	sleep 10
		test T1=T2
	local diff3=_b[T1]-_b[T2]
	local pb3=`r(p)'
	sleep 10
	test T2=T3=T4
	local pc4=`r(p)'
	outreg2 T1 T2 T3 T4 using "$tablas\Reg1.xls", append excel bracket dec(3) keep(T1 T2 T3 T4) ///
	ctitle(sec6_p_60) adjr2 stats(coef se ci) addtext(Basic Controls, YES, SES, YES, Perceptions, NO, Taxation, NO) ///
	adds(Mean Control, `rm', "Difference T1-T4", `diff', p-value1, `p', "Difference T1-T3", `diff2', p-value2, `pa2', "Difference T1-T2", `diff3', p-value3, `pb3', p-value-ALL, `pc4') label

	
reg sec6_p_60 T1 T2 T3 T4 $X_sociodem index_wealth $X_perception [aw=weight], robust
		test T1=T4
	local diff=_b[T1]-_b[T4]
	local p=`r(p)'
	sleep 10
	test T1=T3
	local diff2=_b[T1]-_b[T3]
	local pa2=`r(p)'
	sleep 10
		test T1=T2
	local diff3=_b[T1]-_b[T2]
	local pb3=`r(p)'
	sleep 10
	test T2=T3=T4
	local pc4=`r(p)'
	outreg2 T1 T2 T3 T4 using "$tablas\Reg1.xls", append excel bracket dec(3) keep(T1 T2 T3 T4) ///
	ctitle(sec6_p_60) adjr2 stats(coef se ci) addtext(Basic Controls, YES, SES, YES, Perceptions, YES, Taxation, NO) ///
	adds(Mean Control, `rm', "Difference T1-T4", `diff', p-value1, `p', "Difference T1-T3", `diff2', p-value2, `pa2', "Difference T1-T2", `diff3', p-value3, `pb3', p-value-ALL, `pc4') label


reg sec6_p_60 T1 T2 T3 T4 $X_sociodem index_wealth $X_perception $X_taxation [aw=weight], robust
		test T1=T4
	local diff=_b[T1]-_b[T4]
	local p=`r(p)'
	sleep 10
	test T1=T3
	local diff2=_b[T1]-_b[T3]
	local pa2=`r(p)'
	sleep 10
		test T1=T2
	local diff3=_b[T1]-_b[T2]
	local pb3=`r(p)'
	sleep 10
	test T2=T3=T4
	local pc4=`r(p)'
	outreg2 T1 T2 T3 T4 using "$tablas\Reg1.xls", append excel bracket dec(3) keep(T1 T2 T3 T4) ///
	ctitle(sec6_p_60) adjr2 stats(coef se ci) addtext(Basic Controls, YES, SES, YES, Perceptions, YES, Taxation, YES) ///
	adds(Mean Control, `rm', "Difference T1-T4", `diff', p-value1, `p', "Difference T1-T3", `diff2', p-value2, `pa2', "Difference T1-T2", `diff3', p-value3, `pb3', p-value-ALL, `pc4') label

**********************************************************
****************  Figure 2  ******************************
**********************************************************



foreach var in index_wealth index_trust index_efficiency {
clear all
gen etiqueta=""
save "$data\\`var'_fig.dta", replace
foreach index in `var'_amed `var'_bmed  {
 
use "$data\Redistribución_temp.dta", clear

gen orig=group
replace group=0 if orig==1
replace group=1 if orig==2
replace group=2 if orig==3
replace group=3 if orig==0
replace group=4 if orig==4

sum `var' [aw=weight], d
gen `var'_bmed=(`var'<`r(p50)')
sum `var' [aw=weight], d
gen `var'_amed=(`var'>=`r(p50)')
	
local varname sec6_p_60
local group1 group
local group2 `index'

collapse (mean) y = `varname' (semean) se_y = `varname' (rawsum) weight [aw=weight], by(`group1' `group2' )

append using "$data\\`var'_fig.dta"
save "$data\\`var'_fig.dta", replace

}
}


foreach var in index_wealth index_trust index_efficiency {
use "$data\\`var'_fig.dta", clear
replace etiqueta="`var'<median" if `var'_bmed!=.
replace etiqueta="`var'>=median" if `var'_amed!=.

foreach index in `var'_amed `var'_bmed {
drop if `index'==0
}

sort group etiqueta

gen x = _n
replace x=4 if _n==9
replace x=6 if _n==10

replace x=9 if _n==7
replace x=11 if _n==8

replace x=14 if _n==5
replace x=16 if _n==6

replace x=19 if _n==3
replace x=21 if _n==4

replace x=24 if _n==1
replace x=26 if _n==2

label  define x ///
	5 "20%" 10 "30%" 15 "40%" 20 "50%"  ///
	25 "60%" 
	label  value x x
	
	
gen yu = y + 1.96*se_y
gen yl = y - 1.96*se_y

separate y, by(etiqueta)
separate yu, by(etiqueta)
separate yl, by(etiqueta)
save "$data\\`var'_fig.dta", replace
}

use "$data\index_wealth_fig.dta", clear	  
twoway (scatter y1 x ,  msize(medium) msymbol(circle) mcolor(black)) ///
		(rcap yu1 yl1 x, lcolor(black))  ///       
		(scatter y2 x , msize(medium) msymbol(diamond) mcolor(gs15) mlcolor(black) ) ///
		(rcap yu2 yl2 x, lcolor(black)), ///
		xlabel(5(5)26, noticks labels labsize(med) valuelabel grid glwidth(medthin) glpattern(dash) glcolor(white)) ////
		ylabel(0(2.5)20, grid glwidth(medthin) glpattern(dash) labsize(med) ) ////
	    ytitle("Preferred tax rate", size(med)) xtitle("Hypothetical Tax Rate for Rich") ///
	    legend(order( ///
		1 "SES < median" ///
	    3 "SES ≥ median") ///
		position(4) ring(0) region(lcolor(white)))
graph export "$graph\Fig2A_SES.emf", replace font("Times New Roman")
graph export "$graph\Fig2A_SES.png", replace wid(8000)

use "$data\index_trust_fig.dta", clear	  
twoway (scatter y1 x , msize(medium) msymbol(circle) mcolor(black)) ///
		(rcap yu1 yl1 x, lcolor(black))  ///       
		(scatter y2 x , msize(medium) msymbol(diamond) mcolor(gs15) mlcolor(black)) ///
		(rcap yu2 yl2 x, lcolor(black)), ///
		xlabel(5(5)26, noticks labels labsize(med) valuelabel grid glwidth(medthin) glpattern(dash) glcolor(white)) ////
		ylabel(0(2.5)20, grid glwidth(medthin) glpattern(dash) labsize(med) ) ////
	    ytitle("Preferred tax rate", size(med)) xtitle("Hypothetical Tax Rate for Rich") ///
	    legend(order( ///
		1 "Trust < median" ///
	    3 "Trust ≥ median") ///
		position(4) ring(0) region(lcolor(white)))
graph export "$graph\Fig2B_Trust.emf", replace font("Times New Roman")
graph export "$graph\Fig2B_Trust.png", replace wid(8000)


use "$data\index_efficiency_fig.dta", clear	  
twoway (scatter y1 x , msize(medium) msymbol(circle)mcolor(black)) ///
		(rcap yu1 yl1 x, lcolor(black))  ///       
		(scatter y2 x , msize(medium) msymbol(diamond) mcolor(gs15) mlcolor(black)) ///
		(rcap yu2 yl2 x, lcolor(black)), ///
		xlabel(5(5)26, noticks labels labsize(med) valuelabel grid glwidth(medthin) glpattern(dash) glcolor(white)) ////
		ylabel(0(2.5)20, grid glwidth(medthin) glpattern(dash) labsize(med) ) ////
	    ytitle("Preferred tax rate", size(med)) xtitle("Hypothetical Tax Rate for Rich") ///
	    legend(order( ///
		1 "Perception < median" ///
	    3 "Perception ≥ median") ///
		position(4) ring(0) region(lcolor(white)))
graph export "$graph\Fig2C_Efficiency.emf", replace font("Times New Roman")
graph export "$graph\Fig2C_Efficiency.png", replace wid(8000)
   

***********************************************************
***************Table 3  *************************
***********************************************************
use "$data\Redistribución_temp.dta", clear

foreach var in sec3_p_41_a sec3_p_41_b sec3_p_25_1_1 sec3_p_25_2_1 sec3_p_25_3_1 sec3_p_25_4_1 sec3_p_25_5_1 {
	replace `var'=`var'*10
}
global X_sociodem="female edad married cd_mxcity cd_norte cd_centro trabajo"
global X_perception="index_poverty index_ineq index_trust index_taxrich index_efficiency sec3_p_41_a sec3_p_41_b"
global X_taxation="sec3_p_25_1_1 sec3_p_25_2_1 sec3_p_25_3_1 sec3_p_25_4_1 sec3_p_25_5_1"

sum sec6_p_60 $X_sociodem index_wealth $X_perception $X_taxation [aw=weight]
	
foreach var in 	index_wealth index_trust index_efficiency {
	quietly sum `var' [aw=weight], det
	gen med_`var'=`var'>=`r(p50)'


forval j=0/1 {
quietly sum sec6_p_60 [aw=weight] if T0==1 & med_`var'==`j'
local rm=r(mean)

reg sec6_p_60 T1 T2 T3 T4 $X_sociodem index_wealth $X_perception $X_taxation  [aw=weight] if med_`var'==`j', rob
	test T1=T4
	local diff=_b[T1]-_b[T4]
	local p=`r(p)'
	sleep 10
		test T1=T3
	local diff2=_b[T1]-_b[T3]
	local pa2=`r(p)'
	sleep 10
		test T1=T2
	local diff3=_b[T1]-_b[T2]
	local pb3=`r(p)'
	sleep 10
		test T2=T3=T4
	local pc4=`r(p)'
	if `j'==0 & "`var'"=="index_wealth" {
	outreg2 T1 T2 T3 T4 using "$tablas\Reg2.xls", replace excel bracket dec(3) keep(T1 T2 T3 T4) ///
	ctitle(`var') adjr2 stats(coef se ci) addtext(Median, `j', Basic Controls, YES, SES, YES, Perceptions, YES, Taxation, YES) ///
	adds(Mean Control, `rm', "Difference T1-T4", `diff', p-value1, `p', "Difference T1-T3", `diff2', p-value2, `pa2', "Difference T1-T2", `diff3', p-value3, `pb3', p-value-ALL, `pc4') label
	}
	else {
	outreg2 T1 T2 T3 T4 using "$tablas\Reg2.xls", append excel bracket dec(3) keep(T1 T2 T3 T4) ///
	ctitle(`var') adjr2 stats(coef se ci) addtext(Median, `j', Basic Controls, YES, SES, YES, Perceptions, YES, Taxation, YES) ///
	adds(Mean Control, `rm', "Difference T1-T4", `diff', p-value1, `p', "Difference T1-T3", `diff2', p-value2, `pa2', "Difference T1-T2", `diff3', p-value3, `pb3', p-value-ALL, `pc4') label	
	}
}
}


***********************************************************
***************Figure A2  *************************
***********************************************************

foreach var in index_poverty index_ineq index_taxrich sec3_p_41_b sec3_p_25_1_1 sec3_p_25_2_1 sec3_p_25_3_1 sec3_p_25_4_1 sec3_p_25_5_1 {
clear all
gen etiqueta=""
save "$data\\`var'_fig.dta", replace
foreach index in `var'_amed `var'_bmed  {
 
use "$data\Redistribución_temp.dta", clear

gen orig=group
replace group=0 if orig==1
replace group=1 if orig==2
replace group=2 if orig==3
replace group=3 if orig==0
replace group=4 if orig==4

sum `var' [aw=weight], d
gen `var'_bmed=(`var'<`r(p50)')
sum `var' [aw=weight], d
gen `var'_amed=(`var'>=`r(p50)')
	
local varname sec6_p_60
local group1 group
local group2 `index'

collapse (mean) y = `varname' (semean) se_y = `varname' (rawsum) weight [aw=weight], by(`group1' `group2' )

append using "$data\\`var'_fig.dta"
save "$data\\`var'_fig.dta", replace

}
}

*sec3_p_41_b sec3_p_25_1_1 sec3_p_25_2_1 sec3_p_25_3_1 sec3_p_25_4_1 sec3_p_25_5_1/ corruption perceivedtax_own perceivedtax_poor perceivedtax_middle perceivedtax_rich perceivedtax_srich

foreach var in index_poverty index_ineq index_taxrich sec3_p_41_b sec3_p_25_1_1 sec3_p_25_2_1 sec3_p_25_3_1 sec3_p_25_4_1 sec3_p_25_5_1 {
use "$data\\`var'_fig.dta", clear
replace etiqueta="`var'<median" if `var'_bmed!=.
replace etiqueta="`var'>=median" if `var'_amed!=.

foreach index in `var'_amed `var'_bmed {
drop if `index'==0
}

sort group etiqueta

gen x = _n
replace x=4 if _n==9
replace x=6 if _n==10

replace x=9 if _n==7
replace x=11 if _n==8

replace x=14 if _n==5
replace x=16 if _n==6

replace x=19 if _n==3
replace x=21 if _n==4

replace x=24 if _n==1
replace x=26 if _n==2

label  define x ///
	5 "20%" 10 "30%" 15 "40%" 20 "50%"  ///
	25 "60%" 
	label  value x x
	
	
gen yu = y + 1.96*se_y
gen yl = y - 1.96*se_y

separate y, by(etiqueta)
separate yu, by(etiqueta)
separate yl, by(etiqueta)
save "$data\\`var'_fig.dta", replace
twoway (scatter y1 x ,  msize(medium) msymbol(circle) mcolor(black)) ///
		(rcap yu1 yl1 x, lcolor(black))  ///       
		(scatter y2 x , msize(medium) msymbol(diamond) mcolor(gs15) mlcolor(black) ) ///
		(rcap yu2 yl2 x, lcolor(black)), ///
		xlabel(5(5)26, noticks labels labsize(med) valuelabel grid glwidth(medthin) glpattern(dash) glcolor(white)) ////
		ylabel(0(2.5)20, grid glwidth(medthin) glpattern(dash) labsize(med) ) ////
	    ytitle("Preferred tax rate", size(med)) xtitle("Hypothetical Tax Rate for Rich") ///
	    legend(order( ///
		1 " < median" ///
	    3 " ≥ median") ///
		position(4) ring(0) region(lcolor(white)))
graph export "$graph\Fig2MS_`var'.emf", replace font("Times New Roman")
graph export "$graph\Fig2MS_`var'.png", replace wid(8000)

}




