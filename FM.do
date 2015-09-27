
**** step one ****

use "/Users/abusung/Dropbox/Abu's Dropbox/report 6/ref 6.dta"

*** estimat beta ***

gen t=_n
keep if t<=120

forvalues x = 1/456  {
reg a`x'  market
gen b`x'=_b[market]
}

** transpose **

drop  market 
keep if t==1 

forvalues x = 1/456  {
drop a`x' 
}
drop t

xpose, clear varname format

gen t=_n

gen stock="a"

gen t2=string(t)

gen new= stock+t2

drop t stock t2

egen rank =rank(v1)

gen prof=1 if rank<=46

replace prof=2 if rank>46 & rank<=46*2 

replace prof=3 if rank>46*2 & rank<=46*3 

replace prof=4 if rank>46*3 & rank<=46*4  

replace prof=5 if rank>46*4 & rank<=46*5 

replace prof=6 if rank>46*5 & rank<=46*6 

replace prof=7 if rank>=277 & rank<=321

replace prof=8 if rank>=322 & rank<=366

replace prof=9 if rank>=367 & rank<=411

replace prof=10 if rank>=412 & rank<=456

rename v1 beta


save "/Users/abusung/Dropbox/Abu's Dropbox/report 6/beta.dta", replace

*** step two ****

use "/Users/abusung/Dropbox/Abu's Dropbox/report 6/ref 6.dta", clear

gen t=_n
keep if t>120 & t<=240

drop t market
xpose, clear varname format

rename _varname new

joinby(new) using "/Users/abusung/Dropbox/Abu's Dropbox/report 6/beta.dta" 

save "/Users/abusung/Dropbox/Abu's Dropbox/report 6/step2.dta", replace



** average return of profolio ** 
forvalues x = 1/120  {
use "/Users/abusung/Dropbox/Abu's Dropbox/report 6/step2.dta", clear
collapse(mean) v`x' , by(prof)
xpose, clear varname format
save "/Users/abusung/Dropbox/Abu's Dropbox/report 6/`x'.dta", replace

}

clear
use "/Users/abusung/Dropbox/Abu's Dropbox/report 6/1.dta", clear
 
forvalues x = 2/120  {
 
append using "/Users/abusung/Dropbox/Abu's Dropbox/report 6/`x'.dta"
}

drop if _varname=="prof"
gen t=_n
save "/Users/abusung/Dropbox/Abu's Dropbox/report 6/average.dta", replace

use "/Users/abusung/Dropbox/Abu's Dropbox/report 6/ref 6.dta", clear
gen t=_n
keep if t>120 & t<=240
keep t market
drop t
gen t=_n

joinby(t) using "/Users/abusung/Dropbox/Abu's Dropbox/report 6/average.dta"

***Step 2: CAPM for porfolio**

forvalues x = 1/10  {
reg v`x'  market
gen b`x'=_b[market]
}

drop _varname


keep if t==1 

forvalues x = 1/10  {

drop v`x' 

}
drop  market t


xpose, clear varname format

gen t=_n

gen stock="v"
gen t2=string(t)
gen new= stock+t2

drop t stock t2


rename v1 beta

save "/Users/abusung/Dropbox/Abu's Dropbox/report 6/step2_beta.dta", replace

***** step three ******

use "/Users/abusung/Dropbox/Abu's Dropbox/report 6/ref 6.dta", clear

gen t=_n
keep if t>240 & t<=360

drop t market
xpose, clear varname format

rename _varname new


joinby(new) using "/Users/abusung/Dropbox/Abu's Dropbox/report 6/beta.dta"

save "/Users/abusung/Dropbox/Abu's Dropbox/report 6/step3.dta", replace

*** average return of profolio for third period ****
 
forvalues x = 1/120  {
use "/Users/abusung/Dropbox/Abu's Dropbox/report 6/step3.dta", clear
collapse(mean) v`x' , by(prof)
xpose, clear varname format
save "/Users/abusung/Dropbox/Abu's Dropbox/report 6/s`x'.dta", replace

}

clear
use "/Users/abusung/Dropbox/Abu's Dropbox/report 6/s1.dta", clear
 
forvalues x = 2/120  {
 
append using "/Users/abusung/Dropbox/Abu's Dropbox/report 6/s`x'.dta"
}

drop if _varname=="prof"

drop _varname


xpose, clear varname format

save "/Users/abusung/Dropbox/Abu's Dropbox/report 6/step3_mean.dta", replace

rename _varname new

joinby(new) using "/Users/abusung/Dropbox/Abu's Dropbox/report 6/step2_beta.dta"

save "/Users/abusung/Dropbox/Abu's Dropbox/report 6/step3_beta.dta", replace


forvalues x = 1/120  {
reg v`x'  beta
gen b`x'=_b[beta]
}

drop _varname new beta 
gen t=_n
keep if t==1 

forvalues x = 1/120  {

drop v`x' 

}
drop t


xpose, clear varname format

ttest v1==0

save "/Users/abusung/Dropbox/Abu's Dropbox/report 6/test.dta", replace
