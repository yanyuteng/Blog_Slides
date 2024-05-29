* ssc install ua, replace
cd "~/Desktop/DATA_CN/DATA_Survey/S3_JSNET/JSNET2009"
* ua: unicode encoding set gb18030
* ua: unicode translate *, invalid(mark)
use "2009JSNET(stata).dta", clear

******************************* 1. Dependence var *********************************

**************** network **************** 
** extensity of network
recode e0401 (-9/-7=.), gen(relatives)
recode e0402 (-9/-7=.), gen(friends)
recode e0403 (-9/-7=.), gen(acquaintance)
gen net_size = relatives + friends + acquaintance

** upper reachability of network
gen net_top = 0
replace net_top = 1 if e0618==1
replace net_top = 2 if e0614==1
replace net_top = 3 if e0620==1
replace net_top = 4 if e0604==1
replace net_top = 5 if e0616==1
replace net_top = 6 if e0611==1
replace net_top = 7 if e0607==1
replace net_top = 8 if e0619==1
replace net_top = 9 if e0613==1
replace net_top = 10 if e0615==1
replace net_top = 11 if e0609==1
replace net_top = 12 if e0610==1
replace net_top = 13 if e0606==1
replace net_top = 14 if e0612==1
replace net_top = 15 if e0602==1
replace net_top = 16 if e0603==1
replace net_top = 17 if e0605==1
replace net_top = 18 if e0617==1
replace net_top = 19 if e0608==1
replace net_top = 20 if e0601==1

** heterogeneity of network
gen net_heter = 0
forvalues i=1/9{
	replace net_heter = net_heter + e060`i'
	}
forvalues i=10/20{
	replace net_heter = net_heter + e06`i'
	}

** link with leader
gen net_leader= 0
replace net_leader= 1 if e0602==1 | e0606==1

** link with manager
gen net_manager = 0
replace net_manager = 1 if e0610==1

** link with intellectual
gen net_prof = 0
replace net_prof = 1 if e0601==1 | e0603==1 | e0605==1 | e0607==1 |e0608==1 | e0612==1 | e0617==1
*** 胡荣,阳杨.社会转型与网络资源[J].厦门大学学报(哲学社会科学版),2011(06):111-118.

** social network_factor analysis
factor net_size net_top net_heter net_leader net_manager net_prof, pcf
rotate
predict net_captial


**************** income **************** 

** income b0110 b0808 d0501 d0502
*** recode b0808(-9/0=.),gen(income)
*** replace income = income/10000
recode d0501(-9/-7=.),gen(income1)
recode d0502(-9/-7=.),gen(income2)
gen income = (income1+income2)/10000
recode income(0=.)
drop income1 income2
gen lincome = ln(income)

** familiy income
gen fincome = d1101
replace fincome = fincome/10000 if fincome>=2000
recode fincome (-9/0=.)

** self-assessment of family income
recode a0801(9=.), gen(sfincome)

** self-assessment of family social status
recode a0802(9=.), gen(sfstatu)



********************************* 2. Independence var *****************************

**************** class **************** 
gen employer = d0101
gen selfemployee = d0102
gen employee = d0103
*** b0105 b0803

** ever change job, the last job
recode b0803 (-9/-7=.), gen(employee2)
replace employee2=. if employee!=1|employer==1|selfemployee==1
gen class= 0
replace class=1 if employee2==21|employee2==22|employee2==24|employee2==25|employee2==31|employee2==32
replace class=2 if employee2==27|employee2==28|employee2==41|employee2==51|employee2==61|employee2==81
replace class=3 if employee2==11|employee2==12|employee2==71
replace class=4 if employee2==23|employee2==26|employee2==29|employee2==33
replace class=5 if employee2==13|employee2==42|employee2==52|employee2==62|employee2==72|employee2==82
replace class=6 if employee!=1&employer==1&selfemployee!=1
replace class=7 if employee!=1&employer!=1&selfemployee==1
replace class=8 if employee2==43|employee2==53|employee2==62|employee2==73|employee2==83


** never change job, the first(last) job
recode b0803 (-9/-7=.)(0/100=1), gen(employee3)
recode b0604 (-9/-7=.)(0/100=1), gen(employee33)
recode b0105 (-9/-7=.),gen(employee4)
replace employee4=. if employee3==1 | employee33==1 | employee!=1| employer==1| selfemployee==1
gen class2=0
replace class2=1 if employee4==21|employee4==22|employee4==24|employee4==25|employee4==31|employee4==32
replace class2=2 if employee4==27|employee4==28|employee4==41|employee4==51|employee4==61|employee4==81
replace class2=3 if employee4==11|employee4==12|employee4==71
replace class2=4 if employee4==23|employee4==26|employee4==29|employee4==33
replace class2=5 if employee4==13|employee4==42|employee4==52|employee4==62|employee4==72|employee4==82
replace class2=8 if employee4==43|employee4==53|employee4==62|employee4==73|employee4==83


replace class = class + class2
drop class2 employee2 employee3 employee33 employee4
recode class(0=.)(8=0)


******** interaction in vocational activities ********
** departmental relevance degree
gen degree_depart = b0924+b0925+b0926+b0927+b0928+b0929

** market relevance degree
gen degree_market = b0921+b0922+b0923


**************** control var **************** 

** city_missing

** hukou
recode a0101 (1 2=1)(3 4=0), gen(hukou)

** gender
recode a0301 (2=0), gen(gender)
label define genderl 1"male" 0"female"
label value gender genderl

** age
gen age = 2009 - a0302
gen age2 = age * age

** edu
recode a0305 (11=.)(1=3)(2=6)(3=9)(4=12)(5/7=11)(8=14)(9=16)(10=19), gen(edu)

** party
gen party = a0306
label define partyl 1"party" 0"crowd"
label value party partyl

** economic sector b0106 b0804
*** 1 job
recode b0106 (-9/-7=.)(9=.)(1/4=1)(5/8=2),gen(ecosector1)
*** 2 job
recode b0804 (-9/-7=.)(9=.)(1/4=1)(5/8=2),gen(ecosector2)
*** if job2!=na, job1=0
replace ecosector1=0 if ecosector2==1|ecosector2==2
recode ecosector2(.=0)
*** gen ecosector
gen ecosector = ecosector1 + ecosector2
recode ecosector(0=.)
recode ecosector(2=0)
*** label ecosector
label define ecosectorl 1"gov" 0"market"
label value ecosector ecosectorl

** administrative department in charge b0107 b0805
recode b0107 (-9/-7=.)(10=.)(3 4 =1)(7=1)(1 2=2)(5 6=3)(8 9=3)(20=3),gen(department1)
recode b0805 (-9/-7=.)(10=.)(3 4 =1)(7=1)(1 2=2)(5 6=3)(8 9=3)(20=3),gen(department2)
replace department1=0 if department2==1|department2==2|department2==3
recode department2(.=0)
gen department= department1 + department2
recode department(0=.)
label define departmentl 2"province" 1"county" 3"grass-roots"
label value department departmentl

** danwei size b0109 b0807
recode b0109 (-9/-7=.),gen(danweisize1)
recode b0807 (-9/-7=.),gen(danweisize2)
replace danweisize1=0 if danweisize2!=.
recode danweisize2(.=0)
gen danweisize = danweisize1 + danweisize2
recode danweisize(0=.)



********************************* 3. Analysis *************************************

*****************  omit model1 missing value **************** 
foreach i in net_size net_top net_heter net_leader net_manager net_prof net_captial class degree_depart degree_market hukou{
  drop if `i' == .
}


***************** model 1 **************** 

*** sktest net_captial
*** ladder net_size
gen lnet_size=ln(net_size)
*** sktest lnet_size
*** ladder net_heter
gen lnet_heter = sqrt(net_heter)
*** sktest lnet_heter

** model 1-1
xi: reg net_captial i.class degree_depart degree_market i.hukou
*** estat hettest, iid
*** estat hettest, rhs iid
*** estat vif
outreg2 using model1.doc, replace

** model 1-2
xi: reg lnet_size i.class degree_depart degree_market i.hukou
*** estat hettest, iid
*** estat hettest, rhs iid
*** estat vif
outreg2 using model1.doc

** model 1-3
*** net_top can not trans to normal distribution
xi: reg net_top i.class degree_depart degree_market i.hukou
outreg2 using model1.doc

** model 1-4
xi: reg lnet_heter i.class degree_depart degree_market i.hukou
outreg2 using model1.doc

** model 1-5
xi: logit net_leader i.class degree_depart degree_market i.hukou
outreg2 using model1.doc

** model 1-6
xi: logit net_manager i.class degree_depart degree_market i.hukou
outreg2 using model1.doc

** model 1-7
xi: logit net_prof i.class degree_depart degree_market i.hukou
outreg2 using model1.doc


*****************  omit model2 missing value ****************
foreach i in income fincome sfincome sfstatu gender age age2 edu party ecosector department danweisize{
  drop if `i' == .
}


**************** model 2-1 **************** 

** model 2-1
xi: reg lincome net_captial i.class degree_depart degree_market i.hukou i.gender age age2 edu i.party i.ecosector i.department danweisize
outreg2 using model21.doc, replace

** model 2-2
xi: reg lincome net_size i.class degree_depart degree_market i.hukou i.gender age age2 edu i.party i.ecosector i.department danweisize
outreg2 using model21.doc

** model 2-3
xi: reg lincome net_top i.class degree_depart degree_market i.hukou i.gender age age2 edu i.party i.ecosector i.department danweisize
outreg2 using model21.doc

** model 2-4
xi: reg lincome net_heter i.class degree_depart degree_market i.hukou i.gender age age2 edu i.party i.ecosector i.department danweisize
outreg2 using model21.doc

** model 2-5
xi: reg lincome i.net_leader i.class degree_depart degree_market i.hukou i.gender age age2 edu i.party i.ecosector i.department danweisize
outreg2 using model21.doc

** model 2-6
xi: reg lincome i.net_manager i.class degree_depart degree_market i.hukou i.gender age age2 edu i.party i.ecosector i.department danweisize
outreg2 using model21.doc

** model 2-7
xi: reg lincome i.net_prof i.class degree_depart degree_market i.hukou i.gender age age2 edu i.party i.ecosector i.department danweisize
outreg2 using model21.doc


**************** model 2-2 **************** 

** model 2-1
xi: reg fincome net_captial i.class degree_depart degree_market i.hukou i.gender age age2 edu i.party i.ecosector i.department danweisize
outreg2 using model22.doc, replace

** model 2-2
xi: reg fincome net_size i.class degree_depart degree_market i.hukou i.gender age age2 edu i.party i.ecosector i.department danweisize
outreg2 using model22.doc

** model 2-3
xi: reg fincome net_top i.class degree_depart degree_market i.hukou i.gender age age2 edu i.party i.ecosector i.department danweisize
outreg2 using model22.doc

** model 2-4
xi: reg fincome net_heter i.class degree_depart degree_market i.hukou i.gender age age2 edu i.party i.ecosector i.department danweisize
outreg2 using model22.doc

** model 2-5
xi: reg fincome i.net_leader i.class degree_depart degree_market i.hukou i.gender age age2 edu i.party i.ecosector i.department danweisize
outreg2 using model22.doc

** model 2-6
xi: reg fincome i.net_manager i.class degree_depart degree_market i.hukou i.gender age age2 edu i.party i.ecosector i.department danweisize
outreg2 using model22.doc

** model 2-7
xi: reg fincome i.net_prof i.class degree_depart degree_market i.hukou i.gender age age2 edu i.party i.ecosector i.department danweisize
outreg2 using model22.doc


**************** model 2-3 **************** 

** model 2-1
xi: reg sfincome net_captial i.class degree_depart degree_market i.hukou i.gender age age2 edu i.party i.ecosector i.department danweisize
outreg2 using model23.doc, replace

** model 2-2
xi: reg sfincome net_size i.class degree_depart degree_market i.hukou i.gender age age2 edu i.party i.ecosector i.department danweisize
outreg2 using model23.doc

** model 2-3
xi: reg sfincome net_top i.class degree_depart degree_market i.hukou i.gender age age2 edu i.party i.ecosector i.department danweisize
outreg2 using model23.doc

** model 2-4
xi: reg sfincome net_heter i.class degree_depart degree_market i.hukou i.gender age age2 edu i.party i.ecosector i.department danweisize
outreg2 using model23.doc

** model 2-5
xi: reg sfincome i.net_leader i.class degree_depart degree_market i.hukou i.gender age age2 edu i.party i.ecosector i.department danweisize
outreg2 using model23.doc

** model 2-6
xi: reg sfincome i.net_manager i.class degree_depart degree_market i.hukou i.gender age age2 edu i.party i.ecosector i.department danweisize
outreg2 using model23.doc

** model 2-7
xi: reg sfincome i.net_prof i.class degree_depart degree_market i.hukou i.gender age age2 edu i.party i.ecosector i.department danweisize
outreg2 using model23.doc


**************** model 2-4 **************** 

** model 2-1
xi: reg sfstatu net_captial i.class degree_depart degree_market i.hukou i.gender age age2 edu i.party i.ecosector i.department danweisize
outreg2 using model24.doc, replace

** model 2-2
xi: reg sfstatu net_size i.class degree_depart degree_market i.hukou i.gender age age2 edu i.party i.ecosector i.department danweisize
outreg2 using model24.doc

** model 2-3
xi: reg sfstatu net_top i.class degree_depart degree_market i.hukou i.gender age age2 edu i.party i.ecosector i.department danweisize
outreg2 using model24.doc

** model 2-4
xi: reg sfstatu net_heter i.class degree_depart degree_market i.hukou i.gender age age2 edu i.party i.ecosector i.department danweisize
outreg2 using model24.doc

** model 2-5
xi: reg sfstatu i.net_leader i.class degree_depart degree_market i.hukou i.gender age age2 edu i.party i.ecosector i.department danweisize
outreg2 using model24.doc

** model 2-6
xi: reg sfstatu i.net_manager i.class degree_depart degree_market i.hukou i.gender age age2 edu i.party i.ecosector i.department danweisize
outreg2 using model24.doc

** model 2-7
xi: reg sfstatu i.net_prof i.class degree_depart degree_market i.hukou i.gender age age2 edu i.party i.ecosector i.department danweisize
outreg2 using model24.doc



********************************* 4. FMM ******************************************

****************  choose model ****************  
gen lfincome = ln(fincome)
*** hist lincome
fmm 3: regress lfincome
estat lcprob
predict den,density marginal
histogram lfincome ,normal 

fmm 1: regress lfincome net_captial i.class degree_depart degree_market i.hukou i.gender age age2 edu i.party i.ecosector i.department danweisize
*** estat lcprob
estimates store fmm1
fmm 2: regress lfincome net_captial i.class degree_depart degree_market i.hukou i.gender age age2 edu i.party i.ecosector i.department danweisize
estimates store fmm2
fmm 3: regress lfincome net_captial i.class degree_depart degree_market i.hukou i.gender age age2 edu i.party i.ecosector i.department danweisize
estimates store fmm3
estimates stats fmm1 fmm2 fmm3


****************  improve model_constraint var ****************
*** fmm 3, lcprob(edu):regress lfincome net_captial i.class degree_depart degree_market i.hukou i.gender age age2 edu i.party i.ecosector i.department danweisize
*** estimates store fmm3f
*** estimates stats fmm3 fmm3f

fmm 3, lcprob(gender):regress lfincome net_captial i.class degree_depart degree_market i.hukou i.gender age age2 edu i.party i.ecosector i.department danweisize
estimates store fmm3f
estimates stats fmm3 fmm3f
*** when we set hukou as the constraint var, the model does improve

****************  improve model_different equation ****************
fmm, lcprob(gender):(regress lfincome net_captial i.class degree_depart degree_market i.hukou i.gender age age2 edu i.party i.ecosector i.department danweisize) (regress lfincome net_captial i.class i.hukou i.gender edu) (regress lfincome net_captial i.class degree_depart degree_market i.hukou i.gender age age2 edu i.party i.ecosector i.department danweisize)
estimates store fmm3ff
estimates stats fmm3f fmm3ff
*** does not improve

****************  compare coef ****************
fmm 3, lcprob(gender):regress lfincome net_captial i.class degree_depart degree_market i.hukou i.gender age age2 edu i.party i.ecosector i.department danweisize
estimates store fmm3f
fmm, coeflegend
test _b[lfincome:3.Class#c.net_captial] = _b[lfincome:1.Class#c.net_captial]
*** the outcome of test rejects h0, means there are turely different between them

contrast c.net_captial#a.Class, equation(lfincome)
*** the outcome of test rejects h0, means there are turely different among them



********************************* Yu-Teng Yan ********************************

