/*********************************************
 * FILE   : covid-unemployment.do
 * AUTHOR : Jiin Jeong
 * DATE   : Nov 22, 2020
 * DESC   : Analysis of COVID-19's impact on minority unemployment
 */

/*********************STEP 1: Merge two datasets ************************/
g year = 2020  // current dataset
append using "\\sss.hamilton.edu\jjeong$\Citrix\Downloads\Final Proj\September\cpsb201909.dta"  // file path would differ
replace year = 2019 if year != 2020
label variable year "Year of survey"

/*********************STEP 2: Data cleanup (Drop variables) ************************/
/*** 1. Only keep necessary variables ***/
keep hrhhid pemaritl prchld puchinhh hefaminc gereg gediv gestfips gtmetsta ptdtrace prdthsp pehspnon penatvty pemntvty pefntvty prcitshp prinusyr pesex prtage peeduca pemlr prempnot pelkdur puwk peret1 peabsrsn pemjot pemjnum pehrusl1 pehrusl2 pehrftpt pulk peafever peafnow year

/*** 2. Label and rename variables ***/
// Family variables
label variable hrhhid "Household identifier"
rename hrhhid hhID
label variable pemaritl "Marital status"
rename pemaritl maritalStatus
label variable prchld "Children"
rename prchld childStatus
label variable puchinhh "Change in household composition"
rename puchinhh hhChange
label variable hefaminc "Family income"
rename hefaminc hhIncome

// Geographical variables
label variable gereg "Geographical region (Northeast, Midwest, South, West)"
rename gereg region
label variable gediv "More detailed geographical region"
rename gediv regionDet
label variable gestfips "Geographical state"
rename gestfips state
label variable gtmetsta "Metropolitan status"
rename gtmetsta metroStatus

// Racial variables
label variable ptdtrace "Race category"
rename ptdtrace race
label variable prdthsp "Detailed Hispanic"
rename prdthsp hispanicDet
label variable pehspnon "Hispanic/Non-Hispanic"
rename pehspnon hispanicStatus

// Immigration variables
label variable penatvty "Country of birth"
rename penatvty birth
label variable pemntvty "Mother's country of birth"
rename pemntvty motherBirth
label variable pefntvty "Father's country of birth"
rename pefntvty fatherBirth
label variable prcitshp "Citizenship status"
rename prcitshp citizenStatus
label variable prinusyr "Immigrant year of entry"
rename prinusyr immigYear

// Gender variables
label variable pesex "Sex"
rename pesex sex

// Age and education variables
label variable prtage "Age; top-coded at 85 years"
rename prtage age
label variable peeduca "Highest level of school"
rename peeduca education

// Employment variables
label variable pemlr "Employment status - Detailed"
rename pemlr employment
label variable prempnot "Employment status - Summarized"
rename prempnot employmentDet
label variable pelkdur "Duration of unemployment; top-coded at 119 weeks"
rename pelkdur lookingDur

// Employment detail variables
label variable puwk "Last week, work"
rename puwk lastWkWork
label variable peret1 "Want a job"
rename peret1 wantJob
label variable peabsrsn "Reason absent from work last week"
rename peabsrsn absentReason
label variable pemjot "More than one job?"
rename pemjot otherJobs
label variable pemjnum "Total # of jobs"
rename pemjnum numJobs
label variable pehrusl1 "Hrs/week at main job"
rename pehrusl1 hrsMain
label variable pehrusl2 "Hrs/week at other jobs"
rename pehrusl2 hrsOther
label variable pehrftpt "Work 35+/wk"
rename pehrftpt work35
label variable pulk "Doing things to find work during the last 4 weeks"
rename pulk lookingWork

// Veteran variables
label variable peafever "Veteran status"
rename peafever veteran
label variable peafnow "Active force status"
rename peafnow activeForce


/*********************STEP 3: Data cleanup (Drop missing vals, Generate dummy) ************************/
/*** 1. Drop missing val ***/
drop if employmentDet == -1  // Missing employment status

/*** 2. Generate dummy vars ***/
// Family variables
gen married = .
replace married = 1 if (maritalStatus == 1 | maritalStatus == 2)
replace married = 0 if (maritalStatus == 3 | maritalStatus == 4 | maritalStatus == 5 | maritalStatus == 6)
label variable married "1 if married; 0 otherwise"

gen child = .
replace child = 1 if (childStatus != 0)
replace child = 0 if (childStatus == 0)
label variable child "1 if has children; 0 otherwise"

// Geographical variables
gen metro = .
replace metro = 1 if (metroStatus == 1)
replace metro = 0 if (metroStatus == 2)
label variable metro "1 if metro area; 0 otherwise"

// Racial variables
// 1) Simple category of race: "Only"
gen white = .
replace white = 1 if (race == 1)
replace white = 0 if (race != 1)
label variable white "1 if white-only; 0 otherwise"

gen black = .
replace black = 1 if (race == 2)
replace black = 0 if (race != 2)
label variable black "1 if black-only; 0 otherwise"

gen hispanic = .
replace hispanic = 1 if (hispanicStatus == 1)
replace hispanic = 0 if (hispanicStatus != 1)
label variable hispanic "1 if hispanic; 0 otherwise"

gen native = .
replace native = 1 if (race == 3)
replace native = 0 if (race != 3)
label variable native "1 if American Indian/Alaskan Native-only; 0 otherwise"

gen asian = .
replace asian = 1 if (race == 4)
replace asian = 0 if (race != 4)
label variable asian "1 if Asian-only; 0 otherwise"

gen islander = .
replace islander = 1 if (race == 5)
replace islander = 0 if (race != 5)
label variable islander "1 if Hawaiian/Pacific Islander-only; 0 otherwise"

// 2) Consider multiple categories of race
gen blackMix = .
replace blackMix = 1 if (race == 2 | race == 6 | race == 10 | race == 11 | race == 12)
replace blackMix = 0 if (blackMix != 1)
label variable blackMix "1 if black (mixed race included); 0 otherwise"

gen nativeMix = .
replace nativeMix = 1 if (race == 3 | race == 7 | race == 10 | race == 13)
replace nativeMix = 0 if (nativeMix != 1)
label variable nativeMix "1 if American Indian/Alaskan Native (mixed race included); 0 otherwise"

gen asianMix = .
replace asianMix = 1 if (race == 4 | race == 8 | race == 11 | race == 13)
replace asianMix = 0 if (asianMix != 1)
label variable asianMix "1 if Asian (mixed race included); 0 otherwise"

gen islanderMix = .
replace islanderMix = 1 if (race == 5 | race == 9 | race == 12)
replace islanderMix = 0 if (islanderMix != 1)
label variable islanderMix "1 if Hawaiian/Pacific Islander-only; 0 otherwise"

// Immigration variables
gen citizen = .
replace citizen = 1 if (citizenStatus != 5)
replace citizen = 0 if (citizenStatus == 5)
label variable citizen "1 if US citizen; 0 otherwise"

gen foreignBorn = .
replace foreignBorn = 1 if (citizenStatus == 4 | citizenStatus == 5)
replace foreignBorn = 0 if (citizenStatus == 1 | citizenStatus == 2 | citizenStatus == 3)
label variable foreignBorn "1 if foreign born; 0 otherwise"

// Gender variables
gen female = .
replace female = 1 if (sex == 2)
replace female = 0 if (sex == 1)
label variable female "1 if female; 0 otherwise"

// Age, education, and experience variables (This is how Fairlie, Couch, and Xu calculates)
gen ageSq = age * age
label variable ageSq "Age squared"

gen eduYears = .
replace eduYears = 0 if education==31
replace eduYears = 2.5 if education==32
replace eduYears = 5.5 if education==33
replace eduYears = 7.5 if education==34
replace eduYears = 9 if education==35
replace eduYears = 10 if education==36
replace eduYears = 11 if education==37
replace eduYears = 12 if education==38 | education==39
replace eduYears = 14 if education==40 | education==41 | education==42
replace eduYears = 16 if education==43
replace eduYears = 18 if education==44 | education==45
replace eduYears = 23 if education==46
label variable eduYears "Estimated yrs of education"

gen experience = age - eduYears - 5 
replace experience = 0 if experience < 0  // Replace with 0 if negative
label variable experience "Estimated yrs of experience"
gen experienceSq = experience * experience
label variable experienceSq "Experience squared"

gen expCode = experience // Categorial version of experience
replace expCode = 1 if (experience <= 10)
replace expCode = 2 if (experience > 10 & experience <= 20)
replace expCode = 3 if (experience > 20 & experience <= 30)
replace expCode = 4 if (experience > 30 & experience <= 40)
replace expCode = 5 if (experience > 40 & experience <= 50)
replace expCode = 6 if (experience > 50)
label variable expCode "Experience categorized in 10 yr bins"

// Employment variables
// 1) Excludes not in labor force (NILF).
gen unemployed = .
replace unemployed = 1 if (employmentDet == 2)  // Unemployed is 1
replace unemployed = 0 if (employmentDet == 1)  // Employed is 0
label variable unemployed "1 if unemployed; 0 otherwise (Excludes all NILF)"

// 2) Excludes NILF, but includes discouraged workers.
gen unemployed2 = .
replace unemployed2 = 1 if (employmentDet == 2 | employmentDet == 3)  // Unemployed is 1
replace unemployed2 = 0 if (employmentDet == 1)  // Employed is 0
label variable unemployed2 "1 if unemployed; 0 otherwise (Excludes NILF, includes discouraged workers)"

/*********************STEP 4: Data cleanup (COVID and interaction terms) ************************/
// COVID variable
g covid = .
replace covid = 1 if year == 2020
replace covid = 0 if year == 2019
label variable covid "1 if COVID-time; 0 if not"

// Interaction terms
g whiteCovid = white * covid
label variable whiteCovid "white * covid interaction term (base group; left out of reg)"
g blackCovid = black * covid
label variable blackCovid "black * covid interaction term"
g hispanicCovid = hispanic * covid
label variable hispanicCovid "hispanic * covid interaction term"
g asianCovid = asian * covid
label variable asianCovid "asian * covid interaction term"
g nativeCovid = native * covid
label variable nativeCovid "native * covid interaction term"
g islanderCovid = islander * covid
label variable islanderCovid "native * covid interaction term"

g femaleCovid = female * covid
label variable femaleCovid "female * covid interaction term"

g whiteFemaleCovid = white * female * covid
label variable whiteFemaleCovid "white * female * covid interaction term"
g blackFemaleCovid = black * female * covid
label variable blackFemaleCovid "black * female * covid interaction term"
g asianFemaleCovid = asian * female * covid
label variable asianFemaleCovid "asian * female * covid interaction term"

/*********************STEP 5: Desc Statistics ************************/

// Summary
sum if year == 2020
sum if year == 2019

// Desc stats of our survey pop
tab region year  // Region by year
tab regionDet year
tab state year
tab maritalStatus year
tab sex year
tab education year
tab race year
sum age if year == 2019
sum age if year == 2020
tab unemployed if year == 2019 & age < 18
tab unemployed if year == 2019 & age == 85
sum(eduYears) if year == 2019
sum(eduYears) if year == 2020
sum(experience) if year == 2019
sum(experience) if year == 2020

// Desc stats of unemployment
tab employmentDet year
tab(unemployed) if year == 2020  // Unemployment for 2020
tab(unemployed) if year == 2019  // Unemployment for 2019

sum(unemployed) if year == 2020 & white == 1  // Unemployment for white 2020
sum(unemployed) if year == 2019 & white == 1  // Unemployment for white 2019

sum(unemployed) if year == 2020 & black == 1  // Unemployment for black 2020
sum(unemployed) if year == 2019 & black == 1  // Unemployment for black 2019

sum(unemployed) if year == 2020 & asian == 1  // Unemployment for asian 2020
sum(unemployed) if year == 2019 & asian == 1  // Unemployment for asian 2019

sum(unemployed) if year == 2020 & hispanic == 1  // Unemployment for hispanic 2020
sum(unemployed) if year == 2019 & hispanic == 1  // Unemployment for hispanic 2019

sum(unemployed) if year == 2020 & native == 1  // Unemployment for native 2020
sum(unemployed) if year == 2019 & native == 1  // Unemployment for native 2019

sum(unemployed) if year == 2020 & islander == 1  // Unemployment for islander 2020
sum(unemployed) if year == 2019 & islander == 1  // Unemployment for islander 2019

sum(unemployed) if year == 2020 & female == 0  // Unemployment for men 2020
sum(unemployed) if year == 2019 & female == 0  // Unemployment for men 2019

sum(unemployed) if year == 2020 & female == 1  // Unemployment for women 2020
sum(unemployed) if year == 2019 & female == 1  // Unemployment for women 2019

sum(unemployed) if year == 2020 & foreignBorn == 1  // Unemployment for foreign born 2020
sum(unemployed) if year == 2019 & foreignBorn == 1  // Unemployment for foreign born 2019

sum(unemployed) if year == 2020 & foreignBorn == 0  // Unemployment for native 2020
sum(unemployed) if year == 2019 & foreignBorn == 0  // Unemployment for native 2019

sum(unemployed) if year == 2020 & married == 0  // Unemployment for nonmarried 2020
sum(unemployed) if year == 2019 & married == 0  // Unemployment for nonmarried 2019

sum(unemployed) if year == 2020 & married == 1  // Unemployment for married 2020
sum(unemployed) if year == 2019 & married == 1  // Unemployment for married 2019

sum(unemployed) if year == 2020 & child == 1  // Unemployment for ind w/ child 2020
sum(unemployed) if year == 2019 & child == 1  // Unemployment for ind w/ child 2019

sum(unemployed) if year == 2020 & child == 0  // Unemployment for ind w/o child 2020
sum(unemployed) if year == 2019 & child == 0  // Unemployment for ind w/o child 2019

/*********************STEP 6: DID ************************/
// RACE : Basic regression
reg unemployed black hispanic asian covid blackCovid hispanicCovid asianCovid 
reg unemployed black hispanic asian covid blackCovid hispanicCovid asianCovid, vce(robust)  // hetero
reg unemployed black hispanic asian covid blackCovid hispanicCovid asianCovid, vce(cl state)  // cluster state

// Add in controls
// Age, Experience, Education
reg unemployed black hispanic asian covid blackCovid hispanicCovid asianCovid age eduYears experience
reg unemployed black hispanic asian covid blackCovid hispanicCovid asianCovid age eduYears experience, vce(robust)

// Marital, Gender
reg unemployed black hispanic asian covid blackCovid hispanicCovid asianCovid age eduYears experience state married child female, vce(robust)

// Immigration
reg unemployed black hispanic asian native islander covid blackCovid hispanicCovid asianCovid nativeCovid islanderCovid age eduYears experience female married child metro veteran citizen foreignBorn, vce(robust)

// SEX
reg unemployed covid female femaleCovid
reg unemployed covid female femaleCovid, vce(robust)
reg unemployed covid female femaleCovid, vce(cl state)
reg unemployed covid female femaleCovid age eduYears experience, vce(robust)
reg unemployed covid female femaleCovid age eduYears experience married child, vce(robust)
reg unemployed female covid femaleCovid age eduYears experience married child metro veteran citizen foreignBorn, vce(robust)

/*********************STEP 7: Probit ************************/
probit unemployed age eduYears experience black hispanic native islander asian if covid == 0
margins, dydx(*)
probit unemployed age eduYears experience black hispanic native islander asian if covid == 1
margins, dydx(*)

probit unemployed age eduYears experience female if covid == 0
margins, dydx(*)
probit unemployed age eduYears experience female if covid == 1
margins, dydx(*)
