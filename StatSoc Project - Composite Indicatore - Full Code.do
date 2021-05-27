
****************** PROGETTO STATISTICA SOCIALE - COSTRUZIONE DI UN INDICATORE COMPOSITO
****** ANNALISA SONNATI, MATTEO ZUCCA. CDS STATISTICA. A.A. 2020/2021. CODICE COMPLETO

**************************************************

cd "C:\Users\Matteo Zucca\Desktop\Statistica Sociale\Tocchioni\Progetto - 2\1 & 2 - Creazione e Imputazione\SingleReport"

*** Per creare il dataset in formato stata
import excel "indicatori_gruppo_3.xlsx", sheet("Foglio1") firstrow clear
save "indicatori_gruppo_3.dta",replace
***

******************

*** Per creare i due dataset

**dataset con dati delle settimane 0-24
use "indicatori_gruppo_1.dta",clear
**
foreach j of numlist 2/14 {
merge 1:1 regioni using "indicatori_gruppo_`j'.dta",nogen
}
save "indicatori_monitoraggio_settimane 0-24.dta",replace


**dataset con dati delle settimane 25-42
use "indicatori_gruppo_15.dta",clear
**
foreach j of numlist 16/26 {
merge 1:1 regioni using "indicatori_gruppo_`j'.dta",nogen
}
save "indicatori_monitoraggio_settimane 25-42.dta",replace

**************************************************

cd "C:\Users\Matteo Zucca\Desktop\Statistica Sociale\Tocchioni\Progetto - 2\1 - 2 - 3 - Creaz + Imput + Stand\Backup"
* cd "C:\Users\Matteo Zucca\Desktop\Statistica Sociale\Tocchioni\Progetto - 2\1 - 2 - 3 - Creaz + Imput + Stand"
use "indicatori_monitoraggio_settimane_0-24.dta",replace
use "indicatori_monitoraggio_settimane_25-42.dta", replace
destring * ,replace

*********
desc *
summarize *
misstable summarize *
tab1 Rt_3, m

********* SETTIMANE 0-24

gen mean_Rt_2=(Rt_1+Rt_3)/2
replace Rt_2 = mean_Rt_2 if Rt_2==.

gen mean_Rt_9=(Rt_8+Rt_11)/2
gen mean_Rt_10=(Rt_8+Rt_11)/2
replace Rt_9=mean_Rt_9 if Rt_9==.
replace Rt_10=mean_Rt_10 if Rt_10==.

gen mean_Casi_9=(Casi_8+Casi_10)/2
replace Casi_9=mean_Casi_9 if Casi_9==.

gen mean_Incidenza_9=(Incidenza_8+Incidenza_10)/2
replace Incidenza_9=mean_Incidenza_9 if Incidenza_9==.

gen mean_Casi_10=(Casi_9+Casi_11)/2
replace Casi_10=mean_Casi_10 if Casi_10==.


********* SETTIMANE 25-42
* 1.3 Numero di casi notificati per mese con trasferimento in TI 
* in cui è indicata la data di trasferimento o ricovero in
* TI / totale di casi con trasferimento in TI notificati al
* sistema di sorveglianza nello stesso periodo

* 1.3 -> è un tasso -> imputazione con media cross-sectional (sulle altre regioni)
* settimane 34 e 35 sono le settimane dal 28 dicembre 2020 al 10 gennaio 2021

*3.8: Tasso di occupazione dei posti letto totali di TI (codice 49) per
	* pazienti COVID-19.
*3.9: Tasso di occupazione dei posti letto totali di Area Medica 
	* per pazienti COVID-19

*3.8 e 3.9 -> sono tassi -> varie tipologie possibili -> valuto convenienza
	* settimane 26 e 29 sono quelle che vanno dal 2 novembre 2020 al 
	* 8 novembre 2020 e dal 23 novembre 2020 al 29 novembre 2020

* Indic3_9_29 -> imputato con media longitudinale perchè dati simili

* Indic3_9_26 -> mediana cross-sectional sarebbe 47.5 mentre i valori della
	* settimana prima e dopo sarebbero 18 e 35 e non avrebbe senso
	* per questo motivo si è usato media longitudinale
	
* Indic3_8_26 -> mediana cross-sectional sarebbe 33.5 mentre i valori della
	* settimana prima e dopo sarebbero 12 e 33 e non avrebbe senso
	* per questo motivo si è usato media longitudinale
		
	
gen mean_Indic3_8_26 = (Indic3_8_25+Indic3_8_27)/2
gen mean_Indic3_9_26 = (Indic3_9_25+Indic3_9_27)/2
gen mean_Indic3_9_29 = (Indic3_9_28+Indic3_9_30)/2

replace Indic3_8_26 = mean_Indic3_8_26 if Indic3_8_26==.
replace Indic3_9_26 = mean_Indic3_9_26 if Indic3_9_26==.
replace Indic3_9_29 = mean_Indic3_9_29 if Indic3_9_29==.

egen median_Indic1_3_34 = median(Indic1_3_34)
egen median_Indic1_3_35 = median(Indic1_3_35)

replace Indic1_3_34 = median_Indic1_3_34 if Indic1_3_34==.
replace Indic1_3_35 = median_Indic1_3_35 if Indic1_3_35==.

**************************************************

cd "C:\Users\Matteo Zucca\Desktop\Statistica Sociale\Tocchioni\Progetto - 2\1-2-3 - Until Standardizzazione"

use "indicatori_monitoraggio_settimane_25-42.dta",clear
merge 1:1 regioni using "popolazione_italia.dta"
drop _merge

encode regioni, gen(regioni_num)
order regioni regioni_num
tab regioni_num

* settimana 25 ha report non corretti -> si effettua il drop
drop *_25
save,replace

* standardizzazione indicatori:
* si parte dalla settimana 26 perchè si è droppato il resto
* 3.1 (numero casi riportati negli ultimi 14 gg), 
* 3.5 (numero nuovi focolai),
* 3.6 (Numero di nuovi casi non associati a catene di trasmissione note)
foreach j of numlist 26/43 {
gen Std3_1_`j'=(Indic3_1_`j'/Totale)*100000
gen Std3_5_`j'=(Indic3_5_`j'/Totale)*100000
gen Std3_6_`j'=(Indic3_6_`j'/Totale)*100000
}

summ Std3_*
list regioni Indic3_1_35 Indic3_5_35 Indic3_6_35 Std3_?_35

save,replace

**************************************************

***** Correzione nomi indicatori

** Questo script ha l'obiettivodi correggere i nomi
	* e droppare i vecchi indicatori non standardizzati

** La settimana 25 ha report non corretti e si è droppata
** Indicatori 3.1, 3.5 e 3.6 sono stati portati nella forma
	* di rapporti di composizione, ossia trasformati da numeri	
	* puri a tassi dividendo per la popolazione

* 3.1: numero casi riportati negli ultimi 14 gg. 
* 3.5: numero nuovi focolai.
* 3.6: numero di nuovi casi non associati a catene di trasmissione note

cd "C:\Users\Matteo Zucca\Desktop\Statistica Sociale\Tocchioni\Progetto - 2\4 - Analisi Multivariata"
use "indicatori_monitoraggio_settimane_25-42.dta",clear

** Rimuovere indicatori non standardizzati
drop Indic3_1_*
drop Indic3_5_*
drop Indic3_6_*

* Check che manchino solo loro:
list Indic3_* in 1

** Rinominare gli indicatori standardizzati
rename Std* Indic*

** Ordinare il dataset:
order _all, first alphabetic
order regioni regioni_num TotalePopolaz, first alphabetic
order TotalePopolaz, after(regioni_num) alphabetic

** Salvare il dataset
save,replace

**************************************************


****** HomeWork: Cluster Analysis on Project Dataset

cd "C:\Users\Matteo Zucca\Desktop\Statistica Sociale\Tocchioni\Progetto - 2\4 - Analisi Multivariata"
use "indicatori_monitoraggio_settimane_25-42.dta",clear
set more off, permanently
set logtype text
log using logfile_cluster_homework, replace
* help cluster wardslinkage
* help cluster dendrogram ward

******* Es HomeWork Cluster Analysis
***** 1) Effettuare un’analisi dei gruppi (utilizzando un algoritmo a propria scelta) 
* raggruppando le regioni in una data settimana
**** 2) Disegnare il dendrogramma e valutare il numero di cluster ottimale graficamente
**** 3) Essere in grado di descrivere i gruppi ottenuti in base ai valori medi degli 
* indicatori e alle diverse regioni che compongono ciascun gruppo

*** Settimane scelte: 27 e 37 (Daly e Toccafondi) 

*** Ward's linkage (squared euclidean distance) 
* help cluster wardslinkage
* help cluster dendrogram ward
cluster wardslinkage Indic1_*_27, name(ward)
cluster dendrogram ward /*paint dendrogram*/
cluster generate gruppi_w=groups(2/6), name(ward)
 * generate groups variables for specified cluster from 2 to 6*

* If I choose 2 groups then the group stats are:
tabstat Indic1_*_27, by(gruppi_w2) stat(n mean sd median)
list regioni if gruppi_w2==1
list regioni if gruppi_w2==2

***	Complete linkage (euclidean distance, not squared)
cluster complete Indic3_*_37, name(complete)
cluster dendrogram complete /*paint dendrogram*/
cluster generate gruppi_c=groups(2/6), name(complete)
 * generate groups variables for specified cluster from 2 to 6*

* If I choose 2 groups then the group stats are:
tabstat Indic3_*_37, by(gruppi_c3) stat(n mean sd median)
list regioni if gruppi_c3==1
list regioni if gruppi_c3==2
list regioni if gruppi_c3==3

***	Single linkage (euclidean distance, not squared)
cluster single Indic2_*_30, name(single)
cluster dendrogram single
cluster generate gruppi_s=groups(2/6),name(single)

* If I choose 3 groups then the group stats are:
tabstat Indic2_*_30, by(gruppi_s3) stat(n mean sd median)
list regioni if gruppi_s4==1
list regioni if gruppi_s4==2
list regioni if gruppi_s4==3
list regioni if gruppi_s4==4

*** Differences between Ward, Complete and Single Linkages
tab gruppi_w3 gruppi_c3
tab gruppi_c3 gruppi_s3
tab gruppi_w3 gruppi_s3

*** Remember to drop variable before quitting!
drop ward* gruppi* complete* single*


**************************************************

****** HomeWork: Cronbach's Alpha on Project Dataset

cd "C:\Users\Matteo Zucca\Desktop\Statistica Sociale\Tocchioni\Progetto - 2\4 - Analisi Multivariata"
use "indicatori_monitoraggio_settimane_25-42.dta",clear

set more off, permanently
set logtype text
log using logfile_apha_homework, replace
descr
misstable summ
* help alpha

******* Es HomeWork Alpha Cronbach
***** 1) Calcolare il Cronbach alpha per i tre gruppi di indicatori (#1, #2, #3) 
* per un paio di settimane a scelta. Separare per gruppo e per settimana
**** 2) Verificare se l’inclusione/l’esclusione di uno o più indicatori migliori la coerenza interna del gruppo di
* indicatori -> controllare opzione "i"
**** 3) Essere in grado di commentare il risultato ottenuto

*** Settimane scelte: 27 e 37 (Daly e Toccafondi) 

** Settimana 27
* casewise: delete cases with missing values (abbreviate as "c")
* Has no effect because we have no missing data
alpha Indic1_*_27 , casewise
alpha Indic2_*_27
alpha Indic3_*_27
* Result: 0.2294 (1), 0.2051 (2), 0.3342 (3)

* reverse: reverse signs of variables in parenthesis
* Remember: we want higher value -> higher risk
alpha Indic1_*_27 , reverse(Indic1_*_27)
alpha Indic1_*_27 , reverse(Indic1_4_27)
alpha Indic2_*_27, reverse(Indic2_4_27 Indic2_5_27)
alpha Indic3_*_27, reverse(Indic3_5_27 Indic3_9_27)


* std: standardize items in the scale to mean 0, variance 1
* Here std have a very good effect when not joint with reverse!
alpha Indic1_*_27, std
alpha Indic2_*_27, std
alpha Indic3_*_27, std 
* Result: 0.6284 (1), 0.5273 (2), 0.7244(3)

alpha Indic1_*_27
alpha Indic2_*_27
alpha Indic3_*_27

* i: display item-test and item-rest correlations
* We can appreciate alpha without a single indicator -> higher is better!

alpha Indic1_*_27, i
alpha Indic2_*_27, i
alpha Indic3_*_27, i

alpha Indic1_*_27, std i
alpha Indic2_*_27, std i
alpha Indic3_*_27, std i

* Comments:
	* Group 1: removing indicator 1.4 increases alpha std to 0.7
	* Group 2 e 3: no significant improving found 

* Can alpha increase mixing some variables? Try

alpha Indic3_*_27 Indic2_6_27, std
alpha Indic3_*_27 Indic2_6_27
alpha Indic3_*_27 Indic1_4_26, std
alpha Indic3_*_27 Indic1_4_26

* Rember how to interpret the option "i":
alpha Indic3_*_27 Indic2_6_26, std i 
* at the row named Indic2_6_26 has the same value as
alpha Indic3_*_27, std


** Settimana 37
alpha Indic1_*_37 , casewise
alpha Indic2_*_37
alpha Indic3_*_37
* Result: 0.2034 (1), 0.1365 (2), 0.4870 (3)

* reverse: reverse signs of variables in parenthesis
* Remember: we want higher value -> higher risk
alpha Indic1_*_37, reverse(Indic1_4_37)
alpha Indic2_*_37, reverse(Indic2_5_37 Indic2_6_37)
alpha Indic3_*_37, reverse(Indic3_9_37)

* std: standardize items in the scale to mean 0, variance 1
* Here std have a very good effect when not joint with reverse!
alpha Indic1_*_37, std
alpha Indic2_*_37, std
alpha Indic3_*_37, std 
* Result: 0.4 (1), 0.376 (2), 0.750(3)

* i: display item-test and item-rest correlations
* We can appreciate alpha without a single indicator -> higher is better!

alpha Indic1_*_37, i
alpha Indic2_*_37, i
alpha Indic3_*_37, i

alpha Indic1_*_37, std i
alpha Indic2_*_37, std i
alpha Indic3_*_37, std i

* Comments:
	* Group 2: removing indicator Indic2_6_37 increases 
		* alpha from 0.1365 to 0.3468
	* Group 1 e 3: no significant improving found 

* Can alpha increase mixing some variables? Try

alpha Indic3_*_37 Indic2_6_37, std
alpha Indic3_*_37 Indic2_6_37
alpha Indic3_*_37 Indic1_4_26, std
alpha Indic3_*_37 Indic1_4_26

* Rember how to interpret the option "i":
alpha Indic3_*_37 Indic2_6_26, std i 
* at the row named Indic2_6_26 has the same value as
alpha Indic3_*_37, std

log close

**************************************************
******** 5 - NORMALIZZAZIONE INDICATORI **********
**** Metodo Min-Max

cd "C:\Users\Matteo Zucca\Desktop\Statistica Sociale\Tocchioni\Progetto - 2\5 - Normalizzazione"
use "indicatori_monitoraggio_settimane_25-42.dta", clear

* Warnings:
	* 1. Check that week number 25 has been removed.
	* 2. Check that indicators that we want to exclude, based on Multivariate Analysys,
		* has been removed
	* 3. Beware that some indicators are named trend and other std -> change code!
	
	
**** 5.1 Change indicators' sign
	* remember: higher value -> higher risk
	* change sign of all indicators in group 1 
	* change sign of Indicators: 2.4, 2.5 e 2.6

rename Trend3_4_42 Indic3_4_42	
rename Trend3_4_43 Indic3_4_43	
	
foreach j of numlist 26/43 {
gen IndicP1_1_`j'=Indic1_1_`j'*(-1)
gen IndicP1_2_`j'=Indic1_2_`j'*(-1)
gen IndicP1_3_`j'=Indic1_3_`j'*(-1)
gen IndicP1_4_`j'=Indic1_3_`j'*(-1)

gen IndicP2_4_`j'=Indic2_4_`j'*(-1)
gen IndicP2_5_`j'=Indic2_5_`j'*(-1)
gen IndicP2_6_`j'=Indic2_6_`j'*(-1)
}


**** 5.2 Drop and Rename Indicator
foreach j of numlist 26/43 {
drop Indic1_1_`j' Indic1_2_`j' Indic1_3_`j' Indic1_4_`j'

rename IndicP1_1_`j' Indic1_1_`j'
rename IndicP1_2_`j' Indic1_2_`j'
rename IndicP1_3_`j' Indic1_3_`j'
rename IndicP1_4_`j' Indic1_4_`j'

drop Indic2_4_`j' Indic2_5_`j' Indic2_6_`j'

rename IndicP2_4_`j' Indic2_4_`j'
rename IndicP2_5_`j' Indic2_5_`j'
rename IndicP2_6_`j' Indic2_6_`j'

}


***** 5.3 Standardizzazione: mean e std.dev. variano nel tempo
foreach j of numlist 26/43 {

egen min1_1_`j'=min(Indic1_1_`j')
egen max1_1_`j'=max(Indic1_1_`j')
gen norm1_1_`j'=(Indic1_1_`j' - min1_1_`j')/(max1_1_`j' - min1_1_`j')

egen min1_2_`j'=min(Indic1_2_`j')
egen max1_2_`j'=max(Indic1_2_`j')
gen norm1_2_`j'=(Indic1_2_`j' - min1_2_`j')/(max1_2_`j' - min1_2_`j')

egen min1_3_`j'=min(Indic1_3_`j')
egen max1_3_`j'=max(Indic1_3_`j')
gen norm1_3_`j'=(Indic1_3_`j' - min1_3_`j')/(max1_3_`j' - min1_3_`j')

egen min1_4_`j'=min(Indic1_4_`j')
egen max1_4_`j'=max(Indic1_4_`j')
gen norm1_4_`j'=(Indic1_4_`j' - min1_4_`j')/(max1_4_`j' - min1_4_`j')

}

foreach j of numlist 26/43 {

egen min2_1_`j'=min(Indic2_1_`j')
egen max2_1_`j'=max(Indic2_1_`j')
gen norm2_1_`j'=(Indic2_1_`j' - min2_1_`j')/(max2_1_`j' - min2_1_`j')

egen min2_2_`j'=min(Indic2_2_`j')
egen max2_2_`j'=max(Indic2_2_`j')
gen norm2_2_`j'=(Indic2_2_`j' - min2_2_`j')/(max2_2_`j' - min2_2_`j')

egen min2_4_`j'=min(Indic2_4_`j')
egen max2_4_`j'=max(Indic2_4_`j')
gen norm2_4_`j'=(Indic2_4_`j' - min2_4_`j')/(max2_4_`j' - min2_4_`j')

egen min2_5_`j'=min(Indic2_5_`j')
egen max2_5_`j'=max(Indic2_5_`j')
gen norm2_5_`j'=(Indic2_5_`j' - min2_5_`j')/(max2_5_`j' - min2_5_`j')

egen min2_6_`j'=min(Indic2_6_`j')
egen max2_6_`j'=max(Indic2_6_`j')
gen norm2_6_`j'=(Indic2_6_`j' - min2_6_`j')/(max2_6_`j' - min2_6_`j')

}

foreach j of numlist 26/43 {

egen min3_1_`j'=min(Indic3_1_`j')
egen max3_1_`j'=max(Indic3_1_`j')
gen norm3_1_`j'=(Indic3_1_`j' - min3_1_`j')/(max3_1_`j' - min3_1_`j')

egen min3_2_`j'=min(Indic3_2_`j')
egen max3_2_`j'=max(Indic3_2_`j')
gen norm3_2_`j'=(Indic3_2_`j' - min3_2_`j')/(max3_2_`j' - min3_2_`j')

egen min3_4_`j'=min(Indic3_4_`j')
egen max3_4_`j'=max(Indic3_4_`j')
gen norm3_4_`j'=(Indic3_4_`j' - min3_4_`j')/(max3_4_`j' - min3_4_`j')

egen min3_5_`j'=min(Indic3_5_`j')
egen max3_5_`j'=max(Indic3_5_`j')
gen norm3_5_`j'=(Indic3_5_`j' - min3_5_`j')/(max3_5_`j' - min3_5_`j')

egen min3_6_`j'=min(Indic3_6_`j')
egen max3_6_`j'=max(Indic3_6_`j')
gen norm3_6_`j'=(Indic3_6_`j' - min3_6_`j')/(max3_6_`j' - min3_6_`j')

egen min3_8_`j'=min(Indic3_8_`j')
egen max3_8_`j'=max(Indic3_8_`j')
gen norm3_8_`j'=(Indic3_8_`j' - min3_8_`j')/(max3_8_`j' - min3_8_`j')

egen min3_9_`j'=min(Indic3_9_`j')
egen max3_9_`j'=max(Indic3_9_`j')
gen norm3_9_`j'=(Indic3_9_`j' - min3_9_`j')/(max3_9_`j' - min3_9_`j')


}

foreach j of numlist 26/43 {

egen minTrend3_1_`j'=min(Trend3_1_`j')
egen maxTrend3_1_`j'=max(Trend3_1_`j')
gen normTrend3_1_`j'=(Trend3_1_`j' -minTrend3_1_`j')/(maxTrend3_1_`j' - minTrend3_1_`j')

}

summ norm1_1_26
summ norm1_1_*
list regioni norm1_1_26


**************************************************
******** 5 - NORMALIZZAZIONE INDICATORI **********

cd "C:\Users\Matteo Zucca\Desktop\Statistica Sociale\Tocchioni\Progetto - 2\5 - Normalizzazione"
use "indicatori_monitoraggio_settimane_25-42.dta", clear

* Warnings:
	* 1. Check that week number 25 has been removed.
	* 2. Check that indicators that we want to exclude, based on Multivariate Analysys,
		* has been removed
	* 3. Beware that some indicators are named trend and other std -> change code!
	
	
**** 5.1 Change indicators' sign
	* remember: higher value -> higher risk
	* change sign of all indicators in group 1 
	* change sign of Indicators: 2.4, 2.5 e 2.6

foreach j of numlist 26/43 {
gen IndicP1_1_`j'=Indic1_1_`j'*(-1)
gen IndicP1_2_`j'=Indic1_2_`j'*(-1)
gen IndicP1_3_`j'=Indic1_3_`j'*(-1)
gen IndicP1_4_`j'=Indic1_3_`j'*(-1)

gen IndicP2_4_`j'=Indic2_4_`j'*(-1)
gen IndicP2_5_`j'=Indic2_5_`j'*(-1)
gen IndicP2_6_`j'=Indic2_6_`j'*(-1)
}


**** 5.2 Drop and Rename Indicator
foreach j of numlist 26/43 {
drop Indic1_1_`j' Indic1_2_`j' Indic1_3_`j' Indic1_4_`j'

rename IndicP1_1_`j' Indic1_1_`j'
rename IndicP1_2_`j' Indic1_2_`j'
rename IndicP1_3_`j' Indic1_3_`j'
rename IndicP1_4_`j' Indic1_4_`j'

drop Indic2_4_`j' Indic2_5_`j' Indic2_6_`j'

rename IndicP2_4_`j' Indic2_4_`j'
rename IndicP2_5_`j' Indic2_5_`j'
rename IndicP2_6_`j' Indic2_6_`j'

}


***** 5.3 Standardizzazione: mean e std.dev. variano nel tempo
foreach j of numlist 26/43 {

egen Media1_1_`j'=mean(Indic1_1_`j')
egen StDev1_1_`j'=sd(Indic1_1_`j')
gen norm1_1_`j'=(Indic1_1_`j' - Media1_1_`j')/StDev1_1_`j'

egen Media1_2_`j'=mean(Indic1_2_`j')
egen StDev1_2_`j'=sd(Indic1_2_`j')
gen norm1_2_`j'=(Indic1_2_`j' - Media1_2_`j')/StDev1_2_`j'

egen Media1_3_`j'=mean(Indic1_3_`j')
egen StDev1_3_`j'=sd(Indic1_3_`j')
gen norm1_3_`j'=(Indic1_3_`j' - Media1_3_`j')/StDev1_3_`j'

egen Media1_4_`j'=mean(Indic1_4_`j')
egen StDev1_4_`j'=sd(Indic1_4_`j')
gen norm1_4_`j'=(Indic1_4_`j' - Media1_4_`j')/StDev1_4_`j'

}


foreach j of numlist 26/43 {

egen Media2_1_`j'=mean(Indic2_1_`j')
egen StDev2_1_`j'=sd(Indic2_1_`j')
gen norm2_1_`j'=(Indic2_1_`j' - Media2_1_`j')/StDev2_1_`j'

egen Media2_2_`j'=mean(Indic2_2_`j')
egen StDev2_2_`j'=sd(Indic2_2_`j')
gen norm2_2_`j'=(Indic2_2_`j' - Media2_2_`j')/StDev2_2_`j'

egen Media2_4_`j'=mean(Indic2_4_`j')
egen StDev2_4_`j'=sd(Indic2_4_`j')
gen norm2_4_`j'=(Indic2_4_`j' - Media2_4_`j')/StDev2_4_`j'

egen Media2_5_`j'=mean(Indic2_5_`j')
egen StDev2_5_`j'=sd(Indic2_5_`j')
gen norm2_5_`j'=(Indic2_5_`j' - Media2_5_`j')/StDev2_5_`j'

egen Media2_6_`j'=mean(Indic2_6_`j')
egen StDev2_6_`j'=sd(Indic2_6_`j')
gen norm2_6_`j'=(Indic2_6_`j' - Media2_6_`j')/StDev2_6_`j'
}


foreach j of numlist 26/43 {

egen Media3_1_`j'=mean(Indic3_1_`j')
egen StDev3_1_`j'=sd(Indic3_1_`j')
gen norm3_1_`j'=(Indic3_1_`j' - Media3_1_`j')/StDev3_1_`j'

egen Media3_2_`j'=mean(Indic3_2_`j')
egen StDev3_2_`j'=sd(Indic3_2_`j')
gen norm3_2_`j'=(Indic3_2_`j' - Media3_2_`j')/StDev3_2_`j'

egen Media3_4_`j'=mean(Indic3_4_`j')
egen StDev3_4_`j'=sd(Indic3_4_`j')
gen norm3_4_`j'=(Indic3_4_`j' - Media3_4_`j')/StDev3_4_`j'

egen Media3_5_`j'=mean(Indic3_5_`j')
egen StDev3_5_`j'=sd(Indic3_5_`j')
gen norm3_5_`j'=(Indic3_5_`j' - Media3_5_`j')/StDev3_5_`j'

egen Media3_6_`j'=mean(Indic3_6_`j')
egen StDev3_6_`j'=sd(Indic3_6_`j')
gen norm3_6_`j'=(Indic3_6_`j' - Media3_6_`j')/StDev3_6_`j'

egen Media3_8_`j'=mean(Indic3_8_`j')
egen StDev3_8_`j'=sd(Indic3_8_`j')
gen norm3_8_`j'=(Indic3_8_`j' - Media3_8_`j')/StDev3_8_`j'

egen Media3_9_`j'=mean(Indic3_9_`j')
egen StDev3_9_`j'=sd(Indic3_9_`j')
gen norm3_9_`j'=(Indic3_9_`j' - Media3_9_`j')/StDev3_9_`j'
}

foreach j of numlist 26/43 {

egen Media3_1_`j'=mean(Trend3_1_`j')
egen StDev3_1_`j'=sd(Trend3_1_`j')
gen norm3_1_`j'=(Trend3_1_`j' - Media3_1_`j')/StDev3_1_`j'

}

egen Media3_4_42=mean(Trend3_4_42)
egen StDev3_4_42=sd(Trend3_4_42)
gen norm3_4_42=(Trend3_4_42 - Media3_4_42)/StDev3_4_42

egen Media3_4_43=mean(Trend3_4_43)
egen StDev3_4_43=sd(Trend3_4_43)
gen norm3_4_43=(Trend3_4_43 - Media3_4_43)/StDev3_4_43

summ norm1_1_26
summ norm1_1_*
list regioni norm1_1_26


**************************************************


cd "C:\Users\Matteo Zucca\Desktop\Statistica Sociale\Tocchioni\Progetto - 2\6 - Aggregazione"
use "indicatori_monitoraggio_settimane_25-42.dta", clear

drop regioni_num TotalePopolaz Trend*
drop Indic* normTrend* min* max*


**** Aggregazione senza pesi con la somma

foreach j of numlist 26/43 {

gen IndComp_`j' = norm1_1_`j' + norm1_2_`j' + norm1_3_`j' /*
*/ + norm1_4_`j' + norm2_1_`j' + norm2_2_`j' + norm2_4_`j' /*
*/ + norm2_5_`j' + norm2_6_`j' + norm3_1_`j' + norm3_2_`j'/*
*/ + norm3_4_`j' + norm3_5_`j' + norm3_6_`j' + norm3_8_`j' /*
*/ + norm3_9_`j'

}


**** Aggregazione senza pesi con la media in percentuale

drop IndComp*

foreach j of numlist 26/43 {

gen IndComp_`j' = (norm1_1_`j' + norm1_2_`j' + norm1_3_`j' /*
*/ + norm1_4_`j' + norm2_1_`j' + norm2_2_`j' + norm2_4_`j' /*
*/ + norm2_5_`j' + norm2_6_`j' + norm3_1_`j' + norm3_2_`j'/*
*/ + norm3_4_`j' + norm3_5_`j' + norm3_6_`j' + norm3_8_`j' /*
*/ + norm3_9_`j') * 100/16

}


**** Aggregazione con pesi con la media in percentuale
* pesi: 20% gruppo 1, 30% gruppo 2, 50% gruppo tre
	* doppi a: indicatori gruppo tre

drop IndComp*

foreach j of numlist 26/43 {

gen IndComp_G1_`j' = 0.2 * (norm1_1_`j' + norm1_2_`j' + norm1_3_`j' /*
*/ + norm1_4_`j' )

gen IndComp_G2_`j' = 0.3 * (norm2_1_`j' + norm2_2_`j' + norm2_4_`j' /*
*/ + norm2_5_`j' + norm2_6_`j' )

gen IndComp_G3_`j' = 0.5 * ( norm3_1_`j' + norm3_2_`j'/*
*/ + norm3_4_`j' + norm3_5_`j' + norm3_6_`j' + norm3_8_`j' /*
*/ + norm3_9_`j' )

gen IndComp_`j' = 100* (IndComp_G1_`j' + IndComp_G2_`j' + IndComp_G3_`j' )

}
 
sort IndComp_27
list regioni IndComp_27

sort IndComp_40
list regioni IndComp_40


**************************************************
