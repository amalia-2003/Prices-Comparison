#Proiect MICROECONOMIE MANAGERIALA
#Lica Amalia_Grupa 1045
#Uber Technologies, Inc. (UBER)

#PAS 4
#Import date
setwd("C:/Users/ASUS/Desktop/ProiectMicro")

preturi_actiuni<-read.csv(file="preturiProiect.csv",header=T,sep=",")
#View(preturi_actiuni)
head(preturi_actiuni)
tail(preturi_actiuni)
#fix(preturi_actiuni)

#rentabilitate 
rentabilitate_UBER <- numeric(length(preturi_actiuni$PRET_UBER) - 1)
for (i in 2:length(preturi_actiuni$PRET_UBER)) {
  rentabilitate_UBER[i - 1] <- (preturi_actiuni$PRET_UBER[i] / preturi_actiuni$PRET_UBER[i - 1]) - 1
}
rentabilitate_UBER
#rentabilitate_UBER <- numeric(length(preturi_actiuni$PRET_UBER) - 1):
#Această linie inițializează un vector rentabilitate_UBER ca fiind un vector numeric. 
#Lungimea acestui vector este cu unu mai mică decât lungimea vectorului preturi_actiuni$PRET_UBER
#Decrementarea lungimii cu unu este necesară deoarece rentabilitatea este calculată între două zile consecutive, și astfel pentru n zile, există n-1 rentabilități.

#Loop-ul for:
#for (i in 2:length(preturi_actiuni$PRET_UBER)):
#Acest loop începe de la 2 și merge până la lungimea totală a vectorului de prețuri, preturi_actiuni$PRET_UBER. 
#Începerea de la 2 este necesară pentru că rentabilitatea se calculează între ziua curentă (i) și ziua anterioară (i - 1).

#rentabilitate_UBER[i - 1] <- (preturi_actiuni$PRET_UBER[i] / preturi_actiuni$PRET_UBER[i - 1]) - 1:
#În fiecare iterație a loop-ului, această linie calculează rentabilitatea zilnică. 
#Se face prin împărțirea prețului acțiunii din ziua i la prețul din ziua i-1 și scăderea lui 1. 
#Rezultatul este un procent care reprezintă creșterea sau scăderea procentuală a prețului acțiunii de la o zi la alta.


rentabilitate_NASDAQ <- numeric(length(preturi_actiuni$PRET_NASDAQ) - 1)
for (i in 2:length(preturi_actiuni$PRET_NASDAQ)) {
  rentabilitate_NASDAQ[i - 1] <- (preturi_actiuni$PRET_NASDAQ[i] / preturi_actiuni$PRET_NASDAQ[i - 1]) - 1
}
rentabilitate_NASDAQ

rentabilitate_LYFT <- numeric(length(preturi_actiuni$PRET_LYFT) - 1)
for (i in 2:length(preturi_actiuni$PRET_LYFT)) {
  rentabilitate_LYFT[i - 1] <- (preturi_actiuni$PRET_LYFT[i] / preturi_actiuni$PRET_LYFT[i - 1]) - 1
}
rentabilitate_LYFT

rentabilitati_firme <- data.frame(Date = preturi_actiuni$Date[-1], Rentabilitate_UBER = rentabilitate_UBER, Rentabilitate_NASDAQ = rentabilitate_NASDAQ, Rentabilitate_LYFT = rentabilitate_LYFT)

write.csv(rentabilitati_firme, file = "rentabilitati.csv", row.names = FALSE)


#dataframe
preturi_actiuni_comune<-data.frame(preturi_actiuni$Date,preturi_actiuni$PRET_UBER,preturi_actiuni$PRET_NASDAQ, preturi_actiuni$PRET_LYFT)
#fix(preturi_actiuni_comune)
#View(preturi_actiuni_comune)

preturi_rentabilitate_comune<-data.frame(rentabilitati_firme$Date, rentabilitati_firme$Rentabilitate_UBER, rentabilitati_firme$Rentabilitate_NASDAQ, rentabilitati_firme$Rentabilitate_LYFT)
#fix(preturi_rentabilitate_comune)
#View(preturi_rentabilitate_comune)

#statistici descriptive
summary(preturi_actiuni_comune)
summary(preturi_actiuni_comune[-1])
statistici_descriptive<-summary(preturi_actiuni_comune[-1])

summary(preturi_rentabilitate_comune)
summary(preturi_rentabilitate_comune[-1])
statistici_descriptive_ren<-summary(preturi_rentabilitate_comune[-1])

#abatarea standard
#Abaterea standard ne oferă o măsură a riscului și incertitudinii asociate cu 
#investițiile în aceste acțiuni sau indici. O valoare mai mare a abaterii standard 
#indică o volatilitate mai mare, ceea ce poate însemna un risc mai mare, dar și 
#oportunități potențiale mai mari pentru profit.
sd.uber<-sd(preturi_actiuni$PRET_UBER)
sd.uber
#Abaterea standard de 9.17 indică faptul că majoritatea prețurilor acțiunilor Uber sunt distribuite într-un interval de ± 9.17 unități față de media prețurilor. 
#Acest lucru înseamnă că fluctuațiile zilnice ale prețului acțiunilor Uber sunt în general de aproximativ 9.17 unități în jurul mediei. 
sd.nasdaq<-sd(preturi_actiuni$PRET_NASDAQ)
sd.nasdaq
#Abaterea standard de 1226.67 sugerează o volatilitate foarte mare, cu prețurile 
#distribuite pe un interval larg de ±1226.67 unități față de medie, reflectând fluctuații substanțiale zilnice.
sd.lyft<-sd(preturi_actiuni$PRET_LYFT)
sd.lyft
#Abaterea standard de 2.07 arată o volatilitate scăzută, cu prețurile acțiunilor 
#în general foarte apropiate de medie, fluctuând pe un interval restrâns de ±2.07 unități.

sd.rentUber<-sd(rentabilitati_firme$Rentabilitate_UBER)
sd.rentUber
#Abaterea standard de 0.0228 indică o volatilitate moderată în rentabilitatea zilnică a 
#acțiunilor Uber, ceea ce înseamnă că rentabilitatea se schimbă zilnic cu o valoare medie de ±2.28%.
sd.rentNasdaq<-sd(rentabilitati_firme$Rentabilitate_NASDAQ)
sd.rentNasdaq
#Abaterea standard de 0.0099 sugerează o volatilitate scăzută în rentabilitatea zilnică a indicelui NASDAQ. 
#Rentabilitățile sunt relativ stabile, fluctuând în medie cu ±0.99% pe zi.
sd.rentLyft<-sd(rentabilitati_firme$Rentabilitate_LYFT)
sd.rentLyft
#Abaterea standard de 0.0459 arată o volatilitate mare în rentabilitatea zilnică a acțiunilor Lyft. 
#Acest lucru indică fluctuații mai ample ale rentabilității zilnice, aproximativ ±4.59%

#Coeficienti de variatie 
cv.uber<-sd(preturi_actiuni$PRET_UBER)/mean(preturi_actiuni$PRET_UBER)
cv.uber
#Cu un CV de aproximativ 21.95% pentru Uber, seria este relativ omogenă, iar media este reprezentativă pentru setul de date
cv.nasdaq<-sd(preturi_actiuni$PRET_NASDAQ)/mean(preturi_actiuni$PRET_NASDAQ)
cv.nasdaq
#Cu un CV de aproximativ 8.70% pentru Nasdaq, seria este foarte omogenă, iar media este foarte reprezentativă pentru setul de date,
cv.lyft<-sd(preturi_actiuni$PRET_LYFT)/mean(preturi_actiuni$PRET_LYFT)
cv.lyft
#Cu un CV de aproximativ 18.57% pentru Lyft, seria este relativ omogenă, iar media este reprezentativă 

cv.rentUber<-sd(rentabilitati_firme$Rentabilitate_UBER)/mean(rentabilitati_firme$Rentabilitate_UBER)
cv.uber
#Cu un CV de aproximativ 21.95% pentru rentabilitatea Uber, seria este relativ omogenă, iar media rentabilității este reprezentativă pentru setul de date
cv.rentNasdaq<-sd(rentabilitati_firme$Rentabilitate_NASDAQ)/mean(rentabilitati_firme$Rentabilitate_NASDAQ)
cv.nasdaq
#Un CV de aproximativ 8.70% pentru rentabilitatea NASDAQ arată o serie foarte omogenă, iar media rentabilității este extrem de reprezentativă pentru setul de date
cv.rentLyft<-sd(rentabilitati_firme$Rentabilitate_LYFT)/mean(rentabilitati_firme$Rentabilitate_LYFT)
cv.lyft
#Cu un CV de aproximativ 18.57% pentru rentabilitatea Lyft, seria este considerată relativ omogenă și media rentabilității este reprezentativă pentru setul de date.

#coef de asimetrie si boltire/aplatizare
#install.packages("moments")
library(moments)

#asimetrie
sk.uber<-skewness(preturi_actiuni$PRET_UBER)
sk.uber
#O asimetrie pozitivă de 0.461614 indică o distribuție ușor înclinată spre dreapta.
sk.nasdaq<-skewness(preturi_actiuni$PRET_NASDAQ)
sk.nasdaq
#NASDAQ prezintă o asimetrie de 0.2648936, care este, de asemenea, pozitivă și ușor înclinată spre dreapta, dar mai aproape de zero
sk.lyft<-skewness(preturi_actiuni$PRET_LYFT)
sk.lyft
#Lyft are o asimetrie de 1.307459, care este semnificativ pozitivă și indică o distribuție puternic înclinată spre dreapta

# O asimetrie mai mare, cum este cazul Lyft, indică o mai mare variație în prețurile sale și sugerează o volatilitate mai mare,
#posibil asociată cu riscuri mai mari, dar și cu oportunități de câștig sporite pentru investitori. 
#Asimetria mai mică, cum este cazul NASDAQ, sugerează o distribuție mai stabilă și mai predictibilă, 
#care poate fi preferată de investitorii care caută consistență și mai puțin risc.

sk.rentUber<-skewness(rentabilitati_firme$Rentabilitate_UBER)
sk.rentUber
# Rentabilitatea Uber are o asimetrie pozitivă de 0.3930833, ceea ce indică o ușoară înclinare a distribuției spre dreapta
sk.rentNasdaq<-skewness(rentabilitati_firme$Rentabilitate_NASDAQ)
sk.rentNasdaq
#NASDAQ prezintă o asimetrie foarte aproape de zero, și ușor negativă (-0.02959088), indicând o distribuție foarte aproape de simetrie.
sk.rentLyft<-skewness(rentabilitati_firme$Rentabilitate_LYFT)
sk.rentLyft
#Rentabilitatea Lyft arată o asimetrie puternic negativă de -2.192975, indicând o distribuție semnificativ înclinată spre stânga

#boltire/aplatizare
k.uber<-kurtosis(preturi_actiuni$PRET_UBER)
k.uber
#Coeficientul de aplatizare de 2.660349 indică o distribuție platicurtică. Aceasta sugerează că prețurile acțiunilor Uber sunt mai dispersate în jurul mediei.
k.nasdaq<-kurtosis(preturi_actiuni$PRET_NASDAQ)
k.nasdaq
#Cu un coeficient de aplatizare de 2.222052, NASDAQ arată, de asemenea, o distribuție platicurtică. 
k.lyft<-kurtosis(preturi_actiuni$PRET_LYFT)
k.lyft
#Coeficientul de aplatizare de 4.273524 indică o distribuție leptocurtică pentru Lyft.

k.rentUber<-kurtosis(rentabilitati_firme$Rentabilitate_UBER)
k.rentUber
#Un coeficient de aplatizare de 4.776132 indică o distribuție leptocurtică pentru rentabilitatea Uber
k.rentNasdaq<-kurtosis(rentabilitati_firme$Rentabilitate_NASDAQ)
k.rentNasdaq
#un coeficient de aplatizare de 2.879937 sugerează că distribuția rentabilităților NASDAQ este aproape mezocurtică, dar încă ușor plată.
k.rentLyft<-kurtosis(rentabilitati_firme$Rentabilitate_LYFT)
k.rentLyft
#Un coeficient de aplatizare extrem de mare de 19.16337 indică o distribuție puternic leptocurtică pentru rentabilitatea Lyft.

matrice<-matrix(nrow=3, ncol=3, dimnames=list(c("Pret Uber","Pret Nasdaq", "Pret Lyft"), c("Coeficient de asimetrie", "Coeficient de aplatizare", "Coeficient de variatie")))
matrice[,1]<-c(sk.uber, sk.nasdaq, sk.lyft)
matrice[,2]<-c(k.uber, k.nasdaq, k.lyft)
matrice[,3]<-c(cv.uber,cv.nasdaq, k.lyft)
#View(matrice)


#corelatie
matrice_corelatie<-cor(preturi_actiuni[-1])
matrice_corelatie
#Uber are o corelație relativ puternică cu NASDAQ, indicând influențe comune de piață, în timp ce legătura dintre Lyft și NASDAQ este slabă, reflectând
#o independență mai mare față de mișcările generale ale pieței. Relația între Uber și Lyft, deși pozitivă, este modestă.
write.csv(matrice_corelatie, "Matrice corelatie preturi.csv")

matrice_corelatie2<-cor(rentabilitati_firme[-1])
matrice_corelatie2
#Uber și Lyft prezintă o corelație pozitivă moderată, sugerând o influență comună a factorilor de piață asupra rentabilităților lor. În contrast,
#legăturile fiecărei companii cu NASDAQ sunt fie slab negative, fie aproape inexistente.
write.csv(matrice_corelatie2, "Matrice corelatie rentabilitate.csv")
#coeficienti de corelatie mari, peste 0.85 => legaturi directe si puternice

#forma grafica
#install.packages("corrplot")
library(corrplot)
corrplot(matrice_corelatie, method=c("pie"), type=("lower"))
#Uber si NASDAQ: Cercul mare și întunecat sugerează o corelație pozitivă puternică,
#ceea ce este conform cu coeficientul de corelație 
#Uber si Lyft: Cercul este mai mic și mai deschis la culoare decât cel dintre Uber și NASDAQ,
#indicând o corelație pozitivă mai slabă
#NASDAQ și Lyft : Aici vedem o porțiune foarte mică din cerc colorat, sugerând că nu există
#aproape nicio corelație sau o corelație foarte slabă, posibil negativă, între NASDAQ și Lyft,

corrplot(matrice_corelatie2, method=c("pie"), type=("lower"))
#Rentabilitate_Uber și Rentabilitate_NASDAQ: Se observă un cerc mic și parțial umplut, indicând o corelație negativă slabă.
#Rentabilitate_Uber și Rentabilitate_Lyft: Aici, cercul este mai mare și mai umplut decât în cazul corelației dintre Uber și NASDAQ, semnalând o corelație pozitivă moderată. 
#Rentabilitate_NASDAQ și Rentabilitate_Lyft: Cercul este aproape neumplut, semnificând o corelație foarte slabă și posibil negativă între NASDAQ și Lyft

#grafice
#windows()
par(mfrow=c(2,2))
plot(preturi_actiuni$PRET_UBER, main="Evolutia preturilor pt Uber",col="chocolate",type="l" )
plot(preturi_actiuni$PRET_NASDAQ, main="Evolutia preturilor pt NASDAQ",col="magenta",type="l" )
plot(preturi_actiuni$PRET_LYFT, main="Evolutia preturilor pt Lyft",col="green",type="l" )

#Uber: Prețul acțiunilor Uber a urmat o traiectorie crescătoare, 
#ceea ce reflectă o percepție pozitivă a pieței și posibil 
#rezultate financiare bune în acea perioadă. Această tendință 
#indică faptul că investitorii așteaptă ca Uber să continue să 
#performeze bine, având în vedere cererea înaltă pentru serviciile 
#de transport și livrare, în contextul în care economia își revenea după restricțiile pandemice.

#NASDAQ: Creșterea observată în graficul NASDAQ sugerează că întregul sector tehnologic a avut un comportament bun.
#În mod tipic, acesta poate fi un semn al încrederii în inovație și creșterea potențială a companiilor din acest sector.
#Este posibil să fi fost o perioadă de creștere economică generală sau de 
#entuziasm în rândul investitorilor față de companiile tehnologice.

#Lyft: Graficul acțiunilor Lyft prezintă o volatilitate semnificativă, cu variații mari de preț. Aceste oscilații 
#puternice sunt caracteristice pentru o companie care fie trece printr-o perioadă de incertitudine sau schimbare semnificativă, 
#fie reacționează la evenimente sau știri specifice care afectează percepția investitorilor. Schimbările bruște ar putea fi atribuite 
#anunțurilor de rezultate financiare, schimbărilor strategice importante sau poate reacții la modificările condițiilor de piață.

#windows()
par(mfrow=c(2,2))
plot(rentabilitati_firme$Rentabilitate_UBER, main="Evolutia rentabilitatii pt Uber",col="chocolate",type="l" )
plot(rentabilitati_firme$Rentabilitate_NASDAQ, main="Evolutia rentabilitatii pt NASDAQ",col="magenta",type="l" )
plot(rentabilitati_firme$Rentabilitate_LYFT, main="Evolutia rentabilitatii pt Lyft",col="green",type="l" )

#Pentru Uber, vedem variații destul de moderate ale rentabilității zilnice. Nu există oscilații extreme, ceea ce sugerează că
#nu au existat evenimente de piață sau declarații corporative care să producă schimbări drastice în percepția valorii companiei de către investitori.
#În cazul NASDAQ, variația este mai pronunțată. Acest lucru este tipic pentru un indice de piață, deoarece reflectă rezultatul combinat al
#mișcărilor multor acțiuni. Oscilațiile mai mari pot fi atribuite schimbărilor în sentimentul investitorilor față de sectoarele mai largi ale economiei sau reacțiilor la evenimente macroeconomice globale.
#Lyft arată o volatilitate semnificativ mai mare în rentabilitatea zilnică, cu unele zile prezentând variații mari de preț. Aceasta sugerează că
#evenimentele specifice companiei sau factorii externi au avut un impact mai mare asupra percepției investitorilor și a prețului acțiunilor Lyft.

#boxploturi si histograme
#windows()
par(mfrow=c(3,2))
boxplot(preturi_actiuni$PRET_UBER, main="Boxplot Uber", col="pink")
hist(preturi_actiuni$PRET_UBER, main="Histograma pret Uber", col="pink")
boxplot(preturi_actiuni$PRET_NASDAQ, main="Boxplot Nasdaq", col="purple")
hist(preturi_actiuni$PRET_NASDAQ, main="Histograma pret Nasdaq", col="purple")
boxplot(preturi_actiuni$PRET_LYFT, main="Boxplot Lyft", col="aquamarine")
hist(preturi_actiuni$PRET_LYFT, main="Histograma pret Lyft", col="aquamarine")
#Boxplot Uber:
#Mediană în jurul valorii de 40, cu un interval interquartilic (IQR) care pare să fie
# între aproximativ 35 și 45, sugerând că jumătate din prețurile acțiunilor Uber sunt în acest interval.
#Acest boxplot nu prezintă valori extreme indicate, ceea ce sugerează o distribuție mai consistentă a prețurilor acțiunilor

#Boxplot NASDAQ:
#Mediană în jurul valorii de 14000, cu un IQR cuprins între aproximativ 13000 și 15000.
#Acest boxplot pare să nu aibă valori extreme indicate, ceea ce sugerează o distribuție mai consistentă a prețurilor acțiunilor

#Boxplot Lyft:
#Mediană aproape de 12, cu un IQR între 10 și 14, iar distribuția pare a fi destul de simetrică.
#Există numeroase valori extreme indicate de punctele deasupra valorii de 16, sugerând că prețurile acțiunilor Lyft au avut variatii semnificative.

#Histograma Uber:
#Distribuția prețurilor acțiunilor Uber pare să fie aproximativ normală, cu cea mai mare frecvență în jurul clasei de 40-50.
#Există mai puține frecvențe în clasele de preț mai mici (<30) și mai mari (>50), indicând o posibilă asimetrie spre dreapta.

#Histograma NASDAQ:
#Afișează o distribuție care nu este simetrică, cu o frecvență crescută în intervalul 14000-15000.
#Există mai puține frecvențe în intervalele cele mai scăzute și cele mai înalte, ceea ce ar putea sugera o asimetrie sau o distribuție bimodală.

#Histograma Lyft:
#Prezintă o distribuție cu o frecvență mare în intervalul de 10-12 și mai puține frecvențe la capetele distribuției.
#Forma distribuției este neregulată, cu o concentrare a datelor într-o regiune mai îngustă și cu vârfuri înalte, ceea ce sugerează potențial o leptocurticitate.


#windows()
par(mfrow=c(3,2))
boxplot(rentabilitati_firme$Rentabilitate_UBER, main="Boxplot Rent Uber", col="magenta")
hist(rentabilitati_firme$Rentabilitate_UBER, main="Histograma Rent Uber", col="magenta")
boxplot(rentabilitati_firme$Rentabilitate_NASDAQ, main="Boxplot Rent Nasdaq", col="green")
hist(rentabilitati_firme$Rentabilitate_NASDAQ, main="Histograma Rent Nasdaq", col="green")
boxplot(rentabilitati_firme$Rentabilitate_LYFT, main="Boxplot Rent Lyft", col="blue")
hist(rentabilitati_firme$Rentabilitate_LYFT, main="Histograma Rent Lyft", col="blue")

#Boxplot Rentabilitate Uber:
#Mediana este aproape de 0, indicând că valoarea centrală a rentabilității Uber este foarte mică.
#IQR-ul pare să fie foarte îngust, cea mai mare parte a datelor se încadrează într-un interval strâns în jurul medianei, ceea ce sugerează o variabilitate redusă.
#Există câteva valori extreme, ceea ce arată că au fost unele variații semnificative de rentabilitate, dar acestea sunt puține.

#Boxplot Rentabilitate Nasdaq:
#Mediana este ușor peste 0, similar cu Uber, indicând o valoare mediană mică a rentabilității pentru Nasdaq.
#IQR-ul este ceva mai larg decât pentru Uber, ceea ce sugerează o variabilitate mai mare a rentabilităților.
#Valori extreme: Prezența unor valori extreme indică faptul că există rentabilități care se abat considerabil de la cele mai frecvente valori.

#Boxplot Rentabilitate Lyft:
#Mediana este sub 0, indicând o valoare mediană negativă pentru rentabilitatea Lyft.
#IQR-ul este îngust, arătând că majoritatea valorilor sunt concentrate în jurul unei rentabilități negative.
#Există câteva valori extreme, mai multe decât pentru Uber, ceea ce sugerează o volatilitate mai mare în rentabilitate.

#Histograma Rent Uber:
#Distribuția este concentrată în jurul lui zero, cu cea mai mare frecvență aproape de 0.
#Distribuția este ușor înclinată spre dreapta, indicând că există mai multe observații cu rentabilitate ușor pozitivă.

#Histograma Rent Nasdaq:
#Distribuția arată o dispersie mai mare decât Uber, cu valori care se întind pe un interval mai larg.
#Distribuția este relativ simetrică, cu o ușoară înclinare spre dreapta, ceea ce ar putea indica o tendință spre rentabilități pozitive.


#outliers
outliers_Uber <- boxplot(preturi_actiuni$PRET_UBER, plot = FALSE)$out
outliers_Uber
outliers_Nasdaq <- boxplot(preturi_actiuni$PRET_NASDAQ, plot = FALSE)$out
outliers_Nasdaq
outliers_Lyft <- boxplot(preturi_actiuni$PRET_LYFT, plot = FALSE)$out
outliers_Lyft

zile_outliers_Uber <- preturi_actiuni$Date[preturi_actiuni$PRET_LYFT %in% outliers_Uber]
zile_outliers_Uber
zile_outliers_NASDAQ <- preturi_actiuni$Date[preturi_actiuni$PRET_LYFT %in% outliers_Nasdaq]
zile_outliers_NASDAQ
zile_outliers_Lyft <- preturi_actiuni$Date[preturi_actiuni$PRET_LYFT %in% outliers_Lyft]
zile_outliers_Lyft

#Pentru outlierii prețurilor acțiunilor Lyft din ianuarie și februarie 2023, scăderile semnificative au fost puternic 
#influențate de raportul privind câștigurile companiei din trimestrul 4 din 2022, care a fost publicat la începutul lunii februarie. Raportul a indicat previziuni de venituri mai mici decât se așteptau pentru primul trimestru al lui 2023, 
#ceea ce a făcut ca acțiunile să se prăbușească cu peste 30% în urma anunțului. Acest declin brusc a fost exacerbat de evaluările critice ale analiștilor financiari în timpul apelului privind câștigurile,
#ceea ce a erodat și mai mult încrederea investitorilor.
#În decembrie 2023, outlierii pozitivi au fost influențați de raportul de venituri din trimestrul 4 al companiei, care a depășit așteptările. Acest raport, detaliat în februarie 2024,
#a arătat câștiguri și venituri peste așteptări, ceea ce a dus la o creștere a prețurilor acțiunilor.

outliers_rentUber <- boxplot(rentabilitati_firme$Rentabilitate_UBER, plot = FALSE)$out
outliers_rentUber
outliers_rentNasdaq <- boxplot(rentabilitati_firme$Rentabilitate_NASDAQ, plot = FALSE)$out
outliers_rentNasdaq
outliers_rentLyft <- boxplot(rentabilitati_firme$Rentabilitate_LYFT, plot = FALSE)$out
outliers_rentLyft

zile_outliers_rentUber <- rentabilitati_firme$Date[rentabilitati_firme$Rentabilitate_UBER %in% outliers_rentUber]
zile_outliers_rentUber
zile_outliers_rentNASDAQ <- rentabilitati_firme$Date[rentabilitati_firme$Rentabilitate_NASDAQ %in% outliers_rentNasdaq]
zile_outliers_rentNASDAQ
zile_outliers_rentLyft <- rentabilitati_firme$Date[rentabilitati_firme$Rentabilitate_LYFT %in% outliers_rentLyft]
zile_outliers_rentLyft

#Pe 02.05.2023, Uber a anunțat rezultatele pentru primul trimestru al anului 2023, indicând o creștere substanțială în volumele de rezervări și venituri. Această perioadă a arătat o recuperare semnificativă după pandemie, ceea ce a putut
#influența o performanță deosebit de bună a acțiunilor în acea zi.
#Pe 13.10.2023, Uber a raportat performanțe puternice și o creștere semnificativă în segmentul său de mobilitate, posibil generând fluctuații în rentabilitate datorită unor schimbări în modelul de business care au afectat recunoașterea veniturilor și costurilor.
#Pe 02.11.2023, s-a discutat despre performanțele din trimestrul al treilea al lui Uber, notând o creștere continuă și o margină ajustată de EBITDA îmbunătățită, ceea ce poate explica variațiile din rentabilitate.
#Pe 10.02.2023, Lyft a făcut schimbări în conducere, anunțând un nou CEO, ceea ce a putut genera fluctuații în prețul acțiunilor datorită schimbărilor potențiale de strategie și așteptărilor investitorilor.
#Ajustările efectuate de Trezorerie în ceea ce privește emisiunile de datorie și o poziție mai puțin dură din partea Rezervei Federale în această perioadă ar fi putut relaxa condițiile financiare, influențând performanța NASDAQ.







