# ESS7 (2014) ankeedis on küsitud, kui oluliseks peaks pidama erinevaid tegureid, otsustades sisserändajate Eestisse lubamise üle (küsimused D1-D6). 
# Leidke ESS7 andmestikust vastavad tunnused ja uurige nendevahelisi korrelatsioone (eeldame ülesande lihtsustamiseks, et tunnuste vahel on lineaarne seos). 
# Millised tunnused on üksteisega rohkem seotud, millised vähem? 
# Milliseid järeldusi saab sellise analüüsi põhjal teha?

library(essurvey)

set_email("indrek.soidla@ut.ee")

ee7 <- import_country("Estonia", 7)

# Kui eelnev ei tööta, siis saab andmefaili ka ise sisse lugeda (laadisin eelnevalt andmefaili käsitsi ESS-i lehelt alla, pakkisin lahti ja panin allolevale aadressile üles). Funktsioon read_sav paketis haven võimaldab andmed sisse lugeda samas formaadis nagu seda teevad essurvey funktsioonid, samas formaat haven_labelled võib tekitada probleeme. Alternatiiv on kasutada funktsiooni read.spss paketist foreign, mis loeb andmed sisse lihtsalt arvulisel kujul, sellega ei tohiks probleeme ette tulla neil, kel muidu ESS-i andmete sisselugemine paketiga essurvey veateateid tekitas.

library(haven)
ee7 <- read_sav("http://kodu.ut.ee/~indrekso/ym/ESS7EE.sav")

library(foreign)
ee7 <- read.spss("http://kodu.ut.ee/~indrekso/ym/ESS7EE.sav", to.data.frame = TRUE, use.value.labels = FALSE)

# Vaatame, millised tunnused andmestikus on

names(ee7)

# Lähemal vaatlusel võib aimu saada (sh mõnede juba tuttavate küsimuste ja tunnuste paiknemisest ankeedis ja andmestikus), millised on vajalikud tunnused. Selekteerime vastavate nimedega tunnused andmestikust ee7 ja omistame nad objektile imp. Kuna teame, et edaspidi tahame korrelatsioonimaatriksi jaoks kasutada funktsiooni rcorr, muudame objekti juba siin maatriksiks (funktsiooniga as.matrix).
# ESS-i andmete analüüsimisel peaks üldjuhul kasutama ka kaale, aga teeme siinkohal lihtsuse huvides ja eelnevalt läbitehtu harjutamiseks ülesande esmalt läbi ilma kaaludeta.

library(tidyverse)

imp <- ee7 %>% 
  select(qfimedu, qfimlng, qfimchr, qfimwht, qfimwsk, qfimcmt) %>% 
  as.matrix()

# Laeme funktsiooni rcorr kasutamiseks paketi Hmisc ja arvutame korrelatsioonimaatriksi

library(Hmisc)

rcorr(imp)

# Maatriksis olulise info leidmiseks isegi lihtsam on korrelatsioonimaatriksi visualiseerimine

imp_corr <- rcorr(imp)
library(corrplot)
corrplot(imp_corr[[1]], method = "number")

# Näeme, et tunnuste nimed pole joonisel just eriti informatiivsed, need  võiks asendada eestikeelsete nimedega. Corrplot võtab tunnuste nimed korrelatsioonimaatriksi ridade ja veergude nimedest, seega oleks vaja need ära muuta. Sümbolikombinatsiooniga '\n' saab tähistada reavahetust. Sobivad ridade/veergude nimetused on kõige parem sõnastada eestikeelse ankeedi alusel, vt küsimused D1-D6 https://www.yti.ut.ee/sites/default/files/ssi/ess7_eesti_keelne_ankeet_13.08.pdf.

nimed <- c("Hea haridus", "Eesti keele oskus", "Kristlik \ntaust", "Valge \nnahavärv", "Eestile vajalikud \ntööoskused", "Eesti elulaadi \nomaksvõtt")
colnames(imp_corr[[1]]) <- nimed
rownames(imp_corr[[1]]) <- nimed

corrplot(imp_corr[[1]], method = "number")

# Kui soovime ka korrelatsioonikordajate olulisuse tõenäosuse kohta infot, siis tuleb olulisuse tõenäosuse maatriksile lisada samad ridade/veergude nimed, mis korrelatsioonikordajate maatriksil

colnames(imp_corr[[3]]) <- nimed
rownames(imp_corr[[3]]) <- nimed

corrplot(imp_corr[[1]], method = "number", p.mat = imp_corr[[3]], sig.level = 0.05, insig = 'pch', pch.col = 'grey')

# Antud juhul on kõik korrelatsioonikordajad olulisuse nivool 0,05 statistiliselt olulised.

# Näeme, et teineteisega on tugevamalt seotud kristliku tausta olulisus ja valge nahavärvi olulisus, Eestile vajalike tööoskuste olulisus ja hea hariduse olulisus ning Eestile vajalike tööoskuste olulisus ja Eesti elulaadi omaksvõtmise olulisus. Sisuliste järelduste osas saab nt öelda, et mida olulisem on vastajale sisserände üle otsustamisel kristlik taust, seda olulisem on reeglina ka valge nahavärvi olulisus. Võimalik, et mõlemale küsimusele vastamist on mõjutanud positiivsem suhtumine lähedasest kultuuriruumist või isegi geograafiliselt lähemast piirkonnast pärit immigrantide suhtes.

# Ülesande lihtsustamiseks eeldasime, et tunnuste vahel esineb lineaarne seos. Kui me poleks seda eeldanud, vaid tahtnuksime seda ise järele kontrollida (mis oleks mõttekas), oleks seda saanud visuaalse vaatluse põhjal teha nt funktsiooniga splom paketist lattice, funktsiooniga ggpairs paketist GGally või funktsiooniga corrgram paketist corrgram. 

library(lattice)
splom(imp, alpha = 0.02)

library(GGally)
ggpairs(as.data.frame(imp), lower = list(continuous = wrap("smooth", alpha = 0.01))) #NB! Sirgjooni joonistel ei tohiks käsitleda kui tõendeid lineaarse seose olemasolust, jälgida tuleks, kuivõrd langeb sirgjoon kokku seosega, mida näeme punktiparvest joonisel.

library(corrgram)
corrgram(imp, lower.panel=panel.ellipse, upper.panel=panel.shade, text.panel=panel.txt, diag.panel=panel.minmax, main="Erinevate sisserände kriteeriumide olulisus: korrelatsioonimaatriks") 

# Viimane variant annab ka trendiooned, mis põhinevad LOESS-sobitusel, mis on paindlikum, aga vahel isegi liiga, et mingeid järeldusi teha. Vahel võivad need jooned olla päris sinka-vonka, nii et tõlgendamine on keeruline ja pigem võiks neid võtta reservatsioonidega. Antud juhul väga palju need jooned lineaarsest seosest ei erine, nii et ei ole tõenäoliselt häda, kui kasutada Pearsoni korrelatsioonikordajaid (nagu me ka tegime).

# Ühe tunnuspaari puhul võib kasutada ka lihtsalt ggploti:

ggplot(as.data.frame(imp), aes(qfimedu, qfimlng)) +
  geom_point(alpha = 0.01, size = 4) +
  stat_smooth(method = "lm", formula = y ~ x)


# Kuidas võtta arvesse kaale? R-s on erinevates pakettides erinevaid funktsioone, mille funktsionaalsus on hea Pearsoni korrelatsioonikordaja osas, Spearmani osas on keerulisem.

# Pakett weights, funktsioon wtd.cor - arvutab ainult Pearsoni korrelatsioonikordajad

# Kaasame oma andmestikku ka järelstratifitseerimise kaalu tunnuse

library(weights)

imp2 <- ee7 %>% 
  select(qfimedu, qfimlng, qfimchr, qfimwht, qfimwsk, qfimcmt, pspwght)

wtd.cor(imp2[,1:6], weight = imp2$pspwght)

# Ümardamiseks:

round(wtd.cor(imp2[,1:6], weight = imp2$pspwght)[[1]], 2)

# Võib ka proovida, kui suur erinevus EE7-s nende tunnuste puhul kaalutud ja kaalumata korrelatsioonikordajates on:

round(wtd.cor(imp2[,1:6], weight = imp2$pspwght)[[1]] - rcorr(imp)[[1]], 2)

# Erinevust praktiliselt pole (mis ei pruugi tõsi olla mõnede teiste tunnuste puhul, aga ESS Eesti andmed on vähemalt hilisemate lainete osas ka kaalumata küllaltki esinduslikud).

# Pakett weights ei anna võimalust arvutada Spearmani korrelatsionikordajat. Siin võib välja aidata weightedCorr paketis wCorr, aga see laseb korraga arvutada korrelatsiooni vaid kahe tunnuse (mitte maatriksis olevate tunnuste) vahel. Ka ei meeldi sellele funktsioonile andmelüngad. Tulemus tuleb mõnevõrra erinev järgmisest võimalusest, mis võib tuleneda sellest, kuidas algoritm käsitleb võrdseid astakuid.

install.packages("wCorr")
library(wCorr)
imp2a <- na.omit(imp2)
weightedCorr(imp2a$qfimedu, imp2a$qfimlng, weights = imp2a$pspwght, method = "Spearman")

# Mitmete tunnuste vahel Spearmani korrelatsioonikordaja arvutamine on võimalik paketiga expss, aga olulisuse tõenäosuseid siin kaasa ei anta.

install.packages("expss")
library(expss)
round(w_pearson(imp2[1:6], weight = imp2$pspwght), 2)
round(w_spearman(imp2[1:6], weight = imp2$pspwght), 2)

# Siin on paariviisiline andmelünkade eemaldamine vaikeseade, kõigi andmelünkadega indiviidide eemaldamiseks saab kasutada

round(w_spearman(imp2, weight = as.numeric(imp2[,"pspwght"]), use = "complete.obs"), 2)

# Eelnevad andmete kaalumist võimaldavad Spearmani korrelatsioonikordaja funktsioonid ei esita olulisuse tõenäosuseid. Spearmani korrelatsioonikordaja saame ka siis, kui arvutame tunnuste astakud ja rakendame neile Pearsoni korrelatsioonikordaja arvutamise valemit. Seega olulisuse tõenäosuste saamiseks võiksime kaale arvesse võttes arvutada ise tunnuste väärtuste astakud ja anda funktsioonis wtd.cor esimeseks argumendiks astakud, siis saame väljundis Spearmani korrelatsioonikordajate väärtused ja olulisuse tõenäosused:

imp2a$qfimeduRank <- wtd.rank(imp2a$qfimedu, weights = imp2a$pspwght)
imp2a$qfimlngRank <- wtd.rank(imp2a$qfimlng, weights = imp2a$pspwght)
imp2a$qfimchrRank <- wtd.rank(imp2a$qfimchr, weights = imp2a$pspwght)
imp2a$qfimwhtRank <- wtd.rank(imp2a$qfimwht, weights = imp2a$pspwght)
imp2a$qfimwskRank <- wtd.rank(imp2a$qfimwsk, weights = imp2a$pspwght)
imp2a$qfimcmtRank <- wtd.rank(imp2a$qfimcmt, weights = imp2a$pspwght)

wtd.cor(imp2a[, 8:13], weight = imp2a$pspwght)

# Vaatame korrelatsioonikordajate väärtusi ja olulisuse tõenäosusi ümardatult.

# Korrelatsioonikordajad:

wtd.cor(imp2a[, 8:13], weight = imp2a$pspwght)[[1]] %>% 
  round(2)

# Korrelatsioonikordajate olulisuse tõenäosused:

wtd.cor(imp2a[, 8:13], weight = imp2a$pspwght)[[4]] %>% 
  round(2)


# Kõik olulisuse tõenäosused on väga madalad, seetõttu näitab wtd.cor kolme komakoha esitamisel nagu p väärtus oleks täpselt 0. Korrektne oleks siiski esitada olulisuse tõenäosused p < 0,001, sest olulisuse tõenäosus ei saa kunagi olla täpselt 0.

