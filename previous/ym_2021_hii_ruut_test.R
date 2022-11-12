# ÜIA metodoloogia
# Risttabel ja seosekordajad (hii-ruut-test) 17/12/2021

# Näide: kas mehed ja naised eelistavad parlamendivalimistel (täpsemalt: 2015. aasta Riigikogu valimistel) erinevaid erakondi?

# Loeme ESS-i 8. laine andmed sisse

library(essurvey)
set_email("indrek.soidla@ut.ee")
ee8 <- import_country("Estonia", 8)

# või

library(haven)
ee8 <- read_sav("http://kodu.ut.ee/~indrekso/ym/ESS8EE.sav")

# või

library(foreign)
ee7 <- read.spss("http://kodu.ut.ee/~indrekso/ym/ESS8EE.sav", to.data.frame = TRUE, use.value.labels = FALSE)


library(summarytools)
ctable(ee8$prtvtfee, ee8$gndr, weight = ee8$pspwght)

# Saab ka nii, et iga tunnuse juures andmestiku nime ee8$ ei pea ette andma
with(ee8, ctable(prtvtfee, gndr, weight = pspwght))

# Vaatame kategooriate nimetusi

attr(ee8$prtvtfee, "labels")

# Ka kõigi väikeparteide poolt antud hääletajad on eraldi, paneme osad kategooriad kokku, et jaotus oleks ülevaatlikum ja vastaks hii-ruut-testi eeldustele

ee8$prtvtfee2 <- NA
ee8$prtvtfee2[ee8$prtvtfee == 1] <- "RE"
ee8$prtvtfee2[ee8$prtvtfee == 2] <- "KE"
ee8$prtvtfee2[ee8$prtvtfee == 3] <- "IRL"
ee8$prtvtfee2[ee8$prtvtfee == 4] <- "SDE"
ee8$prtvtfee2[ee8$prtvtfee == 6] <- "EKRE"
ee8$prtvtfee2[ee8$prtvtfee == 5 | ee8$prtvtfee == 9 | ee8$prtvtfee == 10 | ee8$prtvtfee == 12 | ee8$prtvtfee == 13] <- "VP"
ee8$prtvtfee2[ee8$prtvtfee == 11] <- "VE"

# Reastame parteid häälesaagi järgi

library(forcats)
ee8$prtvtfee2 <- fct_relevel(ee8$prtvtfee2, "RE", "KE", "SDE", "IRL", "VE", "EKRE", "VP")

# Tutvume ka tunnuste ühisjaotusega lähemalt

ctable(ee8$prtvtfee2, ee8$gndr, weight = ee8$pspwght, useNA = "no", prop = "r", round.digits = 0)
ctable(ee8$prtvtfee2, ee8$gndr, weight = ee8$pspwght, useNA = "no", prop = "c", round.digits = 0)
ctable(ee8$prtvtfee2, ee8$gndr, weight = ee8$pspwght, useNA = "no", prop = "t", round.digits = 0)

# Hii-ruut-statistiku arvutamiseks kasutame paketist descr funktsiooni crosstab, sest see annab ka teoreetilise jaotuse

install.packages("descr")
library(descr)

crosstab(ee8$prtvtfee2, ee8$gndr, weight = ee8$pspwght, expected = T)

# Arvutame tegelike ja teoreetiliste sageduste põhjal ise hii-ruut-statistiku. Vaatame, millistes risttabeli elementides on tegelik ja teoreetiline jaotus

prt_gndr <- crosstab(ee8$prtvtfee2, ee8$gndr, weight = ee8$pspwght, chisq = T, expected = T, plot = F)
View(prt_gndr)

# Kontrollime üle

prt_gndr$CST$observed
prt_gndr$CST$expected

# Arvutame tegeliku ja teoreetilise jaotuse põhjal lihtjäägid

prt_gndr$CST$observed - prt_gndr$CST$expected

# Arvutame ruutjäägid

(prt_gndr$CST$observed - prt_gndr$CST$expected)**2

# Arvutame standardiseeritud ruutjäägid

(prt_gndr$CST$observed - prt_gndr$CST$expected)**2 / prt_gndr$CST$expected

# Arvutame standardiseeritud ruutjääkide summa ehk hii-ruut-statistiku

sum((prt_gndr$CST$observed - prt_gndr$CST$expected)**2 / prt_gndr$CST$expected)

# Kontrollime, kas saime õige tulemuse

crosstab(ee8$prtvtfee2, ee8$gndr, weight = ee8$pspwght, expected = T, plot = F, chisq = T)

# Arvutame Crameri V. Võime seda ise teha või kasutada funktsiooni

# Ise:

sqrt(13.465/(1178*(2 - 1)))

# Alternatiiv:

hrtest <- crosstab(ee8$prtvtfee2, ee8$gndr, weight = ee8$pspwght, expected = T, plot = F, chisq = T)

View(hrtest) # näeme, et hii-ruut-testi väärtus on elemendis hrtest$CST$statistic ja indiviidide arv elemendis hrtest$total.n

sqrt(hrtest$CST$statistic/(hrtest$total.n*(2 - 1)))

# Funktsiooniga paketis rcompanion (funktsiooni ainsaks argumendis tuleb anda risttabel kahe tunnuse tegeliku sagedusjaotusega):

library(rcompanion)

View(hrtest) # näeme, et tunnuste tegelik sagedusjaotus (indiviidide arvud) on elemendis tab

cramerV(hrtest$tab)