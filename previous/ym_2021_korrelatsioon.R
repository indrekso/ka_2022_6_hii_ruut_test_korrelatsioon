# ÜIA metodoloogia
# Korrelatsioonanalüüsi praktikum 17/12/2021

punktid <- read.csv2("http://kodu.ut.ee/~indrekso/punktid.csv")
punktid

# Kas õpilaste füüsika ja inglise keele testide hinnete vahel esineb korrelatsioon?
library(ggplot2)
ggplot(punktid, aes(fyysika, inglise)) +
  geom_point()

# Kui tugev seos testitulemuste vahel esineb?

cor(punktid$fyysika, punktid$inglise)

# Kuidas korrelatsioonikordaja saadi? Teeme arvutused ise läbi, et seda paremini mõista. Hoiame ka korrelatsioonikordaja arvutamise valemi endal silme ees.

install.packages("imager")
library(imager)
valem_cov_corr <- load.image("http://kodu.ut.ee/~indrekso/cov_corr.png")
plot(valem_cov_corr, axes = F)

# Arvutame iga õpilase kohta tema hinnete erinevused hinnete aritmeetilistest keskmistest

mean(punktid$fyysika)
mean(punktid$inglise)
punktid$fyysika_erin <- punktid$fyysika - mean(punktid$fyysika)
punktid$inglise_erin <- punktid$inglise - mean(punktid$inglise)
punktid

# Arvutame füüsika ja inglise keele hinnete standardhälbed

fyysika_sth <- sqrt(sum(punktid$fyysika_erin**2)/nrow(punktid))
fyysika_sth
inglise_sth <- sqrt(sum(punktid$inglise_erin**2)/nrow(punktid))
inglise_sth

# Standardiseerime iga õpilase hinde erinevuse keskmisest - jagame iga õpilase puhul füüsika hinde erinevuse läbi füüsika hinnete standardhälbega ja inglise keele hinde erinevuse jagame läbi inglise keele hinde standardhälbega

punktid$fyysika_erin_st <- punktid$fyysika_erin / fyysika_sth
punktid$inglise_erin_st <- punktid$inglise_erin / inglise_sth
round(punktid, 2)

# Uurime, kuidas hinnete erinevused aritmeetilisest keskmisest ja nende erinevuste standardiseeritud väärtused üksteisest erinevad. Kasutame selleks paketti ggplot ja lisaks ka plotly, mis ggplotiga saadud joonise interaktiivseks muudab 

fyys_ingl <- ggplot(punktid, aes(fyysika, inglise)) +
  geom_point() +
  geom_vline(aes(xintercept = mean(fyysika))) +
  geom_hline(aes(yintercept = mean(inglise))) +
  stat_smooth(method = "lm", formula = y ~ x, se = F) +
  annotation_raster(valem_cov_corr, xmin=5, xmax=10, ymin=40, ymax=60)

library(plotly)
ggplotly(fyys_ingl)

fyys_ingl_st <- ggplot(punktid, aes(fyysika_erin_st, inglise_erin_st)) +
  geom_point() +
  geom_vline(aes(xintercept = mean(fyysika_erin_st))) +
  geom_hline(aes(yintercept = mean(inglise_erin_st))) +
  stat_smooth(method = "lm", formula = y ~ x, se = F)
ggplotly(fyys_ingl_st)

# Tuletame meelde korrelatsioonikordaja valemi

plot(valem_cov_corr, axes = F)

# Arvutame standardiseeritud hindeerinevuste korrutise

punktid$erin_st_korrutis <- punktid$fyysika_erin_st * punktid$inglise_erin_st
round(punktid, 2)

# Korrutiste summa on aluseks korrelatsioonikordajale - mida suurem on korrutiste summa, seda suurem on korrelatsioonikordaja ja tugevam tunnuste (antud juhul kahe testi hinnete) vaheline korrelatsioon. Need hinnete paarid, mille puhul korrutise väärtus on kõrgem, panustavad korrelatsioonseose tugevusse rohkem. Vaatame seda ka graafiliselt, millised need hinnete paarid on.

fyys_ingl_st_korrutis <- ggplot(punktid, aes(fyysika_erin_st, inglise_erin_st, color = erin_st_korrutis)) +
  geom_point() +
  geom_vline(aes(xintercept = mean(fyysika_erin_st))) +
  geom_hline(aes(yintercept = mean(inglise_erin_st))) +
  stat_smooth(method = "lm", formula = y ~ x, se = F) +
  scale_color_gradient(low="white", high="blue")
ggplotly(fyys_ingl_st_korrutis)

# Viime lineaarse korrelatsioonikordaja arvutuse lõpule.

sum(punktid$erin_st_korrutis)
sum(punktid$erin_st_korrutis)/nrow(punktid)

# Kasutades cor funktsiooni, saame täpselt sama tulemuse

cor(punktid$fyysika, punktid$inglise)

# Praegune näide põhineb genereeritud andmetel, aga kui tegu oleks juhuvalimiga mingist suuremast hulgast testidest/õpilastest, siis tulemuse üldistatavut populatsioonile / seose esinemist populatsioonis saaks hinnata j?ägmise funktsiooniga:

cor.test(punktid$fyysika, punktid$inglise)

# Kuna korrelatsioonikordaja väärtus on kõrge, siis on seos olulisuse nivool 0,05 statistiliselt oluline, hoolimata väikesest valimist. Oluline on siiski tähele panna, et tänu väikesele valimile on korrelatsioonikordaja usaldusvahemik väga lai.


### Spearmani korrelatsioonikordaja ###

punktid3 <- read.csv2("http://kodu.ut.ee/~indrekso/punktid.csv")

plfm <- ggplot(punktid3, aes(fyysika, matemaatika)) +
  geom_point() +
  geom_vline(aes(xintercept = mean(fyysika))) +
  geom_hline(aes(yintercept = mean(matemaatika))) +
  stat_smooth(method = "lm", formula = y ~ x, se = F) +
  xlab("Füüsikahinne") +
  ylab("Matemaatikahinne")
ggplotly(plfm)

# Arvutame astakud

punktid3$fyysika_astak <- rank(punktid3$fyysika, ties.method = "average")
punktid3$matemaatika_astak <- rank(punktid3$matemaatika, ties.method = "average")

fyys_matem_astak <- ggplot(punktid3, aes(fyysika_astak, matemaatika_astak)) +
  geom_point() +
  geom_vline(aes(xintercept = mean(fyysika_astak))) +
  geom_hline(aes(yintercept = mean(matemaatika_astak))) +
  geom_abline(slope = 1)
ggplotly(fyys_matem_astak)

# Tuletame meelde Spearmani korrelatsioonikordaja valemi

valem_sp_corr <- load.image("http://kodu.ut.ee/~indrekso/sp_corr.png")
plot(valem_sp_corr, axes = F)

# Mida suurem on astakute vahe ruutude summa, seda suurem tuleb liige, mille lahutame ühest ja seda nõrgem on korrelatsioonseos kahe tunnuse (õigemini nende astakute) vahel. Seega, mida väiksem on astakute ruutude summa, seda tugevam on seos. Need astakute paarid, mille puhul astakute vahe ruut on väiksem, panustavad korrelatsioonseose tugevusse rohkem. Arvutame iga indiviidi kohta astakute vahe ruudu ja vaatame joonisel, tänu milliste õpilaste hinnetele ilmneb võimalikult tugev seos.

punktid3$astak_vaheruut <- (punktid3$fyysika_astak - punktid3$matemaatika_astak)**2

fyys_matem_astak_vaheruut <- ggplot(punktid3, aes(fyysika_astak, matemaatika_astak, colour = astak_vaheruut)) +
  geom_point() +
  geom_vline(aes(xintercept = mean(fyysika_astak))) +
  geom_hline(aes(yintercept = mean(matemaatika_astak))) +
  geom_abline(slope = 1) +
  scale_color_gradient(low = "blue", high = "white")
ggplotly(fyys_matem_astak_vaheruut)

# Arvutame Spearmani korrelatsioonikordaja

1 - (6*sum(punktid3$astak_vaheruut)) / (nrow(punktid3)*(nrow(punktid3)**2 - 1))


# Kontrollime üle, kas saime õige tulemuse

cor(punktid3$fyysika, punktid3$matemaatika, method = "spearman")

# Kui tegu oleks juhuvalimiga, siis saaks vastavat statistilist hüpoteesi kontrollida jälle funktsiooniga cor.test:

cor.test(punktid3$fyysika, punktid3$matemaatika, method = "spearman")

# Pangem tähele, et lineaarne korrelatsioonikordaja annaks siin erineva tulemuse, sest seos kahe tunnuse vahel ei ole lineaarne:

cor(punktid3$fyysika, punktid3$inglise)


# Mitme tunnuse vaheliste korrelatsioonikordajate arvutamiseks ja esitamiseks korrelatsioonimaatriksina saab kasutada funktsiooni cor:

cor(punktid3[,1:3])

cor(punktid3[,1:3], method = "spearman")

# Samal viisil olulisuse tõenäosuste arvutamiseks on vaja siiski teist funktsiooni:

install.packages("Hmisc")
library(Hmisc)

rcorr(as.matrix(punktid3[,1:3]))

rcorr(as.matrix(punktid3[,1:3]), type = "spearman")

# Vahel annab lihtsamalt ülevaate korrelatsioonimaatriksi visuaalselt esitamine

install.packages("corrplot")
library(corrplot)

punktid3$eesti <- c(60, 98, 89, 29, 56, 45, 35, 26, 58, 6)
punktid3$laulmine <- c(4, 7, 9, 5, 10, 12, 22, 35, 59, 87)
punktid3_rcorr <- rcorr(as.matrix(punktid3[,c("fyysika", "inglise", "matemaatika", "eesti", "laulmine")]), type = "spearman")

corrplot(as.matrix(punktid3_rcorr[[1]]), method = "circle")
corrplot(as.matrix(punktid3_rcorr[[1]]), method = "pie")
corrplot(as.matrix(punktid3_rcorr[[1]]), method = "color")
corrplot(as.matrix(punktid3_rcorr[[1]]), method = "number")

# Saab ka lisada info statistilise olulisuse kohta - argumendiga p.mat (argumendi väärtuseks panna olulisuse tõenäosuste maatriksi asukoht), lisaks saab täpsustada olulisuse nivoo jm üksikasjad

corrplot(as.matrix(punktid3_rcorr[[1]]), method = "number", p.mat = punktid3_rcorr[[3]], sig.level = 0.05, insig = 'pch', pch.col = 'grey')

### Näited küsitlusuuringu andmetega (kui aega jääb, siis iseseisvalt tegemiseks, kui mitte, siis koos)

# ESS7 (2014) ankeedis on küsitud, kui oluliseks peaks pidama erinevaid tegureid, otsustades sisserändajate Eestisse lubamise üle (küsimused D1-D6). 
# Leidke ESS7 andmestikust vastavad tunnused ja uurige nendevahelisi korrelatsioone. 
# Ülesande lihtsustamiseks me
#  - eeldame, et tunnuste vahel on lineaarne seos
#  - ei võta arvesse kaale. 

# Millised tunnused on üksteisega rohkem seotud, millised vähem? 
# Milliseid järeldusi saab sellise analüüsi põhjal teha?