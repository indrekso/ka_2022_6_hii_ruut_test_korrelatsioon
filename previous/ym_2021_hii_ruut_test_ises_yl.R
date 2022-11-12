# ÜIA metodoloogia
# Risttabel ja seosekordajad (hii-ruut-test) 17/12/2021
# Hii-ruut-testi iseseisvate ülesannete lahendused

### Kas haridustase ja poliitikahuvi on omavahel seotud? Kasutan ESS 8. laine andmeid (sama andmestikku kasutasime praktikumis)

# Kodeerime haridustaseme kategooriad ümber (üks võimalik viis, küllaltki laiad kategooriad)

ee8$har <- NA
ee8$har[ee8$edulvlb <= 213] <- "<= Põhiharidus"
ee8$har[ee8$edulvlb > 213 & ee8$edulvlb < 600] <- "Kutseharidus"
ee8$har[ee8$edulvlb == 313] <- "Keskharidus"
ee8$har[ee8$edulvlb >= 600] <- "Kõrgharidus"

library(forcats)
ee8$har <- fct_relevel(ee8$har, "<= Põhiharidus", "Kutseharidus", "Keskharidus", "Kõrgharidus")

# Kontrollime, kas sai õigesti

library(summarytools)
ctable(ee8$edulvlb, ee8$har, prop = "n")

# Teeme andmestiku vajaminevate tunnustega, eemaldame puuduvad väärtused

polhar <- ee8 %>% 
  select(polintr, har, pspwght) %>% 
  na.omit()

# Vaatame tunnuste ühisjaotust risttabelis

ctable(polhar$polintr, polhar$har, weight = polhar$pspwght, useNA = "no", prop = "c", round.digits = 0)

# Kõige väiksem indiviidide arv lahtris on 14. Tegu ei ole küll teoreetilise, vaid tegeliku jaotusega, kuid kui tegelikus jaotuses ei ole väga väikese indiviidide arvuga lahtreid, siis vaevalt neid leidub teoreetilises jaotuses.
# Sisuliseks tõlgendamise pole antud risttabel eriti ülevaatlik, jooniselt peaks võimalik seos kahe tunnuse vahel paremini ilmnema (sama joonise annab ka järgnev funktsioon crosstab, aga kuna osadel teist esines sellega veateade just kaasneva joonisega seoses, siis teeme siin ka ggploti)

library(ggplot2)
ggplot(data = polhar, aes(x = har, weight = pspwght)) +
  geom_bar(aes(fill = as.factor(polintr)), position = position_fill()) +
  xlab("Haridustase") +
  ylab("Osakaal") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_discrete(labels = c("Väga huvitatud", "Üsna huvitatud", "Vähe huvitatud", "Üldse ei tunne huvi"), guide = guide_legend(title = "Huvi poliitika vastu"))

# Tundub küll, et mida kõrgem haridus, seda suurem on poliitikast huvitatute osakaal. Vaatame, mida saame järeldada hii-ruut-statistiku alusel. Kontrollime igaks juhuks üle ka teoreetilise jaotuse. Kui saate veateate, võib abi olla argumendist plot = F (risttabeliga ei kaasne joonist).

crosstab(polhar$polintr, polhar$har, weight = polhar$pspwght, expected = T, chisq = T)

# Nagu eelnevalt arvatud, teoreetilises jaotuses väga väikese sagedusega lahtreid ei esine, seega hii-ruut-statistiku arvutamine ja selle põhjal populatsioonis seose esinemise hindamine on kohane. Hii-ruut-statistiku väärtus on 120,5 (vabadusastmete arv on üheksa), p < 0,001, seega võib öelda, et haridustaseme ja poliitikahuvi vahel esineb seos ja seose esinemist saab kinnitada ka populatsioonis. Kas seos on tugev? Arvutame Crameri V.

library(rcompanion)
polhartab <- crosstab(polhar$polintr, polhar$har, weight = polhar$pspwght)
cramerV(polhartab$tab)

# V = 0,14 ja mõlemas tunnuses on neli kategooriat, mis viitab pigem nõrgemale seosele, kuid seose sisulist tugevust on mõttekam hinnata risttabeli (protsentjaotuse) põhjal.


### Kas haridustase ja viimastel Riigikogu valimistel hääletamine on seotud? Jätke välja valimisõiguseta isikud.

# Teeme andmestiku vajaminevate tunnustega, eemaldame puuduvad väärtused

votehar <- ee8 %>% 
  subset(vote <= 2, select = c(vote, har, pspwght)) %>% 
  na.omit()

# Vaatame tunnuste ühisjaotust risttabelis

ctable(votehar$vote, votehar$har, weight = votehar$pspwght, useNA = "no", prop = "c", round.digits = 0)

# Kõige väiksem indiviidide arv lahtris on 72. Pole võimalik, et teoreetilises sagedusjaotuses esineks lahtreid, kus n < 1 või n <= 5. 
# On selgelt näha, et kõrgharidusega vastajate hulgas on hääletanute osakaal kõrgem kui teiste seas, põhiharidusega ja madalama haridustasemega vastajate hulgas aga teistest oluliselt madalam. Kesk- ja kutseharidusega vastajad on hääletanute osakaalu poolest vahepeal, sealjuures keskharidusega vastajate seas on hääletanuid proportsionaalselt rohkem. 

crosstab(votehar$vote, votehar$har, weight = votehar$pspwght, expected = T, chisq = T)

# Hii-ruut-statistiku väärtus on 205,6 (vabadusastmete arv on kolm), p < 0,001, seega võib öelda, et haridustaseme ja hääletamise vahel esineb seos ja seose esinemist saab kinnitada ka populatsioonis. Funktsioon crosstab võimaldab ka lähemalt uurida, kui palju mingi lahter hii-ruut-statistiku väärtusse panustab. Selleks kuvame tabelis standardiseeritud ruutjäägid (kõigi lahtrite standardiseeritud ruutjääkide summa ongi hii-ruut-statistiku väärtus).

crosstab(votehar$vote, votehar$har, weight = votehar$pspwght, chisq = T, prop.chisq = T)

# Põhilised erinevused tegeliku ja teoreetilise ühisjaotuse vahel tulenevad kõrg- ja põhi- või madalama haridusega vastajate valimistel osalemise jaotusest.

# Kui tugevaks võib leitud seost pidada? Arvutame Crameri V.

library(rcompanion)
votehartab <- crosstab(votehar$vote, votehar$har, weight = votehar$pspwght)
cramerV(votehartab$tab)

# V = 0,34, tegu võiks põhimõtteliselt olla mõõduka seosega, vaatame veel kord ka risttabeleid. Veeruprotsentide põhjal nägime juba enne, et selge erinevus on selle põhjal, kui palju eri haridustasemete esindajatest hääletas:

ctable(votehar$vote, votehar$har, weight = votehar$pspwght, useNA = "no", prop = "c", round.digits = 0)

# Reaprotsendid näitavad samamoodi, et hääletajate hulgas on kõrgharidusega vastajate osakaal palju suurem, mõnevõrra väiksem on kutseharidusega vastajate osakaal ja märkimisväärselt väiksem on põhi- või madalama haridusega vastajate protsent. Võiks öelda, et erinevused on niivõrd selged, et haridustaseme ja valimistel hääletamise seost võiks sisulises mõttes kirjeldada päris tugevana.

ctable(votehar$vote, votehar$har, weight = votehar$pspwght, useNA = "no", prop = "r", round.digits = 0)
