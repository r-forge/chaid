
library("CHAID")

load("auto.Rda")

# 1.) Ergebnisse stimmen mit SPSS überein:

# Einzelne Tests für modell, nutzung und tempo
tmpauto1 <- chaid(lease ~ modell, data = auto) #passt
plot(tmpauto1)
print(tmpauto1)

tmpauto2 <- chaid(lease ~ nutzung, data = auto) #passt     
plot(tmpauto2)
print(tmpauto2)

tmpauto3 <- chaid(lease ~ tempo, data = auto) # passt
plot(tmpauto3)
print(tmpauto3)

# Zweier-Kombi-Tests
tmpauto4 <- chaid(lease ~ modell + nutzung, data = auto) # passt
plot(tmpauto4)
print(tmpauto4)

tmpauto5 <- chaid(lease ~ modell + tempo, data = auto) # passt  
plot(tmpauto5)
print(tmpauto5)

tmpauto6 <- chaid(lease ~ nutzung + tempo, data = auto)  # passt
plot(tmpauto6)
print(tmpauto6)

# Dreier-Kombi-Test
tmpauto7 <- chaid(lease ~ nutzung + tempo + modell, data = auto) # passt
plot(tmpauto7)
print(tmpauto7)

# => modell und nutzung und tempo in den datensatz  !



################################################################################
tmpauto8 <- chaid(lease ~ beruf, data = auto) # passt
plot(tmpauto8)
print(tmpauto8)

tmpauto9 <- chaid(lease ~ modell + beruf, data = auto)  # passt
plot(tmpauto9)
print(tmpauto9)

tmpauto10 <- chaid(lease ~ nutzung + beruf, data = auto) # passt nicht (nur zweiter Split nicht) 
plot(tmpauto10)
print(tmpauto10)

tmpauto11 <- chaid(lease ~ modell + nutzung + beruf, data = auto) # passt auch wieder bei 2.split (beruf) nciht
plot(tmpauto11)
print(tmpauto11)


# Tests mit Datensatz kaffee

load("kaffee.Rda")

# 1.) Nur mit klasse als Einflussgröße => Ergebnis stimmt mit SPSS überein

tmpkaffee1 <- chaid(treue ~ klasse, data = kaffee)
plot(tmpkaffee1)
print(tmpkaffee1)

# 2.) Mit Klasse und Einkommen => Ergebnis stimmt mit SPSS überein

tmpkaffee2 <- chaid(treue ~ klasse + einkm, data=kaffee)
plot(tmpkaffee2)
print(tmpkaffee2)

# 3.) Nur nach Preis => Ergebnis stimmt mit SPSS überein

tmpkaffee3 <- chaid(treue ~ preis, data=kaffee)
plot(tmpkaffee3)
print(tmpkaffee3)
           
# 4.) Nach Preis und Einkommen => Ergebnis stimmt mit SPSS überein 

tmpkaffee4 <- chaid(treue ~ einkm + preis, data = kaffee)
plot(tmpkaffee4)
print(tmpkaffee4)

# 5.) Nach Einkommen  => Ergebnis stimmt mit SPSS überein
tmpkaffee5 <- chaid(treue ~ einkm, data = kaffee)
plot(tmpkaffee5)
print(tmpkaffee5)

###############################################################################

# 6.) Nach Preis und Klasse - passt nicht
tmpkaffee6 <- chaid(treue ~ preis + klasse, data = kaffee)
plot(tmpkaffee6)
print(tmpkaffee6)

# 7.) Nach Preis, Klasse und Einkommen - passt auch nicht
tmpkaffee7 <- chaid(treue ~ preis + klasse + einkm, data = kaffee)
plot(tmpkaffee7)
print(tmpkaffee7)

# Also am besten nur Datensatz mit treue als abhängige und preis und einkm als
# Einflussgrößen oder klasse und einkm als EG (preis und klasse zusammen
# funktioniert nicht so gut ...)


# Tests mit Datensatz kredit

load("kredit.Rda")

# 1.) verm+famges; ok
tempkredit <- chaid(kredit ~ verm + famges, data=kredit)
plot(tempkredit)
print(tempkredit)

# 2.) verm; ok
tempkredit <- chaid(kredit ~ verm, data=kredit)
plot(tempkredit)
print(tempkredit)

# 3.) famges; ok
tempkredit <- chaid(kredit ~ famges, data=kredit)
plot(tempkredit)
print(tempkredit)

### orders
load("orders.Rda")
temporders <- chaid(RESP ~ REG + BUSIVOL + FIRMSIZE + SPEC + AGE,
                    data = orders, weights = FREQ)
plot(temporders)
print(temporders)

### Tests on car data set
load("car.Rda")
tempcar <- chaid(CAR ~ buying + doors + persons + lug_boot + safety, data = car)  
plot(tempcar)
print(tempcar)
