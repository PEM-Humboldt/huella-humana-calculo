# código para comparar entre versiones

# probar ####

dir_Datos_A<- file.path("datos","datos 2018", "resultados")

Ti18 <- crop(rast(file.path(dir_Datos_A,"Ti_he.tif")), r_base)
Pd18 <- crop(rast(file.path(dir_Datos_A,"Pd_he.tif")), r_base)
#Pd18 <- crop(rast(file.path(dir_Datos_A,"Pd_he_new.tif")), r_base)#### con difererntes categorias
dr18 <- crop(rast(file.path(dir_Datos_A,"dr_he.tif")), r_base)
ds18 <- crop(rast(file.path(dir_Datos_A,"ds_he.tif")), r_base)
lu18 <- crop(rast(file.path(dir_Datos_A,"Lu_he.tif")), r_base)
if18 <- crop(rast(file.path(dir_Datos_A,"if_he_300_100all.tif")), r_base)
bi18 <- crop(rast(file.path(dir_Datos_A,"BI_he.tif")), r_base)
H18 <- crop(rast(file.path(dir_Datos_A,"iheh.tif")), r_base)

### t ####
par(mfcol=c(1,1))

T <- Ti_he -Ti18
T
plot(T)

par(mfcol=c(2,1))
plot(Ti18)
plot(Ti_he)


###p ####

par(mfcol=c(1,1))

P <- Pd_he -Pd18
P
plot(P)

par(mfcol=c(2,1))
plot(Pd18)
plot(Pd_he)


###r ####
par(mfcol=c(1,1))

r <- dr_he -dr18
r
plot(r)

par(mfcol=c(2,1))
plot(dr18)
plot(dr_he)


###s ####
par(mfcol=c(1,1))

s <- ds_he -ds18
s
plot(s)

par(mfcol=c(2,1))
plot(ds18)
plot(ds_he)



###l ####
par(mfcol=c(1,1))

l <- Lu -lu18
l
plot(l)

par(mfcol=c(2,1))
plot(lu18)
plot(Lu)


# if####

par(mfcol=c(1,1))

i <- if_he -if18
i
plot(i)

par(mfcol=c(2,1))
plot(if18)
plot(if_he)


par(mfcol=c(1,1))

b <- bi -bi18
b
plot(b)

par(mfcol=c(2,1))
plot(bi18)
plot(bi)



par(mfcol=c(1,1))

h <- IHEH100 -H18
h
plot(h)

par(mfcol=c(2,1))
plot(IHEH100)
plot(H18)

plot(c(IHEH100, H18), breaks=c(0,15,40,60,101))
plot(H18)

