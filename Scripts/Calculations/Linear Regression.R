plot(mean~risk, data = fantasyproj)
m1=lm(mean~risk, data=fantasyproj)
summary(m1)

plot(mean~numfire_dkcost, data = fantasyproj)
m2=lm(mean~numfire_dkcost, data=fantasyproj)
summary(m2)

plot(Floor~numfire_dkcost, data = fantasyproj)
m3=lm(risk~numfire_dkcost+mean, data=fantasyproj)
summary(m3)

fantasyproj$mean2=fantasyproj$mean^2
fantasyproj$numfire_dkcost2=fantasyproj$numfire_dkcost^2
plot(mean~numfire_dkcost2, data = fantasyproj)
m4=lm(risk~numfire_dkcost+numfire_dkcost2+mean+mean2, data=fantasyproj)
summary(m4)

plot(m3$res~m3$fitted)
plot(m4$res~m4$fitted)
