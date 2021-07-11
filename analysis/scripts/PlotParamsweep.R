par(mfrow = c(4,4))

plot(P[,1],result[,3],xlab='R0.mag',ylab='infections')
#lines(P[,1],rep(142,times = 1000),col="red",lwd = 3)

plot(P[,2],result[,3],xlab='R0.tim',ylab='infections')
#lines(P[,2],rep(142,times = 1000),col="red",lwd = 3)

plot(P[,3],result[,3],xlab='symp.mag',ylab='infections')
#lines(P[,3],rep(142,times = 1000),col="red",lwd = 3)

plot(P[,4],result[,3],xlab='symp.tim',ylab='infections')
#lines(P[,4],rep(142,times = 1000),col="red",lwd = 3)

plot(P[,5],result[,3],xlab='asymp.adjust',ylab='infections')
#lines(P[,5],rep(142,times = 1000),col="red",lwd = 3)

plot(P[,6],result[,3],xlab='initial.infected',ylab='infections')
#lines(P[,6],rep(142,times = 1000),col="red",lwd = 3)

plot(P[,7],result[,3],xlab='isolate.effectiveness',ylab='infections')
#lines(P[,7],rep(142,times = 1000),col="red",lwd = 3)

plot(P[,8],result[,3],xlab='quarantine.effectiveness',ylab='infections')
#lines(P[,8],rep(142,times = 1000),col="red",lwd = 3)

plot(P[,9],result[,3],xlab='quarantine.contacts',ylab='infections')
#lines(P[,9],rep(142,times = 1000),col="red",lwd = 3)

plot(P[,10],result[,3],xlab='mask.protection',ylab='infections')
#lines(P[,10],rep(142,times = 1000),col="red",lwd = 3)

plot(P[,11],result[,3],xlab='mask.compliance',ylab='infections')
#lines(P[,11],rep(142,times = 1000),col="red",lwd = 3)

plot(P[,10]*P[,11],result[,3],xlab='mask.protection*mask.compliance',ylab='infections')
#lines(P[,10]*P[,11],rep(142,times = 1000),col="red",lwd = 3)

plot(P[,12],result[,3],xlab='cocoon.size',ylab='infections')
#lines(P[,12],rep(142,times = 1000),col="red",lwd = 3)

plot(P[,13],result[,3],xlab='contacts.cocoon',ylab='infections')
#lines(P[,13],rep(142,times = 1000),col="red",lwd = 3)

plot(P[,14],result[,3],xlab='contacts.company',ylab='infections')
#lines(P[,14],rep(142,times = 1000),col="red",lwd = 3)

plot(P[,15],result[,3],xlab='contacts.random',ylab='infections')
#lines(P[,15],rep(142,times = 1000),col="red",lwd = 3)


dev.off()



########################################################################################
par(mfrow = c(3,4))

boxplot(P[,1],result[,2],xlab='R0.mag',ylab='initial test pos')
lines(P[,1],rep(142,times = 1000),col="red",lwd = 3)

plot(P[,2],result[,2],xlab='R0.tim',ylab='initial test pos')
lines(P[,2],rep(142,times = 1000),col="red",lwd = 3)

plot(P[,3],result[,2],xlab='symp.mag',ylab='initial test pos')
lines(P[,3],rep(142,times = 1000),col="red",lwd = 3)

plot(P[,4],result[,2],xlab='symp.tim',ylab='initial test pos')
lines(P[,4],rep(142,times = 1000),col="red",lwd = 3)

plot(P[,5],result[,2],xlab='asymp.adjust',ylab='initial test pos')
lines(P[,5],rep(142,times = 1000),col="red",lwd = 3)

plot(P[,6],result[,2],xlab='initial.infected',ylab='initial test pos')
lines(P[,6],rep(142,times = 1000),col="red",lwd = 3)

plot(P[,7],result[,2],xlab='isolate.effectiveness',ylab='initial test pos')
lines(P[,7],rep(142,times = 1000),col="red",lwd = 3)

plot(P[,8],result[,2],xlab='quarantine.effectiveness',ylab='initial test pos')
lines(P[,8],rep(142,times = 1000),col="red",lwd = 3)

plot(P[,9],result[,2],xlab='quarantine.contacts',ylab='initial test pos')
lines(P[,9],rep(142,times = 1000),col="red",lwd = 3)

plot(P[,10],result[,2],xlab='mask.protection',ylab='initial test pos')
lines(P[,10],rep(142,times = 1000),col="red",lwd = 3)

plot(P[,11],result[,2],xlab='mask.compliance',ylab='initial test pos')
lines(P[,11],rep(142,times = 1000),col="red",lwd = 3)

plot(P[,10]*P[,11],result[,2],xlab='mask.protection*mask.compliance',ylab='initial test pos')
lines(P[,10]*P[,11],rep(142,times = 1000),col="red",lwd = 3)

dev.off()


########################################################################################

boxplot(result[,2])
boxplot(result[,3])

par(mfrow=c(1,2))
hist(summary_out[,2],xlab = "initial test positives", main = NULL)
abline(v=4,col="red",lwd=3)
hist(summary_out[,3],xlab = "infections", main = NULL)
abline(v=146,col="red",lwd=3)
