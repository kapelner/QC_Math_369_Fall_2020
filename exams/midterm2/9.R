nT = 202
xT = 75
nC = 202
xC = 40

thetahathatT = xT / nT
thetahathatC = xC / nC
thetahathatT / thetahathatC
thetahathatshared = (xT + xC) / (nT + nC)

(thetahathatT - thetahathatC) / sqrt(thetahathatshared * (1 - thetahathatshared) * (1 / nT + 1 / nC))

-1.96 * sqrt(thetahathatshared * (1 - thetahathatshared) * (1 / nT + 1 / nC))
+1.96 * sqrt(thetahathatshared * (1 - thetahathatshared) * (1 / nT + 1 / nC))

-1.96 * sqrt(thetahathatT * (1-thetahathatT) / nT + thetahathatC * (1-thetahathatC) / nC)
+1.96 * sqrt(thetahathatT * (1-thetahathatT) / nT + thetahathatC * (1-thetahathatC) / nC)

(thetahathatT - thetahathatC) - 1.96 * sqrt(thetahathatT * (1-thetahathatT) / nT + thetahathatC * (1-thetahathatC) / nC)
(thetahathatT - thetahathatC) + 1.96 * sqrt(thetahathatT * (1-thetahathatT) / nT + thetahathatC * (1-thetahathatC) / nC)


(1 - thetahathatT) / thetahathatT
thetahathatT / (1 - thetahathatT)

1/2 - 1.96 * 2 / sqrt(nT)
1/2 + 1.96 * 2 / sqrt(nT)

thetahathatT / thetahathatC - 1.96 * sqrt(1 / thetahathatC^2 * thetahathatT * (1-thetahathatT) / nT + thetahathatT^2 / thetahathatC^4 * thetahathatC * (1-thetahathatC) / nC)
thetahathatT / thetahathatC + 1.96 * sqrt(1 / thetahathatC^2 * thetahathatT * (1-thetahathatT) / nT + thetahathatT^2 / thetahathatC^4 * thetahathatC * (1-thetahathatC) / nC)
