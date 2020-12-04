
# ---------------------------------- #
# R-SCRIPT                           #
# MET4 - EMPIRISKE METODER           #
# NHH NORGES HANDELSHØYSKOLE         #
#                                    #
# MODUL 2: HYPOTESETESTING           #
# ---------------------------------- #

## DEL II: Inferens om en populasjon

library(readxl)
testdata <- read_excel("ovelsesdata.xls")

# t-test
gj.snitt <- mean(testdata$X2)
st.avvik <- sd(testdata$X2)
n        <- nrow(testdata) 

T <- (gj.snitt - 100)/(st.avvik/sqrt(n))
K <- qt(0.95, df = n - 1)

t.test(x = testdata$X2, alternative = "greater", mu = 100)

# Varianstest
s <- sd(testdata$X2)
sigma_0 <- 10
n <- length(testdata$X2)

# Testobservatoren
T <- (n-1)*s^2/sigma_0^2

# Kritiske verdier
L <- qchisq(0.025, df = n - 1)
U <- qchisq(0.975, df = n - 1)

## DEL III: Inferens om to populasjoner

gj.snitt1 <- mean(testdata$X1)
gj.snitt2 <- mean(testdata$X2)

S1 <- sd(testdata$X1)
S2 <- sd(testdata$X2)

n1 <- n2 <- nrow(testdata)

Sp <- sqrt(((n1-1)*S1^2 + (n2-1)*S2^2)/(n1 + n2 - 2))

# Antar lik varians
T1 <- (gj.snitt2 - gj.snitt1)/sqrt(Sp^2*(1/n1 + 1/n2))
K1 <- qt(0.975, df = n1 + n2 - 2)

# Antar ulik varians
T2 <- (gj.snitt2 - gj.snitt1)/sqrt(S1^2/n1 + S2^2/n2)
nu <- (S1^2/n1 + S2^2/n2)^2/((S1^2/n1)^2/(n1-1) + (S2^2/n2)^2/(n2-1))
K2 <- qt(0.975, df = nu)

# De to variantene i t.test()-funksjonen
t.test(testdata$X1, testdata$X2, var.equal = TRUE)
t.test(testdata$X1, testdata$X2, var.equal = FALSE)

# Matchede par
D <- testdata$X2 - testdata$X1

gj.snitt_D <- mean(D)
st.avvik_D <- sd(D)

t.test(D, mu = 0)
t.test(testdata$X1, testdata$X2, paired = TRUE)

# Sammenligning av to varianser
F <- S1^2/S2^2
K <- qf(0.975, n1-1, n2-1)

var.test(testdata$X1, testdata$X2)

# Sammenligning av to andeler

p1_hatt <- mean(testdata$A1)
p2_hatt <- mean(testdata$A2)

p_hatt <- mean(c(testdata$A1, testdata$A2))

## DEL IV: Kjikvadrattester

# Eksempel 15.1 

p0 <- c(0.45, 0.40, 0.15)  # Fordeling under H0
f  <- c(102, 82, 16)       # Observerte frekvenser
e  <- p0*sum(f)            # Forv. frekv. under H0

T  <- sum((f - e)^2/e)     # Testobservator

c  <- qchisq(.95, df = 2)  # Kritisk verdi

# Forkast H0?
T > c

# Eventuelt direkte
chisq.test(x = f, p = p0)

# Test for uavhengighet
immigration <-
  read_xls("immigration-wide.xls",
           range = "B2:E12",
           col_names = c("many", "some", "few", "none"))
chisq.test(immigration)

# Eksempel med antall drap
f <- c(259, 387, 261, 131, 40, 13, 3, 0)
e <- c(264, 376, 267, 127, 45, 13, 3, 1)

sum((f - e)^2/e)
qchisq(.95, df = 7)

# Mer om dette eksempelet i David Spiegelhalters bok
# "The Art of Statistics: Learning from Data"
