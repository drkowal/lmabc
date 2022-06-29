#try constraints again

#Given:
x = c(1,2,3,4,5,6,7,8,9,10)
k = c("A","B","C","A","A","C","B","B","A","B")
K = factor(k, levels = c("A","B", "C"))
y = c(234,234,854,235,732,346,147,727,0,142)
plot(x,y,col = K)
x = x - mean(x)

#get formula, pi, df, and dm
pi = c(sum(I(k == "A")),sum(I(k == "B")),sum(I(k == "C"))) / 10
df = data.frame(y,x,K)
dm = cbind(rep(1,10), x,
					 I(k == "A"), I(k == "B"), I(k == "C"),
					 I(k == "A") * x, I(k == "B") * x, I(k == "C") * x)
form = y ~ x + K + K * x

#try baseline encoding:
con_base = rbind(c(1,0,0,0,0,0),
								 c(0,1,0,0,0,0),
								 c(0,0,0,0,0,0),
								 c(0,0,1,0,0,0),
								 c(0,0,0,1,0,0),
								 c(0,0,0,0,0,0),
								 c(0,0,0,0,1,0),
								 c(0,0,0,0,0,1))
Q_base = con_base
XQ_base = dm %*% Q_base
BQ_base = solve(t(XQ_base) %*% XQ_base) %*% t(XQ_base) %*% y
B_base = Q_base %*% BQ_base
fit_base = lm(form, data = df )

cbind(coef(fit_base),BQ_base) # nice :)

#my interpretation of Q:
con_me = rbind(c(0,0,1,0,0,0,0,0),
							 c(0,0,0,1,0,0,0,0),
							 c(pi[1],0,1,0,0,0,0,0),
							 c(pi[2],0,0,0,1,0,0,0),
							 c(pi[3],0,0,0,0,1,0,0),
							 c(0,pi[1],0,1,0,0,0,0),
							 c(0,pi[2],0,0,0,0,1,0),
							 c(0,pi[3],0,0,0,0,0,1))

Q_me = qr.Q(qr(con_me))[,-(1:2)]
XQ_me = dm %*% Q_me
BQ_me = solve(t(XQ_me) %*% XQ_me) %*% t(XQ_me) %*% y
B_me = Q_me %*% BQ_me
fit_abc = lm.abc(form, df)

cbind(coef(fit_abc),B_me) # NICER!!

plot(x,y,col = K)
abline(B_me[1] + B_me[3], B_me[2] + B_me[6], col = palette()[1])
abline(B_me[1] + B_me[4], B_me[2] + B_me[7], col = palette()[2])
abline(B_me[1] + B_me[5], B_me[2] + B_me[8], col = palette()[3])
abline(B_me[1],B_me[2], col = palette()[4])


#OK let's try something absurd:
con_abs = rbind(c(0,0,1,1,1,1,1,1),
								c(0,0,1,1,1,1,1,1),
								c(pi[1],0,1,1,1,1,1,1),
								c(pi[2],0,1,1,1,1,1,1),
								c(pi[3],0,1,1,1,1,1,1),
								c(0,pi[1],1,1,1,1,1,1),
								c(0,pi[2],1,1,1,1,1,1),
								c(0,pi[3],1,1,1,1,1,1))
Q_abs = qr.Q(qr(con_abs))[,-(1:2)]
XQ_abs = dm %*% Q_abs
BQ_abs = solve(t(XQ_abs) %*% XQ_abs) %*% t(XQ_abs) %*% y
B_abs = Q_abs %*% BQ_abs
#it still works...

'''
turns out whatever you put in Q[,-(1:m)], as long as it is spans the full space,
will transform the space into an equivalent minimization problem.

Oh... did you say you wanted to see more variables? I was just thinking the same
thing...
'''

#from lm_abc.R:
n = 200
groups_1 = c("Asian", "Hisp", "NHB", "NHW") # groups
pi_1 = c(0.10, 0.15, 0.20, 0.55) # population proprtions
K_1 = length(groups_1) # number of levels
k_1 = factor(sample(groups_1,
										size  = n, replace = TRUE,
										prob  = pi_1),
						 levels = c("NHW", "Asian", "Hisp", "NHB")) # force NHW as baseline
x1 = rnorm(n = n, mean = 5,  sd = 2) #  not standardized
x2 = x1 + rnorm(n = n, mean = -5,  sd = 1) #  not standardized
cor(x1, x2) # x1, x2 correlated
x1 = x1 - mean(x1)
x2 = x2 - mean(x2)
Ey = 1 + 2*x1 + -2*x2 +
	3*I(k_1==groups_1[1]) -.5*I(k_1==groups_1[2]) +
	.5*I(k_1==groups_1[2])*x1
y  = Ey + rnorm(n=n)


#given this data we may obtain:
pi = c(sum(I(k_1 == "NHW")),sum(I(k_1 == "Asian")),sum(I(k_1 == "Hisp")),sum(I(k_1 == "NHB"))) / n
df = data.frame(y,x1,x2,k_1)
form = y ~ x1 + x2 + k_1 + x1* k_1 + x2 * k_1
Xmat = getFullDesign(formula = form, data = df)
Cmat = rbind(c(0,0,0,1,0,0,0,0,0,0,0,0,0,0,0),
						 c(0,0,0,0,1,0,0,0,0,0,0,0,0,0,0),
						 c(0,0,0,0,0,1,0,0,0,0,0,0,0,0,0),
						 c(pi[1],0,0,1,0,0,0,0,0,0,0,0,0,0,0),
						 c(pi[2],0,0,0,0,0,1,0,0,0,0,0,0,0,0),
						 c(pi[3],0,0,0,0,0,0,1,0,0,0,0,0,0,0),
						 c(pi[4],0,0,0,0,0,0,0,1,0,0,0,0,0,0),
						 c(0,pi[1],0,0,1,0,0,0,0,0,0,0,0,0,0),
						 c(0,pi[2],0,0,0,0,0,0,0,1,0,0,0,0,0),
						 c(0,pi[3],0,0,0,0,0,0,0,0,1,0,0,0,0),
						 c(0,pi[4],0,0,0,0,0,0,0,0,0,1,0,0,0),
						 c(0,0,pi[1],0,0,1,0,0,0,0,0,0,0,0,0),
						 c(0,0,pi[2],0,0,0,0,0,0,0,0,0,1,0,0),
						 c(0,0,pi[3],0,0,0,0,0,0,0,0,0,0,1,0),
						 c(0,0,pi[4],0,0,0,0,0,0,0,0,0,0,0,1))
Q_interp = qr.Q(qr(Cmat))[,-(1:3)]
XQ_interp = Xmat %*% Q_interp
BQ_interp = solve(t(XQ_interp) %*% XQ_interp) %*% t(XQ_interp) %*% y
B_interp = Q_interp %*% BQ_interp
fit_lm = lm(form, df)
fit_abc = lm.abc(form,df)
