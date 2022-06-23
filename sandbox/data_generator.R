random_sum_to_1 = function(leng, m = 100) {
  outpt <- runif(leng, 0, m)
  outpt/sum(outpt)
}

generate_y = function(Xmat, Kmat, a0, a1, b0, b1, interactions, sgma) {
	#current implementation gives random interaction weights

	#add error handling
	#population trend:
	y = a0 + Xmat %*% a1

	n = length(y)
	#level specific trends:
	for (l in 1:ncol(Kmat)) {
		kl = Kmat[,l]
		N_l = length(unique(kl))

		#b0
		for (i in 1:N_l) {
			y = y + b0[[l]][i] * I(kl == letters[i])
		}

		#b1
		for (j in 1:N_l) {
			for (m in 1:ncol(Xmat)) {
				y = y + b1[[m]][[l]][j] * I(kl == letters[j]) * Xmat[,m]
			}
		}
	}

	#interactions
	for (m in interactions) {
		x = m[1]
		y = m[2]
		N_x = length(unique(x))
		N_y = length(unique(y))
		for (i in 1:N_x) {
			for (j in 1:N_y) {
				yold = y
				y = y + floor(rgamma(1,1,1/2)) * I(Kmat[,x] == letters[i]) * I(Kmat[,y] == letters[j])
			}
		}
	}

	#random noise
	y = y + rnorm(n,0,sgma)

	return(y)
}

test_func = function() {
	testt = generate_data(100000,2,4,c(0,100),c(3,4,5,6))

	b0 = list(c(-10,2,10),c(-20,-15,10,30), c(-10,-5,-3,5,10), c(1,2,3,-4,5,6))
	b1 = list(
		list(c(-10,0,10),c(-20,-15,10,30), c(-10,-5,0,5,10), c(1,2,3,4,5,6)),
		list(c(-20,0,15),c(-20,-5,10,30), c(-10,0,0,15,10), c(1,2,-3,4,-5,-6)))
	interactions = list(c(1,2),c(3,4))
	#interactions = list()
	y_testt = generate_y(testt[[1]],testt[[2]], 20, c(5,10), b0, b1, interactions = interactions,25)

	x = testt[[1]][,1]
	y = testt[[1]][,2]
	k1 = testt[[2]][,1]
	k2 = testt[[2]][,2]
	k3 = testt[[2]][,3]
	k4 = testt[[2]][,4]
	dta = data.frame(y_testt,x,y,k1,k2,k3,k4)

	form2 = y_testt ~ x + y + k1 + k2 + k3 + k4 +
		x * k1 + x * k2 + x * k3 + x * k4 +
		y * k1 + y * k2 + y * k3 + y * k4 +
		k1 * k2 + k3 * k4
	fit = lm.abc(formula = form2, data = dta)
	fit_lm = lm(formula = form2, data = dta)
	return(dta)
}

generate_data = function(n,n_cont, n_cat, cont_range, cat_lengths) {
	if (length(cat_lengths) != n_cat) {
		stop("cat_lengths should be a vector of length n_cat")
	}

	Xmat = matrix(data=rep(0,n*n_cont), nrow=n, ncol=n_cont)
	for (i in 1:n_cont) {
		xi = runif(n=n, min = cont_range[1], max = cont_range[2])
		Xmat[,i] = xi
	}

	Kmat = matrix(data=rep(0,n*n_cat), nrow=n, ncol=n_cat)
	longest = max(cat_lengths)
	probs_list = c()
	for (i in 1:n_cat) {
		cat_list = letters[1:cat_lengths[i]]
		probs = random_sum_to_1(cat_lengths[i])
		ki = sample(cat_list,n, replace = TRUE, prob = probs)
		Kmat[,i] = as.vector(ki)
		probs_list = qpcR:::cbind.na(probs_list, probs)
	}
	return(list(Xmat,Kmat,probs_list[,-1]))
}

i <- 1
count2 <- 0

while(i <= 1000) {
	temp <- try(test_func())
	if(inherits(temp,"try-error"))
		count2 <- count2 + 1
	if(i %% 100 == 0) {
		print(i)
	}
	i = i + 1
}
