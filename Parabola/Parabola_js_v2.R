

###############################################
# THINGS WE CAN STILL PLAY AROUND WITH:
#
# Interpolating: Do we really need 28 per week?
# currently using 1 per week for easier
# extrapolation.
###############################################


library(rjags)
library(cdcfluview)

setwd('Z:/FluModeling')

dat_N = dat_G = array(NA, dim = c(10, 52*1, 15))

for (i in 1:10) {
	Ns = Gs = NULL

	dat = get_flu_data('hhs', i, 'ilinet', c(2002:2016))

	dat$season = dat$YEAR - 2002
	dat$season = dat$season + (dat$WEEK >= 40)

	for (ss in 1:(max(dat$season)-1)) {
		N = dat[dat$season == ss, 'X..WEIGHTED.ILI']
		N = approx(smooth.spline(cumsum(N)), n = 52*1)$y
	
		G = rep(NA, length(N))
		for (j in 1:length(G)) {
			if (j == 1) {
				G[j] = (N[j+1] - N[j]) / 2
			} else if (j == length(G)) {
				G[j] = (N[j] - N[j-1]) / 2
			} else {
				G[j] = (N[j+1] - N[j-1]) / 2
			}
		}

		Ns = cbind(Ns, N)
		Gs = cbind(Gs, G)
	}

	N = dat[dat$season == max(dat$season), 'X..WEIGHTED.ILI']

	###########################################################
	# CHANGE THIS LINE FOR THE NUMBER OF WEEKS WE HAVE FOR 2016
	#
	N = approx(smooth.spline(cumsum(N)), n = 4*1)$y
	###########################################################
	
	G = rep(NA, length(N))
	for (j in 1:length(G)) {
		if (j == 1) {
			G[j] = (N[j+1] - N[j]) / 2
		} else if (j == length(G)) {
			G[j] = (N[j] - N[j-1]) / 2
		} else {
			G[j] = (N[j+1] - N[j-1]) / 2
		}
	}

	Ns = cbind(Ns, c(N, rep(NA, nrow(Ns)-length(N))))
	Gs = cbind(Gs, c(G, rep(NA, nrow(Gs)-length(G))))

	dat_N[i, , ] = Ns
	dat_G[i, , ] = Gs


}


nobs = dim(dat_N)[3]

moddat = list('nobs' = nobs, 't' = c(1:nobs), 'G' = dat_G, 'N' = dat_N)

mod = jags.model('parabola_hierarchical_model.txt', moddat)


start.time = proc.time()
jags.samples(mod, c('N', 'G'), n.iter = 1000) -> z
tm = proc.time() - start.time
tm[3] / 60
dim(z[[1]])

plot(z[[1]][10, 5, 15, , 1], type = 'l')
summary(z[[1]][10, 5, 15, , 1])

