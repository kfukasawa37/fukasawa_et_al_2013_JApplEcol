###Contents of "fukasawaetal2013.Rdata"
#C:Number of captures (1979-2010)
#E:Capture effort (100 Trap-days, 2001-2010(NAs are assigned to 1979-2000))
#nyear=32 (=2010-1979+1)
#estart=23 (=2001-1979+1)
load("fukasawaetal2013.Rdata")
if (!require("R2OpenBUGS")) {
  install.packages("R2OpenBUGS")
}

mongoose.model<-function(){
	tau.ni <- 0.01	#precision of vague priors
	#Observation model (2001 to 2010)
	for (i in estart:nyear) {
		#Likelihood
		C[i] ~ dbin(pc[i], s[i])
		#Weibull catchability model(cloglog transformed)
		mu.cloglog.pc[i] <- c + beta * log(E[i])
		#Temporal fluctuation in catchability
 		cloglog.pc[i] ~ dnorm(mu.cloglog.pc[i], tau.c)
		cloglog(pc[i]) <- cloglog.pc[i]
	}
	#Initial state
	n[1] <- 30
	#Process model
	for (i in 1:nyear) {
		mu.s[i] <- n[i] * r
		s[i] ~ dpois(mu.s[i])
		n[i+1] <- s[i] - C[i]
	}
	#Priors
	log.r ~ dnorm(0, tau.ni)
	log(r) <- log.r
	tau.c ~ dgamma(tau.ni, tau.ni)
	sigma.c <- 1/sqrt(tau.c)
	c ~ dnorm(0, tau.ni)
	beta ~ dunif(0, 100)
}

model.file<-"c:/bugstemp/mongoosemodel.txt"	#file path of temporary file which is sent to OpenBUGS
write.model(mongoose.model,model.file)

data<-list("C","E","nyear","estart")

sinit<-C*3+(1:nyear)^0.2*30
sinit[1]<-NA

inits<-function(){
	list(c=-7,
		log.r=log(1.5),
		beta=1,
		tau.c=10,
		s=round(sinit*1.5)
		)
}

parameters<-c("r","c","beta","n","sigma.c","s")

mongoose.res.bugs<-bugs(data,inits,parameters,model.file,
    n.chains=3, n.iter=2000,n.burnin=1000,n.thin=10000,DIC=F,
    ,working.directory="c:/bugstemp/",debug=T)

save.image("fukasawaetal2013_result.Rdata")
