\name{general_linear_regression_p_bootstrap}
\alias{general_linear_regression_p_bootstrap}

\title{
general_linear_regression_p_bootstrap
}
\description{
Evaluation of a bootstrapped (resampled) dataset with a \link{glm} model to obtain a p-value.
}
\usage{
general_linear_regression_p_bootstrap(x,y,n_agg=5,family=gaussian(link = "identity"),...)
}

\arguments{
  \item{x}{
Vector with numerical values associated with experimental conditions (regressor), these are the x-values for the \link{glm} model
}
\item{y}{Matrix obtained by the resampling procedure. y must have as many rows as there are values in x; each column corresponds to a redrawn sample. Sequential blocks of \code{n_agg} columns will be evaluated together in a \link{glm}, so ideally, the number of columns in \code{y} is a multiple of \code{n_agg}. Otherwise only the complete blocks in \code{y} are taken into account. }

  \item{n_agg}{
 Length of the resampling blocks for accross the columns of \code{y}, the will be \code{dim(y)[2]/n_agg} evaluations of the \link{glm}.
}
  \item{family}{
Family argument for the \link{glm} model, see also \link{family}
}

\item{...}{Additional parameters to be passed to \link{glm}}
}

\value{
Numerical p-value for a significant link between \code{y} and \code{x}. In addition, the following attributes are attached to this p-values \code{p}:\cr
\describe{
    \item{\code{attr(p,"F")}}{Average value of the t-statistics squared for the \code{length(y)/n_agg} evaluations of \link{glm}. The t-statistics are obtained from the \link{coefficients} of the summary of the glms (i.e. from \link{summary.glm}. The F-value is the mean of the squared t-values.}
    \item{\code{attr(p,"DF1")}}{Degrees of freedom of the F-statistics. This is 1, since these are obtained from t-values which only have 1 degree of freedom in the numerator }
    \item{\code{attr(p,"DF2")}}{Mean degree of freedom for the residual variance for evaluation of the p-value } 
    \item{\code{attr(p,"adj.r.squared")}}{Adjusted R2 value.  }   
    \item{\code{attr(p,"p_shapiro")}}{Shapiro Wilks P-value for the residuals }
     \item{\code{attr(p,"confint")}}{95\% confidence interval for the regression slope coefficient. In \link{lm}, this is really a slope, in \link{glm} regression, this depends on the link functions, but still, 
     if significant, 0 should not be in this interval }    
    }
}

\details{
Generally, this function is useful in a limited scenario where one has a procedure that best estimates parameters from average curves, but by doing so looses 
information on variability. If there are many x values, then this is less of problem, the issue is more if there are only a few x-values because then despite an originally potentially
very large dataet, only a few x-y pairs result. The result is then that despite a potentially very large dataset used, the standard linear regression (\link{lm}) or even general linear model statistics (\link{glm}) have almost no power to detect even dramatic effects. In that case, subsampling 

}


\author{
Thomas Braschler
}

\keyword{ misc }

\examples{

# Example 1: 
# =======================================================================================================================

# Use here a simplified Arrhenius model for illustration, n_agg =5, 6 different temperatures
# T is the known temperature, in Kelvin; T0 the activation energy in temperature equivalents
# T0 depends on the addition of some catalyst, this is our x
x=c(0,3,6) # Concentrations catalyst
T0=exp(x) # Let's say, this is an exponential dependency
T=c(1,3,10,30,100,300,1000) # Known temperatures
N_per_condition=15
# Theoretical values
theory_reaction_rate = matrix(nrow=length(x),ncol=length(T))
# In reality, we would dispose of measured values with variability rather than theoretical values. Let's say we dispose of 15 values for theoretical value
measured=data.frame(T=vector(mode="numeric",length=0),x=vector(mode="numeric",length=0),measured_reaction_rate=vector(mode="numeric",length=0))
for(ind_x in 1:length(x))
{
	for(ind_T in 1:length(T))
	{
		theory_reaction_rate[ind_x,ind_T]=50*exp(-T0[ind_x]/T[ind_T])
		# Let's say we have normal distribution at log scale
		# Generate a random sample with 15 values
		measurements_random =exp(log(theory_reaction_rate[ind_x,ind_T])+rnorm(N_per_condition))
		measured=rbind(measured,data.frame(T=T[ind_T],x=x[ind_x],measured_reaction_rate=measurements_random))
		
	}
}
plot(measured_reaction_rate ~ T, measured[measured$x==x[1],],pch=21,col="black",bg="black",ylim=c(-10,150))
for(ind_x in 2:length(x))
{
	lines(measured_reaction_rate ~ T, measured[measured$x==x[ind_x],],pch=21,col=palette()[ind_x],bg=palette()[ind_x],type="p")	
}

# For the evaluation, let's first evaluate things globally. Let's say we are interested in knowing for each catalyst concentration x the temperature where the reaction rate is half-maximal. We estimate the maximum reaction rate at the highest temperature:

v_max = aggregate ( measured_reaction_rate ~ x, measured[measured$T==max(measured$T),],FUN=median)
# General aggregation per x and T
v = aggregate(measured_reaction_rate ~ x+T,measured,FUN=median)
for(ind_x in 1:length(x))
{
	lines(measured_reaction_rate ~ T, v[v$x==x[ind_x],],col=palette()[ind_x],type="l")	
}


# For convenience, define here a function that estimates the temperatures for half-maximal reaction rate

estimate_half_temperature_at_half_maximal_rate<-function(v)
{
	x=sort(unique(v$x))
	v_max=v[v$T==max(v$T),]
	
	# For each concentration, to find the temperature where the rate is half-maximal, we go through the temperatures and interpolate between the point just below have maximum and above
	half_temperature = vector(mode="numeric",length=length(x))
	names(half_temperature)=x

	for(ind_x in 1:length(x))
	{
		vx = v[v$x==x[ind_x],]
		first_point_above = min(which(vx$measured_reaction_rate>v_max$measured_reaction_rate[v_max$x==x[ind_x]]/2))
		if(is.infinite(first_point_above))
		{
			half_temperature[as.character(x[ind_x])]=max(vx$T)
		} else {
			if(first_point_above==1)
			{
				half_temperature[as.character(x[ind_x])]=min(vx$T)
			} else {
				half_temperature[as.character(x[ind_x])]=approx(vx$measured_reaction_rate[c(first_point_above-1,first_point_above)],
				vx$T[c(first_point_above-1,first_point_above)],xout=v_max$measured_reaction_rate[v_max$x==x[ind_x]]/2)$y
			}
		}
	
	}
	
	overall_summary = data.frame(x=x, half_temperature=half_temperature)

	return(overall_summary)
	

}

overall_summary=estimate_half_temperature_at_half_maximal_rate(v)





dev.new()

# As one can see, it is quite difficult to infer much from this data. Even though the difference in half-maximal temperature are enormous, the fact that we only have 
# three experimental points does not allow to conclude much
plot(half_temperature~x, overall_summary)
# The linear model is hardly ever significant
summary(lm(half_temperature~x, overall_summary))
# If you run this a couple of times, you will see that the glm with the anticipated variance does better, but still, often misses the dramatic effect
summary(glm(half_temperature~x, overall_summary,family=quasi(link = "log", variance = "mu")))


# That's where bootstrapping can offer a reasonable alternative
n_agg = 3 # Let's generate three subsampling plots
N_total = n_agg*100 # 100 blocks
n_to_sample = round(N_per_condition/n_agg) # Nominal coverage of 1

y=matrix(nrow=length(x),ncol=N_total)

for(ind_bootstrap in 1:N_total)
{

measured_subsampled=measured[FALSE,]

for(ind_x in 1:length(x))
{
	for(ind_T in 1:length(T))
	{
		to_subsample = measured[measured$T==T[ind_T] & measured$x==x[ind_x],]
		measured_subsampled=rbind(measured_subsampled,to_subsample[sample(dim(to_subsample)[1], n_to_sample),])
		
	}
}

v_subsampled=aggregate(measured_reaction_rate ~ x+T,measured_subsampled,FUN=median)

half_temperature_estimate=estimate_half_temperature_at_half_maximal_rate(v_subsampled)

y[match(half_temperature_estimate$x,x),ind_bootstrap]=half_temperature_estimate$half_temperature

}

# x is now the regressor values (catalyst concentrations) as wanted

general_linear_regression_p_bootstrap(x,y,n_agg=n_agg,family=quasi(link = "log", variance = "mu"))





# Example 2: Power (1-beta) and false-positives (alpha) analysis in the setting of example 1
# Basic settings as in example 1
# as before, to get the temperature estimates at half-maximum rate empirically
\dontrun{

estimate_half_temperature_at_half_maximal_rate<-function(v)
{
	x=sort(unique(v$x))
	v_max=v[v$T==max(v$T),]
	
	# For each concentration, to find the temperature where the rate is half-maximal, we go through the temperatures and interpolate between the point just below have maximum and above
	half_temperature = vector(mode="numeric",length=length(x))
	names(half_temperature)=x

	for(ind_x in 1:length(x))
	{
		vx = v[v$x==x[ind_x],]
		first_point_above = min(which(vx$measured_reaction_rate>v_max$measured_reaction_rate[v_max$x==x[ind_x]]/2))
		if(is.infinite(first_point_above))
		{
			half_temperature[as.character(x[ind_x])]=max(vx$T)
		} else {
			if(first_point_above==1)
			{
				half_temperature[as.character(x[ind_x])]=min(vx$T)
			} else {
				half_temperature[as.character(x[ind_x])]=approx(vx$measured_reaction_rate[c(first_point_above-1,first_point_above)],
				vx$T[c(first_point_above-1,first_point_above)],xout=v_max$measured_reaction_rate[v_max$x==x[ind_x]]/2)$y
			}
		}
	
	}
	
	overall_summary = data.frame(x=x, half_temperature=half_temperature)

	return(overall_summary)
	

}



get_p_value_estimate<-function(x,T0,T,N_per_condition)
{

theory_reaction_rate = matrix(nrow=length(x),ncol=length(T))
# In reality, we would dispose of measured values with variability rather than theoretical values. Let's say we dispose of 15 values for theoretical value
measured=data.frame(T=vector(mode="numeric",length=0),x=vector(mode="numeric",length=0),measured_reaction_rate=vector(mode="numeric",length=0))
for(ind_x in 1:length(x))
{
	for(ind_T in 1:length(T))
	{
		theory_reaction_rate[ind_x,ind_T]=50*exp(-T0[ind_x]/T[ind_T])
		# Let's say we have normal distribution at log scale
		# Generate a random sample with 15 values
		measurements_random =exp(log(theory_reaction_rate[ind_x,ind_T])+rnorm(N_per_condition))
		measured=rbind(measured,data.frame(T=T[ind_T],x=x[ind_x],measured_reaction_rate=measurements_random))
		
	}
}

# For the evaluation, let's first evaluate things globally. Let's say we are interested in knowing for each catalyst concentration x the temperature where the reaction rate is half-maximal. We estimate the maximum reaction rate at the highest temperature:

v_max = aggregate ( measured_reaction_rate ~ x, measured[measured$T==max(measured$T),],FUN=median)
# General aggregation per x and T
v = aggregate(measured_reaction_rate ~ x+T,measured,FUN=median)



overall_summary=estimate_half_temperature_at_half_maximal_rate(v)



pvals=vector(length=3, mode="numeric")

names(pvals)=c("lm simple","glm simple","glm bootstrapping")

pvals["lm simple"]=coefficients(summary(lm(half_temperature~x, overall_summary)))["x","Pr(>|t|)"]
pvals["glm simple"]=coefficients(summary(glm(half_temperature~x, overall_summary,family=quasi(link = "log", variance = "mu"))))["x","Pr(>|t|)"]



n_agg = 3 # Let's generate three subsampling plots
N_total = n_agg*100 # 100 blocks
n_to_sample = round(N_per_condition/n_agg) # Nominal coverage of 1

y=matrix(nrow=length(x),ncol=N_total)

for(ind_bootstrap in 1:N_total)
{

measured_subsampled=measured[FALSE,]

for(ind_x in 1:length(x))
{
	for(ind_T in 1:length(T))
	{
		to_subsample = measured[measured$T==T[ind_T] & measured$x==x[ind_x],]
		measured_subsampled=rbind(measured_subsampled,to_subsample[sample(dim(to_subsample)[1], n_to_sample),])
		
	}
}

v_subsampled=aggregate(measured_reaction_rate ~ x+T,measured_subsampled,FUN=median)

half_temperature_estimate=estimate_half_temperature_at_half_maximal_rate(v_subsampled)

y[match(half_temperature_estimate$x,x),ind_bootstrap]=half_temperature_estimate$half_temperature

}

# x is now the regressor values (catalyst concentrations) as wanted

pvals["glm bootstrapping"]=general_linear_regression_p_bootstrap(x,y,n_agg=n_agg,family=quasi(link = "log", variance = "mu"))

return(pvals)

}


# First, the case with a known effect
x=c(0,3,6) # Concentrations catalyst
T0=exp(x) # Let's say, this is an exponential dependency
T=c(1,3,10,30,100,300,1000) # Known temperatures
N_per_condition=15


pfirst=get_p_value_estimate(x,T0,T,N_per_condition)

p_matrix=matrix(ncol=length(pfirst),nrow=100)

p_matrix[1,]=pfirst

colnames(p_matrix)=names(pfirst)

for(ind in 2:(dim(p_matrix)[1]))
{
	p_matrix[ind,]=get_p_value_estimate(x,T0,T,N_per_condition)
	
	cat(paste("Run ", ind, "\n", sep=""))

}


power=pfirst
for(theApproach in names(power))
{
	power[theApproach]=sum(p_matrix[,theApproach]<=0.05)/(dim(p_matrix)[1])
}

# Second, false positives
x=c(0,3,6) # Concentrations catalyst
T0=exp(0*x) # No effect of the catalyst
T=c(1,3,10,30,100,300,1000) # Known temperatures
N_per_condition=15


pfirst=get_p_value_estimate(x,T0,T,N_per_condition)

p_matrix_control=matrix(ncol=length(pfirst),nrow=100)

p_matrix_control[1,]=pfirst

colnames(p_matrix_control)=names(pfirst)

for(ind in 2:(dim(p_matrix_control)[1]))
{
	p_matrix_control[ind,]=get_p_value_estimate(x,T0,T,N_per_condition)
	
	cat(paste("Run ", ind, "\n", sep=""))

}


false_positives=pfirst
for(theApproach in names(false_positives))
{
	false_positives[theApproach]=sum(p_matrix_control[,theApproach]<=0.05)/(dim(p_matrix_control)[1])
}



}






}

