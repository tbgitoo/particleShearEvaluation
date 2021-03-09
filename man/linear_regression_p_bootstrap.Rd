\name{linear_regression_p_bootstrap}
\alias{linear_regression_p_bootstrap}

\title{
linear_regression_p_bootstrap
}
\description{
Uses pre-generated bootstrapping matrices for evaluating relations between variables by linear regression
}
\usage{
linear_regression_p_bootstrap(x,y,n_agg=5,na.rm=FALSE)
}

\arguments{
  \item{x}{
Independent variable for the linear regression; this should be a vector.
}
  \item{n}{
Bootstrapping data. This is a matrix with as many rows as there are elements in \code{x}, and a possible very large number of columns where the number of columns should be multiple of \code{n_agg}, otherwise the last columns are ignored such as to achieve a number of columns which is a multiple of \code{n_agg}. 
}
\item{n_agg}{Number of columns in \code{y} that should be used together to generate a dataset to be used for the linear model (i.e. passed to \link{lm}).}
  \item{na.rm}{
In random resampling, it may happen that non-evaluatable samples are generated (all values identical, for example). Although generally rare, in large bootstrapping studies, it may be advantages to  pass \code{na.rm=TRUE} so that these rare samples are not taken into account.
}
}
\details{
This function assembles datasets for linear regression, calls \link{lm} for carrying out the linear regression, and returns statistical data averaged over the different runs. This is typically useful to analyze bootstrapping data, where many data sets are drawn from an original, larger dataset. \cr\cr
Technically, the function aims at regression the data in \code{y} against the independent variable \code{x}. For this, datasets are progressively assembled from \code{x} and \code{n_agg} columns of \code{y} at a time. Internally, \code{x} is repeated \code{n_agg} times by concatenation, and associated with the \code{n_agg} columns of \code{y} assembled into a single vector by concatenation. On this dataset, linear regression (aka \link{lm}) is run.\cr\cr 
From each run of linear regression, an F statistics and degrees of freedom of the numerator (DF1) and denominator (DF2) as well as an adjusted r squared value is obtained. The function averages these over the number of linear regression runs; the average P-value is calculated using \link{pf} from these average statistic values and return as the main value.
}

\value{Single numerical P-value. In addition, attributes accessible via \link{attr} are attached:\cr
"F" for the average F-statistics\cr
"DF1" for the number of degrees of freedom in the numerator\cr
"DF2" for the number of degrees of freedom in the denominator\cr
"adj.r.squared" for the adjusted R-squared value as calculated by \link{summary.lm}\cr
"p_shapiro" averaged P-value for normality testing. The averaging here is particular: The P-value returned by \link{shapiro.test} is first converted to a Z value by \link{qnorm}, and the Z values averaged over the linear regression run. The average Z-value is converted back to an average P-value via \link{pnorm}}

\author{
Thomas Braschler
}

\examples{
# Example 1: usage
x=c(0,1,2)

# With a known effect
y=matrix(nrow=length(x),ncol=500,data=x)
y=y+matrix(nrow=dim(y)[1], ncol=dim(y)[2], data=rnorm(dim(y)[1]*dim(y)[2]))
linear_regression_p_bootstrap(x,y,n_agg=5)

# Random effect only
y_control=matrix(nrow=dim(y)[1], ncol=dim(y)[2], data=rnorm(dim(y)[1]*dim(y)[2]))
linear_regression_p_bootstrap(x,y_control,n_agg=5)


# Example 2: 
# =======================================================================================================================

# Use of a more reality-inspired scenario: The problem of evaluation of outcomes from an averaged curve.
# This is inspired from an Arrhenius-type equation, we use here the temperature at half-maximal reaction rate for
# assessing catalyst activity (assuming Arrhenius-type saturation at higher temperature). Same scenario as for the general_linear_regression_p_bootstrap,
# but we will use a transformation to address heteroscedasticity

# T is the known temperature, in Kelvin; T0 the activation energy in temperature equivalents
# Let's assume T0 depends on the addition of some catalyst, this is our x. Let's have the dependency quite extreme
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

v_max = aggregate ( measured_reaction_rate ~ x, measured[measured$T==max(measured$T),],FUN=median,na.rm=TRUE)
# General aggregation per x and T
v = aggregate(measured_reaction_rate ~ x+T,measured,FUN=median,na.rm=TRUE)
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
plot(half_temperature~x, overall_summary,log="y")
# With the log transformation, the standard linear model is at the limit of significance, it depends on the run
summary(lm(log(half_temperature)~x, overall_summary))


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
# With the log transformation for the y (temperature at half-maximum rate), this generally highly significant
linear_regression_p_bootstrap(x,log(y),n_agg=n_agg)

# Example 3
# Basic settings as in example 2, power and analysis
# =======================================

# as before, to get the temperature estimates at half-maximum rate empirically


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

v_max = aggregate ( measured_reaction_rate ~ x, measured[measured$T==max(measured$T),],FUN=median,na.rm=TRUE)
# General aggregation per x and T
v = aggregate(measured_reaction_rate ~ x+T,measured,FUN=median,na.rm=TRUE)



overall_summary=estimate_half_temperature_at_half_maximal_rate(v)



pvals=vector(length=2, mode="numeric")

names(pvals)=c("lm","lm bootstrapping")

pvals["lm"]=coefficients(summary(lm(log(half_temperature)~x, overall_summary)))["x","Pr(>|t|)"]

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

v_subsampled=aggregate(measured_reaction_rate ~ x+T,measured_subsampled,FUN=median,na.rm=TRUE)

half_temperature_estimate=estimate_half_temperature_at_half_maximal_rate(v_subsampled)

y[match(half_temperature_estimate$x,x),ind_bootstrap]=half_temperature_estimate$half_temperature

}

# x is now the regressor values (catalyst concentrations) as wanted

pvals["lm bootstrapping"]=linear_regression_p_bootstrap(x,log(y),n_agg=n_agg,na.rm=TRUE)

return(pvals)

}


# First, the case with a known effect
x=c(0,2,4) # Concentrations catalyst
T0=exp(x) # Let's say, this is an exponential dependency
T=c(1,3,10,30,100,300,1000) # Known temperatures
N_per_condition=15
N_simulation=100


pfirst=get_p_value_estimate(x,T0,T,N_per_condition)

p_matrix=matrix(ncol=length(pfirst),nrow=N_simulation)

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
	current_data=p_matrix[,theApproach]
	current_data=current_data[!is.na(current_data)]
	power[theApproach]=sum(current_data<=0.05)/(length(current_data))
}


# Second, false positives
T0=exp(0*x) # No effect of the catalyst


pfirst=get_p_value_estimate(x,T0,T,N_per_condition)

p_matrix_control=matrix(ncol=length(pfirst),nrow=N_simulation)

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
	current_data=p_matrix_control[,theApproach]
	current_data=current_data[!is.na(current_data)]
	false_positives[theApproach]=sum(current_data<=0.05)/(length(current_data))
}

for_barplot=matrix(nrow=2,ncol=length(false_positives),data=c(false_positives, power),byrow=TRUE)

rownames(for_barplot)=c("False positives (alpha)", "True positives (1-beta, power)")
colnames(for_barplot)=names(false_positives)

barplot(for_barplot,beside=TRUE,legend=TRUE)









}

\keyword{ misc }

