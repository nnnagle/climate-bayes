# climate-bayes

Estimate a multiproxy state-space model for climate fields.  The main code is in 15_sample_proxy.R.  It fits a kalman smoother to the modern, annual temperature record, using the instrument record and tree ring observations as the measurement equations.

The instrument record is modeled as temperature plus i.i.d.nnoise.

The tree ring chronologies are modeled as intercept plus slope*temperature plus i.i.d. noise,
with different intercept and slope coefficients for each tree species.

To hopefully simplify the computations, the temperature field is specified as a Gaussian Markov Random Field (i.e. the inverse covariance matrix is very sparse).   

Concerns: the temperature is simulated on a rough lattice, and the temperature value at each instrument and chronology location is interpolated.  I am concerned that the interpolation introduces measurement error and that the coefficients are thus biased toward zero.
