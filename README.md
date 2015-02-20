# climate-bayes

Estimate a multiproxy state-space model for climate fields.  The main code is in 15_sample_proxy.R.  It fits a kalman smoother to the modern, annual temperature record, using the instrument record and tree ring observations as the measurement equations.

To hopefully simplify the computations, the climate field is specified as a Gaussian Markov Random Field (i.e. the inverse covariance matrix is very sparse).   
