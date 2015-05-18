# test

all: file.md

setup: 60_MCMC_setup.Rmd
	Rscript -e "library(knitr); knit('60_MCMC_setup.Rmd')"

file.md: 60_MCMC.Rmd
	Rscript -e "library(knitr); NMC=10000; knit('60_MCMC.Rmd'); source('send.done.mail.R')"

clean:
	rm *.ff
	rm mcout.ff.*
