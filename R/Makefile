# test

all: file.md

download: 01_itrdb.R 02_download_CRU.R
	Rscript -e "source('01_itrdb.R'); source('02_download_CRU.R')"

data_prep: download 20_plot_indiv_sat.R 25_create_spatial_field.R
	Rscript -e "source('20_plot_indiv_sat.R'); source('25_create_spatial_field.R')"

setup: 60_MCMC_setup.Rmd
	Rscript -e "library(knitr); knit('60_MCMC_setup.Rmd')"

file.md: 60_MCMC.Rmd
	Rscript -e "library(knitr); NMC=10000; knit('60_MCMC.Rmd'); source('send.done.mail.R')"

clean:
	rm *.ff
	rm mcout.ff.*
