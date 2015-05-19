# test
NMC?=10

.Phony: download data_prep setup setup-annual run-annual

all: file.md

download: 01_itrdb.R 02_download_CRU.R
	Rscript -e "source('01_itrdb.R'); source('02_download_CRU.R')"

data_prep: 20_plot_indiv_sat.R 25_create_spatial_field.R
	Rscript -e "source('20_plot_indiv_sat.R'); source('25_create_spatial_field.R')"

setup: 60_MCMC_setup.Rmd

# We need to reinitialize if either the master setup file changes or the 
#   run file changes or if mcout_annual_ff.ffData does not exist
60_MCMC_setup_annual.Rmd mcout_annual_ff.ffData: 60_MCMC_setup.Rmd 60_MCMC.Rmd
	cp 60_MCMC_setup.Rmd 60_MCMC_setup_annual.Rmd
	sed -i .bk 's/mcout_ff/mcout_annual_ff/g' 60_MCMC_setup_annual.Rmd
	Rscript -e "library(knitr); knit('60_MCMC_setup_annual.Rmd')"
	rm *.bk

#mcout_annual_ff.ffData: 60_MCMC_setup.Rmd
#	cp 60_MCMC_setup.Rmd 60_MCMC_setup_annual.Rmd
#	sed -i .bk 's/mcout_ff/mcout_annual_ff/g' 60_MCMC_setup_annual.Rmd
#	Rscript -e "library(knitr); knit('60_MCMC_setup_annual.Rmd')"
#	rm *.bk


#mcout_annual_ff.ffData: setup-annual

run-annual: 60_MCMC_setup_annual.Rmd 60_MCMC.Rmd mcout_annual_ff.ffData
	cp 60_MCMC.Rmd 60_MCMC_annual.Rmd
	sed -i .bk 's/mcout_ff/mcout_annual_ff/g' 60_MCMC_annual.Rmd
	Rscript -e "library(knitr); NMC=$(NMC); knit('60_MCMC_annual.Rmd'); source('send.done.mail.R')"
	rm *.bk
  

clean:
	rm *_ff.*
