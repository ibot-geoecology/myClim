.PHONY: install, remove, generate, test, generate-html

install-gitlab:
	R -e 'devtools::install_gitlab("microclimate_r/microclim", host="git.sorbus.ibot.cas.cz", auth_token="5N6cg1k2TNczNj85xf15")'

install: generate
	R -e 'install.packages(".", repos = NULL)'

remove:
	R -e 'remove.packages("myClim")'

generate:
	Rscript data-raw/mc_data_formats.R
	Rscript data-raw/mc_data_sensors.R
	Rscript data-raw/mc_data_physical.R
	Rscript data-raw/mc_data_vwc_parameters.R
	$(RM) NAMESPACE
	R -e 'devtools::document()'
	R -e 'Rd2md::ReferenceManual()'

generate-html:
	R -e 'pkgdown::build_site()'

test:
	R --vanilla -e 'testthat::test_dir("tests")'
