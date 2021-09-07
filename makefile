.PHONY: install, remove, generate, test, generate-html

install-gitlab:
	R -e 'devtools::install_gitlab("microclimate_r/microclim", host="git.sorbus.ibot.cas.cz", auth_token="5N6cg1k2TNczNj85xf15")'

install: generate
	R -e 'install.packages(".", repos = NULL)'

remove:
	R -e 'remove.packages("microclim")'

generate:
	Rscript data-raw/mc_data_formats.R
	R -e 'devtools::document()'
	R -e 'Rd2md::ReferenceManual()'

test:
	R -e 'testthat::test_dir("tests")'
