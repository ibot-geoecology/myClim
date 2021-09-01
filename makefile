.PHONY: install, remove, generate, test

install-gitlab:
	R -e 'devtools::install_gitlab("microclimate_r/microclim", host="git.sorbus.ibot.cas.cz", auth_token="5N6cg1k2TNczNj85xf15")'

install: generate
	R -e 'install.packages(".", repos = NULL)'

remove:
	R -e 'remove.packages("microclim")'

generate:
	Rscript data-raw/source_data_formats.R
	R -e 'devtools::document()'

test:
	R -e 'testthat::test_dir("tests")'
