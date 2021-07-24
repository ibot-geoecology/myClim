.PHONY: install, remove

install:
	# R -e 'devtools::install_gitlab("microclimate_r/package", host="git.sorbus.ibot.cas.cz", auth_token="enzNvQ7Y8xofEtY8giGY")'
	R -e 'install.packages(".", repos = NULL)'

remove:
	R -e 'remove.packages("microclim")'

generate:
	Rscript data-raw/source_data_formats.R
	R -e 'devtools::document()'
