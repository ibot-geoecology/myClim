.PHONY: install, remove

install:
	R -e 'devtools::install_gitlab("microclimate_r/package", host="git.sorbus.ibot.cas.cz", auth_token="enzNvQ7Y8xofEtY8giGY")'

remove:
	R -e 'remove.packages("microclim")'
