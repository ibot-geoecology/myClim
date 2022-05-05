.PHONY: install, build, remove, generate, test, generate-html, generate-documentation, generate-source, install-no-vignette

install-gitlab:
	R -e 'devtools::install_gitlab("microclimate_r/microclim", host="git.sorbus.ibot.cas.cz", auth_token="5N6cg1k2TNczNj85xf15")'

install-no-vignette:
	R -e 'install.packages(".", repos = NULL)'

install: build
	R -e 'install.packages("../myClim_latest.tar.gz", repos=NULL, build_vignettes=TRUE)'

build:
	R -e 'devtools::build(".", path="../myClim_latest.tar.gz")'

remove:
	R -e 'remove.packages("myClim")'

generate: generate-source generate-documentation

generate-source:
	for filename in data-raw/mc_data_*.R; do Rscript "$$filename"; done
	$(RM) NAMESPACE

generate-documentation:
	R -e 'devtools::document()'

generate-html: generate-documentation
	R -e 'pkgdown::build_site(override = list(destination = "../docs"))'

test:
	R --vanilla -e 'testthat::test_dir("tests")'
