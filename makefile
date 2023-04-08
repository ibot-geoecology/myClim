.PHONY: install-no-vignete, install, build, build-dev remove, generate, generate-source, generate-documentation, generate-html, test, check, check-dev

install-no-vignette:
	R -e 'install.packages(".", repos = NULL)'

install: build
	R -e 'install.packages("../myClim_latest.tar.gz", repos=NULL, build_vignettes=TRUE)'

build:
	R -e 'devtools::build(".", path="../myClim_latest.tar.gz")'

build-dev:
	cd .. && RD CMD build myClim

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
	R --vanilla -e 'devtools::test()'

check:
	R --vanilla -e 'devtools::check()'

check-dev:
	cd .. && RD CMD check --as-cran myClim_*.*.*.tar.gz
