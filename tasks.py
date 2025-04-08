from pathlib import Path
from invoke import task

def generate_documentation(c):
    c.run(f"R -e 'devtools::document()'")

@task
def build_dev(c):
    c.run("""cd .. && RD CMD build myClim""")

@task
def generate(c):
    """
    Generate source and documentation
    """
    files = [x for x in Path("data-raw").glob("mc_data_*.R") if x.name != "mc_data_examples.R"]
    for data_file in files:
        c.run(f"Rscript {str(data_file)}", pty=True, echo=True)
    c.run(f"Rscript data-raw/mc_data_examples.R", pty=True, echo=True)
    Path("NAMESPACE").unlink(missing_ok=True)
    generate_documentation(c)

@task
def generate_html(c):
    generate_documentation(c)
    c.run("""R -e 'pkgdown::build_site(override = list(destination = "../docs"))'""")

@task
def check(c):
    c.run("""R --vanilla --no-multiarch -e 'devtools::check()'""", pty=True)

@task
def check_dev(c):
    c.run("""cd .. && RD CMD check --as-cran myClim_*.*.*.tar.gz""", pty=True)

@task
def build(c):
    """
    build myClim package
    """
    c.run("""R -e 'devtools::build(".", path="../myClim_latest.tar.gz")'""")

@task
def install(c, vignette=True):
    """
    install myClim
    vignette - generating vignettes default True
    """
    if vignette:
        build(c)
        c.run("""R -e 'install.packages("../myClim_latest.tar.gz", lib=Sys.getenv("R_LIBS_USER"), repos=NULL, build_vignettes=TRUE)'""")
    else:
        c.run("""R - e 'install.packages(".", repos = NULL)'""")

@task
def install(c, vignette=True):
    """
    install myClim
    vignette - generating vignettes default True
    """
    if vignette:
        c.run("""R -e 'devtools::build(".", path="../myClim_latest.tar.gz")'""")
        c.run("""R -e 'install.packages("../myClim_latest.tar.gz", repos=NULL, build_vignettes=TRUE)'""")
    else:
        c.run("""R - e 'install.packages(".", repos = NULL)'""")

@task
def test(c):
    c.run("""R --vanilla --no-multiarch -e 'devtools::test()'""", pty=True)

