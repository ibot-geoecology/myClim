from pathlib import Path
from invoke import task


@task
def generate(c):
    """
    Generate source and documentation
    """
    for data_file in Path("data-raw").glob("mc_data_*.R"):
        c.run(f"Rscript {str(data_file)}")
    Path("NAMESPACE").unlink(missing_ok=True)
    c.run(f"R -e 'devtools::document()'")

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
def check(c):
    c.run("""R --vanilla --no-multiarch -e 'devtools::check()'""")

