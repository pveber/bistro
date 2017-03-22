FROM pveber/bistro-base:stretch

RUN apt-get install -y r-base=3.3.3-1 libxml2-dev

RUN echo 'install.packages("gplots", repos="https://pbil.univ-lyon1.fr/CRAN/")' | R --vanilla

RUN echo 'source("https://bioconductor.org/biocLite.R") ; biocLite("DESeq2")' | R --vanilla

RUN echo 'source("https://bioconductor.org/biocLite.R") ; biocLite("GenoGAM")' | R --vanilla

RUN apt-get install -y r-cran-rcurl

RUN echo 'source("https://bioconductor.org/biocLite.R") ; biocLite("ChIPQC")' | R --vanilla
