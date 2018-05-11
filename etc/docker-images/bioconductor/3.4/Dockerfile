FROM pveber/bistro-base:stretch

RUN apt-get update && apt-get install -y r-base=3.3.3-1 libxml2-dev libcurl4-openssl-dev
RUN echo 'source("https://bioconductor.org/biocLite.R") ; biocLite()' | R --vanilla
RUN echo 'source("https://bioconductor.org/biocLite.R") ; biocLite("DESeq2")' | R --vanilla
RUN echo 'source("https://bioconductor.org/biocLite.R") ; biocLite("GenoGAM")' | R --vanilla
RUN echo 'source("https://bioconductor.org/biocLite.R") ; biocLite("chipseq")' | R --vanilla
RUN echo 'source("https://bioconductor.org/biocLite.R") ; biocLite("ChIPQC")' | R --vanilla
RUN echo 'install.packages("gplots", repos="https://pbil.univ-lyon1.fr/CRAN/")' | R --vanilla
RUN echo 'install.packages("pvclust", repos="https://pbil.univ-lyon1.fr/CRAN/")' | R --vanilla
RUN echo 'install.packages("pheatmap", repos="https://pbil.univ-lyon1.fr/CRAN/")' | R --vanilla
RUN echo 'install.packages("cluster", repos="https://pbil.univ-lyon1.fr/CRAN/")' | R --vanilla
RUN echo 'source("https://bioconductor.org/biocLite.R") ; biocLite("limma")' | R --vanilla
RUN echo 'install.packages("ggfortify", repos="https://pbil.univ-lyon1.fr/CRAN/")' | R --vanilla
