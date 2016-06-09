FROM pveber/bistro-base:latest

RUN apt-get install -y r-base=3.3.0-1 libxml2-dev

RUN echo 'source("https://bioconductor.org/biocLite.R") ; biocLite("DESeq2")' | R --vanilla
