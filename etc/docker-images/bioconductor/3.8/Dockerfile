FROM pveber/bistro-base:buster

RUN apt-get update && apt-get install -y r-base libxml2-dev libcurl4-openssl-dev
RUN echo 'install.packages("BiocManager", repos="https://pbil.univ-lyon1.fr/CRAN/")' | R --vanilla
RUN echo 'BiocManager::install()' | R --vanilla
RUN echo 'install.packages("ggfortify", repos="https://pbil.univ-lyon1.fr/CRAN/")' | R --vanilla
RUN echo 'install.packages("gplots", repos="https://pbil.univ-lyon1.fr/CRAN/")' | R --vanilla
RUN echo 'install.packages("pvclust", repos="https://pbil.univ-lyon1.fr/CRAN/")' | R --vanilla
RUN echo 'install.packages("pheatmap", repos="https://pbil.univ-lyon1.fr/CRAN/")' | R --vanilla
RUN echo 'install.packages("cluster", repos="https://pbil.univ-lyon1.fr/CRAN/")' | R --vanilla
RUN echo 'BiocManager::install()' | R --vanilla
RUN echo 'BiocManager::install("DESeq2")' | R --vanilla
RUN apt-get install -y libssl-dev
RUN R --vanilla -e 'BiocManager::install("biomaRt")'
RUN R --vanilla -e 'BiocManager::install("GenoGAM")'
RUN R --vanilla -e 'BiocManager::install("chipseq")'
RUN R --vanilla -e 'BiocManager::install("ChIPQC")'
