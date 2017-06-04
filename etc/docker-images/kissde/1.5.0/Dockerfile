FROM pveber/bioconductor:3.4

RUN wget ftp://pbil.univ-lyon1.fr/pub/logiciel/kissplice/tools/kissDE_1.5.0.tar.gz
RUN echo "install.packages('kissDE_1.5.0.tar.gz',repos=NULL, type='source')" | R --vanilla
