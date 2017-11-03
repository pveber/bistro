FROM pveber/bistro-base:stretch

RUN apt-get update && apt-get install -y mafft=7.307-1
RUN wget ftp://pbil.univ-lyon1.fr/pub/logiciel/paraload/paraload-1.2.tar.gz
RUN tar xvfz paraload-1.2.tar.gz
RUN cd paraload && make && cp paraload /usr/bin
