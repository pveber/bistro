FROM debian:stretch

RUN apt-get update && apt-get install -y \
    aspcud bzip2 curl wget git  make m4 unzip gcc \
    pkg-config libncurses5-dev libgdbm-dev \
    sudo
RUN apt-get install -y    \
    bowtie bowtie2 ea-utils fastqc \
    macs trinityrnaseq

RUN apt-get install -y opam

RUN opam init --comp 4.02.3

RUN echo ". /root/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true" >> ~/.profile
RUN echo "export PATH=~/usr/bin:$PATH" >> ~/.profile

ENV PATH /root/usr/bin:/root/.opam/4.02.3/bin:$PATH
ENV CAML_LD_LIBRARY_PATH "/root/.opam/4.02.3/lib/stublibs"

RUN opam install -y cohttp

RUN opam pin add sexplib 113.00.00
RUN opam pin add -k git bistro "https://github.com/pveber/bistro.git#6180f7700c4fb83e3f21d4db5de9ed0b693e8473"

RUN wget https://raw.githubusercontent.com/pveber/compbio-scripts/master/fastool/0.1.4/fastool.0.1.4.sh -O - | bash -s /opt
ENV PATH /opt/fastool/0.1.4/bin:$PATH
