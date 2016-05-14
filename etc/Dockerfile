FROM debian:stretch

RUN apt-get update && apt-get install -y \
    aspcud bzip2 curl wget git  make m4 unzip gcc \
    pkg-config libncurses5-dev libgdbm-dev \
    sudo
RUN apt-get install -y    \
    bowtie bowtie2 ea-utils fastqc \
    macs trinityrnaseq

RUN useradd -m bistro

USER bistro
ENV USER bistro
WORKDIR /home/bistro

RUN mkdir -p usr/bin
RUN wget https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh -O - | sh -s ~/usr/bin 4.02.3

RUN echo ". /home/bistro/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true" >> ~/.profile
RUN echo "export PATH=~/usr/bin:$PATH" >> ~/.profile

ENV PATH /home/bistro/usr/bin:/home/bistro/.opam/4.02.3/bin:$PATH
ENV CAML_LD_LIBRARY_PATH "/home/bistro/.opam/4.02.3/lib/stublibs"

RUN opam install -y cohttp

RUN opam pin add sexplib 113.00.00
RUN opam pin add bistro --dev-repo

USER root

RUN wget https://raw.githubusercontent.com/pveber/compbio-scripts/master/fastool/0.1.4/fastool.0.1.4.sh -O - | bash -s /opt

USER bistro
ENV PATH /opt/fastool/0.1.4/bin:$PATH
