FROM pveber/bistro-base:latest

RUN apt-get update && apt-get install -y libgsl-dev g++
RUN wget "http://www.niehs.nih.gov/research/resources/assets/docs/artsrcmountrainier20160605linuxtgz.tgz"
RUN tar xvfz artsrcmountrainier20160605linuxtgz.tgz && \
    cd art_src_MountRainier_Linux && \
    ./configure && \
    make clean && \
    make && \
    make install
