FROM pveber/bistro-base:jessie

RUN apt-get update && apt-get install -y gcc
RUN wget "https://github.com/fstrozzi/Fastool/archive/0.1.4.tar.gz"
RUN tar xvfz 0.1.4.tar.gz && \
    cd Fastool-0.1.4 && \
    make && \
    cp fastool /usr/bin && \
    cd .. && rm -rf 0.1.4*
