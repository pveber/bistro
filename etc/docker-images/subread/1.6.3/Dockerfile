FROM pveber/bistro-base:stretch

RUN wget \
      -O subread-1.6.3.tar.gz \
      "https://downloads.sourceforge.net/project/subread/subread-1.6.3/subread-1.6.3-source.tar.gz"
RUN tar xvfz subread-1.6.3.tar.gz
RUN apt-get update && apt-get install -y zlib1g-dev
RUN cd subread-1.6.3-source/src && make -f Makefile.Linux
RUN mv subread-1.6.3-source/bin/* /usr/bin
