FROM pveber/bistro-base:stretch

RUN wget \
      -O subread-1.6.0.tar.gz \
      "https://downloads.sourceforge.net/project/subread/subread-1.6.0/subread-1.6.0-Linux-x86_64.tar.gz"
RUN tar xvfz subread-1.6.0.tar.gz
RUN mv subread-1.6.0-Linux-x86_64/bin/* /usr/bin
