FROM pveber/bistro-base:stretch

RUN wget ftp://ftp.ccb.jhu.edu/pub/infphilo/hisat2/downloads/hisat2-2.1.0-Linux_x86_64.zip
RUN unzip hisat2-2.1.0-Linux_x86_64.zip
RUN mv hisat2-2.1.0/* /usr/bin

RUN apt-get update && apt-get -y install python
