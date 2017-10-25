FROM pveber/bistro-base:stretch

RUN wget -O bowtie2.zip "https://downloads.sourceforge.net/project/bowtie-bio/bowtie2/2.3.3.1/bowtie2-2.3.3.1-linux-x86_64.zip?r=https%3A%2F%2Fsourceforge.net%2Fprojects%2Fbowtie-bio%2Ffiles%2Fbowtie2%2F2.3.3.1&ts=1508932398&use_mirror=kent"
RUN unzip bowtie2.zip && cd bowtie2-2.3.3.1-linux-x86_64 && cp bowtie2* /usr/bin
RUN apt-get update && apt-get install libpython-stdlib libtbb2 python python-minimal python2.7 python2.7-minimal
