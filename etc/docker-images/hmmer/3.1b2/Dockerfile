FROM pveber/bistro-base:stretch

RUN apt-get update && \
 apt-get upgrade -y && \
 apt-get install -y wget zlib1g-dev libboost-iostreams-dev libsqlite3-dev libboost-graph-dev liblpsolve55-dev cmake gcc git build-essential

#install hmmer v3
RUN cd  && wget -O- http://eddylab.org/software/hmmer3/3.1b2/hmmer-3.1b2-linux-intel-x86_64.tar.gz | tar zx && \
 cd hmmer-3.1b2-linux-intel-x86_64/ && ./configure && make && make install
