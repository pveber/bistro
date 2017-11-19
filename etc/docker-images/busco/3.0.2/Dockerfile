FROM pveber/bistro-base:stretch

RUN apt-get update && \
 apt-get upgrade -y && \
 apt-get install -y wget zlib1g-dev libboost-iostreams-dev libsqlite3-dev libboost-graph-dev liblpsolve55-dev cmake gcc git build-essential

#install augustus (later, for genome assessment only)
#RUN apt-get install -y augustus
#add augustus bin and script to PATH and add AUGUSTUS_CONFIG_PATH and change busco config.ini file


#install hmmer v3
RUN cd  && wget -O- http://eddylab.org/software/hmmer3/3.1b2/hmmer-3.1b2-linux-intel-x86_64.tar.gz | tar zx && \
 cd hmmer-3.1b2-linux-intel-x86_64/ && ./configure && make && make install

# install ncbi blast 
RUN apt-get install -y ncbi-blast+

# install busco 

# make python3 the default python 
RUN ln -fs /usr/bin/python3 /usr/bin/python

RUN cd /root && git clone http://gitlab.com/ezlab/busco && cd busco && python setup.py install && cp scripts/*.py /usr/bin/ && cp config/config.ini.default config.ini

ENV BUSCO_CONFIG_FILE "/root/busco/config.ini"

