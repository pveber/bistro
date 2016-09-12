FROM pveber/bistro-base:latest

RUN apt-get update && apt-get install -y \
    python python-setuptools libpng-dev libfreetype6-dev \
    python-dev liblapack-dev g++
    

RUN wget -O quast-4.3.tar.gz "https://sourceforge.net/projects/quast/files/quast-4.3.tar.gz/download" && \
    tar xvfz quast-4.3.tar.gz

RUN cd /quast-4.3 && \
    python setup.py install && \
    cd && \
    rm -rf /quast-4.3
