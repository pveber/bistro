FROM debian:buster

RUN apt-get update && apt-get install -y wget python
RUN (wget http://cab.spbu.ru/files/release3.12.0/SPAdes-3.12.0-Linux.tar.gz && \
     cd /usr && tar xvfz ../SPAdes-3.12.0-Linux.tar.gz  --strip-components=1 && \
     rm ../SPAdes-3.12.0-Linux.tar.gz)
