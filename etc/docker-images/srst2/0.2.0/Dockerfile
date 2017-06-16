FROM pveber/bistro-base:jessie

RUN apt-get update && apt-get install -y python-dev python-pip python-scipy python-numpy
RUN apt-get install -y bowtie2=2.2.4-1
RUN apt-get install -y samtools=0.1.19-1
RUN wget https://github.com/katholt/srst2/archive/v0.2.0.tar.gz
RUN tar xzf v0.2.0.tar.gz && rm v0.2.0.tar.gz
RUN pip install srst2-0.2.0/
