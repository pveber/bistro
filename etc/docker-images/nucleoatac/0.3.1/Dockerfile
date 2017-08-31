FROM pveber/bistro-base:stretch

RUN apt-get update && apt-get install -y python-dev python-pip

RUN pip install cython numpy scipy matplotlib pysam

RUN wget https://github.com/GreenleafLab/NucleoATAC/archive/v0.3.1.tar.gz

RUN \
  tar xvfz v0.3.1.tar.gz && \
  cd NucleoATAC-0.3.1 && \
  pip install .
