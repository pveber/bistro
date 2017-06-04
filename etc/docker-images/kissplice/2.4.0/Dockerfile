FROM pveber/bistro-base:jessie

RUN \
  apt-get update && apt-get install -y cmake g++ libhdf5-dev zlib1g-dev --fix-missing

RUN \
  wget ftp://pbil.univ-lyon1.fr/pub/logiciel/kissplice/download/kissplice-2.4.0-p1.tar.gz

RUN \
  tar xf kissplice-2.4.0-p1.tar.gz && \
  rm kissplice-2.4.0-p1.tar.gz && \
  cd kissplice-2.4.0-p1 && \
  cmake . && \
  make && \
  make install
