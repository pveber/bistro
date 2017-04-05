FROM pveber/bistro-base:jessie

RUN \
  apt-get update && apt-get install -y cmake g++ zlib1g-dev libboost-dev libboost-program-options-dev --fix-missing

RUN \
  wget ftp://pbil.univ-lyon1.fr/pub/logiciel/silix/silix-1.2.9.tar.gz && \
  tar xf silix-1.2.9.tar.gz && \
  rm silix-1.2.9.tar.gz && \
  cd silix-1.2.9 && \
  ./configure --enable-hash && \
  make && \
  make install
