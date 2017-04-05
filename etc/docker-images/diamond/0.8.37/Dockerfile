FROM pveber/bistro-base:jessie

RUN \
  apt-get update && apt-get install -y cmake g++ zlib1g-dev --fix-missing

RUN \
  wget https://github.com/bbuchfink/diamond/archive/v0.8.37.tar.gz && \
  tar xf v0.8.37.tar.gz && \
  rm v0.8.37.tar.gz && \
  cd diamond-0.8.37 && \
  mkdir bin && cd bin && \
  cmake .. && \
  make install
