FROM pveber/bistro-base:jessie

RUN \
  apt-get update && apt-get install -y cmake g++ --fix-missing

RUN \
  apt-get install -y libhdf5-dev zlib1g-dev

RUN \
  wget https://github.com/pachterlab/kallisto/archive/v0.43.0.tar.gz && \
  tar xf v0.43.0.tar.gz && rm v0.43.0.tar.gz && \
  cd kallisto-0.43.0 && \
  mkdir build && cd build && \
  cmake -DCMAKE_INSTALL_PREFIX:PATH=$HOME/.local -DCMAKE_LIBRARY_PATH=$HOME/.local .. && \
  make && \
  cd src && make install && \
  ln -s /kallisto-0.43.0/build/src/kallisto /usr/bin/kallisto
