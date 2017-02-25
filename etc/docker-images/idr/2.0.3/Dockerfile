FROM pveber/bistro-base:stretch

RUN \
  wget https://github.com/nboley/idr/archive/2.0.3.zip && \
  unzip 2.0.3.zip
RUN \
  cd idr-2.0.3 && \
  python3 setup.py install

