FROM pveber/bistro-base:latest

RUN apt-get update && apt-get install -y python-dev

RUN wget https://bootstrap.pypa.io/ez_setup.py -O - | python

RUN \
  wget https://pypi.python.org/packages/c6/fe/97319581905de40f1be7015a0ea1bd336a756f6249914b148a17eefa75dc/Cython-0.24.1.tar.gz#md5=890b494a12951f1d6228c416a5789554 && \
  tar xf Cython-0.24.1.tar.gz && rm Cython-0.24.1.tar.gz && cd Cython-0.24.1 && \
  python setup.py install && cd ..

RUN \
  git clone git://github.com/numpy/numpy.git numpy && \
  cd numpy && python setup.py build && python setup.py install && cd ..

RUN \
  wget https://pypi.python.org/packages/9f/99/a8ac96b357f6b0a6f559fe0f5a81bcae12b98579551620ce07c5183aee2c/MACS2-2.1.1.20160309.tar.gz && \
  tar xf MACS2-2.1.1.20160309.tar.gz && rm MACS2-2.1.1.20160309.tar.gz && \
  cd MACS2-2.1.1.20160309 && \
  python setup.py install
