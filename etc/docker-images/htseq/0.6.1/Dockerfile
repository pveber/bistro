FROM pveber/bistro-base:latest

RUN apt-get update && \
    apt-get install -y build-essential \
                       python2.7-dev \
                       python-numpy \
                       python-matplotlib \
                       python-pysam

RUN wget "https://pypi.python.org/packages/3c/6e/f8dc3500933e036993645c3f854c4351c9028b180c6dcececde944022992/HTSeq-0.6.1p1.tar.gz" && \
    tar xvfz HTSeq-0.6.1p1.tar.gz && \
    cd HTSeq-0.6.1p1 && \
    python setup.py build && \
    python setup.py install && \
    cd .. && rm -rf HTSeq*
