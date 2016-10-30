FROM pveber/bistro-base:jessie

RUN cd /usr/local && wget https://github.com/loneknightpy/idba/releases/download/1.1.3/idba-1.1.3.tar.gz
RUN cd /usr/local && tar xfz idba-1.1.3.tar.gz
RUN apt-get update && apt-get install -y g++
RUN cd /usr/local/idba-1.1.3 \
    && sed -i 's/kMaxShortSequence = 128/kMaxShortSequence = 300/g' src/sequence/short_sequence.h \
    && ./configure \
    && make \
    && find bin -type f -executable -print | xargs cp -t /usr/local/bin

