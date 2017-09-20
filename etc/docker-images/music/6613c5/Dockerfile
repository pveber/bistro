FROM pveber/bistro-base:stretch

RUN git clone https://github.com/gersteinlab/MUSIC.git

RUN cd MUSIC && \
    git checkout 6613c532bf && \
    make clean && \
    make

RUN cp MUSIC/bin/MUSIC /usr/bin
