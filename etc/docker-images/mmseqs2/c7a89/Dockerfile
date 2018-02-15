FROM pveber/bistro-base:stretch

RUN apt-get update && apt-get install -y --allow-unauthenticated \
    cmake xxd
RUN wget "https://github.com/soedinglab/MMseqs2/archive/1-c7a89.tar.gz" && \
    tar xvfz 1-c7a89.tar.gz
RUN cd MMseqs2-1-c7a89 && \
    mkdir build && \
    cd build && \
    cmake -DCMAKE_BUILD_TYPE=RELEASE -DCMAKE_INSTALL_PREFIX=. .. && \
    make && \
    make install && \
    cp bin/mmseqs /usr/bin
