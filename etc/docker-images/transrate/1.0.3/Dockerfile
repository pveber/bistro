FROM pveber/bistro-base:stretch

RUN apt-get update
RUN apt-get install -y ruby ruby-dev cmake zlib1g-dev g++

RUN wget http://snap.cs.berkeley.edu/downloads/snap-beta.18-linux.tar.gz && \
    tar xvfz snap-beta.18-linux.tar.gz && \
    chmod a+x snap-aligner && \
    mv snap-aligner /usr/bin && \
    cd / && rm -rf snap-beta.18-linux.tar.gz
RUN wget https://github.com/COMBINE-lab/salmon/releases/download/v0.12.0/salmon-0.12.0_linux_x86_64.tar.gz && \
    tar xvfz salmon-0.12.0_linux_x86_64.tar.gz && \
    cd salmon-0.12.0_linux_x86_64 && \
    cp lib/* /usr/lib && \
    mv bin/salmon /usr/bin && \
    cd / && rm -rf salmon*
RUN wget https://github.com/Blahah/transrate-tools/releases/download/v1.0.0/bam-read_v1.0.0_linux.tar.gz && \
    tar xvfz bam-read_v1.0.0_linux.tar.gz && \
    rm bam-read_v1.0.0_linux.tar.gz && \
    chmod a+x bam-read && \
    mv bam-read /usr/bin && \
    cd / && rm -rf bam-read_v1.0.0_linux.tar.gz

RUN gem install transrate
