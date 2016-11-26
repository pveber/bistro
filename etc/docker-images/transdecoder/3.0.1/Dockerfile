FROM pveber/bistro-base:jessie

RUN apt-get update && \
    apt-get install -y liburi-perl
RUN wget "https://github.com/TransDecoder/TransDecoder/archive/v3.0.1.tar.gz"
RUN tar xvfz v3.0.1.tar.gz
RUN cd TransDecoder-3.0.1 && make && cp -r PerlLib/* /usr/lib/x86_64-linux-gnu/perl/5.20
RUN cd /usr/bin && ln -s /TransDecoder-3.0.1/TransDecoder.LongOrfs . && \
                   ln -s /TransDecoder-3.0.1/TransDecoder.Predict .
