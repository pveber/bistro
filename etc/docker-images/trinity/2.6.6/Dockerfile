FROM pveber/bistro-base:stretch

RUN apt-get update && \
    apt-get install -y --allow-unauthenticated libc6 libgcc1 libgomp1 libhts1 libstdc++6 perl jaligner \
    libcommons-collections4-java libgetopt-java libjung-free-java bowtie bowtie2 \
    python libwww-perl default-jre-headless samtools jellyfish r-base-core \
    r-cran-cluster r-bioc-edger r-bioc-qvalue rsem berkeley-express trimmomatic \
    transdecoder parafly curl
RUN wget "https://github.com/trinityrnaseq/trinityrnaseq/archive/Trinity-v2.6.6.tar.gz"
RUN tar xvfz Trinity-v2.6.6.tar.gz
RUN cd trinityrnaseq-Trinity-v2.6.6 && make && make plugins
RUN cd trinityrnaseq-Trinity-v2.6.6 && cp -r Trinity PerlLib Butterfly Chrysalis Inchworm trinity-plugins util /usr/bin
ENV TRINITY_HOME /trinityrnaseq-Trinity-v2.6.6
