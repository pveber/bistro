FROM pveber/bowtie2:2.2.9

RUN apt-get update && apt-get -y install libconfig-yaml-perl \
    libgd-perl libgd2-xpm-dev build-essential

RUN perl -MCPAN -e "install inc::latest"

RUN wget http://www.bioinformatics.babraham.ac.uk/projects/fastq_screen/fastq_screen_v0.9.4.tar.gz && \
    tar xzf fastq_screen_v0.9.4.tar.gz && \
    rm fastq_screen_v0.9.4.tar.gz && \
    ln -s /fastq_screen_v0.9.4/fastq_screen /usr/bin/fastq_screen
