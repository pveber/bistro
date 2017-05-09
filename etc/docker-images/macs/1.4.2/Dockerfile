FROM pveber/bistro-base:jessie

RUN wget https://github.com/downloads/taoliu/MACS/MACS-1.4.2-1.tar.gz &&\
    tar -xzf MACS-1.4.2-1.tar.gz &&\
    rm MACS-1.4.2-1.tar.gz &&\
    cd MACS-1.4.2 && python setup.py install
