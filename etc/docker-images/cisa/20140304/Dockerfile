FROM pveber/bistro-base:latest

RUN apt-get update && apt-get install -y python ncbi-blast+ mummer

RUN wget "http://sb.nhri.org.tw/CISA/upload/en/2014/3/CISA_20140304-05194132.tar"

RUN tar xvf CISA_20140304-05194132.tar

RUN cd CISA1.3 && cp *.py /usr/local/bin && cp -r src /usr/local/bin # Yes, you do have to do that.
