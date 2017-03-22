FROM pveber/bistro-base:jessie

RUN apt-get update && apt-get install -y python-dev libxslt-dev \
    libatlas-base-dev libcurl4-openssl-dev zlib1g-dev \
    python-matplotlib python-pip gfortran

RUN pip install deeptools==2.4.1
