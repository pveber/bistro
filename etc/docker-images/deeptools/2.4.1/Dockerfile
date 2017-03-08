FROM pveber/bistro-base:latest

RUN apt-get update && apt-get install -y python-dev libxslt-dev \
    libatlas-base-dev libcurl4-openssl-dev zlib1g-dev

RUN wget https://bootstrap.pypa.io/get-pip.py && \
    python get-pip.py

RUN pip install deeptools==2.4.1
