FROM sbtscala/scala-sbt:eclipse-temurin-jammy-17.0.5_8_1.8.2_3.2.1

RUN apt-get update \ 
    && apt-get install -y curl \
    && apt-get install -y net-tools \
    && apt-get install -y git \
    && apt-get install -y libxrender1 libxtst6 libxi6

# install nodejs
RUN curl -fsSL https://deb.nodesource.com/setup_18.x | bash - && \
apt-get install -y nodejs
