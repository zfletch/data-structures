FROM ubuntu:18.04

# Set up build environment
ARG DEBIAN_FRONTEND=noninteractive

# Install global dependencies
RUN apt-get -qq update && apt-get -qq install software-properties-common
RUN add-apt-repository -y ppa:plt/racket
RUN apt-get -qq install ruby racket valgrind nodejs npm curl libffi-dev libgmp-dev zlib1g-dev
RUN curl -sSL https://get.haskellstack.org/ | sh
RUN npm install -g yarn
RUN gem install bundler -v 1.16.1

# Install local dependencies
RUN mkdir /app
WORKDIR /app
ADD Gemfile /app/Gemfile
ADD Gemfile.lock /app/Gemfile.lock
ADD package.json /app/package.json
ADD yarn.lock /app/yarn.lock
ADD data-structures.cabal /app/data-structures.cabal
ADD stack.yaml /app/stack.yaml
RUN bundle config --global silence_root_warning 1 && bundle install
RUN yarn install
RUN stack setup

# Specify what happens when run 
ADD . /app
CMD ["bash"]
