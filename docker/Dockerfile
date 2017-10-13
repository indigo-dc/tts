FROM ubuntu:16.04


RUN set -xe \
	&& buildDeps='wget libncurses5-dev libncursesw5-dev libssl-dev build-essential autoconf git ca-certificates openssl sudo' \
	&& apt-get update \
	&& apt-get upgrade -y \
	&& apt-get install -y --no-install-recommends $buildDeps \
        && rm -rf /var/lib/apt/lists/*

RUN mkdir -p /usr/local/src/ \
    && cd /usr/local/src \
    && git clone "https://github.com/indigo-dc/tts.git" \
    && cd tts \
    && ./utils/build_install_erlang.sh \
    && make rel \
    && mkdir -p /usr/local/watts \
    && cp -r ./_build/default/rel/watts/* /usr/local/watts \
    && rm -rf /usr/local/src/tts

EXPOSE 80
EXPOSE 443

CMD cd /usr/local/watts/ && ./bin/watts console
