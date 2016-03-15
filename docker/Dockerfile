FROM ubuntu:14.04

ENV OTP_VERSION=18.2.1

RUN set -xe \
	&& buildDeps='build-essential autoconf curl ca-certificates libncurses5-dev libssl-dev openssl git' \
	&& apt-get update \
	&& apt-get upgrade -y \
	&& apt-get install -y --no-install-recommends $buildDeps \
    && rm -rf /var/lib/apt/lists/*

RUN set -xe \
	&& OTP_DOWNLOAD_URL="http://erlang.org/download/otp_src_$OTP_VERSION.tar.gz" \
	&& OTP_DOWNLOAD_MD5="ebb226d51f89ec54cc83ce5d417ce55e" \
	&& curl -fSL -o otp-src.tar.gz "$OTP_DOWNLOAD_URL" \
	&& echo "$OTP_DOWNLOAD_MD5 otp-src.tar.gz" | md5sum -c - \
	&& mkdir -p /usr/local/src/otp-src \
	&& tar -xzf otp-src.tar.gz -C /usr/local/src/otp-src --strip-components=1 \
	&& rm otp-src.tar.gz \
	&& cd /usr/local/src/otp-src \
	&& ./configure \
	&& make -j$(nproc) \
	&& make install \
	&& find /usr/local -name examples |xargs rm -rf \
	&& rm -rf /usr/local/src/otp-src

RUN mkdir -p /usr/local/src/ \
    && cd /usr/local/src \
    && git clone "https://github.com/indigo-dc/tts.git" \
    && cd tts \
    && make rel \
    && mkdir -p /usr/local/tts \
    && cp -r ./_rel/tts_release/* /usr/local/tts \
    && rm -rf /usr/local/src/tts


EXPOSE 80
EXPOSE 443 

CMD cd /usr/local/tts/ && ./bin/tts_release console
