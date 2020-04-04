FROM erlang:22.3

ARG PUID
ARG PGID

RUN set -eux \
  && groupadd -o --gid ${PGID} workspace \
  && useradd --uid ${PUID} --gid workspace --shell /bin/bash --create-home workspace

VOLUME /workspace

USER workspace:workspace
WORKDIR /workspace

ENTRYPOINT [""]
CMD ["/bin/bash"]
