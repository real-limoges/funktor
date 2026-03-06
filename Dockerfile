# Stage 1: deps — install system libs and resolve Cabal dependencies
FROM haskell:9.12 AS deps

RUN apt-get update && apt-get install -y --no-install-recommends \
    libsdl2-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /build

COPY funktor.cabal ./
RUN cabal update && cabal build --only-dependencies

# Stage 2: builder — compile the full project and collect shared libs
FROM deps AS builder

COPY . .
RUN cabal build exe:funktor -j$(nproc)

RUN mkdir -p /build/out/libs \
    && cp "$(cabal list-bin funktor)" /build/out/funktor \
    && ldd /build/out/funktor \
        | awk 'NF==4{print $3}' \
        | xargs -I{} cp -L {} /build/out/libs/

# Stage 3: dev — interactive development shell with GHC + Cabal + SDL2
FROM deps AS dev

WORKDIR /workspace
CMD ["bash"]

# Stage 4: runtime — slim distributable image
FROM debian:bookworm-slim AS runtime

RUN apt-get update && apt-get install -y --no-install-recommends \
    libsdl2-2.0-0 \
    && rm -rf /var/lib/apt/lists/*

COPY --from=builder /build/out/funktor /usr/local/bin/funktor
COPY --from=builder /build/out/libs/ /usr/local/lib/
RUN ldconfig

CMD ["funktor"]
