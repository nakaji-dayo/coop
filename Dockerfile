FROM haskell:9.6 AS builder

WORKDIR /app

# Copy cabal file first for dependency caching
COPY coop.cabal ./
RUN cabal update && cabal build --only-dependencies -j4

# Copy source and build
COPY . .
RUN cabal build exe:coop
RUN cp $(cabal list-bin coop) /app/coop-bin

# Runtime image
FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y \
    ca-certificates \
    libgmp10 \
    netbase \
  && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY --from=builder /app/coop-bin /app/coop
COPY config/ /app/config/

EXPOSE 3000

ENTRYPOINT ["/app/coop"]
CMD ["--config", "config/coop-dryrun.dhall"]
