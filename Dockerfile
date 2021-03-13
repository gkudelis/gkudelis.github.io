FROM rust:1.50 AS builder
WORKDIR /builder
COPY builder/ .
RUN cargo build --release

FROM rust:1.50
WORKDIR /content
COPY content/ .
COPY --from=builder /builder/target/release/builder .
CMD ["/content/builder"]
