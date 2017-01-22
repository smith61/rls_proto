# Language Server Protocol in Rust

This crate implements Tokio codec support for the server side of the Language Server Protocol. The crate only implements
encoding and decoding of messages, and does not aim to ever implement IO and request handling as there are many correct
ways to implement either part.

## Usage
In your `Cargo.toml`:
```toml
[dependencies]
rls_proto     = { git = "https://github.com/smith61/rls_proto" }
```

In your `main.rs` or `lib.rs`:
```rust
extern crate rls_proto;

use rls_proto::server;
```