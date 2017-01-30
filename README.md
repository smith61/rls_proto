# Language Server Protocol in Rust

This crate implements Tokio codec support for the Language Server Protocol. The crate only implements
encoding and decoding of messages, and does not aim to ever implement IO and request handling as there are many correct
ways to implement either part.

## Usage
In your `Cargo.toml`:
```toml
[dependencies]
lsp_rs = { git = "https://github.com/smith61/rls_proto" }
```

In your `main.rs` or `lib.rs`:
```rust
extern crate lsp_rs;

use lsp_rs::*;
```