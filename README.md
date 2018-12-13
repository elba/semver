# semver

A library for handling semantic versioning in Idris.

Currently undocumented but feature-complete.

## Building

### [elba](https://github.com/elba/elba)

First, ensure `lightyear` and `containers` are installed - those still rely on
Idris' ipkg system since they don't have an associated elba manifest file.

Then, add this line to the `[dependencies]` section of your project's `elba.toml`:

```toml
"elba/semver" = { git = "https://github.com/elba/semver" }
```

### idris' built-in ipkg

First, ensure `lightyear` and `containers` are installed. Then, clone the
repository and install using the included ipkg:

```shell
$ git clone https://github.com/elba/semver && cd semver
$ idris --install semver.ipkg
```