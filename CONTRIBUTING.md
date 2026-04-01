# Contributing to statim

Thank you for your interest in contributing to statim.
We are genuinely grateful for every contribution; your time and ideas make this project better and directly help us as maintainers.

## Code of Conduct

By participating in this project, you agree to follow the rules in [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md).

## Ways to Contribute

- Report bugs and suggest features using GitHub issues.
- Improve documentation and examples.
- Submit pull requests for fixes and enhancements.

## Development Setup

1. Clone the repository.
2. Install development dependencies in R:

```r
install.packages(c("devtools", "roxygen2", "testthat"))
```

3. Load the package for development:

```r
devtools::load_all()
```

## Pull Request Guidelines

- Keep pull requests focused and small when possible.
- Add or update tests when behavior changes.
- Update documentation for user-facing changes.
- Run checks before submitting:

```r
devtools::check()
```

## Questions

If you are unsure how to approach a change, open an issue to discuss it first.
