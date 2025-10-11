# Copilot Instructions for RPhenoscape

## Project Overview

RPhenoscape is an R package that provides convenient access to the Phenoscape Knowledgebase (KB) API. The package facilitates searching ontology terms, retrieving term info, and querying data matrices. Most functions connect to the Phenoscape KB API and handle JSON, CSV, and NeXML data formats.

## Code Style and Conventions

### R Code Style

- Use `<-` for assignment, not `=`
- Use snake_case for function names (e.g., `get_json_data`, `pkb_args_to_query`)
- Use descriptive parameter names in function definitions
- Include roxygen2 documentation for all exported functions
- Use 2-space indentation
- Keep lines under 80 characters where practical
- Use `::` notation for package function calls (e.g., `httr::GET`, `jsonlite::fromJSON`)

### Documentation

- All exported functions must have roxygen2 documentation with:
  - `@param` descriptions for all parameters
  - `@return` description of what the function returns
  - `@export` directive for functions that should be exported
  - `@importFrom` directives for imported functions
  - `@examples` when appropriate (wrapped in `\dontrun{}` if they require API access)
- Use markdown syntax in documentation (enabled via `Roxygen: list(markdown = TRUE)` in DESCRIPTION)
- The project uses roxygen2 version 7.2.0

### Function Design

- Package-internal functions should be clearly marked in documentation
- Use consistent parameter names across related functions:
  - `url` for API endpoints
  - `query` for query parameters (as named list)
  - `verbose` for controlling message output
  - `forceGET` for forcing HTTP GET instead of POST
- Functions should gracefully handle API failures (e.g., 404 returns NULL, not error)
- Use local closures for managing package state (see `ua()` and `phenoscape_api()` functions)

## API Interaction Patterns

### HTTP Requests

- Use `httr` package for all HTTP requests
- Include custom user agent string via `ua()` function
- Support both GET and POST methods based on query size
  - Use POST for queries with JSON length >= 2048-3072 bytes
  - Provide `forceGET` parameter to override this behavior
- Always use `stop_for_pk_status()` to check response status
- Handle different content types: JSON, CSV, plain text, NeXML

### Data Parsing

- Use `jsonlite::fromJSON()` with `simplifyVector = TRUE, flatten = TRUE`
- Handle UTF-8 encoding explicitly for JSON responses when charset not provided
- Clean JSON-LD names by replacing `@id` with `id` using `clean_jsonld_names()`
- Return NULL for empty responses or 404 errors with appropriate messages

### API Configuration

- Base API URL is configurable via `PHENOSCAPE_API` environment variable
- Default: `https://kb.phenoscape.org/api/v2`
- Use `pkb_api()` helper function to construct endpoint URLs

## Testing

### Test Framework

- Use `testthat` for all tests
- Tests are located in `tests/testthat/` directory
- Test files follow naming convention: `test-{feature}.R`
- Use `context()` to describe test groups

### Test Practices

- Use `skip_on_cran()` for tests that require internet/API access
- Test both success and error cases
- Use appropriate expectations:
  - `expect_is()` for class checks
  - `expect_s3_class()` for S3 class checks
  - `expect_equal()` for value comparisons
  - `expect_match()` for pattern matching
  - `expect_output()` for print methods
  - `expect_gte()` / `expect_lte()` for numeric comparisons
- Prefix testthat functions with package namespace: `testthat::`

### Test Structure

```r
context("Feature description")

test_that("specific behavior being tested", {
  skip_on_cran()  # if test requires API access
  # test code
  testthat::expect_equal(actual, expected)
})
```

## Package Structure

### Dependencies

- **Imports**: jsonlite, httr, dplyr, rlang, methods, stringi, RNeXML (>= 2.4.0)
- **Suggests**: roxygen2, knitr, testthat, plyr, rmarkdown
- Minimum R version: 4.0.0

### Key Modules

- `pkb_get.R`: Core API interaction functions
- `response.R`: HTTP response handling and status codes
- `nexml.R`: NeXML data handling
- `ontotrace.R`: OntoTrace functionality
- `semsim.R`: Semantic similarity computations
- `study.R`: Study-related queries
- `term-type.R`: Term type handling
- `phenotypes.R`: Phenotype queries

## Development Workflow

### Building and Testing

- Use standard R CMD check for package validation
- README.md is generated from README.rmd - edit the .rmd file, not .md
- Use `pkgdown` for website generation (configured in `_pkgdown.yml`)
- CI/CD runs on CircleCI (configuration in `.circleci/config.yml`)

### Version Control

- Follow semantic versioning
- Major version 0 indicates pre-1.0 API that may have breaking changes
- Maintain backward compatibility within minor versions (0.x.n to 0.x.m)
- Provide deprecation paths for breaking changes

## Common Patterns

### Error Handling

```r
# Check API response status
stop_for_pk_status(res)

# Handle empty responses
if (res$status_code == 404 && contLen > 0) {
  err <- httr::content(res, as = "text")
  if (endsWith(err, "resource could not be found.")) return(NULL)
}
```

### IRI Resolution

- Use `get_term_iri()` to resolve ontology terms to IRIs
- Support entity (anatomy), quality (PATO), and taxon ontologies
- Parameters can be provided as labels or IRIs

### Query Building

- Use `pkb_args_to_query()` to build query parameter lists
- Support relationship expansion: "part of", "historical homologous to", "serially homologous to"
- Filter by entity, quality, taxon, study, and phenotype

## Best Practices

1. **Minimize external dependencies**: Use base R and existing package imports
2. **API efficiency**: Batch requests when possible, cache results appropriately
3. **User feedback**: Use `verbose` parameter with `mssg()` helper for progress messages
4. **Error messages**: Provide clear, actionable error messages
5. **Backward compatibility**: Deprecate rather than remove; provide migration guidance
6. **Documentation**: Keep user-facing documentation synchronized with code
7. **Testing**: Test against live API with appropriate skip conditions
8. **Performance**: Consider query size thresholds for GET vs POST methods

## Resources

- Package website: http://rphenoscape.phenoscape.org/
- Source repository: https://github.com/phenoscape/rphenoscape
- Phenoscape KB API: https://kb.phenoscape.org/api/v2/docs/
- Use cases: https://github.com/phenoscape/rphenoscape/wiki/User-Stories
