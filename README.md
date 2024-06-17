# gh-actions-docs

Automatically generate documentation for your GitHub Actions!

# Usage

Run `gh-actions-docs` in the root of your repository to generate documentation for all your GitHub Actions.
The documentation will be added to the file `README.md` by default.
To specify where the documentation should be added, add the following two comments to your README file:

```markdown
<!-- gh-actions-docs-start path=your/cool/action.yml owner=3lvia project=cool-action version=v3 permissions=contents:read,issues:write -->
<!-- gh-actions-docs-end -->
```

Documentation will then be generated between these two comments. You can add multiple comments to generate documentation for multiple actions.

Only the `path` parameter is required, and the `owner`, `project`, `version` and `permissions` parameters are optional.

The parameters `owner`, `project` and `version` are used to generate the "Usage" section.
If any of these parameters are missing, the "Usage" section will not be generated.

The `permissions` parameter is used to generate the "Permissions" section.
If this parameter is missing, the "Permissions" section will not be generated.

## Generate a table of contents

`gh-actions-docs` can also generate a table of contents for your README file.
Add the following two comments to your README file to generate a table of contents:

```markdown
<!-- gh-actions-docs-toc-start -->
<!-- gh-actions-docs-toc-end -->
```

## GitHub Actions

`gh-actions-docs` is primarily intended to be used as a GitHub Action.
The below documentation is actually generated from the `action.yml` file in this repository!

<!-- gh-actions-docs-start path=action.yml owner=3lvia project=gh-actions-docs version=trunk -->

### Inputs

| Name             | Description                                                                                               | Required | Default     |
| ---------------- | --------------------------------------------------------------------------------------------------------- | -------- | ----------- |
| `debug`          | Set to `true` to enable debug output.                                                                     | no       |             |
| `ignore-files`   | Comma-separated list of `actions.yml`-files to ignore when generating action documentation.               | no       |             |
| `ignore-headers` | Comma-separated list of headers to ignore when generating a table of contents.                            | no       |             |
| `no-actions`     | Set to `true` to disable generation of actions documentation.                                             | no       |             |
| `no-description` | Set to `true` to disable generation of the action description.                                            | no       |             |
| `no-inputs`      | Set to `true` to disable generation of the action inputs.                                                 | no       |             |
| `no-name`        | Set to `true` to disable generation of the action name.                                                   | no       |             |
| `no-permissions` | Set to `true` to disable generation of the action permissions.                                            | no       |             |
| `no-toc`         | Set to `true` to disable generation of a table of contents.                                               | no       |             |
| `no-usage`       | Set to `true` to disable generation of the action usage.                                                  | no       |             |
| `readme-file`    | The file to write the documentation to.                                                                   | no       | `README.md` |
| `run-prettier`   | Set to `true` to run Prettier on the generated documentation. This assumes Prettier is already installed. | no       |             |

### Usage

```yaml
- name: gh-actions-docs
  uses: 3lvia/gh-actions-docs@trunk
  with:
    debug:
    # Set to `true` to enable debug output.
    #
    # Required: no

    ignore-files:
    # Comma-separated list of `actions.yml`-files to ignore when generating action documentation.
    #
    # Required: no

    ignore-headers:
    # Comma-separated list of headers to ignore when generating a table of contents.
    #
    # Required: no

    no-actions:
    # Set to `true` to disable generation of actions documentation.
    #
    # Required: no

    no-description:
    # Set to `true` to disable generation of the action description.
    #
    # Required: no

    no-inputs:
    # Set to `true` to disable generation of the action inputs.
    #
    # Required: no

    no-name:
    # Set to `true` to disable generation of the action name.
    #
    # Required: no

    no-permissions:
    # Set to `true` to disable generation of the action permissions.
    #
    # Required: no

    no-toc:
    # Set to `true` to disable generation of a table of contents.
    #
    # Required: no

    no-usage:
    # Set to `true` to disable generation of the action usage.
    #
    # Required: no

    readme-file:
    # The file to write the documentation to.
    #
    # Required: no
    # Default: 'README.md'

    run-prettier:
    # Set to `true` to run Prettier on the generated documentation. This assumes Prettier is already installed.
    #
    # Required: no
```

<!-- gh-actions-docs-end -->

## Docker

You can run `gh-actions-docs` using locally using Docker:

```bash
docker run -v "$(pwd):/opt/app" ghcr.io/3lvia/gh-actions-docs:latest
```

## Local

You can also run `gh-actions-docs` locally using Cabal.
Install [GHCUp](https://www.haskell.org/ghcup), which should include Cabal, and then run:

```bash
cabal update
cabal install --overwrite-policy=always
gh-actions-docs
```

### Environment variables (Docker or local)

See inputs above or [action.yml](action.yml) for all available environment variables when running using Docker or locally with Cabal.
The environment variables use all-caps snake-case, e.g. `IGNORE_FILES` instead of `ignore-files`.
