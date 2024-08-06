# gh-actions-docs

Automatically generate documentation for your GitHub Actions!

# 🚀 Features

- Generates documentation for the action name, description, inputs, permissions and usage.
- Supports multiple `action.yml` files.
- Supports the full GitHub Actions schema, including multiline strings.
- Optionally generates a table of contents for your markdown file.
- Runs Prettier after generating the documentation, to ensure a consistent style (can be disabled).
- Very customizable, with many options to e.g. disable parts of the documentation.

# ⚡ Quickstart

**1.** Add the following to your `README.md` file:

```markdown
<!-- gh-actions-docs-start path=your/cool/action.yml owner=yourgithubaccount project=cool-action version=v1 -->
<!-- gh-actions-docs-end -->
```

**Edit the `path` input to point to your action's `action.yml` file.**

Optionally, you can add the `owner`, `project` and `version` inputs to generate the "Usage" section.
You can also add the `permissions` input to generate the "Permissions" section.

**2.** Use the `gh-actions-docs` action in your workflow:

```yaml
generate-docs:
  name: Generate documentation
  runs-on: ubuntu-latest
  permissions:
    contents: write
  steps:
    - name: Checkout repository
      uses: actions/checkout@v4

    - name: Generate documentation
      uses: 3lvia/gh-actions-docs@v1

    - name: Commit changes
      run: |
        if [[ -z "$(git status --porcelain)" ]]; then
          echo "No changes to commit"
          exit 0
        fi

        git config user.name github-actions
        git config user.email github-actions@github.com

        git add README.md
        git commit -m "Update action documentation"
        git push
```

# 📝 Examples of Generated Documentation

- https://github.com/3lvia/core-github-actions-templates/blob/trunk/README.md
- [This section of the `README.md`](#inputs) and onwards.

# ✍️ Getting Started

`gh-actions-docs` can be used as an executable or as a GitHub Action.
The generated documentation will be added to the file `README.md` by default.
To specify where in the file the documentation should be added, add the following two comments to your README file:

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

## GitHub Action

`gh-actions-docs` is primarily intended to be used as a GitHub Action.
The below documentation is actually generated from the `action.yml` file in this repository
(with name, description and table of contents disabled).

<!-- gh-actions-docs-start path=action.yml owner=3lvia project=gh-actions-docs version=v1 -->

### Inputs

| Name             | Description                                                                                 | Required | Default     |
| ---------------- | ------------------------------------------------------------------------------------------- | -------- | ----------- |
| `debug`          | Set to `true` to enable debug output.                                                       | no       |             |
| `ignore-files`   | Comma-separated list of `actions.yml`-files to ignore when generating action documentation. | no       |             |
| `ignore-headers` | Comma-separated list of headers to ignore when generating a table of contents.              | no       |             |
| `no-actions`     | Set to `true` to disable generation of actions documentation.                               | no       |             |
| `no-description` | Set to `true` to disable generation of the action description.                              | no       |             |
| `no-inputs`      | Set to `true` to disable generation of the action inputs.                                   | no       |             |
| `no-name`        | Set to `true` to disable generation of the action name.                                     | no       |             |
| `no-permissions` | Set to `true` to disable generation of the action permissions.                              | no       |             |
| `no-toc`         | Set to `true` to disable generation of a table of contents.                                 | no       |             |
| `no-usage`       | Set to `true` to disable generation of the action usage.                                    | no       |             |
| `readme-file`    | The file to write the documentation to.                                                     | no       | `README.md` |
| `run-prettier`   | Set to `true` to run Prettier on the generated documentation.                               | no       |             |

### Usage

```yaml
- name: gh-actions-docs
  uses: 3lvia/gh-actions-docs@v1
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
    # Set to `true` to run Prettier on the generated documentation.
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

# 🧑‍💻 Development

## Releasing a new version

To release a new version, update the version tag in both [action.yml](action.yml) and [gh-actions-docs.cabal](gh-actions-docs.cabal).
A new git tag and GitHub release should then be generated when pushing the changes to `trunk`.
