# eslint-plugin-bookkeeping-metrics

Report back software metrics

## Installation

You'll first need to install [ESLint](http://eslint.org):

```
$ npm i eslint --save-dev
```

Next, install `eslint-plugin-bookkeeping-metrics`:

```
$ npm install eslint-plugin-bookkeeping-metrics --save-dev
```


## Usage

Add `bookkeeping-metrics` to the plugins section of your `.eslintrc` configuration file. You can omit the `eslint-plugin-` prefix:

```json
{
    "plugins": [
        "bookkeeping-metrics"
    ]
}
```


Then configure the rules you want to use under the rules section.

```json
{
    "rules": {
        "bookkeeping-metrics/rule-name": 2
    }
}
```

## Supported Rules

* Fill in provided rules here





