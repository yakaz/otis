# Operator: `match`

## Synopsis

`match` takes a mapping, where the only mandatory `key: value` pair must
be a `$(variable): "regex"` pair.


```yaml
- match:
    $(variable): "(.*)"
    captures: [ $(capture) ]
    flags: i
```

The given variable must match the given regular expression for `match`
to succeed.

Regular expression format is the one supported by Erlang's `re` module.
Note that, because the configuration file is YAML, you must escape
backslahes which are part of the regex.

The maping may contain two optional keys:
* **`captures`**: takes a list of variables where all the captured
    string are stored.
* **`flags`**: takes a string made of one-character flags; supported
    flags are:
  * `i`: perform a case-insensitive matching

## Examples

* Capture sub-domain or TLD and store it in the `lang` query string
parameter:

    ```yaml
    - rule:
        - any:
            - match:
                $(HOST): "^([^.]+)\\.example.com$"
                captures: [ $(query:lang) ]
                flags: i
            - match:
                $(HOST): "\\.(.+)$"
                captures: [ $(query:lang) ]
        - goto: NEXT
    ```
