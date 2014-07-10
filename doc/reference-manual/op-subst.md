# Operator: `subst`

## Synopsis

`subst` takes a mapping, where the two mandatory `key: value` pairs must
be a `$(variable): "regex"` pair and a `value: ...` pair.

```yaml
- subst:
    $(variable): ".*"
    value: "New value"
    flags: gi
```

This operation replaces the matched part of the variable's value with
the contents of `value:`.

The operation always succeed.

Regular expression format is the one supported by Erlang's `re` module.
Note that, because the configuration file is YAML, you must escape
backslahes which are part of the regex.

The maping may contain two optional keys:
* **`flags`**: takes a string made of one-character flags; supported
    flags are:
  * `g`: perform the substitution to all matching occurrences, not only
     the first one.
  * `i`: perform a case-insensitive matching

## Examples

* Remove multiple consecutive slashes in path:

    ```yaml
    - rule:
        - subst:
            $(PATH): "//+"
            value: "/"
            flags: g
        - response: 302
    ```
