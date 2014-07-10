# Operator: `set`

## Synopsis

`set` takes a mapping with a single `key: value` pair, where the key must be a variable:

```yaml
- set:
    $(variable): "Hello"
```

The right operand is stored in the variable.

The operation always succeed.

## Examples

* Prepend `/www` to the path:

    ```yaml
    - rule:
        - set:
            $(PATH): "/www/$(PATH)"
    ```
