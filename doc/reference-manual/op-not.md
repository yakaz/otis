# Operator: `not`

## Synopsis

`not` takes a list of operations:

```yaml
- not:
    - ... # Operation 1
    - ... # Operation 2
    - ... # Operation 3
```

As a shortcut, `not` can be prepended to the name of a single operation:

```yaml
- not eq:
    - $(PATH): "/hidden"
```

All children operations **must** fail for `not` to succeed.

## Examples

* The following two rules are equivalent:

    ```yaml
    - rule:
        - not:
            - eq:
                $(SCHEME): "http"
    ```

    ```yaml
    - rule:
        - not eq:
            $(SCHEME): "http"
    ```
