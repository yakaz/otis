# Operator: `goto`

## Synopsis

`goto` takes either:
* the name of a rule

    ```yaml
    - goto: "Check validity"
    ```

* the number of a rule

    ```yaml
    - goto: 4
    ```

* a special keyword

    ```yaml
    - goto: NEXT
    ```

It then jumps to the given rule or stop right there.

This operation always succeeds.

When all operations in a rule succeed and there's no `goto` statement at
the end, the rule is always final. This is equivalent to use the `STOP`
special keyword.

## Special keywords

| Keyword | Description |
|---------|-------------|
| `FIRST`    | Go to the first rule    |
| `PREVIOUS` | Go to the previous rule |
| `NEXT`     | Go to the next rule     |
| `LAST`     | Go to the last rule     |
| `STOP`     | Stop request processing |

## Examples

* The following two rules are equivalent:

    ```yaml
    - name: Log scheme
      rule:
        - log: "Scheme: $(SCHEME)"
    ```

    ```yaml
    - name: Log scheme
      rule:
        - log: "Scheme: $(SCHEME)"
        - goto: STOP
    ```

* Restart from the beginning:

    ```yaml
    - rule:
        - eq:
            $(acceptable): false
        - goto: FIRST
    ```
