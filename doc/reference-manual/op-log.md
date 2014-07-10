# Operator: `log`

## Synopsis

`log` takes a string to log:

```yaml
- log: "Message"
```

This operation always succeeds.

## Examples

* Log accesses:

    ```yaml
    - name: access.log
      rule:
        - log: "$(CLIENT_IP) - $(AUTH_USER) \"$(METHOD) $(URI)\" \"$(header:User-Agent)\""
    ```
