# Operator: `response`

## Synopsis

`response` takes either a valid HTTP code:

```yaml
- response: 404
```

Or a mapping with a valid HTTP code and a reason:

```yaml
- response:
    code: 404
    reason: Resource not found
```

This operation always succeed and is always final.

## Examples

* Redirect `/` to `/login`:

    ```yaml
    - rule:
        - eq:
            $(PATH): "/"
        - set:
            $(PATH): "/login"
        - response: 302
    ```
