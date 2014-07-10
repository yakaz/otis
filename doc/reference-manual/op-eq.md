# Operator: `eq`

## Synopsis

`eq` takes a mapping with a single `key: value` pair, where the key must be a variable:

```yaml
- eq:
    $(variable): "Hello"
```

Both operands must match exactly for `eq` to succeed.

The value could be:
* a string
* an integer
* an IP address
* an IP mask
* an IP addresses range

## Examples

* Only secure connection are accepted:

    ```yaml
    - rule:
        - eq:
            $(SCHEME): "https"
    ```

* Deny `PUT` requests with an empty body:

    ```yaml
    - rule:
        - eq:
            $(METHOD): "PUT"
        - eq:
            $(header:Content-Length): 0
    ```

* Allow trusted IP addresses only:

    ```yaml
    - rule:
        - any:
            # Single IP addess.
            - eq:
                $(CLIENT_IP): 127.0.0.1
            - eq:
                $(CLIENT_IP): ::1
            # IP mask.
            - eq:
                $(CLIENT_IP): 192.168.1.0/24
            - eq:
                $(CLIENT_IP): 2001:aabb:ccdd:eeff:0:0:0:0/64
            # IP addresses range.
            - eq:
                $(CLIENT_IP): 192.168.1.10 192.168.1.20
            - eq:
                $(CLIENT_IP): 2001:aabb:ccdd:eeff:0:0:0:10 2001:aabb:ccdd:eeff:0:0:0:20
    ```
