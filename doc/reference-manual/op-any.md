# Operator: `any`

## Synopsis

`any` takes a list of operations:

```yaml
- any:
    - ... # Operation 1
    - ... # Operation 2
    - ... # Operation 3
```

At least one child operation **must** succeed for `any` to succeed as
well.

## Examples

* If the client is local to the webserver, assumes it always wants to
reach `/heartbeat`:

    ```yaml
    - rule:
        - any:
            - eq:
                $(CLIENT_IP): 127.0.0.1
            - eq:
                $(CLIENT_IP): ::1
        - set:
            $(PATH): "/heartbeat"
    ```

* Combination of `all` and `any` to assert that a user is allowed to
access the website:

    ```yaml
    - rule:
        # Check that the connection fulfills the conditions.
        - any:
            # Case #1: User is authenticated and uses a secure
            # connection.
            - all:
                - eq:
                    $(SCHEME): https
                - match:
                    $(AUTH_USER): "\\." # User name isn't empty.
            # Case #2: The connection comes from the local trusted
            # network.
            - any:
                - eq:
                    $(CLIENT_IP): 192.168.10.0/24
                - eq:
                    $(CLIENT_IP): 2001:aabb:ccdd:eeff:0:0:0:0/64
        # If we reach this point, the connection is authorized: move on
        # to next rule (ie. this rule is not final).
        - goto: NEXT
    ```
