# Operator: `all`

## Synopsis

`all` takes a list of operations:

```yaml
- all:
    - ... # Operation 1
    - ... # Operation 2
    - ... # Operation 3
```

All children operations **must** succeed for `all` to succeed as well.

The `rule` keyword in the rule definition is an equivalent of `all`,
because all top-level operations must succeed for the rule to succeed.

## Examples

* The following two rules are equivalent:

    ```yaml
    - name: "index.html is the default entry point"
      rule:
        - all:
            # Check if the request path ends with a "/".
            - match:
                $(PATH): "/$"
            - set:
                $(PATH): "/index.html"
    ```

    ```yaml
    - name: "index.html is the default entry point"
      rule:
        # Check if the request path ends with a "/".
        - match:
            $(PATH): "/$"
        - set:
            $(PATH): "/index.html"
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
