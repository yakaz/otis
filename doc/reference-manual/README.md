# otis Reference manual

## Configuration file structure

otis configuration file is written using [YAML 1.2](http://www.yaml.org/) syntax.

* The file should contain a single YAML document. If it contains many,
only the first document is considered.

* The YAML document must contain a **list of rules**:
    ```yaml
    - rule: ... # Rule 1
    - rule: ... # Rule 2
    - rule: ... # Rule 3
    ```

    You can use YAML folding syntax if you prefer:
    ```yaml
    [
      rule: ..., # Rule 1
      rule: ..., # Rule 2
      rule: ..., # Rule 3
    ]
    ```

* Rules are **evaluated in the order they appear** in the file.

* A **matching rule is final**, unless specified otherwise. This is
opposite to eg. Apache's `mod_rewrite`.

## Rule structure

* A rule must be a YAML mapping. The following keys are accepted:
  * **`name`**: This allows you to name your rule. The value must be a
    string. It can be used later to jump between rules.
    ```yaml
    - name: "My account short URL"
    ```

  * **`desc`**: This allows you to further document your rule. The value
    must be a string. It's currently unused.
    ```yaml
    - name: "My account short URL"
      desc: >
        A short and simple URL is presented to the user. This rule
        rewrites it to point to the real PHP script.
    ```

  * **`rule`**: This points to a list of operations to evaluate for this
    rule. For a rule to match, all operations must succeed. A rule
    without operations will always succeed and be final.
    ```yaml
    - name: "My account short URL"
      rule:
      - eq:
          $(PATH): "/myaccount"
      - set:
          $(PATH): "/core/accounts.php"
    ```

## Variables

### Syntax

* The syntax is `$(variable_name)`.
* A prefix can be prepended to a variable name to indicate a special
    source (headers, query string, cookies).
* A variable name can be variable itself: `$(host_$(env))`.
* You can use variables in many operations where strings are expected.
* You can change variables' value using `match`, `set` and `subst`
    operators.

### Scope

* Variables are visible in all rules.
* If a rule sets or modifies a variable, next evaluated rules will see
    the new value.
* A new environment is created for each request, therefore variables set
    for a request are not visible to following requests.

### Variable name prefix

Four variable name prefixes are supported:

* **`header:`** followed by the name of an HTTP request header
    (case-insensitive).
    ```yaml
    - eq:
        $(header:Content-Type): "text/css"
    ```

* **`rheader:`** followed by the name of an HTTP response header
    (case-insensitive).
    ```yaml
    - eq:
        $(rheader:Location): "http://www.example.com"
    ```

* **`query:`** followed by the name of a query string parameter
    (case-sensitive).
    ```yaml
    - eq:
        $(query:name): "Bob"
    ```

* **`cookie:`** followed by the name of a cookie (case-sensitive).
    ```yaml
    - eq:
        $(cookie:lang): "fr_FR"
    ```

### Predefined variables

| Variable | Description |
|----------|-------------|
| `$(AUTH_PASSWD)` | Authenticated user password, as given by the web server    |
| `$(AUTH_USER)`   | Authenticated user name, as given by the web server        |
| `$(CLIENT_IP)`   | Remote peer IP address                                     |
| `$(CLIENT_PORT)` | Remote peer TCP port number                                |
| `$(FRAGMENT)`    | URL fragment without the leading `#`                       |
| `$(HOST)`        | The `Host:` request header, the host given by the web server or `$(VHOST_NAME)` |
| `$(METHOD)`      | HTTP request method                                        |
| `$(PATH)`        | Part of the URL between hostname:port and the query string |
| `$(QUERY)`       | Raw query string without the leading `?`                   |
| `$(RULE)`        | Rule's name (`name:`)                                      |
| `$(SCHEME)`      | HTTP request scheme (`http` or `https`)                    |
| `$(SERVER_PORT)` | Server-side TCP port number                                |
| `$(URI)`         | Complete URL                                               |
| `$(VHOST_NAME)`  | Name of the virtual host, as given by the web server       |

### Examples

* Log user name and its IP address:

    ```yaml
    - rule:
        - log: "$(AUTH_USER): $(CLIENT_IP)"
    ```

* Check if the `Host:` request header matches the virtual host name:

    ```yaml
    - rule:
        - eq:
            $(header:Host): $(VHOST_NAME)
    ```

* Check if the sub-domain matches the language given in the query string:

    ```yaml
    - rule:
        - match:
            $(HOST): "^$(query:lang)\\."
    ```

## Operators

| Operator | Description |
|----------|-------------|
| [`all`](op-all.md#operator-all)       | All children operations must succeed for `all` to succeed too               |
| [`any`](op-any.md#operator-any)       | At least one child operation must succeed for `any` to succeed too          |
| [`eq`](op-eq.md#operator-eq)          | Both operands must be equal for `eq` to succeed                             |
| [`goto`](op-goto.md#operator-goto)    | Jump to another rule                                                        |
| [`log `](op-log.md#operator-log)      | Log a message                                                               |
| [`match`](op-match.md#operator-match) | Left operand must match the given regex                                     |
| [`not`](op-not.md#operator-not)       | All children operations must fail for `not` to succeed                      |
| [`response`](op-response.md#operator-response) | Return an HTTP response instead of proceeding further with the HTTP request |
| [`set`](op-set.md#operator-set)       | Set variable's value                                                        |
| [`subst`](op-subst.md#operator-subst) | Change variable's value using a regex                                       |

The `rule` keyword in the rule definition is an equivalent of `all`,
because all top-level operations must succeed for the rule to succeed.
