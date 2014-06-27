# otis: HTTP request rewriting engine

**otis** is an Erlang application to be used with an HTTP agent to rewrite requests.

Rewrite rules are expressed in a YAML configuration file. The rules are used to generate and compile an Erlang module at runtime. This module can be regenerated on-the-fly to change the rules. Thanks to this, the overhead of the rewrite engine remains very low.

Currently, only [Yaws](http://yaws.hyber.org/) is supported. But the glue between the HTTP agent and otis is isolated in a separate module, so it's easy to add support for another web server.

otis is distributed under the terms of the **2-clause BSD license**; see `COPYING`.

## Installation

### Rebar

If you use rebar, you can run the following command to build the application:
```bash
rebar get-deps
rebar compile
```

### Autotools

Requirements:
* [yamerl](https://github.com/yakaz/yamerl)
* [Yaws](http://yaws.hyber.org/)

If you use the Autotools and `make(1)`, run the following commands to build the application:
```bash
# Generate Autotools files.
autoreconf -vif

# Build the application.
./configure
make

# Install it.
sudo make install
```

The default installation path is your Erlang's distribution libraries directory (see `code:lib_dir()`).

## Getting started

Before using otis, the application must be started:
```erlang
application:start(yamerl),
application:start(otis).
```

### Specifying a rules configuration file

Once you have a rules configuration file ready (see below for its syntax), you have to ways of specifying its path to otis:

* Pass the filename to `otis:reload_engine/1`:
    ```erlang
    otis:reload_engine("/path/to/rules.yaml").
    ```

* Use the `config` application environment parameter. Here's an example in `sys.config`:
    ```erlang
    %% sys.config
    [
        {otis, [
            {config, "/path/to/rules.yaml"}
        ]}
    ].
    ```

### Configuring Yaws

Currently, the rewrite engine is global to all virtual hosts, but you can use conditions to only affect a particular vhost.

To enable the rewrite engine, add the following directive to any relevant virtual hosts:
```
<server www.example.com>
    ...
    arg_rewrite_mod = otis_yaws_mod_rewrite
    ...
</server>
```

### Writing rules

The file uses a [YAML](http://www.yaml.org/) syntax. Here's a simple example:
```yaml
- name: www.example.com/myaccount is handled by /core/accounts.php
  rule:
    - eq:
        $(HOST): "www.example.com"
    - eq:
        $(PATH): "/myaccount"
    - set:
        $(PATH): "/core/accounts.php"
```

You can test this rule from the Erlang shell:
```erlang
%% Start otis and load rules.
application:start(yamerl).
application:start(otis).
otis:reload_engine("/path/to/rules.yaml").

otis_reqrw_engine:uri("http://www.example.com").
%% Return: "http://www.example.com/"

otis_reqrw_engine:uri("http://www.example.com/myaccount").
%% Returns: "http://www.example.com/core/accounts.php"
```

One important difference compared to Apache's `mod_rewrite` is that **a matching rule is final, except specified otherwise**! This is the opposite to `mod_rewrite`.

### Reloading rules

* If you use the `config` application environment parameter:
    ```erlang
    otis:reload_engine().
    ```

* If you don't use the `config` parameter:
    ```erlang
    otis:reload_engine("/path/to/rules.yaml").
    ```

If an error occurs while loading the new rules, the previous set of rules remains active.
