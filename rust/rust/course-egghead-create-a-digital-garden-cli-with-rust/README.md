# Digital Garden

A CLI tool for the creation and maintenance of Digital Gardens.

## Commands

### Setting the garden path

```shell
GARDEN_PATH=~/.local/share/garden garden write
garden -p ~/.local/share/garden write
garden --garden-path ~/.local/share/garden write
```

### write

Open a new file to write in our digital garden. Since we
don't necessarily know what we want to title what we're
writing, we'll leave the title as optional and ask the
user for it later if they don't provide it up-front.

```shell
garden write
garden write -t "<title here>"
```
