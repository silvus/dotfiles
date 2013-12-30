My bashrc
====

Somes personals bash aliases (for Ubuntu-like).

Installation
------------

```shell
cd ~ ;
git clone https://github.com/Silvus/.bash.git ;
vi .bash_aliases ;
```

```shell
if [ -f ~/.bash/.bash_import ]; then
	. ~/.bash/.bash_import
fi
```

Optionnal
---------

```shell
touch ~/.bash/.bash_ssh ;
touch ~/.bash/.bash_dev ;
```