# My dotfiles

Managed with GNU stow
- Dependencies needed for install are:
  * `stow`
  * `git`
- Configuration is given for:
  * `alacritty`
  * `fish`
  * `helix`
  * `i3wm`
- Extra recommended packages
  * `gf2`: gdb frontend

## Instructions for cloning
First go to home folder and create a "dotfiles" folder

```bash
$ cd && mkdir dotfiles && cd dotfiles
```
Now clone this repo

```bash
$ git clone
```

and finally stow
```bash
$ stow .
```
If there are any conflicting files delete them from your original location
or use `stow . --adopt` to override mine.


