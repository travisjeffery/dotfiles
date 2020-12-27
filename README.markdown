Hey, these are [@travisjeffery's](http://twitter.com/travisjeffery) dotfiles.

I use Emacs, Arch Linux, Z shell.

``` sh
$ git clone git://github.com/travisjeffery/dotfiles.git && 
    cd dotfiles && 
    ansible-playbook -K site.yaml
```

Though you'll likely want to delete the sync_dir variable in site.yaml to disable tasks that depend on some
private files I sync using Syncthing.


