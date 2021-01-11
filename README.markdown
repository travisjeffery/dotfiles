Hey, these are [@travisjeffery's](http://twitter.com/travisjeffery) dotfiles.

I use Emacs, Arch Linux, Z shell.

I've published this in the hopes that someone finds them useful, but it's not directly usable because it depends on files that are outside of the repo that I sync using Syncthing. You can remove the sync_dir variable in site.yaml to disable the tasks that depend on those files.

Installation would go:

``` sh
$ git clone git://github.com/travisjeffery/dotfiles.git && 
    cd dotfiles && # delete sync_dir var &&
    ansible-galaxy collection install community.kubernetes &&
    ansible-galaxy collection install ansible.posix &&
    ansible-playbook -K site.yaml
```




