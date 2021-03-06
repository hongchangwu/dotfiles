{
  core = {
    whitespace = "trailing-space,space-before-tab";
  };

  color = {
    branch = "auto";
    diff = "auto";
    interactive = "auto";
    status = "auto";
    ui = "auto";
  };

  pull = {
    ff = "only";
  };

  push = {
    default = "current";
  };

  diff = {
    tool = "vimdiff";
    guitool = "gvimdiff";
  };

  difftool = {
    prompt = false;
  };

  merge = {
    tool = "vimdiff";
    guitool = "gvimdiff";
    conflictstyle = "diff3";
  };

  mergetool = {
    prompt = false;
    keepBackup = false;
  };

  alias = {
    co = "checkout";
    cob = "checkout -b";
    cot = "checkout -t";
    a = "add";
    aa = "add -A";
    au = "add -u";
    c = "commit -m";
    ca = "!git add -A && git commit -m";
    cf = "commit --fixup";
    cu = "!git add -u && git commit -m";
    wip = ''commit -am "WIP"'';
    undo = "reset --mixed HEAD~";
    extend = "commit -a --amend --no-edit";
    amend = "commit -a --amend";
    reword = "commit --amend";
    abandon = ''!git add -A && git commit -qm "Abandoned" && git reset --hard HEAD~'';
    l = "!git --no-pager log --oneline --decorate HEAD";
    lg = "log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit HEAD";
    lu = "!git lg -u HEAD";
    last = "!git --no-pager show --name-only --oneline HEAD";
    st = "status --short -uno";
    ls = "ls-files";
    sync-merge = "!git fetch upstream master && git merge FETCH_HEAD";
    sync-rebase = "!git fetch upstream master && git rebase FETCH_HEAD";
    review = "diff --cached";
    cp = "cherry-pick";
    br = "!git --no-pager branch";
  };
}
