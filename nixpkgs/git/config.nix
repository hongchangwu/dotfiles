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
    a = "add";
    aa = "add -A";
    abandon = ''!git add -A && git commit -qm "Abandoned" && git reset --hard HEAD~'';
    amend = "commit -a --amend";
    au = "add -u";
    br = "!git --no-pager branch";
    c = "commit -m";
    ca = "!git add -A && git commit -m";
    cf = "commit --fixup";
    co = "checkout";
    cob = "checkout -b";
    cot = "checkout -t";
    cp = "cherry-pick";
    cu = "!git add -u && git commit -m";
    extend = "commit -a --amend --no-edit";
    l = "!git --no-pager log --oneline --decorate HEAD";
    last = "!git --no-pager show --name-only --oneline HEAD";
    lg = "log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit HEAD";
    ls = "ls-files";
    lu = "!git lg -u HEAD";
    review = "diff --cached";
    reword = "commit --amend";
    st = "status --short -uno";
    sync-merge = "!git fetch upstream master && git merge FETCH_HEAD";
    sync-rebase = "!git fetch upstream master && git rebase FETCH_HEAD";
    undo = "reset --mixed HEAD~";
    wip = ''commit -am "WIP"'';
  };
}
