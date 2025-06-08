{ pkgs, ... }:

{

  home.packages = with pkgs; [
    git
  ];

  programs.git = {
    enable = true;

    extraConfig = {

      # https://blog.gitbutler.com/how-git-core-devs-configure-git/

      include = {
        path = "~/.dotfiles/custom/gitconfig";
      };

      core = {
        editor = "vim";
        # Make sure that CRLF is replaced with LF
        autocrlf = "input";
        eol = "lf";
        # is default
        # excludesfile = ~/.config/git/ignore
        quotepath = false;
      };
      alias = {
        st = "status";
        s = "status -sb";
        lg = "log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit --";
        gr = "grep -p";
        aa = "add --all";
        co = "commit";
        pusom = "push origin master";
        pulom = "pull origin master";
        dif = "diff --stat=160,120";
      };
      color = {
        ui = "auto";
      };
      column = {
        # Output on columns
        ui = "auto";
      };
      branch = {
        # Branch sorting
        sort = "-committerdate";
      };
      tag = {
        # Tag sorting (treating dotted version numbers as a series of integer values for sorting purposes)
        sort = "version:refname";
      };
      init = {
        defaultBranch = "main";
      };
      web = {
        browser = "elinks";
      };
      diff = {
        tool = "vimdiff";
        submodule = "log";
        # Default algo is myers
        algorithm = "histogram";
        # Show moved line on different color
        colorMoved = "plain";
        # Replace the a/ and b/ in your diff header output with where the diff is coming from
        # so i/ (index), w/ (working directory) or c/ commit.
        mnemonicPrefix = true;
        # Detect if a file has been renamed
        renames = true;
      };
      difftool = {
        prompt = false;
      };
      credential = {
        helper = "cache";
      };
      merge = {
        tool = "vimdiff";
        # Insert what the base of it looked like before the conflict
        # (just 'diff3' if git version < 2.3)
        conflictstyle = "zdiff3";
      };
      mergetool = {
        prompt = false;
      };
      status = {
        submoduleSummary = true;
      };
      user = {
        # Insists the user set their user.email and user.name are set before committing
        useConfigOnly = true;
      };
      help = {
        # Git will guess what you meant and try to run it
        # autocorrect = prompt
      };
      commit = {
        # Get diff when writing commit message
        verbose = true;
      };
      push = {
        # "simple" is default since 2.0 (Push only the current branch if its named upstream is identical)
        default = "simple";
        # If the upstream is not set, it will automatically set it
        # autoSetupRemote = true
        # Push all tags that you have locally that aren’t on the server, every time you push anything
        followTags = true;
      };
      fetch = {
        # Make your remote references as close to what is on the remote as possible
        # prune = true
        # pruneTags = true
        # all = true
      };
      pull = {
        rebase = true;
      };
      rebase = {
        # Help in rebase
        autoSquash = true;
        autoStash = true;
        # Takes stacked refs in a branch and makes sure they're also moved when a branch is rebased
        updateRefs = true;
      };
      rerere = {
        # Reuse recorded resolutions (rebases with conflicts)
        enabled = true;
        autoupdate = true;
      };

    };

  };

}

