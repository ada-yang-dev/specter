cask "nix" do
  version "2.94.0"
  sha256 "2048ddb5af8e4cbcdee185a094e1433211eea257246223176c3594cfb1abd90f"

  url "https://install.lix.systems/lix"
  name "Nix"
  desc "Nix package manager (Lix implementation)"
  homepage "https://lix.systems"

  postflight do
    nix_bin = "/nix/var/nix/profiles/default/bin"

    if File.exist?("/nix")
      raise "/nix exists. Remove manually before install."
    end

    system_command "sh", args: [staged_path/"lix", "install", "--no-confirm"]
    gc_marker = "#{HOMEBREW_PREFIX}/var/nix/.gc"
    
    FileUtils.mkdir_p "#{HOMEBREW_PREFIX}/var/nix"
    
    %w[nix-build nix-shell nix-env nix-store nix-instantiate nix-collect-garbage nix-channel].each do |cmd|
      File.write "#{HOMEBREW_PREFIX}/bin/#{cmd}", "#!/bin/sh\necho 'Use: nix' >&2\nexit 1\n"
      FileUtils.chmod 0755, "#{HOMEBREW_PREFIX}/bin/#{cmd}"
    end

    File.write "#{HOMEBREW_PREFIX}/bin/nix", <<~SH
      #!/bin/sh
      gc=#{gc_marker}
      [ -f "$gc" ] && [ $(($(date +%s) - $(stat -f %m "$gc"))) -lt 86400 ] || \\
        { #{nix_bin}/nix-collect-garbage --delete-older-than 1d >/dev/null 2>&1 ||:; touch "$gc"; }
      exec #{nix_bin}/nix "$@"
    SH
    FileUtils.chmod 0755, "#{HOMEBREW_PREFIX}/bin/nix"
  end

  uninstall_preflight do
    system_command "/nix/lix-installer", args: ["uninstall", "--no-confirm"]
    
    %w[nix nix-build nix-shell nix-env nix-store nix-instantiate nix-collect-garbage nix-channel].each do |cmd|
      FileUtils.rm_f "#{HOMEBREW_PREFIX}/bin/#{cmd}"
    end
    FileUtils.rm_rf "#{HOMEBREW_PREFIX}/var/nix"
  end

end
