cask "nix-specter" do
  version :latest
  sha256 :no_check

  url "file:///dev/null"
  name "Specter"
  desc "Authentic terminal primitives enabling autonomous agents to operate interactive applications"
  homepage "https://github.com/ada-yang-dev/specter"

  depends_on cask: "nix"

  postflight do
    rev = Pathname("#{HOMEBREW_PREFIX}/var/nix/specter.rev").then { _1.exist? ? _1.read.strip : nil }
    flake = "github:ada-yang-dev/specter#{rev ? "/#{rev}" : ""}"
    profile = "/nix/var/nix/profiles/per-user/#{ENV["USER"]}/homebrew"

    system_command "nix", args: ["profile", "install", flake, "--profile", profile]
    %w[specter specterd specterctl].each { |b| FileUtils.ln_sf "#{profile}/bin/#{b}", "#{HOMEBREW_PREFIX}/bin/#{b}" }
  end

  uninstall_preflight do
    rev = Pathname("#{HOMEBREW_PREFIX}/var/nix/specter.rev")
    return puts "specter: pinned to #{rev.read.strip}. Remove #{rev} to uninstall." if rev.exist?

    profile = "/nix/var/nix/profiles/per-user/#{ENV["USER"]}/homebrew"
    system_command "nix", args: ["profile", "remove", ".*specter.*", "--profile", profile]
    %w[specter specterd specterctl].each { |b| FileUtils.rm_f "#{HOMEBREW_PREFIX}/bin/#{b}" }
  end
end
